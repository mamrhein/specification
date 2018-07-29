# -*- coding: utf-8 -*-
# ----------------------------------------------------------------------------
# Name:        test_extd_ast_expr
# Purpose:     Test driver for helper classes used in module 'specification'
#              specification.
#
# Author:      Michael Amrhein (michael@adrhinum.de)
#
# Copyright:   (c) 2017 Michael Amrhein
# License:     This program is part of a larger application. For license
#              details please read the file LICENSE.TXT provided together
#              with the application.
# ----------------------------------------------------------------------------


"""Test driver for helper classes used in module 'specification'."""


import ast
import os
import unittest

from specification._extd_ast_expr import (
    ast2src,
    Conjunction,
    Contradiction,
    Disjunction,
    Expression,
    names,
    Negation,
    SourceGenerator,
    src2ast,
)


# obtain test expressions (mapped to the resulting - maybe simplified -
# source code) from file

TEST_EXPRESSIONS = {}
dir_name = os.path.dirname(__file__)
file_path = os.path.join(dir_name, 'test_expressions.txt')
with open(file_path) as txt:
    for line in txt:
        if not line.startswith('#'):
            expr, src = line.rstrip('\n').split('\t')
            TEST_EXPRESSIONS[expr] = src

# code that does not contain a valid expression

INVALID_EXPRESSIONS = [
    "or a",
    "x = 7",
    "del x.a",
]


class SourceGeneratorTest(unittest.TestCase):

    """Tests for :class:SourceGenerator"""

    def test_generator(self):
        """Test against expressions in txt file."""
        generator = SourceGenerator()
        for expr in TEST_EXPRESSIONS:
            # print('---', expr, '___')
            src = TEST_EXPRESSIONS[expr]
            ast_expr = ast.parse(expr, mode='eval')
            self.assertEqual(src, generator(ast_expr))
            # if original expression differs from generated source,
            # parse the latter and check again
            if expr != src:
                ast_expr = ast.parse(src, mode='eval')
                self.assertEqual(src, generator(ast_expr))


class ExpressionTest(unittest.TestCase):

    """Tests for functions operating on AST.Expression"""

    def test_src2ast(self):
        """Test src -> ast"""
        for expr in TEST_EXPRESSIONS:
            self.assertIsInstance(src2ast(expr), Expression)
        for expr in INVALID_EXPRESSIONS:
            self.assertRaises(ValueError, src2ast, expr)

    def test_ast2src(self):
        """Test ast -> src"""
        src = "{x.i + y.i for x, y in {(a, b) for a, b in ml}}"
        expr = src2ast(src)
        self.assertEqual(ast2src(expr), src)

    def test_names(self):
        """Test name extractor"""
        src = "x == a or x == b"
        expr = src2ast(src)
        self.assertEqual(names(expr), {'a', 'b', 'x'})
        src = "{x.i + y.i for x, y in {(a, b) for a, b in ml}}"
        expr = src2ast(src)
        self.assertEqual(names(expr), {'ml'})

    def test_negation(self):
        """Test :class:Negation"""
        expressions = {
            "x": "not x",
            "-x": "not -x",
            "x == 5": "x != 5",
            "x != 5": "x == 5",
            "-3 < x <= y < 5": "-3 >= x > y >= 5",
            "x is y": "x is not y",
            "x in y": "x not in y",
        }
        for src, neg_src in expressions.items():
            expr = src2ast(src)
            neg_expr = Negation(expr)
            self.assertTrue(hasattr(neg_expr.body, 'lineno'))
            self.assertEqual(ast2src(neg_expr), neg_src)
            expr = src2ast(neg_src)
            self.assertEqual(ast2src(Negation(expr)), src)

    def test_conjunction(self):
        """Test :class:Conjunction"""
        expressions = {
            ("x", "y"): "x and y",
            ("-x", "y"): "-x and y",
            ("x == 5", "x != 5"): "x == 5 and x != 5",
            ("-3 < x <= y < 5", "2 ** y != 58"):
                "-3 < x <= y < 5 and 2 ** y != 58",
            ("x is y", "y is x"): "x is y and y is x",
            ("x or y", "y or z"): "(x or y) and (y or z)",
        }
        for (src1, src2), conj_src in expressions.items():
            expr1 = src2ast(src1)
            expr2 = src2ast(src2)
            conj = Conjunction(expr1, expr2)
            self.assertTrue(hasattr(conj.body, 'lineno'))
            self.assertEqual(ast2src(conj), conj_src)
            names1 = names(expr1)
            names2 = names(expr2)
            self.assertEqual(names(conj), names1 | names2)

    def test_disjunction(self):
        """Test :class:Disjunction"""
        expressions = {
            ("x", "y"): "x or y",
            ("-x", "y"): "-x or y",
            ("x == 5", "x != 5"): "x == 5 or x != 5",
            ("-3 < x <= y < 5", "2 ** y != 58"):
                "-3 < x <= y < 5 or 2 ** y != 58",
            ("x is y", "y is x"): "x is y or y is x",
            ("x or y", "y or z"): "x or y or (y or z)",
        }
        for (src1, src2), disj_src in expressions.items():
            expr1 = src2ast(src1)
            expr2 = src2ast(src2)
            disj = Disjunction(expr1, expr2)
            self.assertTrue(hasattr(disj.body, 'lineno'))
            self.assertEqual(ast2src(disj), disj_src)
            names1 = names(expr1)
            names2 = names(expr2)
            self.assertEqual(names(disj), names1 | names2)

    def test_contradiction(self):
        """Test :class:Contradiction"""
        expressions = {
            ("x", "y"): "x and not y or not x and y",
            ("-x", "y"): "-x and not y or not -x and y",
            ("x == 5", "x != 5"): "x == 5 and x == 5 or x != 5 and x != 5",
            ("-3 < x <= y < 5", "2 ** y != 58"):
                "-3 < x <= y < 5 and 2 ** y == 58 or "
                "-3 >= x > y >= 5 and 2 ** y != 58",
            ("x is y", "y is x"):
                "x is y and y is not x or x is not y and y is x",
            ("x or y", "y or z"):
                "(x or y) and not (y or z) or "
                "not (x or y) and (y or z)",
        }
        for (src1, src2), contr_src in expressions.items():
            expr1 = src2ast(src1)
            expr2 = src2ast(src2)
            contr = Contradiction(expr1, expr2)
            self.assertTrue(hasattr(contr.body, 'lineno'))
            self.assertEqual(ast2src(contr), contr_src)
            names1 = names(expr1)
            names2 = names(expr2)
            self.assertEqual(names(contr), names1 | names2)


if __name__ == '__main__':                              # pragma: no cover
    unittest.main()
