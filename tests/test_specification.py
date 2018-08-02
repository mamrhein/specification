# -*- coding: utf-8 -*-
# ----------------------------------------------------------------------------
# Name:        test_specification
# Purpose:     Test driver for module specification.
#
# Author:      Michael Amrhein (michael@adrhinum.de)
#
# Copyright:   (c) 2014 Michael Amrhein
# License:     This program is part of a larger application. For license
#              details please read the file LICENSE.TXT provided together
#              with the application.
# ----------------------------------------------------------------------------


"""Test driver for module specification."""


# standard library imports
from abc import ABC
import ast
from datetime import date
from itertools import combinations
import operator
import unittest

from specification import Specification


op2sym = {
    operator.eq: '==',
    operator.ne: '!=',
    operator.lt: '<',
    operator.ge: '>=',
    operator.gt: '>',
    operator.le: '<=',
    operator.is_: 'is',
    operator.is_not: 'is not',
}


class ITestObj1(ABC):

    a = property()
    b = property()


@ITestObj1.register
class TestObj1:

    def __init__(self, a=1, b=2):
        self.a = a
        self.b = b


class ITestObj2(ITestObj1):

    ab = property(doc='a * b')


@ITestObj2.register
class TestObj2:

    def __init__(self, a=1, b=2):
        self.a = a
        self.b = b

    @property
    def ab(self):
        return self.a * self.b


class TestConstruction(unittest.TestCase):

    def testSpecFromStr(self):
        expr = "b == 2"
        spec = Specification(expr)
        self.assertIsInstance(spec._ast_expr, ast.Expression)
        self.assertEqual(spec._candidate_name, 'b')
        self.assertEqual(spec._context, {})
        # nested attribute
        expr = "b.x.y==2"
        spec = Specification(expr)
        self.assertIsInstance(spec._ast_expr, ast.Expression)
        self.assertEqual(spec._candidate_name, 'b')
        self.assertEqual(spec._context, {})
        # nested attribute and context variables
        y = object()
        z = object()
        expr = "y.a > x.b >= z.c"
        spec = Specification(expr, candidate_name='x')
        self.assertIsInstance(spec._ast_expr, ast.Expression)
        self.assertEqual(spec._candidate_name, 'x')
        self.assertEqual(spec._context, {'y': y, 'z': z})

    def testSpecFromExpression(self):
        src = "x==1"
        expr = ast.parse(src, mode='eval')
        spec = Specification(expr)
        self.assertIsInstance(spec._ast_expr, ast.Expression)
        self.assertEqual(spec._candidate_name, 'x')
        self.assertEqual(spec._context, {})

    def testFailures(self):
        # not an expression
        expr = "x = 1"
        self.assertRaises(ValueError, Specification, expr)
        # wrong candidate name
        expr = "x==1"
        self.assertRaises(ValueError, Specification, expr,
                          candidate_name='a')
        # missing candidate name
        y = 4                                       # noqa
        expr = "x != y"
        self.assertRaises(ValueError, Specification, expr)
        # undefined context variable
        expr = "x > y and x < z"
        self.assertRaises(NameError, Specification, expr, 'x')


class TestComposition(unittest.TestCase):

    def setUp(self):
        self.x = x = object()                        # noqa
        self.y = y = 5                               # noqa
        self.z = z = 13                              # noqa
        self.spec1 = Specification("y == 2")
        self.spec2 = Specification("ab>20")
        self.spec3 = Specification("x==y", candidate_name='x')
        self.spec4 = Specification("x.ab>=z", candidate_name='x')
        self.spec5 = Specification("x.ab>=z", candidate_name='z')
        self.spec6 = Specification("x.ab>=z", candidate_name='x',
                                   namespace = {'z': 7})

    def testComposites(self):
        # implicit name adjustment
        specs = (self.spec1, self.spec2)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(*specs)
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'y')
            self.assertEqual(cspec._context, {})
        specs = (self.spec2, self.spec1)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(*specs)
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'ab')
            self.assertEqual(cspec._context, {})
        specs = (self.spec1, self.spec3)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(*specs)
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'x')
            self.assertEqual(cspec._context, {'y': self.y})
        specs = (self.spec3, self.spec2)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(*specs)
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'x')
            self.assertEqual(cspec._context, {'y': self.y})
        specs = (self.spec2, self.spec3)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(*specs)
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'ab')
            self.assertEqual(cspec._context, {'y': self.y})
        # multiple context vars
        specs = (self.spec2, self.spec3, self.spec4)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = op(op(*specs[:2]), specs[2])
            self.assertIsInstance(cspec._ast_expr, ast.Expression)
            self.assertEqual(cspec._candidate_name, 'ab')
            self.assertEqual(cspec._context, {'y': self.y, 'z': self.z})

    def testNameConflict(self):
        # conflict between candidate name and context variables name
        specs = (self.spec4, self.spec5)
        for op in (operator.and_, operator.or_, operator.xor):
            self.assertRaises(ValueError, op, *specs)
        # conflicting namespaces
        specs = (self.spec4, self.spec6)
        for op in (operator.and_, operator.or_, operator.xor):
            self.assertRaises(ValueError, op, *specs)


class TestNegation(unittest.TestCase):

    def testSimpleSpec(self):
        for op1, op2 in [('==', '!='),
                         ('<', '>='),
                         ('>', '<='),
                         ('is', 'is not')]:
            spec1 = Specification(f'a {op1} 1')
            spec2 = Specification(f'a {op2} 1')
            neg_spec1 = ~spec1
            neg_spec2 = ~spec2
            self.assertTrue(isinstance(neg_spec1, Specification))
            self.assertTrue(isinstance(neg_spec2, Specification))
            self.assertEqual(spec1, neg_spec2)
            self.assertEqual(spec2, neg_spec1)
            self.assertEqual(spec1, ~neg_spec1)
            self.assertEqual(spec2, ~neg_spec2)
        # non-predefined operator:
        op = lambda x, y: x == y                        # noqa
        spec = Specification(f'op(a, 1)', candidate_name='a')
        neg_spec = ~spec
        self.assertTrue(isinstance(neg_spec, Specification))
        self.assertEqual(spec, ~neg_spec)

    def testCompositeSpec(self):
        spec1 = Specification('b == 2')
        spec2 = Specification('ab > 20')
        spec3 = Specification('2 < a <= 7')
        spec4 = Specification('ab not in range(-12, -7)')
        for op in (operator.and_, operator.or_, operator.xor):
            for spec1, spec2 in combinations((spec1, spec2, spec3, spec4), 2):
                spec = op(spec1, spec2)
                neg_spec = ~spec
                self.assertTrue(isinstance(neg_spec, Specification))
                self.assertEqual(spec, ~neg_spec)


class TestIsSatisfiedBy(unittest.TestCase):

    def testSimpleSpec(self):
        val = 5
        tObj = TestObj1(b=val)
        for op1, op2 in [(operator.eq, operator.ne),
                         (operator.lt, operator.ge),
                         (operator.gt, operator.le),
                         (operator.is_, operator.is_not)]:
            spec1 = Specification(f't.b {op2sym[op1]} {val}')
            spec2 = Specification(f't.b {op2sym[op2]} {val}')
            self.assertEqual(spec1.is_satisfied_by(tObj), op1(tObj.b, val))
            self.assertFalse(spec1.is_satisfied_by(tObj) and
                             spec2.is_satisfied_by(tObj))
            spec1 = Specification(f't.b {op2sym[op1]} {-val}')
            spec2 = Specification(f't.b {op2sym[op2]} {-val}')
            self.assertEqual(spec1.is_satisfied_by(tObj), op1(tObj.b, -val))
            self.assertFalse(spec1.is_satisfied_by(tObj) and
                             spec2.is_satisfied_by(tObj))
        # compare instance of subclass
        tObj = TestObj2(b=val)
        spec = Specification(f't.b == {val}')
        self.assertTrue(spec.is_satisfied_by(tObj))
        spec = Specification(f't.b < {val}')
        self.assertFalse(spec.is_satisfied_by(tObj))

    def testAdditionalContext(self):
        year = 1999
        spec = Specification('deadline < date(year, 6, 30)',
                             candidate_name='deadline')
        dt = date(year, 5, 1)
        self.assertTrue(spec(dt))
        self.assertTrue(spec(dt, year=2000))
        self.assertFalse(spec(dt, year=1997))

    def testNameConflict(self):
        spec = Specification('dt >= date(2020, 1, 1)', candidate_name='dt')
        # conflict with given keyword
        candidate = date(2021, 8, 1)
        self.assertRaises(ValueError, spec, candidate, date=date,
                          dt=date.today())

    def testCompositeSpec(self):
        a, b = 5, 7
        ab = a * b
        tObj = TestObj2(a, b)
        spec1 = Specification('t.b == b', candidate_name='t')
        spec2 = Specification('t.ab < ab', candidate_name='t')
        spec3 = Specification('1 <= t.a <= 7')
        cspec = spec1 & spec2 & spec3
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = spec1 | spec2 | spec3
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = spec1 ^ spec2 ^ spec3
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = spec1 & (spec2 & spec3)
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (spec1 & spec2) | spec3
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = (spec1 | spec2) & ~spec3
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (~spec1 | spec2) | ~spec3
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (spec1 & ~spec2) & spec3
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = (spec1 & ~spec2) ^ spec3
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (spec1 ^ spec2) & spec3
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = ~(spec1 ^ spec2) ^ spec3
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = ~(spec1 ^ spec2 ^ spec3)
        self.assertTrue(cspec.is_satisfied_by(tObj, ab=ab))

    def testNestedproperty(self):
        a, b = 5, 7
        ab = a * b
        tObj = TestObj1(a=3, b=TestObj2(a, b))
        spec = Specification(f't.b.ab == {ab}')
        self.assertTrue(spec.is_satisfied_by(tObj))
        spec = ~Specification(f't.b.ab != {ab}')
        self.assertTrue(spec.is_satisfied_by(tObj))
        ispec = Specification('1 <= t.b.a <= 7')
        self.assertTrue(ispec.is_satisfied_by(tObj))
        cspec = spec & ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))


class TestIssue1(unittest.TestCase):

    def setUp(self):
        from decimal import Decimal                     # noqa
        self.spec = Specification("x == Decimal('5.4')",
                                  candidate_name='x')

    def testIsSatisfied(self):
        self.assertFalse(self.spec.is_satisfied_by(5))


class TestReprAndStr(unittest.TestCase):

    def testReprAndStr(self):
        op = lambda x, y: x == y                        # noqa
        spec = Specification('op(o.a,1)', candidate_name='o')
        self.assertEqual(repr(spec),
                         "Specification('op(o.a, 1)', candidate_name='o')")
        self.assertEqual(str(spec), '<o: op(o.a, 1)>')
        self.assertEqual(repr(~spec),
                         "Specification('not op(o.a, 1)', "
                         "candidate_name='o')")
        self.assertEqual(str(~spec), '<o: not op(o.a, 1)>')
        spec = Specification('x.a == 1')
        self.assertEqual(repr(spec), "Specification('x.a == 1')")
        self.assertEqual(str(spec), '<x: x.a == 1>')
        ispec = Specification('0 <  a.b <=6')
        self.assertEqual(repr(ispec), "Specification('0 < a.b <= 6')")
        self.assertEqual(str(ispec), '<a: 0 < a.b <= 6>')
        cspec = ispec & spec
        self.assertEqual(repr(cspec),
                         "Specification('0 < a.b <= 6 and a.a == 1')")
        self.assertEqual(str(cspec), '<a: 0 < a.b <= 6 and a.a == 1>')


class TestEqualityAndHash(unittest.TestCase):

    def test_eq_and_hash(self):

        def op(x, y):
            return x == y                               # pragma: no cover

        spec1 = Specification("op(a, 1)", candidate_name='a')
        spec2 = Specification("op(a,1)", candidate_name='a')
        spec3 = Specification("a == 1")
        self.assertEqual(spec1, spec2)
        self.assertEqual(hash(spec1), hash(spec2))
        self.assertNotEqual(spec1, spec3)
        self.assertNotEqual(hash(spec1), hash(spec3))
        negspec1 = ~spec1
        negspec2 = ~spec2
        negspec3 = ~spec3
        self.assertEqual(negspec1, negspec2)
        self.assertEqual(hash(negspec1), hash(negspec2))
        self.assertNotEqual(negspec1, negspec3)
        self.assertNotEqual(hash(negspec1), hash(negspec3))
        self.assertNotEqual(spec1, negspec1)
        self.assertNotEqual(hash(spec1), hash(negspec1))
        cspec1 = spec1 & negspec2
        cspec2 = spec2 & negspec1
        cspec3 = spec1 & spec3
        self.assertEqual(cspec1, cspec2)
        self.assertEqual(hash(cspec1), hash(cspec2))
        self.assertNotEqual(cspec1, cspec3)
        self.assertNotEqual(hash(cspec1), hash(cspec3))
        self.assertNotEqual(spec1, cspec1)
        self.assertNotEqual(hash(spec1), hash(cspec1))
