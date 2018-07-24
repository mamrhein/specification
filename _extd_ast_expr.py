# -*- coding: utf-8 -*-
# ----------------------------------------------------------------------------
# Name:        _extd_ast_expr
# Purpose:     Extensions of AST subclasses used in expressions
#
# Author:      Michael Amrhein (michael@adrhinum.de)
#
# Copyright:   (c) 2018 ff. Michael Amrhein
# License:     This program is part of a larger application. For license
#              details please read the file LICENSE.TXT provided together
#              with the application.
# ----------------------------------------------------------------------------


"""Extensions of AST subclasses used in expressions"""


# standard library imports
import ast
from ast import AST, Expression
from copy import deepcopy
from functools import singledispatch
from itertools import chain
from typing import Optional, Set, Union

Operator = Union[ast.operator, ast.unaryop, ast.boolop, ast.cmpop]

# TODO: more documentation
# TODO: optimizations by applying boolean algebra rules


class OpBindingManager:

    """Context manager keeping track of nested operators and their binding
    level.
    """

    OPS = {
        ast.Lambda: 1,
        ast.IfExp: 1,
        ast.Or: 3,
        ast.And: 4,
        ast.Not: 5,
        ast.Eq: 6,
        ast.NotEq: 6,
        ast.Lt: 6,
        ast.LtE: 6,
        ast.Gt: 6,
        ast.GtE: 6,
        ast.Is: 6,
        ast.IsNot: 6,
        ast.In: 6,
        ast.NotIn: 6,
        ast.BitOr: 7,
        ast.BitXor: 8,
        ast.BitAnd: 9,
        ast.LShift: 10,
        ast.RShift: 10,
        ast.Add: 11,
        ast.Sub: 11,
        ast.Mult: 12,
        ast.Div: 12,
        ast.FloorDiv: 12,
        ast.Mod: 12,
        ast.UAdd: 13,
        ast.USub: 13,
        ast.Invert: 13,
        ast.Pow: 14,
        ast.Starred: 20,
    }

    def __init__(self) -> None:
        """Initialize instance of OpBindingManager"""
        self.nested_ops = []

    def __call__(self, op: Operator) -> 'OpBindingManager':
        """Append nested operator."""
        self.nested_ops.append((op, self.OPS[type(op)]))
        return self

    def __enter__(self) -> None:
        """Enter context."""
        return None

    def __exit__(self, exc, msg, tb) -> None:
        """Exit context."""
        if exc is None:
            self.nested_ops.pop()

    def diff_binding(self) -> int:
        """Return the difference betweens the binding levels of the current
        and the previous operator.
        """
        try:
            prev_op, prev_op_binding = self.nested_ops[-2]
        except IndexError:
            prev_op, prev_op_binding = None, 0
        try:
            curr_op, curr_op_binding = self.nested_ops[-1]
        except IndexError:
            curr_op, curr_op_binding = None, 0
        # special case
        if prev_op is ast.Pow and isinstance(curr_op, (ast.Invert, ast.USub)):
            return 1
        # print(prev_op, prev_op_binding, curr_op, curr_op_binding)
        return curr_op_binding - prev_op_binding


class SourceGenerator:

    """Generates Python source code from an AST expression."""

    def __init__(self) -> None:
        """Initialize instance of SourceGenerator."""
        self.op_man = OpBindingManager()
        self.compact = False

    # helper

    def parenthesize(self, src: str) -> str:
        """Return `src` embedded in parentheses."""
        return f"({src})"

    def wrap_expr(self, src: str, dfltChaining: bool) -> str:
        """Wrap `src` in parentheses if neccessary."""
        diff_binding = self.op_man.diff_binding()
        if diff_binding < 0 or diff_binding == 0 and not dfltChaining:
            return self.parenthesize(src)
        else:
            return src

    # extended dispatcher (portions copied from standard ast module)

    def visit(self, node: AST, dfltChaining: bool = True) -> str:
        """Process `node` by dispatching to a handler."""
        # print(node.__class__.__name__)
        if node is None:
            return ''
        if isinstance(node, ast.Expression):
            return self.visit(node.body)
        # dispatch to specific or generic method
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node, dfltChaining)

    def generic_visit(self, node: AST, dfltChaining: bool = True) -> str:
        """Default handler, called if no explicit visitor function exists for
        a node.
        """
        for field, value in ast.iter_fields(node):
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, AST):
                        self.visit(item)
            elif isinstance(value, AST):
                self.visit(value)

    # for conveniernce
    __call__ = visit

    # literals

    def visit_NameConstant(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s name as string."""
        return str(node.value)

    def visit_Num(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s number as string."""
        return str(node.n)

    def visit_Str(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s string representation."""
        return repr(node.s)

    CONV_MAP = {115: '!s', 114: '!r', 97: '!a'}

    def visit_FormattedValue(self, node: AST,
                             dfltChaining: bool = True) -> str:
        """Return `node`s value formatted according to its format spec."""
        format_spec = node.format_spec
        return f"{{{self.visit(node.value)}" \
               f"{self.CONV_MAP.get(node.conversion, '')}" \
               f"{':'+self._nested_str(format_spec) if format_spec else ''}}}"

    def _nested_str(self, node: AST) -> str:
        if type(node) is ast.Str:
            return node.s
        if type(node) is ast.JoinedStr:
            return ''.join(self._nested_str(val) for val in node.values)
        return self.visit(node)

    def visit_JoinedStr(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s values concatenated as string."""
        return f'''f"{''.join(self._nested_str(val)
                      for val in node.values)}"'''

    def visit_Bytes(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s bytes as string representation."""
        return repr(node.s)

    def visit_List(self, node: AST, dfltChaining: bool = True) -> str:
        """Return list representation of `node`s elements."""
        return f"[{', '.join([self.visit(elt) for elt in node.elts])}]"

    def visit_Tuple(self, node: AST, dfltChaining: bool = True) -> str:
        """Return tuple representation of `node`s elements."""
        elems = (self.visit(elt) for elt in node.elts)
        return f"({', '.join(elems)}{')' if len(node.elts) != 1 else ',)'}"

    def visit_Set(self, node: AST, dfltChaining: bool = True) -> str:
        """Return set representation of `node`s elements."""
        return '{' + ', '.join([self.visit(elt) for elt in node.elts]) + '}'

    def visit_Dict(self, node: AST, dfltChaining: bool = True) -> str:
        """Return dict representation of `node`s elements."""
        items = (': '.join((self.visit(key), self.visit(value)))
                 for key, value in zip(node.keys, node.values))
        return f"{{{', '.join(items)}}}"

    def visit_Ellipsis(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '...'."""
        return '...'

    # variables

    def visit_Name(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s id."""
        return node.id

    def visit_Starred(self, node: AST, dfltChaining: bool = True) -> str:
        """Return representation of starred expresssion."""
        with self.op_man(node):
            return f"*{self.visit(node.value)}"

    # nested expression
    def visit_Expr(self, node: AST, dfltChaining: bool = True) -> str:
        """Return representation of nested expression."""
        return self.visit(node.value)

    # unary operators

    def visit_UnaryOp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return representation of `node`s operator and operand."""
        op = node.op
        with self.op_man(op):
            return self.visit(op) + self.visit(node.operand)

    def visit_UAdd(self, node: AST, dfltChaining: bool = True) -> str:
        """Return empty string."""
        return ''

    def visit_USub(self, node: AST, dfltChaining: bool = True) -> str:
        """Return minus sign."""
        return '-'

    def visit_Not(self, node: AST, dfltChaining: bool = True) -> str:
        """Return 'not '."""
        return 'not '

    def visit_Invert(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '~'."""
        return '~'

    # binary operators

    def visit_BinOp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s operator and operands as inlined expression."""
        op = node.op
        with self.op_man(op):
            if isinstance(op, ast.Pow):
                # Pow chains right-to-left
                src = self.visit(op).join((self.visit(node.left,
                                                      dfltChaining=False),
                                           self.visit(node.right)))
            else:
                src = self.visit(op).join((self.visit(node.left),
                                           self.visit(node.right,
                                                      dfltChaining=False)))
            return self.wrap_expr(src, dfltChaining)

    def visit_Add(self, node: AST, dfltChaining: bool = True) -> str:
        """Return plus sign."""
        return '+' if self.compact else ' + '

    def visit_Sub(self, node: AST, dfltChaining: bool = True) -> str:
        """Return minus sign."""
        return '-' if self.compact else ' - '

    def visit_Mult(self, node: AST, dfltChaining: bool = True) -> str:
        """Return multiplication sign."""
        return '*' if self.compact else ' * '

    def visit_Div(self, node: AST, dfltChaining: bool = True) -> str:
        """Return division sign."""
        return '/' if self.compact else ' / '

    def visit_FloorDiv(self, node: AST, dfltChaining: bool = True) -> str:
        """Return floor division sign."""
        return '//' if self.compact else ' // '

    def visit_Mod(self, node: AST, dfltChaining: bool = True) -> str:
        """Return percentage sign."""
        return '%' if self.compact else ' % '

    def visit_Pow(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '**'."""
        return '**' if self.compact else ' ** '

    def visit_LShift(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '<<'."""
        return '<<' if self.compact else ' << '

    def visit_RShift(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '>>'."""
        return '>>' if self.compact else ' >> '

    def visit_BitOr(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '|'."""
        return '|' if self.compact else ' | '

    def visit_BitXor(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '^'."""
        return '^' if self.compact else ' ^ '

    def visit_BitAnd(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '&'."""
        return '&' if self.compact else ' & '

    # boolean operators

    def visit_BoolOp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s operator and operands as inlined expression."""
        op = node.op
        with self.op_man(op):
            src = self.visit(op).join([self.visit(node.values[0])] +
                                      [self.visit(val, dfltChaining=False)
                                       for val in node.values[1:]])
            return self.wrap_expr(src, dfltChaining)

    def visit_And(self, node: AST, dfltChaining: bool = True) -> str:
        """Return ' and '."""
        return ' and '

    def visit_Or(self, node: AST, dfltChaining: bool = True) -> str:
        """Return ' or '."""
        return ' or '

    # comparisons

    def visit_Compare(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s operators and operands as inlined expression."""
        # all comparison operators have the same precedence,
        # we just take the first one as representative
        first_op = node.ops[0]
        with self.op_man(first_op):
            cmps = [' '.join((self.visit(op),
                              self.visit(cmp, dfltChaining=False)))
                    for op, cmp in zip(node.ops, node.comparators)]
            src = ' '.join((self.visit(node.left), ' '.join(cmps)))
            return self.wrap_expr(src, dfltChaining)

    def visit_Eq(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '=='."""
        return '=='

    def visit_NotEq(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '!='."""
        return '!='

    def visit_Gt(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '>'."""
        return '>'

    def visit_GtE(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '>='."""
        return '>='

    def visit_Lt(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '<'."""
        return '<'

    def visit_LtE(self, node: AST, dfltChaining: bool = True) -> str:
        """Return '<='."""
        return '<='

    def visit_Is(self, node: AST, dfltChaining: bool = True) -> str:
        """Return 'is'."""
        return 'is'

    def visit_IsNot(self, node: AST, dfltChaining: bool = True) -> str:
        """Return 'is not'."""
        return 'is not'

    def visit_In(self, node: AST, dfltChaining: bool = True) -> str:
        """Return 'in'."""
        return 'in'

    def visit_NotIn(self, node: AST, dfltChaining: bool = True) -> str:
        """Return 'not in'."""
        return 'not in'

    # call

    def visit_keyword(self, node: AST, dfltChaining: bool = True) -> str:
        """Return representation of `node` as keyword arg."""
        arg = node.arg
        if arg is None:
            return f"**{self.visit(node.value)}"
        else:
            return f"{arg}={self.visit(node.value)}"

    def visit_Call(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as function call."""
        args = node.args
        try:
            kwds = node.keywords
        except AttributeError:
            kwds = []
        self.compact = True
        args_src = (self.visit(arg) for arg in args)
        kwds_src = (self.visit(kwd) for kwd in kwds)
        param_src = ', '.join(chain(args_src, kwds_src))
        src = f"{self.visit(node.func)}({param_src})"
        self.compact = False
        return src

    # lambda

    def visit_arguments(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as argument list."""
        args = node.args
        dflts = node.defaults
        vararg = node.vararg
        kwargs = node.kwonlyargs
        kwdflts = node.kw_defaults
        kwarg = node.kwarg
        self.compact = True
        n_args_without_dflt = len(args) - len(dflts)
        args_src = (arg.arg for arg in args[:n_args_without_dflt])
        dflts_src = (f"{arg.arg}={self.visit(dflt)}"
                     for arg, dflt in zip(args[n_args_without_dflt:], dflts))
        vararg_src = (f"*{vararg.arg}",) if vararg else ()
        kwargs_src = ((f"{kw.arg}={self.visit(dflt)}"
                       if dflt is not None else f"{kw.arg}")
                      for kw, dflt in zip(kwargs, kwdflts))
        kwarg_src = (f"**{kwarg.arg}",) if kwarg else ()
        src = ', '.join(chain(args_src, dflts_src, vararg_src,
                              kwargs_src, kwarg_src))
        self.compact = False
        return src

    def visit_Lambda(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as lambda expression."""
        with self.op_man(node):
            src = f"lambda {self.visit(node.args)}: {self.visit(node.body)}"
            return self.wrap_expr(src, dfltChaining)

    # … if … else …

    def visit_IfExp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as ... if ... else ... expression."""
        with self.op_man(node):
            src = " if ".join((self.visit(node.body, dfltChaining=False),
                               " else ".join((self.visit(node.test),
                                              self.visit(node.orelse)))))
            return self.wrap_expr(src, dfltChaining)

    # attribute access

    def visit_Attribute(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as attribute access."""
        return '.'.join((self.visit(node.value), node.attr))

    # subscripting

    def visit_Subscript(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as subscript access."""
        return f"{self.visit(node.value)}[{self.visit(node.slice)}]"

    def visit_Index(self, node: AST, dfltChaining: bool = True) -> str:
        """Return representation of `node`s value."""
        return self.visit(node.value)

    def visit_Slice(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as slice."""
        elems = [self.visit(node.lower), self.visit(node.upper)]
        if node.step is not None:
            elems.append(self.visit(node.step))
        return ':'.join(elems)

    def visit_ExtSlice(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as extended slice."""
        return ', '.join((self.visit(dim) for dim in node.dims))

    # comprehensions

    def visit_comprehension(self, node: AST,
                            dfltChaining: bool = True) -> str:
        """Return `node`s representation as comprehension."""
        target = node.target
        try:
            elts = target.elts          # we have a tuple of names
        except AttributeError:
            names = self.visit(target)
        else:
            names = ', '.join(self.visit(elt) for elt in elts)
        src = f"for {names} in {self.visit(node.iter)}"
        if node.ifs:
            src += f" {' '.join('if ' + self.visit(if_) for if_ in node.ifs)}"
        return src

    def visit_ListComp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as list comprehension."""
        return f"[{self.visit(node.elt)} " \
               f"{' '.join(self.visit(gen) for gen in node.generators)}]"

    def visit_SetComp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as set comprehension."""
        return f"{{{self.visit(node.elt)} " \
               f"{' '.join(self.visit(gen) for gen in node.generators)}}}"

    def visit_DictComp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as dict comprehension."""
        return f"{{{self.visit(node.key)}: {self.visit(node.value)} " \
               f"{' '.join(self.visit(gen) for gen in node.generators)}}}"

    def visit_GeneratorExp(self, node: AST, dfltChaining: bool = True) -> str:
        """Return `node`s representation as generator expression."""
        return f"({self.visit(node.elt)} " \
               f"{' '.join(self.visit(gen) for gen in node.generators)})"


def src2ast(src: str) -> Expression:
    """Return ast.Expression created from source code given in `src`."""
    try:
        return ast.parse(src, mode='eval')
    except SyntaxError:
        raise ValueError("Not a valid expression.") from None


def ast2src(expr: Expression) -> str:
    """Return source code equivalent to `expr`."""
    return SourceGenerator().visit(expr)


def names(expr: AST) -> Set[str]:
    """Names of globals in `expr`."""
    nodes = [node for node in ast.walk(expr) if isinstance(node, ast.Name)]
    loaded = {node.id for node in nodes if isinstance(node.ctx, ast.Load)}
    stored = {node.id for node in nodes if isinstance(node.ctx, ast.Store)}
    return loaded - stored


class _NameReplacer(ast.NodeTransformer):

    def __init__(self, old_name: str, new_name: str) -> None:
        self._old_name = old_name
        self._new_name = new_name

    def visit_Name(self, node: AST) -> Optional[AST]:
        if node.id == self._old_name:
            return ast.copy_location(ast.Name(id=self._new_name,
                                              ctx=ast.Load()), node)
        return node


def replace_name(expr: AST, old_name: str, new_name: str) -> AST:
    """Replace all Name nodes named `old_name` with nodes named `new_name`."""
    return _NameReplacer(old_name, new_name).visit(deepcopy(expr))


@singledispatch
def _negate(node: ast.expr) -> ast.expr:
    return ast.UnaryOp(ast.Not(), node)


@_negate.register(ast.UnaryOp)
def _negate_unary_op(node: ast.UnaryOp) -> ast.expr:
    if isinstance(node.op, ast.Not):
        return node.operand
    return _negate.dispatch(ast.expr)(node)


class _InversCmpOp:

    CPM2INVERS = {
        ast.Eq: ast.NotEq,
        ast.NotEq: ast.Eq,
        ast.Lt: ast.GtE,
        ast.LtE: ast.Gt,
        ast.Gt: ast.LtE,
        ast.GtE: ast.Lt,
        ast.Is: ast.IsNot,
        ast.IsNot: ast.Is,
        ast.In: ast.NotIn,
        ast.NotIn: ast.In,
    }

    def __new__(self, op: ast.cmpop) -> ast.cmpop:
        return self.CPM2INVERS[type(op)]()


@_negate.register(ast.Compare)
def _negate_compare(node: ast.Compare) -> ast.Compare:
    return ast.Compare(node.left,
                       [_InversCmpOp(op) for op in node.ops],
                       node.comparators)


@_negate.register(ast.BoolOp)
def _negate_bool_op(node: ast.BoolOp) -> ast.BoolOp:
    return _negate.dispatch(ast.expr)(node)


def Negation(expr: Expression) -> Expression:
    """Return expression which is the negation of `expr`."""
    expr = Expression(_negate(expr.body))
    return ast.fix_missing_locations(expr)


def Conjunction(expr1: Expression, expr2: Expression) -> Expression:
    """Return expression which is the conjunction of `expr1` and `expr2`."""
    expr = Expression(ast.BoolOp(ast.And(), [expr1.body, expr2.body]))
    return ast.fix_missing_locations(expr)


def Disjunction(expr1: Expression, expr2: Expression) -> Expression:
    """Return expression which is the disjunction of `expr1` and `expr2`."""
    expr = Expression(ast.BoolOp(ast.Or(), [expr1.body, expr2.body]))
    return ast.fix_missing_locations(expr)


def Contradiction(expr1: Expression, expr2: Expression) -> Expression:
    """Return expression which is the contradiction of `expr1` and `expr2`."""
    expr = Disjunction(Conjunction(expr1, Negation(expr2)),
                       Conjunction(Negation(expr1), expr2))
    return ast.fix_missing_locations(expr)
