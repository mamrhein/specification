# -*- coding: utf-8 -*-
# ----------------------------------------------------------------------------
# Name:        specification
# Purpose:     Implementation of base classes for specifications
#
# Author:      Michael Amrhein (michael@adrhinum.de)
#
# Copyright:   (c) 2013 ff. Michael Amrhein
# License:     This program is part of a larger application. For license
#              details please read the file LICENSE.TXT provided together
#              with the application.
# ----------------------------------------------------------------------------


"""Implementation of base classes for specifications"""


# standard library imports
import ast
import inspect
from typing import Any, Callable, Mapping, Optional, Union

# local imports
from ._extd_ast_expr import (
    ast2src,
    src2ast,
    names,
    replace_name,
    Negation,
    Conjunction,
    Disjunction,
    Contradiction,
)


# TODO: more documentation
# TODO: create and use function for 'is_satisfied_by'
# TODO: optimizations by applying boolean algebra rules

# names of builtins
import builtins
BUILTIN_NAMES = set(dir(builtins))
del builtins


def _get_external_callers_namespace(frame):
    module = inspect.getmodule(frame)
    if module is None:
        return frame.f_locals                           # pragma: no cover
    namespace = {name: value for name, value in inspect.getmembers(module)
                 if not name.startswith('__')}
    namespace.update(frame.f_locals)
    return namespace


def _merge_namespaces(ns1: Mapping[str, Any], ns2: Mapping[str, Any]) \
        -> Mapping[str, Any]:
    common_names = ns1.keys() & ns2.keys()
    for name in common_names:
        if ns1[name] is not ns2[name]:
            raise ValueError(f"Name conflict: {name}")
    res_ns = dict(ns1)
    res_ns.update(ns2)
    return res_ns


class Specification:

    """A predicate that determines whether an object does or does not satisfy
    some criteria."""

    def __init__(self, expr: Union[str, ast.Expression],
                 candidate_name: Optional[str] = None,
                 namespace: Optional[Mapping[str, Any]] = None) -> None:
        if isinstance(expr, ast.Expression):
            self._ast_expr = ast_expr = expr
        else:
            self._ast_expr = ast_expr = src2ast(expr)
        self._src_expr = ast2src(ast_expr)
        var_names = names(ast_expr) - BUILTIN_NAMES
        if candidate_name is None:
            if len(var_names) == 1:
                self._candidate_name = var_names.pop()
            else:
                raise ValueError("`expr` contains more than one var name, "
                                 "but no `candidate_name` given.")
        else:
            try:
                var_names.remove(candidate_name)
            except KeyError:
                raise ValueError("Given `candidate_name` not in `expr`.") \
                    from None
            self._candidate_name = candidate_name
        if var_names:           # we need additional context
            if namespace is None:
                # get callers namespace
                frame = inspect.stack()[1].frame
                namespace = _get_external_callers_namespace(frame)
            # check needed names
            context = {}
            undef = []
            for name in var_names:
                try:
                    context[name] = namespace[name]
                except KeyError:
                    undef.append(name)
            if undef:
                msg = (f"Undefined name{'s' * min(len(undef) - 1, 1)}: "
                       f"{', '.join(undef)}")
                raise NameError(msg)
            self._context = context
        else:
            self._context = {}

    def is_satisfied_by(self, candidate: Any, **kwds: Any) -> bool:
        """Return True if `candidate` satisfies the specification."""
        candidate_name = self._candidate_name
        context = self._context
        if context:
            if candidate_name in kwds:
                raise ValueError(f"Candidate name '{candidate_name}' must "
                                 "not be given as keyword.")
            context.update(kwds)
        context[candidate_name] = candidate
        try:
            code = self._code
        except AttributeError:
            self._code = code = compile(self._ast_expr, '<str>', mode='eval')
        return eval(code, context)

    # for convenience:
    def __call__(self, candidate: Any, **kwds: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        return self.is_satisfied_by(candidate, **kwds)

    def __eq__(self, other: Any) -> bool:
        """self == other"""
        # pylint: disable=protected-access
        return (self.__class__ is other.__class__ and
                self._src_expr == other._src_expr)

    def __invert__(self) -> 'Specification':
        """~self => specification which is the negation of self."""
        return self.__class__(Negation(self._ast_expr), self._candidate_name,
                              self._context)

    def _combine(self, other: 'Specification',
                 op: Callable[[ast.Expression, ast.Expression],
                              ast.Expression]) -> 'Specification':
        self_candidate_name = self._candidate_name
        other_candidate_name = other._candidate_name
        namespace = _merge_namespaces(self._context, other._context)
        if self_candidate_name == other_candidate_name:
            return self.__class__(op(self._ast_expr, other._ast_expr),
                                  self_candidate_name, namespace)
        else:
            if self_candidate_name in other._context:
                if other_candidate_name in self._context:
                    raise ValueError(f"Name conflict: '{self_candidate_name}'"
                                     )
                else:
                    return self.__class__(op(replace_name(self._ast_expr,
                                                          self_candidate_name,
                                                          other_candidate_name
                                                          ),
                                             other._ast_expr),
                                          other_candidate_name, namespace)
            return self.__class__(op(self._ast_expr,
                                     replace_name(other._ast_expr,
                                                  other_candidate_name,
                                                  self_candidate_name)),
                                  self_candidate_name, namespace)

    def __and__(self, other: 'Specification') -> 'Specification':
        """self & other => specification which is the conjunction of self
        and other."""
        return self._combine(other, Conjunction)

    def __or__(self, other: 'Specification') -> 'Specification':
        """self | other => specification which is the disjunction of self
        and other."""
        return self._combine(other, Disjunction)

    def __xor__(self, other: 'Specification') -> 'Specification':
        """self ^ other => specification which is the contradiction of self
        and other."""
        return self._combine(other, Contradiction)

    def __hash__(self) -> int:
        """hash(self)"""
        return hash((self.__class__, self._src_expr))

    def __copy__(self):
        """Return `self` (instances of Specification are immutable)."""
        return self                                     # pragma: no cover

    __deepcopy = __copy__

    def __str__(self) -> str:
        """str(self)"""
        return f"<{self._candidate_name}: {self._src_expr}>"

    def __repr__(self) -> str:
        """repr(self)"""
        if self._context:
            return f"{self.__class__.__name__}('{self._src_expr}'" \
                   f", candidate_name='{self._candidate_name}')"
        else:
            return f"{self.__class__.__name__}('{self._src_expr}')"
