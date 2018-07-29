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


class Specification:

    """A predicate that determines whether an object does or does not satisfy
    some criteria."""

    def __init__(self, expr: Union[str, ast.Expression],
                 candidate_name: Optional[str] = None) -> None:
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
        self._var_names = var_names

    def is_satisfied_by(self, candidate: Any,
                        context: Optional[Mapping[str, Any]] = None,
                        **kwds: Any) -> bool:
        """Return True if `candidate` satisfies the specification."""
        context = context or {}
        context.update(kwds)
        candidate_name = self._candidate_name
        if candidate_name in context:
            raise ValueError(f"Candidate name '{candidate_name}' must not be "
                             "an element of `context` or given as keyword.")
        context[candidate_name] = candidate
        try:
            code = self._code
        except AttributeError:
            self._code = code = compile(self._ast_expr, '<str>', mode='eval')
        return eval(code, context)

    # for convenience:
    def __call__(self, candidate: Any,
                 context: Optional[Mapping[str, Any]] = None,
                 **kwds: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        return self.is_satisfied_by(candidate, context, **kwds)

    def __eq__(self, other: Any) -> bool:
        """self == other"""
        # pylint: disable=protected-access
        return (self.__class__ is other.__class__ and
                self._src_expr == other._src_expr)

    def __invert__(self) -> 'Specification':
        """~self => specification which is the negation of self."""
        return self.__class__(Negation(self._ast_expr), self._candidate_name)

    def _combine(self, other: 'Specification',
                 op: Callable[[ast.Expression, ast.Expression],
                              ast.Expression]) -> 'Specification':
        self_candidate_name = self._candidate_name
        other_candidate_name = other._candidate_name
        if self_candidate_name == other_candidate_name:
            return self.__class__(op(self._ast_expr, other._ast_expr),
                                  self_candidate_name)
        else:
            if self_candidate_name in other._var_names:
                if other_candidate_name in self._var_names:
                    raise ValueError(f"Name conflict: '{self_candidate_name}'"
                                     )
                else:
                    return self.__class__(op(replace_name(self._ast_expr,
                                                          self_candidate_name,
                                                          other_candidate_name
                                                          ),
                                             other._ast_expr),
                                          other_candidate_name)
            return self.__class__(op(self._ast_expr,
                                     replace_name(other._ast_expr,
                                                  other_candidate_name,
                                                  self_candidate_name)),
                                  self_candidate_name)

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
        return self

    __deepcopy = __copy__

    def __str__(self) -> str:
        """str(self)"""
        return f"<{self._candidate_name}: {self._src_expr}>"

    def __repr__(self) -> str:
        """repr(self)"""
        if self._var_names:
            return f"{self.__class__.__name__}('{self._src_expr}'" \
                   f", candidate_name='{self._candidate_name}')"
        else:
            return f"{self.__class__.__name__}('{self._src_expr}')"
