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
# $Source$
# $Revision$


"""Implementation of base classes for specifications"""


# standard library imports
from abc import ABCMeta, abstractmethod
from functools import reduce
import operator
from typing import Any, Callable, Iterable, Tuple, Union

# third-party imports
from ivalutils import Interval, ClosedInterval, InvalidInterval


# TODO: more documentation
# TODO: factory function 'from_expression'? Use case ?
# TODO: create and use function for 'is_satisfied_by'
# TODO: more kind of specifications (in, not in, like?, regexp?)
# TODO: optimizations by applying boolean algebra rules


class Specification(metaclass=ABCMeta):

    """A predicate that determines whether an object does or does not satisfy
    some criteria."""

    __slots__ = ()

    @abstractmethod
    def is_satisfied_by(self, candidate: Any) -> bool:
        """Return True if `candidate` satisfies the specification."""

    # for convenience:
    def __call__(self, candidate: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        return self.is_satisfied_by(candidate)

    # should be overwritten in subclasses for optimization
    def _negate(self) -> 'Specification':
        """~self"""
        return NegatedSpecification(self)       # pragma: no cover

    @abstractmethod
    def __eq__(self, other: Any) -> bool:
        """self == other"""

    def __and__(self, other: 'Specification') -> 'Specification':
        """self & other => specification which is the conjunction of self
        and other."""
        return CompositeSpecification(operator.and_, self, other)

    def __or__(self, other: 'Specification') -> 'Specification':
        """self | other => specification which is the disjunction of self
        and other."""
        return CompositeSpecification(operator.or_, self, other)

    def __xor__(self, other: 'Specification') -> 'Specification':
        """self ^ other => specification which is the contradiction of self
        and other."""
        return CompositeSpecification(operator.xor, self, other)

    @abstractmethod
    def __hash__(self) -> int:
        """hash(self)"""

    def __invert__(self) -> 'Specification':
        """~self => specification which is the negation of self."""
        return self._negate()

    @abstractmethod
    def term(self, candidate_name: str) -> str:
        """Return an expression (as string) equivalent to `self`."""
        pass                                    # pragma: no cover

    def __repr__(self) -> str:
        """repr(self)"""
        return f"<x: {self.term('x')}>"


class BaseSpecification(Specification):

    """Abstract base class for simple (i. e. non-composite) specifications."""

    __slots__ = ()

    @abstractmethod
    def _params(self):
        pass                                    # pragma: no cover

    def __hash__(self) -> int:
        """hash(self)"""
        return hash((self.__class__, self._params()))

    def __eq__(self, other: Any) -> bool:
        """self == other"""
        # pylint: disable=protected-access
        return (self.__class__ is other.__class__ and
                self._params() == other._params())


class ValueSpecification(BaseSpecification):

    """Specification comparing an attribute value with a given value.

    It is satisfied if for a given candidate, operator, attr_name and value
    operator(candidate.attr_name, value) returns True."""

    __slots__ = ('_attr_name', '_attr', '_operator', '_value')

    _mapOp2Invers = {operator.eq: operator.ne,
                     operator.ne: operator.eq,
                     operator.lt: operator.ge,
                     operator.ge: operator.lt,
                     operator.gt: operator.le,
                     operator.le: operator.gt,
                     operator.is_: operator.is_not,
                     operator.is_not: operator.is_}

    def __init__(self, attr_name: str, op: Callable[[Any, Any], bool],
                 value: Any) -> None:
        """Create a Specification which is satisfied for a given object if
        op(object.attr_name, value) returns True."""
        super().__init__()
        self._attr_name = attr_name
        self._attr = operator.attrgetter(attr_name)
        self._operator = op
        self._value = value

    def _params(self) -> Tuple[str, Callable[[Any, Any], bool], Any]:
        return (self._attr_name, self._operator, self._value)

    def is_satisfied_by(self, candidate: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        try:
            attr_val = self._attr(candidate)
        except AttributeError:
            return False
        return self._operator(attr_val, self._value)

    def _negate(self) -> Specification:
        """~self"""
        try:
            inv_op = self._mapOp2Invers[self._operator]
        except KeyError:
            return NegatedSpecification(self)
        return self.__class__(self._attr_name, inv_op, self._value)

    def term(self, candidate_name: str) -> str:
        """Return an expression (as string) equivalent to `self`."""
        return (f"{candidate_name}.{self._attr_name} "
                f"{self._operator.__name__} {self._value}")


class IntervalSpecification(BaseSpecification):

    """Specification checking an attribute value against a given interval.

    It is satisfied if for a given candidate, attr_name and interval
    candidate.attr_name in interval is True."""

    __slots__ = ('_attr_name', '_attr', '_ival')

    def __init__(self, attr_name: str,
                 ival: Union[Interval, Tuple[Any, Any]]) -> None:
        """Create a specification which is satisfied for a given object if
        object.attr_name in ival is True.

        ival must be given as Interval instance or as a tuple of two values.
        In the latter case a closed interval is created, taking the first
        value as minimum and the second as maximum."""
        super().__init__()
        self._attr_name = attr_name
        self._attr = operator.attrgetter(attr_name)
        if isinstance(ival, Interval):
            self._ival = ival
        elif isinstance(ival, tuple):
            try:
                minimum, maximum = ival
            except ValueError:
                raise ValueError(
                    "2 values required as interval tuple, %s given"
                    % (len(ival))) from None
            self._ival = ClosedInterval(minimum, maximum)
        else:
            raise TypeError("ival must be given as Interval instance or as a "
                            "tuple of two values.")

    def _params(self) -> Tuple[str, Interval]:
        return (self._attr_name, self._ival)

    def is_satisfied_by(self, candidate: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        try:
            attr_val = self._attr(candidate)
        except AttributeError:
            return False
        return attr_val in self._ival

    def _negate(self) -> Specification:
        """~self"""
        specs = []
        adjacent_limit = self._ival.lower_limit.adjacent_limit()
        if adjacent_limit:
            ival = Interval(upper_limit=adjacent_limit)
            specs.append(IntervalSpecification(self._attr_name, ival))
        adjacent_limit = self._ival.upper_limit.adjacent_limit()
        if adjacent_limit:
            ival = Interval(lower_limit=adjacent_limit)
            specs.append(IntervalSpecification(self._attr_name, ival))
        nspecs = len(specs)
        if nspecs == 2:
            return CompositeSpecification(operator.or_, *specs)
        elif nspecs == 1:
            return specs[0]
        else:
            raise InvalidInterval("(-inf .. +inf) can not be inverted.")

    def term(self, candidate_name: str) -> str:
        """Return an expression (as string) equivalent to `self`."""
        return f"{candidate_name}.{self._attr_name} in {self._ival}"


class NegatedSpecification(Specification):

    """Specification which is the negation of another specification."""

    __slots__ = ('_spec',)

    def __init__(self, spec: Specification) -> None:
        assert isinstance(spec, Specification)
        self._spec = spec

    def __hash__(self) -> int:
        """hash(self)"""
        return hash((self.__class__, self._spec))

    def __eq__(self, other: Any) -> bool:
        """self == other"""
        # pylint: disable=protected-access
        return (isinstance(other, self.__class__) and
                self._spec == other._spec)

    def is_satisfied_by(self, candidate: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        return not self._spec(candidate)

    def _negate(self) -> Specification:
        """~self"""
        return self._spec

    def term(self, candidate_name: str) -> str:
        """Return an expression (as string) equivalent to `self`."""
        return f"not ({self._spec.term(candidate_name)})"


def xnor(a, b):
    "Same as not a ^ b."
    return not a ^ b


def contr(it: Iterable) -> bool:
    """Return it[0] ^ it[1] ^ ... ^ it[n]"""
    return reduce(operator.xor, it)


def ncontr(it: Iterable) -> bool:
    """Return not (it[0] ^ it[1] ^ ... ^ it[n])"""
    return reduce(xnor, it)


class CompositeSpecification(Specification):

    """Specification which combines other specifications."""

    __slots__ = ('_op', '_specs', '_canonical_order')

    _map_op2neg = {operator.and_: operator.or_,
                   operator.or_: operator.and_,
                   operator.xor: xnor,
                   xnor: operator.xor}

    _map_op2iter = {operator.and_: all,
                    operator.or_: any,
                    operator.xor: contr,
                    xnor: ncontr}

    def __init__(self, op: Callable[[Iterable], bool], *specs: Specification):
        assert op in self._map_op2iter
        assert len(specs) > 1
        assert all(isinstance(spec, Specification) for spec in specs)
        # TODO: reduce nesting level if possible
        self._op = op
        self._specs = tuple(specs)
        self._canonical_order = tuple(sorted(specs, key=hash))

    def __hash__(self) -> int:
        """hash(self)"""
        return hash((self.__class__, self._op, self._canonical_order))

    def __eq__(self, other: Any) -> bool:
        """self == other"""
        # pylint: disable=protected-access
        return (isinstance(other, self.__class__) and
                self._op == other._op and
                self._canonical_order == other._canonical_order)

    def is_satisfied_by(self, candidate: Any) -> bool:
        """Return True if candidate satisfies the specification."""
        return self._map_op2iter[self._op]((spec(candidate)
                                            for spec in self._specs))

    def _negate(self) -> Specification:
        """~self"""
        return self.__class__(self._map_op2neg[self._op],
                              *(~spec for spec in self._specs))

    def term(self, candidate_name: str) -> str:
        """Return an expression (as string) equivalent to `self`."""
        templ = "{}(" + "{}, " * (len(self._specs) - 1) + "{})"
        return templ.format(self._map_op2iter[self._op].__name__,
                            *(spec.term(candidate_name)
                              for spec in self._specs))
