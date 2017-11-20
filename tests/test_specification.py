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
# $Source$
# $Revision$


"""Test driver for module specification."""


# standard library imports
from abc import ABC
from itertools import chain, combinations
import operator
import unittest

# third-party imports
from ivalutils import (
    Interval, ClosedInterval, LowerOpenInterval, UpperOpenInterval,
    IntervalChain, InvalidInterval
)
from specification import (
    Specification, NegatedSpecification, CompositeSpecification,
    IntervalSpecification, ValueSpecification, xnor
)


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


class TestObj3:

    def __init__(self, a):
        self.a = a


class TestBaseSpecifications(unittest.TestCase):

    def testValueSpec(self):
        params = ('b', operator.eq, 2)
        spec = ValueSpecification(*params)
        self.assertTrue(isinstance(spec, Specification))
        self.assertEqual(spec._params(), params)
        # nested attribute
        params = ('b.x.y', operator.eq, 2)
        spec = ValueSpecification(*params)
        self.assertTrue(isinstance(spec, Specification))
        self.assertEqual(spec._params(), params)

    def testIntervalSpec(self):
        params = ('b', ClosedInterval(2, 7))
        spec = IntervalSpecification(*params)
        self.assertTrue(isinstance(spec, Specification))
        self.assertEqual(spec._params(), params)
        spec = IntervalSpecification('b', (2, 7))
        self.assertTrue(isinstance(spec, Specification))
        self.assertEqual(spec._params(), params)
        # nested attribute
        params = ('b.xyz', ClosedInterval(2, 7))
        spec = IntervalSpecification(*params)
        self.assertTrue(isinstance(spec, Specification))
        # wrong number of values
        self.assertRaises(ValueError, IntervalSpecification, 'b', (2, 7, 3))
        # wrong type of interval
        self.assertRaises(TypeError, IntervalSpecification, 'b', {2, 7})


class TestComposition(unittest.TestCase):

    def setUp(self):
        self.vspec1 = ValueSpecification('b', operator.eq, 2)
        self.vspec2 = ValueSpecification('ab', operator.gt, 20)
        self.vspec3 = ValueSpecification('a', operator.eq, 2)
        self.ispec1 = IntervalSpecification('a', ClosedInterval(2, 7))
        self.ispec2 = IntervalSpecification('ab', (-12, -7))
        self.ispec3 = IntervalSpecification('ab', (-112, 7))

    def testCompositeSpec(self):
        specs = (self.vspec1, self.vspec2)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = CompositeSpecification(op, *specs)
            for idx, spec in enumerate(specs):
                self.assertIs(spec, cspec._specs[idx])
        specs = (self.ispec2, self.vspec3, self.ispec1)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec = CompositeSpecification(op, *specs)
            for idx, spec in enumerate(specs):
                self.assertIs(spec, cspec._specs[idx])
        # invalid operator
        self.assertRaises(AssertionError, CompositeSpecification,
                          operator.eq, self.vspec1, self.vspec2)
        # invalid numbers of specs
        self.assertRaises(AssertionError, CompositeSpecification,
                          operator.or_)
        self.assertRaises(AssertionError, CompositeSpecification,
                          operator.or_, self.vspec1)
        # invalid argument
        self.assertRaises(AssertionError, CompositeSpecification,
                          operator.or_, self.vspec1, self.vspec1, 'abc')
        # reduced nesting
        specs1 = (self.vspec1, self.vspec2)
        specs2 = (self.ispec2, self.vspec3, self.ispec1)
        xcspec = CompositeSpecification(xnor, *specs1)
        for op in (operator.and_, operator.or_, operator.xor):
            cspec1 = CompositeSpecification(op, *specs1)
            cspec2 = CompositeSpecification(op, *specs2)
            ncspec = CompositeSpecification(op, cspec1, self.ispec3, xcspec,
                                            cspec2)
            for idx, spec in enumerate(chain(specs1, (self.ispec3, xcspec),
                                             specs2)):
                self.assertIs(spec, ncspec._specs[idx])


class TestNegation(unittest.TestCase):

    def testValueSpec(self):
        for op1, op2 in [(operator.eq, operator.ne),
                         (operator.lt, operator.ge),
                         (operator.gt, operator.le),
                         (operator.is_, operator.is_not)]:
            spec1 = ValueSpecification('a', op1, 1)
            spec2 = ValueSpecification('a', op2, 1)
            neg_spec1 = ~spec1
            neg_spec2 = ~spec2
            self.assertTrue(isinstance(neg_spec1, Specification))
            self.assertTrue(isinstance(neg_spec2, Specification))
            self.assertEqual(spec1, neg_spec2)
            self.assertEqual(spec2, neg_spec1)
            self.assertEqual(spec1, ~neg_spec1)
            self.assertEqual(spec2, ~neg_spec2)
        # non-predefined operator:
        spec = ValueSpecification('a', lambda x, y: x == y, 1)
        neg_spec = ~spec
        self.assertTrue(isinstance(neg_spec, Specification))
        self.assertEqual(spec, ~neg_spec)

    def testIntervalSpec(self):
        spec = IntervalSpecification('a', (1, 7))
        neg_spec = ~spec
        self.assertTrue(isinstance(neg_spec, Specification))
        spec = IntervalSpecification('ab', LowerOpenInterval(-12))
        neg_spec = ~spec
        self.assertTrue(isinstance(neg_spec, Specification))
        spec = IntervalSpecification('ab', UpperOpenInterval(-12))
        neg_spec = ~spec
        self.assertTrue(isinstance(neg_spec, Specification))
        spec = IntervalSpecification('a', Interval())
        self.assertRaises(InvalidInterval, operator.invert, spec)

    def testCompositeSpec(self):
        vspec1 = ValueSpecification('b', operator.eq, 2)
        vspec2 = ValueSpecification('ab', operator.gt, 20)
        ispec1 = IntervalSpecification('a',
                                            ClosedInterval(2, 7))
        ispec2 = IntervalSpecification('ab', (-12, -7))
        for op in (operator.and_, operator.or_, operator.xor):
            for spec1, spec2 in combinations((vspec1, vspec2,
                                              ispec1, ispec2), 2):
                spec = op(spec1, spec2)
                neg_spec = ~spec
                self.assertTrue(isinstance(neg_spec, Specification))


class TestImmutable(unittest.TestCase):

    def testImmutable(self):
        spec = ValueSpecification('b', lambda x, y: x == y, 2)
        self.assertRaises(AttributeError, setattr, spec, 'x', 'xx')
        spec = ~spec
        self.assertRaises(AttributeError, setattr, spec, 'x', 'xx')
        spec = IntervalSpecification('a', (1, 7))
        self.assertRaises(AttributeError, setattr, spec, 'x', 'xx')
        spec = spec & ValueSpecification('b', operator.eq, 2)


class TestIsSatisfiedBy(unittest.TestCase):

    def testValueSpec(self):
        val = 5
        tObj = TestObj1(b=val)
        for op1, op2 in [(operator.eq, operator.ne),
                         (operator.lt, operator.ge),
                         (operator.gt, operator.le),
                         (operator.is_, operator.is_not)]:
            spec1 = ValueSpecification('b', op1, val)
            spec2 = ValueSpecification('b', op2, val)
            self.assertEqual(spec1.is_satisfied_by(tObj), op1(tObj.b, val))
            self.assertFalse(spec1.is_satisfied_by(tObj) and
                             spec2.is_satisfied_by(tObj))
            spec1 = ValueSpecification('b', op1, -val)
            spec2 = ValueSpecification('b', op2, -val)
            self.assertEqual(spec1.is_satisfied_by(tObj), op1(tObj.b, -val))
            self.assertFalse(spec1.is_satisfied_by(tObj) and
                             spec2.is_satisfied_by(tObj))
        # compare instance of subclass
        tObj = TestObj2(b=val)
        spec = ValueSpecification('b', operator.eq, val)
        self.assertTrue(spec.is_satisfied_by(tObj))
        spec = ValueSpecification('b', operator.lt, val)
        self.assertFalse(spec.is_satisfied_by(tObj))
        # object with incompatible interface never satisfies spec
        tObj = TestObj3(val)
        for op in (operator.eq, operator.ne, operator.ge, operator.gt):
            spec = ValueSpecification('b', op, val)
            self.assertFalse(spec.is_satisfied_by(tObj))

    def testIntervalSpec(self):
        a, b = 5, 7
        ab = a * b
        tObj = TestObj2(a, b)
        for ival in [ClosedInterval(0, 10), ClosedInterval(20, 50),
                     LowerOpenInterval(-12), LowerOpenInterval(72),
                     UpperOpenInterval(-12), UpperOpenInterval(72)]:
            spec = IntervalSpecification('ab', ival)
            self.assertEqual(spec.is_satisfied_by(tObj), ab in ival)
        limits = [3, ab]
        for lower_closed in (True, False):
            for add_lower_inf in (True, False):
                for add_upper_inf in (True, False):
                    ic = IntervalChain(limits, lower_closed=lower_closed,
                                       add_lower_inf=add_lower_inf,
                                       add_upper_inf=add_upper_inf)
                    for ival in ic:
                        spec = IntervalSpecification('ab', ival)
                        self.assertEqual(spec.is_satisfied_by(tObj),
                                         ab in ival)
        # compare instance of subclass
        val = 23
        tObj = TestObj2(b=val)
        spec = IntervalSpecification('b', (20, 50))
        self.assertTrue(spec.is_satisfied_by(tObj))
        spec = IntervalSpecification('b', (3, 17))
        self.assertFalse(spec.is_satisfied_by(tObj))
        # object with incompatible interface never satisfies spec
        tObj = TestObj3(val)
        for min_max in ((0, 5), (20, 25), (67, 70)):
            spec = IntervalSpecification('b', min_max)
            self.assertFalse(spec.is_satisfied_by(tObj))

    def testCompositeSpec(self):
        a, b = 5, 7
        ab = a * b
        tObj = TestObj2(a, b)
        vspec1 = ValueSpecification('b', operator.eq, b)
        vspec2 = ValueSpecification('ab', operator.lt, ab)
        ispec = IntervalSpecification('a', (1, 7))
        cspec = CompositeSpecification(operator.and_, vspec1, vspec2, ispec)
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = CompositeSpecification(operator.or_, vspec1, vspec2, ispec)
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = CompositeSpecification(operator.xor, vspec1, vspec2, ispec)
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 & vspec2) & ispec
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 & vspec2) | ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 | vspec2) & ~ispec
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (~vspec1 | vspec2) | ~ispec
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 & ~vspec2) & ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 & ~vspec2) ^ ispec
        self.assertFalse(cspec.is_satisfied_by(tObj))
        cspec = (vspec1 ^ vspec2) & ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = ~(vspec1 ^ vspec2) ^ ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))
        cspec = ~CompositeSpecification(operator.xor, vspec1, vspec2, ispec)
        self.assertTrue(cspec.is_satisfied_by(tObj))

    def testNestedproperty(self):
        a, b = 5, 7
        ab = a * b
        tObj = TestObj1(a=3, b=TestObj2(a, b))
        vspec = ValueSpecification('b.ab', lambda x, y: x == y, ab)
        self.assertTrue(vspec.is_satisfied_by(tObj))
        vspec = ~ValueSpecification('b.ab',
                                    lambda x, y: x != y, ab)
        self.assertTrue(vspec.is_satisfied_by(tObj))
        ispec = IntervalSpecification('b.a', (1, 7))
        self.assertTrue(ispec.is_satisfied_by(tObj))
        cspec = vspec & ispec
        self.assertTrue(cspec.is_satisfied_by(tObj))
        # undefined nested attribute => result is False
        vspec = ValueSpecification('b.x', operator.eq, ab)
        self.assertFalse(vspec.is_satisfied_by(tObj))
        ispec = IntervalSpecification('b.x', (1, 7))
        self.assertFalse(ispec.is_satisfied_by(tObj))


class TestRepr(unittest.TestCase):

    def testRepr(self):

        def op(x, y):
            return x == y                               # pragma: no cover

        vspec = ValueSpecification('a', op, 1)
        self.assertEqual(repr(vspec), '<x: x.a op 1>')
        self.assertEqual(repr(~vspec), '<x: not (x.a op 1)>')
        vspec = ValueSpecification('a', operator.eq, 1)
        self.assertEqual(repr(vspec), '<x: x.a eq 1>')
        ispec = IntervalSpecification('a', (0, 6))
        self.assertEqual(repr(ispec), '<x: x.a in [0 .. 6]>')
        cspec = ispec & vspec
        self.assertEqual(repr(cspec), '<x: all(x.a in [0 .. 6], x.a eq 1)>')
        vspec1 = ValueSpecification('b', operator.eq, 7)
        vspec2 = ValueSpecification('ab', operator.lt, 35)
        ispec = IntervalSpecification('a', (1, 7))
        cspec = CompositeSpecification(operator.xor, vspec1, vspec2, ispec)
        self.assertEqual(repr(cspec),
                         '<x: contr(x.b eq 7, x.ab lt 35, x.a in [1 .. 7])>')
        cspec = ~cspec
        self.assertEqual(repr(cspec),
                         '<x: ncontr(x.b ne 7, x.ab ge 35, '
                         'any(x.a in (-inf .. 1), x.a in (7 .. +inf)))>')


class TestEqualityAndHash(unittest.TestCase):

    def test_eq_and_hash(self):

        def op(x, y):
            return x == y                               # pragma: no cover

        vspec1 = ValueSpecification('a', op, 1)
        vspec2 = ValueSpecification('a', op, 1)
        vspec3 = ValueSpecification('a', operator.eq, 1)
        self.assertEqual(vspec1, vspec2)
        self.assertEqual(hash(vspec1), hash(vspec2))
        self.assertNotEqual(vspec1, vspec3)
        self.assertNotEqual(hash(vspec1), hash(vspec3))
        negvspec1 = NegatedSpecification(vspec1)
        negvspec2 = NegatedSpecification(vspec2)
        negvspec3 = NegatedSpecification(vspec3)
        self.assertEqual(negvspec1, negvspec2)
        self.assertEqual(hash(negvspec1), hash(negvspec2))
        self.assertNotEqual(negvspec1, negvspec3)
        self.assertNotEqual(hash(negvspec1), hash(negvspec3))
        self.assertNotEqual(vspec1, negvspec1)
        self.assertNotEqual(hash(vspec1), hash(negvspec1))
        ispec1 = IntervalSpecification('a', (0, 6))
        ispec2 = IntervalSpecification('a', (0, 6))
        ispec3 = IntervalSpecification('a', (0, 5))
        self.assertEqual(ispec1, ispec2)
        self.assertEqual(hash(ispec1), hash(ispec2))
        self.assertNotEqual(ispec1, ispec3)
        self.assertNotEqual(hash(ispec1), hash(ispec3))
        self.assertNotEqual(vspec1, ispec1)
        self.assertNotEqual(hash(vspec1), hash(ispec1))
        cspec1 = ispec1 & vspec1
        cspec2 = ispec2 & vspec2
        cspec3 = ispec3 & vspec3
        self.assertEqual(cspec1, cspec2)
        self.assertEqual(hash(cspec1), hash(cspec2))
        self.assertNotEqual(cspec1, cspec3)
        self.assertNotEqual(hash(cspec1), hash(cspec3))
        self.assertNotEqual(vspec1, cspec1)
        self.assertNotEqual(hash(vspec1), hash(cspec1))
        # test canonical order:
        cspec1 = CompositeSpecification(operator.and_, vspec1, vspec3, ispec1,
                                        ispec3)
        cspec2 = CompositeSpecification(operator.and_, vspec1, ispec3, vspec3,
                                        ispec1)
        self.assertEqual(cspec1, cspec2)
        self.assertEqual(hash(cspec1), hash(cspec2))
