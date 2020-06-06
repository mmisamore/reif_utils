# Description

This module collects a variety of utility predicates intended for use with Ulrich Neumerkel's
`reif.pl` library. Please see [the paper](https://arxiv.org/abs/1607.01590) by Neumerkel and Kral
for background. It is intended for use with SWI Prolog; please see LICENSE for terms of use.

Most of the implementations here are trivial due to standing on the shoulders of Markus Triska's
excellent `clpfd` library for SWI Prolog. The real value provided here is in the tests: every
provided predicate is tested in all possible modes, with the goal of eliminating all unnecessary 
choice points. So far this adds up to over 220 tests.

