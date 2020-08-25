# Description

This module collects a variety of utility predicates intended for use with Ulrich Neumerkel's
`reif.pl` library. Please see [the paper](https://arxiv.org/abs/1607.01590) by Neumerkel and Kral
for background. It is intended for use with SWI Prolog; please see LICENSE for terms of use.

The implementations provided here fall into the following groups:
* Pure, reified integer comparison predicates (leveraging Markus Triska's excellent `clpfd` library 
  for SWI Prolog) 
* Impure, reified term equivalence and term-ordering predicates 
* Pure, reified term-ordering predicates

The real value provided here is in the tests: every provided predicate is implemented and tested 
in all possible computable modes, with the goal of eliminating all unnecessary choice points.

To install this module via `pack_install`, run
```
pack_install('https://github.com/mmisamore/reif_utils/archive/1.0.0.zip').
```

