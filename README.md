Diff for ClojureScript sequences

## Overview

Provides `diff` and `patch` functions for ClojureScript sequences where `(diff a b) -> x` and `(patch a x) -> b`.
Also provides `edit-distance` and `levenshtein-distance` functions for calculating the difference between two sequences.

This library is based on https://github.com/brentonashworth/clj-diff

Resources on diff:

* 'An O(NP) Sequence Comparison Algorithm' by Sun Wu, Udi Manber, Gene Myers and Web Miller
* 'An O(ND) Difference Algorithm and its Variations' by Eugene W. Myers
* http://neil.fraser.name/writing/diff/
* http://en.wikipedia.org/wiki/Levenshtein_distance

## Usage

Add the following dependency to your project.clj file:

    [com.wagjo/diff-cljs "0.2.0"]

## Public API

* `(wagjo.diff.core/diff a b)`
* `(wagjo.diff.core/patch a edit-script)`
* `(wagjo.diff.core/edit-distance a b)` or `(wagjo.diff.core/edit-distance edit-script)`
* `(wagjo.diff.core/levenshtein-distance a b)` or `(wagjo.diff.core/levenshtein-distance edit-script)`
* `(wagjo.diff.core/longest-common-subseq a b)`

## License

Copyright (C) 2011, 2012, 2013 Brenton Ashworth, Jozef Wagner.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0
(http://opensource.org/licenses/eclipse-1.0.php) which can be found
 in the file epl-v10.html at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound
by the terms of this license.

You must not remove this notice, or any other, from this software.
