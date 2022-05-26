# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Make `foldrDefault` and `foldlDefault` stack safe (#148)

## [v6.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v6.0.0) - 2022-04-27

Breaking changes:
- Migrate FFI to ES modules (#146 by @kl0tl and @JordanMartinez)
- Drop deprecated `foldMap1Default` (#147 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Narrow down unnecessarily imprecise type of `mapWithIndexArray` (#145)

## [v5.0.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v5.0.1) - 2021-04-20

Other improvements:
- Fix warnings revealed by v0.14.1 PureScript release (#135 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#120)
- Removed `fold1Default` and deprecated `foldMap1Default` (#128)

New features:
- Added `findMapWithIndex` (#119)
- Added `foldr1`, `foldl1`, `foldr1Default`, `foldl1Default`, `foldMap1DefaultR`, `foldMap1DefaultL` (#121, #128)
- Added `maximumBy` and `minimumBy` to `Data.Semigroup.Foldable` (#123) 
- Added `lookup` to `Data.Foldable`; this function previously lived in `Data.Tuple` in the `purescript-tuples` package (#131)

Bugfixes:

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#127)
- Added a CHANGELOG.md file and pull request template (#129, #130)
- Wrapped `traverseArrayImpl` IIFE in parentheses (#52)
- Added examples for `sequence` and `traverse` (#115)
- Changed `foldM` type signature to more closely match `foldl` (#111)
- This package now depends on the `purescript-const`, `purescript-either`, `purescript-functors`, `purescript-identity`, and `purescript-tuples` packages, and contains instances previously in those packages or the `purescript-bifunctors` or `purescript-profunctor` packages (#131)

## [v4.1.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v4.1.1) - 2018-11-23

Added examples to documentation for `intercalate` (@shmish111)

## [v4.1.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v4.1.0) - 2018-10-05

- Added missing exports for `minimum` and `maximum` from `Data.Semigroup.Foldable` (@paluh)

## [v4.0.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v4.0.1) - 2018-09-19

- Fixed totally wrong example in the documentation for `scanr`! (@ewaldgrusk)

## [v4.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v4.0.0) - 2018-05-23

- Updated for PureScript 0.12
- `traverse` for `Array` is now divide-and-conquer (@S11001001)
- `findWithIndex` returns both index and value of the found item (@mbid)
- Added `Traversable1` instances for `Dual` and `Multiplicative` (@matthewleon)
- Added `minimum` and `maximum` for `Foldable1` (@colehaus)
- Added functions for default `Foldable` implementations based on `FoldableWithIndex` (@matthewleon)
- Added `intercalate` and `intercalateMap` for `Foldable1` (@matthewleon)

## [v3.7.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.7.1) - 2018-01-10

- Fixed shadowed name warnings

## [v3.7.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.7.0) - 2018-01-09

- Added `indexl` and `indexr` for `Foldable`s (@safareli)

## [v3.6.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.6.1) - 2017-09-18

Fix test for `foldrDefault` (@tekerson)

## [v3.6.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.6.0) - 2017-08-18

Export `oneOfMap` (@natefaubion)

## [v3.5.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.5.0) - 2017-08-18

Add `oneOfMap` (@natefaubion)

## [v3.4.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.4.0) - 2017-07-10

Add `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex` classes (@mbid)

## [v3.3.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.3.1) - 2017-06-21

Fix `foldMapDefaultL` (@mbid)

## [v3.3.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.3.0) - 2017-06-04

Add `Foldable1` and `Traversable1` (@LukaJCB)

## [v3.2.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.2.0) - 2017-06-03

Add a generic `foldM` which works with any `Foldable`. (@clayrat)

## [v3.1.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.1.0) - 2017-06-03

Add `surroundMap` and `surround` (@LiamGoodacre)

## [v3.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v3.0.0) - 2017-03-26

- Updated for PureScript 0.11
- Added `null` for `Foldable` (@matthewleon)
- Added `length` for `Foldable` (@matthewleon)
- Eta-reduced some functions (@mlang)

## [v2.2.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v2.2.0) - 2017-02-06

- Added instances for the `Bifunctor` newtypes (@LiamGoodacre)

## [v2.1.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v2.1.0) - 2017-01-16

- Added left-only and right-only varieties of `bitraverse`/`bifor`

## [v2.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v2.0.0) - 2016-10-02

- Added `findMap` (@LiamGoodacre)
- Relaxed `and`, `or`, `any`, `all` to `HeytingAlgebra` from `BooleanAlgebra`
- Updated dependencies

## [v1.0.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.3](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v1.0.0-rc.3) - 2016-05-24

- Fixes for the upcoming psc 0.9.1

## [v1.0.0-rc.2](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v1.0.0-rc.2) - 2016-05-20

- Updated dependencies
- `find` now returns the first value matching the predicate

## [v1.0.0-rc.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v1.0.0-rc.1) - 2016-03-13

- Release candidate for the psc 0.8+ core libraries

## [v0.4.2](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.4.2) - 2015-11-30

- `maximum`, `minimum`, `maximumBy`, `minimumBy` (@hdgarrood)

## [v0.4.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.4.1) - 2015-11-01

- Removed unused imports

## [v0.4.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.4.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.4.0-rc.2](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.4.0-rc.2) - 2015-06-06

- Fixed behaviour of `foldr` for `Array`.

## [v0.4.0-rc.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.4.0-rc.1) - 2015-06-06

Initial release candidate of the library intended for the 0.7 compiler.

## [v0.3.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.3.1) - 2015-03-19

Updated docs

## [v0.3.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.3.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies
- Added `Foldable` and `Traversable` instances for the `Additive`, `Dual`, `First`, `Last`, and `Multiplicative` monoids

## [v0.2.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.2.1) - 2014-12-18

- Added `scanl` and `scanr` (@paf31)

## [v0.2.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.2.0) - 2014-12-17

- Removed instance for deprecated `Ref` type

## [v0.1.6](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.6) - 2014-12-11



## [v0.1.5](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.5) - 2014-12-02

- Added `mapAccumL` and `mapAccumR`.

## [v0.1.4](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.4) - 2014-10-24

- Added `intercalate` to `Data.Foldable` (@garyb)

## [v0.1.3](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.3) - 2014-07-14



## [v0.1.2](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.2) - 2014-06-14

- Now uses "proper" `Unit` type instead of `{}` (garyb)
- Removed implied `Functor` constraint from some types that are `Applicative` (garyb)

## [v0.1.1](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.1) - 2014-05-24

- Added `lookup` (paf31)

## [v0.1.0](https://github.com/purescript/purescript-foldable-traversable/releases/tag/v0.1.0) - 2014-04-25



