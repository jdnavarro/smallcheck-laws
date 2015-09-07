# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [0.3] - 2015-09-07
### Added
- *Exhaustive* property testing using `Series` product. Previous
  properties renamed appending `Sum`.

### Changed
- Rename `mconcat` `Monoid` property.

## [0.2] - 2015-09-04
### Removed
- Move `Tasty` modules to a separate package
  [`tasty-laws`](https://hackage.haskell.org/package/tasty-laws).

## [0.1] - 2015-08-05
### Added
- Functor laws.
- Applicative laws.
- Monoid Laws.
- Monad laws.

[0.3]: https://github.com/jdnavarro/smallcheck-laws/compare/v0.2...v0.3
[0.2]: https://github.com/jdnavarro/smallcheck-laws/compare/v0.1...v0.2
[0.1]: https://github.com/jdnavarro/smallcheck-laws/compare/bf1caa...v0.1
