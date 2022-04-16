# calligraphy
[![calligraphy on hackage](https://img.shields.io/hackage/v/calligraphy)](http://hackage.haskell.org/package/calligraphy)
[![calligraphy on Stackage Nightly](https://stackage.org/package/calligraphy/badge/nightly)](https://stackage.org/nightly/package/calligraphy)

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)

## Philosophy, and a warning

There is an abundance of abandoned Haskell tooling projects (think formatters, linters, editor plugins, IDEs, etc.)
The reason for this is that Haskell, let alone GHC, is poorly specified, incredibly complicated, and changing constantly.
If you don't have a strategy (like being backed by Tweag, or your name being by Neil Mitchell) for addressing this, reality eventually catches up with you.

So, too, it is with `calligraphy`.
Working with HIE files instead of Haskell source files allows us to leverage GHC for parsing and type checking, which is nice, but HIE files are themselves nothing but views into GHC's eldritch heart, and dealing with them comes with its own threats to sanity.
So, how _do_ we deal with this?

The goal of `calligraphy` is not to be accurate, it is to be useful and robust to changes.

simplicity, maintainability, robustness > stablity
robustness through simplicity

Change between GHC version

maintainability > stability
we change as little as possible to account for GHC versions
If that causes idiosyncrasies, so be it

Reference
  not necessarily trying to be constant
  just keep an eye on changes

