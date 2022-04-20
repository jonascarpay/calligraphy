# calligraphy
[![calligraphy on Hackage](https://img.shields.io/hackage/v/calligraphy)](http://hackage.haskell.org/package/calligraphy)
[![calligraphy on Stackage Nightly](https://stackage.org/package/calligraphy/badge/nightly)](https://stackage.org/nightly/package/calligraphy)

![Calligraphy](./calligraphy.svg)

## Philosophy, a warning, and a roadmap

Writing, and especially maintaining, Haskell tooling really hard.
Haskell, let alone GHC, is underspecified, overcomplicated, and constantly changing.
There is an abundance of abandoned projects (think formatters, linters, editor plugins, IDEs, etc.) 
If you don't have a strategy, reality eventually catches up with you, and so too it is with `calligraphy`.
Working with HIE files instead of Haskell source files allows us to leverage GHC for parsing and type checking, which is nice, but HIE files are themselves nothing but untyped views into GHC's eldritch heart, and come with their own threats to sanity.
So, how _do_ we deal with this?

The goal of `calligraphy` is not to be accurate, it is to be as simple as possible while still being useful.
If we can get 80% accuracy for 20% of the effort, that's great, and if we can get 64% accuracy for 4% effort, that's even better.
That means that **`calligraphy` will sometimes be wrong.**
When this happens, please open a bug report, but know that there's a high chance it's not worth fixing.

Here's an example.
The typing logic is currently ~15 lines.
It works by, for every identifier, walking the type HIE gives us for that identifier, and adding an edge to every identifier it references.
This works perfectly in 95% of cases, but field accessors will, _only on GHC 9.2_ and _only sometimes_, not get the type of their parent data type.
That's annoying, but we have to draw a line somewhere, and `calligraphy` always errs towards simplicity and maintainability
We _could_ try to integrate other sources of type information, like type signatures, but for this project the 15 lines is more important than the 95%.

But, simple ain't easy.
There's a test suite that makes sure that a certain reference module produces the same graph across all GHC versions.
It took months, a lot of failed attempts, and a hefty dose of sunk cost fallacy to figure out a simple and robust way to make that test pass, and there's almost certainly still ways to make it simpler and more general.
I'm very open to questions and suggestions on how to do this, especially if you have experience with GHC/HIE files.

This also goes for the configuration system.
Currently `calligraphy` is configured through a ton of command line flags.
If you have ideas of how to do a more general/flexible way of formulating graph queries, please get in touch.
