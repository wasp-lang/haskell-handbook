# Cabal & Stack

Simplest way to write some code in Haskell and get going is to write it directly in the ghci REPL, or to write a single-file script and then load it and execute it with ghci. You could also directly compile your .hs files with ghc.

However, if you are building anything a bit more complex, you will want to organize it into a project/package that builds into an executable and/or library and very likely depends on other Haskell packages.

The standard/official solution for this is Cabal, and then there are popular alternatives like Stack (or more general Nix, Bazel, Shake, ...) that build on top of Cabal in order to provide different experience.

[Cabal](https://haskell.org/cabal) and [Stack](https://haskellstack.org) are the most popular ones so we will shortly introduce both of them here and talk about the differences between them.

## Cabal

[Cabal](https://haskell.org/cabal) is the standard package system for Haskell software.

It helps you configure (e.g. define version, dependencies, ...), build (compile) and distribute (e.g. publish as a library) your Haskell project - basically all of the stuff you need to manage your project.

This means that, simply put, Cabal is for Haskell what npm is for Javascript, what pip is for Python, or what Cargo is for Rust.

Some important terms when talking about Cabal:
- **cabal package** or just **package** -> unit of code distribution in Haskell. Central part of it is its *cabal file*, which describes it. *cabal package* is analogous to *npm package* for Javascript, or *crate* in case of Rust.
- **cabal file** -> file named <name_of_your_package>.cabal that describes the package (version, dependencies, ...), analogous to package.json for npm or Cargo.toml for Cargo.
- **cabal CLI** or just **cabal** -> `cabal` is a command line tool that you use to build and install Cabal packages (based on the *cabal file*). Analogous to `npm` for Javascript, `pip` for Python, or `cargo` for Rust.
- **Hackage** -> central package archive for cabal packages. Analogous to PyPI for Python, or crates.io for Rust.

### Dependency resolution
One of the main things that cabal does is providing(downloading) the packages that are dependencies for our project.

Each dependency is referenced by its name and the range of versions you are ok with (e.g. `template-haskell: >=2.16 && <2.19`), and `cabal` makes sure to aquire the newest version of each package that fits the specified version range.
It will do the same thing for dependencies of dependencies, recursively, until it aquires all of the packages needed to build your project.

All of this is very similar to how other package managers (npm, pip, ...) do it.

Where things get interesting is when there are multiple dependencies using the same package as their dependency.
For example, your project depends on `foo: >=1.3 && <1.4` and `bar: >=12.0 && <12.1`, where `foo` depends on `filepath: >=1.2 && <2.0` and `bar` depends on `filepath: >=1.0 && < 1.5`.
In such situation, cabal tries to find the newest version of the `filepath` package that satisfies constraints from both `foo` and `bar`, in this case that could be e.g. `filepath 1.4.2`.

If all of the contraints can't be satisfied at the same time (e.g. `foo` had `filepath: >=1.0 && <2.0` and `bar` had `filepath: >=2.0 && <3`), cabal will use multiple versions of the same package, causing so called [**diamond dependency problem**](https://well-typed.com/blog/2008/04/the-dreaded-diamond-dependency-problem/). This is not a problem if `foo` and `bar` don't expose types from `filepath` in their interfaces, but if they do, then we might end up with different versions of same types in our code and get a type error. This is also what is often referenced as "cabal hell", although the term itself is actually a bit wider.

## Stack

[Stack](https://haskellstack.org) is alternative to `cabal` - it takes different approach to some things in attempt to resolve some of the issues that you might encounter with cabal (e.g. diamond dependency).

Some important terms when talking about Stack:
- **stack project** -> Haskell project with `package.yaml` and `stack.yaml` files, which are analogous to `.cabal` file in Cabal project.
- **stack CLI** or just **stack** -> alternative to `cabal` CLI, you use it to build your stack project and install dependencies (based on the `package.yaml` and `stack.yaml` files).
- **Stackage** -> curated set of packages from Hackage, organized into snapshots where each snapshot has fixed version of ghc and fixed version of each package.

Stack is not completely separate from Cabal, actually it relies very much on parts of it:
- `stack` CLI internally uses Cabal library, which is the core logic also used by `cabal` CLI. So you could say that `stack` CLI and `cabal` CLI are both frontends for Cabal.
- The packages that `stack` downloads or produces are still cabal packages - there is no such thing as "stack package". Cabal is the only format used to describe Haskell packages. Stack actually generates a .cabal file based on package.yaml and stack.yaml.
- Packages are still downloaded from Hackage - it is just that Stackage provides instructions on which versions of packages work together, for a curated set of pakages (so that dependencies can be nicely resolved).

Main benefits that Stack provides next to Cabal are:
- Fixed version of GHC and dependencies means you have a reproducible build.
- Stack downloads and uses the correct GHC version automatically, on a per-project basis.
- It avoids "diamond dependency problem", since you know that all the packages from the snapshot are compatible regarding their dependency versions.

## Stack vs Cabal

Which one is a better choice, and for what use case? How do you decide which one to use?

That seems to be a hard question, as there are different opinions online and no clear answer.

Stack does take care of ghc, but you can also handle ghc with `ghcup`.

Stack also helps avoid problems with dependency resolution, but cabal also improved its dependency resolution algorithm with time, and you can still get into trouble with Stack if you need to add package that is not in Stackage.

In practice, I would say that going with Stack is the best starting option, since it ensures that correct version of ghc is used and you don't have to think about the versions of your dependencies.
Then, with time, as your needs/requirements grow, you should be able to make a choice if it makes sense to move to cabal at some point or not.

## Resources
- Comments I got on first version of this article: https://www.reddit.com/r/haskell/comments/qm4xlg/request_for_review_short_article_on_cabal_and/

TODO: Explain how with cabal freeze + with-compiler you can achieve reproducibility, similar as Stack offers.
TODO: How do you pick which version of GHC to use with cabal -> the one that ghcup recommends? Why that one? If there are multiple developers working on same codebase, how do I ensure they all use the same version of cabal and ghc? Is that even important? cabal-freeze freezes package versions, similar like package.lock.json does, but does it freeze ghc version?
