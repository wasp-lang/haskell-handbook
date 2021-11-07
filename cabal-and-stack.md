# Cabal & Stack

Simplest way to write some code in Haskell and get going is to write it directly in the ghci REPL, or to write a single-file script and then load it and execute it with ghci. You could also directly compile your .hs files with ghc.

However, if you are building anything a bit more complex, you will want to organize it into a project/package that builds into an executable and/or library and very likely depends on other Haskell packages.

The standard/official solution for this is Cabal, and then there are popular alternatives like Stack (or more general, non Haskell specific solutions like Nix, Bazel, Shake, ...) that build on top of Cabal in order to provide different experience.

[Cabal](https://haskell.org/cabal) and [Stack](https://haskellstack.org) are the most popular ones so we will shortly introduce both of them here and talk about the differences between them.

## Cabal

[Cabal](https://haskell.org/cabal) is the standard package system for Haskell software.

It helps you configure (e.g. define version, dependencies, ...), build (compile) and distribute (e.g. publish as a library) your Haskell project - basically all of the stuff you need to manage your project.

This means that, simply put, Cabal is for Haskell what npm is for Javascript, what pip is for Python, or what Cargo is for Rust.

Some important terms when talking about Cabal:
- **cabal package** or just **package** -> unit of code distribution in Haskell. Central part of it is its *cabal file*, which describes it. *cabal package* is analogous to *npm package* for Javascript, or *crate* in case of Rust.
- **cabal file** -> file named <name_of_your_package>.cabal that describes the package (version, dependencies, ...), analogous to package.json for npm or Cargo.toml for Cargo.
- **cabal-install** or **cabal CLI** or just **cabal** -> `cabal` is a command line tool that you use to build and install Cabal packages (based on the *cabal file*). Analogous to `npm` for Javascript, `pip` for Python, or `cargo` for Rust.
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
- **stack project** -> Haskell project with `package.yaml` and `stack.yaml` files, which are analogous to `.cabal` file in Cabal project (and are actually used behind the scenes to generate cabal file).
- **stack CLI** or just **stack** -> alternative to `cabal` CLI, you use it to build your stack project and install dependencies (based on the `package.yaml` and `stack.yaml` files).
- **Stackage** -> curated set of packages from Hackage, organized into snapshots where each snapshot has fixed version of ghc and fixed version of each package.

Stack is not completely separate from Cabal, actually it relies very much on parts of it:
- `stack` CLI internally uses Cabal library, which is the core logic also used by `cabal` CLI.
  So you could say that `stack` CLI and `cabal` CLI are both frontends for Cabal.
- The packages that `stack` downloads or produces are still cabal packages - there is no such thing as "stack package".
  Stack actually generates a .cabal file based on package.yaml and stack.yaml.
- Packages are not hosted on Stackage, they are still hosted on Hackage.
  What Stackage does is provide additional instructions on which versions of packages work together, for a curated set of packages (so that dependencies can be nicely resolved).

Main benefits that Stack provides next to Cabal are:
- Fixed version of GHC and dependencies means you have a reproducible build by default.
  Cabal can also pin down dependencies with `cabal freeze` but it is not a default behaviour and requires a bit more effort.
  Additionaly `with-compiler` option in `cabal.project` file can be used to pin down the version of GHC.
- Stack downloads and uses the correct GHC version automatically, on a per-project basis.
  With Cabal it is recommended to use `ghcup` for managing ghc and other tooling.
- It avoids "diamond dependency problem", since you know that all the packages from the snapshot are compatible regarding their dependency versions.

## Stack vs Cabal

Which one is a better choice, and for what use case? How do you decide which one to use?

That seems to be a hard question, as there are different opinions online and no clear answer.

While Stack was much more user friendly at the beginning, Cabal improved a lot in the meantime and got closer to Stack in that regard - these days official recommended approach, which is `ghcup` + `cabal`, is getting a lot of positive feedback.

Stack does take care of ghc, but you can also handle ghc pretty nicely with `ghcup`. However, you will still have to make sure you switch ghc version manually if you are switching between cabal projects that use different versions of ghc, while Stack will use the correct version automatically.

Stack also helps avoid problems with dependency resolution, but cabal also improved its dependency resolution algorithm lately so it is not that much of an issue any more, and you can still get into trouble with Stack if you need to add package that is not in Stackage.

In practice, both going with `ghcup` + `cabal` or going with `stack` are similarly good options these days.
Personally and subjectively, I would put it this way:
- If you want to stick with the officially recommended approach and keep things "standard", first option (cabal) might be the best.
- On the other hand, if you prefer a bit frendlier experience with more things taken care of out of the box by default (and therefore less space for doing something wrong or overly complicated), second option (stack) might be a better choice.

## Resources
- Comments I got on first version of this article: https://www.reddit.com/r/haskell/comments/qm4xlg/request_for_review_short_article_on_cabal_and/
- Similar but older article from Kowainik: https://kowainik.github.io/posts/2018-06-21-haskell-build-tools
- List of advanced cabal tips, first few cover replicating Stack's reproducibility via Cabal with index-state, freeze and similar: https://lukelau.me/haskell/posts/making-the-most-of-cabal/
