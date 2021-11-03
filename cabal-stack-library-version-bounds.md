# [WIP] Package management (both when building your project/package and when depending on other packages)

TODO: This document is (will be) covering too much, we should probably split it into multiple docs.


TODO: This doc is work in progress. Here is just a sketch of what to write about:
 - stack vs cabal -> cabal is THE package mager and works like other package managers, while stack is alternative
                     that uses the same core library (cabal) but uses the concept of snapshots on top to solve some of the problems cabal has/had.
                     There are no "stack" packages -> on Hackage, all packages are cabal packages. To understand this better, explain
                     the difference between cabal format, cabal tool and cabal packages.
 - cabal diamond dependency problem -> explain what it is and why it happens, mention how this is not a problem in non-typed language (e.g. js + npm).
 - talk about how cabal resolves dependencies, if we haven't covered it enough yet.
 - talk about dependency version bounds
   - if you are writing project that is not published on Hackage
     - if you are using Stack
       - then you can do without version bounds, since they will be determined by the stack resolver that you use anyway.
     - if you are using Cabal
       - then you need to define version bounds so that new versions of dependencies that possibly introduce braking changes are not suddenly downloaded.
         You don't need to keep these version bounds wide though, they could be just `== x.y.z.a` or `== x.y.*` to ensure that no breaking changes surprise you.
   - if you are writing a project/library that you are publishing on Hackage
     - no matter if you are using Stack or Cabal, the uploaded package needs to be uploaded with .cabal that defines the version bounds, Hackage requires that.
       - if using Stack, `stack sdist` generated cabal project with .cabal. For that file to have version bounds, you need to define them in package.yaml or set
         `pvp-bounds: both` in stackage.yaml so that Stack automatically infers the version bounds based on the resolver and puts those into generated .cabal file.
         Once uploaded to Hackage, there is nothing stack-related left in the package itself - it is a cabal package.
         There is nothing such as "Stack package" - Stack is "just" a different client/approach for working with cabal packages.
       - if using Cabal, your .cabal file is the one that is used and therefore it needs to have version bounds.
     - other important thing is that you want your version bounds to not be too strict, because otherwise cabal will have very hard time resolving dependenies
       when your package is involved since it is very limited in what it can work with.
       Ideally, your version bounds would be as wide as possible while on the same keeping confidence that your library will work with correctly with those.
       There might be some version bounds that you just know need to be so (e.g. you need foobar package to be >=2.0 because 2.0 introduced a feature you need),
       but often you will not be sure how far back or forward you can go with some dep without causing problems.
       Good way is to say: it is enough if I support couple of last versions of GHC, and focus on that.
       Then every so and so you update the bounds, as new GHC versions come out: raise the lower ones, and the upper ones.
       Maybe the nicest way to go about this is to say that you will support couple of last LTS Stackage resolvers and the nightly one.
       This automatically covers couple last GHCs and you have an easy way of running those tests.
       We should ideally test this in CI: test our package with multiple Stack resolvers (nightly, and couple of last LTSes).
       More about this here: https://github.com/wasp-lang/strong-path/issues/37 .

# Cabal & Stack

Simplest way to write some code in Haskell and get going is to write it directly in the ghci REPL, or to write a single-file script and then load it and execute it with ghci.

However, if you are building anything a bit more complex, you will want to organize it into a project/package that builds into an executable and/or library and very likely depends on other Haskell packages.

The standard/official solution for this is Cabal, and then there are popular alternatives like Stack (or more general Nix, Bazel, Shake, ...) that build on top of Cabal in order to provide different experience.

[Cabal](https://haskell.org/cabal) and [Stack](https://haskellstack.org) are the most popular ones so we will shortly introduce both of them here and talk about the differences between them.

## Cabal

Cabal is the standard package system for Haskell software.

It helps you configure (e.g. define version, dependencies, ...), build (compile) and distribute (e.g. publish as a library) your Haskell project - basically all of the stuff you need to manage your project.

This means that, simply put, Cabal is for Haskell what npm is for Javascript, what pip is for Python, or what Cargo is for Rust.

Some important terms when talking about Cabal:
- **cabal package** or just **package** -> unit of code distribution in Haskell. Central part of it is its *cabal file*, which describes it. *cabal package* is analogous to *npm package* for Javascript, or *crate* in case of Rust.
- **cabal file** -> file named <name_of_your_package>.cabal that describes the package (version, dependencies, ...), analogous to package.json for npm or Cargo.toml for Cargo.
- **cabal CLI** or just **cabal** -> `cabal` is a command line tool that you use to build and install Cabal packages (based on the *cabal file*). Analogous to `npm` for Javascript, `pip` for Python, or `cargo` for Rust.
- **Hackage** -> central package archive for cabal packages. Analogous to PyPI for Python, or crates.io for Rust.

When downloading and resolving packages that are dependencies of your project, `cabal` mostly works as you would expect -> each package is referenced by its name and the range of versions you are ok with (e.g. `filepath: >=1.2 & <1.5`), and `cabal` makes sure to download the newest version of each package that fits the specified version range.
So, similar as `npm` or other package managers do.

However, there is a specific problem that comes with `cabal`, and which is caused by Haskell being a statically typed language: TODO diamond problem

TODO: multiple GHC versions with cabal? Stack takes care of that? How does cabal know which GHC to use for specific project? What about versions changing with time -> which is why we need cabal freeze? What is cabal freeze? Is it like package.lock.json? Stack also solves this.





