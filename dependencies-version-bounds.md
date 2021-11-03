# [WIP] Determining dependencies version bounds

TODO: This doc is work in progress. Here is just a sketch of what to write about:
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



When writing a Haskell project, be it a Stack or Cabal project, you will have a list of dependencies, for which you can specify version bounds (e.g. `filepath: >=1.2 && <1.3` where `filepath` is name of the dependency and `>=1.2 && <1.3` is its version bounds).

Sometimes it can be hard to figure out if you should specify version bounds at all or not, and if yes, how do you determine the bounds. Here we are going to offer an approach that helps with answering these questions.

## Resources
- https://www.reddit.com/r/haskell/comments/d4o7d6/how_should_you_choose_version_bounds_for_library/
- https://www.reddit.com/r/haskell/comments/qhn8es/how_do_i_figure_out_dependency_version_bounds_for/
