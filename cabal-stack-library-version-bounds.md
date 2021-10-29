# Stack, Cabal and figuring out dependencies version bounds

TODO: This document is (will be) covering too much, we should probably split it into multiple docs.

TODO: Write the doc. Here is just a sketch of what to write about:
 - stack vs cabal -> cabal is THE package mager and works like other package managers, while stack is alternative
                     that uses the same core library (cabal) but uses the concept of snapshots on top to solve some of the problems cabal has/had.
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
