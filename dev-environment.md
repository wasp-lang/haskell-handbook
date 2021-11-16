One challenge in Haskell is making it easy to work on multiple projects using different build tools (cabal, stack)
and possibly different versions of GHC and also different additional tools (stan, hlint, ormolu, ...).

For some tools, like hls and stan it is important to use versions that were compiled with the same version
of GHC as was used for the project.

For others, we just want reproducibility and ease of usage/installation.

It seems like Nix could be the ultimate solution, but it is known as hard and complex.

If stack/cabal supported something similar to npm + package.json's dev-dependencies and scripts, that would help a lot, but they don't.
We did end up implementing a solution in Wasp, which is Stack project, where se use special stack-<tool>.yaml files for each tool and
then install tools localy for project and call them via special bash script, and that is not so bad, but is hackish.
  
It seems on the other hand a decent amount of people is just using `ghcup` and manually switching versions of tools as needed,
but that is very manual and not reproducible.
  
Summary:
  - Nix -> probably the best solution
  - use stack-<tool>.yaml approach for ok experience in stack projects.
  - Go with `ghcup` and manual switching and it will be ok (but not great).
  
Reddit discussion on this topic that I started: https://www.reddit.com/r/haskell/comments/qv7ben/how_do_you_keep_toolstoolchain_in_sync_with/ .

TODO: write into more details about this, also investigate better how to solve this. Maybe explore Nix solution.
