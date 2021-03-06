# WIP

TODO: Rename title to private vs public, or internal implementation vs API? And then I can talk about exporting vs not exporting from the module as simplest form of it,
and then comes using Internal to indicate which modules are implementation details of their parent module. I should also provide specific exmaple with module structure.
And here is reddit question where I asked about this: https://www.reddit.com/r/haskell/comments/ppyxzv/how_to_define_module_api_toward_other_modules/ .

==============

When we have a module in Haskell that has submodules, it can be important to define the API that this whole module tree should have toward the rest of the project.

For example, if our compiler project has Analyzer module that has Analyzer.Common and Analyzer.Combinators modules in it, is the rest of the code,
that uses Analyzer, supposed to import only Analyzer, or is it allowed to also import Analyzer.Common or maybe Analyzer.Combinators?

I would say that often we want the rest of the code to import Analyzer module, while the rest we regard as implementation details of that whole module tree.

On the other hand, sometimes we do want to allow importing specific sub-modules from the module tree, e.g. we might want Analyzer to be the main API,
but we also want users of the code to import Analyzer.Combinators in case they need Combinators, which are kind of an "extra", they are not always needed,
so they are separated into special module, but they are also not internal implementation details, they are part of the Analyzer API toward the rest of the code.

As far as I am aware, there is no mechanism in Haskell that can enforce this.
Instead, the best solution I am aware of is usage of "Internal" submodules to indicate that something is internal to the parent module.
E.g., if we have Analyzer,  Analyzer.Internal.Combinators and Analyzer.Internal.Common, then it is clear to consumer of Analyzer module that
Analyzer.Internal.Combinators and Analyzer.Internal.Common are implementation details and not part of the Analyzer API.
However, this is a convention and is not enforced by compiler in any way.

What I don't like about Internal is that, besides not being enforced, it is boring/tedious to rename the modules/files in order to make them Internal.

Question: Is there a better way? Maybe some kind of GHC extension that doesn't allow importing internal modules, taking Internal into account?
