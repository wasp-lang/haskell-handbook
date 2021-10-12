Difference between foldl, foldl' and foldr: https://wiki.haskell.org/Foldr_Foldl_Foldl%27 .

TLDR (from the Conclusion chapter of the link above); normally you will want to use foldr or foldl', as foldl will in most cases accumuluate unnecesary memory.

foldr is not only the right fold, it is also most commonly the right fold to use, in particular when transforming lists (or other foldables) into lists with related elements in the same order.

The other very useful fold is foldl'. It can be thought of as a foldr with these differences:

foldl' conceptually reverses the order of the list. One consequence is that a foldl' (unlike foldr) applied to an infinite list will be bottom; it will not produce any usable results, just as an express reverse would not. Note that foldl' (flip (:)) []==reverse.
foldl' often has much better time and space performance than a foldr would for the reasons explained in the previous sections.
You should pick foldl' principally in two cases:

When the list to which it is applied is large, but definitely finite, you do not care about the implicit reversal (for example, because your combining function is commutative like (+), (*), or Set.union), and you seek to improve the performance of your code.
When you actually do want to reverse the order of the list, in addition to possibly performing some other transformation to the elements. In particular, if you find that you precede or follow your fold with a reverse, it is quite likely that you could improve your code by using the other fold and taking advantage of the implicit reverse.
foldl is rarely the right choice. It gives you the implicit reverse of fold, but without the performance gains of foldl'. Only in rare, or specially constructed cases like in the previous section, will it yield better results than foldl'
