In Haskell, due to its unique properties (functional, immutable, pure, referential transparency, ...), the data structures most used are different than in other mainstream languages.

Here, we give a high-level overview of the most commonly used data structures used in Haskell, roughly ordered by the popularity / likelihood of using them. The goal of this document is not to teach how to use each of them, but to give you a feeling of what are your options / what others commonly use.

## List

- [Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html)

Your "bread and butter" data structure in Haskell.

The concept of list fits great with Haskell's properties (immutability, ref transp, ...) and most of the standard functions operate on lists.

While limited in which operations it can efficiently perform, it is simple, easy to use, and a great default choice.

## Map

- [Data.Map (containers)](https://hackage.haskell.org/package/containers/docs/Data-Map.html)
- [Data.HashMap.Strict (unordered-containers)](https://hackage-content.haskell.org/package/unordered-containers/docs/Data-HashMap-Strict.html)

Map is extremely versatile, fast and efficient both for reading and updating, and can be a good solution in many use cases, e.g. you can even use it instead of 2D array if performance is not crucial.

`HashMap` from `unordered-containers` has an extra requirement of key having to be hashable, but in return you get better performance + there is `Strict` version which can be a good default. "Bad" side (if you need that, often you don't) is that it doesn't guarantee order of elements.

## Set

- [Data.Set (containers)](https://hackage.haskell.org/package/containers/docs/Data-Set.html)
- [Data.HashSet (unordered-containers)](https://hackage.haskell.org/package/unordered-containers/docs/Data-HashSet.html)

`Data.HashSet` can be faster, especially when value comparisons are expensive (e.g. strings), but it requires keys to be hasheable and does not guarantee the order.

## Sequence

- [Data.Sequence (containers)](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html)

Similar to list, but with the following differences:
- finite
- strict
- O(1) read/update of both first and last elements (unlike just first in lists)
- wider variety of efficient operations (logarithmic concatenation, splitting, lookup, ...)

Good choice when doing some more serious data processing and need better performance than what a list can offer (for the operations where Sequence is faster), but don't mind the fact it is finite/strict.

## Array and Vector

While data structures like `List` and `Sequence` fit great into Haskell, they don't offer an O(1) random access, and especially not update of an element. If you need max performance for those operations, you will want to use `Vector` or `Array` instead.

- [Data.Array](https://hackage.haskell.org/package/array)
- [Data.Vector (vector)](https://hackage.haskell.org/package/vector)

`Array` is the standard, versatile implementation, while `Vector` is an "efficient array (with Int indexes)", with simpler API and better performance.
If not sure, and need only standard Int indexes, you will likely want to go with `Vector` by default.
If you need more control (e.g. non-Int indexes), you will want to go with `Array`.

### Efficient mutation/update
While `Array` (or `Vector`) offers great performance and is easy to use for reading, performant mutation/update is not easy/obvious to achieve due to Haskell's unique properties.

Array is a piece of consecutive memory, which is what enables fast random access. Not a bunch of pieces of memory and references to them, like a list or map or a tree.
In lists, we can easily create a new piece of memory just for the updated part of it and reuse the rest of the initial list, since they are chunks of memory, but we can't do that with array since it is one consecutive, single piece of memory.
To update a part of array, we have to either mutate it in place, which is fast but goes against Haskell's immutability, or we need to copy the whole array while changing only that piece in the copy, which is then much slower.

The best solution is to do the update-heavy part of the code in the [`ST` monad](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad-ST.html), which allows us to mutate data in place, but also keeps the code pure from the caller's perspective. It does normally mean that once `ST` code starts, you will convert ("thaw") array into its immutable version, operate on it, and then finally at the end convert ("freeze") it back to immutable version and return it. This doesn't make much sense if you do very little mutation in the `ST`, due to cost of that one thaw/freeze, but it does make sense if you keep most/all of your mutation-heavy logic in there and the cost of thaw/freeze is negligible compared to it.
`Array` and `Vector` offer special mutable variants and functions for this very purpose.

If you don't mind writing your update logic in IO instead of pure code, you can also do the mutation in `IO`, both `Array` and `Vector` support that also via their mutable variants. And you can, if you want, turn that array again into immutable version and send it to pure code if needed.


### Multidimensional arrays/vectors

If you want to construct multidimensional (e.g. 2D) array, you can use a pair (e.g. `(Int, Int)`) as an index for `Array`, or you can use an one-dimensional `Vector` and write your own logic that will map 2d indexes to 1d indexes.
