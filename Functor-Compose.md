https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor-Compose.html

`Compose` is type from the `base` which enables us to compose two Functors into a Functor whose `fmap` reaches all the way to the inner value.

In a way, it does for Functor and Applicative what monad transformers do for Monad -> allows composing them.

## Example
e.g. if we have two functors, `Either String` and `Maybe`, and we compose them manually like `type MyType = Either String (Maybe Int)`,
`fmap` would apply over the `Maybe Int` part, meaning we would have to do another `fmap` then to get to the `Int`.

However, if we do `type MyType = Compose (Either String) Maybe Int` and then do `fmap` on it, it will apply over the `Int`, passing through both functors, because that is how `Compose` implements its instance of Functor.
Internally, structure of `MyType` would be the same as when we did it manually: `Either String (Maybe Int)`, and we can get to it by using `getCompose myType`.

## Compose vs monad transformers
`Compose` works for Functors and Applicative Functors, but not for Monads.

Why not for Monads? Because composition of (applicative) functors is always a (applicative) functor, while compositions of monads is not always a monad.
Basically, for Functors we always know how to compose any two of them and it can be done automatically (which is what Compose does).
For Monads, we can't automatically compose any two monads, there is just not enough information, but if we have a specific monad we are working with, we can write how it composes with other monads -> monad transformers.
Therefore, Compose is like monad transformers but for Functors, and it is more powerful because it works for any two Functors, while for any two monads we need to have a monad transformer that is implemented for one of them specifically.

