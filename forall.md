In Haskell, there is a keyword `forall`.

You might have seen it in advanced code like this:
```hs
f :: forall a b. a -> b
f = ...
  where g :: b
        g = ...

mapPair :: (forall x. x -> f x) -> (a, b) -> (f a, f b)

data Showable = forall s. (Showable s) => Showable s
```

`forall` is a type quantifier, and it gives extra meaning to polymorphic type signatures (types with letters e.g. `:: a`, `:: a -> b`, `:: a -> Int`, ...).

While normaly `forall` plays a role of the "universal quantifier", it can also play a role of the "existential quantifier" (depends on the situation).

What does this mean and how can `forall` be used? Below we explain it in relatively simple fashion!
NOTE: we assume you are comfortable with basic polymorphism in Haskell.

## Quick match/logic reminder

In mathematical logic, we have

- **[universal quantifier](https://en.wikipedia.org/wiki/Universal_quantification)**
  - symbol: ∀x
  - interpretation: "for all", "given any"
  - example: `∀x P(x)` means "for all x predicate P(x) is true".
- **[existential quantifier](https://en.wikipedia.org/wiki/Existential_quantification)**
  - symbol: ∃x
  - interpretation: "there exists", "there is at least one", "for some"
  - example: `∃x P(x)` means "there is some x for which predicate P(x) is true".

## Vanilla Haskell (no extensions)

In Haskell, all polymorphic type signatures are considered to be implicitly prefixed with `forall`.

Therefore, if you see code
```hs
f :: a -> a
g :: a -> (a -> b) -> b
```
that is the same as
```hs
f :: forall a. a -> a
g :: forall a b. a -> (a -> b) -> b
```

What `forall` here does is play the role of universal quantifier.
For function `f`, it means it is saying "for all types, this function takes that type and returns the same type.".
Other way to put it would be "this funtion can be called with value of any type as its first argument, and it will return the value of that same type".

Since `forall` is already implicit, writing it explicitly doesn't really do anything!

Not only that, but without any extensions, you can't even write `forall` explicitly, you will get a syntax error, since `forall` is not a keyword in Haskell.

So what is the purpose of `forall` then? Well, obviously to be used with extensions :)!

The simplest extension is `ExplicitForAll`, which allows you to explicitly write `forall` (as we did above).
This is not useful on its own though, since as we said above, explicitly writing `forall` doesn't change anything, it was already implicitly there.

However, there are other extensions that make use of `forall` keyword, and these are: `ScopedTypeVariables`, `RankNTypes` and `ExistentialQuantification`.
All these extensions automatically enable `ExplicitForAll` extension, which means you don't need to enable it yourself when using any of these.

Since `forall` is useful only when used with at least one of these extensions, let's take a look at how it is used in each one of those!

## `forall` and extension [ScopedTypeVariables](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#lexically-scoped-type-variables)

`ScopedTypeVariables` enables lexical scoping of type variables by explicitly introducing them with `forall`.

Let's take a look at following example:
```hs
f :: [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
```

This code does not compile, because compiler can't match type of `ys` with the return type of `f`.
Why though, when they are both `[a]`? Well, that is because that is not the same `a`!
Try changing `ys :: [b]` and you will get the exact same error,
because it is exactly the same code -> `a` in `ys :: [a]` and `a` in `f :: [a] -> [a]` are different `a`s and there is no connection between them.
`a` in `ys :: [a]` stands for "any type", and not "that type that is reffered to with `a` in the type signature above".

This is where `ScopedTypeVariables` comes in:
```hs
{-# LANGUAGE ScopedTypeVariables #-}

f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
```

`forall` now gets special powers: the type variables bound by a forall (in our case `a`) scope over the entire definition
of the accompanying value declaration (in this case definition of `f`).

This means that any mention of type `a` in the definition of `f` now refers to that `a` from the type signature of `f`,
which is exactly what we needed, and the code example above now compiles.

## `forall` and extension [RankNTypes](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#arbitrary-rank-polymorphism)

I don't feel I understand `RankNTypes` well enough to provide a perfect explanation of it, but I will try below to describe practical side of it bluntly.

Basically, what `RankNTypes` does is enable you to use `forall` nested in type signatures, so that it does not apply to the whole type signature but just the part of it.

This enables some cool things that you were not able to do to before, for example you can specify that your function takes a polymorphic function as an argument.

Take a look at this example:
```hs
foo :: (forall a. a -> a) -> (Char,Bool)
bar :: forall a. ((a -> a) -> (Char, Bool))
```

If we now have `f :: Int -> Int` and `g :: x -> x`, you can pass `g` to `foo` but you can't pass it `f`, while for the `bar` it is opposite.
`foo` needs a polymorphic function as the first argument, while `bar` needs any function that takes one argument of some type and returns that same type.

Another example is `mapTuple` function:
```hs
liftPair :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
```
```
>> liftPair (:[]) (1, "a")
([1], ["a"])
```
`mapTuple` takes polymorphic function and applies it to both values in the pair.
There would be no way to write its type signature without using `RankNTypes`.

## `forall` and extension [ExistentialQuantification]()

TODO








----------------------- TODO --------------------

Great SO question/answers: https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do .

Implicitly, `forall` is there when you have polymorphic function: `f :: a -> b` is same as `f :: forall a b. a -> b`.
It says that this function works for any `a` or `b`. You can add it to make it explicit, but on its own this doesn't really do anything.

If you use `ScopedTypeVariables` extension, `forall` gets extra role/powers -> it "fixes" the names of the polymorphic variables through the rest of the scope.
So for example, if we have `f :: forall a. a -> Int` and `f` has a where clause `where g :: a` (which we can do because of ScopedTypeVariables),
then that `a` will now refer to the same type as `a` in `f :: forall a. a -> Int` refers to.
Otherwise, if f was defined as `f :: a -> Int`, it would be different: `a` in `where g :: a` would be reffering to some new `a`
(we could have named it `b` in that case, it wouldn't change anything).

With `RankNTypes` extension, you can nest `forall`s withing function types and data definitions.
`forall` gets another role: it enables you to pass a polymorphic function as an argument to another function.
Now you can do smth like `mapTuple :: (forall x. x -> f x) -> (a, b) -> (f a, f b)`.
The big thing here is, you applied `forall` on just this `x -> f x` function, not on the whole `mapTuple` function, and that changes things.
It defines the scope of `x`. And now you can call this function on both `a` and `b` and that is ok, otherwise you couldn't.
So basically, you can use `forall` on parts of the type signature and that gives you more expressivity (e.g. you can describe polymorphic function).
This is also very nice example:
```hs
foo :: (forall a. a -> a) -> (Char,Bool)
bar :: forall a. ((a -> a) -> (Char, Bool))
```
If we now have `f :: Int -> Int` and `g :: x -> x`, you can pass `g` to `foo` but you can't pass it `f`, while for the `bar` it is opposite.
`foo` needs a polymoprhic function as the first argument, while `bar` needs any function that takes one argument of some type and returns that same type.

With `ExistentialQuantification` extension, `forall` gets even more powers: now you can do `data MyBox = forall a. MyBox a`. So called "hidden types".
However, now it doesn't play role of the universal quantifier any more -> now it plays the role of the existential quantifier.
`forall` in `ExistentialQuantification` makes most sense when used with constraints, like `data Showable = forall a. (Show a) => Showable a`.
Btw. this can now be done via GADTs, and some people say it is nicer -> I should also mention that and compare the two extensions.

There is only really one thing to remember about 'forall': it binds types to some scope.
Once you understand that, everything is fairly easy. It is the equivalent of 'lambda' (or a form of 'let') on the type level 

Mention how `forall` can be useful with TypeApplications since it specifies the order of the type variables, which is important when applying types.

Further reading: https://dl.acm.org/doi/10.1145/178243.178246 .
