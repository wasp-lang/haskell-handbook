In Haskell, functions are instances of Functor, Applicative and Monad.

Very good article with a lot of details: https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/ .

## TLDR;

### Function as a Functor

```hs
g <$> f == \x -> g (f x) == g . f
```

What really happens is just composition. Basically you apply `g` on the return value of the function `f`.

Visually, we go from
```
      f
x --> _
```
to
```
      f     g
x --> _ --> _
```


### Function as an Applicative

```hs
g <*> f == \x -> g x (f x)
```

You apply `g` onto two arguments, first `x` and second `f x`.
I think of it as piping `x` first into `f`, and then piping `x` and `f x` into `g`. 

Visually, we go from
```
      f
x --> _
```
to
```
            g
x --------> _
│     f
└---> _ --> _
```

### Function as a Monad

```hs
f >>= g = \x -> g (f x) x
```

Same as with applicative, but flipped order of arguments that get passed to `g`.

Visually, we go from
```
      f
x --> _
```
to
```
      f     g
x --> _ --> _
└---------> _
```

Note that for Applicative and Monad `g` must be a function that takes at least two arguments, while for Functor it must take at least one argument.
