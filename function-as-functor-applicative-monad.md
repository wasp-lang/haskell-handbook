In Haskell, functions are instances of Functor, Applicative and Monad.

Very good article with a lot of details: https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/ .

TLDR;
```hs
-- Function as a Functor: what really happens is just composition.
-- Basically you apply `g` on the return value of the function `f`.
-- Visually, we go from
--         f
--   x --> _
-- to
--         f     g
--   x --> _ --> _
g <$> f == \x -> g (f x) == g . f

-- Function as an Applicative: You apply `g` onto two arguments, first `x` and second `f x`.
-- I think of it as piping `x` first into `f`, and then piping `x` and `f x` into `g`. 
-- Visually, we go from
--         f
--   x --> _
-- to
--               g
--   x --------> _
--   |     f
--   └---> _ --> _
g <*> f == \x -> g x (f x)

-- Function as a Monad: Same as with applicative, but flipped
-- order of arguments that get passed to `g`.
-- Visually, we go from
--         f
--   x --> _
-- to
--         f     g
--   x --> _ --> _
--   └---------> _
f >>= g = \x -> g (f x) x

-- Note that for Applicative and Monad `g` must be a function that takes at least two arguments, while for Functor it must take at least one argument.
```
