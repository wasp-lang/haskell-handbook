Wikipedia definition: https://en.wikipedia.org/wiki/Expression_problem  
TLDR; How do you make your code open to both adding new operations and new types without modifying existing code?

In OOP, by default it is easy to add new types (add new class that implements existing operations) but to add new operation you need to modify the existing classes.  
In FP, by default it is easy to add new operation (new new function that pattern matches on existing types) but to add new type you need to modify the existing operations.

Solution for this in OOP is Visitor Pattern.

In Haskell, there isn't a super simple and elegant solution to this, but in practice you normally don't really need it and it is ok getting by with pattern matching (and type classes if needed). Type system already helps a lot by warning you where changes are needed, avoiding the issue of forgetting to make the change in some place in the code.

There are solutions out there however, from simpler to more complex ones.
Some materials on the topic:
 1. General discussion on Reddit about it: https://www.reddit.com/r/haskell/comments/4gjf7g/is_solving_the_expression_problem_worth_the_bother/
 2. Simple(st) solution to it: http://lambda-the-ultimate.org/node/4394#comment-68002
 3. More complex, famous solution to it: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

## Example of Expression Problem in Haskell

We have a simple interpreter:
```hs
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
```

If we want to add new operation, we just add additional function:
```hs
render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ "+" ++ render y ++ ")"
```

But if we want to add another type of expression, we need to modify existing functions (and Expression Problem is all about not modifying existing code):
```hs
data Expr = Val Int | Add Expr Expr | Sub Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
```

So the question is, how can we add another type (`Sub Expr Expr`) without modifying existing code?

## Solutions

I (Martin) read an [online discussion](http://lambda-the-ultimate.org/node/4394#comment-68002) that said:

```
a widely known Haskell solution to the expression problem, is to lift all constructors to the type level, and use type classes to implement cases where you'd normally use pattern matching. The solution presented in this paper essentially makes this the canonical method of pattern matching, ie. pattern matching is reduced to dictionary dispatch. The connections to OO vtables is obvious I hope.
```

so I came up with following solution:

```hs
{-# LANGUAGE GADTs #-}

module ExpressionProblem where

import Data.Typeable (Typeable)

------ Operations ------

class Eval a where
  eval :: a -> Int

class Render a where
  render :: a -> String

------ Top lvl data type -------

data Expr where
  Expr :: (IsExpr e) => e -> Expr
  deriving (Typeable)

-- NOTE: This breaks the Expression Problem, as we need to add new operations
-- into list of constraints here!
class (Typeable e, Eval e, Render e) => IsExpr e

instance Eval Expr where
  eval (Expr e) = eval e

instance Render Expr where
  render (Expr e) = render e

------- Data types --------

data Val = Val Int
  deriving (Typeable)

instance IsExpr Val

instance Eval Val where
  eval (Val x) = x

instance Render Val where
  render (Val x) = show x

-----

data Add = Add Expr Expr
  deriving (Typeable)

instance IsExpr Add

instance Eval Add where
  eval (Add x y) = eval x + eval y

instance Render Add where
  render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

add :: (IsExpr e1, IsExpr e2) => e1 -> e2 -> Add
add e1 e2 = Add (Expr e1) (Expr e2)

------- Usage ---------

main :: IO ()
main = do
  let expr = add (Val 42) (add (Val 314) (Val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
```

However, as you can see above, it didn't work completely. It gets pretty close, but there is still one place where each new operation needs to be added, so Expression Problem isn't solved.

I [asked on reddit](https://www.reddit.com/r/haskell/comments/pcx4cx/solving_expression_problem_for_simple_interpreter/) for help, and community suggested that complete solution in a relatively approachable way can be achieved by "Taggles Final" approach.

### Tagless Final

Article about this approach (solving expression problem is just one application of it): http://okmij.org/ftp/tagless-final/course/lecture.pdf .

Solving our interpreter example with it:

```hs
class Add e where
  add :: e -> e -> e

class Val e where
  val :: Int -> e

newtype Eval = Eval { eval :: Int }

instance Add Eval where
  add (Eval x) (Eval y) = Eval (x + y)

instance Val Eval where
  val = Eval

newtype Render = Render { render :: String }

instance Add Render where
  add (Render x) (Render y) = Render (concat ["(", x, " + ", y, ")"])

instance Val Render where
  val x = Render (show x)

main :: IO ()
main = do
  let 
    expr :: (Add e, Val e) => e
    expr = add (val 42) (add (val 314) (val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
```

TODO: Explain if this approach works or not. Explain the logic behind using this approach, how do we translate from original thing to this (it is not very intuitive). But first make sure this also works when you have functions and data which need to have expression part of their signature, I am still figuring this out.

