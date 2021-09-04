[Wikipedia definition of the Expression Problem](https://en.wikipedia.org/wiki/Expression_problem) 
TLDR; How do you make your code open to both adding new operations and new types without modifying existing code?

In OOP, by default it is easy to add new types (add new class that implements existing operations) but to add new operations you need to modify the existing classes.  
In FP, by default it is easy to add new operation (new new function that pattern matches on existing types) but to add new types you need to modify the existing operations.

In OOP, the Visitor Pattern can be used to flip the problem so it is the same as in Haskell.

In Haskell, there isn't a super simple and elegant solution to this, but in practice you normally don't need it and it is OK getting by with pattern matching (and type classes if needed). The type system already helps a lot by warning you where changes are needed, avoiding the issue of forgetting to make the change across all parts of the code.

There are solutions out there however, from simpler to more complex ones.
Some materials on the topic:
 1. [General discussion on Reddit](https://www.reddit.com/r/haskell/comments/4gjf7g/is_solving_the_expression_problem_worth_the_bother/)
 2. [Simple(st) solution](http://lambda-the-ultimate.org/node/4394#comment-68002)
 3. [More complex, famous solution](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)
 4. Nice blog [post about the expression problem in C++ vs Clojure](https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/)

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


> a widely known Haskell solution to the expression problem, is to lift all constructors to the type level, and use type classes to implement cases where you'd normally use pattern matching. The solution presented in this paper essentially makes this the canonical method of pattern matching, ie. pattern matching is reduced to dictionary dispatch. The connections to OO vtables is obvious I hope.

so I came up with the following solution:

```hs
{-# LANGUAGE GADTs #-}

module ExpressionProblem where

import Data.Typeable (Typeable)

------ Operations ------

class Eval a where
  eval :: a -> Int

class Render a where
  render :: a -> String

------ Top level data type -------

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

However, as you can see above, it didn't work completely. It gets pretty close, but there is still one place where each new operation needs to be added, so the Expression Problem isn't solved.

Similar solution is depicted in [this article](https://core.ac.uk/download/pdf/24067153.pdf), section 2.4, with the following caveats:

> Also, values of a given extensible datatype cannot be aggregated or
stored in a straightforward manner. Furthermore, the definition of
functions that return values of the extensible datatype cannot adopt
the aforementioned recipe. 

which really makes it analogous to my solution above -> if you want to make it more practical by allowing aggregating and storing these data types in a straightforward manner (above I achieve that with `data Expr`), then you need to something like I did and Expression Problem is not completely solved any more.

I [asked on reddit](https://www.reddit.com/r/haskell/comments/pcx4cx/solving_expression_problem_for_simple_interpreter/) for help, and community suggested that a complete solution in a relatively straightforward way can be achieved with the  _Tagless Final_ approach.

### Tagless Final

Article about this approach (solving expression problem is just one application of it): http://okmij.org/ftp/tagless-final/course/lecture.pdf .

Solving our interpreter example with it:

```hs
------ Data types ------

class Add e where
  add :: e -> e -> e

class Val e where
  val :: Int -> e

class Mul e where
  mul :: e -> e -> e

---- Operations -----

newtype Eval = Eval {eval :: Int}

instance Add Eval where
  add (Eval x) (Eval y) = Eval (x + y)

instance Val Eval where
  val = Eval

instance Mul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)

-----

newtype Render = Render {render :: String}

instance Add Render where
  add (Render x) (Render y) = Render (concat ["(", x, " + ", y, ")"])

instance Val Render where
  val x = Render (show x)

instance Mul Render where
  mul (Render x) (Render y) = Render (concat ["(", x, " * ", y, ")"])

---- Usage ----

main :: IO ()
main = do
  let expr :: (Add e, Val e, Mul e) => e
      expr = add (val 42) (mul (val 314) (val 217))
  putStrLn $ render expr
  putStrLn $ show $ eval expr
```

A good explanation of reasoning behind this can be found in a [discussion that I lead with reddit user that proposed this as a solution](https://www.reddit.com/r/haskell/comments/pcx4cx/solving_expression_problem_for_simple_interpreter/hanstk4?utm_source=share&utm_medium=web2x&context=3) .

Although this approach is not overly complex, I found it hard to intuitively grasp, since operations become data and data becomes operations.
I haven't yet found a good model to grasp it better intuitively, but as author of the solution said:

> As for intuition, I still just think about it as the mechanical translation from constructors -> type classes and interpretations as instances.

So, data constructors become type classes, and operations (interpretations) become instances of those.

The tricky part in practice seems to be dealing with type signatures.
If you add a new piece of structure, e.g. `Mul` next to `Add` and `Val`, you have to update type signatures from `(Add e, Val e)` to `(Add e, Val e, Mul e)`.
Does this mean that Expression Problem is broken? Really depends on how those type signatures are used. What if we want to have some bigger type, maybe some monad transformer stack, that contains an expression as part of its state -> what would the type signature be there, and if we need to change it when we add new data type / structure, is that causing the Expression Problem? I don't understand this well enough yet, and I haven't created a real world example to test how it behaves.

## Conclusion
The "Tagless Final" approach seems like a good way to completely solve Expression Problem in Haskell, although I am not yet convinced practical is the solution in real world use cases (but I haven't understood / tested it out well enough so that is why).

On the other hand, the simpler approach I used above almost solves it (requires updating of just one line when a new operation is added). I find it more intuitive, and I am more assured of its practicality in real world use cases, so I would probably pick that one for now if need be.

Finally, there are other ways to solve this problem that I haven't investigated yet (because they seemed more complex); it might be worth looking into [Data Types A La Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) .

And important thing to ask ourselves -> is the Expression Problem really a problem? It seems to me like it rarely is!
