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
 
