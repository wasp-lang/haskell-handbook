Good starting tutorial: https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial .
Also good starting tutorial: https://markkarpov.com/tutorial/th.html .

## Quick overview

NOTE: This is just a quick overview, you should supplement it with in-depth tutorials (check the list above).

Template Haskell (TH) allows us to write "macros" -> Haskell code that runs during compile time (so called "meta code") and generates Haskell code that becomes part of our Haskell program (so called "object code").

You need to enable `TemplateHaskell` extension for it to work, and you will want to import `Language.Haskell.TH` for main stuff.

### Main features / concepts
1. **Splicing**: `$( )`. Runs given `Q` monad to obtain the TH AST and generates code from it at the place of the call.
2. **`Q`**: Quotation monad, inside it we construct the TH AST.
3. **TH AST**: `Exp`, `Decl`, ... . Description of object code we want to generate.

These three form the basis of TH and they are intervined.

Further we have:

4. **Syntax construction functions**: easier way to construct TH AST while in `Q` monad.
5. **Quotation brackets**: `[| |]`, `[e| |]`, `[p| |]`, `[d| |]`, `[t| |]`. Easier way to construct TH AST by transforming code into AST. Inverse operation of splicing.
6. **`Lift` typeclass**: `lift :: Lift a => a -> Q Exp`. Makes type convertible into TH AST.
7. **Quoted identifiers**: `'myFunc`, `''MyType`.
8. **Reification**: `reify` function that can be used in `Q` to look up AST of existing code. Kind of like reflection API in Java.
9. **Quasi quotes**: `[myQuoter|some_content|]`. You can define `QuasiQuoter` which then parses `String` into `Q` and after that it is normal splicing.

### Splicing, Q and AST
Main concept in TH is *splicing* -> in the code, you can write `$(myTHExp)` where `myTHExp` has to be a Haskell expression that evaluates to `Q Exp`, `Q [Decl]` or something similar.
`$( )` is called splicing operator, `Q` is "quotation monad" which is main monad of TH in which code is constructed, and `Exp`, `Decl` and others are how TH describes the program to be generated (it is an AST).
So what splicing operator does is run given `Q` monad, obtain the result which is AST representing the code, and embed it into source of your Haskell code at the place where splicing was called.

```hs
genId :: Q Exp
genId = do
  x <- newName "x"
  lamE [varP x] (varE x)
```

Usually we will write functions that take some args and return `Q a` where `a` is part of TH AST, and then we splice those with `$()` where we need to generate the code.
One of the most popular `Q` fuctions is `newName`.

When constructing TH AST, we can use TH provided data constructors, or we can use helper functions they provided which can be easier to use since you don't need to unpack values from `Q`. Look for "syntax construction functions" to learn more.

### Quotation brackets and quoted identifiers.

Quotation brackets (`[| |]`) allow you to build object code without building AST directly, instead you write normal Haskell code and it gets "parsed" into AST.
Quotation brackets are considered to be inverse operation to splicing -> the lift code to AST, instead of AST into code.
They can be called such as `[e| |]` for expressions, `[p| |]` for pattern, `[d| |]` for declaration or `[t| |]` for type.
`[| |]` is just a shortcut for `[e| |]`.

```hs
genId' :: Q Exp
genId' = [| \x -> x |]
```

TH also introduces "quoted identifiers" -> you can refer to Haskell identifier from within TH program by prefixing it with one (`'myFunc'`) or two (`''MyType`) quotes.

### Lift

It is important to also mention `Lift` typeclass from TH. If a type implements it, it can be lifted into `Q` monad. `Lift` has single method: `lift :: Lift a => a -> Q Exp`.
It allows writing something like `[e| myValueOfLiftableType |]` for custom types -> now "quotation brackets" will know how to transform that into TH AST.

### Reification

Reification which allows you to query compile-time information about other program parts from inside your meta program.
Main way of using is it is by calling `reify` function on the `Name` (TH type representing identifier).

### Quasi quotes

Quasi quotes allow you to write stuff like `[myQuoter|some_content|]`, where `myQuoter` is of `QuasiQuoter` type.

Conecptually, `myQuoter` receives the content as String and then parses it into `Q` monad. That `Q` value is then normally spliced and code is generated.
`QuasiQuoter` is just type that combines 4 such parsers, for expressions, patterns, types and declarations, and you can define only some of those if you wish.

To use "quasi quotes", you need to enable extension `QuasiQuotes`.

### Using GHCI to learn about TH AST

First, enable TemplateHaskell extension in GHCI and import `Language.Haskell.TH` module:
```
{-# LANGUAGE TemplateHaskell #-}
:m +Language.Haskell.TH
```

Now you can do

```hs
runQ [| some_code |]
```

and ghci will output the TH AST! 

For example `runQ [| \x -> x |]` will return `LamE [VarP x_1] (VarE x_1)`.
