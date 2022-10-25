Cabal allows you to write a single, reproducible, self-contained Haskell file that describes at its top the dependencies and configuration.
You can then run such file with cabal and it will pull in those dependencies, build the file and then run it.

This is great when you want to write a short, single file script/program with Haskell, and you want it to be reproducible and complete (regarding dependencies it uses),
but you don't want to create a whole new cabal project for it. You end up with a single file that you can easily run on its own!

In order to write such script, you need:
1. To have `main :: IO ()` function in the file.
2. To add `#!/usr/bin/env cabal` shebang at the top of the file.
3. To add comment just below the shebang describing the dependencies and similar.

Example:
```hs
#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4.7 && < 5, split
-}

import Data.List.Split (splitOn)

main :: IO ()
main = do
  let orders = splitOn "x" "axbxc"
  putStrLn $ head orders
```

Now you can make this file executable by doing `chmod +x` on it and then just call it with `./my-script.hs`.
You don't really need to use .hs extension, you could just call it `my-script`, but with .hs it is easier to get
IDE to work correctly when editing it.

You will notice cabal compiles your file every time you run it, and outputs quite some output -> if you want to avoid that output, you can modify the shebang to be:
```hs
#!/usr/bin/env -S cabal run -v0
```
which will set cabal verbosity to 0 and ensure that only the output from your script gets printed.

Reference:
- https://stackoverflow.com/questions/65539773/compiling-a-haskell-script-with-external-dependencies-without-cabal/65541020#65541020
- https://github.com/haskell/cabal/issues/4652#issuecomment-1012778481
