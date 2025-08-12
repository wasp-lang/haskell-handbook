# Modern Records

Records in Haskell (`data Animal = { name :: String, numLegs :: Int }`) are often called out as being clunky to work with, so there have been more than a few attempts at improving that.

While the ultimate solution for working with records is probably optics/lenses, we will not go into it here, as it comes with a learning curve and makes it harder for newcomers to understand the codebase.

Instead, we will focus on a set of language extensions that bring easy-to-understand, intuitive quality of life improvements when working with records:
- `DuplicateRecordFields` -> Allows multiple records in the same module to use the same field names (`data Person = { name :: String }; data Animal = { name :: String }`).
- `OverloadedRecordDot` -> Allows accessing record field in a similar fashion to other languages: `animal.name` instead of `name animal`.
- `NoFieldSelectors` -> Functions (selectors) (e.g. `name :: Animal -> String`) are no longer automatically generated for each field in the record, allowing for shorter names and avoiding naming conflicts.
- `NamedFieldPuns` -> Allows writing `sayHi Animal{name} = "Hi " <> name` instead of `sayHi Animal{name=name} = "Hi " <> name`.

Above is just a short overview of what these do; check each one of them for more details. All four of these extensions interact well together, and normally, you can just set them as default extensions for the whole project.

There is also `OverloadedRecordUpdate` that allows for nested updates like `person { address.streetNum = 42 }`, but it is still experimental and somewhat rough, which is why we are not recommending it by default.

## Resources

Here is video by Well Typed on the topic: https://www.youtube.com/watch?v=9hrDm7xDpig&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7 .
