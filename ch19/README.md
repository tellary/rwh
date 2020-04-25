Exercise on p. 462

1. Take the `Either` example and made it work with laziness in
   the style of the `Maybe` example.

    > See `divByLazy` in [`divBy.hs`](./divBy.hs)

Exercises on p. 465

1. Write a `many` parser, with type `Parser a -> Parser [a]`.
   It should apply a parser until it fails.

    > See `many3` in [`MyExceptTParse.hs`](../ch18/MyExceptTParse.hs)
    > under `ch18`, it uses `catchError` directly.
    >
    > `many2` uses `(<|>)`, it's a re-implementation of `Alternative (many)`.
    > The later should be used in practice. Also `(<|>)` is implemented
    > with `catchError`.

2. Use `many` to write an `int` parser, with type `Parser Int`.
   It should accept negative as well as positive integers.

    > `int` parser is available in
    > [`MyExceptTParse.hs`](../ch18/MyExceptTParse.hs)
    > under `ch18`.
    > Some Tests are in [`MyExceptTParseSpec.hs`](../ch18/MyExceptTParseSpec.hs)

3. Modify your `int` parser to throw `NumericOverflow` exception
   if it detects a numeric overflow while parsing.

    > `int` parser in
    > [`MyExceptTParse.hs`](../ch18/MyExceptTParse.hs)
    > detects `NumericOverflow` by comparing output of `show`
    > with input of `read`
