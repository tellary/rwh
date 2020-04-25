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
