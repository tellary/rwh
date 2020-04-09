Exercises at the end of Chapter 15:

1. Using QuickCheck, write a test for an action in the `MonadHandle` monad,
   to see if it tries to write to a file handle that is not open.
   Try it out on `safeHello`.

    "catches an illegal operation on a closed handle" test for
    both `HandleIO` and `LogIO` exists and passes
    in [`MonadHandleSpec`](./MonadHandleSpec.hs).
    
    I omit the QuickCheck part of the exercise in favor of the
    `Hspec` test, because
    
    1. I don't understand what does QuickCheck has to do with this test.
       What kind of arbitrary data I should supply to test
       a property against?
       
        Should I create an arbitrary instance to generate an open and a closed
        `System.IO (Handle)`? This doesn't make sense to me and, see
        next item (2).

    2. The book doesn't explain how to write QuickCheck properties on monadic code.
    
        I'm not in the mood of learning the
        [`Test.QuickCheck.Monadic`][quick-check-monadic] package right
        now, given that I don't see it's useful in the context of open/closed
        handles. It'll be a future task.

2. Write an action that tries to write to a file handle that it has closed.
   Does your test catch this bug?

    See `writeToClosedHandle` in [`MonadHandleSpec`](./MonadHandleSpec.hs)
    and tests that use it.

3. In a form-encoded string, the same key may appear several times,
   with or without values, e.g. `key&key=1&key=2`.
   What type might you use to represent the values associated with a key
   in this sort of string?
   Write a parser that correctly captures all of the information.

    I decided to go with the `[(String, Maybe String)]` representation
    in [`URLQueryParser`](../ch16/parsec/URLQueryParser.hs).

[quick-check-monadic]: https://hackage.haskell.org/package/QuickCheck-2.14/docs/Test-QuickCheck-Monadic.html
