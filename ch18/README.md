Exercises on p. 436:

1. Modify the `App` type synonym to swap the order of `ReaderT` and `WriterT`.
   What effect does this have on the `runApp` execution function?
   
    The question seem to have a typo, because the `App` type is defined as follows:

    ```haskell
    -- file: ch18/UglyStack.hs
    type App = ReaderT AppConfig (StateT AppState IO)
    ```

    Swapping `ReaderT` and `StateT` will change the example from the book
    so that the last line, `in runStateT (runReaderT k config) state`
    will be `in runReaderT (runStateT k state) config`:

    ```haskell
    -- file: ch18/UglyStack.hs
    runApp :: App a -> Int -> IO (a, AppState)
    runApp k maxDepth =
        let config = AppConfig maxDepth
            state = AppState 0
        in runReaderT (runStateT k state) config
    ```

    I was answering this question after changes for the following questions 2 and 3
    were already done. I did the following two commits to swap transformers
    in my code:

    ```
    commit 753f65178503e07303cc2624013a77c248bd5113 (HEAD -> master, origin/master)
    Author: Ilya Silvestrov <tellary@gmail.com>
    Date:   Tue Apr 14 20:06:14 2020 -0700

        Swap `WriterT` and `ReaderT` in `App`
    ```

    ```
    commit 66612187813e742ee5acd5a8cb2f6e612c4244c7 (HEAD -> master, origin/master)
    Author: Ilya Silvestrov <tellary@gmail.com>
    Date:   Tue Apr 14 20:59:03 2020 -0700

        Swap `ReaderT` and `StateT` in `App`
    ```

2. Add the WriterT transformer to the App monad transformer stack.
   Modify `runApp` to work with this new setup.

    Done. See `newtype App` in 
    [`CountFoldersContent.hs`](./CountFoldersContent.hs).

3. Rewrite the `constrainedCount` function to record results using
   the `WriterT` transformer in your new App stack.

    Done in the `countFoldersContent0` in
    [`CountFoldersContent.hs`](./CountFoldersContent.hs).
