{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module LimitedStream where

import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, Stream, uncons, runP)
import Text.ParserCombinators.Parsec (getInput, setInput)

type LimitCharParser u = Parsec (LimitedStream [Char]) u

data LimitedStream s = LimitedStream Int s | UnlimitedStream s

instance Monad m => Stream (LimitedStream [tok]) m tok where
  uncons (LimitedStream _ [])     = return $ Nothing
  uncons (LimitedStream 0 _ )     = return $ Nothing
  uncons (LimitedStream n (t:ts)) = return (Just (t, LimitedStream (n - 1) ts))

  uncons (UnlimitedStream [])     = return $ Nothing
  uncons (UnlimitedStream (t:ts)) = return (Just (t, UnlimitedStream ts))

limit n p = do
  input <- getInput
  case input of
    UnlimitedStream wrappedInput -> do
      setInput (LimitedStream n wrappedInput)
      p
    _                     -> p

parseLimitedFromFile :: LimitCharParser () a -> FilePath -> IO (Either ParseError a)
parseLimitedFromFile p fname
    = do input <- readFile fname
         return (runP p () fname $ UnlimitedStream input)
