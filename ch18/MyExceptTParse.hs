{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module MyExceptTParse
  ( ParseError (..)
  , ParseResult
  , ParseState (..)
  , (<|>)
  , digit
  , empty
  , eof
  , char
  , int
  , many
  , many2
  , many3
  , optional
  , parse
  , satisfy
  , some
  , some2
  , some3
  , string
  ) where

import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad       (when)
import Control.Monad.Trans (lift)
import Data.Char           (isDigit)
import MyExceptT
import MyState
import Text.Printf         (printf)

data ParseState = ParseState {
  stOffset :: Int,
  stString :: String
  } deriving (Eq, Show)

data ParseError
  = NumericOverflow
  | EndOfInput Char
  | EmptyParse
  | Chatty String
  deriving (Eq, Show)

newtype MyExceptTParse a = P
  { unMyExceptTParse :: MyExceptT ParseError (MyState ParseState) a
  } deriving
  ( Functor, Applicative, Monad
  , MyMonadError ParseError
  )

type ParseResult a = (Either ParseError a, ParseState)

parse :: MyExceptTParse a -> String -> ParseResult a
parse p str
  = myRunState (runMyExceptT . unMyExceptTParse $ p) $ ParseState 0 str

instance MyMonadState s m => MyMonadState s (MyExceptT e m) where
  get = lift get
  put = lift . put

instance Alternative (MyExceptTParse) where
  empty   = myThrowError $ EmptyParse
  a <|> b = a `myCatchError` \_ -> b

char c = satisfy (== c) $ printf "'%c'" c

satisfy :: (Char -> Bool) -> String -> MyExceptTParse Char
satisfy p desc = P $ do
  st <- get
  let str = stString st
  let off = stOffset st
  case str of
    [] -> myThrowError
          . Chatty
          $ printf "Unexpected eof while %s is expected" desc
    c:cs
      | p c -> do
          put $ st { stOffset = off + 1, stString = cs }
          return c
      | otherwise -> myThrowError
                     . Chatty
                     $ printf "Unexpected char '%c' while %s is expected" c desc

string :: String -> MyExceptTParse String
string s = P $ do
  st <- get
  let str = stString st
  let off = stOffset st
  let l   = length   s
  let s'  = take l   str
  if null s'
    then myThrowError
         . Chatty
         $ printf (   "Unexpected eof: string \"%s\" is expected, "
                   ++ "but \"%s\" is available") s s'
    else
      if s == s'
      then do
        put $ st { stOffset = off + l, stString = drop l str }
        return s
      else myThrowError
           . Chatty
           $ printf "Unexpected string \"%s\" while \"%s\" is expected" s' s

eof :: MyExceptTParse ()
eof = P $ do
  str <- stString <$> get
  case str of
    c:_ -> myThrowError $ EndOfInput c
    _   -> return ()

digit :: MyExceptTParse Char
digit = satisfy isDigit $ printf "digit"

int :: (Show a, Read a, Integral a) => MyExceptTParse a
int = do
  minus <- optional (char '-')
  ds    <- some digit
  let i =  read ds
  when (show i /= ds) $ myThrowError NumericOverflow
  case minus of
    Just _  -> return $ -i
    Nothing -> return $ i

-- Re-implementations of `Alternative (many)`
-- for the sake of Chapter 19 exercise 1 at p. 465

-- Re-implementation via `(<|>)`
some2 p = (:) <$> p <*> many2 p
many2 p = (:) <$> p <*> many2 p <|> pure []

-- Re-implementation via `myCatchError`
some3 p = (:) <$> p <*> many3 p
many3 p = (:) <$> p <*> many3 p `myCatchError` \_ -> pure []
