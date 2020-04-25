{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module MyExceptTParse
  ( ParseState (..)
  , (<|>)
  , empty
  , char
  , many
  , optional
  , parse
  , satisfy
  , some
  , string
  ) where

import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad.Trans (lift)
import MyExceptT
import MyState
import Text.Printf         (printf)

data ParseState = ParseState {
  stOffset :: Int,
  stString :: String
  } deriving (Eq, Show)

newtype MyExceptTParse a = P
  { unMyExceptTParse :: MyExceptT String (MyState ParseState) a
  } deriving
  ( Functor, Applicative, Monad
  , MyMonadError String
  )

parse :: MyExceptTParse a -> String -> (Either String a, ParseState)
parse p str
  = myRunState (runMyExceptT . unMyExceptTParse $ p) $ ParseState 0 str

instance MyMonadState s m => MyMonadState s (MyExceptT e m) where
  get = lift get
  put = lift . put

instance Alternative (MyExceptTParse) where
  empty   = myThrowError $ "Empty parser"
  a <|> b = P . myExceptT $ do
    ea <- runMyExceptT . unMyExceptTParse $ a
    case ea of
      r1@(Right _)  -> return r1
      Left _        -> do
        eb <- runMyExceptT . unMyExceptTParse $ b
        case eb of
          r2@(Right _) -> return r2
          l @(Left  _) -> return l

char c = satisfy (== c) $ printf "'%c'" c

satisfy :: (Char -> Bool) -> String -> MyExceptTParse Char
satisfy p desc = P $ do
  st <- get
  let str = stString st
  let off = stOffset st
  case str of
    [] -> myThrowError $ printf "Unexpected eof while %s is expected" desc
    c:cs
      | p c -> do
          put $ st { stOffset = off + 1, stString = cs }
          return c
      | otherwise -> myThrowError
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
         $ printf (   "Unexpected eof: string \"%s\" is expected, "
                   ++ "but \"%s\" is available") s s'
    else
      if s == s'
      then do
        put $ st { stOffset = off + l, stString = drop l str }
        return s
      else myThrowError
           $ printf "Unexpected string \"%s\" while \"%s\" is expected" s' s

eof :: MyExceptTParse a
eof = P $ do
  str <- stString <$> get
  case str of
    c:_ -> myThrowError $ printf "EOF expected but '%c' found" c
