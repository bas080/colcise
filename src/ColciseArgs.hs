{-# LANGUAGE DeriveDataTypeable #-}
-- TODO: check if the langage extensions are correct
-- http://hackage.haskell.org/package/cmdargs

module ColciseArgs where

import System.Console.CmdArgs

data ColciseArgs = ColciseArgs
  { trim :: Bool
  , delimeter :: String
  , separator :: String
  } deriving (Typeable, Data, Show, Eq)

colciseArgs :: ColciseArgs
colciseArgs = ColciseArgs {
  trim = False,
  delimeter = " ",
  separator = " "
}

