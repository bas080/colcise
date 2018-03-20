module Main where

import System.Environment (getArgs)
import System.IO (getContents)
import Colcise
import ColciseArgs
import System.Console.CmdArgs

-- TODO: use this example to improve argument parsing
-- https://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/

main :: IO ()
main = do
  args <- cmdArgs colciseArgs
  contents <- getContents
  putStrLn $ toFormattedTable args contents
