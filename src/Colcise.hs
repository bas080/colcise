module Colcise where

import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (pack, strip, unpack)
import Text.Regex (Regex, mkRegex, splitRegex)
import ColciseArgs

data LineLayout
  = ColumnPadding
  | ColumnValue
  | ColumnSeparator
  deriving (Show)

type FileLayout = [LineLayout]

charToLineLayout :: Char -> LineLayout
charToLineLayout 'v' = ColumnValue
charToLineLayout 'p' = ColumnPadding
charToLineLayout 's' = ColumnSeparator

toLineLayout :: String -> FileLayout
toLineLayout = concat . repeat . map charToLineLayout

-- layout
type Width = Int

type ColumnLayout = Width

type TableLayout = [ColumnLayout]

widths :: [String] -> [Int]
widths = map length

widest :: [Int] -> [Int] -> [Int]
widest = zipWith max

merge :: [a] -> [a] -> [a]
merge a b = a ++ snd (splitAt (length a) b)

toTableLayout :: Table -> TableLayout
toTableLayout =
  foldl
    (\acc row -> (widest (merge acc (widths row)) (merge (widths row) acc)))
    []

-- delimeter
delimeterRegex :: String -> Bool -> Regex
delimeterRegex s True = delimeterRegex ("(" ++ s ++ ")+") False
delimeterRegex s False = mkRegex s

-- table
type Cell = String

type Row = [Cell]

type Table = [Row]

type Delimeter = Regex

toTable :: Delimeter -> String -> Table
toTable delimeter content = map (splitRegex delimeter) (lines content)

mapTable :: (Row -> Row) -> Table -> Table
mapTable = map

mapRow :: (Cell -> Cell) -> Row -> Row
mapRow = map

strippedTable :: Table -> Table
strippedTable = mapTable $ mapRow $ unpack . strip . pack

-- TODO: append this after an error occurs. Do make sure to write to error
-- Try 'cat --help' for more information.
-- formatted table
padding :: Width -> String -> String
padding width str = replicate (width - length str) ' '

data Alignment
  = LeftAlignment
  | RightAlignment
  deriving (Show)

paddedCell :: Alignment -> Int -> String -> String
paddedCell LeftAlignment width str = str ++ padding width str
paddedCell RightAlignment width str = padding width str ++ str

-- helper
when :: Bool -> (a -> a) -> a -> a
when value fn input =
  if value
    then fn input
    else input

toFormattedTable :: ColciseArgs -> String -> String
toFormattedTable args contents =
  init $
  unlines $
  map
    (intercalate (separator args) .
     zipWith3 paddedCell (repeat LeftAlignment) tableLayout)
    table
  where
    ignoreSubsequent = True
    table =
      when (trim args) strippedTable $
      toTable (delimeterRegex (delimeter args) ignoreSubsequent) contents
    tableLayout = toTableLayout table
