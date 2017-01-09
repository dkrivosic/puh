module CSVUtils
( parseCSV
, showCSV
, colFields
, readCSV
, writeCSV
) where

  import Data.List
  import Data.List.Split

  type Separator = String
  type Document  = String
  type CSV = [Entry]
  type Entry = [Field]
  type Field = String

  malformedCSV :: CSV -> Bool
  malformedCSV csv = not $ all ((==(length $ head csv)) . length) csv

  parseCSV :: Separator -> Document -> CSV
  parseCSV sep doc
    | head sep `notElem` doc = error "The separator does not occur in the text"
    | malformedCSV csv       = error "The CSV file is not well-formed"
    | otherwise              = csv
      where csv = map (splitOn sep) . lines $ doc

  showCSV :: Separator -> CSV -> Document
  showCSV sep csv
    | malformedCSV csv = error "The CSV file is not well-formed"
    | otherwise        = unlines $ map (intercalate sep) csv

  colFields :: Int -> CSV -> [Field]
  colFields n csv
    | (length $ head csv) <= n || n < 0 = error "There is no column specified in the CSV document"
    | otherwise                         = map (!! n) csv

  readCSV :: Separator -> FilePath -> IO CSV
  readCSV sep f = do
    s <- readFile f
    return $ parseCSV sep s

  writeCSV :: Separator -> FilePath -> CSV -> IO ()
  writeCSV sep f csv = do
    let s = showCSV sep csv
    writeFile f s
