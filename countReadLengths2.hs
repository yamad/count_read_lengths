-- | Tally sequencing read lengths in a FASTQ file

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as M

import Data.List (splitAt)
import GHC.Int (Int64)
import System.Environment

type ReadLength = Int64
type Count = Integer
newtype ReadCount = RC { unRC :: M.Map ReadLength Count }


main :: IO ()
-- main = do
--   args <- getArgs
--   contents <- mapM B.readFile args
--   let counts = map fastqReadLengths contents
--   mapM_ print counts
main = getArgs >>=
  mapM B.readFile >>=
  mapM_ (print . fastqReadLengths)


-- | tally all read lengths, given FASTQ file contents
fastqReadLengths :: B.ByteString -> ReadCount
fastqReadLengths =
  let fastqReadLines = every 4 . drop 1 . B.lines
  in RC . tally . map B.length . fastqReadLines


-- | tally occurrences
tally :: (Num a, Ord k) => [k] -> M.Map k a
tally xs = M.fromListWith (+) $ zip xs (repeat 1)


-- | take every nth element, starting with first element
every :: Int -> [a] -> [a]
every _ [] = []
every n (x:xs) = x : every n (drop (n-1) xs)


-- | COUNT LENGTH per line display format
instance Show ReadCount where
  show = unlines . map showKV . M.toList . unRC
    where showKV (k,v) = unwords [show v, show k]
