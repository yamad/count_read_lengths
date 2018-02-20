-- | Tally sequencing read lengths in a FASTQ file
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import System.Environment

type ReadLength = Int
type Count = Integer
newtype ReadCount = RC { unRC :: M.Map ReadLength Count }

-- | COUNT LENGTH per line display format
instance Show ReadCount where
  show = unlines . map showCount . M.toList . unRC
    where showCount (l,n) = unwords [show n, show l]


main :: IO ()
main = getArgs >>=
  mapM B.readFile >>=
  mapM_ (print . fastqReadLengths)

-- | tally all read lengths, given FASTQ file contents
fastqReadLengths :: B.ByteString -> ReadCount
fastqReadLengths = RC . tally . map B.length . fastqReads
  where fastqReads = every 4 . drop 1 . B.lines

-- | tally occurrences
tally :: (Num a, Ord k) => [k] -> M.Map k a
tally xs = M.fromListWith (+) $ zip xs (repeat 1)

-- | take every nth element, starting with first element
every :: Int -> [a] -> [a]
every _ [] = []
every n (x:xs) = x : every n (drop (n-1) xs)
