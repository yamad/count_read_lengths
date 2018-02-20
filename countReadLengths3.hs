-- | Tally sequencing read lengths in a FASTQ file

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (group, sort)
import GHC.Int (Int64)
import System.Environment

type ReadCount = [(Int64, Int)]

main :: IO ()
main = getArgs >>=
  mapM B.readFile >>=
  mapM_ (putStr . showReadCount . fastqReadLengths)

-- | tally all read lengths, given FASTQ file contents
fastqReadLengths :: B.ByteString -> ReadCount
fastqReadLengths = counter . map B.length . fastqReads
  where fastqReads = every 4 . drop 1 . B.lines

-- | tally occurrences
counter :: Ord a => [a] -> [(a, Int)]
counter s = map (\x -> (head x, length x)) . group . sort $ s

-- | take every nth element, starting with first element
every :: Int -> [a] -> [a]
every _ [] = []
every n (x:xs) = x : every n (drop (n-1) xs)

-- | COUNT LENGTH per line display format
showReadCount :: ReadCount -> String
showReadCount = unlines . map (\(k,v) -> unwords [show v, show k])
