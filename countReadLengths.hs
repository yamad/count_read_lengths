-- | Tally sequencing read lengths in a FASTQ file
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import System.Environment

type ReadLength = Int
type Count = Integer
newtype ReadCount = RC { unRC :: M.Map ReadLength Count }


main :: IO ()
main = getArgs >>=
  mapM tallyFileReads >>=
  mapM_ (putStr . show)


tallyFileReads :: FilePath -> IO ReadCount
tallyFileReads f = fastqReadLengths <$> B.readFile f


-- | tally all read lengths, given FASTQ file contents
fastqReadLengths :: B.ByteString -> ReadCount
fastqReadLengths = RC . tally . map B.length . fastqReadLines
  where
    fastqReadLines = takeStart 2 4 . B.lines
    tally xs = M.fromListWith (+) $ zip xs (repeat 1)


-- | COUNT LENGTH per line display format
instance Show ReadCount where
  show = unlines . map showKV . M.toList . unRC
    where showKV (k,v) = unwords [show v, show k]


-- | take every nth element, starting with mth element
takeStart :: Int -> Int -> [a] -> [a]
takeStart m n xs = takeEvery n $ drop (m - 1) xs
  where
    -- | take every nth element, starting with first element
    takeEvery :: Int -> [a] -> [a]
    takeEvery _ [] = []
    takeEvery n (x:xs) = x : takeEvery n (drop (n - 1) xs)
