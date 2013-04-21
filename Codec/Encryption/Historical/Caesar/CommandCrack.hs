
module Main where

import Codec.Encryption.Historical.Caesar.Analysis
import Codec.Encryption.Historical.Caesar.Implementation
import System.Environment

main :: IO ()
main = getArgs >>= resolve

resolve :: [String] -> IO ()

resolve ["--help"] = usage
resolve ["-h"] = usage

resolve [f] = do
  cypher <- getContents
  corpus <- readFile f

  let
    stripped_corpus = caesar_encode 0 corpus
    histo           = histogram stripped_corpus
    decrypted       = crack histo cypher

  putStrLn $ decrypted

resolve [] = resolve ["/usr/share/dict/snow_white_abridged.txt"]

resolve _ = usage

usage :: IO ()
usage = putStrLn "Usage: cat <cypher> | crack_caeser [corpus]"
