
module Main where

import Codec.Encryption.Historical.XOR.Analysis
import Codec.Encryption.Historical.Utilities.Histogram
import System.Environment

main :: IO ()
main = getArgs >>= resolve

resolve :: [String] -> IO ()

resolve ("--help":_) = usage
resolve ("-h":    _) = usage

resolve [f] = resolve ["100",f]

resolve [l,f] = do
  cypher <- getContents
  corpus <- readFile f

  let
    histo           = histogram corpus
    decrypted       = crack (read l) histo cypher

  putStrLn $ decrypted

resolve [] = resolve ["/usr/share/dict/snow_white_abridged.txt"]

resolve _ = usage

usage :: IO ()
usage = putStrLn "Usage: cat <cypher> | crack_xor [corpus]"
