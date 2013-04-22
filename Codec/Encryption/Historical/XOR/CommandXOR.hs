
module Main where

import Codec.Encryption.Historical.XOR.Implementation
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= encoder

encoder :: [String] -> IO ()
encoder [key] = do

  plain  <- getContents

  let cypher = xor_decode key plain

  putStrLn cypher

encoder _ = putStrLn "Usage: cat foo.txt | encode_xor <key>"
