
module Main where

import Codec.Encryption.Historical.Caesar.Implementation
import System.Environment (getArgs)
import Safe

main :: IO ()
main = getArgs >>= encoder . map readMay

encoder :: [Maybe Int] -> IO ()
encoder [Just offset] = do

  plain  <- getContents

  let cypher = caesar_decode offset plain

  putStrLn cypher

encoder _ = putStrLn "Usage: cat foo.txt | decode_caesar <offset>"
