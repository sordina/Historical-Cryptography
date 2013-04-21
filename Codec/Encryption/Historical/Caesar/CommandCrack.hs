
module Main where

import Codec.Encryption.Historical.Caesar.Analysis
import Codec.Encryption.Historical.Caesar.Implementation

main :: IO ()
main = do

  cypher <- getContents
  corpus <- readFile "/usr/share/dict/snow_white_abridged.txt" -- TODO: Allow user to specify

  let
    stripped_corpus = caesar_encode 0 corpus
    histo           = histogram stripped_corpus
    decrypted       = crack histo cypher

  putStrLn $ decrypted
