Historical Cryptography
=======================

A library serving two purposes:

* Historical cryptographic algorithms
* Algorithms to break the aforementioned

## Implementation Usage

```haskell
import Codec.Encryption.Historical.Caesar.Implementation

main = putStrLn encrypted
  where
      bidirectional = caesar 13 -- ROT13
      encrypted     = encrypt caesar "Hello World"
```

## Analysis Usage

```haskell
import Codec.Encryption.Historical.Caesar.Implementation
import Codec.Encryption.Historical.Caesar.Analysis

main :: IO ()
main = do
  cyper_text <- readFile "super_secret.txt"
  corpus     <- readFile "snow_white_abridged.txt"

  let
    stripped_corpus = caesar_encode 0 corpus
    histo           = histogram stripped_corpus
    decrypted       = crack histo cyper_text

  print decrypted
```

## Tests

To run the test-suite, just call `make test`.

## Cryptography Sources

* [History of Cryptography](http://en.wikipedia.org/wiki/History_of_cryptography)
* [Classical Cipher       ](http://en.wikipedia.org/wiki/Classical_cipher)
* [Caesar Cipher          ](http://en.wikipedia.org/wiki/Caesar_cipher)

## Corpus Sources

* [Snow White             ](http://www.gutenberg.org/dirs/etext04/grimm10a.txt)

# TODO

QuikCheck can cause issues with GHCi. To work around this load files with -

    ghci -Wall -package QuickCheck-2.5.1.1 -i. "%"
