Historical Cryptography
=======================

A library serving two purposes:

* Historical cryptographic algorithms
* Algorithms to break the aforementioned

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
