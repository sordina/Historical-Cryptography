help:
	@echo "Targets:"
	@cat Makefile | grep -i '^[a-z]*:' | sed 's/^/    /' | sed 's/://'

home:
	open "https://github.com/sordina/Historical-Cryptography"

issues:
	open "https://github.com/sordina/Historical-Cryptography/issues"

newissue:
	open "https://github.com/sordina/Historical-Cryptography/issues/new"

test:
	cabal configure --enable-tests
	cabal build
	cabal test
