##
## EPITECH PROJECT, 2025
## glados
## File description:
## Makefile
##

NAME = glados
SRC = src/Main.hs

all:
	stack build
	cp "`stack path --local-install-root`/bin/$(NAME)" .

run: all
	./$(NAME)

test:
	stack test

clean:
	stack clean
	rm -f src/Main
	rm -rf test/__pycache__ .pytest_cache test/.pytest_cache
	rm -f *.ll

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).cabal
	rm -rf dist-newstyle
	rm -rf .hpc
	rm -f *.tix
	rm -f glados-test.log
	rm -f *.ll
	stack purge

re: clean all

coverage:
	cabal test --enable-coverage

.PHONY: all run clean re coverage test
