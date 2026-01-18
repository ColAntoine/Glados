##
## EPITECH PROJECT, 2025
## glados
## File description:
## Makefile
##

NAME = glados
SRC = src/Main.hs

all:
	stack build --test --no-run-tests
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
	stack clean
	stack test glados:coverage-tests --coverage
	@echo "\nCoverage report generated at:"
	@echo ".stack-work/install/x86_64-linux/*/hpc/index.html"

.PHONY: all run clean re coverage test
