##
## EPITECH PROJECT, 2025
## mypandoc
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

clean:
	stack clean
	rm -f src/Main
	rm -rf test/__pycache__ .pytest_cache test/.pytest_cache

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).cabal
	stack purge

re: clean all

.PHONY: all run clean re
