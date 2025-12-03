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

# Build Flux (Part 2 language) located in `FLUX/` scaffold
flux:
	cd FLUX && stack build
	cd FLUX && cp "`stack path --local-install-root`/bin/flux" ..

flux-run: flux
	./flux

clean:
	stack clean
	cd FLUX && stack clean || true
	rm -f src/Main
	rm -rf test/__pycache__ .pytest_cache test/.pytest_cache

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).cabal
	rm -f flux
	stack purge

re: clean all

.PHONY: all run clean re flux flux-run
