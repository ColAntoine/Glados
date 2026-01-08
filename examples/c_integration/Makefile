##
## EPITECH PROJECT, 2025
## glados - C Integration Example
## File description:
## Makefile for building Flux C integration example
##

NAME = a.out

# Directories
FLUX_DIR = src/flux
C_DIR = src/c
OBJ_DIR = obj

# Glados compiler (built from parent project)
GLADOS_ROOT = ../..
GLADOS = $(GLADOS_ROOT)/glados

# Source files (wildcards)
FLUX_SRC = $(wildcard $(FLUX_DIR)/*.flux)
C_SRC = $(wildcard $(C_DIR)/*.c)

# Object files
FLUX_OBJ = $(patsubst $(FLUX_DIR)/%.flux,$(OBJ_DIR)/%_flux.o,$(FLUX_SRC))
C_OBJ = $(patsubst $(C_DIR)/%.c,$(OBJ_DIR)/%.o,$(C_SRC))
ALL_OBJ = $(FLUX_OBJ) $(C_OBJ)

# Compiler flags
CC = clang
CFLAGS = -Wall -Wextra

all: glados $(NAME)

# Always run make in glados directory to ensure it's up to date
glados:
	$(MAKE) -C $(GLADOS_ROOT)

# Create object directory
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Compile Flux files to object files with C ABI
$(OBJ_DIR)/%_flux.o: $(FLUX_DIR)/%.flux | $(OBJ_DIR)
	$(GLADOS) -c $< -o $(OBJ_DIR)/$*_flux -cabi

# Compile C source files
$(OBJ_DIR)/%.o: $(C_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Link everything together
$(NAME): $(ALL_OBJ)
	$(CC) $(ALL_OBJ) -o $(NAME)

run: $(NAME)
	./$(NAME)

clean:
	rm -rf $(OBJ_DIR)
	rm -f *.ll *.s

fclean: clean
	rm -f $(NAME)

re: fclean all

# Debug target to show detected files
debug:
	@echo "Flux sources: $(FLUX_SRC)"
	@echo "C sources: $(C_SRC)"
	@echo "Flux objects: $(FLUX_OBJ)"
	@echo "C objects: $(C_OBJ)"

.PHONY: all glados run clean fclean re debug
