# Project variables
SRC:=src
BIN:=bin
TARGET:=euler

# Haskell tools
GHC:=ghc
HLINT:=hlint

.PHONY = all build run clean

# Default target
all: build

# Build the project
build:
	mkdir -p $(BIN)
	$(GHC) -o $(TARGET) -c $(SRC)/* -outputdir=$(BIN)

# Run the project
run: build
	./$(TARGET)

# Clean up build artifacts
clean:
	rm -rf $(BIN)/*
