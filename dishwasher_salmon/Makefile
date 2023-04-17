CC?=gcc
CCOPTS?=-std=c11 -Iinclude -Wall -Wextra -pedantic -O2

HDRS=$(wildcard include/*.h)
OBJS=$(patsubst src/%.c, build/%.o, $(wildcard src/*.c))

ifeq ($(OS),Windows_NT)
	EXEEXT=.exe
else
	EXEEXT=
endif

all: bin/dishwasher_salmon$(EXEEXT)

clean:
	-rm build/*
	-rm bin/*

build/%.o: src/%.c $(HDRS)
	$(CC) $(CCOPTS) -c $< -o $@

bin/dishwasher_salmon$(EXEEXT): $(OBJS) $(HDRS)
	$(CC) $(CCOPTS) $(OBJS) -o $@

.PHONY: all
