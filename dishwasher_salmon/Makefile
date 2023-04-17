CC?=gcc
CCOPTS?=-std=c11 -Iinclude -Wall -Wextra -pedantic -static -O2
CCOPTS_DBG?=-std=c11 -Iinclude -Wall -Wextra -pedantic -static -g

HDRS=$(wildcard include/*.h)
OBJS=$(patsubst src/%.c, build/%.o, $(wildcard src/*.c))
OBJS_DBG=$(patsubst src/%.c, build/%_dbg.o, $(wildcard src/*.c))

ifeq ($(OS),Windows_NT)
	EXEEXT=.exe
else
	EXEEXT=
endif

all: bin/dishwasher_salmon$(EXEEXT) bin/dishwasher_salmon_dbg$(EXEEXT)

clean:
	-rm build/*
	-rm bin/*

$(OBJS): build/%.o: src/%.c $(HDRS)
	$(CC) $(CCOPTS) -c $< -o $@

$(OBJS_DBG): build/%_dbg.o: src/%.c $(HDRS)
	$(CC) $(CCOPTS_DBG) -c $< -o $@

bin/dishwasher_salmon$(EXEEXT): $(OBJS) $(HDRS)
	$(CC) $(CCOPTS) $(OBJS) -o $@

bin/dishwasher_salmon_dbg$(EXEEXT): $(OBJS_DBG) $(HDRS)
	$(CC) $(CCOPTS_DBG) $(OBJS_DBG) -o $@

.PHONY: all
