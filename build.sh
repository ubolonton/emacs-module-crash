#!/usr/bin/env bash
gcc -ggdb3 -Wall -fPIC -c main.c && gcc -shared -o testing.so main.o
