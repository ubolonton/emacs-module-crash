#!/usr/bin/env bash

EMACS=emacs

EMACS -batch -l ert \
      -L $PWD \
      -l test.el \
      -f ert-run-tests-batch-and-exit
