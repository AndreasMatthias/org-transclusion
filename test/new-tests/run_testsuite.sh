#!/bin/bash

emacs -batch -l testsuite-gh.el -f ert-run-tests-batch-and-exit
