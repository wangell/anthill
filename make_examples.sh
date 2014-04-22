#!/bin/bash
mkdir -p examples_output
ls examples/ -1 | xargs -I % ./anthill examples/% examples_output/%.html
