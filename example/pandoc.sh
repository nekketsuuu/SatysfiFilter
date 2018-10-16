#!/bin/sh

pandoc -f markdown -t html --filter ./SatysfiFilter.sh -o test.html test.md
