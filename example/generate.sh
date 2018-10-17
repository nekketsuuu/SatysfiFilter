#!/bin/sh

pandoc -f markdown -t html --filter ./SatysfiFilter.sh -o generated/test.html test.md
