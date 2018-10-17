#!/bin/bash

set -eux

p () {
  pandoc -f gfm -t html5 --filter ./SatysfiFilter.sh -o "generated/$1.html" "$1.md"
}

p index
p child
