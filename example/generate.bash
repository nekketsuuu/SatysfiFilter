#!/bin/bash

set -eux

p () {
  # Note: markdown_github is deprecated. Here I just want to use `fenced_code_ttributes`.
  pandoc -f markdown_github+fenced_code_attributes -t html5 --filter ./SatysfiFilter.sh -o "generated/$1.html" "$1.md"
}

./clean.sh
p index
p child
