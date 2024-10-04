#!/bin/sh

# M-x tree-sitter-langs-install-latest-grammar
# then: ./move-tree-sitter-langs.sh

for file in ~/.emacs.d/straight/build-*/tree-sitter-langs/bin/*.dylib; do
  cp -f "$file" ~/.emacs.d/tree-sitter/libtree-sitter-$(basename "$file")
  echo "File: libtree-sitter-$(basename "$file") copied!"
done
