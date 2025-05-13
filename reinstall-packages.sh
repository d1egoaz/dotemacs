#!/bin/bash

rm -rf eln-cache
rm -rf straight/build*

cd straight/repos
for dir in */; do
  if [ -d "$dir/.git" ]; then
    echo "Updating $dir..."
    (cd "$dir" && git pull) || echo "Failed to update $dir, continuing..."
  else
    echo "$dir is not a Git repository, skipping..."
  fi
done

