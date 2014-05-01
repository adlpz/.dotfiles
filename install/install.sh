#!/bin/bash

set -e

cd $(dirname $0)

DOTFILES=../

echo "~> Linking..."

FILES=$(ls -A $DOTFILES | grep -v install | xargs -n1 -IF echo $(pwd)/../F)
for f in $FILES
    do
            if ln -s $f ~/ 2>/dev/null; then
                        echo "Linked $(basename $f)"
                            else
                                        echo "$(basename $f) already exists in the home directory. Skipping..."
                                            fi
                                            done
