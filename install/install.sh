#!/bin/bash


FMT="%-40s%s\n"
print_tabbed() {
    printf $FMT "$1" "$2"
}
set -e

cd $(dirname $0)

DOTFILES=..

echo "~> Linking..."

#FILES=$(ls -A $DOTFILES | grep -v 'install\|\.git' | xargs -n1 -IF echo $(pwd)/../F)

WD=$(pwd)
FILES=$(find $DOTFILES $DOTFILES/.config -maxdepth 1 -not -name ".config" -a -not -name "$DOTFILES" -a -not -name "install" -a -not -name ".git*")
for f in $FILES
do
    SRCPATH=$WD/$f
    DSTPATH=~/$(realpath --relative-to=$DOTFILES $f)
    COMMAND="ln -sT $SRCPATH $DSTPATH"
    if [ -L $DSTPATH ]; then
        print_tabbed "$DSTPATH" "already exists as a symbolic link pointing to $(readlink -f $DSTPATH)"
    elif [ -f $DSTPATH ]; then
        print_tabbed $FMT "$DSTPATH" "already exists as a regular file"
    elif [ -d $DSTPATH ]; then
        print_tabbed "$DSTPATH" "already exists as a directory."
    elif [ -e $DSTPATH ]; then
        print_tabbed "$DSTPATH" "already exists."
    else
        echo "$SRCPATH" "Linked to $DSTPATH"
        $COMMAND
    fi
done
