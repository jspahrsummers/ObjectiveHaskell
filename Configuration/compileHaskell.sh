#!/bin/bash
#
# Script for compiling *.hs files in an Xcode project, meant to be used with
# a build rule.

declare -a imports

# Use Objective-C header search paths to locate Haskell modules too
for dir in $HEADER_SEARCH_PATHS
do
    # Because Haskell module names should be qualified, we need to move one
    # folder up (which will usually be from the target folder to the project
    # folder)
    dir=`dirname "$dir"`

    imports=( "${imports[@]}" "-i$dir" )
done

lockfile="$INPUT_FILE_DIR/ghc.lock"

lockfile -r 0 -l 60 -s 1 "$lockfile"
if [ $? -eq 0 ]
then
    find "$INPUT_FILE_DIR" -name '*.hs' -print0 | \
    xargs -0 /usr/local/bin/ghc \
        -XForeignFunctionInterface -XTemplateHaskell \
        -Werror -fwarn-incomplete-patterns -fwarn-dodgy-imports -fwarn-dodgy-exports -fwarn-unused-binds -fwarn-hi-shadowing -fwarn-identities -fwarn-monomorphism-restriction \
        -framework Foundation ${imports[@]} \
        -c -O -threaded --make \
        "$@"

    st=$?
    rm -f "$lockfile"

    exit $st
fi
