#!/bin/bash
#
# Script for compiling *.hs files in an Xcode project, meant to be used with
# a build rule.

IMPORTS=""

# Use Objective-C header search paths to locate Haskell modules too
for DIR in $HEADER_SEARCH_PATHS
do
    # Because Haskell module names should be qualified, we need to move one
    # folder up (which will usually be from the target folder to the project
    # folder)
    DIR=`dirname "$DIR"`

    IMPORTS="$IMPORTS -i$DIR"
done

lockfile -r 0 ghc.lock
if [ $? -eq 0 ]
then
    /usr/local/bin/ghc \
        -XForeignFunctionInterface -XTemplateHaskell \
        -Werror -fwarn-incomplete-patterns -fwarn-dodgy-imports -fwarn-dodgy-exports -fwarn-unused-binds -fwarn-hi-shadowing -fwarn-identities -fwarn-monomorphism-restriction \
        -framework Foundation $IMPORTS \
        -c -O -threaded --make \
        "$@" \
        "$INPUT_FILE_PATH"

    rm -f ghc.lock
fi
