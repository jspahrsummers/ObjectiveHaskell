#!/bin/bash
#
# Script for compiling *.hs files in an Xcode project, meant to be used with
# a build rule.

/usr/local/bin/ghc -XForeignFunctionInterface -XTemplateHaskell -framework Foundation -c -O "$INPUT_FILE_PATH"
