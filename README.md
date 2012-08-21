Tools for making it easier to integrate Haskell and Objective-C.

**Only Mac OS X is supported at the moment. iOS may be supported in the future.**

# Getting Started

**To obtain Objective-Haskell:**

    $ git clone git://github.com/jspahrsummers/ObjectiveHaskell.git
    $ cd ObjectiveHaskell
    $ git submodule update --init --recursive

You will also need to have the following packages installed using [Homebrew](https://github.com/mxcl/homebrew/) and `cabal` (part of the `haskell-platform`):

    $ brew install haskell-platform
    $ cabal install aeson

**To add Objective-Haskell to a project:**

 1. Add ObjectiveHaskell.xcodeproj to your main project file.
 2. In your project's Info pane, under Configurations:
    - Under Debug and next to the line with your project, select ObjectiveHaskell-Debug from the drop-down.
    - Under Release, select ObjectiveHaskell-Release.
 3. In your project's Build Settings, add the ObjectiveHaskell folder within the repository to your Header Search Paths. (It does not need to be recursive.)
 4. For each target in your project that may have Haskell sources:
    - Set up a dependency on and link in the ObjectiveHaskell library.
    - Add a build rule for files matching `*.hs` which executes the following
      script (where `ObjectiveHaskell` is the path to the repository):
      
      `ObjectiveHaskell/Configuration/compileHaskell.sh`
      
      and has the output files:

        * `$(INPUT_FILE_DIR)/$(INPUT_FILE_BASE)_stub.h`
        * `$(INPUT_FILE_DIR)/$(INPUT_FILE_BASE).o`
     
      If you want to pass any additional flags to GHC, you may pass them to `compileHaskell.sh` within the build rule.
 5. `#import "ObjectiveHaskell.h"` in your project's prefix header.
 6. In files where you want to use Haskell functions, `#import "MODULE_stub.h"`, where `MODULE` is the name of your Haskell file.

Some of these steps will probably be automated or simplified in the future.

Note that when adding Haskell sources to a target, Xcode puts them in the Copy Bundle Resources phase by default. You will have to manually add them to the Compile Sources phase instead.

# License

**Copyright (c) 2012 Justin Spahr-Summers**

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# See Also

 * [Using Haskell in an Xcode Cocoa project](http://www.haskell.org/haskellwiki/Using_Haskell_in_an_Xcode_Cocoa_project)
 * [Using the FFI](http://www.haskell.org/haskellwiki/GHC/Using_the_FFI)
 * [hoc](http://code.google.com/p/hoc/)
