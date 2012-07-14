Tools for making it easier to integrate Haskell and Objective-C.

Right now, the goal is to make it easy to invoke Haskell from Objective-C. Eventually, invoking Objective-C from Haskell will receive attention too.

# Getting Started

**To obtain Objective-Haskell:**

    $ git clone git://github.com/jspahrsummers/ObjectiveHaskell.git
    $ cd ObjectiveHaskell
    $ git submodule update --init --recursive

You will also need to have the latest `haskell-platform` installed via [Homebrew](https://github.com/mxcl/homebrew/).

**To add Objective-Haskell to a project:**

 1. Add ObjectiveHaskell.xcodeproj to your main project file.
 2. In your project's Info pane, under Configurations:
    - Under Debug and next to the line with your project, select ObjectiveHaskell-Debug from the drop-down.
    - Under Release, select ObjectiveHaskell-Release.
 3. In your project's Build Settings, add the ObjectiveHaskell folder to your Header Search Paths. (It does not need to be recursive.)
 4. For each target in your project that may have Haskell sources:
    - Set up a dependency on the ObjectiveHaskell target.
    - Add a build rule for files matching `*.hs` which executes the following
      script:
      
      `/usr/local/bin/ghc -c -O "$INPUT_FILE_PATH"`.
      
      and has the output files:

        * `$(INPUT_FILE_DIR)/$(INPUT_FILE_BASE)_stub.h`
        * `$(INPUT_FILE_DIR)/$(INPUT_FILE_BASE).o`
 5. `#import "ObjectiveHaskell.h"` in your project's prefix header.
 6. In files where you want to use Haskell functions, `#import "MODULE_stub.h"`, where `MODULE` is the name of your Haskell file.

Some of these steps will probably be automated or simplified in the future.

Note that when adding Haskell sources to a target, Xcode puts them in the Copy Bundle Resources phase by default. You will have to manually add them to the Compile Sources phase instead.

# License

This project is released into the public domain, and can be used for free and without attribution for any use.

# See Also

 * [Using Haskell in an Xcode Cocoa project](http://www.haskell.org/haskellwiki/Using_Haskell_in_an_Xcode_Cocoa_project)
 * [Using the FFI](http://www.haskell.org/haskellwiki/GHC/Using_the_FFI)
 * [hoc](http://code.google.com/p/hoc/)
