UM32
====

A 32-bit universal machine based on the 2006 ICFP Programming Contest um32 codex.

This was (initially) done over a weekend inbetween wedding events while staying at a hotel in the
mountains. I have been toying around with designing a language using Haskell, and have been trying
to decide if I eventually want to use a virtual machine or not. I saw a Stack Overflow post pointing
to the UM32, saying it was feasible for a programmer to do in a couple hours, which I had while
waiting for different events. I also wanted to take the opportunity to use StateT.

StateT is interesting, although I do feel I need to further study Monad Transformers to completely
"get it." Given my use case, I would almost want to just use a world-passing-style for the machine.
I think I should also add in an error condition for when the machine errors out, rather than simply halting.
Unfortunately I did not have enough time to really consider how I would optimally do that.

The code isn't the best, and there are a couple of architecture mistakes I made that I will hopefully
come back to change. I also want to organize the code, comment better, and finally learn HUnit to provide
test coverage. I will also hopefully take the time to put together a basic Assembler.

Usage
====
$ ghc Emulator

$ Emulator filename.um