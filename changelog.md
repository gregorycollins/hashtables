# Hashtables changelog

## 1.2.2.0
  - Bumped vector bounds.

  - Added `lookupIndex` and `nextByIndex` functions.
  - Add `mutate` function.

Thanks to contributors:

  - Vykintas Baltrušaitis.
  - Franklin Chen
  - Iavor Diatchki
  - Eric Mertins

## 1.2.1.1
  - Bumped vector bounds.

## 1.2.1.0

  - Fixed an FFI import typo bug
    (https://github.com/gregorycollins/hashtables/pull/27), thanks to Thijs
    Alkemade for the fix.

## 1.2.0.2

  - Fixed serious bug (https://github.com/gregorycollins/hashtables/issues/24)
    in basic hash table making it impossible to reliably store more than 64k
    elements (after shortening the hash code arrays to 16 bits I neglected to
    realize that I was storing item counts using the same array type).

## 1.2.0.1

  - Fixed bug in C code re: clang interpreting "inline" strictly according to
    (insane) C99 semantics: http://clang.llvm.org/compatibility.html#inline

  - Fixed a compile bug affecting versions of base older than 4.4.

  - Changed int type from Int to Word in CheapPseudoRandomBitStream to fix an
    integer overflow warning.

## 1.2.0.0

### Switch to smaller hash codes to go faster and save space.

Before, in the basic and cuckoo hash tables, we were storing full
machine-word-sized hash codes in the table so that we could quickly search a
whole cache line for a key (or a combination of keys) without branching.

It turns out that a full machine word is not really necessary for this
application; switching to a 16-bit key will very slightly increase the number
of hash collisions within buckets (meaning that we'll compare more keys), but
will pay big dividends in terms of:

  - reduced wastage of RAM

  - searching more keys at once, allowing buckets to grow bigger

  - more cache hits on the hash codes array.

### Other

  - Dependency bumps

  - Fix definitions of forwardSearch2 and forwardSearch3 in PORTABLE mode (also
    used on Windows) to match C implementations.

## 1.1.2.1
  - Fixes for GHC 7.8 compatibility.

## 1.1.2.0
  - Bump allowable versions of hashable, primitive, and vector, blacklisting
    some bad hashable versions

  - Add specialize pragmas for fromListWithSizeHint

## 1.1.0.2
  - Use CPP to allow compilation against base 4.2/4.3.

## 1.1.0.1
  - Re-added SPECIALIZE pragmas that were previously removed.

## 1.1.0.0
  - Add 'fromListWithSizeHint'
  - 'fromList': don't be strict in the list argument

## 1.0.1.8
Bump vector and primitive dependencies.

## 1.0.1.7
Fix bug in C FFI code (not correctly promoting CInt to Int).

## 1.0.1.6
Fix for benchmark suite .cabal file.

## 1.0.1.5
Added benchmark suite.

## 1.0.1.4
Bump test-framework dependency.

## 1.0.1.3
Bump testsuite dependencies.

## 1.0.1.2
Fix testsuite on Windows.

## 1.0.1.1
Build fix for Windows.

## 1.0.1.0

Bugfix for http://github.com/gregorycollins/hashtables/issues/1 (Basic.lookup
loops).
