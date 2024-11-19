# Hashtables changelog

## 1.4.0

Replace deprecated Mutable Array function, which modifies the signature of the `length`
function and hence the API.
Support more recent compilers.

## 1.3.1

Fix Noncanonical mappend definition warning.
Support more recent compilers.


## 1.3

Support Hashable 1.4. This new version of Hashable drops the Eq constraint, so the Eq constraint
needs to be dropped in the SPECIALIZE statements in Hashtables.

## 1.2.4.2

Cabal file: add missing other-modules
Silence import warnings. Know that we require ghc >= 7.8.
Fix build with GHC 9.2

## 1.2.4.1

Update some test suite dep upper bounds.

## 1.2.4.0

 - Fix a [correctness bug](https://github.com/gregorycollins/hashtables/issues/55)
with cuckoo hash tables and the new `mutate` function introduced in 1.2.3.0.

 - Bring test suite into main .cabal file

## 1.2.3.4

Fix build with GHC 8.8.

## 1.2.3.3

Fix build with certain versions of `primitive` (thx again Carter)

## 1.2.3.2

  - CPP fix for breakage caused by primitive 0.7 (thx Carter)

  - Fix some haddock syntax errors (thx Frederik Hanghøj Iversen)

  - Fix typo in module reference (thx Don Allen)

## 1.2.3.1

  - Fix building with GHC <7.10 (thx Vanessa McHale)

## 1.2.3.0

  - update for Semigroup/monoid breakage with GHC 8.4 (thx Fumiaki Kinoshita)

  - Implement mutateST and mutateIO (thx Andy Morris)

  - Fix build breakage w/ "pre" function (thx Andy Morris)

## 1.2.2.1

  - Adjusted base lower bound (it was incorrect), bumped some testsuite +
    benchmark bounds.

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
