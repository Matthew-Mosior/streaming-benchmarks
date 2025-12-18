# Introduction

The following benchmarking data was generated via the `benchmark.sh` bash script found in `scripts`.

This benchmark requires [hyperfine](https://github.com/sharkdp/hyperfine).

These benchmarks perform 20 runs per command.  Each run searches for the pattern ATAAA in a text composed of a random distribution of the characters A, T, G, and C).

# Test Data

This benchmark runs on files containing a random distribution of the characters A, T, G and C.

The following table details the number of characters (A,T,G,C) contained in each of the sizes:

| Size | Number of characters |
|:---|---:|
| `small` | `1000` |
| `medium` | `100000` |
| `large` | `1000000` |
| `extra_large` | `10000000` |

# Compiler/Codegen Information

Chez Scheme version used by idris2-streams-string-search: 10.2.0

GHC version used by haskell-conduit-string-search: 
Warning: Stack has not been tested with GHC versions 9.10 and above, and using 9.10.3, this may
         fail.

Warning: Stack has not been tested with Cabal versions 3.12 and above, but version 3.12.1.0 was
         found, this may fail.
The Glorious Glasgow Haskell Compilation System, version 9.10.3

GHC version used by haskell-pipes-string-search: 
Warning: Stack has not been tested with GHC versions 9.10 and above, and using 9.10.3, this may
         fail.

Warning: Stack has not been tested with Cabal versions 3.12 and above, but version 3.12.1.0 was
         found, this may fail.
The Glorious Glasgow Haskell Compilation System, version 9.10.3

# Architecture

CPU: AMD Ryzen 9 5900X 12-Core Processor

RAM: 127345212 kB

OS: Ubuntu 25.04

# Results

## small

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search small < /dev/null > /dev/null` | 59.8 ± 1.3 | 58.1 | 62.2 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe small < /dev/null > /dev/null` | 6.5 ± 0.3 | 6.0 | 6.9 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe small < /dev/null > /dev/null` | 6.4 ± 0.3 | 5.8 | 6.8 | 1.00 |

## medium

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search medium < /dev/null > /dev/null` | 59.6 ± 1.4 | 57.9 | 63.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe medium < /dev/null > /dev/null` | 7.1 ± 0.2 | 6.5 | 7.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe medium < /dev/null > /dev/null` | 6.9 ± 0.3 | 6.1 | 7.3 | 1.00 |

## large

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search large < /dev/null > /dev/null` | 80.0 ± 1.0 | 78.8 | 82.1 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe large < /dev/null > /dev/null` | 12.7 ± 0.3 | 12.2 | 13.3 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe large < /dev/null > /dev/null` | 11.3 ± 0.3 | 10.8 | 12.0 | 1.00 |

## extra_large

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search extra_large < /dev/null > /dev/null` | 232.7 ± 2.7 | 230.4 | 243.1 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe extra_large < /dev/null > /dev/null` | 46.6 ± 0.9 | 45.2 | 48.8 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe extra_large < /dev/null > /dev/null` | 42.4 ± 0.5 | 41.6 | 43.3 | 1.00 |

