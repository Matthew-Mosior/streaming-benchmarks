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
| `extra_extra_large` | `100000000` |
| `massive` | `1000000000` |

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
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search small < /dev/null > /dev/null` | 60.9 ± 1.4 | 58.7 | 63.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe small < /dev/null > /dev/null` | 6.4 ± 0.2 | 6.0 | 6.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe small < /dev/null > /dev/null` | 6.5 ± 0.3 | 5.8 | 7.0 | 1.00 |

## medium

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search medium < /dev/null > /dev/null` | 60.5 ± 1.3 | 57.9 | 62.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe medium < /dev/null > /dev/null` | 7.3 ± 0.3 | 6.5 | 7.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe medium < /dev/null > /dev/null` | 7.0 ± 0.2 | 6.6 | 7.4 | 1.00 |

## large

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search large < /dev/null > /dev/null` | 80.5 ± 1.1 | 78.9 | 82.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe large < /dev/null > /dev/null` | 12.9 ± 0.6 | 12.0 | 14.1 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe large < /dev/null > /dev/null` | 11.3 ± 0.2 | 10.9 | 12.0 | 1.00 |

## extra_large

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search extra_large < /dev/null > /dev/null` | 234.1 ± 4.5 | 225.6 | 247.9 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe extra_large < /dev/null > /dev/null` | 46.7 ± 0.8 | 45.5 | 49.1 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe extra_large < /dev/null > /dev/null` | 44.3 ± 0.7 | 43.0 | 45.5 | 1.00 |

## extra_extra_large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search extra_extra_large < /dev/null > /dev/null` | 1.758 ± 0.029 | 1.716 | 1.848 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe extra_extra_large < /dev/null > /dev/null` | 376.9 ± 1.5 | 373.8 | 380.9 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe extra_extra_large < /dev/null > /dev/null` | 347.8 ± 1.4 | 344.8 | 350.8 | 1.00 |

## massive

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-string-search/build/exec/idris2-streams-string-search massive < /dev/null > /dev/null` | 16.856 ± 0.177 | 16.520 | 17.122 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-conduit-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-conduit-string-search-exe massive < /dev/null > /dev/null` | 3.665 ± 0.005 | 3.657 | 3.674 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/string-searching/haskell-pipes-string-search/.stack-work/install/x86_64-linux/138cdacc7a878f35e9368a91ce84e1cdcbac46757bd78575931e872b0bf479ed/9.10.3/bin/haskell-pipes-string-search-exe massive < /dev/null > /dev/null` | 3.377 ± 0.004 | 3.371 | 3.385 | 1.00 |

