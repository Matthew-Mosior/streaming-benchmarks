# Introduction

The following benchmarking data was generated via the `benchmark.sh` bash script found in `scripts`.

This benchmark requires [hyperfine](https://github.com/sharkdp/hyperfine).

These benchmarks perform 20 runs per command.

# Test Data

This benchmark runs on files containing fahrenheit temperatures to the tenths decimal place.

The following table details the number of fahrenheit temperatures in each of the sizes:

| Size | Number of fahrenheit temperatures |
|:---|---:|
| `small` | `1000` |
| `medium` | `100000` |
| `large` | `1000000` |
| `extra_large` | `10000000` |

# Compiler/Codegen Information

Chez Scheme version used by idris2-streams-fahrenheit: 10.1.0

GHC version used by haskell-conduit-fahrenheit: The Glorious Glasgow Haskell Compilation System, version 9.8.4

GHC version used by haskell-pipes-fahrenheit: The Glorious Glasgow Haskell Compilation System, version 9.8.4

# Architecture

CPU: AMD Ryzen 9 5900X 12-Core Processor

RAM: 127345280 kB

OS: Ubuntu 24.10

# Results

## small

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit small < /dev/null > /dev/null` | 56.5 ± 0.6 | 54.8 | 57.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe small < /dev/null > /dev/null` | 11.9 ± 0.3 | 11.2 | 12.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe small < /dev/null > /dev/null` | 11.4 ± 0.3 | 10.7 | 11.9 | 1.00 |

## medium

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit medium < /dev/null > /dev/null` | 159.4 ± 2.2 | 157.6 | 168.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe medium < /dev/null > /dev/null` | 339.5 ± 3.1 | 333.4 | 346.1 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe medium < /dev/null > /dev/null` | 282.6 ± 2.6 | 278.5 | 289.7 | 1.00 |

## large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit large < /dev/null > /dev/null` | 1.019 ± 0.006 | 1.009 | 1.030 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe large < /dev/null > /dev/null` | 3.337 ± 0.015 | 3.316 | 3.367 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe large < /dev/null > /dev/null` | 2.763 ± 0.012 | 2.742 | 2.787 | 1.00 |

## extra_large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit extra_large < /dev/null > /dev/null` | 9.669 ± 0.064 | 9.537 | 9.803 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe extra_large < /dev/null > /dev/null` | 33.198 ± 0.127 | 32.995 | 33.476 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe extra_large < /dev/null > /dev/null` | 27.516 ± 0.085 | 27.377 | 27.664 | 1.00 |

