# Introduction

The following benchmarking data was generated via the `benchmark.sh` bash script found in `scripts`.

This benchmark requires [hyperfine](https://github.com/sharkdp/hyperfine).

These benchmarks perform 20 runs per command (converting fahrenheit to celsius).

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

Chez Scheme version used by idris2-streams-fahrenheit: 10.2.0

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
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit small < /dev/null > /dev/null` | 57.3 ± 0.5 | 56.3 | 58.2 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe small < /dev/null > /dev/null` | 11.8 ± 0.4 | 11.0 | 12.6 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe small < /dev/null > /dev/null` | 11.2 ± 0.4 | 10.4 | 12.0 | 1.00 |

## medium

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit medium < /dev/null > /dev/null` | 159.5 ± 0.6 | 158.5 | 160.5 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe medium < /dev/null > /dev/null` | 337.2 ± 3.2 | 331.7 | 342.0 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe medium < /dev/null > /dev/null` | 278.6 ± 1.7 | 276.5 | 281.5 | 1.00 |

## large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit large < /dev/null > /dev/null` | 1.036 ± 0.015 | 1.018 | 1.090 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe large < /dev/null > /dev/null` | 3.319 ± 0.018 | 3.286 | 3.352 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe large < /dev/null > /dev/null` | 2.733 ± 0.013 | 2.709 | 2.756 | 1.00 |

## extra_large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit extra_large < /dev/null > /dev/null` | 9.732 ± 0.083 | 9.537 | 9.878 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe extra_large < /dev/null > /dev/null` | 33.247 ± 0.168 | 32.908 | 33.545 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe extra_large < /dev/null > /dev/null` | 27.264 ± 0.092 | 27.112 | 27.463 | 1.00 |

