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
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit small < /dev/null > /dev/null` | 56.6 ± 0.5 | 55.4 | 57.3 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe small < /dev/null > /dev/null` | 12.1 ± 0.4 | 11.1 | 12.8 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe small < /dev/null > /dev/null` | 11.3 ± 0.3 | 10.8 | 11.8 | 1.00 |

## medium

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit medium < /dev/null > /dev/null` | 158.5 ± 0.5 | 157.7 | 159.6 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe medium < /dev/null > /dev/null` | 339.3 ± 2.6 | 334.9 | 343.7 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe medium < /dev/null > /dev/null` | 283.3 ± 2.6 | 279.5 | 287.5 | 1.00 |

## large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit large < /dev/null > /dev/null` | 1.024 ± 0.009 | 1.009 | 1.038 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe large < /dev/null > /dev/null` | 3.339 ± 0.017 | 3.311 | 3.372 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe large < /dev/null > /dev/null` | 2.766 ± 0.012 | 2.746 | 2.801 | 1.00 |

## extra_large

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit extra_large < /dev/null > /dev/null` | 9.729 ± 0.131 | 9.578 | 10.126 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-conduit-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-conduit-fahrenheit-exe extra_large < /dev/null > /dev/null` | 33.269 ± 0.163 | 33.044 | 33.649 | 1.00 |
| `/home/matthewmosior/Software/git/streaming-benchmarks/fahrenheit/haskell-pipes-fahrenheit/.stack-work/install/x86_64-linux/2acef0ab84f6b1d2cc473baef3f4b2ab50325060cd28867e1c86ba26b4ae7bef/9.8.4/bin/haskell-pipes-fahrenheit-exe extra_large < /dev/null > /dev/null` | 27.712 ± 0.079 | 27.572 | 27.822 | 1.00 |

