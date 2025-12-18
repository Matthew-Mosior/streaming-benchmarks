#!/bin/bash

set -euo pipefail

# Default number of runs
DEFAULT_RUNS=20
RUNS=${1:-$DEFAULT_RUNS}

# Validate that RUNS is a positive integer
if ! [[ "$RUNS" =~ ^[0-9]+$ && "$RUNS" -gt 0 ]]; then
  echo "Usage: $0 [number_of_runs]"
  echo "  number_of_runs must be a positive integer"
  exit 1
fi

# List of size options to benchmark
SIZES=("small" "medium" "large" "extra_large" "extra_extra_large" "massive")

# Resolve paths to the stack-built executables
PACK_IDRIS2_STREAMS_EXEC="../idris2-streams-string-search/build/exec/idris2-streams-string-search"
STACK_CONDUIT_EXEC=$(cd ../haskell-conduit-string-search && stack exec which haskell-conduit-string-search-exe)
STACK_PIPES_EXEC=$(cd ../haskell-pipes-string-search && stack exec which haskell-pipes-string-search-exe)

# List of commands to benchmark (executables)
PROJECTS=(
  "$PACK_IDRIS2_STREAMS_EXEC"
  "$STACK_CONDUIT_EXEC"
  "$STACK_PIPES_EXEC"
)

# Human-friendly names for display
NAMES=(
  "idris2-streams"
  "conduit"
  "pipes"
)

# Get the Chez Scheme version (for markdown)
get_chez_scheme_version_md() {
  echo "Chez Scheme version used by idris2-streams-string-search: $(scheme --version 2>&1)"
}

# Output GHC version for conduit.
get_ghc_version_stack_conduit_md() {
  echo "GHC version used by haskell-conduit-string-search: $(cd ../haskell-conduit-string-search && stack ghc -- --version 2>&1)"
}

# Output GHC version for pipes.
get_ghc_version_stack_pipes_md() {
  echo "GHC version used by haskell-pipes-string-search: $(cd ../haskell-pipes-string-search && stack ghc -- --version 2>&1)"
}

# Output cpu info
get_cpu_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "CPU: $(sysctl -n machdep.cpu.brand_string)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "CPU: $(grep -m1 "model name" /proc/cpuinfo | cut -d ':' -f2- | sed 's/^ *//' 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

# Output memory info
get_memory_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "RAM: $(sysctl -n hw.memsize)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "RAM: $(sed -n 's/^MemTotal:[ \t]*//p' /proc/meminfo 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

# Output os info
get_os_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "OS: $(sw_vers -productName)" "$(sw_vers -productVersion)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "OS: $(grep ^PRETTY_NAME= /etc/os-release | cut -d= -f2- | tr -d '"' 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

# Set output file
OUTPUT_FILE="../README.md"

# Build projects
cd ../idris2-streams-string-search && pack build
cd ../haskell-conduit-string-search && stack build
cd ../haskell-pipes-string-search && stack build
cd ../scripts

# Initialize the output markdown file
echo "# Introduction" > "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "The following benchmarking data was generated via the \`benchmark.sh\` bash script found in \`scripts\`." >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "This benchmark requires [hyperfine](https://github.com/sharkdp/hyperfine)." >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "These benchmarks perform 20 runs per command.  Each run searches for the pattern "ATAAA" in a text composed of a random distribution of the characters A, T, G, and C)." >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "# Test Data" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "This benchmark runs on files containing a random distribution of the characters A, T, G and C." >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "The following table details the number of characters (A,T,G,C) contained in each of the sizes:" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "| Size | Number of characters |" >> "$OUTPUT_FILE"
echo "|:---|---:|" >> "$OUTPUT_FILE"
echo "| \`small\` | \`1000\` |" >> "$OUTPUT_FILE"
echo "| \`medium\` | \`100000\` |" >> "$OUTPUT_FILE"
echo "| \`large\` | \`1000000\` |" >> "$OUTPUT_FILE"
echo "| \`extra_large\` | \`10000000\` |" >> "$OUTPUT_FILE"
echo "| \`extra_extra_large\` | \`100000000\` |" >> "$OUTPUT_FILE"
echo "| \`massive\` | \`1000000000\` |" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "# Compiler/Codegen Information" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_chez_scheme_version_md)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_ghc_version_stack_conduit_md)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_ghc_version_stack_pipes_md)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "# Architecture" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_cpu_info)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_memory_info)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "$(get_os_info)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "# Results" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Generate test data
echo "Generating test data ..."
mkdir ../resources
./generate_nucleotides.sh 1000 "../resources/nucleotides_small.txt"
./generate_nucleotides.sh 100000 "../resources/nucleotides_medium.txt"
./generate_nucleotides.sh 1000000 "../resources/nucleotides_large.txt"
./generate_nucleotides.sh 10000000 "../resources/nucleotides_extra_large.txt"
./generate_nucleotides.sh 100000000 "../resources/nucleotides_extra_extra_large.txt"
./generate_nucleotides.sh 1000000000 "../resources/nucleotides_massive.txt"

# Run all benchmarks by size first
for SIZE in "${SIZES[@]}"; do
  echo "Running all benchmarks for size: $SIZE"
  echo "## $SIZE" >> "$OUTPUT_FILE"
  echo "" >> "$OUTPUT_FILE"
  for i in "${!PROJECTS[@]}"; do
    CMD="${PROJECTS[$i]}"
    NAME="${NAMES[$i]}"
    echo "  Benchmarking $NAME ($CMD) ..."
    RUN_CMD="$CMD $SIZE"
    hyperfine --runs "$RUNS" --warmup 1 "$RUN_CMD < /dev/null > /dev/null" \
      --export-markdown "temp_${NAME}_${SIZE}.md"
    if [[ "$i" -eq 0 ]]; then
      cat "temp_${NAME}_${SIZE}.md" >> "$OUTPUT_FILE"
    else
      tail -n +3 "temp_${NAME}_${SIZE}.md" >> "$OUTPUT_FILE"
    fi
    rm "temp_${NAME}_${SIZE}.md"
  done
  echo "" >> "$OUTPUT_FILE"
done

# Remove test data
rm ../resources/nucleotides_*.txt
rmdir ../resources
