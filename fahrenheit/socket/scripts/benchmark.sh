#!/bin/bash

set -euo pipefail

# Durations to run the TCP server
DURATIONS=(20 40 80)

# Base TCP server port
BASE_PORT=12345

# Executables
PACK_IDRIS2_STREAMS_EXEC="../idris2-streams-fahrenheit/build/exec/idris2-streams-fahrenheit"
STACK_CONDUIT_EXEC=$(cd ../haskell-conduit-fahrenheit && stack exec which haskell-conduit-fahrenheit-exe)
STACK_PIPES_EXEC=$(cd ../haskell-pipes-fahrenheit && stack exec which haskell-pipes-fahrenheit-exe)

PROJECTS=(
  "$PACK_IDRIS2_STREAMS_EXEC"
  "$STACK_CONDUIT_EXEC"
  "$STACK_PIPES_EXEC"
)

NAMES=(
  "idris2-streams"
  "conduit"
  "pipes"
)

# Markdown output file
OUTPUT_FILE="../README.md"

# ----------------------
# Helper functions
# ----------------------

get_chez_scheme_version_md() {
  echo "Chez Scheme version used by idris2-streams-fahrenheit: $(scheme --version 2>&1)"
}

get_ghc_version_stack_conduit_md() {
  echo "GHC version used by haskell-conduit-fahrenheit: $(cd ../haskell-conduit-fahrenheit && stack ghc -- --version 2>&1)"
}

get_ghc_version_stack_pipes_md() {
  echo "GHC version used by haskell-pipes-fahrenheit: $(cd ../haskell-pipes-fahrenheit && stack ghc -- --version 2>&1)"
}

get_cpu_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "CPU: $(sysctl -n machdep.cpu.brand_string)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "CPU: $(grep -m1 "model name" /proc/cpuinfo | cut -d ':' -f2- | sed 's/^ *//' 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

get_memory_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "RAM: $(sysctl -n hw.memsize)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "RAM: $(sed -n 's/^MemTotal:[ \t]*//p' /proc/meminfo 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

get_os_info() {
  if [[ "$(uname)" == "Darwin" ]]; then
    echo -n "OS: $(sw_vers -productName)" "$(sw_vers -productVersion)"
  elif [[ "$(uname)" == "Linux" ]]; then
    echo -n "OS: $(grep ^PRETTY_NAME= /etc/os-release | cut -d= -f2- | tr -d '"' 2>&1)"
  else
    echo "Unknown OS: $(uname)"
  fi
}

# ----------------------
# Build projects
# ----------------------
cd ../idris2-streams-fahrenheit && pack build
cd ../haskell-conduit-fahrenheit && stack build
cd ../haskell-pipes-fahrenheit && stack build
cd ../scripts

# ----------------------
# Initialize README.md
# ----------------------
{
echo "# TCP Streaming Fahrenheit to Celsius Benchmark"
echo ""
echo "This benchmark streams Fahrenheit values over TCP to Idris2 and Haskell clients."
echo ""
echo "# Compiler/Codegen Information"
echo ""
echo "$(get_chez_scheme_version_md)"
echo ""
echo "$(get_ghc_version_stack_conduit_md)"
echo ""
echo "$(get_ghc_version_stack_pipes_md)"
echo ""
echo "# Architecture"
echo ""
echo "$(get_cpu_info)"
echo ""
echo "$(get_memory_info)"
echo ""
echo "$(get_os_info)"
echo ""
echo "# Results"
echo ""
} > "$OUTPUT_FILE"

# ----------------------
# Benchmark function
# ----------------------
run_benchmark() {
  local duration=$1
  local client_cmd=$2
  local client_name=$3
  local port=$4

  echo "Running $client_name for $duration seconds on port $port..."
  echo "### $client_name, $duration seconds, port $port" >> "$OUTPUT_FILE"

  # Start TCP server in background
  ./fahrenheit_tcp_server.sh "$port" "$duration" &
  SERVER_PID=$!

  # Wait briefly for server to start
  sleep 1.0

  # Run client and capture stdout + stderr
  OUTPUT=$("$client_cmd" "$port" 2>&1)

  # Print client output to terminal
  echo "$OUTPUT"

  # Append client output to README.md
  {
    echo '```'
    echo "$OUTPUT"
    echo '```'
    echo ""
  } >> "$OUTPUT_FILE"

  # Wait for server to finish
  wait $SERVER_PID
}

# ----------------------
# Benchmark loop with unique ports
# ----------------------
for duration_index in "${!DURATIONS[@]}"; do
  duration=${DURATIONS[$duration_index]}
  echo "## TCP Benchmark Duration: $duration seconds" >> "$OUTPUT_FILE"
  echo "" >> "$OUTPUT_FILE"

  for i in "${!PROJECTS[@]}"; do
    # Assign a unique port per duration+client
    PORT=$((BASE_PORT + duration_index*10 + i))
    run_benchmark "$duration" "${PROJECTS[$i]}" "${NAMES[$i]}" "$PORT"
  done
done
