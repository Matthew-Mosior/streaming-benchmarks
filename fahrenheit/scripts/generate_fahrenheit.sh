#!/bin/bash

# Usage: ./generate_fahrenheit.sh <count> <filename>
# Example: ./generate_fahrenheit.sh 10 temps.txt

count=${1:-10}
outfile=$2

if [ -z "$outfile" ]; then
  echo "Usage: $0 <count> <output_file>"
  exit 1
fi

# Clear the file or create it fresh
> "$outfile"

for ((i = 0; i < count; i++)); do
  whole=$(( RANDOM % 81 + 20 ))
  fraction=$(( RANDOM % 10 ))

  if [ "$fraction" -eq 0 ]; then
    echo "${whole}" >> "$outfile"
  else
    echo "${whole}.${fraction}" >> "$outfile"
  fi
done

echo "Generated $count temperatures in '$outfile'"
