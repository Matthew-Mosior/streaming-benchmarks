#!/bin/bash

# Usage: ./generate_nucleotides.sh <char_count> <output_file> [motif_chance]
# motif_chance = 1 in N positions inserts "ATAAA" (default: 1/200)

set -euo pipefail

count=${1:-1000}
outfile=${2:-dna.txt}
motif_divisor=${3:-200}

motif="ATAAA"
motif_len=5
alphabet=(A T G C)

if [ -z "${outfile:-}" ]; then
  echo "Usage: $0 <char_count> <output_file> [motif_chance]"
  exit 1
fi

> "$outfile"

chars_written=0
line_pos=0

write_char() {
  printf "%s" "$1" >> "$outfile"
  ((++chars_written))
  ((++line_pos))

  if (( line_pos == 80 )); then
    printf "\n" >> "$outfile"
    line_pos=0
  fi
}

while (( chars_written < count )); do
  remaining=$((count - chars_written))

  if (( remaining >= motif_len )) && (( RANDOM % motif_divisor == 0 )); then
    for ((i = 0; i < motif_len; i++)); do
      write_char "${motif:i:1}"
      (( chars_written >= count )) && break
    done
  else
    write_char "${alphabet[RANDOM % 4]}"
  fi
done

# Final newline if needed
if (( line_pos > 0 )); then
  printf "\n" >> "$outfile"
fi

echo "Generated $count characters in '$outfile'"
