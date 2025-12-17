#!/bin/bash
set -euo pipefail

PORT=${1:-12345}
DURATION=${2:-20}  # Duration in seconds to run the server

echo "Starting TCP server on port $PORT for $DURATION seconds..."

timeout "$DURATION" nc -l "$PORT" -k -q 0 | while true; do
    # 1-in-10 chance to emit a string instead of a number
    if (( RANDOM % 10 == 0 )); then
        # length 3–8
        len=$(( RANDOM % 6 + 3 ))

        # generate alphabetic string
        str=$(
            tr -dc 'a-zA-Z' </dev/urandom | head -c "$len"
        )

        printf "%s\n" "$str" || break
    else
        whole=$(( RANDOM % 81 + 20 ))
        fraction=$(( RANDOM % 10 ))

        if (( fraction == 0 )); then
            printf "%d\n" "$whole" || break
        else
            printf "%d.%d\n" "$whole" "$fraction" || break
        fi
    fi

    sleep 0.01
done

echo "TCP server on port $PORT stopped after $DURATION seconds."
