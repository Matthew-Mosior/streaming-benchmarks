#!/bin/bash

HOST="localhost"
PORT=12345
DURATION=20 # seconds

echo "Waiting for TCP server on $HOST:$PORT..."
until nc -z $HOST $PORT 2>/dev/null; do
    sleep 0.1
done
echo "Server is up. Starting throughput test..."

(
    start_time=$(date +%s)
    end_time=$((start_time + DURATION))
    count=0

    while :; do
        now=$(date +%s)
        if [ "$now" -ge "$end_time" ]; then
            break
        fi

        # Generate fast random temperature
        whole=$(( RANDOM % 81 + 20 ))
        fraction=$(( RANDOM % 10 ))

        if [ "$fraction" -eq 0 ]; then
            printf "%s\n" "$whole"
        else
            printf "%s.%s\n" "$whole" "$fraction"
        fi

        count=$((count + 1))
    done

    # Print to STDERR so it doesn't go into the TCP stream
    printf "Sent %d readings in %d seconds\n" "$count" "$DURATION" >&2

) | nc $HOST $PORT
