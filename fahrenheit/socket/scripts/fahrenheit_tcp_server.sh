#!/bin/bash

HOST="localhost"
PORT=12345
DURATION=20

MAX_CHUNK=$((0xfff))   # 4095 bytes
SMALL_CHUNK_MAX=$((0xff)) # 255 bytes

echo "Waiting for TCP server on $HOST:$PORT..."
until nc -z "$HOST" "$PORT" 2>/dev/null; do
    sleep 0.05
done
echo "Server is up. Starting throughput test..."

(
    start_time=$(date +%s)
    end_time=$((start_time + DURATION))
    count=0

    buffer=""
    buf_size=0

    while :; do
        now=$(date +%s)
        (( now >= end_time )) && break

        # Generate temperature
        whole=$(( RANDOM % 81 + 20 ))
        fraction=$(( RANDOM % 10 ))

        if (( fraction == 0 )); then
            line="${whole}\n"
        else
            line="${whole}.${fraction}\n"
        fi

        line_len=${#line}
        buffer+="$line"
        buf_size=$((buf_size + line_len))
        count=$((count + 1))

        # Randomly force a small chunk with delimiter
        if (( RANDOM % 50 == 0 && buf_size < SMALL_CHUNK_MAX )); then
            buffer+="\r\n\r\n"
            printf "%s" "$buffer"
            buffer=""
            buf_size=0
            continue
        fi

        # Flush when buffer hits max chunk size
        if (( buf_size >= MAX_CHUNK )); then
            printf "%s" "$buffer"
            buffer=""
            buf_size=0
        fi
    done

    # Flush remainder
    if (( buf_size > 0 )); then
        printf "%s" "$buffer"
    fi

    printf "Sent %d readings in %d seconds\n" "$count" "$DURATION" >&2

) | nc "$HOST" "$PORT"
