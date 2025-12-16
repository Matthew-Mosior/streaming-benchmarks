#!/bin/bash

HOST="localhost"
PORT=12345
DURATION=20

MAX_CHUNK=$((0xfff))        # 4095 bytes
SMALL_CHUNK_MAX=$((0xff))   # 255 bytes

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

    flush_buffer() {
        printf "%s" "$buffer"
        buffer=""
        buf_size=0
    }

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

        # If adding this line would exceed MAX_CHUNK, flush first
        if (( buf_size + line_len > MAX_CHUNK )); then
            flush_buffer
        fi

        buffer+="$line"
        buf_size=$((buf_size + line_len))
        count=$((count + 1))

        # Randomly force a small chunk (< 0xff bytes)
        if (( RANDOM % 50 == 0 )); then
            # Flush current buffer if non-empty
            if (( buf_size > 0 )); then
                flush_buffer
            fi

            # Build a small chunk with delimiter
            target=$(( (RANDOM % (SMALL_CHUNK_MAX - 8)) + 8 ))
            small_buf=""
            small_size=0

            while (( small_size < target )); do
                w=$(( RANDOM % 81 + 20 ))
                f=$(( RANDOM % 10 ))
                if (( f == 0 )); then
                    l="${w}\n"
                else
                    l="${w}.${f}\n"
                fi
                small_buf+="$l"
                small_size=${#small_buf}
                count=$((count + 1))
            done

            # Embed chunk barrier
            small_buf+="\r\n\r\n"
            printf "%s" "$small_buf"
        fi
    done

    # Flush any remainder (must be <= MAX_CHUNK)
    if (( buf_size > 0 )); then
        flush_buffer
    fi

    printf "Sent %d readings in %d seconds\n" "$count" "$DURATION" >&2

) | nc "$HOST" "$PORT"
