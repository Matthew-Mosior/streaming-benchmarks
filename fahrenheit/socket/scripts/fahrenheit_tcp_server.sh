#!/bin/bash

PORT=12345
DURATION=20

MAX_CHUNK=$((0xfff))        # 4095 bytes
SMALL_CHUNK_MAX=$((0xff))   # 255 bytes

echo "Starting TCP server on port $PORT..."

# Function to flush buffer to socket
flush_buffer() {
    printf "%s" "$buffer"
    buffer=""
    buf_size=0
}

# This block generates temperatures and pipes directly into nc
{
    start_time=$(date +%s)
    end_time=$((start_time + DURATION))
    count=0
    buffer=""
    buf_size=0

    while :; do
        now=$(date +%s)
        (( now >= end_time )) && break

        # Generate Fahrenheit temperature
        whole=$(( RANDOM % 81 + 20 ))
        fraction=$(( RANDOM % 10 ))

        if (( fraction == 0 )); then
            line="${whole}\n"
        else
            line="${whole}.${fraction}\n"
        fi

        line_len=${#line}

        # Flush if adding line exceeds MAX_CHUNK
        if (( buf_size + line_len > MAX_CHUNK )); then
            flush_buffer
        fi

        buffer+="$line"
        buf_size=$((buf_size + line_len))
        count=$((count + 1))

        # Random small chunk with embedded delimiter
        if (( RANDOM % 50 == 0 )); then
            # Flush current buffer first
            if (( buf_size > 0 )); then
                flush_buffer
            fi

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

    # Flush any remaining data
    if (( buf_size > 0 )); then
        flush_buffer
    fi

    printf "Sent %d readings in %d seconds\n" "$count" "$DURATION" >&2

} | nc -l "$PORT" -q 0   # -q 0 ensures nc exits immediately after EOF
