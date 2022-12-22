#! /bin/sh
set -e

if [ "$#" -ne 1 ]; then
    echo "Parameter expected: <day>"
    exit 1
fi


if [ "$1" == "--help" ]; then
    echo "Usage: $0 <dayxx>"
    exit 1
fi

if ! [ -d "$1" ]; then
    echo "Not a directory: $1"
    exit 1
fi

cd "$1"

case "$1" in
    # Rust solutions
    "day15a" | "day15b" | \
    "day16a" | "day16b" | \
    "day17a" | "day17b" | \
    "day18a" | "day18b" | \
    "day19a" | "day19b" | \
    "day20a" | "day20b" | \
    "day21a" | "day21b" | \
    "day22a" | "day22b" | \
    "day23a" | "day23b" | \
    "day24a" | "day24b" | \
    "day25a" | "day25b" ) 
       RUST_BACKTRACE=1 cargo run <input.txt
       ;;
    # Default solutions -- Haskell
    *) stack run <input.txt
       ;;
esac
