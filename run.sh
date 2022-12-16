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
    "day15a" | "day15b") 
       cargo run <input.txt
       ;;
    # Default solutions -- Haskell
    *) stack run <input.txt
       ;;
esac
