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

if [ -d "$1" ]; then
    echo "Error! Directory '$1' already exists! Refusing to recreate."
    exit 1
fi

mkdir -p "$1/app"
cp "template00/app/Main.hs" "$1/app/Main.hs"
cp "template00/.gitignore" "$1/.gitignore"
cp "template00/stack.yaml" "$1/stack.yaml"
cp "template00/stack.yaml.lock" "$1/stack.yaml.lock"
sed "s/template00/$1/g" "template00/CHANGELOG.md" > "$1/CHANGELOG.md"
sed "s/template00/$1/g" "template00/template00.cabal" > "$1/$1.cabal"
