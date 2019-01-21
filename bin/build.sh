#!/bin/sh

set -e

lineseparator="================================="

echo "$lineseparator"
echo "Prepare build target"
echo "$lineseparator"
rm -rf ./build

mkdir -p ./build

uglify=./node_modules/.bin/uglifyjs
js="./build/app.js"
min="./build/app.min.js"

echo "Compile elm"
elm make --optimize --output=$js $@

$uglify $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | $uglify --mangle --output=$min

echo "$lineseparator"
echo "Compiled size:    $(cat $js | wc -c) bytes  ($js)"
echo "Minified size:    $(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size:     $(cat $min | gzip -c | wc -c) bytes"
echo "$lineseparator"
