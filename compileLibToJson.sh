#!/bin/bash

GAC="/Users/Eggi/Development/gap/bin/x86_64-apple-darwin14.5.0-gcc-default64/gac"
GAP_DIR="/Users/Eggi/Development/gap"

DEST="/Users/Eggi/Development/gap/json_output"

for file in $GAP_DIR/lib/*.gi
do 

  filename=${file##*/}

  #create raw json output
  $GAC -o $DEST/$filename.json -C $file

  #prettify
  python -m json.tool $DEST/$filename.json > $DEST/$filename.prettified.json

done