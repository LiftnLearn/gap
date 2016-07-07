#!/bin/bash

#usage: pass the root directory of the gap installation as first argument

if [ -z "$1" ]; then
        echo "usage: ./compileLibToJson.sh <absolute path to GAP installation>"
        exit 1
fi

GAP_DIR="$1"
GAC="`find $GAP_DIR -name \"gac\"`"

DEST="$GAP_DIR/json_output"

for file in $GAP_DIR/lib/*.gi
do 

  filename=${file##*/}

  #create raw json output
  $GAC -o $DEST/$filename.json -C $file

  #prettify
  python -m json.tool $DEST/$filename.json > $DEST/$filename.prettified.json

done
