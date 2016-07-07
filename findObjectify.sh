#!/bin/bash

#usage: pass the root directory of the gap installation as first argument

if [ -z "$1" ]; then
        echo "usage: ./findObjectify.sh <absolute path to GAP installation>"
        exit 1
fi

GAP_DIR=$1
GAP="$GAP_DIR/bin/gap.sh"

JSON="$GAP_DIR/json_output"
DEST="$GAP_DIR/objectifies.txt"

for file in $JSON/*.json
do 

  filename=${file##*/}

  echo "" >  $DEST

  #create raw json output
  echo "LoadPackage(\"json\");
        file := IO_File(\"$file\", \"r\");;
        stream := IO_ReadUntilEOF(file);;
        record := JsonStringToGap(stream);;
        
        Read(\"findObjectify.g\");
        processJSON(record, \"$DEST\"); quit;" | $GAP

done
