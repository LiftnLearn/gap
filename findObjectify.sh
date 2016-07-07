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

echo "" >  $DEST

for file in $JSON/*.json
do 

  filename=${file##*/}
  echo "$filename \n" >> $DEST

  #create raw json output
  echo "LoadPackage(\"json\");
        file := IO_File(\"$file\", \"r\");;
        string := IO_ReadUntilEOF(file);;
        record := JsonStringToGap(string);;
        
        Read(\"$GAP_DIR/findObjectify.g\");
        processJSON(record, \"$DEST\"); quit;" | $GAP

done
