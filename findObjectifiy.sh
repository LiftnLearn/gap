#!/bin/bash

GAP="/Users/Eggi/Development/gap/bin/gap.sh"
GAP_DIR="/Users/Eggi/Development/gap"

JSON="/Users/Eggi/Development/gap/json_output"
DEST="/Users/Eggi/Development/gap/objectifies.txt"

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
