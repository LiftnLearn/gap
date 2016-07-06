LoadPackage("json");
file := IO_File("test.json", "r");;
stream := IO_ReadUntilEOF(file);;
record := JsonStringToGap(stream);;

Read("findObjectify.g");
processJSON(record);