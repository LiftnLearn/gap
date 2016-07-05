LoadPackage("json");
file := IO_File("../gap/test.json", "r");;
stream := IO_ReadUntilEOF(file);;
rec := JsonStringToGap(stream);;