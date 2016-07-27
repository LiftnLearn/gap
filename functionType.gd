LoadPackage("MitM", false);

#determineFunctionType
#returns most general answer that will always hold, as otherwise you could just run it with it?

compileJSON := 
function()
    local dest, file, string, f, gapPath, record, func;

    gapPath := GAPInfo.RootPaths[2];

    for f in DirectoryContents(Concatenation(gapPath, "/lib")) do
       #ignore hidden files
       if(f[1] = '.' or f = "DELETED" or f = "hpc") then
           continue;
       fi;

       dest := Concatenation(gapPath, "json_output/", f, ".json");

       #make sure file exists and is empty
       PrintTo(dest, "");

       func := ReadAsFunction(Concatenation(gapPath, "lib/", f));
       JSON_CompileFunc(dest, func, "");
    od;
end;
