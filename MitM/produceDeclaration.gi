Read("MitM/produceDeclaration.gd");

printListWithSeparator :=
function(list, separator)
    local i, str;

    str := "";

    i := 1;
    while (i <= Length(list)) do
        if(i > 1) then str := Concatenation(str, separator); fi;
        str := Concatenation(str, list[i]);
        i := i + 1;
    od;

    return str;
end;

#TODO: just use some sort of fold?
unwrapFilters :=
function(list)
    local i, ans, filterWithAnd;

    ans := [];

    i := 1;
    while (i <= Length(list)) do
        filterWithAnd := getFilterList(list[i]);
        Add(ans, printListWithSeparator(filterWithAnd, " and "));
        i := i + 1;
    od;

    return ans;
end;

declareOperationHandler :=
function(obj)
    local node, declaredOperations, declaredOperation, i;

    if(not(IsBound(obj.declaredOperations))) then
        declaredOperations := rec();
    else
        declaredOperations := obj.declaredOperations;
    fi;

    node := obj.node;

    if(isDeclareOperation(node)) then 
        declaredOperation := rec();
        declaredOperation.name := node.args[1];
        declaredOperation.inputFilters := unwrapFilters(node.args[2]);

        declaredOperations.(declaredOperation.name) := declaredOperation; 
    fi;

    return rec(declaredOperations := declaredOperations);
end;

outputGlobalFunctions :=
function(funcName)
    local str;

    #what about filters?
    str := Concatenation("MitM_DeclareGlobalFunction(\"", funcName, "\");\n");
    str := Concatenation(str, "MitM_InstallGlobalFunction(",
        funcName, ", function (arg...) return CallFuncList( ",
        funcName, ", arg); end);\n");

    return str;
end;

outputConstructor :=
function(recName, inputFilters, resultFilters)
    local i, str;

    str := Concatenation("MitM_DeclareConstructor( \"MitM_", recName, "\", ");
    str := Concatenation(str, "[", printListWithSeparator(inputFilters, ", "), "], ");

    str := Concatenation(str, printListWithSeparator(resultFilters, " and "), ");\n");

    return str;
end;

outputInstallMethods :=
function(recName, inputFilters, wrapperName)
    local i, str;

    str := "";

    i := 1;
    while (i <= Length(inputFilters)) do
        #for each of the installed methods output wrapper call
        str := Concatenation(str, wrapperName, "( MitM_", recName, ", [",
                printListWithSeparator(unwrapFilters(inputFilters[i]), ", "),
                "], function (arg...) return CallFuncList( ", recName,
                " , arg); end);\n");
        i := i + 1;
    od;
    str := Concatenation(str, "\n");

    return str;
end;

mergeAndOutputToFile :=
function(objectifys, declaredOperations, outputDest)
    local i, recName;

    for recName in RecNames(objectifys) do
        if (objectifys.(recName).type = "BindGlobal") then
            ;
        elif (objectifys.(recName).type = "InstallGlobalFunction") then
            AppendTo(outputDest, outputGlobalFunctions(recName));
        elif(IsBound(declaredOperations.(recName))) then
            AppendTo(outputDest, outputConstructor(recName,
                declaredOperations.(recName).inputFilters,
                objectifys.(recName).commonFilters));

            AppendTo(outputDest, outputInstallMethods(recName, 
                objectifys.(recName).inputFilters.InstallMethod, "MitM_InstallMethod"));
            AppendTo(outputDest, outputInstallMethods(recName,
                objectifys.(recName).inputFilters.InstallOtherMethod, "MitM_InstallOtherMethod"));
        fi;
    od; 
end;

#TODO: find out what happened to DoubleCoset -> has an InstallOtherMethod call
#TODO: make sure invalid values are handled (i.e. where filter not determined)
declareOperations :=
function(headerRec, implementRec, outputDest)
    local objectifys, declaredOperations, recName, i;

    #initialize output-file
    PrintTo(outputDest, "");
 
    objectifys := processJson(implementRec); #TODO: rename this, these are not actually objectifys anymore

    declaredOperations := traverseJsonRecordDriver(headerRec,
                            declareOperationHandler).declaredOperations;

    for recName in RecNames(objectifys) do
        objectifys.(recName).commonFilters := Intersection(objectifys.(recName).filters);
        if(Length(objectifys.(recName).commonFilters) = 0) then
            objectifys.(recName).commonFilters := [IsObject];
        fi;
    od;

    mergeAndOutputToFile(objectifys, declaredOperations, outputDest);
end;

declareOperationsCaller := 
function(fileDec, fileImpl)
    local stringDec, stringImpl, recordDec, recordImpl, g, r;

    fileDec := IO_File(Concatenation("json_output/", fileDec));#csetgrp.gd.json");
    fileImpl := IO_File(Concatenation("json_output/", fileImpl));#csetgrp.gi.json");
    stringDec := IO_ReadUntilEOF(fileDec);;
    stringImpl := IO_ReadUntilEOF(fileImpl);;
    recordDec := JsonStringToGap(stringDec);;
    recordImpl := JsonStringToGap(stringImpl);;

    declareOperations(recordDec, recordImpl, "test.gd");
end;


#for each InstallMethod write MitM_InstallMethod(same signature...)
createMitM_InstallMethods :=
function()
end;
