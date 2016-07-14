Read("MitM/produceDeclaration.gd");

unwrapFilters :=
function(list)
    local i, j, ans, str, filterWithAND;

    ans := [];

    i := 1;
    while (i <= Length(list)) do
        str := "";
        filterWithAND := getFilterList(list[i]);

        j := 1;
        while( j <= Length(filterWithAND)) do
            if(j > 1) then
                str := Concatenation(str, " and ");
            fi;
            str := Concatenation(str, filterWithAND[j]);
            j := j + 1;
        od;
        Add(ans, str);
        i := i + 1;
    od;

    return ans;
end;

printListOfStrings :=
function(list)
    local i, str;
    i := 1;
    str := "[";

    while(i <= Length(list)) do
        if( i > 1) then str := Concatenation(str, ", "); fi;
        str := Concatenation(str, list[i]);
        i := i + 1;
    od;
    
    str := Concatenation(str, "]");
    return str;
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

    #Print(objectifys, declaredOperations);
    for recName in RecNames(objectifys) do
        objectifys.(recName).commonFilters := Intersection(objectifys.(recName).filters);
    od;

    #merge and output to file 
    for recName in RecNames(objectifys) do
        #TODO: handle other cases
        if(not(IsBound(declaredOperations.(recName))) or not(objectifys.(recName).type = "InstallMethod")) then
            continue;
        fi;

        i := 1;
        AppendTo(outputDest, "MitM_DeclareConstructor( \"MitM_", recName, "\", [");
        while (i <= Length(declaredOperations.(recName).inputFilters)) do
                if(i > 1) then AppendTo(outputDest, ", "); fi;
                AppendTo(outputDest,
                   declaredOperations.(recName).inputFilters[i]);
                i := i + 1;
        od;
        AppendTo(outputDest, "], ");
        
        i := 1;
        while (i <= Length(objectifys.(recName).commonFilters)) do
            if(i > 1) then
                AppendTo(outputDest, " and ");
            fi;
            AppendTo(outputDest, objectifys.(recName).commonFilters[i]);
            i := i + 1;
        od;

        AppendTo(outputDest, ");\n");
        
        i := 1;
        while (i <= Length(objectifys.(recName).inputFilters)) do
            #for each of the installed methods output wrapper call
            AppendTo(outputDest, "MitM_InstallMethod( MitM_", recName, ", ",
                    printListOfStrings(unwrapFilters(objectifys.(recName).inputFilters[i])),
                    ", function (arg...) return CallFuncList( ", recName,
                    " , arg); end);\n");
            i := i + 1;
        od;
        AppendTo(outputDest, "\n");
    od; 
end;

declareOperationsCaller := 
function()
    local fileDec, fileImpl, stringDec, stringImpl, recordDec, recordImpl, g, r;

    fileDec := IO_File("json_output/csetgrp.gd.json");
    fileImpl := IO_File("json_output/csetgrp.gi.json");
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
