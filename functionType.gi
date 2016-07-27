Read("functionType.gd");

handleStat :=
function(stat, mapping)
    local type;
    type := [IsObject];

    if(IsBound(stat.stat.type) and stat.stat.type = "functionCall" and IsBound(stat.stat.name)
        and IsBoundGlobal(Concatenation("MitM_", stat.stat.name.identifier))) then
         ;#TODO: get filter
    elif(IsBound(stat.stat.type) and stat.stat.type = "assign") then
        mapping.(stat.stat.left) := handleStat(stat.stat.right, mapping);
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall") then
        ;
    elif(IsBound(stat.stat.type) = "return") then
        if(stat.stat.void) then type := [];
        else type := handleStat(stat.stat.("return"));
        fi;
    else #cases that are not taken care of
        Print(stat);
    fi;

    return type;    
end;

#print value of 'func' to File
#apply JSON compiler to file 
#for every assignment add value to set
#go over each line of the compiled function, for every operation call recursively apply this

determineMethodOutputType :=
function(operationName, filters)
    local func, tempFileName, funcRecord, mapping, stat, i;
    #determine if function is bound
    func := ApplicableMethodTypes(operationName, filters);

    tempFileName := "determineMethodOutputHelper.temp";
    JSON_CompileFunc(tempFileName, func, "");
    
    funcRecord := JsonStringToGap(IO_ReadUntilEOF(IO_File(tempFileName)));

    #add all parameters with their types to the mapping
    mapping := rec();

    i := 1;
    while i <= Length(funcRecord.param) do
        mapping.(funcRecord.param[i]) := filters[i];
        i := i + 1;
    od;

    for stat in funcRecord.body.stat do
        if(not (IsBound(stat.stat.type) and stat.stat.type = "return")) then
            handleStat(stat, mapping);
        else
            #because this is a top-level return (can't be under some condition!)
            #TODO: handle conditional returns
            return handleStat(stat, mapping);
        fi;
    od;
   
    #I don't think this should ever happen ?
    return []; #no return type, has only side effects
end;

#Print(determineMethodOutputType(PrintObj, [IsAdditiveMagma]));
