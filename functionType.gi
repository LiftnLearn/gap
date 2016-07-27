Read("functionType.gd");

handleExpr :=
function(expr, variableMapping)
    Print(expr, "\n --------------------- \n");
    if(IsBound(expr.type)) then
        return [expr.type];
    else
        return [IsObject];
    fi;
end;

handleStat :=
function(stat, variableMapping)
    local type;
    type := [IsObject];

    Print(stat, "\n --------------------- \n");

    if(IsBound(stat.stat.type) and stat.stat.type = "functionCall"
      and IsBound(stat.stat.name) and IsBoundGlobal(
      Concatenation("MitM_", stat.stat.name.identifier))) then
         ;#TODO: function containing objectify -> get output filter
    elif(IsBound(stat.stat.type) and stat.stat.type = "assign") then
        variableMapping.(stat.stat.left) :=
          handleExpr(stat.stat.right, variableMapping);
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall") then
        ;
    elif(IsBound(stat.stat.type) and stat.stat.type = "return") then
        if(stat.stat.void) then type := [];
        else type := handleExpr(stat.stat.("return"), variableMapping);
        fi;
    else #cases that are not taken care of
        Print("unknown type");
        #Print(stat);
    fi;

    return type;    
end;

#for every assignment add value to set
#go over each line of the compiled function,
#for every operation call recursively apply this
determineMethodOutputType :=
function(operationName, filters)
    local func, tempFileName, funcRecord, variableMapping, stat, i;
    
    #determine if function is bound
    func := ApplicableMethodTypes(operationName, filters);

    tempFileName := "determineMethodOutputHelper.temp";
    JSON_CompileFunc(tempFileName, func, "");
    
    funcRecord := JsonStringToGap(IO_ReadUntilEOF(IO_File(tempFileName)));

    #add all parameters with their types to the variableMapping
    variableMapping := rec();

    i := 1;
    while i <= Length(funcRecord.param) do
        variableMapping.(funcRecord.param[i]) := filters[i];
        i := i + 1;
    od;

    for stat in funcRecord.body.stat do
        if(not (IsBound(stat.stat.type) and stat.stat.type = "return")) then
            handleStat(stat, variableMapping);
        else
            #because this is a top-level return (can't be under some condition!)
            #TODO: handle conditional returns
            return handleStat(stat, variableMapping);
        fi;
    od;
   
    #I don't think this should ever happen ?
    return []; #no return type, has only side effects
end;

Print(determineMethodOutputType(InvariantBilinearForm,[IsMatrixGroup and HasInvariantQuadraticForm]));
