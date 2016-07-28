Read("functionType.gd");

handleExpr :=
function(expr, variableMapping)
    local argTypes, arg;
    Print(expr, "\n --------------------- \n");

    if(IsList(expr)) then
        return [IsList];
    elif(IsBool(expr) or (IsBound(expr.type) and expr.type = "BoolExpr")) then
        return [IsBool];
    elif(IsBound(expr.type) and expr.type = "functionCall" 
    and IsBound(expr.name)) then
        if(IsBoundGlobal(Concatenation("MitM_", expr.name.identifier))) then
            #function containing objectify -> get output filter
        else
            #analyze return type of called function
            argTypes := [];

            for arg in expr.args do
                argTypes.add(handleExpr(arg));
            od;
            
            return determineMethodOutputType(expr.name, argTypes);
        fi;
    elif(IsBound(expr.type) and expr.type = "variable") then
        #simple variable lookup 
        if(expr.subtype = "LVar") then 
            return variableMapping.(expr.identifier);
        elif(expr.subtype = "GVar") then
            #TODO: this gives redundant filters, is there some way to give only
            #basic ones? -> unwrap from back until whole list found
            return NamesFilter(TRUES_FLAGS(TypeObj(expr.name.identifier)![2]));
        fi;
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
         #this would actually be a procedure call, but shouldn't matter
         ;#TODO: function containing objectify -> get output filter
    elif(IsBound(stat.stat.type) and stat.stat.type = "assign") then
        if(stat.stat.subtype = "LVar") then
            variableMapping.(stat.stat.left) :=
              handleExpr(stat.stat.right, variableMapping);
        fi;
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall") then
        ;
    elif(IsBound(stat.stat.type) and stat.stat.type = "return") then
        if(stat.stat.void) then
            type := [];
        else
            type := handleExpr(stat.stat.("return"), variableMapping);
        fi;
    else #cases that are not taken care of
        Print("unknown type \n");
        #Print(stat);
    fi;

    return type;    
end;

#for every assignment add value to set
#go over each line of the compiled function,
#for every operation call recursively apply this
determineMethodOutputType :=
function(func, filters)
    local tempFileName, funcRecord, variableMapping, stat, i;
    
    #determine if function is bound
    #func := ApplicableMethodTypes(operationName, filters);

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
            #for conditional returns: find all return statements in the function,
            #put them in a list, find common type
            return handleStat(stat, variableMapping);
        fi;
    od;
   
    #I don't think this should ever happen ?
    return []; #no return type, has only side effects
end;

Print(determineMethodOutputType(IS_PGROUP_FOR_NILPOTENT, [IsObject]));

#Print(determineMethodOutputType(ApplicableMethodTypes(InvariantBilinearForm,[IsMatrixGroup and HasInvariantQuadraticForm]), [IsMatrixGroup and HasInvariantQuadraticForm]));
