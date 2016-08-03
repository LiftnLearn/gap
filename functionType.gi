Read("functionType.gd");

handleExpr :=
function(expr, variableMapping)
    local argTypes, arg;
#    Print(expr, "\n --------------------- \n");

    if(IsList(expr)) then
        #shouldn't I figure out what kind of list this is ?
        return IsList;
    elif(IsBool(expr) or (IsBound(expr.type) and expr.type = "BoolExpr")) then
        return IsBool;
    elif(IsBound(expr.type) and expr.type = "functionCall" and
    IsBound(expr.name)) then
        if(IsBoundGlobal(Concatenation("MitM_", expr.name.identifier))) then
            #TODO: function containing objectify -> get output filter
            #ideally from some lookup
            return IsObject;
        elif(KnowsDictionary(RETURN_TYPE_DICT, expr.name.identifier)) then
            return LookupDictionary(RETURN_TYPE_DICT, expr.name.identifier);
        elif(IsKernelFunction(ValueGlobal(expr.name.identifier))) then
            return IsObject; #maybe hardcode some classic ones, i.e. length?
        else
            #analyze return type of called function
            argTypes := [];

            for arg in expr.args do
                Append(argTypes, [handleExpr(arg, variableMapping)]);
            od;
            
            return determineMethodOutputType(expr.name.identifier, argTypes);
        fi;
    elif(IsBound(expr.type) and expr.type = "variable") then
        #simple variable lookup 
        if(expr.subtype = "LVar" or expr.subtype = "RefLVar") then 
            return variableMapping.(expr.identifier);
        elif(expr.subtype = "GVar" or expr.subtype = "RefGVar") then
            #TODO: this gives redundant filters, is there some way to give only
            #basic ones? -> unwrap from back until whole list found
            return NamesFilter(TRUES_FLAGS(TypeObj(expr.name.identifier)![2]));
        else
            #TODO
            Print("unknown variable lookup\n");
            return IsObject;
        fi;
    else
        return IsObject;
    fi;
end;

handleStat :=
function(stat, variableMapping)
    local s, recNames, recName, variableMappingForIf, variableMappingForElse;
#    Print(stat, "\n --------------------- \n");

    if(IsList(stat.stat)) then #sequence of statements
        for s in stat.stat do
            handleStat(s, variableMapping);
        od;
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall" 
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
            Append(variableMapping.returns, [[]]);
        else
            if(not(IsMutable(variableMapping.returns))) then Error("List not mutable"); fi;
            Append(variableMapping.returns,
                [handleExpr(stat.stat.("return"), variableMapping)]);
        fi;
    elif(IsBound(stat.stat.type) and stat.stat.type = "for") then
        #TODO: possibly consider whats actually in the list
        variableMapping.(stat.stat.var) := IsObject;        
        for s in stat.stat.("do") do
            handleStat(s, variableMapping);
        od;
    elif(IsBound(stat.stat.type) and stat.stat.type = "if") then
        #all variable assignments in here must be aggregated and then be
        #checked for matching types
        #do a structural copy
        
        variableMappingForIf := StructuralCopy(variableMapping);
        variableMappingForElse := StructuralCopy(variableMapping);

        handleStat(stat.stat.("then"), variableMappingForIf);
        if(IsBound(stat.stat.("else"))) then
            handleStat(stat.stat.("else"), variableMappingForElse);
        fi;

        recNames := ShallowCopy(RecNames(variableMappingForIf));
        Append(recNames, RecNames(variableMappingForElse));

        recNames := DuplicateFreeList(recNames);    
        for recName in recNames do
            if(recName = "returns") then
                continue;
            elif(not(IsBound(variableMappingForIf.(recName)))) then
                variableMapping.(recName) := variableMappingForElse.(recName);
            elif(not(IsBound(variableMappingForElse.(recName)))) then
                variableMapping.(recName) := variableMappingForIf.(recName);
            else
                if(variableMappingForIf.(recName) =
                variableMappingForElse.(recName)) then
                    variableMapping.(recName) := variableMappingForIf.(recName);
                else
                    variableMapping.(recName) := IsObject;
                fi;
            fi;
        od;
    else #cases that are not taken care of
        Print("unknown type \n");
        Print(stat);
    fi;
end;

#for every assignment add value to set
#go over each line of the compiled function,
#for every operation call recursively apply this
determineMethodOutputType :=
function(func, filters)
    local tempFileName, funcRecord, variableMapping, stat, i, temp;
    
    #determine if function is bound
    #func := ApplicableMethodTypes(operationName, filters);

#    Print("determineMethodOutputType", func, "\n", filters, "\n");

    tempFileName := "determineMethodOutputHelper.temp";
    JSON_CompileFunc(tempFileName, func, "");
    
    funcRecord := JsonStringToGap(IO_ReadUntilEOF(IO_File(tempFileName)));

    #add all parameters with their types to the variableMapping
    #returns is a list of all possible return types
    variableMapping := rec( returns := [] );

    i := 1;
    while i <= Length(funcRecord.param) do
        variableMapping.(funcRecord.param[i]) := filters[i];
        i := i + 1;
    od;

    handleStat(funcRecord.body, variableMapping);
   
    #TODO: ignore cases in which fail is returned
    #TODO: possibly return intersection of most basic common filter
    if(Length(variableMapping.returns) > 0) then
        temp := variableMapping.returns[1];
        for i in variableMapping.returns do
            if(not(temp = i)) then
                variableMapping.returns := IsObject;
                break;
            fi;
        od;
        variableMapping.returns := temp;
    fi;

    #empty list ([]) means no return type detected
    #possibly : anything wrapped in a list means it's a list of that type
    #   i.e. [IsInt] for a list of integers
    return variableMapping.returns;
end;

Print(determineMethodOutputType(IS_PGROUP_FOR_NILPOTENT, [IsObject]));

#Print(determineMethodOutputType(ApplicableMethodTypes(InvariantBilinearForm,[IsMatrixGroup and HasInvariantQuadraticForm]), [IsMatrixGroup and HasInvariantQuadraticForm]));
