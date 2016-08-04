Read("functionType.gd");

#keeps track of all called functions to find recursion
calledFunctions := [];

handleExpr :=
function(expr, variableMapping)
    local argTypes, arg;
#    Print(expr, "\n --------------------- \n");
    Print("handling expr\n");

    if(IsList(expr)) then
        #shouldn't I figure out what kind of list this is ?
        return IsList;
    elif(IsInt(expr)) then
        return IsInt;
    elif(IsBool(expr) or (IsBound(expr.type) and expr.type = "BoolExpr")) then
        return IsBool;
    elif(IsBound(expr.type) and expr.type = "functionCall" and
    IsBound(expr.name) and expr.name.subtype = "RefGVar") then
        if(expr.name.identifier = "Objectify" or
        IsBoundGlobal(Concatenation("MitM_", expr.name.identifier))) then
            #TODO: function containing objectify -> get output filter
            #ideally from some lookup
            LackOfData("Call containing Objectify");
            return IsObject;
        elif(KnowsDictionary(RETURN_TYPE_DICT, expr.name.identifier)) then
            return LookupDictionary(RETURN_TYPE_DICT, expr.name.identifier);
        elif(IsKernelFunction(ValueGlobal(expr.name.identifier))) then
            LackOfData("KernelFunction");
            return IsObject;
        else
            #analyze return type of called function
            argTypes := [];

            for arg in expr.args do
                Append(argTypes, [handleExpr(arg, variableMapping)]);
            od;
            
            return determineMethodOutputType(expr.name.identifier, argTypes, false);
        fi;
    elif(IsBound(expr.type) and expr.type = "functionCall") then 
        LackOfData("Local function called");
        return IsObject;
    elif(IsBound(expr.type) and expr.type = "variable") then
        #simple variable lookup 
        if(expr.subtype = "LVar") then 
            return variableMapping.(expr.identifier);
        elif(expr.subtype = "GVar") then
            #Print(expr.name);
            return findBasicFilters(NamesFilter(TRUES_FLAGS(TypeObj(expr.identifier)![2])));
        else
            #TODO
#            Print("Unknown variable lookup: ", expr, "\n");
            LackOfData("Unknown variable lookup");
            return IsObject;
        fi;
    elif(IsBound(expr.type) and expr.type = "listAccess") then
        LackOfData("listAccess");
        return IsObject;
    else
#        Print("Unknown expression: ", expr, "\n");
        LackOfData("Unknown expression");
        return IsObject;
    fi;
end;

handleStat :=
function(stat, variableMapping)
    local s, recNames, recName, variableMappingForIf, variableMappingForElse;
#    Print(stat, "\n --------------------- \n");
    Print("handling stat\n");

    #TODO: make this look nice, get rid of repeated IsBounds

    if(IsList(stat.stat)) then #sequence of statements
        for s in stat.stat do
            handleStat(s, variableMapping);
        od;
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall" 
    and IsBound(stat.stat.name) and IsBound(stat.stat.name.identifier) and IsBoundGlobal(
    Concatenation("MitM_", stat.stat.name.identifier))) then
         #this would actually be a procedure call, but shouldn't matter
         ;#TODO: function containing objectify -> get output filter
    elif(IsBound(stat.stat.type) and stat.stat.type = "assign") then
        if(stat.stat.subtype = "LVar") then
            variableMapping.(stat.stat.left) :=
              handleExpr(stat.stat.right, variableMapping);
        fi;
    elif(IsBound(stat.stat.type) and stat.stat.type = "functionCall") then
        #only of relevance if function has side effects
        #TODO: possibly infer that the left type in "Add" is always a list,
        #and for Append the left and right ones are lists
        return;
    elif(IsBound(stat.stat.type) and stat.stat.type = "Info") then
        return;
    elif(IsBound(stat.stat.type) and stat.stat.type = "return") then
        if(stat.stat.void) then
            Append(variableMapping.returns, [[]]);
        else
            Append(variableMapping.returns,
                [handleExpr(stat.stat.("return"), variableMapping)]);
        fi;
    elif(IsBound(stat.stat.type) and (stat.stat.type in ["for", "while"])) then
        #TODO: possibly consider whats actually in the list
        if(stat.stat.type = "for") then 
            LackOfData("Type of iterator variable");
            variableMapping.(stat.stat.var):=IsObject;
        fi; 

        for s in stat.stat.("do") do
            handleStat(s, variableMapping);
        od;
    elif(IsBound(stat.stat.type) and stat.stat.type = "if") then
        #all variable assignments in here must be aggregated and then be
        #checked for matching types
        #do a structural copy
        
        variableMappingForIf := StructuralCopy(variableMapping);
        variableMappingForIf.returns := [];
        variableMappingForElse := StructuralCopy(variableMappingForIf);

        handleStat(stat.stat.("then"), variableMappingForIf);
        if(IsBound(stat.stat.("else"))) then
            handleStat(stat.stat.("else"), variableMappingForElse);
        fi;

        recNames := ShallowCopy(RecNames(variableMappingForIf));
        Append(recNames, RecNames(variableMappingForElse));

        recNames := DuplicateFreeList(recNames);    
        #TODO: can this be simplified
        for recName in recNames do
            if(recName = "returns") then
                Append(variableMapping.returns, variableMappingForIf.returns);
                Append(variableMapping.returns, variableMappingForElse.returns);
            elif(not(IsBound(variableMappingForIf.(recName)))) then
                variableMapping.(recName) := variableMappingForElse.(recName);
            elif(not(IsBound(variableMappingForElse.(recName)))) then
                variableMapping.(recName) := variableMappingForIf.(recName);
            else
                variableMapping.(recName) :=
                    findBasicFilters([variableMappingForIf.(recName),
                    variableMappingForElse.(recName)]);
            fi;
        od;
    else #cases that are not taken care of
        #Print("Unknown type: ", stat, "\n");
        LackOfData("Unknown statement type");
    fi;
end;

#for every assignment add value to set
#go over each line of the compiled function,
#for every operation call recursively apply this
determineMethodOutputType :=
function(funcName, filters, code)
    local func, tempFileName, funcRecord, variableMapping, stat, i, temp;
    Print("determining output type: ", funcName, " ", filters, " ", code, " ", IsKernelFunction(code), "\n");
    func := false;
 
    if(code <> false and IsKernelFunction(code)) then
        return IsObject;
    fi;
   

    if(funcName in calledFunctions) then
        #Error("Recursion detected");
        LackOfData("Recursion");
        return IsObject;
    else
#        Print(funcName, " ", calledFunctions, "\n");
        Add(calledFunctions, funcName);
    fi;
    
    if(code = false and IsOperation(ValueGlobal(funcName))) then
        func := ApplicableMethodTypes(ValueGlobal(funcName), filters);
    elif(code = false) then
        func := ValueGlobal(funcName);
    else
        func := code;
    fi;


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

    Remove(calledFunctions);
   
    if(Length(variableMapping.returns) > 0) then
        #Print(variableMapping.returns, "\n");
        variableMapping.returns := findBasicFilters(variableMapping.returns);        
    fi;

    #empty list ([]) means no return type detected
    #possible feature: anything wrapped in a list means it's a list of that type
    #   i.e. [IsInt] for a list of integers
    return variableMapping.returns;
end;

#Print(determineMethodOutputType("IS_PGROUP_FROM_SIZE", [IsObject]));
#Print(determineMethodOutputType("IsPGroup", [IsGroup and IsNilpotentGroup], false));
#Print(determineMethodOutputType(GrowthFunctionOfGroup, [IsGroup and HasGeneratorsOfGroup,IsPosInt]));
