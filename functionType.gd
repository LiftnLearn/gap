LoadPackage("MitM", false);

#determineFunctionType
#returns most general answer that will always hold, as otherwise you
#could just run it with it?

determineMethodOutputType := function() ; end;

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

getFilterList :=
function(filter)
    return TRUES_FLAGS(WITH_IMPS_FLAGS(FLAGS_FILTER(filter)));
end;

#convenience function that takes a list of filters and returns the minimum set
#of filters for a given set of filterIDs
#test case: IsCollection and IsGroup -> both fulfill IsCollection
#bug: this assumes that filters only are created of lower ranked filters,
#this seems however not to be always the case (i.e. filter 15, IsList, implies
#filter 56, IsListOrCollection
findBasicFilters :=
function(filters)
    local el, i, filter, filterIDs, uniqueFilterIDs, resultFilterIDList,
        ans, newFilter, filterID;

    filterIDs := [];
    #find implied filters, take intersection, build up to biggest common filter (and combine with and)
    for filter in filters do
        if(filter = []) then
            continue; #function might or might not return, we speculate
        elif(not(filter = fail)) then
            Add(filterIDs, getFilterList(filter));
        fi;
    od;

    #apply patch that fixes Intersection on empty list
    if(filterIDs <> []) then
        uniqueFilterIDs := Intersection(filterIDs);
    else
        uniqueFilterIDs := [];
    fi;

    resultFilterIDList := [];
    while (Length(uniqueFilterIDs) >= 1) do
        filterID := uniqueFilterIDs[Length(uniqueFilterIDs)];
        newFilter := false;
        for el in getFilterList(FILTERS[filterID]) do
            if(el in uniqueFilterIDs) then
                Remove(uniqueFilterIDs, Position(uniqueFilterIDs, el));
                newFilter := true;
            fi;
        od;

        if(newFilter) then
            Add(resultFilterIDList, filterID);
        fi;
    od;

    #combine all filters with AND
    ans := true;
    for i in resultFilterIDList do
        if(IsBool(ans)) then ans := FILTERS[i];
        else ans := ans and FILTERS[i];
        fi;
    od;

    if(ans = true) then ans := IsObject; fi;

    return ans;
end;

#hardcode return types of certain functions
if(not(IsBound(RETURN_TYPE_DICT))) then
    BIND_GLOBAL("RETURN_TYPE_DICT", NewDictionary("some string", true));

    AddDictionary(RETURN_TYPE_DICT, "Length", IsInt);
    AddDictionary(RETURN_TYPE_DICT, "Size", IsInt);
fi;

statisticsFile := "statistics.txt";

#hints at possible areas of improvement in analyzer
problemList := [];
allProblemList := [];
allProblemCountList := [];

LackOfData :=
function(msg)
    local pos;

    Add(problemList, msg);

    pos := Position(allProblemList, msg);
    
    if(pos = fail) then
        Add(allProblemList, msg);
        Add(allProblemCountList, 0);
    else
        allProblemCountList[pos] := allProblemCountList[pos] + 1;
    fi;
end;

#bug: seems to crash if it is called more than once per session
runOverAllOperations :=
function()
    local i, j, a, l, operation, methods, mpos, mres, filter, isObjectCount, overallCount;

    isObjectCount := 0;
    overallCount := 0;

    allProblemList := [];
    allProblemCountList := [];

    #clear file
    PrintTo(statisticsFile, "");

    for i in [1..Length(OPERATIONS)/2] do
        operation := rec();
        operation.name := NameFunction(OPERATIONS[2*i - 1]);

        for a in [1..6] do
            methods := METHODS_OPERATION(OPERATIONS[2*i - 1], a);
            
            for j in [1..Int(Length(methods)/(a+4))] do
                mpos := (j-1) * (a + 4) + 1;
                mres := rec( filters := List([1..a],
                                argnum ->
                                        List(TRUES_FLAGS(methods[mpos + argnum]),
                                                         x-> FILTERS[x])
                                        ),
                            rank := methods[mpos + a + 2],
                            comment := methods[mpos + a + 3]
                        );

                #hints at possible areas of improvement in analyzer
                problemList := [];

                filter := determineMethodOutputType(operation.name,
                    List(mres.filters, l -> findBasicFilters(l)),
                    methods[mpos+a+1]);

                overallCount := overallCount + 1;

                if(filter = IsObject) then
                    AppendTo(statisticsFile, operation.name, ": ", String(problemList), "\n");
                    isObjectCount := isObjectCount + 1;
                fi;
            od;
        od;
    od;

    AppendTo(statisticsFile, "---------------------\n",
                            "Summary of statistics:\n",
                            "Overall functions analyzed: ", overallCount, "\n",
                            "Found IsObject as result: ", isObjectCount, "\n",
                            "\nReasons for IsObject occurences:\n",
                            "(There might be multiple reasons per function)\n");

    for i in [1..Length(allProblemList)] do
        AppendTo(statisticsFile, allProblemList[i], ": ", allProblemCountList[i], "\n");
    od;
end;

