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

getFilterList :=
function(filter)
    return TRUES_FLAGS(WITH_IMPS_FLAGS(FLAGS_FILTER(filter)));
end;

#convenience function that takes a list of filters and returns the minimum set
#of filters for a given set of filterIDs
findBasicFilters :=
function(filters)
    local el, i, filter, filterIDs, uniqueFilterIDs, resultFilterIDList,
        ans, newFilter, filterID;

    filterIDs := [];
    #find implied filters, take intersection, build up to biggest common filter (and combine with and)
    for filter in filters do
        Add(filterIDs, getFilterList(filter));
    od;

    uniqueFilterIDs := Intersection(filterIDs);

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

#test case: IsCollection and IsGroup -> both fulfill IsCollection

determineMethodOutputType := function() ; end;
