Read("findObjectify.gd");

flattenToStrings :=
function(root, flattenList)
    local i;

    if(IsString(root)) then
        Add(flattenList, root);
    elif(not IsString(root) and IsList(root)) then
        for i in root do
            flattenToStrings(i, flattenList);
        od;
    elif(IsRecord(root)) then
        for i in RecNames(root) do
            flattenToStrings(root.(i), flattenList);
        od;
    fi;
end;


getFilterList :=
function(root)
    local filterList, flattenedList, i; 
    
    #flatten the tree
    flattenedList := [];
    flattenToStrings(root, flattenedList);

    #check for each string if it is a filter
    filterList := [];
    for i in flattenedList do
        if(IsBoundGlobal(i) and (IsObject = ValueGlobal(i) or IsFilter(ValueGlobal(i)))) then
            Add(filterList, i);
        fi;
    od;

    return filterList;
end;


flattenToRecords :=
function(root, recordList)
    local i;

    if(IsRecord(root)) then
        Add(recordList, root);

        for i in RecNames(root) do
            flattenToRecords(root.(i), recordList);
        od;
    elif(IsList(root)) then
        for i in root do
            flattenToRecords(i, recordList);
        od;
    fi;
end;

findLocalAssignments :=
function(surroundingFunction, identifier)
    local assignments, records, r;

    records := [];
    flattenToRecords(surroundingFunction, records);

    assignments := []; 
    for r in records do
        if(IsBound(r.type) and r.type = "assign"
          and IsBound(r.left) and r.left = identifier) then
            Add(assignments, r);
        fi;
    od;

    return assignments;
end;


handleLocalVariable :=
function(surroundingFunction, type)
    local assignments, i, listOfAssignmentFilters, filters;

    #now find all assignments to this
    # variable within the surrounding function
    #use flatten for this / or some other generic lib-function
    #   find all assignments
    assignments := findLocalAssignments(surroundingFunction, type.identifier);

    #[list of lists]: each sublist contains filters in a given assignment -> find biggest common subset
    #TODO: find better name
    listOfAssignmentFilters := [];
    for i in assignments do
        Add(listOfAssignmentFilters, getFilterList(i));
    od;

    filters := [];

    #find common subset
    if(Length(listOfAssignmentFilters) > 0) then
        filters := Intersection(listOfAssignmentFilters);
    fi;

    return filters;
end;

handleGlobalVariable :=
function(identifier)
    local filterIDs, filters, i;

    filters := [];

    if(IsBoundGlobal(identifier)
      and IsType(ValueGlobal(identifier))) then
        #return all filters of this type

        filterIDs := TRUES_FLAGS(ValueGlobal(identifier)![2]);
        for i in filterIDs do
            Add(filters, NameFunction(FILTERS[i]));
        od;
    fi;

    return filters;
end;

#TODO: put handling of Objectify's in separate function
#TODO: write header that describes how all these functions are related
#known bugs: 
analyzeRecordForObjectifies :=
function(obj)
    local args, assignments, type, i, filterSubtree, statNode,
      listOfAssignmentFilters, filters, objectifyFound, constructorName,
      node, line, surroundingFunction, surroundingFunctionLine,
      objectifyFunctionLine, objectifys;

    node := obj.node;

    if(not(IsBound(obj.surroundingFunction))) then
        surroundingFunction := false; #is there some null equivalent in GAP?
        surroundingFunctionLine := 1;
        objectifyFunctionLine := 1;
        objectifys := rec();
        line := 1;
    else
        surroundingFunction := obj.surroundingFunction;
        surroundingFunctionLine := obj.surroundingFunctionLine;
        objectifyFunctionLine := obj.objectifyFunctionLine;
        objectifys := obj.objectifys;
        line := obj.line;
    fi;

    filters := [];
    objectifyFound := false;

    if(IsBound(node.type) and node.type = "debugInfo") then
        line := node.line;
        statNode := node.stat;

        #cache a surrounding function if one is found
        if (isSurroundingFunction(statNode)) then
            surroundingFunction := statNode; 
            surroundingFunctionLine := node.line;
        fi;

    #Objectify(..., ...) being found
    #TODO: take care of BindGlobal
    elif(isObjectify(node) and not(surroundingFunction.name.identifier = "BindGlobal")) then
        objectifyFound := true;
        constructorName := surroundingFunction.args[1].identifier;

        if(not(IsBound(objectifys.(constructorName)))) then
            objectifys.(constructorName) := rec();
            objectifys.(constructorName).multipleObjectifiesFlag := false;
            objectifys.(constructorName).inputFilters := [];
        fi;

        
        #first Objectify call within surrounding function
        if(not(objectifyFunctionLine = surroundingFunctionLine)) then
            objectifyFunctionLine := surroundingFunctionLine;
            objectifys.(constructorName).type := surroundingFunction.name.identifier;
                    
            if(node.name.identifier = "Objectify") then
                type := node.args[1];
            else #ObjectifyWithAttributes
                type := node.args[2];
            fi;

            if(objectifys.(constructorName).type = "InstallMethod") then
                i := 1;
                while( i < Length(surroundingFunction.args)) do
                    if(IsList(surroundingFunction.args[i]) and not(IsString(surroundingFunction.args[i]))) then
                        Add(objectifys.(constructorName).inputFilters, surroundingFunction.args[i]);
                    fi;
                    i := i + 1;
                od;
            fi;


            #case Objectify(NewType(..., filters), ...)
            if (IsBound(type.type)
              and type.type = "functionCall"
              and IsBound(type.name) and type.name.identifier = "NewType") then
                filterSubtree := type.args[2];
                filters := getFilterList(filterSubtree);

            #case where we find a variable
            elif(isVariable(type)) then
                if(IsBound(type.subtype)
                  and type.subtype = "RefLVar") then
                    filters := handleLocalVariable(surroundingFunction, type);
                elif(IsBound(type.subtype)
                  and type.subtype = "RefGVar") then
                    filters := handleGlobalVariable(type.identifier);
                fi;
            fi;

            #if no proper filters could be deduced
            if(objectifyFound = true and Length(filters)=0) then
                filters := ["IsObject"];
            fi;

            if(IsBound(objectifys.(constructorName).filters)) then
                Add(objectifys.(constructorName).filters, filters);
            else
                objectifys.(constructorName).filters := [filters];
            fi;

        else #multiple Objectify calls within single function
            objectifys.(constructorName).multipleObjectifiesFlag := true;

            objectifys.(constructorName).filters := ["IsObject"];
        fi;
    fi;

    
    return rec(line := line, surroundingFunction := surroundingFunction,
      surroundingFunctionLine := surroundingFunctionLine,
      objectifyFunctionLine := objectifyFunctionLine, objectifys := objectifys);
end;


traverseJsonRecordDriver :=
function(obj, recordHandler)
    local stack, node, el, i, line, args, list, stackInfo_list, temp;

    stack := [obj];
    temp := rec();
    
    while Length(stack) > 0 do
        #pop last element and traverse subtree
        node := Remove(stack);

        if(not IsString(node) and IsList(node)) then
            stackInfo_list := rec( type  := "stackInfo_list",
                                   list  := node,
                                   index := 1    );

            Add(stack, stackInfo_list);
            Add(stack, node[stackInfo_list.index]);
        elif (IsRecord(node) and IsBound(node.type)
          and node.type="stackInfo_list") then
            #when iterating a list there will always be the following
            # order: stack(..., stackInfo_list, el_of_list)

            #stackInfo_list:
            #   node.type = stackInfo_list
            #   node.list = list
            #   node.index = #index of list that was last explored
            list := node.list;
            
            #only keep iterating over the list if not already fully
            # explored
            if( node.index < Length(list) ) then
                stackInfo_list := rec( type  := "stackInfo_list",
                                       list  := list,
                                       index := node.index + 1    );
                Add(stack, stackInfo_list);
                Add(stack, list[stackInfo_list.index]);
            fi;

        elif (IsRecord(node)) then 
            temp.node := node;
            temp := recordHandler(temp);
            
            #always keep traversing down
            for i in RecNames(node) do
                Add(stack, node.(i));
            od;
        fi;
    od;

    return temp;
end;





#convenience function to analyze all files for objectify calls
#TODO: make path to JSON files configurable, as well as name of output file
#TODO: update this to account for changes in processJSON
#callProcessJSON := 
#function()
#    local dest, file, string, f, gapPath, record;
#
#    gapPath := GAPInfo.RootPaths[2];
#    dest := Concatenation(gapPath, "/objectifies.txt");
#
#    PrintTo(dest, "");
#
#    for f in DirectoryContents(Concatenation(gapPath, "/json_output")) do
#       if(f[1] = '.') then
#           continue;
#       fi;
#
#       file := IO_File(Concatenation(gapPath, "/json_output/", f), "r");;
#       AppendTo(dest, f, "\n");
#       string := IO_ReadUntilEOF(file);;
#       record := JsonStringToGap(string);;
#       
#       declareConstructors(record, dest);;
#    od;
#end;
