LoadPackage("json");

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


#idea: first fully flatten the tree and then simply look at all
#       strings if they are filters, then append them
#TODO: can filters be combined differently than just with AND -> 
#       would need more sophisticated algorithm
getFilterList :=
function(root)
    local filterList, flattenedList, i; 
    
    #flatten the tree
    flattenedList := [];
    flattenToStrings(root, flattenedList);

    filterList := [];
    for i in flattenedList do
        if(IsBoundGlobal(i) and IsFilter(ValueGlobal(i))) then
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
          and IsBound(r.left) and r.left = "K") then
            Add(assignments, r);
        fi;
    od;

    return assignments;
end;

isFunctionCall :=
function(node)
    return (IsRecord(node)
      and IsBound(node.type)
      and node.type = "functionCall");
end;

isSurroundingFunction :=
function(statNode)
    return (isFunctionCall(statNode) and (statNode.name.identifier = "InstallMethod" 
      or statNode.name.identifier = "InstallOtherMethod"
      or statNode.name.identifier = "InstallGlobalFunction"
      or statNode.name.identifier = "BindGlobal"));
end;

isObjectify :=
function(node)
    return (IsBound(node.type)
      and node.type = "functionCall"
      and IsBound(node.name)
      and IsBound(node.name.identifier)
      and (node.name.identifier = "Objectify"
      or node.name.identifier = "ObjectifyWithAttributes"));
end;

isVariable :=
function(type)
    return IsRecord(type) and IsBound(type.type)
      and type.type = "variable";
end;

#TODO: as the functions are in local scope it doesn't
#help to pass variables, does it?
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
function(type)
    local filterIDs, filters, i;

    filters := [];

    if(IsBoundGlobal(type.identifier)
      and IsType(ValueGlobal(type.identifier))) then
        #return all filters of this type

        filterIDs := TRUES_FLAGS(ValueGlobal(type.identifier)![2]);
        for i in filterIDs do
            Add(filters, NameFunction(FILTERS[i]));
        od;
    fi;

    return filters;
end;

#TODO: put handling of Objectify's in separate function
#TODO: write header that describes how all these functions are related
#known bugs: 
handleRecord :=
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
    elif(isObjectify(node) and not(surroundingFunction.name.identifier = "BindGlobal")) then
        objectifyFound := true;
        constructorName := surroundingFunction.args[1].identifier;

        if(not(IsBound(objectifys.(constructorName)))) then
            objectifys.(constructorName) := rec();
            objectifys.(constructorName).type := surroundingFunction.name.identifier;
            objectifys.(constructorName).multipleObjectifiesFlag := false;
        fi;

        #first Objectify call within surrounding function
        if(not(objectifyFunctionLine = surroundingFunctionLine)) then
            objectifyFunctionLine := surroundingFunctionLine;
                    
            if(node.name.identifier = "Objectify") then
                type := node.args[1];
            else #ObjectifyWithAttributes
                type := node.args[2];
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
                    filters := handleGlobalVariable(type);
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

isDeclareOperation :=
function(node)
    return (isFunctionCall(node) and node.name.identifier = "DeclareOperation");
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
        declaredOperation.inputFilters := [];

        i := 1;
        while (i <= Length(node.args[2])) do
            Add(declaredOperation.inputFilters, node.args[2][i].identifier);
            #TODO: verify that this is correct( what about with AND combined filters?)
            i := i + 1;
        od;

        declaredOperations.(declaredOperation.name) := declaredOperation; 
    fi;

    return rec(declaredOperations := declaredOperations);
end;

processJson := 
function(record)
    return traverseJsonRecordDriver(record, handleRecord).objectifys;
end;

#headerRec : record that represents the original declaration-file (*.gd)
#implementRec : record that represents the original declaration-file (*.gi)
#outputDest : path to which to write new declaration-file
#This function produces a declaration file containing the MitM-constructors
# and InstallMethods that save the arguments used to create objects.
#
#pseudocode
#for each DeclareOperation write MitM_DeclareConstructor(operationName, 
#[inputFilters...], outputFilters(expand using AND));
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
        objectifys.(recName) := Intersection(objectifys.(recName).filters);
    od;
    #TODO: make sure invalid values are handled (i.e. where filter not determined)

    #merge and output to file 
    for recName in RecNames(objectifys) do
        if(not(IsBound(declaredOperations.(recName)))) then
            continue;
        fi;

        AppendTo(outputDest, "MitM_DeclareConstructor( \"", recName, "\", ",
                 declaredOperations.(recName).inputFilters, ", ");
        
        i := 1;
        while (i <= Length(objectifys.(recName))) do
            if(i > 1) then
                AppendTo(outputDest, " and ");
            fi;
            AppendTo(outputDest, objectifys.(recName)[i]);
            i := i + 1;
        od;

        AppendTo(outputDest, ");\n");
    od; 
end;

#for each InstallMethod write MitM_InstallMethod(same signature...)
createMitM_InstallMethods :=
function()
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

