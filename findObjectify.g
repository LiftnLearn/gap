#Read("gaptypes.g");

outputFile := "";

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

handleRecord :=
    function(stack, node, line, surroundingFunction)
        local args, assignments, type, i, filterSubtree,
          listOfAssignmentFilters, filters, filterIDs, objectifyFound;

        filters := [];
        objectifyFound := false;

        if(IsBound(node.type) and node.type = "debugInfo") then
            line := node.line; #as we are using essentially a
                               #stack this might give some
                               #peculiar results

        #cache a surrounding function if one is found
        elif (IsRecord(node)
                  and IsBound(node.type)
                  and node.type = "functionCall"
                  and IsBound(node.name.identifier)
                  and (node.name.identifier = "InstallMethod" 
                  or node.name.identifier = "InstallOtherMethod"
                  or node.name.identifier = "InstallGlobalFunction")) then
            surroundingFunction := node; 

        #Objectify(..., ...) being found
        elif(IsBound(node.type)
          and node.type = "functionCall"
          and IsBound(node.name.identifier)
          and (node.name.identifier = "Objectify"
          or node.name.identifier = "ObjectifyWithAttributes")) then

            objectifyFound := true;
            AppendTo(outputFile, "line ", line, ": Objectify found", "\n");
    
            if(node.name.identifier = "Objectify") then
                type := node.args[1];
            else #ObjectifyWithAttributes
                type := node.args[2];
            fi;

            #case Objectify(NewType(..., filters), ...)
            if (IsBound(type.type)
              and type.type = "functionCall"
              and type.name.identifier = "NewType") then
                filterSubtree := type.args[2];
                
                filters := getFilterList(filterSubtree);

            #more general case
            else
                #distinguish cases -> TODO: modularize/refactor here,
                #also put the case from above here (the previous elif)

                #case where we find a local variable
                if(IsRecord(type) and IsBound(type.type)
                  and type.type = "variable") then
                    if(IsBound(type.subtype)
                      and type.subtype = "RefLVar") then
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

                        #find common subset
                        if(Length(listOfAssignmentFilters) > 0) then
                            filters := Intersection(listOfAssignmentFilters);
                        fi;

                        #   filter those for the right variable
                        #   if necessary find common subset
                    elif(IsBound(type.subtype)
                      and type.subtype = "RefGVar") then

                        if(IsBoundGlobal(type.identifier)
                          and IsType(ValueGlobal(type.identifier))) then
                            #return all filters of this type

                            filterIDs := TRUES_FLAGS(ValueGlobal(type.identifier)![2]);
                            for i in filterIDs do
                                Add(filters, NameFunction(FILTERS[i]));
                            od;
                        fi;

                    fi;
                fi;
            fi;
        fi; 

        #always keep traversing down
        for i in RecNames(node) do
            Add(stack, node.(i));
        od;

        if(objectifyFound = true and Length(filters) = 0) then
            filters := ["IsObject"];
        fi;

        return rec(line:=line, surroundingFunction:=surroundingFunction,
          filters:=filters);
    end;


processJSON :=
    function(obj, file)
        local stack, node, el, i, line, args, list, stackInfo_list,
              surroundingFunction, temp;

        outputFile := file;

        surroundingFunction := false; #is there some null equivalent in GAP?

        stack := [obj];
        line := 1;
        
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

                temp := handleRecord(stack, node, line, surroundingFunction);

                surroundingFunction := temp.surroundingFunction;
                line := temp.line;

                if (Length(temp.filters) > 0) then
                    AppendTo(outputFile, "\t", temp.filters, "\n");
                fi;

            else
#                if(IsString(node) and node = "Objectify") then
#                    Print("line ", line, ": Objectify found", "\n");
#                fi;
            fi;

        od;

    end;
