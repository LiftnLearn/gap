LoadPackage("json");


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

#known bugs: 
handleRecord :=
    function(stack, node, line, surroundingFunction,
                surroundingFunctionLine, objectifyFunctionLine, message)

        local args, assignments, type, i, filterSubtree, statNode,
          listOfAssignmentFilters, filters, filterIDs, objectifyFound,
          out, multipleObjectifiesFlag;

        out := OutputTextString(message, true); 

        filters := [];
        objectifyFound := false;
        multipleObjectifiesFlag := false;

        if(IsBound(node.type) and node.type = "debugInfo") then
            line := node.line; #as we are using essentially a
                               #stack this might give some
                               #peculiar results

            statNode := node.stat;
 
            #cache a surrounding function if one is found
            if (IsRecord(statNode)
                  and IsBound(statNode.type)
                  and statNode.type = "functionCall"
                  and IsBound(statNode.name) #shouldn't this always be the case?
                  and IsBound(statNode.name.identifier)
                  and (statNode.name.identifier = "InstallMethod" 
                  or statNode.name.identifier = "InstallOtherMethod"
                  or statNode.name.identifier = "InstallGlobalFunction"
                  or statNode.name.identifier = "BindGlobal")) then
                surroundingFunction := statNode; 
                surroundingFunctionLine := node.line;
            fi;

        #Objectify(..., ...) being found
        elif(IsBound(node.type)
          and node.type = "functionCall"
          and IsBound(node.name)
          and IsBound(node.name.identifier)
          and (node.name.identifier = "Objectify"
          or node.name.identifier = "ObjectifyWithAttributes")) then

            objectifyFound := true;
            
            if(not(objectifyFunctionLine = surroundingFunctionLine)) then
                CloseStream(out);
                AppendTo(outputFile, message);
                
                message := "";
                out := OutputTextString(message, true);
                objectifyFunctionLine := surroundingFunctionLine;
            else
                multipleObjectifiesFlag := true;
                CloseStream(out);

                message := "\tMultiple Objectify calls where found in the function ending at line: "; #TODO: insert line
                out := OutputTextString(message, true);
                AppendTo(out, objectifyFunctionLine, "\n");
                CloseStream(out);

                return rec(line := line, surroundingFunction := surroundingFunction,
                  surroundingFunctionLine := surroundingFunctionLine, filters := filters,
                  objectifyFunctionLine := objectifyFunctionLine, message := message);
            fi;
            
            AppendTo(out, "\t", "line ", line, ": Objectify found", "\n");
    
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

        if (Length(filters) > 0) then
            AppendTo(out, "\t\t", filters, "\n");
        fi;

        CloseStream(out);

        return rec(line := line, surroundingFunction := surroundingFunction,
                  surroundingFunctionLine := surroundingFunctionLine, filters := filters,
                  objectifyFunctionLine := objectifyFunctionLine, message := message);

    end;


processJSON :=
    function(obj, file)
        local stack, node, el, i, line, args, list, stackInfo_list,
              surroundingFunction, surroundingFunctionLine, temp,
              objectifyFunctionLine, message;

        outputFile := file;

        surroundingFunction := false; #is there some null equivalent in GAP?
        surroundingFunctionLine := 1;

        objectifyFunctionLine := 1;
        
        message := "";

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

                temp := handleRecord(stack, node, line,
                            surroundingFunction, surroundingFunctionLine,
                            objectifyFunctionLine, message);

                surroundingFunction := temp.surroundingFunction;
                surroundingFunctionLine := temp.surroundingFunctionLine;
                objectifyFunctionLine := temp.objectifyFunctionLine;
                message := temp.message;
                line := temp.line;
        
            else
#                if(IsString(node) and node = "Objectify") then
#                    Print("line ", line, ": Objectify found", "\n");
#                fi;
            fi;

        od;

        AppendTo(outputFile, message);
    end;


record := false;

#convenience function to analyze all files for objectify calls
#TODO: make path to JSON files configurable, as well as name of output file
callProcessJSON := 
    function(gapPath)
        local dest, file, string, f;

        dest := Concatenation(gapPath, "/objectifies.txt");

        PrintTo(dest, "");

        for f in DirectoryContents(Concatenation(gapPath, "/json_output")) do
           if(f[1] = '.') then
               continue;
           fi;

           file := IO_File(Concatenation(gapPath, "/json_output/", f), "r");;
           AppendTo(dest, f, "\n");
           string := IO_ReadUntilEOF(file);;
           record := JsonStringToGap(string);;
           
           processJSON(record, dest);;
        od;
    end;

