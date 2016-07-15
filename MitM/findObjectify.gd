LoadPackage("json");;

isFunctionCall :=
function(node)
    return (IsRecord(node)
      and IsBound(node.type)
      and node.type = "functionCall");
end;

isSurroundingFunction :=
function(statNode)
    return (isFunctionCall(statNode) and (statNode.name.identifier in
      ["InstallMethod", "InstallOtherMethod", "InstallGlobalFunction",
       "BindGlobal"]));
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

#########################################################################
# flattenToStrings
#
# function(root, flattenList)
# 
# Function that takes a record or list and returns all contained strings.
#
# root : record or list to be searched
# flattenList : list into which resulting strings should be inserted
#
#########################################################################
flattenToStrings := function(root, flattenList) end;

#########################################################################
# getFilterList
#
# function(root)
# 
# Function that takes a record or list and returns all contained filters.
#
# root : record or list to be searched
#
#########################################################################
getFilterList := function(root) end;

#########################################################################
# flattenToRecords
#
# function(root, recordList)
# 
# Function that takes a record or list and returns all contained subrecords.
#
# root : record or list to be searched
# recordList : list into which resulting records should be inserted
#
#########################################################################
flattenToRecords := function(root, recordList) end;

#########################################################################
# findLocalAssignments
#
# function(surroundingFunction, identifier)
# 
# Function that takes a function and searches in the function body for
# occurences of local assignments to the variable with the string 'identifier'
# as it's identifier.
#
# surroundingFunction : function whose body is supposed to be searched 
#                       for local assignments
# identifier : searches surroundingFunction for all assignments to the
#               local variable named 'identifier'
#
#########################################################################
findLocalAssignments := function(surroundingFunction, identifier) end;

#########################################################################
# findGlobalVariable
#
# function(identifier)
# 
# Function that takes an identifier and checks if it is a globally bound
# type. If so it returns a list of associated filters.
#
# identifier : name of a type that is in the global scope
#
#########################################################################
handleGlobalVariable := function(identifier) end;

#########################################################################
# deduceInputFilters 
#
# function(args)
# 
# Function that takes a list of arguments and returns the one which
# denotes the inputFilters used by the method selection.
#
# args : list of arguments to be searched for inputFilters
#
#########################################################################
deduceInputFilters := function(args) end;

#########################################################################
# handleFirstObjectify
#
# function(node, objectifys, constructorName)
# 
# Function that finds the used filters within an Objectify call if possible.
#
# node : current node being analyzed from syntax tree
# objectifys : collection-object containing already found objectifies
# constructorName : name of method/function being installed
# surroundingFunction : function surrounding node
#
#########################################################################
handleFirstObjectify := function(node, objectifys,
                        constructorName, surroundingFunction) end;

#########################################################################
# analyzeRecordForObjectifies
#
# function(obj)
# 
# Function that takes a wrapped record and searches it for objectify calls.
# It returns a record and annotates it with metadata such that it can
# keep information over the run of the record traversal.
#
# To be used together with traverseJsonRecordDriver
#
# The objectifies-attribute on the returned object contains all found
# 'Objectify'-calls.
#
# obj : Object that contains a record at obj.node
#
#########################################################################
analyzeRecordForObjectifies := function(obj) end;

#########################################################################
# traverseJsonRecordDriver
#
# function(obj, recordHandler)
# 
# Function that takes a record that was created from the json
# representation of a GAP-file and traverses it. It passes every node of
# the object in a depth first manner and applies the 'recordHandler'-
# function to every record it encounters. Finally it returns the last
# result it obtained from the recordHandler.
#
# The recordHandler must have the signature function(obj) and obtains a
# record with the current node as rec(node:=node) originally and in every
# following iteration it always gets the record it returned supplemented
# with the current node. The recordHandler must return an record.
#
# root : record representing a syntax tree to be searched
# recordHandler : function to be applied to all encountered records
#
#########################################################################
traverseJsonRecordDriver := function(obj, recordHandler) end;

#########################################################################
# processJson
#
# function(record)
#
# Convenience wrapper that takes a record that was created from the
# json representation of a GAP-file and returns an object containing
# all Objectify calls within the original GAP-file.
#
# record : record obtained from json representation of a GAP-file
#
#########################################################################
processJson := 
function(record)
    return traverseJsonRecordDriver(record, analyzeRecordForObjectifies).objectifys;
end;
