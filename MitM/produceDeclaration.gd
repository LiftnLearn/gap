Read("MitM/findObjectify.gi");

isDeclareOperation :=
function(node)
    return (isFunctionCall(node) and node.name.identifier = "DeclareOperation");
end;

#########################################################################
# unwrapFilters
#
# function(list)
#
# Gets a list of filter-records and returns a string representing it nicely
#
# Example: [rec(...HasSize...IsObject), rec(IsObject...)] ->
#          ["HasSize and IsObject", "IsObject"]
#
# list: list of filter-records
#
#########################################################################
unwrapFilters := function(list) end;

#########################################################################
# printListOfStrings
#
# function(list)
#
# Gets a list of strings and returns a string representing it nicely.
#
# Example: ["This", "is", "an", "example"] -> [This, is, an, example]
#
# list: list of strings
#
#########################################################################
printListOfStrings := function(list) end;

#########################################################################
# declareOperationHandler
#
# function(obj)
#
# Function that takes a wrapped record and searches it for
# 'DeclareOperation'-calls. It returns a record and annotates it with
# metadata such that it can keep information over the run of the record
# traversal.
#
# To be used together with traverseJsonRecordDriver in findObjectify.gd
#
# The declaredOperations-attribute on the returned object contains all found
# 'DeclareOperation'-calls.
#
# obj : Object that contains a record at obj.node 
#
#########################################################################
declareOperationHandler := function(obj) end;

#########################################################################
# declareOperations
#
# function(headerRec, implementRec, outputDest)
# 
# Function that processes a header and a implementation file in order to
# autogenerate type-annotated MitM-equivalents. The result is written to
# outputDest.
#
# headerRec : record that represents the original declaration-file (*.gd)
# implementRec : record that represents the original declaration-file (*.gi)
# outputDest : path to which to write new declaration-file
#
#########################################################################
declareOperations := function(headerRec, implementRec, outputDest) end;
