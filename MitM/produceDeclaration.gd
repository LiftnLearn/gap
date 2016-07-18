Read("MitM/findObjectify.gi");

isDeclareOperation :=
function(node)
    return (isFunctionCall(node) and node.name.identifier = "DeclareOperation");
end;

#########################################################################
# printListWithSeparator
#
# function(list, separator)
#
# Gets a list of filters and returns a string representing it nicely
#
# Example: ([HasSize, IsObject], ", ") -> "HasSize, IsObject"
#
# list: list
# separator : string to be inserted between list elements
#
#########################################################################
connectFiltersWithAnd := function(list, separator) end;

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
# outputConstructor
#
# function(funcName)
#
# Convenience function for formatting output for global functions
#
# funcName : name of global function to be wrapped
#
#########################################################################
outputGlobalFunctions := function(funcName) end;
 
#########################################################################
# outputConstructor
#
# function(recName, inputFilters, resultFilters)
#
# Convenience function for outputting MitM_Constructors.
#
# recName : name of the function in the original constructor
# inputFilters : list of filters for input object
# resultFilters : filters that output object will adhere to
#
#########################################################################
outputConstructor := function(recName, inputFilters, resultFilters) end;

#########################################################################
# outputInstallMethods
#
# function(recName, inputFilters)
#
# Convenience function for outputting MitM_InstallMethods.
#
# recName : name of the function in the original constructor
# inputFilters : list of filters for input object (used in method selection)
# wrapperName : name of the wrapper to be called (i.e. either MitM_InstallMethod
#               or MitM_InstallOtherMethod)
#
#########################################################################
outputInstallMethods := function(recName, inputFilters, wrapperName) end;

#########################################################################
# mergeAndOutputToFile
#
# function(objectifys, declareOperations, outputDest);
#
# Does the final processing and outputs the actual declaration file.
#
# objectifys: record collecting all objectify calls in .gi file
# declareOperations : record collecting all declared operations in .gd file
# outputDest : path to output destinationn
#
#########################################################################
mergeAndOutputToFile := function(objectifys, declareOperations, outputDest) end;

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
