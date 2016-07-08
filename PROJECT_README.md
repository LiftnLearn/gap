### README ###

This document describes the work done on the current project and explains
how to use any resulting software.

## Goal ##

The goal of the project is to build a prototype for semantic interfacing
between GAP and MMT. For this purpose an object in GAP needs to be
annotated with information of how it was created, s.t. it can be
exported to MMT.

## Approach ##

Every function in GAP that is at some point calling the 'Objectify'-
function is considered a constructor. The current approach is to provide
as a proof of concept for each of them a functionally equivalent wrapper
function that inside calls the original function and additionally stores
information about what arguments were used to call the function on the
resulting object.

## What was done so far ##

At first the GAP-to-C compiler was modified to produce what is essentially
a syntax tree in a JSON-output. The resulting output was then interpreted
in order to deduce as much type-information as possible for the output-
objects of each constructor (The input-types are already available in
the code). The goal here was to have the equivalent of a Las Vegas-algorithm,
where each output of a type that is given is actually correct and it
it otherwise signaled if a type cannot be deduced.

## Usage ##

In the gap/-directory of this branch you will find a script 'compileLibToJson.sh'
that has to be called with the absolute path of the gap/-directory and then
compiles all files in the gap/lib/-directory into a JSON output.

When it is executed the first time you have to manually create a 'json_output'-
folder in the gap/-directory. An example is given below:

    #mkdir json_output
    bash compileLibToJson.sh <absolute path to gap/-directory>

You can then use the GAP-file 'findObjectify.g' to extract type information.
For this purpose you should start GAP and then type the following:

    Read("findObjectify.g");
    callProcessJson("<absolute path to gap/-directory"); 
  

This will output a file called 'objectifies.txt' containing the extracted
type-information and place it in the gap/-directory. 
