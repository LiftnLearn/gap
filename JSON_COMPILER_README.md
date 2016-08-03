# JSON COMPILER
This branch contains an adaption of the GAP-to-C compiler that converts
a GAP-function into a JSON parse tree.

Additionally there is a basic type inference for functions implemented.
While the MitM-package can be used to determine the output type of functions
containing objectify-calls this is a more generalized version that is
supposed to work for all functions.

##What it can do
The JSON-compiler works on all library files and produces sensible output.

The type inference works on basic cases. 

###How it works
The JSON compiler works very much like the GAP-to-C compiler as it
is built from the same skeleton. Instead of outputting C code it simply
prints JSON. The only other significant change is that for the C code
inner functions were pre-compiled but in this case they are "compiled"
in place s.t. the function definitions are output at the proper place.

The type inference does the opposite work as the compiler by splitting
the syntax tree again into expressions and statements and always keeping an
environment of types for all current variables. It might recursively analyze
other functions in case they are called.

##What it can't do
Depending on the final use case the JSON-output might have to be minorly
adapted.

The type inference is not reliable in all cases until all language
features are properly implemented. It also returns quite often IsObject
as an output type. The latter one can probably best be fixed by hard-coding
the output types of certain functions (i.e. Length -> Int) as the results
become easily meaningless as soon as IsObject appears somewhere.

The results of the MitM-package for functions containing Objectify-calls
is currently also not being considered, even though the type inference
does recognize if a function contains an Objectify-call.

Currently can also not just pass operations to the type inference but
has to manually enter which function will be called with the given
argument types.

##Bugs
No bugs are currently known for the compiler.

The type inference does not work on recursive functions as the analyser would
then call itself. This also applies if the recursion is just indirect.

##How to use
The JSON compiler can be used by passing a file name and function
to be compiled to the "JSON_CompileFunc"-function. The third argument has no
function, but has to be a string and is there for conforming with the signature
of the original CompileFunc for C code.

    JSON_CompileFunc(tempFileName, func, "");

The output type of function can be determined using the functionType-files.
The function 'determineMethodOutputType' expects a function to be
compiled and the filters of the arguments being passed.

    Read("functionType.gi");

    determineMethodOutputType("IS_PGROUP_FOR_NILPOTENT", [IsObject]);

## Examples

    determineMethodOutputType("IS_PGROUP_FROM_SIZE", [IsObject])) -> IsBool

    #example of speculative method, there is an implicit void-return at
    #the end of the function
    determineMethodOutputType("IsPGroup", [IsGroup and IsNilpotentGroup]);
