/****************************************************************************
**
*W  JSON_compiler.h                  GAP source                   Felix Schmoll
**
**
**  This file declares the functions of the GAP to JSON compiler.
*/

#ifndef GAP_JSON_COMPILER_H
#define GAP_JSON_COMPILER_H


/****************************************************************************
**
*F  JSON_CompileFunc(<output>,<func>,<name>,<magic1>,<magic2>) . . . . . . compile
*/
extern Int JSON_CompileFunc (
            Char *              output,
            Obj                 func,
            Char *              name,
            Int                 magic1,
            Char *              magic2 );

/****************************************************************************
**
*F  JSON_SetCompileOpts( <string> ) . . parse the compiler options from <string>
**                                 and set the appropriate variables
**                                 unrecognised options are ignored for now
*/

extern void JSON_SetCompileOpts( Char *opts );


/****************************************************************************
**

*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * *
*/

/****************************************************************************
**

*F  JSON_InitInfoCompiler() . . . . . . . . . . . . . . .  table of init functions
*/
StructInitInfo * JSON_InitInfoCompiler ( void );


#endif // GAP_JSON_COMPILER_H

/****************************************************************************
**

*E  compiler.h  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
*/
