/****************************************************************************
**
*W  JSONcompiler.c                  GAP source                 Frank Celler
*W                                                         & Ferenc Ràkòczi
*W                                                         & Martin Schönert
*W                                                         & Felix Schmoll
**
**
*Y  Copyright (C)  1997,  Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
*Y  (C) 1998 School Math and JSON_Comp. Sci., University of St Andrews, Scotland
*Y  Copyright (C) 2002 The GAP Group
**
**  This file contains the GAP to C compiler.
*/
#include        <stdio.h>               /* Input/Output for debugging      */
#include        <stdlib.h> 
#include        <ctype.h>
#include        <errno.h>
#include        <stdarg.h>              /* variable argument list macros   */

#include        "system.h"              /* Ints, UInts                     */

#include        "gasman.h"              /* garbage collector               */
#include        "objects.h"             /* objects                         */
#include        "scanner.h"             /* scanner                         */

#include        "gvars.h"               /* global variables                */

#include        "ariths.h"              /* basic arithmetic                */
#include        "integer.h"

#include        "bool.h"                /* booleans                        */

#include        "gap.h"                 /* error handling, initialisation  */

#include        "calls.h"               /* generic call mechanism          */
/*N 1996/06/16 mschoene func expressions should be different from funcs    */

#include        "lists.h"               /* generic lists                   */

#include        "records.h"             /* generic records                 */
#include        "precord.h"             /* plain records                   */

#include        "plist.h"               /* plain lists                     */

#include        "stringobj.h"              /* strings                         */

#include        "code.h"                /* coder                           */

#include        "exprs.h"               /* expressions                     */
#include        "stats.h"               /* statements                      */

#include        "JSON_compiler.h"            /* compiler                        */

#include        "hpc/tls.h"             /* thread-local storage            */

#include        "vars.h"                /* variables                       */

FILE* json;

/****************************************************************************
**

*F * * * * * * * * * * * * * compilation flags  * * * * * * * * * * * * * * *
*/


/****************************************************************************
**


*V  JSON_CompFastIntArith  . . option to emit code that handles small ints. faster
*/
Int JSON_CompFastIntArith;


/****************************************************************************
**
*V  JSON_CompFastPlainLists  . option to emit code that handles plain lists faster
*/
Int JSON_CompFastPlainLists ;


/****************************************************************************
**
*V  JSON_CompFastListFuncs . . option to emit code that inlines calls to functions
*/
Int JSON_CompFastListFuncs;


/****************************************************************************
**
*V  JSON_CompCheckTypes  . . . . option to emit code that assumes all types are ok.
*/
Int JSON_CompCheckTypes ;


/****************************************************************************
**
*V  JSON_CompCheckListElements .  option to emit code that assumes list elms exist
*/
Int JSON_CompCheckListElements;

/****************************************************************************
**
*V  JSON_CompOptNames . .  names for all the compiler options passed by gac
**
*/

struct JSON_CompOptStruc { const Char *extname;
  Int *variable;
  Int val;};

struct JSON_CompOptStruc JSON_CompOptNames[] = {
  { "FAST_INT_ARITH", &JSON_CompFastIntArith, 1 },
  { "FAST_PLAIN_LISTS", &JSON_CompFastPlainLists, 1 },
  { "FAST_LIST_FUNCS", &JSON_CompFastListFuncs, 1 },
  { "NO_CHECK_TYPES", &JSON_CompCheckTypes, 0 },
  { "NO_CHECK_LIST_ELMS", &JSON_CompCheckListElements, 0 }};

#define N_JSON_CompOpts  (sizeof(JSON_CompOptNames)/sizeof(struct JSON_CompOptStruc))


/****************************************************************************
**
*F  SetJSON_CompileOpts( <string> ) . . parse the compiler options from <string>
**                                 and set the appropriate variables
**                                 unrecognised options are ignored for now
*/

void SetJSON_CompileOpts( Char *opts )
{
  Char *s = opts;
  Int i;
  while (*s)
    {
      while (IsSpace(*s))
        s++;
      for (i = 0; i < N_JSON_CompOpts; i++)
        {
          if (0 == strncmp(JSON_CompOptNames[i].extname,
                             s,
                             strlen(JSON_CompOptNames[i].extname)))
            {
              *(JSON_CompOptNames[i].variable) = JSON_CompOptNames[i].val;
              break;
            }
        }
      while (*s && *s != ',')
        s++;
      if (*s == ',')
        s++;
    }
  return;
}

/****************************************************************************
**
*V  JSON_CompCheckPosObjElements .  option to emit code that assumes pos elm exist
*/
Int JSON_CompCheckPosObjElements;


/****************************************************************************
**
*V  JSON_CompPass  . . . . . . . . . . . . . . . . . . . . . . . . . compiler pass
**
**  'JSON_CompPass' holds the number of the current pass.
**
**  The compiler does two passes over the source.
**
**  In the first pass it only collects information but emits no code.
**
**  It finds  out which global  variables and record names  are used, so that
**  the  compiler can output  code to define  and initialize global variables
**  'G_<name>' resp. 'R_<name>' to hold their identifiers.
**
**  It finds out   which arguments and local  variables  are used  as  higher
**  variables  from inside local functions,  so that  the compiler can output
**  code to allocate and manage a stack frame for them.
**
**  It finds out how many temporary variables are used, so that the compiler
**  can output code to define corresponding local variables.
**
**  In the second pass it emits code.
**
**  The only difference between the  first pass and  the second pass is  that
**  'JSON_Emit'  emits  no code  during the first  pass.   While  this causes many
**  unneccessary  computations during the first pass,  the  advantage is that
**  the two passes are guaranteed to do exactly the same computations.
*/
Int JSON_CompPass;


/****************************************************************************
**

*F * * * * * * * * * * * * temp, C, local functions * * * * * * * * * * * * *
*/


/****************************************************************************
**
*T  CVar  . . . . . . . . . . . . . . . . . . . . . . .  type for C variables
**
**  A C variable represents the result of compiling an expression.  There are
**  three cases (distinguished by the least significant two bits).
**
**  If the  expression is an  immediate integer  expression, the  C  variable
**  contains the value of the immediate integer expression.
**
**  If the  expression is an immediate reference  to a  local variable, the C
**  variable contains the index of the local variable.
**
**  Otherwise the expression  compiler emits code  that puts the value of the
**  expression into a  temporary variable,  and  the C variable contains  the
**  index of that temporary variable.
*/
typedef UInt           CVar;

#define IS_INTG_CVAR(c) ((((UInt)(c)) & 0x03) == 0x01)
#define INTG_CVAR(c)    (((Int)(c)) >> 2)
#define CVAR_INTG(i)    ((((UInt)(i)) << 2) + 0x01)

#define IS_TEMP_CVAR(c) ((((UInt)(c)) & 0x03) == 0x02)
#define TEMP_CVAR(c)    (((UInt)(c)) >> 2)
#define CVAR_TEMP(l)    ((((UInt)(l)) << 2) + 0x02)

#define IS_LVAR_CVAR(c) ((((UInt)(c)) & 0x03) == 0x03)
#define LVAR_CVAR(c)    (((UInt)(c)) >> 2)
#define CVAR_LVAR(l)    ((((UInt)(l)) << 2) + 0x03)


/****************************************************************************
**
*F  JSON_SetInfoCVar( <cvar>, <type> ) . . . . . . .  set the type of a C variable
*F  JSON_GetInfoCVar( <cvar> ) . . . . . . . . . . .  get the type of a C variable
*F  JSON_HasInfoCVar( <cvar>, <type> ) . . . . . . . test the type of a C variable
**
*F  JSON_NewInfoCVars()  . . . . . . . . . allocate a new info bag for C variables
*F  JSON_CopyInfoCVars( <dst>, <src> ) . .  copy between info bags for C variables
*F  JSON_MergeInfoCVars( <dst>, <src> )  . . . merge two info bags for C variables
*F  JSON_IsEqInfoCVars( <dst>, <src> ) . . . compare two info bags for C variables
**
**  With each function we  associate a C  variables information bag.  In this
**  bag we store  the number of the  function, the number of local variables,
**  the  number of local  variables that  are used  as higher variables,  the
**  number  of temporaries  used,  the number of  loop  variables needed, the
**  current  number  of used temporaries.
**
**  Furthermore for  each local variable and  temporary we store what we know
**  about this local variable or temporary, i.e., whether the variable has an
**  assigned value, whether that value is an integer, a boolean, etc.
**
**  'JSON_SetInfoCVar' sets the    information   for  the  C variable      <cvar>.
**  'JSON_GetInfoCVar' gets   the   information  for   the  C    variable  <cvar>.
**  'JSON_HasInfoCVar' returns true if the C variable <cvar> has the type <type>.
**
**  'JSON_NewInfoCVars'  creates    a    new    C  variables     information  bag.
**  'JSON_CopyInfoCVars' copies the C  variables information from <src> to  <dst>.
**  'JSON_MergeInfoCVars' merges the C variables information  from <src> to <dst>,
**  i.e., if there are two paths to a  certain place in  the source and <dst>
**  is the information gathered  along one path  and <src> is the information
**  gathered along the other path, then  'JSON_MergeInfoCVars' stores in <dst> the
**  information for   that   point  (independent   of  the  path  travelled).
**  'JSON_IsEqInfoCVars' returns   true  if <src>    and <dst> contain   the  same
**  information.
**
**  Note that  the numeric  values for the  types  are defined such  that  if
**  <type1> implies <type2>, then <type1> is a bitwise superset of <type2>.
*/
typedef UInt4           LVar;

#define INFO_FEXP(fexp)         PROF_FUNC(fexp)
#define NEXT_INFO(info)         PTR_BAG(info)[0]
#define NR_INFO(info)           (*((Int*)(PTR_BAG(info)+1)))
#define NLVAR_INFO(info)        (*((Int*)(PTR_BAG(info)+2)))
#define NHVAR_INFO(info)        (*((Int*)(PTR_BAG(info)+3)))
#define NTEMP_INFO(info)        (*((Int*)(PTR_BAG(info)+4)))
#define NLOOP_INFO(info)        (*((Int*)(PTR_BAG(info)+5)))
#define CTEMP_INFO(info)        (*((Int*)(PTR_BAG(info)+6)))
#define TNUM_LVAR_INFO(info,i)  (*((Int*)(PTR_BAG(info)+7+(i))))

#define TNUM_TEMP_INFO(info,i)  \
    (*((Int*)(PTR_BAG(info)+7+NLVAR_INFO(info)+(i))))

#define SIZE_INFO(nlvar,ntemp)  (sizeof(Int) * (8 + (nlvar) + (ntemp)))

#define W_UNUSED                0       /* TEMP is currently unused        */
#define W_HIGHER                (1L<<0) /* LVAR is used as higher variable */
#define W_UNKNOWN               ((1L<<1) | W_HIGHER)
#define W_UNBOUND               ((1L<<2) | W_UNKNOWN)
#define W_BOUND                 ((1L<<3) | W_UNKNOWN)
#define W_INT                   ((1L<<4) | W_BOUND)
#define W_INT_SMALL             ((1L<<5) | W_INT)
#define W_INT_POS               ((1L<<6) | W_INT)
#define W_BOOL                  ((1L<<7) | W_BOUND)
#define W_FUNC                  ((1L<<8) | W_BOUND)
#define W_LIST                  ((1L<<9) | W_BOUND)

#define W_INT_SMALL_POS         (W_INT_SMALL | W_INT_POS)

void            JSON_SetInfoCVar (
    CVar                cvar,
    UInt                type )
{
    Bag                 info;           /* its info bag                    */

    /* get the information bag                                             */
    info = INFO_FEXP( CURR_FUNC );

    /* set the type of a temporary                                         */
    if ( IS_TEMP_CVAR(cvar) ) {
        TNUM_TEMP_INFO( info, TEMP_CVAR(cvar) ) = type;
    }

    /* set the type of a lvar (but do not change if its a higher variable) */
    else if ( IS_LVAR_CVAR(cvar)
           && TNUM_LVAR_INFO( info, LVAR_CVAR(cvar) ) != W_HIGHER ) {
        TNUM_LVAR_INFO( info, LVAR_CVAR(cvar) ) = type;
    }
}

Int             JSON_GetInfoCVar (
    CVar                cvar )
{
    Bag                 info;           /* its info bag                    */

    /* get the information bag                                             */
    info = INFO_FEXP( CURR_FUNC );

    /* get the type of an integer                                          */
    if ( IS_INTG_CVAR(cvar) ) {
        return ((0 < INTG_CVAR(cvar)) ? W_INT_SMALL_POS : W_INT_SMALL);
    }

    /* get the type of a temporary                                         */
    else if ( IS_TEMP_CVAR(cvar) ) {
        return TNUM_TEMP_INFO( info, TEMP_CVAR(cvar) );
    }

    /* get the type of a lvar                                              */
    else if ( IS_LVAR_CVAR(cvar) ) {
        return TNUM_LVAR_INFO( info, LVAR_CVAR(cvar) );
    }

    /* hmm, avoid warning by compiler                                      */
    else {
        return 0;
    }
}

Int             JSON_HasInfoCVar (
    CVar                cvar,
    Int                 type )
{
    return ((JSON_GetInfoCVar( cvar ) & type) == type);
}


Bag             JSON_NewInfoCVars ( void )
{
    Bag                 old;
    Bag                 new;
    old = INFO_FEXP( CURR_FUNC );
    new = NewBag( TNUM_BAG(old), SIZE_BAG(old) );
    return new;
}

void            JSON_CopyInfoCVars (
    Bag                 dst,
    Bag                 src )
{
    Int                 i;
    if ( SIZE_BAG(dst) < SIZE_BAG(src) )  ResizeBag( dst, SIZE_BAG(src) );
    if ( SIZE_BAG(src) < SIZE_BAG(dst) )  ResizeBag( src, SIZE_BAG(dst) );
    NR_INFO(dst)    = NR_INFO(src);
    NLVAR_INFO(dst) = NLVAR_INFO(src);
    NHVAR_INFO(dst) = NHVAR_INFO(src);
    NTEMP_INFO(dst) = NTEMP_INFO(src);
    NLOOP_INFO(dst) = NLOOP_INFO(src);
    CTEMP_INFO(dst) = CTEMP_INFO(src);
    for ( i = 1; i <= NLVAR_INFO(src); i++ ) {
        TNUM_LVAR_INFO(dst,i) = TNUM_LVAR_INFO(src,i);
    }
    for ( i = 1; i <= NTEMP_INFO(dst) && i <= NTEMP_INFO(src); i++ ) {
        TNUM_TEMP_INFO(dst,i) = TNUM_TEMP_INFO(src,i);
    }
}

void            JSON_MergeInfoCVars (
    Bag                 dst,
    Bag                 src )
{
    Int                 i;
    if ( SIZE_BAG(dst) < SIZE_BAG(src) )  ResizeBag( dst, SIZE_BAG(src) );
    if ( SIZE_BAG(src) < SIZE_BAG(dst) )  ResizeBag( src, SIZE_BAG(dst) );
    if ( NTEMP_INFO(dst)<NTEMP_INFO(src) )  NTEMP_INFO(dst)=NTEMP_INFO(src);
    for ( i = 1; i <= NLVAR_INFO(src); i++ ) {
        TNUM_LVAR_INFO(dst,i) &= TNUM_LVAR_INFO(src,i);
    }
    for ( i = 1; i <= NTEMP_INFO(dst) && i <= NTEMP_INFO(src); i++ ) {
        TNUM_TEMP_INFO(dst,i) &= TNUM_TEMP_INFO(src,i);
    }
}

Int             JSON_IsEqInfoCVars (
    Bag                 dst,
    Bag                 src )
{
    Int                 i;
    if ( SIZE_BAG(dst) < SIZE_BAG(src) )  ResizeBag( dst, SIZE_BAG(src) );
    if ( SIZE_BAG(src) < SIZE_BAG(dst) )  ResizeBag( src, SIZE_BAG(dst) );
    for ( i = 1; i <= NLVAR_INFO(src); i++ ) {
        if ( TNUM_LVAR_INFO(dst,i) != TNUM_LVAR_INFO(src,i) ) {
            return 0;
        }
    }
    for ( i = 1; i <= NTEMP_INFO(dst) && i <= NTEMP_INFO(src); i++ ) {
        if ( TNUM_TEMP_INFO(dst,i) != TNUM_TEMP_INFO(src,i) ) {
            return 0;
        }
    }
    return 1;
}


/****************************************************************************
**
*F  JSON_NewTemp( <name> ) . . . . . . . . . . . . . . .  allocate a new temporary
*F  JSON_FreeTemp( <temp> )  . . . . . . . . . . . . . . . . . .  free a temporary
**
**  'JSON_NewTemp' allocates  a  new  temporary   variable (<name>  is   currently
**  ignored).
**
**  'JSON_FreeTemp' frees the temporary <temp>.
**
**  Currently  allocations and deallocations   of  temporaries are done  in a
**  strict nested (laff -- last allocated, first freed) order.  This means we
**  do not have to search for unused temporaries.
*/
typedef UInt4           Temp;

Temp            JSON_NewTemp (
    const Char *        name )
{
    Temp                temp;           /* new temporary, result           */
    Bag                 info;           /* information bag                 */

    /* get the information bag                                             */
    info = INFO_FEXP( CURR_FUNC );

    /* take the next available temporary                                   */
    CTEMP_INFO( info )++;
    temp = CTEMP_INFO( info );

    /* maybe make room for more temporaries                                */
    if ( NTEMP_INFO( info ) < temp ) {
        if ( SIZE_BAG(info) < SIZE_INFO( NLVAR_INFO(info), temp ) ) {
            ResizeBag( info, SIZE_INFO( NLVAR_INFO(info), temp+7 ) );
        }
        NTEMP_INFO( info ) = temp;
    }
    TNUM_TEMP_INFO( info, temp ) = W_UNKNOWN;

    /* return the temporary                                                */
    return temp;
}

void            JSON_FreeTemp (
    Temp                temp )
{
    Bag                 info;           /* information bag                 */

    /* get the information bag                                             */
    info = INFO_FEXP( CURR_FUNC );

    /* check that deallocations happens in the correct order               */
    if ( temp != CTEMP_INFO( info ) && JSON_CompPass == 2 ) {
        //Pr("PROBLEM: freeing t_%d, should be t_%d\n",(Int)temp,CTEMP_INFO(info));
    }

    /* free the temporary                                                  */
    TNUM_TEMP_INFO( info, temp ) = W_UNUSED;
    CTEMP_INFO( info )--;
}


/****************************************************************************
**
*F  JSON_CompSetUseHVar( <hvar> )  . . . . . . . . register use of higher variable
*F  JSON_CompGetUseHVar( <hvar> )  . . . . . . . . get use mode of higher variable
*F  JSON_GetLevlHVar( <hvar> ) . . . . . . . . . . .  get level of higher variable
*F  JSON_GetIndxHVar( <hvar> ) . . . . . . . . . . .  get index of higher variable
**
**  'JSON_CompSetUseHVar'  register (during pass 1)   that the variable <hvar>  is
**  used  as   higher  variable, i.e.,  is  referenced   from inside  a local
**  function.  Such variables  must be allocated  in  a stack frame  bag (and
**  cannot be mapped to C variables).
**
**  'JSON_CompGetUseHVar' returns nonzero if the variable <hvar> is used as higher
**  variable.
**
**  'JSON_GetLevlHVar' returns the level of the  higher variable <hvar>, i.e., the
**  number of  frames  that must be  walked upwards   for the  one containing
**  <hvar>.  This may be properly  smaller than 'LEVEL_HVAR(<hvar>)', because
**  only those compiled functions that have local variables  that are used as
**  higher variables allocate a stack frame.
**
**  'JSON_GetIndxHVar' returns the index of the higher  variable <hvar>, i.e., the
**  position of <hvar> in the stack frame.  This may be properly smaller than
**  'INDEX_HVAR(<hvar>)', because only those  local variable that are used as
**  higher variables are allocated in a stack frame.
*/
typedef UInt4           HVar;

void            JSON_CompSetUseHVar (
    HVar                hvar )
{
    Bag                 info;           /* its info bag                    */
    Int                 i;              /* loop variable                   */

    /* only mark in pass 1                                                 */
    if ( JSON_CompPass != 1 )  return;

    /* walk up                                                             */
    info = INFO_FEXP( CURR_FUNC );
    for ( i = 1; i <= (hvar >> 16); i++ ) {
        info = NEXT_INFO( info );
    }

    /* set mark                                                            */
    if ( TNUM_LVAR_INFO( info, (hvar & 0xFFFF) ) != W_HIGHER ) {
        TNUM_LVAR_INFO( info, (hvar & 0xFFFF) ) = W_HIGHER;
        NHVAR_INFO(info) = NHVAR_INFO(info) + 1;
    }

}

Int             JSON_CompGetUseHVar (
    HVar                hvar )
{
    Bag                 info;           /* its info bag                    */
    Int                 i;              /* loop variable                   */

    /* walk up                                                             */
    info = INFO_FEXP( CURR_FUNC );
    for ( i = 1; i <= (hvar >> 16); i++ ) {
        info = NEXT_INFO( info );
    }

    /* get mark                                                            */
    return (TNUM_LVAR_INFO( info, (hvar & 0xFFFF) ) == W_HIGHER);
}

UInt            JSON_GetLevlHVar (
    HVar                hvar )
{
    UInt                levl;           /* level of higher variable        */
    Bag                 info;           /* its info bag                    */
    Int                 i;              /* loop variable                   */

    /* walk up                                                             */
    levl = 0;
    info = INFO_FEXP( CURR_FUNC );
#if 0
    if ( NHVAR_INFO(info) != 0 ) 
#endif
      levl++;
    for ( i = 1; i <= (hvar >> 16); i++ ) {
        info = NEXT_INFO( info );
#if 0
        if ( NHVAR_INFO(info) != 0 ) 
#endif
          levl++;
    }

    /* return level (the number steps to go up)                            */
    return levl - 1;
}

UInt            JSON_GetIndxHVar (
    HVar                hvar )
{
    UInt                indx;           /* index of higher variable        */
    Bag                 info;           /* its info bag                    */
    Int                 i;              /* loop variable                   */

    /* walk up                                                             */
    info = INFO_FEXP( CURR_FUNC );
    for ( i = 1; i <= (hvar >> 16); i++ ) {
        info = NEXT_INFO( info );
    }

    /* walk right                                                          */
    indx = 0;
    for ( i = 1; i <= (hvar & 0xFFFF); i++ ) {
        if ( TNUM_LVAR_INFO( info, i ) == W_HIGHER )  indx++;
    }

    /* return the index                                                    */
    return indx;
}


/****************************************************************************
**
*F  JSON_CompSetUseGVar( <gvar>, <mode> )  . . . . register use of global variable
*F  JSON_CompGetUseGVar( <gvar> )  . . . . . . . . get use mode of global variable
**
**  'JSON_CompSetUseGVar' registers (during pass 1) the use of the global variable
**  with identifier <gvar>.
**
**  'JSON_CompGetUseGVar'  returns the bitwise OR  of all the <mode> arguments for
**  the global variable with identifier <gvar>.
**
**  Currently the interpretation of the <mode> argument is as follows
**
**  If '<mode> &  COMP_USE_GVAR_ID' is nonzero, then  the produced code shall
**  define  and initialize 'G_<name>'    with  the identifier of  the  global
**  variable (which may  be different from  <gvar>  by the time the  compiled
**  code is actually run).
**
**  If '<mode> & COMP_USE_GVAR_COPY' is nonzero, then the produced code shall
**  define  and initialize 'GC_<name>' as a  copy of  the global variable
**  (see 'InitCopyGVar' in 'gvars.h').
**
**  If '<mode> & COMP_USE_GVAR_FOPY' is nonzero, then the produced code shall
**  define and  initialize  'GF_<name>' as   a  function copy  of the  global
**  variable (see 'InitFopyGVar' in 'gvars.h').
*/
typedef UInt    GVar;

#define COMP_USE_GVAR_ID        (1L << 0)
#define COMP_USE_GVAR_COPY      (1L << 1)
#define COMP_USE_GVAR_FOPY      (1L << 2)

Bag             JSON_CompInfoGVar;

void            JSON_CompSetUseGVar (
    GVar                gvar,
    UInt                mode )
{
    /* only mark in pass 1                                                 */
    if ( JSON_CompPass != 1 )  return;

    /* resize if neccessary                                                */
    if ( SIZE_OBJ(JSON_CompInfoGVar)/sizeof(UInt) <= gvar ) {
        ResizeBag( JSON_CompInfoGVar, sizeof(UInt)*(gvar+1) );
    }

    /* or with <mode>                                                      */
    ((UInt*)PTR_BAG(JSON_CompInfoGVar))[gvar] |= mode;
}

UInt            JSON_CompGetUseGVar (
    GVar                gvar )
{
    return ((UInt*)PTR_BAG(JSON_CompInfoGVar))[gvar];
}

void            JSON_Emit (
    const char *        fmt,
    ... );

/****************************************************************************
**
*F  JSON_CompSetUseRNam( <rnam>, <mode> )  . . . . . . register use of record name
*F  JSON_CompGetUseRNam( <rnam> )  . . . . . . . . . . get use mode of record name
**
**  'JSON_CompSetUseRNam' registers  (during pass  1) the use   of the record name
**  with identifier <rnam>.  'JSON_CompGetUseRNam'  returns the bitwise OR  of all
**  the <mode> arguments for the global variable with identifier <rnam>.
**
**  Currently the interpretation of the <mode> argument is as follows
**
**  If '<mode> & COMP_USE_RNAM_ID'  is nonzero, then  the produced code shall
**  define and initialize  'R_<name>' with the  identifier of the record name
**  (which may be  different from <rnam> when the  time the  compiled code is
**  actually run).
*/
typedef UInt    RNam;

#define COMP_USE_RNAM_ID        (1L << 0)

Bag             JSON_CompInfoRNam;

void            JSON_CompSetUseRNam (
    RNam                rnam,
    UInt                mode )
{
//    JSON_Emit( "\"(JSON_CompSetUseRNam) RNAME_SET: %s\"", NAME_RNAM(rnam) );

    /* only mark in pass 1                                                 */
    if ( JSON_CompPass != 1 )  return;

    /* resize if neccessary                                                */
    if ( SIZE_OBJ(JSON_CompInfoRNam)/sizeof(UInt) <= rnam ) {
        ResizeBag( JSON_CompInfoRNam, sizeof(UInt)*(rnam+1) );
    }

    /* or with <mode>                                                      */
    ((UInt*)PTR_BAG(JSON_CompInfoRNam))[rnam] |= mode;

}

UInt            JSON_CompGetUseRNam (
    RNam                rnam )
{
    return ((UInt*)PTR_BAG(JSON_CompInfoRNam))[rnam];
}


/****************************************************************************
**
*F  JSON_Emit( <fmt>, ... )  . . . . . . . . . . . . . . . . . . . . . . emit code
**
**  'JSON_Emit' outputs the   string  <fmt> and the  other  arguments,  which must
**  correspond  to the '%'  format elements  in  <fmt>.  Nothing  is actually
**  outputted if 'JSON_CompPass' is not 2.
**
**  'JSON_Emit'   supports the following   '%'  format elements:  '%d' formats  an
**  integer,   '%s' formats a  string,  '%S' formats a    string with all the
**  necessary escapes, %C does the same  but uses only  valid C escapes, '%n'
**  formats a  name   ('_' is  converted   to '__',  special  characters  are
**  converted to     '_<hex1><hex2>'),    '%c'  formats     a  C     variable
**  ('INTOBJ_INT(<int>)'  for integers,  'a_<name>' for arguments, 'l_<name>'
**  for locals, 't_<nr>' for temporaries), and '%%' outputs a single '%'.
*/
char* preProcessControlSequences(char* str, int len) {
  Bag bag;
  bag = NewBag(T_STRING, sizeof(char) * len * 4 + 1);

  char* newStr = (char*) PTR_BAG(bag);//malloc(sizeof(char) * len * 4 + 1); //+ 1 for NULL
  
  int i = 0;
  char* p, *j;
  for(p = str, j = newStr; i < len; ++i, ++p, ++j) {
    if(!isprint(*p)) {
       sprintf(j, "\\%03u", *p); //TODO: maybe use snprintf?
       j += 3;
    } else {
        *j = *p;
    }
  }
  *j = '\0';

  return newStr;
}


char* escapeWhitespace(char* str) {
    //allocate new output string
    Bag bag;
    bag = NewBag(T_STRING, sizeof(char) * strlen(str) * 5);

    char* newStr = (char*) PTR_BAG(bag);

    //  char* newStr = (char*) malloc(sizeof(char) * strlen(str) * 5); //TODO: set this back to 2 after redundant \ is removed
        
    char* p, *j;
    for(p = str, j = newStr; (*p) != '\0'; ++j, ++p) {
        if(isspace(*p)) {
            if(*p == ' ') {
              *j = *p;
            } else if(*p == '\n') {
                *j = '\\';
                ++j; 
                *j = 'n';
            } else if(*p == '\t') {
                *j = '\\';
                ++j;
                *j = 't';
            } else if(*p == '\r') {
                *j = '\\';
                ++j;
                *j = 'r';
            } else {
              fprintf(stderr, "ERROR: unknown escape sequence in " 
                              "compiler.c, escapeWhitespace: %d\n", *p);
            }
        } else if(*p == '\b') {
                *j = '\\';
                ++j;
                *j = 'b';
        } else if(*p == '\\') {
            *j = '\\';
            ++j;
            *j = '\\';
        } else if(*p == '\"') {
            *j = '\\';
            ++j;
            *j = '\"';
        } else if(*p == '\01') {
            *j = '\\';
            ++j;
            *j = '\\';
            ++j;
            *j = '>';
        } else if(*p == '\02') {
            *j = '\\';
            ++j;
            *j = '\\'; //TODO: this is one too much but otherwise most prettifiers crash
            ++j;
            *j = '<';
        } else if(*p == '\03') {
            *j = '\\';
            ++j;
            *j = '\\';
            ++j;
            *j = 'c';
        } else {
            *j = *p;
        }
    }

    *j = '\0';
    return newStr;
}

void            JSON_Emit (
    const char *        fmt,
    ... )
{
    Int                 narg;           /* number of arguments             */
    va_list             ap;             /* argument list pointer           */

    /* are we in pass 2?                                                   */
    if ( JSON_CompPass != 2 )  return;

    /* get the information bag                                             */
    narg = NARG_FUNC( CURR_FUNC );
    if (narg < 0) {
        narg = -narg;
    }

    va_start( ap, fmt );

    /* preformat strings / escape whitespace */
    
    for(const char *p = fmt; *p != '\0'; ++p) {
        if(*p == '%') {
            ++p;
            if(*p == 's') { //found string
                char* str = va_arg(ap, char*);
                str = escapeWhitespace(str);
                fprintf(json, "%s", str);
            } else if(*p == 'd') { //found integer
                int i = va_arg(ap, int);
                fprintf(json, "%d", i);
            } else if(*p == 'S') { //found string with prepended size
                char* str = va_arg(ap, char*);
                int len = (int) *(UInt*)str;
                str += sizeof(UInt);
                char* preprocessedStr = preProcessControlSequences(str, len);
                str = escapeWhitespace(preprocessedStr);
                fprintf(json, "%s", str);
            } else {
                fprintf(stderr, "ERROR: Unexpected string in compiler.c, JSON_Emit\n");
            }
        } else { //just print character
            fputc(*p, json);
        }
    }

    va_end( ap );

}


/****************************************************************************
**

*F * * * * * * * * * * * * * * compile checks * * * * * * * * * * * * * * * *
*/


/****************************************************************************
**


*F  JSON_CompCheckBound( <obj>, <name> ) emit code to check that <obj> has a value
*/
void JSON_CompCheckBound (
    CVar                obj,
    Char *              name )
{
  //these are semantics not syntax -> irrelevant for JSON
}


/****************************************************************************
**
*F  JSON_CompCheckFuncResult( <obj> )  . emit code to check that <obj> has a value
*/
void JSON_CompCheckFuncResult (
    CVar                obj )
{
}


/****************************************************************************
**
*F  JSON_CompCheckIntSmall( <obj> )   emit code to check that <obj> is a small int
*/
void JSON_CompCheckIntSmall (
    CVar                obj )
{
}



/****************************************************************************
**
*F  JSON_CompCheckIntSmallPos( <obj> ) emit code to check that <obj> is a position
*/
void JSON_CompCheckIntSmallPos (
    CVar                obj )
{
}

/****************************************************************************
**
*F  JSON_CompCheckIntPos( <obj> ) emit code to check that <obj> is a position
*/
void JSON_CompCheckIntPos (
    CVar                obj )
{
}


/****************************************************************************
**
*F  JSON_CompCheckBool( <obj> )  . . .  emit code to check that <obj> is a boolean
*/
void JSON_CompCheckBool (
    CVar                obj )
{
}



/****************************************************************************
**
*F  JSON_CompCheckFunc( <obj> )  . . . emit code to check that <obj> is a function
*/
void JSON_CompCheckFunc (
    CVar                obj )
{
}


/****************************************************************************
**

*F * * * * * * * * * * * *  compile expressions * * * * * * * * * * * * * * *
*/


/****************************************************************************
**

*F  JSON_CompExpr( <expr> )  . . . . . . . . . . . . . . . . compile an expression
**
**  'JSON_CompExpr' compiles the expression <expr> and returns the C variable that
**  will contain the result.
*/
CVar (* JSON_CompExprFuncs[256]) ( Expr expr );


CVar JSON_CompExpr (
    Expr                expr )
{
    return (* JSON_CompExprFuncs[ TNUM_EXPR(expr) ])( expr );
}


/****************************************************************************
**
*F  JSON_CompUnknownExpr( <expr> ) . . . . . . . . . . . .  log unknown expression
*/
CVar JSON_CompUnknownExpr (
    Expr                expr )
{
    //JSON_Emit( "CANNOT COMPILE EXPRESSION OF TNUM %d;\n", TNUM_EXPR(expr) );
    JSON_Emit("\"Unknown expression\"");
    return 0;
}



/****************************************************************************
**
*F  JSON_CompBoolExpr( <expr> )  . . . . . . . compile bool expr and return C bool
*/
CVar (* JSON_CompBoolExprFuncs[256]) ( Expr expr );

CVar JSON_CompBoolExpr (
    Expr                expr )
{
    return (* JSON_CompBoolExprFuncs[ TNUM_EXPR(expr) ])( expr );
}


/****************************************************************************
**
*F  JSON_CompUnknownBool( <expr> ) . . . . . . . . . .  use 'JSON_CompExpr' and convert
*/
CVar JSON_CompUnknownBool (
    Expr                expr )
{
    CVar                res;            /* result                          */
    CVar                val;            /* value of expression             */

    /* allocate a new temporary for the result                             */
    res = CVAR_TEMP( JSON_NewTemp( "res" ) );

    /* compile the expression and check that the value is boolean          */
    val = JSON_CompExpr( expr );
    JSON_CompCheckBool( val );
    
    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( res, W_BOOL );

    /* free the temporary                                                  */
    if ( IS_TEMP_CVAR( val ) )  JSON_FreeTemp( TEMP_CVAR( val ) );

    /* return the result                                                   */
    return res;
}
    
/****************************************************************************
**
*V  G_Length  . . . . . . . . . . . . . . . . . . . . . . . function 'Length'
*/
GVar G_Length;



/****************************************************************************
**
*F  JSON_CompFunccall0to6Args( <expr> )  . . . T_FUNCCALL_0ARGS...T_FUNCCALL_6ARGS
*/
extern CVar JSON_CompRefGVarFopy (
            Expr                expr );


CVar JSON_CompFunccall0to6Args (
    Expr                expr )
{
    CVar                result;         /* result, result                  */
    CVar                func;           /* function                        */
    CVar                args [8];       /* arguments                       */
    Int                 narg;           /* number of arguments             */
    Int                 i;              /* loop variable                   */

    JSON_Emit("{ \"type\":\"functionCall\", \"name\":"); 

    /* allocate a temporary for the result                                 */
    result = CVAR_TEMP( JSON_NewTemp( "result" ) );

    /* compile the reference to the function                               */
    if ( TNUM_EXPR( FUNC_CALL(expr) ) == T_REF_GVAR ) {
        func = JSON_CompRefGVarFopy( FUNC_CALL(expr) );
    }
    else {
        func = JSON_CompExpr( FUNC_CALL(expr) );
    }

    JSON_Emit(", \"args\":[");

    /* compile the argument expressions                                    */
    narg = NARG_SIZE_CALL(SIZE_EXPR(expr));
    for ( i = 1; i <= narg; i++ ) {
        args[i] = JSON_CompExpr( ARGI_CALL(expr,i) );
        if( i < narg) {
          JSON_Emit(", ");
        }
    }

    /* emit the code for the procedure call                                */
    //JSON_Emit( "%c = CALL_%dARGS( %c", result, narg, func );
    for ( i = 1; i <= narg; i++ ) {
        //JSON_Emit( ", %c", args[i] );
    }

    /* free the temporaries                                                */
    for ( i = narg; 1 <= i; i-- ) {
        if ( IS_TEMP_CVAR( args[i] ) )  JSON_FreeTemp( TEMP_CVAR( args[i] ) );
    }
    if ( IS_TEMP_CVAR( func ) )  JSON_FreeTemp( TEMP_CVAR( func ) );

    JSON_Emit("] }");

    /* return the result                                                   */
    return result;
}


/****************************************************************************
**
*F  JSON_CompFunccallXArgs( <expr> ) . . . . . . . . . . . . . .  T_FUNCCALL_XARGS
*/
CVar JSON_CompFunccallXArgs (
    Expr                expr )
{
    CVar                result;         /* result, result                  */
    CVar                func;           /* function                        */
    CVar                argl;           /* argument list                   */
    CVar                argi;           /* <i>-th argument                 */
    UInt                narg;           /* number of arguments             */
    UInt                i;              /* loop variable                   */

    JSON_Emit("{\"type\":\"FunccallXArgs\", \"function\":");
    /* allocate a temporary for the result                                 */
    result = CVAR_TEMP( JSON_NewTemp( "result" ) );

    /* compile the reference to the function                               */
    if ( TNUM_EXPR( FUNC_CALL(expr) ) == T_REF_GVAR ) {
        func = JSON_CompRefGVarFopy( FUNC_CALL(expr) );
    }
    else {
        func = JSON_CompExpr( FUNC_CALL(expr) );
        JSON_CompCheckFunc( func );
    }

    /* compile the argument expressions                                    */
    narg = NARG_SIZE_CALL(SIZE_EXPR(expr));
    argl = CVAR_TEMP( JSON_NewTemp( "argl" ) );
    //JSON_Emit( "%c = NEW_PLIST( T_PLIST, %d );\n", argl, narg );
    //JSON_Emit( "SET_LEN_PLIST( %c, %d );\n", argl, narg );
  
    JSON_Emit(", \"args\":[");

    for ( i = 1; i <= narg; i++ ) {
        argi = JSON_CompExpr( ARGI_CALL( expr, i ) );
        if(i < narg) { JSON_Emit(", ");}
        if ( IS_TEMP_CVAR( argi ) )  JSON_FreeTemp( TEMP_CVAR( argi ) );
    }

    /* emit the code for the procedure call                                */
    //JSON_Emit( "%c = CALL_XARGS( %c, %c );\n", result, func, argl );

    /* emit code for the check (sets the information for the result)       */
    JSON_CompCheckFuncResult( result );

    JSON_Emit("]}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( argl ) )  JSON_FreeTemp( TEMP_CVAR( argl ) );
    if ( IS_TEMP_CVAR( func ) )  JSON_FreeTemp( TEMP_CVAR( func ) );

    /* return the result                                                   */
    return result;
}

/****************************************************************************
**
*F  JSON_CompFunccallOpts( <expr> ) . . . . . . . . . . . . . .  T_FUNCCALL_OPTS
*/
CVar JSON_CompFunccallOpts(
                      Expr expr)
{
  JSON_Emit("{ \"type\":\"FunccallOpts\", \"opts\":");
  CVar opts = JSON_CompExpr(ADDR_STAT(expr)[0]);
  JSON_Emit(", \"result\":");
  GVar pushOptions;
  GVar popOptions;
  CVar result;
  pushOptions = GVarName("PushOptions");
  popOptions = GVarName("PopOptions");
  JSON_CompSetUseGVar(pushOptions, COMP_USE_GVAR_FOPY);
  JSON_CompSetUseGVar(popOptions, COMP_USE_GVAR_FOPY);
  //JSON_Emit("CALL_1ARGS( GF_PushOptions, %c );\n", opts);
  if (IS_TEMP_CVAR( opts) ) JSON_FreeTemp( TEMP_CVAR( opts ));
  result = JSON_CompExpr(ADDR_STAT(expr)[1]);
  JSON_Emit("}");
  //JSON_Emit("CALL_0ARGS( GF_PopOptions );\n");
  return result;
}
     
void JSON_CompFunc (
    Obj                 func );

/****************************************************************************
**
*F  JSON_CompFuncExpr( <expr> )  . . . . . . . . . . . . . . . . . . . T_FUNC_EXPR
*/
CVar JSON_CompFuncExpr (
    Expr                expr )
{
    CVar                func;           /* function, result                */
    CVar                tmp;            /* dummy body                      */

    Obj                 fexs;           /* function expressions list       */
    Obj                 fexp;           /* function expression             */

    /* get the number of the function                                      */
    fexs = FEXS_FUNC( CURR_FUNC );
    fexp = ELM_PLIST( fexs, ((Int*)ADDR_EXPR(expr))[0] );

    JSON_CompFunc( fexp );

    /* allocate a new temporary for the function                           */
    func = CVAR_TEMP( JSON_NewTemp( "func" ) );

    /* make the function (all the pieces are in global variables)          */
    //JSON_Emit( "%c = NewFunction( NameFunc[%d], NargFunc[%d], NamsFunc[%d]",
    //      func, nr, nr, nr );
    //JSON_Emit( ", HdlrFunc%d );\n", nr );

    /* this should probably be done by 'NewFunction'                       */
    //JSON_Emit( "ENVI_FUNC( %c ) = TLS(CurrLVars);\n", func );
    tmp = CVAR_TEMP( JSON_NewTemp( "body" ) );
    
    JSON_FreeTemp( TEMP_CVAR( tmp ) );

    /* we know that the result is a function                               */
    JSON_SetInfoCVar( func, W_FUNC );

    /* return the number of the C variable that will hold the function     */
    return func;
}


/****************************************************************************
**
*F  JSON_CompOr( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_OR
*/
CVar JSON_CompOr (
    Expr                expr )
{
    CVar                val;            /* or, result                      */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */
    Bag                 only_left;      /* info after evaluating only left */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"or\", ");
    JSON_Emit("\"left\":");

    /* compile the left expression                                         */
    left = JSON_CompBoolExpr( ADDR_EXPR(expr)[0] );
    
    only_left = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( only_left, INFO_FEXP(CURR_FUNC) );

    JSON_Emit(", \"right\":");

    /* compile the right expression                                        */
    right = JSON_CompBoolExpr( ADDR_EXPR(expr)[1] );
    //JSON_Emit( "%c = (%c ? True : False);\n", val, right );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean                                  */
    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), only_left );
    JSON_SetInfoCVar( val, W_BOOL );

    JSON_Emit("}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompOrBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_OR
*/
CVar JSON_CompOrBool (
    Expr                expr )
{
    CVar                val;            /* or, result                      */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */
    Bag                 only_left;      /* info after evaluating only left */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );
    
    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"orBool\", ");
    JSON_Emit("\"left\":");

    /* compile the left expression                                         */
    left = JSON_CompBoolExpr( ADDR_EXPR(expr)[0] );
    //JSON_Emit( "%c = %c;\n", val, left );
    //JSON_Emit( "if ( ! %c ) {\n", val );
    only_left = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( only_left, INFO_FEXP(CURR_FUNC) );

    JSON_Emit(", \"right\":");

    /* compile the right expression                                        */
    right = JSON_CompBoolExpr( ADDR_EXPR(expr)[1] );
    //JSON_Emit( "%c = %c;\n", val, right );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), only_left );
    JSON_SetInfoCVar( val, W_BOOL );

    JSON_Emit("}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompAnd( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_AND
*/
CVar JSON_CompAnd (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right1;         /* right operand 1                 */
    CVar                right2;         /* right operand 2                 */
    Bag                 only_left;      /* info after evaluating only left */

    right2 = 0;

    /* allocate a temporary for the result                                 */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"and\", ");
    JSON_Emit("\"left\":");

    /* compile the left expression                                         */
    left = JSON_CompExpr( ADDR_EXPR(expr)[0] );
    only_left = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( only_left, INFO_FEXP(CURR_FUNC) );

    JSON_Emit(", \"right\":");

    /* emit the code for the case that the left value is 'false'           */
    //JSON_Emit( "if ( %c == False ) {\n", left );
    //JSON_Emit( "%c = %c;\n", val, left );
    //JSON_Emit( "}\n" );

    /* emit the code for the case that the left value is 'true'            */
    //JSON_Emit( "else if ( %c == True ) {\n", left );
    right1 = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    JSON_CompCheckBool( right1 );
    //JSON_Emit( "%c = %c;\n", val, right1 );
    //JSON_Emit( "}\n" );

    /* emit the code for the case that the left value is a filter          */
    //JSON_Emit( "else {\n" );
    //right2 = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    //JSON_Emit( "%c = NewAndFilter( %c, %c );\n", val, left, right2 );
    //JSON_Emit( "}\n" );

    /* we know precious little about the result                            */
    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), only_left );
    JSON_SetInfoCVar( val, W_BOUND );

    JSON_Emit("}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right2 ) )  JSON_FreeTemp( TEMP_CVAR( right2 ) );
    if ( IS_TEMP_CVAR( right1 ) )  JSON_FreeTemp( TEMP_CVAR( right1 ) );
    if ( IS_TEMP_CVAR( left   ) )  JSON_FreeTemp( TEMP_CVAR( left   ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompAndBool( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . T_AND
*/
CVar JSON_CompAndBool (
    Expr                expr )
{
    CVar                val;            /* or, result                      */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */
    Bag                 only_left;      /* info after evaluating only left */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"andBool\", ");
    JSON_Emit("\"left\":");

    /* compile the left expression                                         */
    left = JSON_CompBoolExpr( ADDR_EXPR(expr)[0] );
    //JSON_Emit( "%c = %c;\n", val, left );
    //JSON_Emit( "if ( %c ) {\n", val );
    only_left = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( only_left, INFO_FEXP(CURR_FUNC) );

    JSON_Emit(", \"right\":");

    /* compile the right expression                                        */
    right = JSON_CompBoolExpr( ADDR_EXPR(expr)[1] );
    //JSON_Emit( "%c = %c;\n", val, right );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), only_left );
    JSON_SetInfoCVar( val, W_BOOL );

    JSON_Emit("}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompNot( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_NOT
*/
CVar JSON_CompNot (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* operand                         */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"not\", ");
    JSON_Emit("\"left\":");

    /* compile the operand                                                 */
    left = JSON_CompBoolExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit("}");

    /* invert the operand                                                  */
    //JSON_Emit( "%c = (%c ? False : True);\n", val, left );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( left ) )  JSON_FreeTemp( TEMP_CVAR( left ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompNotBoot( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . T_NOT
*/
CVar JSON_CompNotBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* operand                         */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"notBool\", ");
    JSON_Emit("\"left\":");

    /* compile the operand                                                 */
    left = JSON_CompBoolExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit("}");

    /* invert the operand                                                  */
    //JSON_Emit( "%c = (Obj)(UInt)( ! ((Int)%c) );\n", val, left );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( left ) )  JSON_FreeTemp( TEMP_CVAR( left ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompEq( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_EQ
*/
CVar JSON_CompEq (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"Eq\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
//JSON_Emit("%c = ((((Int)%c) == ((Int)%c)) ? True : False);\n", val, left, right);
    }
    else {
        //JSON_Emit( "%c = (EQ( %c, %c ) ? True : False);\n", val, left, right );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompEqBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_EQ
*/
CVar JSON_CompEqBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"EqBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) == ((Int)%c));\n", val, left, right);
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)(EQ( %c, %c ));\n", val, left, right );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompNe( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_NE
*/
CVar JSON_CompNe (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"notEqual\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
//JSON_Emit("%c = ((((Int)%c) == ((Int)%c)) ? False : True);\n", val, left, right);
    }
    else {
        //JSON_Emit( "%c = (EQ( %c, %c ) ? False : True);\n", val, left, right );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompNeBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_NE
*/
CVar JSON_CompNeBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"notEqualBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) != ((Int)%c));\n", val, left, right );
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)( ! EQ( %c, %c ));\n", val, left, right );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompLt( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_LT
*/
CVar JSON_CompLt (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"lessThan\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
//JSON_Emit( "%c = ((((Int)%c) < ((Int)%c)) ? True : False);\n", val, left, right );
    }
    else {
        //JSON_Emit( "%c = (LT( %c, %c ) ? True : False);\n", val, left, right );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompLtBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_LT
*/
CVar JSON_CompLtBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"lessThanBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");
   
    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) < ((Int)%c));\n", val, left, right );
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)(LT( %c, %c ));\n", val, left, right );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompGe( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_GE
*/
CVar JSON_CompGe (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"greaterEqual\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
 //JSON_Emit("%c = ((((Int)%c) < ((Int)%c)) ? False : True);\n", val, left, right);
    }
    else {
        //JSON_Emit( "%c = (LT( %c, %c ) ? False : True);\n", val, left, right );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompGeBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_GE
*/
CVar JSON_CompGeBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"greaterEqualBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) >= ((Int)%c));\n", val, left, right );
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)(! LT( %c, %c ));\n", val, left, right );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompGt( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_GT
*/
CVar JSON_CompGt (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"greaterThan\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");
   
    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
 //JSON_Emit("%c = ((((Int)%c) < ((Int)%c)) ? True : False);\n", val, right, left);
    }
    else {
        //JSON_Emit( "%c = (LT( %c, %c ) ? True : False);\n", val, right, left );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompGtBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_GT
*/
CVar JSON_CompGtBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"greaterThanBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");
   
    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) < ((Int)%c));\n", val, right, left );
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)(LT( %c, %c ));\n", val, right, left );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompLe( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_LE
*/
CVar JSON_CompLe (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"lessEqual\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");
   
    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
//JSON_Emit("%c = ((((Int)%c) < ((Int)%c)) ?  False : True);\n", val, right, left);
    }
    else {
        //JSON_Emit( "%c = (LT( %c, %c ) ?  False : True);\n", val, right, left );
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompLeBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_LE
*/
CVar            JSON_CompLeBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"lessEqualBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "%c = (Obj)(UInt)(((Int)%c) >= ((Int)%c));\n", val, right, left );
    }
    else {
        //JSON_Emit( "%c = (Obj)(UInt)(! LT( %c, %c ));\n", val, right, left );
    }

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompIn( <expr> )  . . . . . . . . . . . . . . . . . . . . . . . . .  T_IN
*/
CVar JSON_CompIn (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"in\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "%c = (IN( %c, %c ) ?  True : False);\n", val, left, right );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompInBool( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_IN
*/
CVar JSON_CompInBool (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"BoolExpr\", \"subtype\":\"inBool\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "%c = (Obj)(UInt)(IN( %c, %c ));\n", val, left, right );

    /* we know that the result is boolean (should be 'W_CBOOL')            */
    JSON_SetInfoCVar( val, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompSum( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_SUM
*/
CVar JSON_CompSum (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"sum\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "C_SUM_INTOBJS( %c, %c, %c )\n", val, left, right );
    }
    else if ( JSON_CompFastIntArith ) {
        //JSON_Emit( "C_SUM_FIA( %c, %c, %c )\n", val, left, right );
    }
    else {
        //JSON_Emit( "C_SUM( %c, %c, %c )\n", val, left, right );
    }

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) && JSON_HasInfoCVar(right,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompAInv( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_AINV
*/
CVar JSON_CompAInv (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"aInv\", ");
    JSON_Emit("\"left\":");

    /* compile the operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) ) {
        //JSON_Emit( "C_AINV_INTOBJS( %c, %c )\n", val, left );
    }
    else if ( JSON_CompFastIntArith ) {
        //JSON_Emit( "C_AINV_FIA( %c, %c )\n", val, left );
    }
    else {
        //JSON_Emit( "C_AINV( %c, %c )\n", val, left );
    }

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompDiff( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_DIFF
*/
CVar JSON_CompDiff (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"diff\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "C_DIFF_INTOBJS( %c, %c, %c )\n", val, left, right );
    }
    else if ( JSON_CompFastIntArith ) {
        //JSON_Emit( "C_DIFF_FIA( %c, %c, %c )\n", val, left, right );
    }
    else {
        //JSON_Emit( "C_DIFF( %c, %c, %c )\n", val, left, right );
    }

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) && JSON_HasInfoCVar(right,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompProd( <expr> )  . . . . . . . . . . . . . . . . . . . . . . .  T_PROD
*/
CVar JSON_CompProd (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"prod\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    if ( JSON_HasInfoCVar(left,W_INT_SMALL) && JSON_HasInfoCVar(right,W_INT_SMALL) ) {
        //JSON_Emit( "C_PROD_INTOBJS( %c, %c, %c )\n", val, left, right );
    }
    else if ( JSON_CompFastIntArith ) {
        //JSON_Emit( "C_PROD_FIA( %c, %c, %c )\n", val, left, right );
    }
    else {
        //JSON_Emit( "C_PROD( %c, %c, %c )\n", val, left, right );
    }

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) && JSON_HasInfoCVar(right,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompInv( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_INV
**
** C_INV is not defined, so I guess this never gets called SL
**
*/
CVar JSON_CompInv (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"inv\", ");
    JSON_Emit("\"left\":");

    /* compile the operands                                                */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "C_INV( %c, %c )\n", val, left );

    /* set the information for the result                                  */
    JSON_SetInfoCVar( val, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompQuo( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_QUO
*/
CVar JSON_CompQuo (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"quo\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "%c = QUO( %c, %c );\n", val, left, right );

    /* set the information for the result                                  */
    JSON_SetInfoCVar( val, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompMod( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_MOD
*/
CVar JSON_CompMod (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"mod\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "%c = MOD( %c, %c );\n", val, left, right );

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) && JSON_HasInfoCVar(right,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompPow( <expr> ) . . . . . . . . . . . . . . . . . . . . . . . . . T_POW
*/
CVar JSON_CompPow (
    Expr                expr )
{
    CVar                val;            /* result                          */
    CVar                left;           /* left operand                    */
    CVar                right;          /* right operand                   */

    /* allocate a new temporary for the result                             */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"arithmetic\", \"subtype\":\"pow\", ");
    JSON_Emit("\"left\":");

    /* compile the two operands                                            */
    left  = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"right\":");
    right = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code                                                       */
    //JSON_Emit( "%c = POW( %c, %c );\n", val, left, right );

    /* set the information for the result                                  */
    if ( JSON_HasInfoCVar(left,W_INT) && JSON_HasInfoCVar(right,W_INT) ) {
        JSON_SetInfoCVar( val, W_INT );
    }
    else {
        JSON_SetInfoCVar( val, W_BOUND );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( right ) )  JSON_FreeTemp( TEMP_CVAR( right ) );
    if ( IS_TEMP_CVAR( left  ) )  JSON_FreeTemp( TEMP_CVAR( left  ) );

    /* return the result                                                   */
    return val;
}


/****************************************************************************
**
*F  JSON_CompIntExpr( <expr> ) . . . . . . . . . . . . . . .  T_INTEXPR/T_INT_EXPR
*
* This is complicated by the need to produce code that will compile correctly
* in 32 or 64 bit and with or without GMP.
*
* The problem is that when we compile the code, we know the integer representation
* of the stored literal in the compiling process
* but NOT the representation which will apply to the compiled code or the endianness
*
* The solution to this is macros: C_MAKE_INTEGER_BAG( size, type) 
*                                 C_SET_LIMB2(bag, limbnumber, value)
*                                 C_SET_LIMB4(bag, limbnumber, value)
*                                 C_SET_LIMB8(bag, limbnumber, value)
*
* we compile using the one appropriate for the compiling system, but their
* definition depends on the limb size of the target system.
*
*/

CVar JSON_CompIntExpr (
    Expr                expr )
{
    CVar                val;
    Int                 siz;
    Int                 i;
    UInt                typ;

    if ( IS_INTEXPR(expr) ) {
        JSON_Emit("%d", INT_INTEXPR(expr));
        return CVAR_INTG( INT_INTEXPR(expr) );
    }
    else {
        val = CVAR_TEMP( JSON_NewTemp( "val" ) );
        siz = SIZE_EXPR(expr) - sizeof(UInt);
	typ = *(UInt *)ADDR_EXPR(expr);
	//JSON_Emit( "%c = C_MAKE_INTEGER_BAG(%d, %d);\n",val, siz, typ);
        JSON_Emit("%d", val);
        if ( typ == T_INTPOS ) {
            JSON_SetInfoCVar(val, W_INT_POS);
        }
        else {
            JSON_SetInfoCVar(val, W_INT);
	      }

        for ( i = 0; i < siz/INTEGER_UNIT_SIZE; i++ ) {
#if INTEGER_UNIT_SIZE == 2
	    //JSON_Emit( "C_SET_LIMB2( %c, %d, %d);\n",val, i, ((UInt2 *)((UInt *)ADDR_EXPR(expr) + 1))[i]);
#else
#if INTEGER_UNIT_SIZE == 4
	    //JSON_Emit( "C_SET_LIMB4( %c, %d, %dL);\n",val, i, ((UInt4 *)((UInt *)ADDR_EXPR(expr) + 1))[i]);
#else
	    //JSON_Emit( "C_SET_LIMB8( %c, %d, %dLL);\n",val, i, ((UInt8*)((UInt *)ADDR_EXPR(expr) + 1))[i]);
#endif
#endif
        }
//	if (siz <= 8)
	  //JSON_Emit("%c = C_NORMALIZE_64BIT(%c);\n", val,val);
        return val;
    }
}


/****************************************************************************
**
*F  JSON_CompTrueExpr( <expr> )  . . . . . . . . . . . . . . . . . . . T_TRUE_EXPR
*/
CVar JSON_CompTrueExpr (
    Expr                expr )
{
    CVar                val;            /* value, result                   */

    /* allocate a new temporary for the 'true' value                       */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    /* emit the code                                                       */
    //JSON_Emit( "%c = True;\n", val );
    JSON_Emit("true");

    /* we know that the result is boolean ;-)                              */
    JSON_SetInfoCVar( val, W_BOOL );

    /* return 'true'                                                       */
    return val;
}


/****************************************************************************
**
*F  JSON_CompFalseExpr( <expr> ) . . . . . . . . . . . . . . . . . .  T_FALSE_EXPR
*/
CVar JSON_CompFalseExpr (
    Expr                expr )
{
    CVar                val;            /* value, result                   */

    /* allocate a new temporary for the 'false' value                      */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    /* emit the code                                                       */
    //JSON_Emit( "%c = False;\n", val );
    JSON_Emit("false");

    /* we know that the result is boolean ;-)                              */
    JSON_SetInfoCVar( val, W_BOOL );

    /* return 'false'                                                      */
    return val;
}


/****************************************************************************
**
*F  JSON_CompCharExpr( <expr> )  . . . . . . . . . . . . . . . . . . . T_CHAR_EXPR
*/
CVar            JSON_CompCharExpr (
    Expr                expr )
{
    CVar                val;            /* result                          */

    /* allocate a new temporary for the char value                         */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"character\", \"val\":");

    /* emit the code                                                       */
    //JSON_Emit( "%c = ObjsChar[%d];\n", val, (Int)(((UChar*)ADDR_EXPR(expr))[0]));

    JSON_Emit("\"%d\"}", (Int)(((UChar*)ADDR_EXPR(expr))[0]));

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( val, W_BOUND );

    /* return the value                                                    */
    return val;
}


/****************************************************************************
**
*F  JSON_CompPermExpr( <expr> )  . . . . . . . . . . . . . . . . . . . T_PERM_EXPR
*/
CVar JSON_CompPermExpr (
    Expr                expr )
{
    CVar                perm;           /* result                          */
    CVar                lcyc;           /* one cycle as list               */
    CVar                lprm;           /* perm as list of list cycles     */
    CVar                val;            /* one point                       */
    Int                 i;
    Int                 j;
    Int                 n;
    Int                 csize;
    Expr                cycle;

    JSON_Emit("{ \"type\":\"permutation\", \"components\":[");

    /* check for the identity                                              */
    if ( SIZE_EXPR(expr) == 0 ) {
        perm = CVAR_TEMP( JSON_NewTemp( "idperm" ) );
        //JSON_Emit( "%c = IdentityPerm;\n", perm );
        JSON_SetInfoCVar( perm, W_BOUND );
        JSON_Emit("]}");
        return perm;
    }

    /* for each cycle create a list                                        */
    perm = CVAR_TEMP( JSON_NewTemp( "perm" ) );
    lcyc = CVAR_TEMP( JSON_NewTemp( "lcyc" ) );
    lprm = CVAR_TEMP( JSON_NewTemp( "lprm" ) );

    /* start with the identity permutation                                 */
    //JSON_Emit( "%c = IdentityPerm;\n", perm );

    /* loop over the cycles                                                */
    n = SIZE_EXPR(expr)/sizeof(Expr);
    //JSON_Emit( "%c = NEW_PLIST( T_PLIST, %d );\n", lprm, n );
    //JSON_Emit( "SET_LEN_PLIST( %c, %d );\n", lprm, n );

    for ( i = 1;  i <= n;  i++ ) {
        cycle = ADDR_EXPR(expr)[i-1];
        csize = SIZE_EXPR(cycle)/sizeof(Expr);
        //JSON_Emit( "%c = NEW_PLIST( T_PLIST, %d );\n", lcyc, csize );
        //JSON_Emit( "SET_LEN_PLIST( %c, %d );\n", lcyc, csize );
        //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", lprm, i, lcyc );
        //JSON_Emit( "CHANGED_BAG( %c );\n", lprm );

        JSON_Emit("[");
        /* loop over the entries of the cycle                              */
        for ( j = 1;  j <= csize;  j++ ) {
            val = JSON_CompExpr( ADDR_EXPR(cycle)[j-1] );
            if(j != csize) {
              JSON_Emit(",");
            }
            //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", lcyc, j, val );
            //JSON_Emit( "CHANGED_BAG( %c );\n", lcyc );
            if ( IS_TEMP_CVAR(val) )  JSON_FreeTemp( TEMP_CVAR(val) );
        }
        JSON_Emit("]");
        if(i != n) {
          JSON_Emit(", ");
        }
    }
    //JSON_Emit( "%c = Array2Perm( %c );\n", perm, lprm );

    /* free the termporaries                                               */
    JSON_FreeTemp( TEMP_CVAR(lprm) );
    JSON_FreeTemp( TEMP_CVAR(lcyc) );

    JSON_Emit("]}");

    return perm;
}


/****************************************************************************
**
*F  JSON_CompListExpr( <expr> )  . . . . . . . . . . . . . . . . . . . T_LIST_EXPR
*/
extern CVar JSON_CompListExpr1 ( Expr expr );
extern void JSON_CompListExpr2 ( CVar list, Expr expr );
extern CVar JSON_CompRecExpr1 ( Expr expr );
extern void JSON_CompRecExpr2 ( CVar rec, Expr expr );

CVar JSON_CompListExpr (
    Expr                expr )
{
    CVar                list;           /* list, result                    */

    /* compile the list expression                                         */
    list = JSON_CompListExpr1( expr );
    JSON_CompListExpr2( list, expr );

    /* return the result                                                   */
    return list;
}


/****************************************************************************
**
*F  JSON_CompListTildeExpr( <expr> ) . . . . . . . . . . . . . .  T_LIST_TILD_EXPR
*/
CVar JSON_CompListTildeExpr (
    Expr                expr )
{
    CVar                list;           /* list value, result              */
    CVar                tilde;          /* old value of tilde              */

    JSON_Emit("{ \"type\":\"ListTilde\", \"list\":");

    /* remember the old value of '~'                                       */
    tilde = CVAR_TEMP( JSON_NewTemp( "tilde" ) );
    //JSON_Emit( "%c = VAL_GVAR( Tilde );\n", tilde );

    /* create the list value                                               */
    list = JSON_CompListExpr1( expr );

    /* assign the list to '~'                                              */
    //JSON_Emit( "AssGVar( Tilde, %c );\n", list );

    /* evaluate the subexpressions into the list value                     */
    JSON_CompListExpr2( list, expr );

    /* restore old value of '~'                                            */
    //JSON_Emit( "AssGVar( Tilde, %c );\n", tilde );
    if ( IS_TEMP_CVAR( tilde ) )  JSON_FreeTemp( TEMP_CVAR( tilde ) );

    JSON_Emit("}");

    /* return the list value                                               */
    return list;
}


/****************************************************************************
**
*F  JSON_CompListExpr1( <expr> ) . . . . . . . . . . . . . . . . . . . . . . local
*
* Create list.
*/
CVar JSON_CompListExpr1 (
    Expr                expr )
{
    CVar                list;           /* list, result                    */

    /* allocate a temporary for the list                                   */
    list = CVAR_TEMP( JSON_NewTemp( "list" ) );

    /* emit the code to make the list                                      */
    //JSON_Emit( "%c = NEW_PLIST( T_PLIST, %d );\n", list, len );
    //JSON_Emit( "SET_LEN_PLIST( %c, %d );\n", list, len );

    /* we know that <list> is a list                                       */
    JSON_SetInfoCVar( list, W_LIST );

    /* return the list                                                     */
    return list;
}


/****************************************************************************
**
*F  JSON_CompListExpr2( <list>, <expr> ) . . . . . . . . . . . . . . . . . . local
*
* Fill list.
*/
void JSON_CompListExpr2 (
    CVar                list,
    Expr                expr )
{
    CVar                sub;            /* subexpression                   */
    Int                 len;            /* logical length of the list      */
    Int                 i;              /* loop variable                   */

    JSON_Emit("[");
    /* get the length of the list                                          */
    len = SIZE_EXPR( expr ) / sizeof(Expr);

    /* emit the code to fill the list                                      */
    for ( i = 1; i <= len; i++ ) {

        /* if the subexpression is empty                                   */
        if ( ADDR_EXPR(expr)[i-1] == 0 ) {
            continue;
        }

        /* special case if subexpression is a list expression              */
        else if ( TNUM_EXPR( ADDR_EXPR(expr)[i-1] ) == T_LIST_EXPR ) {
            sub = JSON_CompListExpr1( ADDR_EXPR(expr)[i-1] );
            //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", list, i, sub );
            //JSON_Emit( "CHANGED_BAG( %c );\n", list );
            JSON_CompListExpr2( sub, ADDR_EXPR(expr)[i-1] );
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        /* special case if subexpression is a record expression            */
        else if ( TNUM_EXPR( ADDR_EXPR(expr)[i-1] ) == T_REC_EXPR ) {
            sub = JSON_CompRecExpr1( ADDR_EXPR(expr)[i-1] );
            //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", list, i, sub );
            //JSON_Emit( "CHANGED_BAG( %c );\n", list );
            JSON_CompRecExpr2( sub, ADDR_EXPR(expr)[i-1] );
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        /* general case                                                    */
        else {
            sub = JSON_CompExpr( ADDR_EXPR(expr)[i-1] );
            //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", list, i, sub );
            if ( ! JSON_HasInfoCVar( sub, W_INT_SMALL ) ) {
                //JSON_Emit( "CHANGED_BAG( %c );\n", list );
            }
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        // if this is not last element
        if(i != len) {
          JSON_Emit(", ");
        }

    }
    JSON_Emit("]");

}


/****************************************************************************
**
*F  JSON_CompRangeExpr( <expr> ) . . . . . . . . . . . . . . . . . .  T_RANGE_EXPR
*/
CVar JSON_CompRangeExpr (
    Expr                expr )
{
    CVar                range;          /* range, result                   */
    CVar                first;          /* first  element                  */
    CVar                second;         /* second element                  */
    CVar                last;           /* last   element                  */

    /* allocate a new temporary for the range                              */
    range = CVAR_TEMP( JSON_NewTemp( "range" ) );

    JSON_Emit("{ \"type\":\"range\", ");

    /* evaluate the expressions                                            */
    if ( SIZE_EXPR(expr) == 2 * sizeof(Expr) ) {
        JSON_Emit("\"first\":");
        first  = JSON_CompExpr( ADDR_EXPR(expr)[0] );
        second = 0;
        JSON_Emit(", \"last\":");
        last   = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    }
    else {
        JSON_Emit("\"first\":");
        first  = JSON_CompExpr( ADDR_EXPR(expr)[0] );
        JSON_Emit(", \"second\":");
        second = JSON_CompExpr( ADDR_EXPR(expr)[1] );
        JSON_Emit(", \"last\":");
        last   = JSON_CompExpr( ADDR_EXPR(expr)[2] );
    }
    
    JSON_Emit("}");

    /* we know that the result is a list                                   */
    JSON_SetInfoCVar( range, W_LIST );

    /* free the temporaries                                                */
    if ( SIZE_EXPR(expr) == 2 * sizeof(Expr) ) {
        if ( IS_TEMP_CVAR( last   ) )  JSON_FreeTemp( TEMP_CVAR( last   ) );
        if ( IS_TEMP_CVAR( first  ) )  JSON_FreeTemp( TEMP_CVAR( first  ) );
    }
    else {
        if ( IS_TEMP_CVAR( last   ) )  JSON_FreeTemp( TEMP_CVAR( last   ) );
        if ( IS_TEMP_CVAR( second ) )  JSON_FreeTemp( TEMP_CVAR( second ) );
        if ( IS_TEMP_CVAR( first  ) )  JSON_FreeTemp( TEMP_CVAR( first  ) );
    }

    /* return the range                                                    */
    return range;
}


/****************************************************************************
**
*F  JSON_CompStringExpr( <expr> )  . . . . . . . . . . compile a string expression
*/
CVar JSON_CompStringExpr (
    Expr                expr )
{
    CVar                string;         /* string value, result            */

    /* allocate a new temporary for the string                             */
    string = CVAR_TEMP( JSON_NewTemp( "string" ) );

    //TODO:
//    JSON_Emit("\"%S\"", (sizeof(UInt) + (Char*)ADDR_EXPR(expr)));
    JSON_Emit("\"%S\"", (Char*)ADDR_EXPR(expr));
//    Pr( "%C", (Int)(sizeof(UInt) + (Char*)ADDR_EXPR(expr)), 0L);

    /* create the string and copy the stuff                                */
    //JSON_Emit( "C_NEW_STRING( %c, %d, \"%C\" );\n",

          /* the sizeof(UInt) offset is to get past the length of the string
             which is now stored in the front of the literal */
//          string, SIZE_EXPR(expr)-1-sizeof(UInt),
//          sizeof(UInt)+ (Char*)ADDR_EXPR(expr) );

    /* we know that the result is a list                                   */
    JSON_SetInfoCVar( string, W_LIST );

    /* return the string                                                   */
    return string;
}


/****************************************************************************
**
*F  JSON_CompRecExpr( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_REC_EXPR
*/
CVar JSON_CompRecExpr (
    Expr                expr )
{
    CVar                rec;            /* record value, result            */

    /* compile the record expression                                       */
    rec = JSON_CompRecExpr1( expr );
    JSON_CompRecExpr2( rec, expr );

    /* return the result                                                   */
    return rec;
}


/****************************************************************************
**
*F  JSON_CompRecTildeExpr( <expr> )  . . . . . . . . . . . . . . . T_REC_TILD_EXPR
*/
CVar JSON_CompRecTildeExpr (
    Expr                expr )
{
    CVar                rec;            /* record value, result            */
    CVar                tilde;          /* old value of tilde              */

    JSON_Emit("{ \"type\":\"RecordTilde\", \"record\":");

    /* remember the old value of '~'                                       */
    tilde = CVAR_TEMP( JSON_NewTemp( "tilde" ) );
    //JSON_Emit( "%c = VAL_GVAR( Tilde );\n", tilde );

    /* create the record value                                             */
    rec = JSON_CompRecExpr1( expr );

    /* assign the record value to the variable '~'                         */
    //JSON_Emit( "AssGVar( Tilde, %c );\n", rec );

    /* evaluate the subexpressions into the record value                   */
    JSON_CompRecExpr2( rec, expr );

    /* restore the old value of '~'                                        */
    //JSON_Emit( "AssGVar( Tilde, %c );\n", tilde );
    if ( IS_TEMP_CVAR( tilde ) )  JSON_FreeTemp( TEMP_CVAR( tilde ) );

    JSON_Emit("}");

    /* return the record value                                             */
    return rec;
}


/****************************************************************************
**
*F  JSON_CompRecExpr1( <expr> )  . . . . . . . . . . . . . . . . . . . . . . local
*
*   Allocate the record.
*/
CVar JSON_CompRecExpr1 (
    Expr                expr )
{
    CVar                rec;            /* record value, result            */

    /* allocate a new temporary for the record                             */
    rec = CVAR_TEMP( JSON_NewTemp( "rec" ) );

    /* emit the code to allocate the new record object                     */
    //JSON_Emit( "%c = NEW_PREC( %d );\n", rec, len );

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( rec, W_BOUND );

    /* return the record                                                   */
    return rec;
}


/****************************************************************************
**
*F  JSON_CompRecExpr2( <rec>, <expr> ) . . . . . . . . . . . . . . . . . . . local
*
*   Assign values to already allocated record.
*/
void            JSON_CompRecExpr2 (
    CVar                rec,
    Expr                expr )
{
    CVar                rnam;           /* name of component               */
    CVar                sub;            /* value of subexpression          */
    Int                 len;            /* number of components            */
    Expr                tmp;            /* temporary variable              */
    Int                 i;              /* loop variable                   */

    /* get the number of components                                        */
    len = SIZE_EXPR( expr ) / (2*sizeof(Expr));

    JSON_Emit("{ \"type\":\"record\", \"components\":{");

    /* handle the subexpressions                                           */
    for ( i = 1; i <= len; i++ ) {

        /* handle the name                                                 */
        tmp = ADDR_EXPR(expr)[2*i-2];
        rnam = CVAR_TEMP( JSON_NewTemp( "rnam" ) );
        if ( IS_INTEXPR(tmp) ) {
            JSON_Emit("\"%s\"", NAME_RNAM((UInt)INT_INTEXPR(tmp)));
            JSON_CompSetUseRNam( (UInt)INT_INTEXPR(tmp), COMP_USE_RNAM_ID );
           // JSON_Emit( "%c = (Obj)R_%n;\n",
           //       rnam, NAME_RNAM((UInt)INT_INTEXPR(tmp)) );
        }
        else {
            sub = JSON_CompExpr( tmp );
//            JSON_Emit( "%c = (Obj)RNamObj( %c );\n", rnam, sub );
        }

        JSON_Emit(":");

        /* if the subexpression is empty (cannot happen for records)       */
        tmp = ADDR_EXPR(expr)[2*i-1];
        if ( tmp == 0 ) {
            if ( IS_TEMP_CVAR( rnam ) )  JSON_FreeTemp( TEMP_CVAR( rnam ) );
            continue;
        }

        /* special case if subexpression is a list expression             */
        else if ( TNUM_EXPR( tmp ) == T_LIST_EXPR ) {
            sub = JSON_CompListExpr1( tmp );
            //JSON_Emit( "AssPRec( %c, (UInt)%c, %c );\n", rec, rnam, sub );
            JSON_CompListExpr2( sub, tmp );
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        /* special case if subexpression is a record expression            */
        else if ( TNUM_EXPR( tmp ) == T_REC_EXPR ) {
            sub = JSON_CompRecExpr1( tmp );
            //JSON_Emit( "AssPRec( %c, (UInt)%c, %c );\n", rec, rnam, sub );
            JSON_CompRecExpr2( sub, tmp );
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        /* general case                                                    */
        else {
            sub = JSON_CompExpr( tmp );
            //JSON_Emit( "AssPRec( %c, (UInt)%c, %c );\n", rec, rnam, sub );
            if ( IS_TEMP_CVAR( sub ) )  JSON_FreeTemp( TEMP_CVAR( sub ) );
        }

        if ( IS_TEMP_CVAR( rnam ) )  JSON_FreeTemp( TEMP_CVAR( rnam ) );

        if( i < len ) {
          JSON_Emit(", ");
        }
    }
    //JSON_Emit( "SortPRecRNam( %c, 0 );\n", rec );

    JSON_Emit("}}");
}


/****************************************************************************
**
*F  JSON_CompRefLVar( <expr> ) . . . . . . .  T_REFLVAR/T_REF_LVAR...T_REF_LVAR_16
*/
CVar JSON_CompRefLVar (
    Expr                expr )
{
    CVar                val;            /* value, result                   */
    LVar                lvar;           /* local variable                  */

    /* get the local variable                                              */
    if ( IS_REFLVAR(expr) ) {
        lvar = LVAR_REFLVAR(expr);
    }
    else {
        lvar = (LVar)(ADDR_EXPR(expr)[0]);
    }

    /* emit the code to get the value                                      */
    if ( JSON_CompGetUseHVar( lvar ) ) {
        val = CVAR_TEMP( JSON_NewTemp( "val" ) );
    }
    else {
        val = CVAR_LVAR(lvar);
    }

    JSON_Emit("{\"type\":\"variable\", \"subtype\":\"RefLVar\"");
    JSON_Emit(", \"identifier\":\"%s\"}", NAME_LVAR(lvar));

    /* return the value                                                    */
    return val;
}


/****************************************************************************
**
*F  JSON_CompIsbLVar( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_ISB_LVAR
*/
CVar JSON_CompIsbLVar (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                val;            /* value                           */
    LVar                lvar;           /* local variable                  */

    /* get the local variable                                              */
    lvar = (LVar)(ADDR_EXPR(expr)[0]);

    JSON_Emit("{\"type\":\"isBound\", \"subtype\":\"LVar\"");
    JSON_Emit(", \"identifier\":\"%s\"}", NAME_LVAR(lvar));

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* emit the code to get the value                                      */
    if ( JSON_CompGetUseHVar( lvar ) ) {
        val = CVAR_TEMP( JSON_NewTemp( "val" ) );
    }
    else {
        val = CVAR_LVAR(lvar);
    }

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( val ) )  JSON_FreeTemp( TEMP_CVAR( val ) );

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompRefHVar( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_REF_HVAR
*/
CVar JSON_CompRefHVar (
    Expr                expr )
{
    CVar                val;            /* value, result                   */
    HVar                hvar;           /* higher variable                 */

    /* get the higher variable                                             */
    hvar = (HVar)(ADDR_EXPR(expr)[0]);
    JSON_CompSetUseHVar( hvar );

    /* allocate a new temporary for the value                              */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    /* emit the code to check that the variable has a value                */
    JSON_CompCheckBound( val, NAME_HVAR(hvar) );

    JSON_Emit("{\"type\":\"variable\", \"subtype\":\"RefHVar\"");
    JSON_Emit(", \"identifier\":\"%s\"}", NAME_HVAR(hvar));
 
    /* return the value                                                    */
    return val;
}


/****************************************************************************
**
*F  JSON_CompIsbHVar( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_ISB_HVAR
*/
CVar JSON_CompIsbHVar (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                val;            /* value                           */
    HVar                hvar;           /* higher variable                 */

    /* get the higher variable                                             */
    hvar = (HVar)(ADDR_EXPR(expr)[0]);
    JSON_CompSetUseHVar( hvar );

    /* allocate new temporaries for the value and the result               */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( val ) )  JSON_FreeTemp( TEMP_CVAR( val ) );

    JSON_Emit("{\"type\":\"isBound\", \"subtype\":\"HVar\"");
    JSON_Emit(", \"identifier\":\"%s\"}", NAME_HVAR(hvar));

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompRefGVar( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_REF_GVAR
*/
CVar JSON_CompRefGVar (
    Expr                expr )
{
    CVar                val;            /* value, result                   */
    GVar                gvar;           /* higher variable                 */

    /* get the global variable                                             */
    gvar = (GVar)(ADDR_EXPR(expr)[0]);
    JSON_CompSetUseGVar( gvar, COMP_USE_GVAR_COPY );

    /* allocate a new global variable for the value                        */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"variable\", \"subtype\":\"RefGVar\"");
    JSON_Emit( ", \"identifier\":\"%s\"}", NameGVar(gvar));

    /* emit the code to check that the variable has a value                */
    JSON_CompCheckBound( val, NameGVar(gvar) );

    /* return the value                                                    */
    return val;
}


/****************************************************************************
**
*F  JSON_CompRefGVarFopy( <expr> ) . . . . . . . . . . . . . . . . . . . . . local
*/
CVar JSON_CompRefGVarFopy (
    Expr                expr )
{
    CVar                val;            /* value, result                   */
    GVar                gvar;           /* higher variable                 */

    /* get the global variable                                             */
    gvar = (GVar)(ADDR_EXPR(expr)[0]);
    JSON_CompSetUseGVar( gvar, COMP_USE_GVAR_FOPY );

    /* allocate a new temporary for the value                              */
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    JSON_Emit("{\"type\":\"variable\", \"subtype\":\"RefGVarFopy\"");
    /* emit name of the global variable                                    */
    JSON_Emit(", \"identifier\":\"%s\"}", NameGVar(gvar));

    /* we know that the object in a function copy is a function            */
    JSON_SetInfoCVar( val, W_FUNC );

    /* return the value                                                    */
    return val;
}


/****************************************************************************
**
*F  JSON_CompIsbGVar( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_ISB_GVAR
*/
CVar JSON_CompIsbGVar (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                val;            /* value, result                   */
    GVar                gvar;           /* higher variable                 */

    /* get the global variable                                             */
    gvar = (GVar)(ADDR_EXPR(expr)[0]);
    JSON_CompSetUseGVar( gvar, COMP_USE_GVAR_COPY );

    JSON_Emit("{\"type\":\"isBound\", \"subtype\":\"GVar\"");
    JSON_Emit( ", \"identifier\":\"%s\"}", NameGVar(gvar));

    /* allocate new temporaries for the value and the result               */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );
    val = CVAR_TEMP( JSON_NewTemp( "val" ) );

    /* emit the code to get the value                                      */
    //JSON_Emit( "%c = GC_%n;\n", val, NameGVar(gvar) );

    /* emit the code to check that the variable has a value                */
    //JSON_Emit( "%c = ((%c != 0) ? True : False);\n", isb, val );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( val ) )  JSON_FreeTemp( TEMP_CVAR( val ) );

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompElmList( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_ELM_LIST
*/
CVar JSON_CompElmList (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */

    JSON_Emit( "{ \"type\":\"listAccess\", \"list\":");

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    /* compile the list expression (checking is done by 'ELM_LIST')        */
    list = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit( ", \"position\":");

    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit( "}" );

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );

    /* return the element                                                  */
    return elm;
}


/****************************************************************************
**
*F  JSON_CompElmsList( <expr> )  . . . . . . . . . . . . . . . . . . . T_ELMS_LIST
*/
CVar JSON_CompElmsList (
    Expr                expr )
{
    CVar                elms;           /* elements, result                */
    CVar                list;           /* list                            */
    CVar                poss;           /* positions                       */

    /* allocate a new temporary for the elements                           */
    elms = CVAR_TEMP( JSON_NewTemp( "elms" ) );

    JSON_Emit("{ \"type\":\"elmslist\", \"list\":");

    /* compile the list expression (checking is done by 'ElmsListCheck')   */
    list = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"pos\":");

    /* compile the position expression (checking done by 'ElmsListCheck')  */
    poss = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to get the element                                    */
    //JSON_Emit( "%c = ElmsListCheck( %c, %c );\n", elms, list, poss );

    /* we know that the elements are a list                                */
    JSON_SetInfoCVar( elms, W_LIST );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( poss ) )  JSON_FreeTemp( TEMP_CVAR( poss ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );

    /* return the elements                                                 */
    return elms;
}


/****************************************************************************
**
*F  JSON_CompElmListLev( <expr> )  . . . . . . . . . . . . . . . .  T_ELM_LIST_LEV
*/
CVar JSON_CompElmListLev (
    Expr                expr )
{
    CVar                lists;          /* lists                           */
    CVar                pos;            /* position                        */
    Int                 level;          /* level                           */

    JSON_Emit("{ \"type\":\"elmListLev\", \"lists\":");

    /* compile the lists expression                                        */
    lists = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"pos\":");

    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit(", \"level\":");
    /* get the level                                                       */
    level = (Int)(ADDR_EXPR(expr)[2]);

    JSON_Emit("%d}", level);

    /* emit the code to select the elements from several lists (to <lists>)*/
    //JSON_Emit( "ElmListLevel( %c, %c, %d );\n", lists, pos, level );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos   ) )  JSON_FreeTemp( TEMP_CVAR( pos   ) );

    /* return the lists                                                    */
    return lists;
}


/****************************************************************************
**
*F  JSON_CompElmsListLev( <expr> ) . . . . . . . . . . . . . . . . T_ELMS_LIST_LEV
*/
CVar JSON_CompElmsListLev (
    Expr                expr )
{
    CVar                lists;          /* lists                           */
    CVar                poss;           /* positions                       */
    Int                 level;          /* level                           */

    JSON_Emit("{ \"type\":\"ElmsListLev\", \"list\":");

    /* compile the lists expression                                        */
    lists = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"position\":");

    /* compile the position expression (checking done by 'ElmsListLevel')  */
    poss = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    /* get the level                                                       */
    level = (Int)(ADDR_EXPR(expr)[2]);

    JSON_Emit(", \"level\":%d", level);

    JSON_Emit("}");

    /* emit the code to select the elements from several lists (to <lists>)*/
    //JSON_Emit( "ElmsListLevelCheck( %c, %c, %d );\n", lists, poss, level );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( poss  ) )  JSON_FreeTemp( TEMP_CVAR( poss  ) );

    /* return the lists                                                    */
    return lists;
}


/****************************************************************************
**
*F  JSON_CompIsbList( <expr> ) . . . . . . . . . . . . . . . . . . . .  T_ISB_LIST
*/
CVar JSON_CompIsbList (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */

    JSON_Emit("{ \"type\":\"CompIsbList\", \"list\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the list expression (checking is done by 'ISB_LIST')        */
    list = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"position\":");

    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to test the element                                   */
    //JSON_Emit( "%c = C_ISB_LIST( %c, %c );\n", isb, list, pos );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );

    /* return the element                                                  */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompElmRecName( <expr> )  . . . . . . . . . . . . . . . .  T_ELM_REC_NAME
*/
CVar JSON_CompElmRecName (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                record;         /* the record, left operand        */
    UInt                rnam;           /* the name, right operand         */

    JSON_Emit("{ \"type\":\"CompElmRecName\", \"record\":");

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    /* compile the record expression (checking is done by 'ELM_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = (UInt)(ADDR_EXPR(expr)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    /* emit the code to select the element of the record                   */
    //JSON_Emit( "%c = ELM_REC( %c, R_%n );\n", elm, record, NAME_RNAM(rnam) );

    JSON_Emit("}");

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the element                                                  */
    return elm;
}


/****************************************************************************
**
*F  JSON_CompElmRecExpr( <expr> )  . . . . . . . . . . . . . . . .  T_ELM_REC_EXPR
*/
CVar JSON_CompElmRecExpr (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                record;         /* the record, left operand        */
    CVar                rnam;           /* the name, right operand         */

    JSON_Emit("{ \"type\":\"CompElmRecExpr\", \"record\":");

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    /* compile the record expression (checking is done by 'ELM_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* compile the record name expression                                  */
    rnam = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to select the element of the record                   */
    //JSON_Emit( "%c = ELM_REC( %c, RNamObj(%c) );\n", elm, record, rnam );

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the element                                                  */
    return elm;
}


/****************************************************************************
**
*F  JSON_CompIsbRecName( <expr> )  . . . . . . . . . . . . . . . .  T_ISB_REC_NAME
*/
CVar JSON_CompIsbRecName (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                record;         /* the record, left operand        */
    UInt                rnam;           /* the name, right operand         */

    JSON_Emit("{ \"type\":\"CompIsbRecName\", \"record\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the record expression (checking is done by 'ISB_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = (UInt)(ADDR_EXPR(expr)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    /* emit the code to test the element                                   */
    //JSON_Emit( "%c = (ISB_REC( %c, R_%n ) ? True : False);\n",
    //      isb, record, NAME_RNAM(rnam) );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    JSON_Emit("}");

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompIsbRecExpr( <expr> )  . . . . . . . . . . . . . . . .  T_ISB_REC_EXPR
*/
CVar JSON_CompIsbRecExpr (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                record;         /* the record, left operand        */
    CVar                rnam;           /* the name, right operand         */

    JSON_Emit("{\"type\":\"CompIsbRecExpr\", \"record\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the record expression (checking is done by 'ISB_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* compile the record name expression                                  */
    rnam = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to test the element                                   */
    //JSON_Emit( "%c = (ISB_REC( %c, RNamObj(%c) ) ? True : False);\n",
    //      isb, record, rnam );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompElmPosObj( <expr> ) . . . . . . . . . . . . . . . . . .  T_ELM_POSOBJ
*/
CVar JSON_CompElmPosObj (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */

    JSON_Emit("{\"type\":\"getListElement\", \"compiledIn\":\"CompElmPosObj\""
         ", \"list\":"
    );

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    /* compile the list expression (checking is done by 'ELM_LIST')        */
    list = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"position\":");

    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit("}");

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );

    /* return the element                                                  */
    return elm;
}


/****************************************************************************
**
*F  JSON_CompElmsPosObj( <expr> )  . . . . . . . . . . . . . . . . . T_ELMS_POSOBJ
*/
CVar JSON_CompElmsPosObj (
    Expr                expr )
{
    //JSON_Emit( "CANNOT COMPILE EXPRESSION OF TNUM %d;\n", TNUM_EXPR(expr) );
    return 0;
}


/****************************************************************************
**
*F  JSON_CompElmPosObjLev( <expr> )  . . . . . . . . . . . . . .  T_ELM_POSOBJ_LEV
*/
CVar JSON_CompElmPosObjLev (
    Expr                expr )
{
    //JSON_Emit( "CANNOT COMPILE EXPRESSION OF TNUM %d;\n", TNUM_EXPR(expr) );
    return 0;
}


/****************************************************************************
**
*F  JSON_CompElmsPosObjLev( <expr> ) . . . . . . . . . . . . . . . . T_ELMS_POSOBJ
*/
CVar JSON_CompElmsPosObjLev (
    Expr                expr )
{
    //JSON_Emit( "CANNOT COMPILE EXPRESSION OF TNUM %d;\n", TNUM_EXPR(expr) );
    return 0;
}


/****************************************************************************
**
*F  JSON_CompIsbPosObj( <expr> ) . . . . . . . . . . . . . . . . . .  T_ISB_POSOBJ
*/
CVar JSON_CompIsbPosObj (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */

    JSON_Emit("{ \"type\":\"CompIsbPosObj\", \"list\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the list expression (checking is done by 'ISB_LIST')        */
    list = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"pos\":");

    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_EXPR(expr)[1] );
    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit("}");

    /* emit the code to test the element                                   */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_POSOBJ ) {\n", list );
    //JSON_Emit( "%c = (%i <= SIZE_OBJ(%c)/sizeof(Obj)-1\n", isb, pos, list );
    //JSON_Emit( "   && ELM_PLIST(%c,%i) != 0 ? True : False);\n", list, pos );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_APOSOBJ ) {\n", list );
    //JSON_Emit( "%c = Elm0AList(%c,%i) != 0 ? True : False;\n", isb, list, pos );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "%c = (ISB_LIST( %c, %i ) ? True : False);\n", isb, list, pos );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );

    /* return the element                                                  */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompElmObjName( <expr> )  . . . . . . . . . . . . . . . T_ELM_COMOBJ_NAME
*/
CVar JSON_CompElmComObjName (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                record;         /* the record, left operand        */
    UInt                rnam;           /* the name, right operand         */

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    JSON_Emit("{\"type\":\"recordAccess\", \"record\":");

    /* compile the record expression (checking is done by 'ELM_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = (UInt)(ADDR_EXPR(expr)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    JSON_Emit("}");

    /* emit the code to select the element of the record                   */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "%c = ElmPRec( %c, R_%n );\n", elm, record, NAME_RNAM(rnam) );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ) {\n", record );
    //JSON_Emit( "%c = ElmARecord( %c, R_%n );\n", elm, record, NAME_RNAM(rnam) );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "%c = ELM_REC( %c, R_%n );\n", elm, record, NAME_RNAM(rnam) );
    //JSON_Emit( "}\n" );

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the element                                                  */
    return elm;
}



/****************************************************************************
**
*F  JSON_CompElmComObjExpr( <expr> ) . . . . . . . . . . . . . . T_ELM_COMOBJ_EXPR
*/
CVar JSON_CompElmComObjExpr (
    Expr                expr )
{
    CVar                elm;            /* element, result                 */
    CVar                record;         /* the record, left operand        */
    CVar                rnam;           /* the name, right operand         */

    JSON_Emit("{ \"type\":\"CompElmComObjExpr\", ");

    /* allocate a new temporary for the element                            */
    elm = CVAR_TEMP( JSON_NewTemp( "elm" ) );

    JSON_Emit("\"record\":");

    /* compile the record expression (checking is done by 'ELM_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to select the element of the record                   */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "%c = ElmPRec( %c, RNamObj(%c) );\n", elm, record, rnam );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "%c = ElmARecord( %c, RNamObj(%c) );\n", elm, record, rnam );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "%c = ELM_REC( %c, RNamObj(%c) );\n", elm, record, rnam );
    //JSON_Emit( "}\n" );

    /* we know that we have a value                                        */
    JSON_SetInfoCVar( elm, W_BOUND );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the element                                                  */
    return elm;
}


/****************************************************************************
**
*F  JSON_CompIsbComObjName( <expr> ) . . . . . . . . . . . . . . T_ISB_COMOBJ_NAME
*/
CVar JSON_CompIsbComObjName (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                record;         /* the record, left operand        */
    UInt                rnam;           /* the name, right operand         */

    JSON_Emit("{\"type\":\"CompIsbComObjName\", \"record\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the record expression (checking is done by 'ISB_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = (UInt)(ADDR_EXPR(expr)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    JSON_Emit("}");

    /* emit the code to test the element                                   */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "%c = (IsbPRec( %c, R_%n ) ? True : False);\n",
    //      isb, record, NAME_RNAM(rnam) );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "%c = (IsbARecord( %c, R_%n ) ? True : False);\n",
    //            isb, record, NAME_RNAM(rnam) );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "%c = (ISB_REC( %c, R_%n ) ? True : False);\n",
    //      isb, record, NAME_RNAM(rnam) );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**
*F  JSON_CompIsbComObjExpr( <expr> ) . . . . . . . . . . . . . . T_ISB_COMOBJ_EXPR
*/
CVar JSON_CompIsbComObjExpr (
    Expr                expr )
{
    CVar                isb;            /* isbound, result                 */
    CVar                record;         /* the record, left operand        */
    UInt                rnam;           /* the name, right operand         */

    JSON_Emit("{ \"type\":\"CompIsbComObjExpr\", \"record\":");

    /* allocate a new temporary for the result                             */
    isb = CVAR_TEMP( JSON_NewTemp( "isb" ) );

    /* compile the record expression (checking is done by 'ISB_REC')       */
    record = JSON_CompExpr( ADDR_EXPR(expr)[0] );

    JSON_Emit(", \"name\":");

    /* get the name (stored immediately in the expression)                 */
    rnam = JSON_CompExpr( ADDR_EXPR(expr)[1] );

    JSON_Emit("}");

    /* emit the code to test the element                                   */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "%c = (IsbPRec( %c, RNamObj(%c) ) ? True : False);\n",
    //      isb, record, rnam );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "%c = (IsbARecord( %c, RNamObj(%c) ) ? True : False);\n",
    //            isb, record, rnam );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "%c = (ISB_REC( %c, RNamObj(%c) ) ? True : False);\n",
    //      isb, record, rnam );
    //JSON_Emit( "}\n" );

    /* we know that the result is boolean                                  */
    JSON_SetInfoCVar( isb, W_BOOL );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );

    /* return the result                                                   */
    return isb;
}


/****************************************************************************
**

*F * * * * * * * * * * * * * compile statements * * * * * * * * * * * * * * *
*/


/****************************************************************************
**

*F  JSON_CompStat( <stat> )  . . . . . . . . . . . . . . . . . compile a statement
**
**  'JSON_CompStat' compiles the statement <stat>.
*/
void (* JSON_CompStatFuncs[256]) ( Stat stat );

void JSON_CompStat (
    Stat                stat )
{
    //emit line number
    JSON_Emit("{ \"type\":\"debugInfo\", \"line\":%d, \"stat\":", LINE_STAT(stat));
    (* JSON_CompStatFuncs[ TNUM_STAT(stat) ])( stat );
    JSON_Emit("}");
}


/****************************************************************************
**
*F  JSON_CompUnknownStat( <stat> ) . . . . . . . . . . . . . . . . signal an error
*/
void JSON_CompUnknownStat (
    Stat                stat )
{
    printf("ERROR: unknownStat");
    //JSON_Emit( "CANNOT COMPILE STATEMENT OF TNUM %d;\n", TNUM_STAT(stat) );
}


/****************************************************************************
**
*V  G_Add . . . . . . . . . . . . . . . . . . . . . . . . . .  function 'Add'
*/
GVar G_Add;


/****************************************************************************
**
*F  JSON_CompProccall0to6Args( <stat> )  . . . T_PROCCALL_0ARGS...T_PROCCALL_6ARGS
*/
void JSON_CompProccall0to6Args (
    Stat                stat )
{
    CVar                func;           /* function                        */
    CVar                args[8];        /* arguments                       */
    UInt                narg;           /* number of arguments             */
    UInt                i;              /* loop variable                   */

    JSON_Emit( "{ \"type\":\"functionCall\", \"name\":");

    /* compile the reference to the function                               */
    if ( TNUM_EXPR( FUNC_CALL(stat) ) == T_REF_GVAR ) {
        func = JSON_CompRefGVarFopy( FUNC_CALL(stat) );
//        JSON_Emit("%s", NAME_FUNC( FUNC_CALL(stat) ));
    }
    else {
        func = JSON_CompExpr( FUNC_CALL(stat) );
    }

    JSON_Emit(", \"args\":[");

    /* compile the argument expressions                                    */
    narg = NARG_SIZE_CALL(SIZE_STAT(stat));
    for ( i = 1; i <= narg; i++ ) {
        args[i] = JSON_CompExpr( ARGI_CALL(stat,i) );
        if( i < narg) {
            JSON_Emit( ", " );
        }
    }

    JSON_Emit( "]}" );

    /* free the temporaries                                                */
    for ( i = narg; 1 <= i; i-- ) {
        if ( IS_TEMP_CVAR( args[i] ) )  JSON_FreeTemp( TEMP_CVAR( args[i] ) );
    }
    if ( IS_TEMP_CVAR( func ) )  JSON_FreeTemp( TEMP_CVAR( func ) );
}


/****************************************************************************
**
*F  JSON_CompProccallXArgs . . . . . . . . . . . . . . . . . . .  T_PROCCALL_XARGS
*/
void JSON_CompProccallXArgs (
    Stat                stat )
{
    CVar                func;           /* function                        */
    CVar                argl;           /* argument list                   */
    CVar                argi;           /* <i>-th argument                 */
    UInt                narg;           /* number of arguments             */
    UInt                i;              /* loop variable                   */

    JSON_Emit( "{ \"type\":\"functionCall\", \"name\":" );

    /* compile the reference to the function                               */
    if ( TNUM_EXPR( FUNC_CALL(stat) ) == T_REF_GVAR ) {
        func = JSON_CompRefGVarFopy( FUNC_CALL(stat) );
    }
    else {
        func = JSON_CompExpr( FUNC_CALL(stat) );
        JSON_CompCheckFunc( func );
    }

    JSON_Emit(",\"args\":[");

    /* compile the argument expressions                                    */
    narg = NARG_SIZE_CALL(SIZE_STAT(stat));
    argl = CVAR_TEMP( JSON_NewTemp( "argl" ) );
    //JSON_Emit( "%c = NEW_PLIST( T_PLIST, %d );\n", argl, narg );
    //JSON_Emit( "SET_LEN_PLIST( %c, %d );\n", argl, narg );
    for ( i = 1; i <= narg; i++ ) {
        argi = JSON_CompExpr( ARGI_CALL( stat, i ) );
        //JSON_Emit( "SET_ELM_PLIST( %c, %d, %c );\n", argl, i, argi );
        if ( ! JSON_HasInfoCVar( argi, W_INT_SMALL ) ) {
            //JSON_Emit( "CHANGED_BAG( %c );\n", argl );
        }
        if ( IS_TEMP_CVAR( argi ) )  JSON_FreeTemp( TEMP_CVAR( argi ) );

        if( i < narg) {
          JSON_Emit(", ");
        }
    }

    JSON_Emit( "]}" );

    /* emit the code for the procedure call                                */
    //JSON_Emit( "CALL_XARGS( %c, %c );\n", func, argl );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( argl ) )  JSON_FreeTemp( TEMP_CVAR( argl ) );
    if ( IS_TEMP_CVAR( func ) )  JSON_FreeTemp( TEMP_CVAR( func ) );
}

/****************************************************************************
**
*F  JSON_CompProccallXArgs( <expr> ) . . . . . . . . . . . . . .  T_PROCCALL_OPTS
*/
void JSON_CompProccallOpts(
                      Stat stat)
{
  JSON_Emit("{ \"type\":\"ProccallOpts\", \"opts\":");
  CVar opts = JSON_CompExpr(ADDR_STAT(stat)[0]);
  GVar pushOptions;
  GVar popOptions;
  pushOptions = GVarName("PushOptions");
  popOptions = GVarName("PopOptions");
  JSON_CompSetUseGVar(pushOptions, COMP_USE_GVAR_FOPY);
  JSON_CompSetUseGVar(popOptions, COMP_USE_GVAR_FOPY);
  //JSON_Emit("CALL_1ARGS( GF_PushOptions, %c );\n", opts);
  if (IS_TEMP_CVAR( opts) ) JSON_FreeTemp( TEMP_CVAR( opts ));
  JSON_Emit(", \"proc\":");
  JSON_CompStat(ADDR_STAT(stat)[1]);
  //JSON_Emit("CALL_0ARGS( GF_PopOptions );\n");
  JSON_Emit("}");
}
     

/****************************************************************************
**
*F  JSON_CompSeqStat( <stat> ) . . . . . . . . . . . . .  T_SEQ_STAT...T_SEQ_STAT7
*/
void JSON_CompSeqStat (
    Stat                stat )
{
    UInt                nr;             /* number of statements            */
    UInt                i;              /* loop variable                   */

    /* get the number of statements                                        */
    nr = SIZE_STAT( stat ) / sizeof(Stat);

    JSON_Emit("[");

    /* compile the statements                                              */
    for ( i = 1; i <= nr; i++ ) {
        JSON_CompStat( ADDR_STAT( stat )[i-1] );
        if( i < nr) {
          JSON_Emit(", ");
        }
    }

    JSON_Emit("]");
}


/****************************************************************************
**
*F  JSON_CompIf( <stat> )  . . . . . . . . T_IF/T_IF_ELSE/T_IF_ELIF/T_IF_ELIF_ELSE
*/
void JSON_CompIf (
    Stat                stat )
{
    CVar                cond;           /* condition                       */
    UInt                nr;             /* number of branches              */
    Bag                 info_in;        /* information at branch begin     */
    Bag                 info_out;       /* information at branch end       */
    UInt                i;              /* loop variable                   */

    /* get the number of branches                                          */
    nr = SIZE_STAT( stat ) / (2*sizeof(Stat));

    JSON_Emit( "{\"type\":\"if\", \"cond\":" );
//    JSON_Emit("open");

    /* compile the expression                                              */
    cond = JSON_CompBoolExpr( ADDR_STAT( stat )[0] );

    JSON_Emit( ", \"then\":" );

    /* emit the code to test the condition                                 */
    //JSON_Emit( "if ( %c ) {\n", cond );
    if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );

    /* remember what we know after evaluating the first condition          */
    info_in = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( info_in, INFO_FEXP(CURR_FUNC) );

    /* compile the body                                                    */
    JSON_CompStat( ADDR_STAT( stat )[1] );

    /* remember what we know after executing the first body                */
    info_out = JSON_NewInfoCVars();
    JSON_CopyInfoCVars( info_out, INFO_FEXP(CURR_FUNC) );

    /* emit the rest code                                                  */

    /* loop over the 'elif' branches                                       */
    for ( i = 2; i <= nr; i++ ) {

        /* do not handle 'else' branch here                                */
        if ( i == nr && TNUM_EXPR(ADDR_STAT(stat)[2*(i-1)]) == T_TRUE_EXPR )
            break;

        JSON_Emit( ",\"else\":");
        JSON_Emit("{ \"type\":\"debugInfo\", \"line\":%d, \"stat\":", LINE_STAT(stat));
        JSON_Emit("{\"type\":\"if\", \"cond\":"); 
    //    JSON_Emit("open");

        /* emit the 'else' to connect this branch to the 'if' branch       */
        //JSON_Emit( "else {\n" );

        /* this is what we know if we enter this branch                    */
        JSON_CopyInfoCVars( INFO_FEXP(CURR_FUNC), info_in );

        /* compile the expression                                          */
        cond = JSON_CompBoolExpr( ADDR_STAT( stat )[2*(i-1)] );

        JSON_Emit( ", \"then\":" );

        /* emit the code to test the condition                             */
        //JSON_Emit( "if ( %c ) {\n", cond );
        if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );

        /* remember what we know after evaluating all previous conditions  */
        JSON_CopyInfoCVars( info_in, INFO_FEXP(CURR_FUNC) );

        /* compile the body                                                */
        JSON_CompStat( ADDR_STAT( stat )[2*(i-1)+1] );

        /* remember what we know after executing one of the previous bodies*/
        JSON_MergeInfoCVars( info_out, INFO_FEXP(CURR_FUNC) );
    }

    /* handle 'else' branch                                                */
    if ( i == nr ) {

        JSON_Emit( ",\"else\":" );

        /* emit the 'else' to connect this branch to the 'if' branch       */
        //JSON_Emit( "else {\n" );

        /* this is what we know if we enter this branch                    */
        JSON_CopyInfoCVars( INFO_FEXP(CURR_FUNC), info_in );

        /* compile the body                                                */
        JSON_CompStat( ADDR_STAT( stat )[2*(i-1)+1] );

        /* remember what we know after executing one of the previous bodies*/
        JSON_MergeInfoCVars( info_out, INFO_FEXP(CURR_FUNC) );
    }

    /* fake empty 'else' branch                                            */
    else {

       // if( JSON_CompPass == 2) {
       //   JSON_Emit("else=''");
       // }

        /* this is what we know if we enter this branch                    */
        JSON_CopyInfoCVars( INFO_FEXP(CURR_FUNC), info_in );

        /* remember what we know after executing one of the previous bodies*/
        JSON_MergeInfoCVars( info_out, INFO_FEXP(CURR_FUNC) );

    }

    /* close all unbalanced parenthesis                                    */
    for ( i = 2; i <= nr; i++ ) {
        if ( i == nr && TNUM_EXPR(ADDR_STAT(stat)[2*(i-1)]) == T_TRUE_EXPR )
            break;
        JSON_Emit( "}}" );
   //     JSON_Emit("close");
    }
    
    JSON_Emit( "}" );
    //JSON_Emit("endOfElse close");

    /* put what we know into the current info                              */
    JSON_CopyInfoCVars( INFO_FEXP(CURR_FUNC), info_out );
}


/****************************************************************************
**
*F  JSON_CompFor( <stat> ) . . . . . . . T_FOR...T_FOR3/T_FOR_RANGE...T_FOR_RANGE3
*/
void JSON_CompFor (
    Stat                stat )
{
    UInt                var;            /* loop variable                   */
    Char                vart;           /* variable type                   */
    CVar                list;           /* list to loop over               */
    CVar                islist;         /* is the list a proper list       */
    CVar                first;          /* first loop index                */
    CVar                last;           /* last  loop index                */
    CVar                lidx;           /* loop index variable             */
    CVar                elm;            /* element of list                 */
    Int                 i;              /* loop variable                   */

    JSON_Emit( "{ \"type\":\"for\", \"var\":\"" );

    /* handle 'for <lvar> in [<first>..<last>] do'                         */
    if ( IS_REFLVAR( ADDR_STAT(stat)[0] )
      && ! JSON_CompGetUseHVar( LVAR_REFLVAR( ADDR_STAT(stat)[0] ) )
      && TNUM_EXPR( ADDR_STAT(stat)[1] ) == T_RANGE_EXPR
      && SIZE_EXPR( ADDR_STAT(stat)[1] ) == 2*sizeof(Expr) ) {

        /* get the local variable                                          */
        var = LVAR_REFLVAR( ADDR_STAT(stat)[0] );

        JSON_Emit("%s", NAME_LVAR( var ));
        JSON_Emit( "\", \"in\":" );

        /* allocate a new temporary for the loop variable                  */
        lidx = CVAR_TEMP( JSON_NewTemp( "lidx" ) );

        JSON_Emit("{\"type\":\"range\", \"first\":");

        /* compile and check the first and last value                      */
        first = JSON_CompExpr( ADDR_EXPR( ADDR_STAT(stat)[1] )[0] );

        JSON_Emit(", \"last\":");

        /* compile and check the last value                                */
        /* if the last value is in a local variable,                       */
        /* we must copy it into a temporary,                               */
        /* because the local variable may change its value in the body     */
        last  = JSON_CompExpr( ADDR_EXPR( ADDR_STAT(stat)[1] )[1] );

        JSON_Emit("}");

        if ( IS_LVAR_CVAR(last) ) {
            elm = CVAR_TEMP( JSON_NewTemp( "last" ) );
            //JSON_Emit( "%c = %c;\n", elm, last );
            last = elm;
        }

        ///* find the invariant temp-info                                    */
        //pass = JSON_CompPass;
        //JSON_CompPass = 99;
        //prev = JSON_NewInfoCVars();
        //do {
        //    JSON_CopyInfoCVars( prev, INFO_FEXP(CURR_FUNC) );
        //    if ( JSON_HasInfoCVar( first, W_INT_SMALL_POS ) ) {
        //        JSON_SetInfoCVar( CVAR_LVAR(var), W_INT_SMALL_POS );
        //    }
        //    else {
        //        JSON_SetInfoCVar( CVAR_LVAR(var), W_INT_SMALL );
        //    }
        //    //TODO: what is this? why is this loop compiled twice?
        //    for ( i = 2; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
        //        JSON_CompStat( ADDR_STAT(stat)[i] );
        //    }
        //    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), prev );
        //} while ( ! JSON_IsEqInfoCVars( INFO_FEXP(CURR_FUNC), prev ) );
        //JSON_CompPass = pass;

        /* emit the code for the loop                                      */
        //JSON_Emit( "for ( %c = %c;\n",                lidx, first );
        //JSON_Emit( "      ((Int)%c) <= ((Int)%c);\n", lidx, last  );
        //JSON_Emit( "      %c = (Obj)(((UInt)%c)+4) ", lidx, lidx  );
        //JSON_Emit( ") {\n" );

        /* emit the code to copy the loop index into the loop variable     */
        //JSON_Emit( "%c = %c;\n", CVAR_LVAR(var), lidx );

        /* set what we know about the loop variable                        */
        if ( JSON_HasInfoCVar( first, W_INT_SMALL_POS ) ) {
            JSON_SetInfoCVar( CVAR_LVAR(var), W_INT_SMALL_POS );
        }
        else {
            JSON_SetInfoCVar( CVAR_LVAR(var), W_INT_SMALL );
        }

        JSON_Emit(", \"do\":[");
        /* compile the body                                                */
        for ( i = 2; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
            if(i > 2) {
                JSON_Emit(",");
            }
            JSON_CompStat( ADDR_STAT(stat)[i] );
        }

        /* free the temporaries                                            */
        if ( IS_TEMP_CVAR( last  ) )  JSON_FreeTemp( TEMP_CVAR( last  ) );
        if ( IS_TEMP_CVAR( first ) )  JSON_FreeTemp( TEMP_CVAR( first ) );
        if ( IS_TEMP_CVAR( lidx  ) )  JSON_FreeTemp( TEMP_CVAR( lidx  ) );

    }
    /* handle other loops                                                  */
    else {

        //TODO:
        
        /* get the variable (initialize them first to please 'lint')       */
        if ( IS_REFLVAR( ADDR_STAT(stat)[0] )
          && ! JSON_CompGetUseHVar( LVAR_REFLVAR( ADDR_STAT(stat)[0] ) ) ) {
            var = LVAR_REFLVAR( ADDR_STAT(stat)[0] );
            vart = 'l';
        }
        else if ( IS_REFLVAR( ADDR_STAT(stat)[0] ) ) {
            var = LVAR_REFLVAR( ADDR_STAT(stat)[0] );
            vart = 'm';
        }
        else if ( T_REF_LVAR <= TNUM_EXPR( ADDR_STAT(stat)[0] )
               && TNUM_EXPR( ADDR_STAT(stat)[0] ) <= T_REF_LVAR_16
               && ! JSON_CompGetUseHVar( ADDR_EXPR( ADDR_STAT(stat)[0] )[0] ) ) {
            var = (UInt)(ADDR_EXPR( ADDR_STAT(stat)[0] )[0]);
            vart = 'l';
        }
        else if ( T_REF_LVAR <= TNUM_EXPR( ADDR_STAT(stat)[0] )
               && TNUM_EXPR( ADDR_STAT(stat)[0] ) <= T_REF_LVAR_16 ) {
            var = (UInt)(ADDR_EXPR( ADDR_STAT(stat)[0] )[0]);
            vart = 'm';
        }
        else if ( TNUM_EXPR( ADDR_STAT(stat)[0] ) == T_REF_HVAR ) {
            var = (UInt)(ADDR_EXPR( ADDR_STAT(stat)[0] )[0]);
            vart = 'h';
        }
        else /* if ( TNUM_EXPR( ADDR_STAT(stat)[0] ) == T_REF_GVAR ) */ {
            var = (UInt)(ADDR_EXPR( ADDR_STAT(stat)[0] )[0]);
            JSON_CompSetUseGVar( var, COMP_USE_GVAR_ID );
            vart = 'g';
        }

        if(vart == 'l' || vart == 'm') {
            JSON_Emit("%s", NAME_LVAR( var ));
        } else if(vart == 'g') {
          JSON_Emit( "%s", NameGVar(var));
        } else if(vart == 'h') {
          JSON_Emit( "%s", NAME_HVAR(var));
        }

        JSON_Emit( "\", \"in\":" );

        /* allocate a new temporary for the loop variable                  */
        lidx   = CVAR_TEMP( JSON_NewTemp( "lidx"   ) );
        elm    = CVAR_TEMP( JSON_NewTemp( "elm"    ) );
        islist = CVAR_TEMP( JSON_NewTemp( "islist" ) );

        /* compile and check the first and last value                      */
        list = JSON_CompExpr( ADDR_STAT(stat)[1] );

        /* SL Patch added to try and avoid a bug */
        if (IS_LVAR_CVAR(list))
          {
            CVar copylist;
            copylist = CVAR_TEMP( JSON_NewTemp( "copylist" ) );
            //JSON_Emit("%c = %c;\n",copylist, list);
            list = copylist;
          }
        /* end of SL patch */

        ///* find the invariant temp-info                                    */
        //pass = JSON_CompPass;
        //JSON_CompPass = 99;
        //prev = JSON_NewInfoCVars();
        //do {
        //    JSON_CopyInfoCVars( prev, INFO_FEXP(CURR_FUNC) );
        //    if ( vart == 'l' ) {
        //        JSON_SetInfoCVar( CVAR_LVAR(var), W_BOUND );
        //    }
        //    for ( i = 2; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
        //        JSON_CompStat( ADDR_STAT(stat)[i] );
        //    }
        //    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), prev );
        //} while ( ! JSON_IsEqInfoCVars( INFO_FEXP(CURR_FUNC), prev ) );
        //JSON_CompPass = pass;


        /* emit the code to copy the loop index into the loop variable     */
        if ( vart == 'l' ) {
            //JSON_Emit( "%c = %c;\n",
            //      CVAR_LVAR(var), elm );
        }
        else if ( vart == 'm' ) {
            //JSON_Emit( "ASS_LVAR( %d, %c );\n",
            //      JSON_GetIndxHVar(var), elm );
        }
        else if ( vart == 'h' ) {
            //JSON_Emit( "ASS_LVAR_%dUP( %d, %c );\n",
            //      JSON_GetLevlHVar(var), JSON_GetIndxHVar(var), elm );
        }
        else if ( vart == 'g' ) {
            //JSON_Emit( "AssGVar( G_%n, %c );\n",
            //      NameGVar(var), elm );
        }

        /* set what we know about the loop variable                        */
        if ( vart == 'l' ) {
            JSON_SetInfoCVar( CVAR_LVAR(var), W_BOUND );
        }

        JSON_Emit(", \"do\":[");

        /* compile the body                                                */
        for ( i = 2; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
            if(i > 2) {
                JSON_Emit(",");
            }
            JSON_CompStat( ADDR_STAT(stat)[i] );
        }

        /* free the temporaries                                            */
        if ( IS_TEMP_CVAR( list   ) )  JSON_FreeTemp( TEMP_CVAR( list   ) );
        if ( IS_TEMP_CVAR( islist ) )  JSON_FreeTemp( TEMP_CVAR( islist ) );
        if ( IS_TEMP_CVAR( elm    ) )  JSON_FreeTemp( TEMP_CVAR( elm    ) );
        if ( IS_TEMP_CVAR( lidx   ) )  JSON_FreeTemp( TEMP_CVAR( lidx   ) );
    }
  
    JSON_Emit( "]}" );
}


/****************************************************************************
**
*F  JSON_CompWhile( <stat> ) . . . . . . . . . . . . . . . . .  T_WHILE...T_WHILE3
*/
void JSON_CompWhile (
    Stat                stat )
{
    CVar                cond;           /* condition                       */
    UInt                i;              /* loop variable                   */

    /* find an invariant temp-info                                         */
    /* the emits are probably not needed                                   */
    //pass = JSON_CompPass;
    //JSON_CompPass = 99;
    ////JSON_Emit( "while ( 1 ) {\n" );
    //prev = JSON_NewInfoCVars();
    //do {
    //    JSON_CopyInfoCVars( prev, INFO_FEXP(CURR_FUNC) );
    //    cond = JSON_CompBoolExpr( ADDR_STAT(stat)[0] );
    //    //JSON_Emit( "if ( ! %c ) break;\n", cond );
    //    if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );
    //    for ( i = 1; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
    //        JSON_CompStat( ADDR_STAT(stat)[i] );
    //    }
    //    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), prev );
    //} while ( ! JSON_IsEqInfoCVars( INFO_FEXP(CURR_FUNC), prev ) );
    ////JSON_Emit( "}\n" );
    //JSON_CompPass = pass;

    JSON_Emit( "{\"type\":\"while\", \"cond\":" );

    /* emit the code for the loop                                          */
    //JSON_Emit( "while ( 1 ) {\n" );

    /* compile the condition                                               */
    cond = JSON_CompBoolExpr( ADDR_STAT(stat)[0] );
    //JSON_Emit( "if ( ! %c ) break;\n", cond );
    if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );

    JSON_Emit( ", \"do\":[" );

    /* compile the body                                                    */
    for ( i = 1; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
        if( i > 1) {
            JSON_Emit(", ");
        }
        JSON_CompStat( ADDR_STAT(stat)[i] );
    }

    /* thats it                                                            */
    JSON_Emit( "]}" );
    //JSON_Emit( "/* od */\n" );

}


/****************************************************************************
**
*F  JSON_CompRepeat( <stat> )  . . . . . . . . . . . . . . .  T_REPEAT...T_REPEAT3
*/
void JSON_CompRepeat (
    Stat                stat )
{
    CVar                cond;           /* condition                       */
    UInt                i;              /* loop variable                   */

    ///* find an invariant temp-info                                         */
    ///* the emits are probably not needed                                   */
    //pass = JSON_CompPass;
    //JSON_CompPass = 99;
    ////JSON_Emit( "do {\n" );
    //prev = JSON_NewInfoCVars();
    //do {
    //    JSON_CopyInfoCVars( prev, INFO_FEXP(CURR_FUNC) );
    //    for ( i = 1; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
    //        JSON_CompStat( ADDR_STAT(stat)[i] );
    //    }
    //    cond = JSON_CompBoolExpr( ADDR_STAT(stat)[0] );
    //    if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );
    //    JSON_MergeInfoCVars( INFO_FEXP(CURR_FUNC), prev );
    //} while ( ! JSON_IsEqInfoCVars( INFO_FEXP(CURR_FUNC), prev ) );
    //JSON_CompPass = pass;
    

    JSON_Emit("{\"type\":\"repeat\", \"do\":[");

    /* compile the body                                                    */
    for ( i = 1; i < SIZE_STAT(stat)/sizeof(Stat); i++ ) {
        if(i > 1) { JSON_Emit(",");}
        JSON_CompStat( ADDR_STAT(stat)[i] );
    }

    JSON_Emit("], \"until\":");

    /* compile the condition                                               */
    cond = JSON_CompBoolExpr( ADDR_STAT(stat)[0] );
    
    if ( IS_TEMP_CVAR( cond ) )  JSON_FreeTemp( TEMP_CVAR( cond ) );

    JSON_Emit( " }" );
}


/****************************************************************************
**
*F  JSON_CompBreak( <stat> ) . . . . . . . . . . . . . . . . . . . . . . . T_BREAK
*/
void JSON_CompBreak (
    Stat                stat )
{
    /* print a comment                                                     */
    if ( JSON_CompPass == 2 ) {
        JSON_Emit( "{\"type\":\"break\"}" );// PrintStat( stat ); //JSON_Emit( " */\n" );
    }

    //JSON_Emit( "break;\n" );
}

/****************************************************************************
**
*F  JSON_CompContinue( <stat> ) . . . . . . . . . . . . . . . . . . . . T_CONTINUE
*/
void JSON_CompContinue (
    Stat                stat )
{
        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
        JSON_Emit( "{\"type\":\"continue\"}" ); //PrintStat( stat ); //JSON_Emit( " */\n" );

    //JSON_Emit( "continue;\n" );
}


/****************************************************************************
**
*F  JSON_CompReturnObj( <stat> ) . . . . . . . . . . . . . . . . . .  T_RETURN_OBJ
*/
void JSON_CompReturnObj (
    Stat                stat )
{
    CVar                obj;            /* returned object                 */

        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( " { \"type\":\"return\", \"void\":false, \"return\":" ); 

    /* compile the expression                                              */
    obj = JSON_CompExpr( ADDR_STAT(stat)[0] );

    /* emit code to remove stack frame                                     */
    //JSON_Emit( "RES_BRK_CURR_STAT();\n" );
    //JSON_Emit( "SWITCH_TO_OLD_FRAME(oldFrame);\n" );

    /* emit code to return from function                                   */
    //JSON_Emit( "return %c;\n", obj );

    /* free the temporary                                                  */
    if ( IS_TEMP_CVAR( obj ) )  JSON_FreeTemp( TEMP_CVAR( obj ) );
    
    JSON_Emit( "} " );
}


/****************************************************************************
**
*F  JSON_CompReturnVoid( <stat> )  . . . . . . . . . . . . . . . . . T_RETURN_VOID
*/
void JSON_CompReturnVoid (
    Stat                stat )
{
    JSON_Emit("{ \"type\":\"return\", \"void\":true}");
}


/****************************************************************************
**
*F  JSON_CompAssLVar( <stat> ) . . . . . . . . . . . .  T_ASS_LVAR...T_ASS_LVAR_16
*/
void            JSON_CompAssLVar (
    Stat                stat )
{
    LVar                lvar;           /* local variable                  */
    CVar                rhs;            /* right hand side                 */

    //TODO
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"LVar\", " );

    JSON_Emit("\"right\":");
    /* compile the right hand side expression                              */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit(", \"left\":");
    /* emit the code for the assignment                                    */
    lvar = (LVar)(ADDR_STAT(stat)[0]);
    if ( JSON_CompGetUseHVar( lvar ) ) {
        //JSON_Emit( "ASS_LVAR( %d, %c );\n", JSON_GetIndxHVar(lvar), rhs );
        JSON_Emit( "\"t_%d\"", JSON_GetIndxHVar(lvar));
    }
    else {
        //JSON_Emit( "%c = %c;\n", CVAR_LVAR(lvar), rhs );
        JSON_Emit( "\"%s\"", NAME_LVAR(lvar));
//        JSON_SetInfoCVar( CVAR_LVAR(lvar), JSON_GetInfoCVar( rhs ) );
    }

    /* free the temporary                                                  */
    if ( IS_TEMP_CVAR( rhs ) )  JSON_FreeTemp( TEMP_CVAR( rhs ) );
    
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompUnbLVar( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_UNB_LVAR
*/
void JSON_CompUnbLVar (
    Stat                stat )
{
    LVar                lvar;           /* local variable                  */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbLVar\", " );

    /* emit the code for the assignment                                    */
    lvar = (LVar)(ADDR_STAT(stat)[0]);

    JSON_Emit("\"identifier\":\"%s\"", NAME_LVAR(lvar));
    if ( JSON_CompGetUseHVar( lvar ) ) {
        //JSON_Emit( "ASS_LVAR( %d, 0 );\n", JSON_GetIndxHVar(lvar) );
    }
    else {
        //JSON_Emit( "%c = 0;\n", CVAR_LVAR( lvar ) );
        JSON_SetInfoCVar( lvar, W_UNBOUND );
    }
    
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompAssHVar( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_ASS_HVAR
*/
void JSON_CompAssHVar (
    Stat                stat )
{
    HVar                hvar;           /* higher variable                 */
    CVar                rhs;            /* right hand side                 */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"HVar\", \"rhs\":" );

    /* compile the right hand side expression                              */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[1] );

    /* emit the code for the assignment                                    */
    hvar = (HVar)(ADDR_STAT(stat)[0]);
    JSON_CompSetUseHVar( hvar );
    JSON_Emit(", \"identifier\":\"%s\"", NAME_HVAR(hvar));
    //JSON_Emit( "ASS_LVAR_%dUP( %d, %c );\n",
    //       JSON_GetLevlHVar(hvar), JSON_GetIndxHVar(hvar), rhs );

    /* free the temporary                                                  */
    if ( IS_TEMP_CVAR( rhs ) )  JSON_FreeTemp( TEMP_CVAR( rhs ) );
    
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompUnbHVar( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_UNB_HVAR
*/
void JSON_CompUnbHVar (
    Stat                stat )
{
    HVar                hvar;           /* higher variable                 */

    /* print a comment                                                     */
     //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbHVar\", " );

    /* emit the code for the assignment                                    */
    hvar = (HVar)(ADDR_STAT(stat)[0]);
    JSON_CompSetUseHVar( hvar );
    JSON_Emit("\"identifier\":\"%s\"", NAME_HVAR(hvar));
    //JSON_Emit( "ASS_LVAR_%dUP( %d, 0 );\n",
    //      JSON_GetLevlHVar(hvar), JSON_GetIndxHVar(hvar) );
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompAssGVar( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_ASS_GVAR
*/
void JSON_CompAssGVar (
    Stat                stat )
{
    GVar                gvar;           /* global variable                 */
    CVar                rhs;            /* right hand side                 */

    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"GVar\", ");

    /* emit the code for the assignment                                    */
    gvar = (GVar)(ADDR_STAT(stat)[0]);
    JSON_CompSetUseGVar( gvar, COMP_USE_GVAR_ID );
    JSON_Emit( "\"gvar\":\"%s\"", NameGVar(gvar));
    JSON_Emit(", \"rightHandSide\":" );

    /* compile the right hand side expression                              */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit( "}\n" );

    /* free the temporary                                                  */
    if ( IS_TEMP_CVAR( rhs ) )  JSON_FreeTemp( TEMP_CVAR( rhs ) );
}


/****************************************************************************
**
*F  JSON_CompUnbGVar( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_UNB_GVAR
*/
void            JSON_CompUnbGVar (
    Stat                stat )
{
    GVar                gvar;           /* global variable                 */

    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbGVar\", " );
   // PrintStat( stat );

    /* emit the code for the assignment                                    */
    gvar = (GVar)(ADDR_STAT(stat)[0]);
    JSON_Emit( "\"gvar\":\"%s\"", NameGVar(gvar));
    JSON_CompSetUseGVar( gvar, COMP_USE_GVAR_ID );
    //JSON_Emit( "AssGVar( G_%n, 0 );\n", NameGVar(gvar) );
    
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompAssList( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_ASS_LIST
*/
void JSON_CompAssList (
    Stat                stat )
{
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */
    CVar                rhs;            /* right hand side                 */

    /* print a comment                                                     */
        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"list\", " );

    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"position\":");
    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_STAT(stat)[1] );
    JSON_CompCheckIntPos( pos );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    /* emit the code                                                       */
    if ( JSON_CompFastPlainLists ) {
        if ( JSON_HasInfoCVar( rhs, W_INT_SMALL ) ) {
            //JSON_Emit( "C_ASS_LIST_FPL_INTOBJ( %c, %c, %c )\n", list, pos, rhs );
        }
        else {
            //JSON_Emit( "C_ASS_LIST_FPL( %c, %c, %c )\n", list, pos, rhs );
        }
    }
    else {
        //JSON_Emit( "C_ASS_LIST( %c, %c, %c );\n", list, pos, rhs );
    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs  ) )  JSON_FreeTemp( TEMP_CVAR( rhs  ) );
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
    
    JSON_Emit( "}" );
}


/****************************************************************************
**
*F  JSON_CompAsssList( <stat> )  . . . . . . . . . . . . . . . . . . . T_ASSS_LIST
*/
void JSON_CompAsssList (
    Stat                stat )
{
    CVar                list;           /* list                            */
    CVar                poss;           /* positions                       */
    CVar                rhss;           /* right hand sides                */

        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"sList\", " );
    
    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"position\":");
    /* compile and check the position expression                           */
    poss = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand side                                         */
    rhss = JSON_CompExpr( ADDR_STAT(stat)[2] );

    /* emit the code                                                       */
    //JSON_Emit( "AsssListCheck( %c, %c, %c );\n", list, poss, rhss );

    JSON_Emit( "}" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhss ) )  JSON_FreeTemp( TEMP_CVAR( rhss ) );
    if ( IS_TEMP_CVAR( poss ) )  JSON_FreeTemp( TEMP_CVAR( poss ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
}


/****************************************************************************
**
*F  JSON_CompAssListLev( <stat> )  . . . . . . . . . . . . . . . .  T_ASS_LIST_LEV
*/
void JSON_CompAssListLev (
    Stat                stat )
{
    CVar                lists;          /* lists                           */
    CVar                pos;            /* position                        */
    CVar                rhss;           /* right hand sides                */
    Int                 level;          /* level                           */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"listLev\", " );

    JSON_Emit("\"list\":");
    /* compile the list expressions                                        */
    lists = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"position\":");
    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_STAT(stat)[1] );
    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand sides                                        */
    rhss = JSON_CompExpr( ADDR_STAT(stat)[2] );

    /* get the level                                                       */
    level = (Int)(ADDR_STAT(stat)[3]);
    JSON_Emit(", \"level\":%d", level);

    JSON_Emit( "}" );
    /* emit the code                                                       */
    //JSON_Emit( "AssListLevel( %c, %c, %c, %d );\n", lists, pos, rhss, level );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhss  ) )  JSON_FreeTemp( TEMP_CVAR( rhss  ) );
    if ( IS_TEMP_CVAR( pos   ) )  JSON_FreeTemp( TEMP_CVAR( pos   ) );
    if ( IS_TEMP_CVAR( lists ) )  JSON_FreeTemp( TEMP_CVAR( lists ) );
}


/****************************************************************************
**
*F  JSON_CompAsssListLev( <stat> ) . . . . . . . . . . . . . . . . T_ASSS_LIST_LEV
*/
void JSON_CompAsssListLev (
    Stat                stat )
{
    CVar                lists;          /* list                            */
    CVar                poss;           /* positions                       */
    CVar                rhss;           /* right hand sides                */
    Int                 level;          /* level                           */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"sListLev\", " );

    JSON_Emit("\"list\":");
    /* compile the list expressions                                        */
    lists = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"position\":");
    /* compile and check the position expression                           */
    poss = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand side                                         */
    rhss = JSON_CompExpr( ADDR_STAT(stat)[2] );

    /* get the level                                                       */
    level = (Int)(ADDR_STAT(stat)[3]);
    JSON_Emit(", \"level\":%d", level);

    /* emit the code                                                       */
    //JSON_Emit( "AsssListLevelCheck( %c, %c, %c, %d );\n",
    //      lists, poss, rhss, level );
    
    JSON_Emit( "}" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhss  ) )  JSON_FreeTemp( TEMP_CVAR( rhss ) );
    if ( IS_TEMP_CVAR( poss  ) )  JSON_FreeTemp( TEMP_CVAR( poss ) );
    if ( IS_TEMP_CVAR( lists ) )  JSON_FreeTemp( TEMP_CVAR( lists ) );
}


/****************************************************************************
**
*F  JSON_CompUnbList( <stat> ) . . . . . . . . . . . . . . . . . . . .  T_UNB_LIST
*/
void JSON_CompUnbList (
    Stat                stat )
{
    CVar                list;           /* list, left operand              */
    CVar                pos;            /* position, left operand          */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"unbList\", " );

    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"position\":");
    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_STAT(stat)[1] );
    JSON_CompCheckIntPos( pos );

    /* emit the code                                                       */
    //JSON_Emit( "C_UNB_LIST( %c, %c );\n", list, pos );
    
    JSON_Emit( "}" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
}


/****************************************************************************
**
*F  JSON_CompAssRecName( <stat> )  . . . . . . . . . . . . . . . .  T_ASS_REC_NAME
*/
void JSON_CompAssRecName (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    UInt                rnam;           /* name, left operand              */
    CVar                rhs;            /* rhs, right operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"recName\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"name\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = (UInt)(ADDR_STAT(stat)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    //JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit( "}" );

    /* emit the code for the assignment                                    */
    //JSON_Emit( "ASS_REC( %c, R_%n, %c );\n", record, NAME_RNAM(rnam), rhs );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs    ) )  JSON_FreeTemp( TEMP_CVAR( rhs    ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompAssRecExpr( <stat> )  . . . . . . . . . . . . . . . .  T_ASS_REC_EXPR
*/
void JSON_CompAssRecExpr (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    CVar                rnam;           /* name, left operand              */
    CVar                rhs;            /* rhs, right operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"recExpr\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"rnam\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit(", \"rhs\":");
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit( "}" );

    /* emit the code for the assignment                                    */
    //JSON_Emit( "ASS_REC( %c, RNamObj(%c), %c );\n", record, rnam, rhs );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs    ) )  JSON_FreeTemp( TEMP_CVAR( rhs    ) );
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompUnbRecName( <stat> )  . . . . . . . . . . . . . . . .  T_UNB_REC_NAME
*/
void JSON_CompUnbRecName (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    UInt                rnam;           /* name, left operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbRecName\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"rnam\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = (UInt)(ADDR_STAT(stat)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    /* emit the code for the assignment                                    */
    //JSON_Emit( "UNB_REC( %c, R_%n );\n", record, NAME_RNAM(rnam) );

    JSON_Emit( "}" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompUnbRecExpr( <stat> )  . . . . . . . . . . . . . . . .  T_UNB_REC_EXPR
*/
void            JSON_CompUnbRecExpr (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    CVar                rnam;           /* name, left operand              */

        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbRecExpr\", " );


    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"rnam\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = JSON_CompExpr( ADDR_STAT(stat)[1] );

    /* emit the code for the assignment                                    */
    //JSON_Emit( "UNB_REC( %c, RNamObj(%c) );\n", record, rnam );

    JSON_Emit( "}" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompAssPosObj( <stat> ) . . . . . . . . . . . . . . . . . .  T_ASS_POSOBJ
*/
void JSON_CompAssPosObj (
    Stat                stat )
{
    CVar                list;           /* list                            */
    CVar                pos;            /* position                        */
    CVar                rhs;            /* right hand side                 */

        //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"posObj\", " );

    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"pos\":");
    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_STAT(stat)[1] );
//    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit(", \"rightHandSide\":");
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit( "}" );

    /* emit the code                                                       */
//    if ( JSON_HasInfoCVar( rhs, W_INT_SMALL ) ) {
        //JSON_Emit( "C_ASS_POSOBJ_INTOBJ( %c, %i, %c )\n", list, pos, rhs );
//    }
//    else {
        //JSON_Emit( "C_ASS_POSOBJ( %c, %i, %c )\n", list, pos, rhs );
//    }

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs  ) )  JSON_FreeTemp( TEMP_CVAR( rhs  ) );
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
}



/****************************************************************************
**
*F  JSON_CompAsssPosObj( <stat> )  . . . . . . . . . . . . . . . . . T_ASSS_POSOBJ
*/
void JSON_CompAsssPosObj (
    Stat                stat )
{
    CVar                list;           /* list                            */
    CVar                poss;           /* positions                       */
    CVar                rhss;           /* right hand sides                */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"sPosObj\", " );

    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"poss\":");
    /* compile and check the position expression                           */
    poss = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit(", \"rhss\":");
    /* compile the right hand side                                         */
    rhss = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit( "}" );

    /* emit the code                                                       */
    //JSON_Emit( "AsssPosObjCheck( %c, %c, %c );\n", list, poss, rhss );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhss ) )  JSON_FreeTemp( TEMP_CVAR( rhss ) );
    if ( IS_TEMP_CVAR( poss ) )  JSON_FreeTemp( TEMP_CVAR( poss ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
}


/****************************************************************************
**
*F  JSON_CompAssPosObjLev( <stat> )  . . . . . . . . . . . . . .  T_ASS_POSOBJ_LEV
*/
void JSON_CompAssPosObjLev (
    Stat                stat )
{
    //JSON_Emit( "CANNOT COMPILE STATEMENT OF TNUM %d;\n", TNUM_STAT(stat) );
}


/****************************************************************************
**
*F  JSON_CompAsssPosObjLev( <stat> ) . . . . . . . . . . . . . . T_ASSS_POSOBJ_LEV
*/
void JSON_CompAsssPosObjLev (
    Stat                stat )
{
    //JSON_Emit( "CANNOT COMPILE STATEMENT OF TNUM %d;\n", TNUM_STAT(stat) );
}


/****************************************************************************
**
*F  JSON_CompUnbPosObj( <stat> ) . . . . . . . . . . . . . . . . . .  T_UNB_POSOBJ
*
u
*/
void JSON_CompUnbPosObj (
    Stat                stat )
{
    CVar                list;           /* list, left operand              */
    CVar                pos;            /* position, left operand          */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"UnbPosObj\", " );

    JSON_Emit("\"list\":");
    /* compile the list expression                                         */
    list = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"pos\":");
    /* compile and check the position expression                           */
    pos = JSON_CompExpr( ADDR_STAT(stat)[1] );
    JSON_CompCheckIntSmallPos( pos );

    JSON_Emit( "}" );

    /* emit the code                                                       */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_POSOBJ ) {\n", list );
    //JSON_Emit( "if ( %i <= SIZE_OBJ(%c)/sizeof(Obj)-1 ) {\n", pos, list );
    //JSON_Emit( "SET_ELM_PLIST( %c, %i, 0 );\n", list, pos );
    //JSON_Emit( "}\n}\n" );
    //JSON_Emit( "else {\n" );
    //JSON_Emit( "UNB_LIST( %c, %i );\n", list, pos );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( pos  ) )  JSON_FreeTemp( TEMP_CVAR( pos  ) );
    if ( IS_TEMP_CVAR( list ) )  JSON_FreeTemp( TEMP_CVAR( list ) );
}


/****************************************************************************
**
*F  JSON_CompAssComObjName( <stat> ) . . . . . . . . . . . . . . T_ASS_COMOBJ_NAME
*/
void JSON_CompAssComObjName (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    UInt                rnam;           /* name, left operand              */
    CVar                rhs;            /* rhs, right operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"ComObjName\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"rnam\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = (UInt)(ADDR_STAT(stat)[1]);
    JSON_Emit("\"%s\"", NAME_RNAM(rnam));
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    JSON_Emit(", \"rhs\":");
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit( "}" );

    /* emit the code for the assignment                                    */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "AssPRec( %c, R_%n, %c );\n", record, NAME_RNAM(rnam), rhs );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "AssARecord( %c, R_%n, %c );\n", record, NAME_RNAM(rnam), rhs );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "ASS_REC( %c, R_%n, %c );\n", record, NAME_RNAM(rnam), rhs );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs    ) )  JSON_FreeTemp( TEMP_CVAR( rhs    ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompAssComObjExpr( <stat> ) . . . . . . . . . . . . . . T_ASS_COMOBJ_EXPR
*/
void JSON_CompAssComObjExpr (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    CVar                rnam;           /* name, left operand              */
    CVar                rhs;            /* rhs, right operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );

    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"ComObjExpr\", " );

    JSON_Emit( "\"record\":" );
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit( ", \"name\":" );
    /* get the name (stored immediately in the statement)                  */
    rnam = JSON_CompExpr( ADDR_STAT(stat)[1] );

    JSON_Emit( ", \"rhs\":" );
    /* compile the right hand side                                         */
    rhs = JSON_CompExpr( ADDR_STAT(stat)[2] );

    JSON_Emit("}");

    /* emit the code for the assignment                                    */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "AssPRec( %c, RNamObj(%c), %c );\n", record, rnam, rhs );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "AssARecord( %c, RNamObj(%c), %c );\n", record, rnam, rhs );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "ASS_REC( %c, RNamObj(%c), %c );\n", record, rnam, rhs );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rhs    ) )  JSON_FreeTemp( TEMP_CVAR( rhs    ) );
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompUnbComObjName( <stat> ) . . . . . . . . . . . . . . T_UNB_COMOBJ_NAME
*/
void JSON_CompUnbComObjName (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    UInt                rnam;           /* name, left operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbComObjName\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"name\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = (UInt)(ADDR_STAT(stat)[1]);
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    JSON_Emit("\"%s\"", NAME_RNAM(rnam));

    JSON_Emit("}");

    /* emit the code for the assignment                                    */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "UnbPRec( %c, R_%n );\n", record, NAME_RNAM(rnam) );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "UnbARecord( %c, R_%n );\n", record, NAME_RNAM(rnam) );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "UNB_REC( %c, R_%n );\n", record, NAME_RNAM(rnam) );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}


/****************************************************************************
**
*F  JSON_CompUnbComObjExpr( <stat> ) . . . . . . . . . . . . . . T_UNB_COMOBJ_EXPR
*/
void JSON_CompUnbComObjExpr (
    Stat                stat )
{
    CVar                record;         /* record, left operand            */
    UInt                rnam;           /* name, left operand              */

    //JSON_Emit( "\n/* " ); PrintStat( stat ); //JSON_Emit( " */\n" );
    JSON_Emit( "{\"type\":\"assign\", \"subtype\":\"UnbComObjExpr\", " );

    JSON_Emit("\"record\":");
    /* compile the record expression                                       */
    record = JSON_CompExpr( ADDR_STAT(stat)[0] );

    JSON_Emit(", \"name\":");
    /* get the name (stored immediately in the statement)                  */
    rnam = JSON_CompExpr( ADDR_STAT(stat)[1] );
    JSON_CompSetUseRNam( rnam, COMP_USE_RNAM_ID );

    //JSON_Emit("\"%s\"", NAME_RNAM(rnam));

    JSON_Emit("}");

    /* emit the code for the assignment                                    */
    //JSON_Emit( "if ( TNUM_OBJ(%c) == T_COMOBJ ) {\n", record );
    //JSON_Emit( "UnbPRec( %c, RNamObj(%c) );\n", record, rnam );
    //JSON_Emit( "#ifdef HPCGAP\n" );
    //JSON_Emit( "} else if ( TNUM_OBJ(%c) == T_ACOMOBJ ) {\n", record );
    //JSON_Emit( "UnbARecord( %c, RNamObj(%c) );\n", record, rnam );
    //JSON_Emit( "#endif\n" );
    //JSON_Emit( "}\nelse {\n" );
    //JSON_Emit( "UNB_REC( %c, RNamObj(%c) );\n", record, rnam );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( rnam   ) )  JSON_FreeTemp( TEMP_CVAR( rnam   ) );
    if ( IS_TEMP_CVAR( record ) )  JSON_FreeTemp( TEMP_CVAR( record ) );
}

/****************************************************************************
**
*F  JSON_CompEmpty( <stat> )  . . . . . . . . . . . . . . . . . . . . . . . T_EMPY
*/
void JSON_CompEmpty (
    Stat                stat )
{
  //JSON_Emit("\n/* ; */\n");
  JSON_Emit("{\"type\":\"empty\"}");
}
  
/****************************************************************************
**
*F  JSON_CompInfo( <stat> )  . . . . . . . . . . . . . . . . . . . . . . .  T_INFO
*/
void JSON_CompInfo (
    Stat                stat )
{
    CVar                tmp;
    CVar                sel;
    CVar                lev;
    CVar                lst;
    Int                 narg;
    Int                 i;


    JSON_Emit("{\"type\":\"Info\", \"sel\":");

    sel = JSON_CompExpr( ARGI_INFO( stat, 1 ) );

    JSON_Emit(", \"lev\":");

    lev = JSON_CompExpr( ARGI_INFO( stat, 2 ) );

    JSON_Emit(", \"args\":[");

    lst = CVAR_TEMP( JSON_NewTemp( "lst" ) );
    tmp = CVAR_TEMP( JSON_NewTemp( "tmp" ) );
    if ( IS_TEMP_CVAR( tmp ) )  JSON_FreeTemp( TEMP_CVAR( tmp ) );
    narg = NARG_SIZE_INFO(SIZE_STAT(stat))-2;
    for ( i = 1;  i <= narg;  i++ ) {
        tmp = JSON_CompExpr( ARGI_INFO( stat, i+2 ) );
        if( i < narg) {
            JSON_Emit(",");
        }
        if ( IS_TEMP_CVAR( tmp ) )  JSON_FreeTemp( TEMP_CVAR( tmp ) );
    }

    JSON_Emit("]}");

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( lst ) )  JSON_FreeTemp( TEMP_CVAR( lst ) );
    if ( IS_TEMP_CVAR( lev ) )  JSON_FreeTemp( TEMP_CVAR( lev ) );
    if ( IS_TEMP_CVAR( sel ) )  JSON_FreeTemp( TEMP_CVAR( sel ) );
}


/****************************************************************************
**
*F  JSON_CompAssert2( <stat> ) . . . . . . . . . . . . . . . . . .  T_ASSERT_2ARGS
*/
void JSON_CompAssert2 (
    Stat                stat )
{
    CVar                lev;            /* the level                       */
    CVar                cnd;            /* the condition                   */

    JSON_Emit("{\"type\":\"Assert\", \"subtype\":\"Assert2\", ");
    JSON_Emit("\"level\":");
    lev = JSON_CompExpr( ADDR_STAT(stat)[0] );
    JSON_Emit(", \"condition\":");
    cnd = JSON_CompBoolExpr( ADDR_STAT(stat)[1] );
    JSON_Emit("}");
    
    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( cnd ) )  JSON_FreeTemp( TEMP_CVAR( cnd ) );
    if ( IS_TEMP_CVAR( lev ) )  JSON_FreeTemp( TEMP_CVAR( lev ) );
}


/****************************************************************************
**
*F  JSON_CompAssert3( <stat> ) . . . . . . . . . . . . . . . . . .  T_ASSERT_3ARGS
*/
void JSON_CompAssert3 (
    Stat                stat )
{
    CVar                lev;            /* the level                       */
    CVar                cnd;            /* the condition                   */
    CVar                msg;            /* the message                     */

    JSON_Emit("{\"type\":\"Assert\", \"subtype\":\"Assert3\", ");
    JSON_Emit("\"level\":");

    //JSON_Emit( "\n/* Assert( ... ); */\n" );
    lev = JSON_CompExpr( ADDR_STAT(stat)[0] );
    //JSON_Emit( "if ( ! LT(CurrentAssertionLevel, %c) ) {\n", lev );
    JSON_Emit(", \"condition\":");
    cnd = JSON_CompBoolExpr( ADDR_STAT(stat)[1] );
    //JSON_Emit( "if ( ! %c ) {\n", cnd );
    JSON_Emit(", \"message\":");
    msg = JSON_CompExpr( ADDR_STAT(stat)[2] );
    JSON_Emit("}");
    //JSON_Emit( "if ( %c != (Obj)(UInt)0 )", msg );
    //JSON_Emit( "{\n if ( IS_STRING_REP ( %c ) )\n", msg);
    //JSON_Emit( "   PrintString1( %c);\n else\n   PrintObj(%c);\n}\n", msg, msg );
    //JSON_Emit( "}\n" );
    //JSON_Emit( "}\n" );

    /* free the temporaries                                                */
    if ( IS_TEMP_CVAR( msg ) )  JSON_FreeTemp( TEMP_CVAR( msg ) );
    if ( IS_TEMP_CVAR( cnd ) )  JSON_FreeTemp( TEMP_CVAR( cnd ) );
    if ( IS_TEMP_CVAR( lev ) )  JSON_FreeTemp( TEMP_CVAR( lev ) );
}



/****************************************************************************
**

*F * * * * * * * * * * * * * * start compiling  * * * * * * * * * * * * * * *
*/


/****************************************************************************
**

*F  JSON_CompFunc( <func> )  . . . . . . . . . . . . . . . . .  compile a function
**
**  'JSON_CompFunc' compiles the function <func>, i.e., it emits  the code for the
**  handler of the function <func> and the handlers of all its subfunctions.
*/
Obj JSON_CompFunctions;
Int JSON_CompFunctionsNr;

void JSON_CompFunc (
    Obj                 func )
{
    Bag                 info;           /* info bag for this function      */
    Int                 narg;           /* number of arguments             */
    Int                 nloc;           /* number of locals                */
    Bag                 oldFrame;       /* old frame                       */
    Int                 i;              /* loop variable                   */

    /* get the number of arguments and locals                              */
    narg = NARG_FUNC(func);
    if (narg < 0) {
        narg = -narg;
    }
    nloc = NLOC_FUNC(func);

    JSON_Emit("{ \"type\":\"function\", \"param\":[");
  
    /* in the first pass allocate the info bag                             */
    if ( JSON_CompPass == 1 ) {
        JSON_CompFunctionsNr++;
        GROW_PLIST(    JSON_CompFunctions, JSON_CompFunctionsNr );
        SET_ELM_PLIST( JSON_CompFunctions, JSON_CompFunctionsNr, func );
        SET_LEN_PLIST( JSON_CompFunctions, JSON_CompFunctionsNr );
        CHANGED_BAG(   JSON_CompFunctions );

        info = NewBag( T_STRING, SIZE_INFO(narg+nloc,8) );
        NEXT_INFO(info)  = INFO_FEXP( CURR_FUNC );
        NR_INFO(info)    = JSON_CompFunctionsNr;
        NLVAR_INFO(info) = narg + nloc;
        NHVAR_INFO(info) = 0;
        NTEMP_INFO(info) = 0;
        NLOOP_INFO(info) = 0;

        INFO_FEXP(func) = info;
        CHANGED_BAG(func);
    }

    /* switch to this function (so that 'ADDR_STAT' and 'ADDR_EXPR' work)  */
    SWITCH_TO_NEW_LVARS( func, narg, nloc, oldFrame );

    /* get the info bag                                                    */
    info = INFO_FEXP( CURR_FUNC );
    
//    Int pass = JSON_CompPass;
//    JSON_CompPass = 99;
//    fexs = FEXS_FUNC(func);
//    for ( i = 1;  i <= LEN_PLIST(fexs);  i++ ) {
//        JSON_CompFunc( ELM_PLIST( fexs, i ) );
//    }
//    JSON_CompPass = pass;

    for ( i = 1; i <= narg; i++ ) {
        JSON_Emit("\"%s\"", NAME_LVAR(i));
        if(i < narg) {
          JSON_Emit(", ");
        }
    }

    JSON_Emit("], \"body\":");
 
    /* compile the body                                                    */
    JSON_CompStat( FIRST_STAT_CURR_FUNC );

    /* we know all the arguments have values                               */
    for ( i = 1; i <= narg; i++ ) {
        JSON_SetInfoCVar( CVAR_LVAR(i), W_BOUND );
    }
    for ( i = narg+1; i <= narg+nloc; i++ ) {
        JSON_SetInfoCVar( CVAR_LVAR(i), W_UNBOUND );
    }
    
    /* switch back to old frame                                            */
    SWITCH_TO_OLD_LVARS( oldFrame );
  
    JSON_Emit("}");
}


/****************************************************************************
**
*F  JSON_CompileFunc( <output>, <func>, <name> ) . . . compile
*/
Int JSON_CompileFunc (
    Char *              output,
    Obj                 func,
    Char *              name,
    Int                 magic1,
    Char *              magic2 )
{
    UInt                col;

    /* open the output file                                                */
    if ( ! OpenOutput( output ) ) {
        return 0;
    }

    json = fopen(output, "w");

    if(!json) {
      printf("JSON file couldn't be openend.\n");
      printf("ERROR: %s\n", strerror(errno));
      exit(EXIT_FAILURE);
    }

    col = SyNrCols;
    SyNrCols = 255;


    /* create 'JSON_CompInfoGVar' and 'JSON_CompInfoRNam'                            */
    JSON_CompInfoGVar = NewBag( T_STRING, sizeof(UInt) * 1024 );
    JSON_CompInfoRNam = NewBag( T_STRING, sizeof(UInt) * 1024 );

    /* create the list to collection the function expressions              */
    JSON_CompFunctionsNr = 0;
    JSON_CompFunctions = NEW_PLIST( T_PLIST, 8 );
    SET_LEN_PLIST( JSON_CompFunctions, 0 );

    /* first collect information about variables                           */
    JSON_CompPass = 1;
    JSON_CompFunc( func );

    /* ok, lets emit some code now                                         */
    JSON_CompPass = 2;

    /********** create a JSON structure **********/

    /* now compile the handlers                                            */
    JSON_CompFunc( func );

    fclose(json);

    /* close the output file                                               */
    SyNrCols = col;
    CloseOutput();

    
    /* return success                                                      */
    return JSON_CompFunctionsNr;
}


/****************************************************************************
**
*F  JSON_FuncCOMPILE_FUNC( <self>, <output>, <func>, <name>, <magic1>, <magic2> )
*/
Obj JSON_FuncCOMPILE_FUNC (
    Obj                 self,
    Obj                 arg )
{
    Obj                 output;
    Obj                 func;
    Obj                 name;
    Obj                 magic1;
    Obj                 magic2;
    Int                 nr;
    Int                 len;

    /* unravel the arguments                                               */
    len = LEN_LIST(arg); 
    if ( len < 5 ) {
        ErrorQuit( "usage: JSON_COMPILE_FUNC( <output>, <func>, <name>, %s",
                   (Int)"<magic1>, <magic2>, ... )", 0 );
        return 0;
    }
    output = ELM_LIST( arg, 1 );
    func   = ELM_LIST( arg, 2 );
    name   = ELM_LIST( arg, 3 );
    magic1 = ELM_LIST( arg, 4 );
    magic2 = ELM_LIST( arg, 5 );
    
    /* compile the function                                                */
    nr = JSON_CompileFunc(
        (char*) CHARS_STRING(output), func, (char*) CHARS_STRING(name),
        INT_INTOBJ(magic1), (char*) CHARS_STRING(magic2) );


    /* return the result                                                   */
    return INTOBJ_INT(nr);
}


/****************************************************************************
**

*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * *
*/

/****************************************************************************
**

*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

    { "JSON_COMPILE_FUNC", -1, "arg",
      JSON_FuncCOMPILE_FUNC, "src/JSON_compiler.c:JSON_COMPILE_FUNC" },

    { 0 }

};


/****************************************************************************
**

*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel (
    StructInitInfo *    module )
{
    Int                 i;              /* loop variable                   */

    JSON_CompFastIntArith = 1;
    JSON_CompFastListFuncs = 1;
    JSON_CompFastPlainLists = 1;
    JSON_CompCheckTypes = 1;
    JSON_CompCheckListElements = 1;
    JSON_CompCheckPosObjElements = 0;
    JSON_CompPass = 0;
    
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    /* announce the global variables                                       */
    InitGlobalBag( &JSON_CompInfoGVar,  "src/compiler.c:JSON_CompInfoGVar"  );
    InitGlobalBag( &JSON_CompInfoRNam,  "src/compiler.c:JSON_CompInfoRNam"  );
    InitGlobalBag( &JSON_CompFunctions, "src/compiler.c:JSON_CompFunctions" );

    /* enter the expression compilers into the table                       */
    for ( i = 0; i < 256; i++ ) {
        JSON_CompExprFuncs[ i ] = JSON_CompUnknownExpr;
    }

    JSON_CompExprFuncs[ T_FUNCCALL_0ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_1ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_2ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_3ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_4ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_5ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_6ARGS  ] = JSON_CompFunccall0to6Args;
    JSON_CompExprFuncs[ T_FUNCCALL_XARGS  ] = JSON_CompFunccallXArgs;
    JSON_CompExprFuncs[ T_FUNC_EXPR       ] = JSON_CompFuncExpr;

    JSON_CompExprFuncs[ T_OR              ] = JSON_CompOr;
    JSON_CompExprFuncs[ T_AND             ] = JSON_CompAnd;
    JSON_CompExprFuncs[ T_NOT             ] = JSON_CompNot;
    JSON_CompExprFuncs[ T_EQ              ] = JSON_CompEq;
    JSON_CompExprFuncs[ T_NE              ] = JSON_CompNe;
    JSON_CompExprFuncs[ T_LT              ] = JSON_CompLt;
    JSON_CompExprFuncs[ T_GE              ] = JSON_CompGe;
    JSON_CompExprFuncs[ T_GT              ] = JSON_CompGt;
    JSON_CompExprFuncs[ T_LE              ] = JSON_CompLe;
    JSON_CompExprFuncs[ T_IN              ] = JSON_CompIn;

    JSON_CompExprFuncs[ T_SUM             ] = JSON_CompSum;
    JSON_CompExprFuncs[ T_AINV            ] = JSON_CompAInv;
    JSON_CompExprFuncs[ T_DIFF            ] = JSON_CompDiff;
    JSON_CompExprFuncs[ T_PROD            ] = JSON_CompProd;
    JSON_CompExprFuncs[ T_INV             ] = JSON_CompInv;
    JSON_CompExprFuncs[ T_QUO             ] = JSON_CompQuo;
    JSON_CompExprFuncs[ T_MOD             ] = JSON_CompMod;
    JSON_CompExprFuncs[ T_POW             ] = JSON_CompPow;

    JSON_CompExprFuncs[ T_INTEXPR         ] = JSON_CompIntExpr;
    JSON_CompExprFuncs[ T_INT_EXPR        ] = JSON_CompIntExpr;
    JSON_CompExprFuncs[ T_TRUE_EXPR       ] = JSON_CompTrueExpr;
    JSON_CompExprFuncs[ T_FALSE_EXPR      ] = JSON_CompFalseExpr;
    JSON_CompExprFuncs[ T_CHAR_EXPR       ] = JSON_CompCharExpr;
    JSON_CompExprFuncs[ T_PERM_EXPR       ] = JSON_CompPermExpr;
    JSON_CompExprFuncs[ T_PERM_CYCLE      ] = JSON_CompUnknownExpr;
    JSON_CompExprFuncs[ T_LIST_EXPR       ] = JSON_CompListExpr;
    JSON_CompExprFuncs[ T_LIST_TILD_EXPR  ] = JSON_CompListTildeExpr;
    JSON_CompExprFuncs[ T_RANGE_EXPR      ] = JSON_CompRangeExpr;
    JSON_CompExprFuncs[ T_STRING_EXPR     ] = JSON_CompStringExpr;
    JSON_CompExprFuncs[ T_REC_EXPR        ] = JSON_CompRecExpr;
    JSON_CompExprFuncs[ T_REC_TILD_EXPR   ] = JSON_CompRecTildeExpr;

    JSON_CompExprFuncs[ T_REFLVAR         ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR        ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_01     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_02     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_03     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_04     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_05     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_06     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_07     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_08     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_09     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_10     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_11     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_12     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_13     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_14     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_15     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_REF_LVAR_16     ] = JSON_CompRefLVar;
    JSON_CompExprFuncs[ T_ISB_LVAR        ] = JSON_CompIsbLVar;
    JSON_CompExprFuncs[ T_REF_HVAR        ] = JSON_CompRefHVar;
    JSON_CompExprFuncs[ T_ISB_HVAR        ] = JSON_CompIsbHVar;
    JSON_CompExprFuncs[ T_REF_GVAR        ] = JSON_CompRefGVar;
    JSON_CompExprFuncs[ T_ISB_GVAR        ] = JSON_CompIsbGVar;

    JSON_CompExprFuncs[ T_ELM_LIST        ] = JSON_CompElmList;
    JSON_CompExprFuncs[ T_ELMS_LIST       ] = JSON_CompElmsList;
    JSON_CompExprFuncs[ T_ELM_LIST_LEV    ] = JSON_CompElmListLev;
    JSON_CompExprFuncs[ T_ELMS_LIST_LEV   ] = JSON_CompElmsListLev;
    JSON_CompExprFuncs[ T_ISB_LIST        ] = JSON_CompIsbList;
    JSON_CompExprFuncs[ T_ELM_REC_NAME    ] = JSON_CompElmRecName;
    JSON_CompExprFuncs[ T_ELM_REC_EXPR    ] = JSON_CompElmRecExpr;
    JSON_CompExprFuncs[ T_ISB_REC_NAME    ] = JSON_CompIsbRecName;
    JSON_CompExprFuncs[ T_ISB_REC_EXPR    ] = JSON_CompIsbRecExpr;

    JSON_CompExprFuncs[ T_ELM_POSOBJ      ] = JSON_CompElmPosObj;
    JSON_CompExprFuncs[ T_ELMS_POSOBJ     ] = JSON_CompElmsPosObj;
    JSON_CompExprFuncs[ T_ELM_POSOBJ_LEV  ] = JSON_CompElmPosObjLev;
    JSON_CompExprFuncs[ T_ELMS_POSOBJ_LEV ] = JSON_CompElmsPosObjLev;
    JSON_CompExprFuncs[ T_ISB_POSOBJ      ] = JSON_CompIsbPosObj;
    JSON_CompExprFuncs[ T_ELM_COMOBJ_NAME ] = JSON_CompElmComObjName;
    JSON_CompExprFuncs[ T_ELM_COMOBJ_EXPR ] = JSON_CompElmComObjExpr;
    JSON_CompExprFuncs[ T_ISB_COMOBJ_NAME ] = JSON_CompIsbComObjName;
    JSON_CompExprFuncs[ T_ISB_COMOBJ_EXPR ] = JSON_CompIsbComObjExpr;

    JSON_CompExprFuncs[ T_FUNCCALL_OPTS   ] = JSON_CompFunccallOpts;
    
    /* enter the boolean expression compilers into the table               */
    for ( i = 0; i < 256; i++ ) {
        JSON_CompBoolExprFuncs[ i ] = JSON_CompUnknownBool;
    }

    JSON_CompBoolExprFuncs[ T_OR              ] = JSON_CompOrBool;
    JSON_CompBoolExprFuncs[ T_AND             ] = JSON_CompAndBool;
    JSON_CompBoolExprFuncs[ T_NOT             ] = JSON_CompNotBool;
    JSON_CompBoolExprFuncs[ T_EQ              ] = JSON_CompEqBool;
    JSON_CompBoolExprFuncs[ T_NE              ] = JSON_CompNeBool;
    JSON_CompBoolExprFuncs[ T_LT              ] = JSON_CompLtBool;
    JSON_CompBoolExprFuncs[ T_GE              ] = JSON_CompGeBool;
    JSON_CompBoolExprFuncs[ T_GT              ] = JSON_CompGtBool;
    JSON_CompBoolExprFuncs[ T_LE              ] = JSON_CompLeBool;
    JSON_CompBoolExprFuncs[ T_IN              ] = JSON_CompInBool;

    /* enter the statement compilers into the table                        */
    for ( i = 0; i < 256; i++ ) {
        JSON_CompStatFuncs[ i ] = JSON_CompUnknownStat;
    }

    JSON_CompStatFuncs[ T_PROCCALL_0ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_1ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_2ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_3ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_4ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_5ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_6ARGS  ] = JSON_CompProccall0to6Args;
    JSON_CompStatFuncs[ T_PROCCALL_XARGS  ] = JSON_CompProccallXArgs;

    JSON_CompStatFuncs[ T_SEQ_STAT        ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT2       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT3       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT4       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT5       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT6       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_SEQ_STAT7       ] = JSON_CompSeqStat;
    JSON_CompStatFuncs[ T_IF              ] = JSON_CompIf;
    JSON_CompStatFuncs[ T_IF_ELSE         ] = JSON_CompIf;
    JSON_CompStatFuncs[ T_IF_ELIF         ] = JSON_CompIf;
    JSON_CompStatFuncs[ T_IF_ELIF_ELSE    ] = JSON_CompIf;
    JSON_CompStatFuncs[ T_FOR             ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_FOR2            ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_FOR3            ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_FOR_RANGE       ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_FOR_RANGE2      ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_FOR_RANGE3      ] = JSON_CompFor;
    JSON_CompStatFuncs[ T_WHILE           ] = JSON_CompWhile;
    JSON_CompStatFuncs[ T_WHILE2          ] = JSON_CompWhile;
    JSON_CompStatFuncs[ T_WHILE3          ] = JSON_CompWhile;
    JSON_CompStatFuncs[ T_REPEAT          ] = JSON_CompRepeat;
    JSON_CompStatFuncs[ T_REPEAT2         ] = JSON_CompRepeat;
    JSON_CompStatFuncs[ T_REPEAT3         ] = JSON_CompRepeat;
    JSON_CompStatFuncs[ T_BREAK           ] = JSON_CompBreak;
    JSON_CompStatFuncs[ T_CONTINUE        ] = JSON_CompContinue;
    JSON_CompStatFuncs[ T_RETURN_OBJ      ] = JSON_CompReturnObj;
    JSON_CompStatFuncs[ T_RETURN_VOID     ] = JSON_CompReturnVoid;

    JSON_CompStatFuncs[ T_ASS_LVAR        ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_01     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_02     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_03     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_04     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_05     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_06     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_07     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_08     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_09     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_10     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_11     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_12     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_13     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_14     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_15     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_ASS_LVAR_16     ] = JSON_CompAssLVar;
    JSON_CompStatFuncs[ T_UNB_LVAR        ] = JSON_CompUnbLVar;
    JSON_CompStatFuncs[ T_ASS_HVAR        ] = JSON_CompAssHVar;
    JSON_CompStatFuncs[ T_UNB_HVAR        ] = JSON_CompUnbHVar;
    JSON_CompStatFuncs[ T_ASS_GVAR        ] = JSON_CompAssGVar;
    JSON_CompStatFuncs[ T_UNB_GVAR        ] = JSON_CompUnbGVar;

    JSON_CompStatFuncs[ T_ASS_LIST        ] = JSON_CompAssList;
    JSON_CompStatFuncs[ T_ASSS_LIST       ] = JSON_CompAsssList;
    JSON_CompStatFuncs[ T_ASS_LIST_LEV    ] = JSON_CompAssListLev;
    JSON_CompStatFuncs[ T_ASSS_LIST_LEV   ] = JSON_CompAsssListLev;
    JSON_CompStatFuncs[ T_UNB_LIST        ] = JSON_CompUnbList;
    JSON_CompStatFuncs[ T_ASS_REC_NAME    ] = JSON_CompAssRecName;
    JSON_CompStatFuncs[ T_ASS_REC_EXPR    ] = JSON_CompAssRecExpr;
    JSON_CompStatFuncs[ T_UNB_REC_NAME    ] = JSON_CompUnbRecName;
    JSON_CompStatFuncs[ T_UNB_REC_EXPR    ] = JSON_CompUnbRecExpr;

    JSON_CompStatFuncs[ T_ASS_POSOBJ      ] = JSON_CompAssPosObj;
    JSON_CompStatFuncs[ T_ASSS_POSOBJ     ] = JSON_CompAsssPosObj;
    JSON_CompStatFuncs[ T_ASS_POSOBJ_LEV  ] = JSON_CompAssPosObjLev;
    JSON_CompStatFuncs[ T_ASSS_POSOBJ_LEV ] = JSON_CompAsssPosObjLev;
    JSON_CompStatFuncs[ T_UNB_POSOBJ      ] = JSON_CompUnbPosObj;
    JSON_CompStatFuncs[ T_ASS_COMOBJ_NAME ] = JSON_CompAssComObjName;
    JSON_CompStatFuncs[ T_ASS_COMOBJ_EXPR ] = JSON_CompAssComObjExpr;
    JSON_CompStatFuncs[ T_UNB_COMOBJ_NAME ] = JSON_CompUnbComObjName;
    JSON_CompStatFuncs[ T_UNB_COMOBJ_EXPR ] = JSON_CompUnbComObjExpr;

    JSON_CompStatFuncs[ T_INFO            ] = JSON_CompInfo;
    JSON_CompStatFuncs[ T_ASSERT_2ARGS    ] = JSON_CompAssert2;
    JSON_CompStatFuncs[ T_ASSERT_3ARGS    ] = JSON_CompAssert3;
    JSON_CompStatFuncs[ T_EMPTY           ] = JSON_CompEmpty;

    JSON_CompStatFuncs[ T_PROCCALL_OPTS   ] = JSON_CompProccallOpts;
    /* return success                                                      */
    return 0;
}


/****************************************************************************
**
*F  PostRestore( <module> ) . . . . . . . . . . . . . after restore workspace
*/
static Int PostRestore (
    StructInitInfo *    module )
{
    /* get the identifiers of 'Length' and 'Add' (for inlining)            */
    G_Length = GVarName( "Length" );
    G_Add    = GVarName( "Add"    );

    /* return success                                                      */
    return 0;
}


/****************************************************************************
**
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary (
    StructInitInfo *    module )
{
    /* init filters and functions                                          */
    InitGVarFuncsFromTable( GVarFuncs );

    /* return success                                                      */
    return PostRestore( module );
}


/****************************************************************************
**
*F  InitInfoJSON_Compiler() . . . . . . . . . . . . . . .  table of init functions
*/
static StructInitInfo module = {
    MODULE_BUILTIN,                     /* type                           */
    "JSON_compiler",                         /* name                           */
    0,                                  /* revision entry of c file       */
    0,                                  /* revision entry of h file       */
    0,                                  /* version                        */
    0,                                  /* crc                            */
    InitKernel,                         /* initKernel                     */
    InitLibrary,                        /* initLibrary                    */
    0,                                  /* checkInit                      */
    0,                                  /* preSave                        */
    0,                                  /* postSave                       */
    PostRestore                         /* postRestore                    */
};

StructInitInfo * InitInfoJSON_Compiler ( void )
{
    return &module;
}


/****************************************************************************
**

*E  JSON_compiler.c  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
*/

