#prototype of the function that is supposed to annotate type information
#for the Math in the Middle project as part of OpenDreamKit

BIND_GLOBAL( "MitM_DeclareConstructor",
  function( name, inputFilters, outputFilter )
    DeclareOperation(name, inputFilters); 
  end );

BIND_GLOBAL( "MitM_InstallMethod",
  function( arg... )
     local list;
     list := arg{[1..(Length(arg)-1)]};
     Append(list,
     [(function(local_arg...)
      local G;
      CallFuncList(arg[(Length(arg))], (local_arg));
      #store arg somewhere as attribute of G
      #DeclareAttribute
      return G;
     end)]);
     CallFuncList(InstallMethod, list);
  end );

MitM_DeclareConstructor( "MitM_GroupWithGenerators",
    [IsCollection],
    IsGroup and IsAttributeStoringRep and 
    HasGeneratorsOfMagmaWithInverses);

MitM_DeclareConstructor( "MitM_GroupWithGenerators",
    [ IsCollection, IsMultiplicativeElementWithInverse ],
    IsGroup and IsAttributeStoringRep and
    HasGeneratorsOfMagmaWithInverses and HasOne);

MitM_InstallMethod( MitM_GroupWithGenerators,
    [IsCollection],
    function( arg... )
      return CallFuncList( GroupWithGenerators, arg );
    end);

MitM_InstallMethod( MitM_GroupWithGenerators,
    [IsCollection, IsMultiplicativeElementWithInverse],
    function( arg... )
      return CallFuncList( GroupWithGenerators, arg );
    end);

MitM_InstallMethod( MitM_GroupWithGenerators,
    [ IsList and IsEmpty, IsMultiplicativeElementWithInverse ],
    function( arg... )
      return CallFuncList( GroupWithGenerators, arg );
    end);

#This one has more specific output filters,
#should we make use of this in MitM_InstallMethod?
#InstallMethod( MitM_DeclareConstructor,
#    "GroupWithGenerators",
#    [ IsList and IsEmpty, IsMultiplicativeElementWithInverse ],
#    IsGroup and IsAttributeStoringRep and
#    HasGeneratorsOfMagmaWithInverses and
#    IsFinitelyGeneratedGroup and HasOne); 

#MitM_DeclareGlobalFunction( "SubgroupByProperty" );
#
#InstallMethod( MitM_DeclareConstructor,
#    "SubgroupByProperty",
#    [],
#    IsMagmaWithInverses and IsAttributeStoringRep 
#    and HasElementTestFunction);
#
#InstallMethod( MitM_DeclareConstructor,
#    "SubgroupShell",
#    [],
#    IsMagmaWithInverses and IsAttributeStoringRep);
