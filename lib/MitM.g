#prototype of the function that is supposed to annotate type information
#for the Math in the Middle project as part of OpenDreamKit

#attributes where information about how object was
#constructed is stored
DeclareAttribute("MitM_ConstructorName", IsObject);
DeclareAttribute("MitM_ConstructorArgs", IsObject);

DeclareOperation("MitM_OM", [IsObject]);

BIND_GLOBAL( "MitM_DeclareConstructor",
  function( name, inputFilters, outputFilter )
    DeclareOperation(name, inputFilters); 
  end );

BIND_GLOBAL( "MitM_DeclareGlobalFunction",
  function( name, outputFilter )
    DeclareGlobalFunction(name); 
  end );

wrapper :=
function( functionToBeCalled)
  return function( arg...)
    local list;
    list := arg{[1..(Length(arg)-1)]};
    Append(list,
    [(function(local_arg...)

      local G;
      G:= CallFuncList(arg[(Length(arg))], (local_arg));

      SetMitM_ConstructorName(G, NameFunction(arg[1]));
      SetMitM_ConstructorArgs(G, local_arg);

      return G;
    end)]);
    CallFuncList(functionToBeCalled, list);
  end;
end;

#wrapper for InstallMethod that saves arguments used to 
#construct the object
BIND_GLOBAL( "MitM_InstallMethod",
    wrapper(InstallMethod) );

BIND_GLOBAL( "MitM_InstallOtherMethod",
    wrapper(InstallOtherMethod) );

BIND_GLOBAL( "MitM_InstallGlobalFunction",
    wrapper(InstallGlobalFunction) );

BIND_GLOBAL( "MitM_BindGlobal",
    wrapper(BindGlobal) );

#reimplementation of the methods with explicit filters
#for the output
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

MitM_DeclareConstructor( "MitM_SubgroupByProperty", [],
    IsMagmaWithInverses and IsAttributeStoringRep 
    and HasElementTestFunction);

MitM_InstallMethod( MitM_SubgroupByProperty,
    [],
    function( arg... )
      return CallFuncList(SubgroupByProperty, arg);
    end
);

MitM_DeclareConstructor( "MitM_SubgroupShell", [],
    IsMagmaWithInverses and IsAttributeStoringRep);

MitM_InstallMethod( MitM_SubgroupShell, [],
    function( arg... )
      return CallFuncList(SubgroupByProperty, arg);
    end
);
