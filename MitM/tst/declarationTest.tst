gap> Read("produceDeclaration.gi");;
gap> Read("mitm.g");;
gap> Read("MitM_OM.gd");;
gap> Read("test.gd");;
gap> g := MitM_GroupWithGenerators([(1,2,3),(1,2)]);
Group([ (1,2,3), (1,2) ])
gap> r := MitM_RightCoset(g, (1,2));
RightCoset(Group([ (1,2,3), (1,2) ]),(1,2))
gap> Print(MitM_OM(g));
<OMOBJ>
    <OMA>
        <OMS cd='????' name='MitM_GroupWithGenerators'/>
        <OMA> <OMS cd="list1" name="list"/> <OMA> <OMS cd="permut1" name="permutation"/> <OMI>2</OMI> <OMI>3</OMI> <OMI>1</OMI> </OMA> <OMA> <OMS cd="permut1" name="permutation"/> <OMI>\
2</OMI> <OMI>1</OMI> </OMA> </OMA>
    <OMA>
</OMOBJ>
