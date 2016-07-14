gap> G := MitM_GroupWithGenerators([(1,2)]);
Group([ (1,2) ])
gap> Size(G);
2
gap> GeneratorsOfGroup(G);
[ (1,2) ]
gap> RightCoset(G, (2,3,4));
RightCoset(Group([ (1,2) ]),(2,3,4))

#gap> f := Filename(DirectoriesLibrary(), "../MitM/tst/group.tst");
#Test(f);