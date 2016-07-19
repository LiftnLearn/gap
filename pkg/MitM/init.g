#############################################################################
##
#W    init.g                 The Example package                Werner Nickel
#W                                                                Greg Gamble
##

#############################################################################
##  Starting from GAP 4.4 and having  a  PackageInfo.g  file  available,  the
##  commands  `DeclarePackage'  and   `DeclarePackageAutoDocumentation'   are
##  ignored. They are substituted by the entries:
##   .PackageName, .Version, .PackageDoc, .Dependencies and .AvailabilityTest
##  specified in the PackageInfo.g file.
##
##  Since GAP 4.4, commands with `Pkg' in their name have `Package'  instead,
##  e.g. `ReadPkg' became `ReadPackage'.
##

#############################################################################
##
#R  Read the declaration files.
##

ReadPackage( "MitM", "lib/MitM.g");
ReadPackage( "MitM", "lib/MitM_OM.g");
ReadPackage( "MitM", "lib/findObjectify.gd");
ReadPackage( "MitM", "lib/produceDeclaration.gd" );

#E  init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here

