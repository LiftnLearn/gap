LoadPackage("Openmath");

#TestMethod for printing metadata of group construction in OMDoc
InstallMethod(MitM_OM, [IsGroup],
    function(group)
        Print("<OMOBJ>\n\t<OMA>\n\t\t<OMS cd='prog1' name='function_call'/>\n\t\t<OMV name='");
        PrintObj(MitM_ConstructorName(group));
        Print("'/>\n\t</OMA>\n");
        Print("\t<OMA>\n\t\t<OMS cd='prog1' name='call_arguments'/>");
        Print("\n");
        t := OpenMathXMLWriter(OutputTextUser());
        OMPut(t, MitM_ConstructorArgs(group)[1][1]);
        Print("</OMA>\n</OMOBJ>");
    end
);

#to use do the following:
#f := Filename(DirectoriesLibrary(), "../MitM/MitM_OM.gd");
#Read(f);
