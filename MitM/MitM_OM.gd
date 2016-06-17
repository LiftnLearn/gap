LoadPackage("Openmath");

#TestMethod for printing metadata of group construction in OMDoc
InstallMethod(MitM_OM, [IsGroup],
    function(group)
        local str;
        str := Concatenation("<OMOBJ>\n\t<OMA>\n\t\t<OMS cd='prog1' name='function_call'/>\n\t\t<OMV name='",
               MitM_ConstructorName(group),
               "'/>\n\t</OMA>\n",
               "\t<OMA>\n\t\t<OMS cd='prog1' name='call_arguments'/>",
               OMString(MitM_ConstructorArgs(group)[1][1]:noomobj), 
               "</OMA>\n</OMOBJ>");
        return str;
    end
);

#to use do the following:
#f := Filename(DirectoriesLibrary(), "../MitM/MitM_OM.gd");
#Read(f);
