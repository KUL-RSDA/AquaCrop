unit StartUnit;

interface

USES Global, interface_global;

PROCEDURE GetProjectType(constref TheProjectFile : string;
                         VAR TheProjectType : repTypeProject);
PROCEDURE StartTheProgram;


implementation

USES SysUtils,InitialSettings,interface_initialsettings,Run,interface_run, interface_startunit;



PROCEDURE GetProjectType(constref TheProjectFile : string;
                         VAR TheProjectType : repTypeProject);
VAR i,lgth : INTEGER;
    TheExtension : string;

BEGIN
TheProjectType := TypeNone;
lgth := length(TheProjectFile);
IF (lgth > 0) THEN
   BEGIN
   i := 1;
   WHILE ((TheProjectFile[i] <> '.') AND (i < lgth)) DO i := i +1;
   IF (i = (lgth - 3)) THEN
      BEGIN
      TheExtension := COPY(TheProjectFile,(i+1),(i+3));
      FOR i := 1 to 3 do TheExtension[i] := UpCase(TheExtension[i]);
      IF (TheExtension = 'PRO')
         THEN TheProjectType := TypePRO
         ELSE BEGIN
              IF (TheExtension = 'PRM')
                 THEN TheProjectType := TypePRM
                 ELSE TheProjectType := TypeNone;
              END;
      END;
   END;
END; // GetProjectType


PROCEDURE StartTheProgram;
VAR
    iproject, nprojects : integer;
    ListProjectsFile, TheProjectFile : string;
    ListProjectFileExist : boolean;
    TheProjectType : repTypeProject;

BEGIN
    InitializeTheProgram;

    ListProjectsFile := GetListProjectsFile();
    ListProjectFileExist := FileExists(ListProjectsFile);
    nprojects := GetNumberOfProjects();

    IF (nprojects > 0) THEN
    BEGIN
        WriteProjectsInfo('');
        WriteProjectsInfo('Projects handled:');
    END;

    FOR iproject := 1 to nprojects DO
    BEGIN
        TheProjectFile := GetProjectFileName(iproject);
        GetProjectType(TheProjectFile, TheProjectType);
        InitializeProject(iproject, TheProjectFile, TheProjectType);
        RunSimulation(TheProjectFile,TheProjectType);
    END;

    IF (nprojects = 0) THEN
    BEGIN
        WriteProjectsInfo('');
        WriteProjectsInfo('Projects loaded: None');

        IF ListProjectFileExist
        THEN WriteProjectsInfo('File ''ListProjects.txt'' does not contain ANY project file')
        ELSE WriteProjectsInfo('Missing File ''ListProjects.txt'' in LIST directory');
    END;

    FinalizeTheProgram;
END;

end.
