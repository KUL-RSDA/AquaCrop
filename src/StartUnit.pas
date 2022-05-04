unit StartUnit;

interface

USES Global, interface_global;

PROCEDURE StartTheProgram;


implementation

USES SysUtils,InitialSettings,interface_initialsettings,Run,interface_run, interface_startunit;




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
