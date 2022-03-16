library PlugInBareV70;

uses
  Global in 'Global.pas',
  Run in 'Run.pas',
  StartUnit in 'StartUnit.pas';


PROCEDURE WriteProjectsInfo_wrap(constref line : PChar);
VAR
    str : AnsiString;
BEGIN
    str := AnsiString(line);
    WriteProjectsInfo(str);
END;


PROCEDURE GetListProjectsFile_wrap(out ListProjectsFile : PChar;
                                   out len : integer);
VAR
    str : AnsiString;
BEGIN
    str := GetListProjectsFile();
    ListProjectsFile := PChar(str);
    len := Length(str);
END;


PROCEDURE GetProjectFileName_wrap(constref iproject : integer;
                                  out TheProjectFile : PChar;
                                  out len : integer);
VAR
    str : AnsiString;
BEGIN
    str := GetProjectFileName(iproject);
    TheProjectFile := PChar(str);
    len := Length(str);
END;


PROCEDURE GetProjectType_wrap(constref TheProjectFile : PChar;
                              out TheProjectType : integer);
VAR
    str : AnsiString;
    ptype : repTypeProject;
BEGIN
    str := AnsiString(TheProjectFile);
    GetProjectType(str, ptype);
    TheProjectType := ord(ptype);
END;


PROCEDURE InitializeProject_wrap(constref iproject : integer;
                                 constref TheProjectFile : PChar;
                                 constref TheProjectType : integer);
VAR
    str : AnsiString;
    ptype : repTypeProject;
BEGIN
    str := AnsiString(TheProjectFile);
    ptype := repTypeProject(TheProjectType);
    InitializeProject(iproject, str, ptype);
END;


PROCEDURE RunSimulation_wrap(constref TheProjectFile : PChar;
                             constref TheProjectType : integer);
VAR
    str : AnsiString;
    ptype : repTypeProject;
BEGIN
    str := AnsiString(TheProjectFile);
    ptype := repTypeProject(TheProjectType);
    RunSimulation(str, ptype);
END;


exports
  StartTheProgram name 'starttheprogram_',
  InitializeTheProgram name 'initializetheprogram_',
  FinalizeTheProgram name 'finalizetheprogram_',
  GetNumberOfProjects name 'getnumberofprojects_',
  WriteProjectsInfo_wrap name 'writeprojectsinfo_wrap_',
  GetListProjectsFile_wrap name 'getlistprojectsfile_wrap_',
  GetProjectFileName_wrap name 'getprojectfilename_wrap_',
  GetProjectType_wrap name 'getprojecttype_wrap_',
  InitializeProject_wrap name 'initializeproject_wrap_',
  RunSimulation_wrap name 'runsimulation_wrap_';


begin

end.
