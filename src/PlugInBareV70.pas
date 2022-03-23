library PlugInBareV70;

uses
  Global in 'Global.pas',
  interface_global in 'interface_global.pas',
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



PROCEDURE InitializeSimulation_wrap(constref TheProjectFile : PChar;
                                    constref TheProjectType : integer);
VAR
    str : AnsiString;
    ptype : repTypeProject;
BEGIN
    str := AnsiString(TheProjectFile);
    ptype := repTypeProject(TheProjectType);
    InitializeSimulation(str, ptype);
END;


PROCEDURE InitializeRun_wrap(constref NrRun : ShortInt;
                             constref TheProjectType : integer);
VAR
    ptype : repTypeProject;
BEGIN
    ptype := repTypeProject(TheProjectType);
    InitializeRun(NrRun, ptype);
END;


PROCEDURE FinalizeRun1_wrap(constref NrRun : ShortInt;
                            constref TheProjectFile : PChar;
                            constref TheProjectType : integer);
VAR
    str : AnsiString;
    ptype : repTypeProject;
BEGIN
    str := AnsiString(TheProjectFile);
    ptype := repTypeProject(TheProjectType);
    FinalizeRun1(NrRun, str, ptype);
END;


PROCEDURE FinalizeRun2_wrap(constref NrRun : ShortInt;
                            constref TheProjectType : integer);
VAR
    ptype : repTypeProject;
BEGIN
    ptype := repTypeProject(TheProjectType);
    FinalizeRun2(NrRun, ptype);
END;


exports
  // Related to StartUnit.pas procedures
  InitializeTheProgram name 'initializetheprogram_',
  FinalizeTheProgram name 'finalizetheprogram_',
  GetNumberOfProjects name 'getnumberofprojects_',
  WriteProjectsInfo_wrap name 'writeprojectsinfo_wrap_',
  GetListProjectsFile_wrap name 'getlistprojectsfile_wrap_',
  GetProjectFileName_wrap name 'getprojectfilename_wrap_',
  GetProjectType_wrap name 'getprojecttype_wrap_',
  InitializeProject_wrap name 'initializeproject_wrap_',
  // Related to Run.pas procedures
  GetDayNri name 'getdaynri_',
  InitializeSimulation_wrap name 'initializesimulation_wrap_',
  InitializeRun_wrap name 'initializerun_wrap_',
  AdvanceOneTimeStep name 'advanceonetimestep_',
  FinalizeRun1_wrap name 'finalizerun1_wrap_',
  FinalizeRun2_wrap name 'finalizerun2_wrap_',
  FinalizeSimulation name 'finalizesimulation_',
  // Related to interface_global.pas procedures
  GetSimulation_NrRuns name 'getsimulation_nrruns_',
  GetSimulation_ToDayNr name 'getsimulation_todaynr_';

begin

end.
