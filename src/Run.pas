unit Run;

interface

uses Global, interface_global, interface_run, interface_rootunit, interface_tempprocessing, interface_climprocessing, interface_simul, interface_inforesults;




PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);

implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;


PROCEDURE FileManagement();
VAR RepeatToDay : LongInt;

BEGIN (* FileManagement *)
RepeatToDay := GetSimulation_ToDayNr();
REPEAT
  AdvanceOneTimeStep()
UNTIL ((GetDayNri()-1) = RepeatToDay);
END; // FileManagement


PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);
VAR NrRun : ShortInt;
    NrRuns : integer;

BEGIN
InitializeSimulation(TheProjectFile_, TheProjectType);

CASE TheProjectType OF
    TypePRO : NrRuns := 1;
    TypePRM : NrRuns := GetSimulation_NrRuns();
    else;
END;

FOR NrRun := 1 TO NrRuns DO
BEGIN
   InitializeRun(NrRun, TheProjectType);
   FileManagement();
   FinalizeRun1(NrRun, GetTheProjectFile(), TheProjectType);
   FinalizeRun2(NrRun, TheProjectType);
END;

FinalizeSimulation();
END; // RunSimulation


end.
