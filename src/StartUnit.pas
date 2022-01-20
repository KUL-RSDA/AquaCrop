unit StartUnit;

interface

PROCEDURE StartTheProgram;


implementation

USES SysUtils,Global,interface_global,InitialSettings,interface_initialsettings,Run,interface_run, interface_startunit;

VAR fProjects : textFile;


PROCEDURE GetTimeAggregationResults(VAR OutputAggregate : ShortInt);
VAR FullFileName,TempString : string;
    f0 : TextFile;
    n,i : INTEGER;

BEGIN
OutputAggregate := 0; // simulation period 0: season
FullFileName := CONCAT(PathNameSimul,'AggregationResults.SIM');
IF (FileExists(FullFileName) = true) THEN
   BEGIN
   Assign(f0,FullFileName);
   Reset(f0);
   READLN(f0,TempString);
   n := Length(TempString);
   IF (n > 0) THEN
      BEGIN
      i := 1;
      While ((TempString[i] = ' ') AND (i < n)) DO i := i + 1;
      IF (TempString[i] = '1')
         THEN OutputAggregate := 1 // 1: daily aggregation
         ELSE BEGIN
              IF (TempString[i] = '2')
                 THEN OutputAggregate := 2 // 2 : 10-daily aggregation
                 ELSE BEGIN
                      IF (TempString[i] = '3')
                         THEN OutputAggregate := 3 // 3 : monthly aggregation
                         ELSE OutputAggregate := 0; // 0 : seasonal results only
                      END;
              END;
      END;
   Close(f0);
   END;
END; (* GetTimeAggregationResults *)


PROCEDURE GetRequestDailyResults(VAR Out1Wabal,Out2Crop,Out3Prof,Out4Salt,
                                     Out5CompWC,Out6CompEC,Out7Clim,OutDaily : BOOLEAN);
VAR FullFileName,TempString : string;
    f0 : TextFile;
    n,i : INTEGER;

BEGIN
Out1Wabal := false;
Out2Crop := false;
Out3Prof := false;
Out4Salt := false;
Out5CompWC := false;
Out6CompEC := false;
Out7Clim := false;

FullFileName := CONCAT(PathNameSimul,'DailyResults.SIM');
IF (FileExists(FullFileName) = true) THEN
   BEGIN
   Assign(f0,FullFileName);
   Reset(f0);
   WHILE NOT Eof(f0) DO
     BEGIN
     READLN(f0,TempString);
     n := Length(TempString);
     IF (n > 0) THEN
        BEGIN
        i := 1;
        While ((TempString[i] = ' ') AND (i < n)) DO i := i + 1;
        IF (TempString[i] = '1') THEN Out1Wabal := true;
        IF (TempString[i] = '2') THEN Out2Crop := true;
        IF (TempString[i] = '3') THEN Out3Prof := true;
        IF (TempString[i] = '4') THEN Out4Salt := true;
        IF (TempString[i] = '5') THEN Out5CompWC := true;
        IF (TempString[i] = '6') THEN Out6CompEC := true;
        IF (TempString[i] = '7') THEN Out7Clim := true;
        END;
     END;
   Close(f0);
   END;
IF ((Out1Wabal = true) OR (Out2Crop = true) OR (Out3Prof = true) OR
   (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true) )
   THEN OutDaily := true
   ELSE OutDaily := false;
END; (* GetRequestDailyResults *)


PROCEDURE GetRequestParticularResults(VAR Part1Mult,Part2Eval : BOOLEAN);
VAR FullFileName,TempString : string;
    f0 : TextFile;
    n,i : INTEGER;

BEGIN
Part1Mult := false;
Part2Eval := false;

FullFileName := CONCAT(PathNameSimul,'ParticularResults.SIM');
IF (FileExists(FullFileName) = true) THEN
   BEGIN
   Assign(f0,FullFileName);
   Reset(f0);
   WHILE NOT Eof(f0) DO
     BEGIN
     READLN(f0,TempString);
     n := Length(TempString);
     IF (n > 0) THEN
        BEGIN
        i := 1;
        While ((TempString[i] = ' ') AND (i < n)) DO i := i + 1;
        IF (TempString[i] = '1') THEN Part1Mult := true;
        IF (TempString[i] = '2') THEN Part2Eval := true;
        END;
     END;
   Close(f0);
   END;
END; (* GetRequestParticularResults *)



PROCEDURE PrepareReport(OutputAggregate : ShortInt;
                        Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily,
                        Part1Mult,Part2Eval : BOOLEAN);
BEGIN
Assign(fProjects,CONCAT(PathNameOutp,'ListProjectsLoaded.OUT'));
Rewrite(fProjects);
WRITE(fProjects,'Intermediate results: ');
CASE OutputAggregate OF
     1 : WRITELN(fProjects,'daily results');
     2 : WRITELN(fProjects,'10-daily results');
     3 : WRITELN(fProjects,'monthly results');
     else WRITELN(fProjects,'None created');
     end;
WRITELN(fProjects);
IF OutDaily
   THEN BEGIN
        WRITELN(fProjects,'Daily output results:');
        IF Out1Wabal THEN WRITELN(fProjects,'1. - soil water balance');
        IF Out2Crop THEN WRITELN(fProjects,'2. - crop development and production');
        IF Out3Prof THEN WRITELN(fProjects,'3. - soil water content in the soil profile and root zone');
        IF Out4Salt THEN WRITELN(fProjects,'4. - soil salinity in the soil profile and root zone');
        IF Out5CompWC THEN WRITELN(fProjects,'5. - soil water content at various depths of the soil profile');
        IF Out6CompEC THEN WRITELN(fProjects,'6. - soil salinity at various depths of the soil profile');
        IF Out7Clim THEN WRITELN(fProjects,'7. - climate input parameters');
        END
   ELSE WRITELN(fProjects,'Daily output results: None created');
WRITELN(fProjects);
IF ((Part1Mult = True) OR  (Part2Eval = True))
   THEN BEGIN
        WRITELN(fProjects,'Particular results:');
        IF Part1Mult THEN WRITELN(fProjects,'1. - biomass and yield at multiple cuttings (for herbaceous forage crops)');
        IF Part2Eval THEN WRITELN(fProjects,'2. - evaluation of simulation results (when Field Data)');
        END
   ELSE WRITELN(fProjects,'Particular results: None created');
END; (* PrepareReport *)



PROCEDURE HandleProjectList;
VAR TheProjectFile,ListProjectsFile,NrString,TestFile : string;
    NrProjects,Pri : Integer;
    TheProjectType : repTypeProject;
    CanSelect,ListProjectFileExist,ProgramParametersAvailable : BOOLEAN;
    TotalSimRuns : Integer;
    SimNr : ShortInt;
    f123,fend : TextFile;




    PROCEDURE GetProjectType(VAR TheProjectFile : string;
                             VAR TheProjectType : repTypeProject);
    VAR i,lgth : INTEGER;
        TheExtension : string;

    BEGIN
    TheProjectType := TypeNone;
    TheProjectFile := Trim(TheProjectFile);
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
    END; (* GetProjectType *)



    PROCEDURE ComposeFileForProgramParameters(TheFileNameProgram : string;
                                          VAR FullFileNameProgramParameters : string);
    VAR TheLength : INTEGER;
        TheExtension : STRING;
    BEGIN
    FullFileNameProgramParameters := '';
    TheLength := Length(TheFileNameProgram);
    TheExtension := Copy(TheFileNameProgram,(TheLength-2),3); // PRO or PRM
    // file name program parameters
    FullFileNameProgramParameters := Copy(TheFileNameProgram,1,(TheLength-3));
    // path file progrm parameters
    FullFileNameProgramParameters := CONCAT(Trim(PathNameParam),FullFileNameProgramParameters);
    // extension file program parameters
    IF (TheExtension = 'PRO')
       THEN FullFileNameProgramParameters := CONCAT(FullFileNameProgramParameters,'PP1')
       ELSE FullFileNameProgramParameters := CONCAT(FullFileNameProgramParameters,'PPn');
    END; (* ComposeFileForProgramParameters *)



    PROCEDURE LoadProgramParametersProjectPlugIn(FullFileNameProgramParameters : string;
                                             VAR ProgramParametersAvailable : BOOLEAN);
    VAR f0 : TextFile;
        i : INTEGER;
    BEGIN
    IF FileExists(FullFileNameProgramParameters)
       THEN BEGIN // load set of program parameters
            ProgramParametersAvailable := true;
            Assign(f0,FullFileNameProgramParameters);
            Reset(f0);
            WITH SimulParam DO
              BEGIN
              // crop
              Readln(f0,EvapDeclineFactor); // evaporation decline factor in stage 2
              Readln(f0,KcWetBare); //Kc wet bare soil [-]
              Readln(f0,PercCCxHIfinal); // CC threshold below which HI no longer increase(% of 100)
              Readln(f0,RootPercentZmin); //Starting depth of root sine function (% of Zmin)
              Readln(f0,MaxRootZoneExpansion); // cm/day
              MaxRootZoneExpansion := 5.00; // fixed at 5 cm/day
              Readln(f0,KsShapeFactorRoot); // Shape factor for effect water stress on rootzone expansion
              Readln(f0,TAWGermination);  // Soil water content (% TAW) required at sowing depth for germination
              Readln(f0,pAdjFAO); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
              Readln(f0,DelayLowOxygen); //number of days for full effect of deficient aeration
              Readln(f0,ExpFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
              Readln(f0,Beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
              Readln(f0,ThicknessTopSWC);  // Thickness top soil (cm) in which soil water depletion has to be determined
              // field
              Readln(f0,EvapZmax); //maximum water extraction depth by soil evaporation [cm]
              // soil
              READLN(f0,SimulParam.RunoffDepth); //considered depth (m) of soil profile for calculation of mean soil water content
              READLN(f0,i);   // correction CN for Antecedent Moisture Class
              IF (i = 1)
                 THEN SimulParam.CNcorrection := true
                 ELSE SimulParam.CNcorrection := false;
              READLN(f0,SimulParam.SaltDiff); // salt diffusion factor (%)
              READLN(f0,SimulParam.SaltSolub); // salt solubility (g/liter)
              READLN(f0,SimulParam.RootNrDF); // shape factor capillary rise factor
              SimulParam.IniAbstract := 5; // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
              // Temperature
              Readln(f0,Tmin);   //Default minimum temperature (degC) if no temperature file is specified
              Readln(f0,Tmax);   //Default maximum temperature (degC) if no temperature file is specified
              Readln(f0,GDDMethod); //Default method for GDD calculations
              IF (GDDMethod > 3) THEN GDDMethod := 3;
              IF (GDDMethod < 1) THEN GDDMethod := 1;
              // Rainfall
              Readln(f0,i);
              Case i OF
                0 : EffectiveRain.Method := Full;
                1 : EffectiveRain.Method := USDA;
                2 : EffectiveRain.Method := Percentage;
                end;
              Readln(f0,EffectiveRain.PercentEffRain); // IF Method is Percentage
              Readln(f0,EffectiveRain.ShowersInDecade);  // For estimation of surface run-off
              Readln(f0,EffectiveRain.RootNrEvap); // For reduction of soil evaporation
              END;
            // close
            Close(f0);
            END
       ELSE BEGIN // take the default set of program parameters (already read in InitializeSettings)
            ProgramParametersAvailable := false;
            END;
    END; (* LoadProgramParametersProjectPlugIn *)




BEGIN  (* HandleProjectList *)
// Initialize
NrProjects := 0;

// Check for ListProjects.txt in LIST subdirectory
ListProjectsFile := CONCAT(PathNameList,'ListProjects.txt');
ListProjectFileExist := FileExists(ListProjectsFile);

IF ListProjectFileExist THEN
   BEGIN
   Assign(f123,ListProjectsFile);
   Reset(f123);
   WHILE NOT Eof(f123) DO
      BEGIN
      READLN(f123);
      NrProjects := NrProjects + 1;
      END;
   Reset(f123);
   END;

IF (NrProjects > 0) THEN
     BEGIN
     Pri := 0;
     WRITELN(fProjects);
     WRITELN(fProjects,'Projects handled:');
     REPEAT
       Pri := Pri + 1;
       Str(Pri:8,NrString);
       CanSelect := true;
       READLN(f123,TheProjectFile);
       TheProjectFile := Trim(TheProjectFile);
       // get TheProjectType
       GetProjectType(TheProjectFile,TheProjectType);
       // check if project file exists
       IF (TheProjectType <> TypeNone) THEN
          BEGIN
          TestFile := CONCAT(PathNameList,TheProjectFile);
          IF (FileExists(TestFile) = false) THEN CanSelect := false;
          END;

       IF ((TheProjectType <> TypeNone) AND CanSelect)
          THEN BEGIN // run the project after cheking environment and simumation files
               //1. Set No specific project
               InitializeSettings;
               CASE TheProjectType OF
                    TypePRO : BEGIN
                              // 2. Assign single project file
                              SetProjectFile(TheProjectFile);
                              ProjectFileFull := CONCAT(PathNameList,GetProjectFile());
                              //3. Check if Environment and Simulation Files exist
                              CanSelect := true;
                              CheckFilesInProject(ProjectFileFull,(1),CanSelect);
                              //4. load project parameters
                              IF CanSelect THEN
                                 BEGIN
                                 ProjectDescription := 'undefined';
                                 ComposeFileForProgramParameters(GetProjectFile(),FullFileNameProgramParameters);
                                 LoadProgramParametersProjectPlugIn(FullFileNameProgramParameters,ProgramParametersAvailable);
                                 ComposeOutputFileName(GetProjectFile());
                                 END;
                              END;
                    TypePRM : BEGIN
                              // 2. Assign multiple project file
                              MultipleProjectFile := TheProjectFile;
                              MultipleProjectFileFull := CONCAT(PathNameList,MultipleProjectFile);
                              //2bis. Get number of Simulation Runs
                              GetNumberSimulationRuns(MultipleProjectFileFull,TotalSimRuns);
                              //3. Check if Environment and Simulation Files exist for all runs
                              CanSelect := true;
                              SimNr := 0;
                              WHILE (CanSelect AND (SimNr < TotalSimRuns)) DO
                                BEGIN
                                SimNr := SimNr + 1;
                                CheckFilesInProject(MultipleProjectFileFull,SimNr,CanSelect);
                                END;
                              //4. load project parameters
                              IF CanSelect THEN
                                 BEGIN
                                 MultipleProjectDescription := 'undefined';
                                 ComposeFileForProgramParameters(MultipleProjectFile,FullFileNameProgramParameters);
                                 LoadProgramParametersProjectPlugIn(FullFileNameProgramParameters,ProgramParametersAvailable);
                                 ComposeOutputFileName(MultipleProjectFile);
                                 Simulation.MultipleRun := true;
                                 Simulation.NrRuns := TotalSimRuns;
                                 CheckForKeepSWC(MultipleProjectFileFull,Simulation.NrRuns,Simulation.MultipleRunWithKeepSWC,Simulation.MultipleRunConstZrx);
                                 END;
                              END;
                        else
                        end;
               //5. Run
               IF CanSelect
                  THEN BEGIN
                       IF (ProgramParametersAvailable = True)
                          THEN WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project loaded - with its program parameters')
                          ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project loaded - default setting of program parameters');
                       RunSimulation(TheProjectFile,TheProjectType);
                       END
                  ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project NOT loaded',
                                     ' - Missing Environment and/or Simulation file(s)');
               END
          ELSE BEGIN // not a project file or missing in the LIST  dirtectory
               IF CanSelect
                  THEN WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : is NOT a project file')
                  ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : project file NOT available in LIST directory');
               END;
     UNTIL(Pri >= NrProjects);
     END;

IF (NrProjects = 0) THEN // no projects
   BEGIN
   WRITELN(fProjects);
   WRITELN(fProjects,'Projects loaded: None');
   IF ListProjectFileExist
      THEN WRITELN(fProjects,'File ''ListProjects.txt'' does not contain ANY project file')
      ELSE WRITELN(fProjects,'Missing File ''ListProjects.txt'' in LIST directory');
   END;

IF ListProjectFileExist THEN Close(f123);
Close(fProjects);

// all done
Assign(fend,CONCAT(PathNameOutp,'AllDone.OUT'));
Rewrite(fend);
WRITELN(fend,'All done');
Close(fend);
END;   (* HandleProjectList *)




PROCEDURE StartTheProgram;
BEGIN
Decimalseparator := '.';
PathNameOutp := 'OUTP/';
PathNameSimul := 'SIMUL/';
PathNameList :=  'LIST/';
PathNameParam := 'PARAM/';
PathNameProg := '';

GetTimeAggregationResults(OutputAggregate);
GetRequestDailyResults(Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily);
GetRequestParticularResults(Part1Mult,Part2Eval);
PrepareReport(OutputAggregate,Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily,
              Part1Mult,Part2Eval);
HandleProjectList;
END;


end.
