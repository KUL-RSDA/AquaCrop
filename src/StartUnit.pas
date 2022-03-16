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
FullFileName := CONCAT(GetPathNameSimul(),'AggregationResults.SIM');
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

FullFileName := CONCAT(GetPathNameSimul(),'DailyResults.SIM');
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

FullFileName := CONCAT(GetPathNameSimul(),'ParticularResults.SIM');
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
Assign(fProjects,CONCAT(GetPathNameOutp(),'ListProjectsLoaded.OUT'));
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


FUNCTION GetListProjectsFile() : string;
BEGIN
    GetListProjectsFile := CONCAT(PathNameList,'ListProjects.txt');
END;


FUNCTION GetNumberOfProjects() : integer;
VAR
    NrProjects : integer;
    ListProjectsFile : string;
    ListProjectFileExist : boolean;
    fhandle : TextFile;
BEGIN
    ListProjectsFile := GetListProjectsFile();
    ListProjectFileExist := FileExists(ListProjectsFile);
    NrProjects := 0;

    IF ListProjectFileExist THEN
    BEGIN
        Assign(fhandle, ListProjectsFile);
        Reset(fhandle);
        WHILE NOT EOF(fhandle) DO
        BEGIN
            READLN(fhandle);
            NrProjects := NrProjects + 1;
        END;
        Close(fhandle);
    END;

    GetNumberOfProjects := NrProjects;
END;


FUNCTION GetProjectFileName(constref iproject : integer) : string;
VAR
    jproject : integer;
    ListProjectsFile, TheProjectFile : string;
    fhandle : TextFile;
BEGIN
    {$I+}  // turn on IO checks
    ListProjectsFile := GetListProjectsFile();
    Assert(FileExists(ListProjectsFile), 'ListProjectsFile does not exist');

    Assign(fhandle, ListProjectsFile);
    Reset(fhandle);

    // Read until we arrive at the selected project
    FOR jproject := 1 to iproject DO
    BEGIN
        READLN(fhandle, TheProjectFile);
    END;
    Close(fhandle);

    GetProjectFileName := TheProjectFile;
END;


PROCEDURE InitializeTheProgram;
BEGIN
Decimalseparator := '.';
SetPathNameOutp('OUTP/');
SetPathNameSimul('SIMUL/');
PathNameList :=  'LIST/';
PathNameParam := 'PARAM/';
SetPathNameProg('');

GetTimeAggregationResults(OutputAggregate);
GetRequestDailyResults(Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily);
GetRequestParticularResults(Part1Mult,Part2Eval);
PrepareReport(OutputAggregate,Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily,
              Part1Mult,Part2Eval);
END;


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
END; // GetProjectType


PROCEDURE InitializeProject(constref iproject : integer;
                            constref TheProjectFile : string;
                            constref TheProjectType : repTypeProject);
VAR NrString,TestFile : string;
    CanSelect,ProgramParametersAvailable : BOOLEAN;
    TotalSimRuns : Integer;
    SimNr : ShortInt;
    FullFileNameProgramParametersLocal : string;
    MultipleRunWithKeepSWC_temp : boolean;
    MultipleRunConstZrx_temp : double;


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
    END; // ComposeFileForProgramParameters


    PROCEDURE LoadProgramParametersProjectPlugIn(FullFileNameProgramParameters : string;
                                             VAR ProgramParametersAvailable : BOOLEAN);
    VAR f0 : TextFile;
        i,simul_RpZmi,simul_lowox : INTEGER;
        effrainperc,effrainshow,effrainrootE,simul_saltdiff,simul_saltsolub, simul_root,simul_ed,simul_pCCHIf,simul_SFR,simul_TAWg,simul_beta,simul_Tswc,simul_EZma,simul_GDD : ShortInt;
        simul_rod,simul_kcWB,simul_RZEma,simul_pfao,simul_expFsen,simul_Tmi,simul_Tma : double;
    BEGIN
    IF FileExists(FullFileNameProgramParameters)
       THEN BEGIN // load set of program parameters
            ProgramParametersAvailable := true;
            Assign(f0,FullFileNameProgramParameters);
            Reset(f0);
            // crop
            Readln(f0,simul_ed); // evaporation decline factor in stage 2
            SetSimulParam_EvapDeclineFactor(simul_ed);
            Readln(f0,simul_kcWB); //Kc wet bare soil [-]
            SetSimulParam_KcWetBare(simul_kcWB);
            Readln(f0,simul_pCCHIf); // CC threshold below which HI no longer increase(% of 100)
            SetSimulParam_PercCCxHIfinal(simul_pCCHIf);

            Readln(f0,simul_RpZmi); //Starting depth of root sine function (% of Zmin)
            SetSimulParam_RootPercentZmin(simul_RpZmi);
            Readln(f0,simul_RZEma); // cm/day
            SetSimulParam_MaxRootZoneExpansion(simul_RZEma);

            SetSimulParam_MaxRootZoneExpansion(5.00); // fixed at 5 cm/day
            Readln(f0,simul_SFR); // Shape factor for effect water stress on rootzone expansion
            SetSimulParam_KsShapeFactorRoot(simul_SFR);
            Readln(f0,simul_TAWg);  // Soil water content (% TAW) required at sowing depth for germination

            SetSimulParam_TAWGermination(simul_TAWg);
            Readln(f0,simul_pfao); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
            SetSimulParam_pAdjFAO(simul_pfao);
            Readln(f0,simul_lowox); //number of days for full effect of deficient aeration

            SetSimulParam_DelayLowOxygen(simul_lowox);
            Readln(f0,simul_expFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
            SetSimulParam_ExpFsen(simul_expFsen);
            Readln(f0,simul_beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered

            SetSimulParam_Beta(simul_beta);
            Readln(f0,simul_Tswc);  // Thickness top soil (cm) in which soil water depletion has to be determined
            SetSimulParam_ThicknessTopSWC(simul_Tswc);
            // field

            Readln(f0,simul_EZma); //maximum water extraction depth by soil evaporation [cm]
            SetSimulParam_EvapZmax(simul_EZma);
            // soil
            READLN(f0,simul_rod); //considered depth (m) of soil profile for calculation of mean soil water content

            SetSimulParam_RunoffDepth(simul_rod);
            READLN(f0,i);   // correction CN for Antecedent Moisture Class
            IF (i = 1)
               THEN SetSimulParam_CNcorrection(true)

               ELSE SetSimulParam_CNcorrection(false);
            READLN(f0,simul_saltdiff); // salt diffusion factor (%)
            SetSimulParam_SaltDiff(simul_saltdiff); 
            READLN(f0,simul_saltsolub); // salt solubility (g/liter)

            SetSimulParam_SaltSolub(simul_saltsolub);
            READLN(f0,simul_root); // shape factor capillary rise factor
            SetSimulParam_RootNrDF(simul_root);             
            SetSimulParam_IniAbstract(5); // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions

            // Temperature
            Readln(f0,Tmin);   //Default minimum temperature (degC) if no temperature file is specified
            SetSimulParam_Tmin(simul_Tmi);
            Readln(f0,simul_Tma);   //Default maximum temperature (degC) if no temperature file is specified

            SetSimulParam_Tmax(simul_Tma);
            Readln(f0,simul_GDD); //Default method for GDD calculations
            SetSimulParam_GDDMethod(simul_GDD);
            IF (GetSimulParam_GDDMethod() > 3) THEN SetSimulParam_GDDMethod(3);

            IF (GetSimulParam_GDDMethod() < 1) THEN SetSimulParam_GDDMethod(1);
            // Rainfall
            Readln(f0,i);
            Case i OF

              0 : SetSimulParam_EffectiveRain_Method(Full);
              1 : SetSimulParam_EffectiveRain_Method(USDA);
              2 : SetSimulParam_EffectiveRain_Method(Percentage);
              end;

            Readln(f0,effrainperc); // IF Method is Percentage
            SetSimulParam_EffectiveRain_PercentEffRain(effrainperc);
            Readln(f0,effrainshow);  // For estimation of surface run-off
            SetSimulParam_EffectiveRain_ShowersInDecade(effrainshow);

            Readln(f0,effrainrootE); // For reduction of soil evaporation
            SetSimulParam_EffectiveRain_RootNrEvap(effrainrootE);
            // close
            Close(f0);

            END
       ELSE BEGIN // take the default set of program parameters (already read in InitializeSettings)
            ProgramParametersAvailable := false;
            END;

    END; // LoadProgramParametersProjectPlugIn

BEGIN
    Str(iproject:8,NrString);

    // check if project file exists
    IF (TheProjectType <> TypeNone) THEN
    BEGIN
        TestFile := CONCAT(PathNameList,TheProjectFile);
        IF (FileExists(TestFile) = false) THEN CanSelect := false;
    END;

    IF ((TheProjectType <> TypeNone) AND CanSelect) THEN
    BEGIN // run the project after cheking environment and simumation files
        //1. Set No specific project
        InitializeSettings;

        CASE TheProjectType OF
        TypePRO : BEGIN
            // 2. Assign single project file
            SetProjectFile(TheProjectFile);
            SetProjectFileFull(CONCAT(PathNameList,GetProjectFile()));
            //3. Check if Environment and Simulation Files exist
            CanSelect := true;
            CheckFilesInProject(GetProjectFileFull(),(1),CanSelect);
            //4. load project parameters
            IF CanSelect THEN
            BEGIN
                ProjectDescription := 'undefined';
                FullFileNameProgramParametersLocal := GetFullFileNameProgramParameters();
                ComposeFileForProgramParameters(GetProjectFile(),FullFileNameProgramParametersLocal);
                SetFullFileNameProgramParameters(FullFileNameProgramParametersLocal);
                LoadProgramParametersProjectPlugIn(GetFullFileNameProgramParameters(),ProgramParametersAvailable);
                ComposeOutputFileName(GetProjectFile());
            END;
        END;

        TypePRM : BEGIN
            // 2. Assign multiple project file
            SetMultipleProjectFile(TheProjectFile);
            SetMultipleProjectFileFull(CONCAT(PathNameList,GetMultipleProjectFile()));
            //2bis. Get number of Simulation Runs
            GetNumberSimulationRuns(GetMultipleProjectFileFull(),TotalSimRuns);
            //3. Check if Environment and Simulation Files exist for all runs
            CanSelect := true;
            SimNr := 0;
            WHILE (CanSelect AND (SimNr < TotalSimRuns)) DO
            BEGIN
                SimNr := SimNr + 1;
                CheckFilesInProject(GetMultipleProjectFileFull(),SimNr,CanSelect);
            END;

            //4. load project parameters
            IF CanSelect THEN
            BEGIN
                 MultipleProjectDescription := 'undefined';
                 FullFileNameProgramParametersLocal := GetFullFileNameProgramParameters();
                 ComposeFileForProgramParameters(GetMultipleProjectFile(),FullFileNameProgramParametersLocal);
                 SetFullFileNameProgramParameters(FullFileNameProgramParametersLocal);
                 LoadProgramParametersProjectPlugIn(GetFullFileNameProgramParameters(),ProgramParametersAvailable);
                 ComposeOutputFileName(GetMultipleProjectFile());
                 SetSimulation_MultipleRun(true);
                 SetSimulation_NrRuns(TotalSimRuns);
                 MultipleRunWithKeepSWC_temp := GetSimulation_MultipleRunWithKeepSWC();
                 MultipleRunConstZrx_temp := GetSimulation_MultipleRunConstZrx();
                 CheckForKeepSWC(GetMultipleProjectFileFull(),GetSimulation_NrRuns(),MultipleRunWithKeepSWC_temp,MultipleRunConstZrx_temp);
                 SetSimulation_MultipleRunWithKeepSWC(MultipleRunWithKeepSWC_temp);
                 SetSimulation_MultipleRunConstZrx(MultipleRunConstZrx_temp);
            END;
        END;

        ELSE
        END;

        //5. Run
        IF CanSelect THEN
        BEGIN
            IF (ProgramParametersAvailable = True)
            THEN WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project loaded - with its program parameters')
            ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project loaded - default setting of program parameters');
        END
        ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : Project NOT loaded',
                     ' - Missing Environment and/or Simulation file(s)');
    END
    ELSE
    BEGIN // not a project file or missing in the LIST  dirtectory
        IF CanSelect
        THEN WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : is NOT a project file')
        ELSE WRITELN(fProjects,Trim(NrString),'. - ',Trim(TheProjectFile),' : project file NOT available in LIST directory');
    END;
END;



PROCEDURE FinalizeTheProgram;
VAR
    fend : TextFile;

BEGIN
    Close(fProjects);

    // all done
    Assign(fend,CONCAT(GetPathNameOutp(),'AllDone.OUT'));
    Rewrite(fend);
    WRITELN(fend,'All done');
    Close(fend);
END;


PROCEDURE WriteProjectsInfo(constref line : string);
BEGIN
    WRITELN(fProjects, line);
END;


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
