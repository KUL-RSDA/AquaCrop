unit Run;

interface

uses Global, interface_global, interface_run;

PROCEDURE RunSimulation(TheProjectFile : string;
                        TheProjectType : repTypeProject);



implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;

TYPE rep_GwTable = RECORD
     DNr1, DNr2 : LongInt;
     Z1, Z2 : INTEGER;  // cm
     EC1, EC2 : double; // dS/m
     end;

TYPE rep_plotPar = RECORD
       PotVal, ActVal : double;
       end;

TYPE repIrriInfoRecord = Record
       NoMoreInfo : BOOLEAN;
       FromDay,
       ToDay,
       TimeInfo,
       DepthInfo  : INTEGER;
       end;

TYPE rep_StressTot = RECORD
       Salt,
       Temp,
       Exp,
       Sto,
       Weed : double;
       NrD  : INTEGER;
       end;

TYPE repCutInfoRecord = Record
       NoMoreInfo : BOOLEAN;
       FromDay,
       ToDay,
       IntervalInfo : INTEGER;
       IntervalGDD,
       MassInfo  : double;
       end;

TYPE rep_Transfer = Record
       Store      : BOOLEAN; // transfer of assimilates from above ground parts to root system is active
       Mobilize   : BOOLEAN; // transfer of assimialtes from root system to above ground parts is active
       ToMobilize : double;  // Total mass of assimilates (ton/ha) to mobilize at start of the season
       Bmobilized : double;  // Cumulative sum of assimilates (ton/ha) mobilized form root system
       end;

var  fRun, fDaily, fHarvest, fEval : text;
     fEToSIM,fRainSIM,fTempSIM,fIrri,fCuts,fObs : text;
     PlotVarCrop : rep_plotPar;
     SumETo, SumGDD, GDDayi,Ziprev,SumGDDPrev,TESTVAL : double;
     WaterTableInProfile,StartMode,NoMoreCrop : BOOLEAN;
     GlobalIrriECw : BOOLEAN; // for versions before 3.2 where EC of irrigation water was not yet recorded
     GwTable : rep_GwTable;
     CCxWitheredTpot,CCxWitheredTpotNoS : double;
     StressTot : rep_StressTot;
     Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF : double;
     Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
     CO2i : double;
     PreviousStressLevel,StressSFadjNEW : ShortInt;
     SumKcTop,SumKcTopStress,SumKci,Zeval,CCxCropWeedsNoSFstress,fWeedNoS,
     CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,WeedRCi,CCiActualWeedInfested : double;

     DayNri : LongInt;
     EToDataSet,RainDataSet,TminDataSet,TmaxDataSet : rep_SimulationEventsDbl;
     IrriInterval : INTEGER;
     IrriInfoRecord1,IrriInfoRecord2 : repIrriInfoRecord;
     Tadj, GDDTadj : INTEGER;
     DayFraction,GDDayFraction,Bin,Bout : double;
     TimeSenescence : double; // calendar days or GDDays
     DayLastCut,NrCut,SumInterval : INTEGER;
     Transfer : rep_Transfer;
     CGCadjustmentAfterCutting : BOOLEAN;
     BprevSum,YprevSum,SumGDDcuts : double;
     CutInfoRecord1,CutInfoRecord2 : repCutInfoRecord;
     CGCref,GDDCGCref : double;
     HItimesBEF,ScorAT1,SCorAT2,HItimesAT1,HItimesAT2,HItimesAT,alfaHI,alfaHIAdj : double;
     StressLeaf,StressSenescence : double;   // % stress for leaf expansion and senescence
     TargetTimeVal, TargetDepthVal, Step : Integer;

     // DelayedGermination
     NextSimFromDayNr : LongInt; // the Simulation.FromDayNr for next run if delayed germination and KeepSWC

     // Evaluation
     DayNr1Eval,DayNrEval : LongInt;
     LineNrEval : INTEGER;
     
// specific for StandAlone
     PreviousSum : rep_sum;
     PreviousSumETo,PreviousSumGDD : double;
     PreviousBmob,PreviousBsto : double;
     NoYear : BOOLEAN;
     StageCode : ShortInt;
     PreviousDayNr : LongInt;
     



PROCEDURE OpenOutputRun(TheProjectType : repTypeProject;
                         VAR fRun : text);
VAR totalname : string;

BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(PathNameOutp,OutputName,'PROseason.OUT');
      TypePRM : totalname := CONCAT(PathNameOutp,OutputName,'PRMseason.OUT');
      end;
Assign(fRun,totalname);
Rewrite(fRun);
WRITELN(fRun,'AquaCrop 7.0 (October 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
WRITELN(fRun);
WRITELN(fRun,'    RunNr     Day1   Month1    Year1     Rain      ETo       GD     CO2',
             '      Irri   Infilt   Runoff    Drain   Upflow        E     E/Ex       Tr      TrW   Tr/Trx',
             '    SaltIn   SaltOut    SaltUp  SaltProf',
             '     Cycle   SaltStr  FertStr  WeedStr  TempStr   ExpStr   StoStr',
             '  BioMass  Brelative   HI    Y(dry)  Y(fresh)    WPet      Bin     Bout     DayN   MonthN    YearN');
WRITELN(fRun,'                                           mm       mm    °C.day    ppm',
             '        mm       mm       mm       mm       mm       mm        %       mm       mm        %',
             '    ton/ha    ton/ha    ton/ha    ton/ha',
             '      days       %        %        %        %        %        %  ',
             '  ton/ha        %       %    ton/ha   ton/ha    kg/m3   ton/ha   ton/ha');
END; (* OpenOutputRun *)


PROCEDURE OpenOutputDaily(TheProjectType : repTypeProject;
                          VAR fDaily : text);
VAR totalname : string;
BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(PathNameOutp,OutputName,'PROday.OUT');
      TypePRM : totalname := CONCAT(PathNameOutp,OutputName,'PRMday.OUT');
      end;
Assign(fDaily,totalname);
Rewrite(fDaily);
WRITELN(fDaily,'AquaCrop 7.0 (October 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
END; (* OpenOutputDaily *)


PROCEDURE OpenPart1MultResults(TheProjectType : repTypeProject;
                               VAR fHarvest : text);
VAR totalname : string;
BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(PathNameOutp,OutputName,'PROharvests.OUT');
      TypePRM : totalname := CONCAT(PathNameOutp,OutputName,'PRMharvests.OUT');
      end;
Assign(fHarvest,totalname);
Rewrite(fHarvest);
WRITELN(fHarvest,'AquaCrop 7.0 (October 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
WRITELN(fHarvest,'Biomass and Yield at Multiple cuttings');
END; (* OpenPart1MultResults *)



PROCEDURE WriteTitleDailyResults(TheProjectType : repTypeProject;
                                 TheNrRun : ShortInt;
                                 VAR fDaily : text);
VAR Str1,Str2 : string;
    NodeD,Zprof : double;
    Compi : INTEGER;
BEGIN
// A. Run number
WRITELN(fDaily);
IF (TheProjectType = TypePRM) THEN
   BEGIN
   Str(TheNrRun:4,Str1);
   WRITELN(fDaily,'   Run:',Str1);
   END;

// B. thickness of soil profile and root zone
IF ((Out1Wabal) OR (Out3Prof = true) OR (Out4Salt = true)) THEN
   BEGIN
   Zprof := 0;
   FOR compi :=1 to NrCompartments DO Zprof := Zprof + Compartment[compi].Thickness;
   Str(Zprof:4:2,Str1);
   IF (ROUND(Soil.RootMax*1000) = ROUND(Crop.RootMax*1000))
      THEN Str(Crop.RootMax:4:2,Str2)
      ELSE Str(Soil.RootMax:4:2,Str2);
   END;

// C. 1st line title
WRITE(fDaily,'   Day Month  Year   DAP Stage');

// C1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'   WC(',Str1,')   Rain     Irri   Surf   Infilt   RO    Drain       CR    Zgwt',
               '       Ex       E     E/Ex     Trx       Tr  Tr/Trx    ETx      ET  ET/ETx')
      ELSE WRITELN(fDaily,'   WC(',Str1,')   Rain     Irri   Surf   Infilt   RO    Drain       CR    Zgwt',
               '       Ex       E     E/Ex     Trx       Tr  Tr/Trx    ETx      ET  ET/ETx');
   END;
// C2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'      GD       Z     StExp  StSto  StSen StSalt StWeed   CC      CCw     StTr  Kc(Tr)     Trx       Tr      TrW  Tr/Trx   WP',
        '    Biomass     HI    Y(dry)  Y(fresh)  Brelative    WPet      Bin     Bout')
      ELSE WRITELN(fDaily,'      GD       Z     StExp  StSto  StSen StSalt StWeed   CC      CCw     StTr  Kc(Tr)     Trx       Tr      TrW  Tr/Trx   WP',
        '    Biomass     HI    Y(dry)  Y(fresh)  Brelative    WPet      Bin     Bout');
   END;
// C3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   IF ((Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'  WC(',Str1,') Wr(',Str2,')     Z       Wr    Wr(SAT)    Wr(FC)   Wr(exp)   Wr(sto)   Wr(sen)   Wr(PWP)')
      ELSE WRITELN(fDaily,'  WC(',Str1,') Wr(',Str2,')     Z       Wr    Wr(SAT)    Wr(FC)   Wr(exp)   Wr(sto)   Wr(sen)   Wr(PWP)');
   END;
// C4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'    SaltIn    SaltOut   SaltUp   Salt(',Str1,')  SaltZ     Z       ECe    ECsw   StSalt  Zgwt    ECgw')
      ELSE WRITELN(fDaily,'    SaltIn    SaltOut   SaltUp   Salt(',Str1,')  SaltZ     Z       ECe    ECsw   StSalt  Zgwt    ECgw');
   END;
// C5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
   WRITE(fDaily,'       WC01');
   FOR Compi := 2 TO (NrCompartments-1) DO
       BEGIN
       Str(Compi:2,Str1);
       WRITE(fDaily,'       WC',Str1);
       END;
   Str(NrCompartments:2,Str1);
   IF ((Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'       WC',Str1)
      ELSE WRITELN(fDaily,'       WC',Str1);
   END;
// C6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   WRITE(fDaily,'      ECe01');
   FOR Compi := 2 TO (NrCompartments-1) DO
       BEGIN
       Str(Compi:2,Str1);
       WRITE(fDaily,'      ECe',Str1);
       END;
   Str(NrCompartments:2,Str1);
   IF (Out7Clim = true)
      THEN WRITE(fDaily,'      ECe',Str1)
      ELSE WRITELN(fDaily,'      ECe',Str1);
   END;
// C7. Climate input parameters
IF Out7Clim THEN WRITELN(fDaily,'     Rain       ETo      Tmin      Tavg      Tmax      CO2');

// D. 2nd line title
WRITE(fDaily,'                              ');
// D1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'        mm      mm       mm     mm     mm     mm       mm       mm      m ',
               '       mm       mm     %        mm       mm    %        mm      mm       %')
      ELSE WRITELN(fDaily,'        mm      mm       mm     mm     mm     mm       mm       mm      m ',
               '       mm       mm     %        mm       mm    %        mm      mm       %');
   END;
// D2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'    °C-day     m       %      %      %      %      %      %       %       %       -        mm       mm       mm    %     g/m2',
        '    ton/ha      %    ton/ha   ton/ha       %       kg/m3   ton/ha   ton/ha')
      ELSE WRITELN(fDaily,'    °C-day     m       %      %      %      %      %      %       %       %       -        mm       mm       mm    %     g/m2',
        '    ton/ha      %    ton/ha   ton/ha       %       kg/m3   ton/ha   ton/ha');
   END;
// D3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   IF ((Out4Salt) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'      mm       mm       m       mm        mm        mm        mm        mm        mm         mm')
      ELSE WRITELN(fDaily,'      mm       mm       m       mm        mm        mm        mm        mm        mm        mm');
   END;
// D4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,'    ton/ha    ton/ha    ton/ha    ton/ha    ton/ha     m      dS/m    dS/m      %     m      dS/m')
      ELSE WRITELN(fDaily,'    ton/ha    ton/ha    ton/ha    ton/ha    ton/ha     m      dS/m    dS/m      %     m      dS/m');
   END;
// D5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
   NodeD := Compartment[1].Thickness/2;
   WRITE(fDaily,NodeD:11:2);
   FOR Compi := 2 TO (NrCompartments-1) DO
       BEGIN
       NodeD := NodeD + Compartment[Compi-1].Thickness/2 + Compartment[Compi].Thickness/2;
       WRITE(fDaily,NodeD:11:2);
       END;
   NodeD := NodeD + Compartment[NrCompartments-1].Thickness/2 + Compartment[NrCompartments].Thickness/2;
   IF ((Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,NodeD:11:2)
      ELSE WRITELN(fDaily,NodeD:11:2);
   END;
// D6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   NodeD := Compartment[1].Thickness/2;
   WRITE(fDaily,NodeD:11:2);
   FOR Compi := 2 TO (NrCompartments-1) DO
       BEGIN
       NodeD := NodeD + Compartment[Compi-1].Thickness/2 + Compartment[Compi].Thickness/2;
       WRITE(fDaily,NodeD:11:2);
       END;
   NodeD := NodeD + Compartment[NrCompartments-1].Thickness/2 + Compartment[NrCompartments].Thickness/2;
   IF (Out7Clim = true)
      THEN WRITE(fDaily,NodeD:11:2)
      ELSE WRITELN(fDaily,NodeD:11:2);
   END;
// D7. Climate input parameters
IF Out7Clim THEN WRITELN(fDaily,'       mm        mm       °C        °C        °C       ppm');
END; (* WriteTitleDailyResults *)


PROCEDURE WriteTitlePart1MultResults(TheProjectType : repTypeProject;
                                     TheNrRun : ShortInt;
                                     VAR fHarvest : text);
VAR Str1 : string;
    Dayi,Monthi,Yeari : INTEGER;
    Nr : Double;
BEGIN
// A. Run number
WRITELN(fHarvest);
IF (TheProjectType = TypePRM) THEN
   BEGIN
   Str(TheNrRun:4,Str1);
   WRITELN(fHarvest,'   Run:',Str1);
   END;
// B. Title
WRITELN(fHarvest,'    Nr   Day  Month Year   DAP Interval  Biomass    Sum(B)   Dry-Yield  Sum(Y) Fresh-Yield  Sum(Y)');
WRITELN(fHarvest,'                                 days     ton/ha    ton/ha    ton/ha    ton/ha    ton/ha    ton/ha');
// C. start crop cycle
DetermineDate(Crop.Day1,Dayi,Monthi,Yeari);
NoYear := (Yeari = 1901);
IF NoYear THEN
   BEGIN
   IF (Dayi = 0) THEN Dayi := 1;
   Yeari := 9999;
   END;
Nr := 0;
Str(Nr:6:0,Str1);
WRITELN(fHarvest,Str1,Dayi:6,Monthi:6,Yeari:6,Nr:34:3,Nr:20:3,Nr:20:3);
END; (* WriteTitlePart1MultResults *)



PROCEDURE CreateEvalData(NrRun : ShortInt;
                         VAR fObs,fEval : text);
VAR dayi, monthi, yeari : INTEGER;
    StrNr,totalname,TempString : string;

BEGIN
// open input file with field data
Assign(fObs,ObservationsFileFull); // Observations recorded in File
Reset(fObs);
READLN(fObs); // description
READLN(fObs); // AquaCrop Version number
READLN(fObs,Zeval); //  depth of sampled soil profile
READLN(fObs,dayi);
READLN(fObs,monthi);
READLN(fObs,yeari);
DetermineDayNr(dayi,monthi,yeari,DayNr1Eval);
READLN(fObs); // title
READLN(fObs); // title
READLN(fObs); // title
READLN(fObs); // title
LineNrEval := undef_int;
IF (NOT Eof(fObs)) THEN
   BEGIN
   LineNrEval := 11;
   READLN(fObs,DayNrEval);
   DayNrEval := DayNr1Eval + DayNrEval -1;
   WHILE ((DayNrEval < Simulation.FromDayNr) AND (LineNrEval <> undef_int)) DO
      BEGIN
      IF (Eof(fObs))
         THEN LineNrEval := undef_int
         ELSE BEGIN
              LineNrEval := LineNrEval + 1;
              READLN(fObs,DayNrEval);
              DayNrEval := DayNr1Eval + DayNrEval -1;
              END;
      END;
   END;
IF (LineNrEval = undef_int) THEN Close(fObs);
// open file with simulation results, field data
IF (Simulation.MultipleRun AND (Simulation.NrRuns > 1))
   THEN Str(NrRun:3,StrNr)
   ELSE StrNr := '';
totalname := CONCAT(PathNameSimul,'EvalData',Trim(StrNr),'.OUT');
Assign(fEval,totalname);
Rewrite(fEval);
WRITELN(fEval,'AquaCrop 7.0 (June 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
WRITELN(fEval,'Evaluation of simulation results - Data');
Str(Zeval:5:2,TempString);
WRITELN(fEval,'                                                                                     for soil depth: ',Trim(TempString),' m');
WRITELN(fEval,'   Day Month  Year   DAP Stage   CCsim   CCobs   CCstd    Bsim      Bobs      Bstd   SWCsim  SWCobs   SWstd');
WRITELN(fEval,'                                   %       %       %     ton/ha    ton/ha    ton/ha    mm       mm      mm');
END; (* CreateEvalData *)


PROCEDURE ResetPreviousSum(VAR PreviousSum : rep_sum;
                           VAR SumETo,SumGDD,PreviousSumETo,PreviousSumGDD,
                               PreviousBmob,PreviousBsto : double);
BEGIN

WITH PreviousSum DO
  BEGIN
  Epot := 0.0;
  Tpot := 0.0;
  Rain := 0.0;
  Irrigation := 0.0;
  Infiltrated := 0.0;
  Runoff := 0.0;
  Drain := 0.0;
  Eact := 0.0;
  Tact := 0.0;
  TrW := 0.0;
  ECropCycle := 0.0;
  CRwater := 0.0;
  Biomass := 0.0;
  YieldPart := 0.0;
  BiomassPot := 0.0;
  BiomassUnlim := 0.0;
  SaltIn := 0.0;
  SaltOut := 0.0;
  CRsalt := 0.0;
  END;
SumETo := 0.0;
SumGDD := 0.0;
PreviousSumETo := 0.0;
PreviousSumGDD := 0.0;
PreviousBmob := 0.0;
PreviousBsto := 0.0;
END; (* ResetPreviousSum *)


PROCEDURE AdjustForWatertable;
Var Ztot, Zi : double;
    compi : INTEGER;
BEGIN
Ztot := 0;
FOR compi := 1 to NrCompartments DO
    BEGIN
    Ztot := Ztot + Compartment[compi].Thickness;
    Zi := Ztot - Compartment[compi].Thickness/2;
    IF (Zi >= (ZiAqua/100)) THEN // compartment at or below groundwater table
       BEGIN
       Compartment[compi].Theta := SoilLayer[Compartment[compi].Layer].SAT/100;
       DetermineSaltContent(ECiAqua,Compartment[compi]);
       END;
    END;
END; (* AdjustForWatertable *)


PROCEDURE GetGwtSet(DayNrIN : LongInt;
                    VAR GwT : rep_GwTable);
VAR f0 : TextFile;
    FileNameFull : string;
    DayNr1Gwt,DNrini : LongInt;
    i,dayi,monthi,yeari,Zini,yearACT : INTEGER;
    DayDouble,Zm,ECini : double;
    StringREAD : ShortString;
    TheEnd : BOOLEAN;
BEGIN
// FileNameFull
IF (GetGroundWaterFile() <> '(None)')
   THEN FileNameFull := GetGroundWaterFileFull()
   ELSE FileNameFull := CONCAT(PathNameProg,'GroundWater.AqC');

// Get DayNr1Gwt
Assign(f0,FileNameFull);
Reset(f0);
READLN(f0); // description
READLN(f0); // AquaCrop Version number
READLN(f0); // Mode
READLN(f0,dayi);
READLN(f0,monthi);
READLN(f0,yeari);
DetermineDayNr(dayi,monthi,yeari,DayNr1Gwt);

// Read first observation
FOR i := 1 TO 3 DO READLN(f0);
READLN(f0,StringREAD);
SplitStringInThreeParams(StringREAD,DayDouble,Zm,GwT.EC2);
GwT.DNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
GwT.Z2 := ROUND(Zm * 100);
IF (Eof(f0))
   THEN TheEnd := true
   ELSE TheEnd := false;

// Read next observations
IF TheEnd
   THEN BEGIN // only one observation
        GwT.DNr1 := Simulation.FromDayNr;
        GwT.Z1 := GwT.Z2;
        GwT.EC1 := GwT.EC2;
        GwT.DNr2 := Simulation.ToDayNr;
        END
   ELSE BEGIN
        // defined year
        IF (DayNr1Gwt > 365) THEN
           BEGIN
           IF (DayNrIN < GwT.DNr2)
              THEN BEGIN
                   // DayNrIN before 1st observation
                   GwT.DNr1 := Simulation.FromDayNr;
                   GwT.Z1 := GwT.Z2;
                   GwT.EC1 := GwT.EC2;
                   END
              ELSE BEGIN
                   // DayNrIN after or at 1st observation
                   REPEAT
                     GwT.DNr1 := GwT.DNr2;
                     GwT.Z1 := GwT.Z2;
                     GwT.EC1 := GwT.EC2;
                     READLN(f0,StringREAD);
                     SplitStringInThreeParams(StringREAD,DayDouble,Zm,GwT.EC2);
                     GwT.DNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
                     GwT.Z2 := ROUND(Zm * 100);
                     IF (DayNrIN < GwT.DNr2) THEN TheEnd := true;
                   UNTIL (TheEnd OR Eof(f0));
                   IF (NOT TheEnd) THEN // DayNrIN after last observation
                      BEGIN
                      GwT.DNr1 := GwT.DNr2;
                      GwT.Z1 := GwT.Z2;
                      GwT.EC1 := GwT.EC2;
                      GwT.DNr2 := Simulation.ToDayNr;
                      END;
                   END;
           END; // defined year

        // undefined year
        IF (DayNr1Gwt <= 365) THEN
           BEGIN
           DetermineDate(DayNrIN,dayi,monthi,yearACT);
           IF (yearACT <> 1901) THEN
              BEGIN
              // make 1st observation defined
              DetermineDate(GwT.DNr2,dayi,monthi,yeari);
              DetermineDayNr(dayi,monthi,yearACT,GwT.DNr2);
              END;
           IF (DayNrIN < GwT.DNr2)
              THEN BEGIN // DayNrIN before 1st observation
                   REPEAT
                     READLN(f0,StringREAD);
                     SplitStringInThreeParams(StringREAD,DayDouble,Zm,GwT.EC1);
                     GwT.DNr1 := DayNr1Gwt + ROUND(DayDouble) - 1;
                     DetermineDate(GwT.DNr1,dayi,monthi,yeari);
                     DetermineDayNr(dayi,monthi,yearACT,GwT.DNr1);
                     GwT.Z1 := ROUND(Zm * 100);
                   UNTIL Eof(f0);
                   GwT.DNr1 := GwT.DNr1 - 365;
                   END
              ELSE BEGIN
                   // save 1st observation
                   DNrini := GwT.DNr2;
                   Zini := GwT.Z2;
                   ECini := GwT.EC2;
                   // DayNrIN after or at 1st observation
                   REPEAT
                     GwT.DNr1 := GwT.DNr2;
                     GwT.Z1 := GwT.Z2;
                     GwT.EC1 := GwT.EC2;
                     READLN(f0,StringREAD);
                     SplitStringInThreeParams(StringREAD,DayDouble,Zm,GwT.EC2);
                     GwT.DNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
                     IF (yearACT <> 1901) THEN
                        BEGIN
                        // make observation defined
                        DetermineDate(GwT.DNr2,dayi,monthi,yeari);
                        DetermineDayNr(dayi,monthi,yearACT,GwT.DNr2);
                        END;
                     GwT.Z2 := ROUND(Zm * 100);
                     IF (DayNrIN < GwT.DNr2) THEN TheEnd := true;
                   UNTIL (TheEnd OR Eof(f0));
                   IF (NOT TheEnd) THEN // DayNrIN after last observation
                      BEGIN
                      GwT.DNr1 := GwT.DNr2;
                      GwT.Z1 := GwT.Z2;
                      GwT.EC1 := GwT.EC2;
                      GwT.DNr2 := DNrini + 365;
                      GwT.Z2 := Zini;
                      GwT.EC2 := ECini;
                      END;
                   END;
           END; // undefined year
        END; // more than 1 observation
Close(f0);
END; (* GetGwtSet *)




PROCEDURE RelationshipsForFertilityAndSaltStress(VAR Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF,
                                                 Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double);
VAR X10,X20,X30,X40,X50,X60,X70,X80,X90 : double;
    BioTop,BioLow : ShortInt;
    StrTop,StrLow : double;

BEGIN
// 1. Soil fertility
FracBiomassPotSF := 1;
// 1.a Soil fertility (Coeffb0,Coeffb1,Coeffb2 : Biomass-Soil Fertility stress)
IF Crop.StressResponse.Calibrated
   THEN BEGIN
        StressBiomassRelationship(Crop.DaysToCCini,Crop.GDDaysToCCini,
               Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,Crop.DaysToHarvest,
               Crop.DaysToFlowering,Crop.LengthFlowering,
               Crop.GDDaysToGermination,Crop.GDDaysToFullCanopy,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
               Crop.WPy,Crop.HI,
               Crop.CCo,Crop.CCx,Crop.CGC,Crop.GDDCGC,Crop.CDC,Crop.GDDCDC,
               Crop.KcTop,Crop.KcDecline,Crop.CCEffectEvapLate,
               Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,Crop.GDtranspLow,Crop.WP,Crop.dHIdt,CO2i,
               Crop.Day1,Crop.DeterminancyLinked,Crop.StressResponse,Crop.subkind,Crop.ModeCycle,
               Coeffb0,Coeffb1,Coeffb2,X10,X20,X30,X40,X50,X60,X70);
        END
   ELSE BEGIN
        Coeffb0 := undef_int;
        Coeffb1 := undef_int;
        Coeffb2 := undef_int;
        END;
// 1.b Soil fertility : FracBiomassPotSF
IF ((GetManagement_FertilityStress() <> 0) AND Crop.StressResponse.Calibrated) THEN
   BEGIN
   BioLow := 100;
   StrLow := 0;
   REPEAT
     BioTop := BioLow;
     StrTop := StrLow;
     BioLow := BioLow - 1;
     StrLow := Coeffb0 + Coeffb1*BioLow + Coeffb2*BioLow*BioLow;
   UNTIL ((StrLow >= GetManagement_FertilityStress())
               OR (BioLow <= 0) OR (StrLow >= 99.99));
   IF (StrLow >= 99.99) THEN StrLow := 100;
   IF (abs(StrLow-StrTop) < 0.001)
      THEN FracBiomassPotSF := BioTop
      ELSE FracBiomassPotSF := BioTop - (GetManagement_FertilityStress() - StrTop)/(StrLow-StrTop);
   FracBiomassPotSF := FracBiomassPotSF/100;
   END;

// 2. soil salinity (Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : CCx/KsSto - Salt stress)
IF (Simulation.SalinityConsidered = true)
   THEN BEGIN
        CCxSaltStressRelationship(Crop.DaysToCCini,Crop.GDDaysToCCini,
               Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,Crop.DaysToHarvest,
               Crop.DaysToFlowering,Crop.LengthFlowering,Crop.GDDaysToFlowering,Crop.GDDLengthFlowering,
               Crop.GDDaysToGermination,Crop.GDDaysToFullCanopy,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
               Crop.WPy,Crop.HI,
               Crop.CCo,Crop.CCx,Crop.CGC,Crop.GDDCGC,Crop.CDC,Crop.GDDCDC,
               Crop.KcTop,Crop.KcDecline,Crop.CCEffectEvapLate,
               Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,Crop.GDtranspLow,Crop.WP,Crop.dHIdt,CO2i,                     
               Crop.Day1,Crop.DeterminancyLinked,Crop.subkind,Crop.ModeCycle,
               Crop.CCsaltDistortion,
               Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,X10,X20,X30,X40,X50,X60,X70,X80,X90);
        END
   ELSE BEGIN
        Coeffb0Salt := undef_int;
        Coeffb1Salt := undef_int;
        Coeffb2Salt := undef_int;
        END;
END; (* RelationshipsForFertilityAndSaltStress *)



PROCEDURE CreateDailyClimFiles(FromSimDay,ToSimDay : LongInt);
VAR totalname,totalnameOUT : string;
    fETo,fRain,fTemp,fEToS,fRainS,fTempS : text;
    StringREAD : ShortString;
    i : INTEGER;
    RunningDay : LongInt;
BEGIN
// 1. ETo file
IF (GetEToFile() <> '(None)')
   THEN BEGIN
        totalname := GetEToFilefull();
        IF FileExists(totalname)
           THEN BEGIN
                // open file and find first day of simulation period
                CASE EToRecord.DataType OF
                  Daily   : BEGIN
                            Assign(fETo,totalname);
                            Reset(fETo);
                            READLN(fETo); // description
                            READLN(fETo); // time step
                            READLN(fETo); // day
                            READLN(fETo); // month
                            READLN(fETo); // year
                            READLN(fETo);
                            READLN(fETo);
                            READLN(fETo);
                            FOR i := EToRecord.FromDayNr TO (FromSimDay - 1) DO READLN(fETo);
                            READLN(fETo,ETo);
                            END;
                  Decadely: BEGIN
                            GetDecadeEToDataSet(FromSimDay,EToDataSet);
                            i := 1;
                            While (EToDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            ETo := EToDataSet[i].Param;
                            END;
                  Monthly : BEGIN
                            GetMonthlyEToDataSet(FromSimDay,EToDataSet);
                            i := 1;
                            While (EToDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            ETo := EToDataSet[i].Param;
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(PathNameSimul,'EToData.SIM');
                Assign(fEToS,totalnameOUT);
                Rewrite(fEToS);
                WRITELN(fEToS,ETo:10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE EToRecord.DataType OF
                         Daily   : BEGIN
                                   IF Eof(fETo)
                                      THEN BEGIN
                                           Reset(fETo);
                                           READLN(fETo); // description
                                           READLN(fETo); // time step
                                           READLN(fETo); // day
                                           READLN(fETo); // month
                                           READLN(fETo); // year
                                           READLN(fETo);
                                           READLN(fETo);
                                           READLN(fETo);
                                           READLN(fETo,ETo);
                                           END
                                      ELSE READLN(fETo,ETo);
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > EToDataSet[31].DayNr) THEN GetDecadeEToDataSet(RunningDay,EToDataSet);
                                   i := 1;
                                   While (EToDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   ETo := EToDataSet[i].Param;
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > EToDataSet[31].DayNr) THEN GetMonthlyEToDataSet(RunningDay,EToDataSet);
                                   i := 1;
                                   While (EToDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   ETo := EToDataSet[i].Param;
                                   END;
                         end;
                    WRITELN(fEToS,ETo:10:4);
                    END;
                // Close files
                IF (EToRecord.DataType = Daily) THEN Close(fETo);
                Close(fEToS);
                END
           ELSE BEGIN
                //
                END;
        END;
// 2. Rain File
IF (GetRainFile() <> '(None)')
   THEN BEGIN
        totalname := GetRainFilefull();
        IF FileExists(totalname)
        THEN BEGIN
             // open file and find first day of simulation period
             CASE RainRecord.DataType OF
                  Daily   : BEGIN
                            Assign(fRain,totalname);
                            Reset(fRain);
                            READLN(fRain); // description
                            READLN(fRain); // time step
                            READLN(fRain); // day
                            READLN(fRain); // month
                            READLN(fRain); // year
                            READLN(fRain);
                            READLN(fRain);
                            READLN(fRain);
                            FOR i := RainRecord.FromDayNr TO (FromSimDay - 1) DO READLN(fRain);
                            READLN(fRain,Rain);
                            END;
                  Decadely: BEGIN
                            GetDecadeRainDataSet(FromSimDay,RainDataSet);
                            i := 1;
                            While (RainDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            Rain := RainDataSet[i].Param;
                            END;
                  Monthly : BEGIN
                            GetMonthlyRainDataSet(FromSimDay,RainDataSet);
                            i := 1;
                            While (RainDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            Rain := RainDataSet[i].Param;
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(PathNameSimul,'RainData.SIM');
                Assign(fRainS,totalnameOUT);
                Rewrite(fRainS);
                WRITELN(fRainS,Rain:10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE RainRecord.DataType OF
                         Daily   : BEGIN
                                   IF Eof(fRain)
                                      THEN BEGIN
                                           Reset(fRain);
                                           READLN(fRain); // description
                                           READLN(fRain); // time step
                                           READLN(fRain); // day
                                           READLN(fRain); // month
                                           READLN(fRain); // year
                                           READLN(fRain);
                                           READLN(fRain);
                                           READLN(fRain);
                                           READLN(fRain,Rain);
                                           END
                                      ELSE READLN(fRain,Rain);
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > RainDataSet[31].DayNr) THEN GetDecadeRainDataSet(RunningDay,RainDataSet);
                                   i := 1;
                                   While (RainDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   Rain := RainDataSet[i].Param;
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > RainDataSet[31].DayNr) THEN GetMonthlyRainDataSet(RunningDay,RainDataSet);
                                   i := 1;
                                   While (RainDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   Rain := RainDataSet[i].Param;
                                   END;
                         end;
                    WRITELN(fRainS,Rain:10:4);
                    END;
             // Close files
             IF (RainRecord.DataType = Daily) THEN Close(fRain);
             Close(fRainS);
             END
        ELSE BEGIN
             //
             END;
        END;

// 3. Temperature file
IF (TemperatureFile <> '(None)')
   THEN BEGIN
        totalname := TemperatureFilefull;
        IF FileExists(totalname)
           THEN BEGIN
                // open file and find first day of simulation period
                CASE TemperatureRecord.DataType OF
                  Daily   : BEGIN
                            Assign(fTemp,totalname);
                            Reset(fTemp);
                            READLN(fTemp); // description
                            READLN(fTemp); // time step
                            READLN(fTemp); // day
                            READLN(fTemp); // month
                            READLN(fTemp); // year
                            READLN(fTemp);
                            READLN(fTemp);
                            READLN(fTemp);
                            FOR i := TemperatureRecord.FromDayNr TO (FromSimDay - 1) DO READLN(fTemp);
                            READLN(fTemp,StringREAD);  // i.e. DayNri
                            SplitStringInTwoParams(StringREAD,Tmin,Tmax);
                            END;
                  Decadely: BEGIN
                            GetDecadeTemperatureDataSet(FromSimDay,TminDataSet,TmaxDataSet);
                            i := 1;
                            While (TminDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            Tmin := TminDataSet[i].Param;
                            Tmax := TmaxDataSet[i].Param;
                            END;
                  Monthly : BEGIN
                            GetMonthlyTemperatureDataSet(FromSimDay,TminDataSet,TmaxDataSet);
                            i := 1;
                            While (TminDataSet[i].DayNr <> FromSimDay) Do i := i+1;
                            Tmin := TminDataSet[i].Param;
                            Tmax := TmaxDataSet[i].Param;
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(PathNameSimul,'TempData.SIM');
                Assign(fTempS,totalnameOUT);
                Rewrite(fTempS);
                WRITELN(fTempS,Tmin:10:4,Tmax:10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE TemperatureRecord.DataType OF
                         Daily   : BEGIN
                                   IF Eof(fTemp)
                                      THEN BEGIN
                                           Reset(fTemp);
                                           READLN(fTemp); // description
                                           READLN(fTemp); // time step
                                           READLN(fTemp); // day
                                           READLN(fTemp); // month
                                           READLN(fTemp); // year
                                           READLN(fTemp);
                                           READLN(fTemp);
                                           READLN(fTemp);
                                           READLN(fTemp,StringREAD);
                                           SplitStringInTwoParams(StringREAD,Tmin,Tmax);
                                           END
                                      ELSE READLN(fTemp,Tmin,Tmax);
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > TminDataSet[31].DayNr) THEN GetDecadeTemperatureDataSet(RunningDay,TminDataSet,TmaxDataSet);
                                   i := 1;
                                   While (TminDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   Tmin := TminDataSet[i].Param;
                                   Tmax := TmaxDataSet[i].Param;
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > TminDataSet[31].DayNr) THEN GetMonthlyTemperatureDataSet(RunningDay,TminDataSet,TmaxDataSet);
                                   i := 1;
                                   While (TminDataSet[i].DayNr <> RunningDay) Do i := i+1;
                                   Tmin := TminDataSet[i].Param;
                                   Tmax := TmaxDataSet[i].Param;
                                   END;
                         end;
                    WRITELN(fTempS,Tmin:10:4,Tmax:10:4);
                    END;
                // Close files
                IF (TemperatureRecord.DataType = Daily) THEN Close(fTemp);
                Close(fTempS);
                END
           ELSE BEGIN
                //
                END;
        END;
END; (* CreateDailyClimFiles *)


PROCEDURE OpenClimFilesAndGetDataFirstDay(FirstDayNr : LongInt;
                                          VAR fEToSIM,fRainSIM,fTempSIM : text);
VAR totalname : string;
    i : LongInt;
BEGIN
// ETo file
IF (GetEToFile() <> '(None)') THEN
   BEGIN
   totalname := CONCAT(PathNameSimul,'EToData.SIM');
   Assign(fEToSIM,totalname);
   Reset(fEToSIM);
   IF (FirstDayNr = Simulation.FromDayNr)
      THEN READLN(fEToSIM,ETo)
      ELSE BEGIN
           FOR i := Simulation.FromDayNr TO (FirstDayNr - 1) DO READLN(fEToSIM,ETo);
           READLN(fEToSIM,ETo);
           END;
   END;
// Rain file
IF (GetRainFile() <> '(None)') THEN
   BEGIN
   totalname := CONCAT(PathNameSimul,'RainData.SIM');
   Assign(fRainSIM,totalname);
   Reset(fRainSIM);
   IF (FirstDayNr = Simulation.FromDayNr)
      THEN READLN(fRainSIM,Rain)
      ELSE BEGIN
           FOR i := Simulation.FromDayNr TO (FirstDayNr - 1) DO READLN(fRainSIM,Rain);
           READLN(fRainSIM,Rain);
           END;
   END;
// Temperature file
IF (TemperatureFile <> '(None)')
   THEN BEGIN
        totalname := CONCAT(PathNameSimul,'TempData.SIM');
        Assign(fTempSIM,totalname);
        Reset(fTempSIM);
        IF (FirstDayNr = Simulation.FromDayNr)
           THEN READLN(fTempSIM,Tmin,Tmax)
           ELSE BEGIN
                FOR i := Simulation.FromDayNr TO (FirstDayNr - 1) DO READLN(fTempSIM,Tmin,Tmax);
                READLN(fTempSIM,Tmin,Tmax);
                END;
        END
   ELSE BEGIN
        Tmin := SimulParam.Tmin;
        Tmax := SimulParam.Tmax;
        END;
END; (* OpenClimFilesAndGetDataFirstDay *)


PROCEDURE GetSumGDDBeforeSimulation(VAR SumGDDtillDay,SumGDDtillDayM1 : double);
VAR totalname : string;
    fTemp : text;
    i : LongInt;
    StringREAD : ShortString;
    DayX : LongInt;
BEGIN
Simulation.SumGDD := 0;
IF (TemperatureFile <> '(None)')
   THEN BEGIN
        totalname := TemperatureFilefull;
        IF FileExists(totalname)
           THEN BEGIN
                CASE TemperatureRecord.DataType OF
                  Daily   : BEGIN
                            Assign(fTemp,totalname);
                            Reset(fTemp);
                            READLN(fTemp); // description
                            READLN(fTemp); // time step
                            READLN(fTemp); // day
                            READLN(fTemp); // month
                            READLN(fTemp); // year
                            READLN(fTemp);
                            READLN(fTemp);
                            READLN(fTemp);
                            // days before first day of simulation (= DayNri)
                            FOR i := TemperatureRecord.FromDayNr TO (DayNri - 1) DO
                                BEGIN
                                IF (i < Crop.Day1)
                                   THEN READLN(fTemp)
                                   ELSE BEGIN
                                        READLN(fTemp,StringREAD);
                                        SplitStringInTwoParams(StringREAD,Tmin,Tmax);
                                        Simulation.SumGDD := Simulation.SumGDD
                                                             + DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
                                        END;
                                END;
                            Close(fTemp);
                            END;
                  Decadely: BEGIN
                            DayX := Crop.Day1;
                            // first day of cropping
                            GetDecadeTemperatureDataSet(DayX,TminDataSet,TmaxDataSet);
                            i := 1;
                            While (TminDataSet[i].DayNr <> DayX) Do i := i+1;
                            Tmin := TminDataSet[i].Param;
                            Tmax := TmaxDataSet[i].Param;
                            Simulation.SumGDD := DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
                            // next days
                            WHILE (DayX < DayNri) DO
                                 BEGIN
                                 DayX := DayX + 1;
                                 IF (DayX > TminDataSet[31].DayNr) THEN
                                    BEGIN
                                    GetDecadeTemperatureDataSet(DayX,TminDataSet,TmaxDataSet);
                                    i := 0;
                                    END;
                                 i := i+1;
                                 Tmin := TminDataSet[i].Param;
                                 Tmax := TmaxDataSet[i].Param;
                                 Simulation.SumGDD := Simulation.SumGDD
                                         + DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
                                 END;
                            END;
                  Monthly : BEGIN
                            DayX := Crop.Day1;
                            // first day of cropping
                            GetMonthlyTemperatureDataSet(DayX,TminDataSet,TmaxDataSet);
                            i := 1;
                            While (TminDataSet[i].DayNr <> DayX) Do i := i+1;
                            Tmin := TminDataSet[i].Param;
                            Tmax := TmaxDataSet[i].Param;
                            Simulation.SumGDD := DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
                            // next days
                            WHILE (DayX < DayNri) DO
                                  BEGIN
                                  DayX := DayX + 1;
                                  IF (DayX > TminDataSet[31].DayNr) THEN
                                     BEGIN
                                     GetMonthlyTemperatureDataSet(DayX,TminDataSet,TmaxDataSet);
                                     i := 0;
                                     END;
                                  i := i+1;
                                  Tmin := TminDataSet[i].Param;
                                  Tmax := TmaxDataSet[i].Param;
                                  Simulation.SumGDD := Simulation.SumGDD
                                         + DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
                                  END;
                            END;
                  end;
                END
           ELSE BEGIN
                //
                END;
        END;
IF (TemperatureFile = '(None)')
   THEN BEGIN
        Simulation.SumGDD := DegreesDay(Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,SimulParam.GDDMethod) * (DayNri - Crop.Day1 + 1);
        IF (Simulation.SumGDD < 0) THEN Simulation.SumGDD := 0;
        SumGDDtillDay := Simulation.SumGDD;
        SumGDDtillDayM1 := DegreesDay(Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,SimulParam.GDDMethod) * (DayNri - Crop.Day1);
        IF (SumGDDtillDayM1 < 0) THEN SumGDDtillDayM1 := 0;
        end
   ELSE BEGIN
        SumGDDtillDay := Simulation.SumGDD;
        SumGDDtillDayM1 := SumGDDtillDay - DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
        END;
END; (* GetSumGDDBeforeSimulation *)


PROCEDURE OpenIrrigationFile(VAR fIrri : text);
VAR totalname : string;
    StringREAD : ShortString;
    i,DNr : INTEGER;
    Ir1,Ir2 : double;
    VersionNr : double;

BEGIN
IF ((IrriMode = Manual) OR (IrriMode = Generate)) THEN
   BEGIN
   IF (GetIrriFile() <> '(None)')
      THEN totalname := IrriFileFull
      ELSE totalname := CONCAT(PathNameProg,'IrriSchedule.AqC');
   Assign(fIrri,totalname);
   Reset(fIrri);
   READLN(fIrri); // description
   READLN(fIrri,VersionNr); // AquaCrop version
   IF (ROUND(VersionNr*10) < 32)
      THEN GlobalIrriECw := true
      ELSE GlobalIrriECw := false;
   FOR i := 1 TO 6 DO READLN(fIrri);  // irrigation info (already loaded)
   CASE IrriMode OF
        Manual   : BEGIN
                   IF (IrriFirstDayNr = undef_int)
                      THEN DNr := DayNri - Crop.Day1 + 1
                      ELSE DNr := DayNri - IrriFirstDayNr + 1;
                   REPEAT
                   IF Eof(fIrri)
                      THEN IrriInfoRecord1.NoMoreInfo := true
                      ELSE BEGIN
                           IrriInfoRecord1.NoMoreInfo := false;
                           READLN(fIrri,StringREAD);
                           IF GlobalIrriECw
                              THEN SplitStringInTwoParams(StringREAD,Ir1,Ir2)
                              ELSE SplitStringInThreeParams(StringREAD,Ir1,Ir2,Simulation.IrriECw);
                           IrriInfoRecord1.TimeInfo := ROUND(Ir1);
                           IrriInfoRecord1.DepthInfo := ROUND(Ir2);
                           END;
                   UNTIL ((IrriInfoRecord1.NoMoreInfo) OR (IrriInfoRecord1.TimeInfo >= DNr));
                   END;
        Generate : BEGIN
                   FOR i := 1 TO 2 DO READLN(fIrri); // time and depth criterion (already loaded)
                   IrriInfoRecord1.NoMoreInfo := false;
                   IF (ROUND(VersionNr*10) < 32)
                      THEN READLN(fIrri,IrriInfoRecord1.FromDay,IrriInfoRecord1.TimeInfo,IrriInfoRecord1.DepthInfo)
                      ELSE READLN(fIrri,IrriInfoRecord1.FromDay,IrriInfoRecord1.TimeInfo,
                                  IrriInfoRecord1.DepthInfo,Simulation.IrriECw);
                   IF Eof(fIrri)
                      THEN IrriInfoRecord1.ToDay := Crop.DayN - Crop.Day1 + 1
                      ELSE BEGIN
                           IrriInfoRecord2.NoMoreInfo := false;
                           IF GlobalIrriECw
                              THEN READLN(fIrri,IrriInfoRecord2.FromDay,IrriInfoRecord2.TimeInfo,IrriInfoRecord2.DepthInfo)
                              ELSE READLN(fIrri,IrriInfoRecord2.FromDay,IrriInfoRecord2.TimeInfo,
                                          IrriInfoRecord2.DepthInfo,Simulation.IrriECw);
                           IrriInfoRecord1.ToDay := IrriInfoRecord2.FromDay - 1;
                           END;
                   END;
        end;
   END;
END; (* OpenIrrigationFile *)


PROCEDURE GetNextHarvest;
VAR InfoLoaded : BOOLEAN;
    DayNrXX : LongInt;
BEGIN
CASE GetManagement_Cuttings_Generate() OF
 false: BEGIN
        IF (NOT Eof(fCuts))
           THEN BEGIN
                READLN(fCuts,CutInfoRecord1.FromDay);
                CutInfoRecord1.NoMoreInfo := false;
                IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) THEN
                   BEGIN // scroll to start growing cycle
                   DayNrXX := GetManagement_Cuttings_FirstDayNr() + CutInfoRecord1.FromDay -1;
                   WHILE ((DayNrXX < Crop.Day1) AND (CutInfoRecord1.NoMoreInfo = false)) DO
                     BEGIN
                     IF (NOT Eof(fCuts))
                        THEN BEGIN
                             READLN(fCuts,CutInfoRecord1.FromDay);
                             DayNrXX := GetManagement_Cuttings_FirstDayNr() + CutInfoRecord1.FromDay -1;
                             END
                        ELSE CutInfoRecord1.NoMoreInfo := true;
                     END; // while loop
                   END; // scroll to start growing cycle
                END
           ELSE CutInfoRecord1.NoMoreInfo := true;
        END;
 true : BEGIN
        IF (NrCut = 0) THEN
           BEGIN
           CASE GetManagement_Cuttings_Criterion() OF
                IntDay : READLN(fCuts,CutInfoRecord1.FromDay,CutInfoRecord1.IntervalInfo);
                IntGDD : READLN(fCuts,CutInfoRecord1.FromDay,CutInfoRecord1.IntervalGDD);
                DryB,DryY,FreshY : READLN(fCuts,CutInfoRecord1.FromDay,CutInfoRecord1.MassInfo);
                end;
           IF (CutInfoRecord1.FromDay < GetManagement_Cuttings_Day1()) THEN CutInfoRecord1.FromDay := GetManagement_Cuttings_Day1();
           END;
        InfoLoaded := false;
        REPEAT
        IF (NOT Eof(fCuts))
           THEN BEGIN
                CASE GetManagement_Cuttings_Criterion() OF
                     IntDay : READLN(fCuts,CutInfoRecord2.FromDay,CutInfoRecord2.IntervalInfo);
                     IntGDD : READLN(fCuts,CutInfoRecord2.FromDay,CutInfoRecord2.IntervalGDD);
                     DryB,DryY,FreshY : READLN(fCuts,CutInfoRecord2.FromDay,CutInfoRecord2.MassInfo);
                     end;
                IF (CutInfoRecord2.FromDay < GetManagement_Cuttings_Day1()) THEN CutInfoRecord2.FromDay := GetManagement_Cuttings_Day1();
                IF (CutInfoRecord2.FromDay <= CutInfoRecord1.FromDay)
                   THEN BEGIN // CutInfoRecord2 becomes CutInfoRecord1
                        CutInfoRecord1.FromDay := CutInfoRecord2.FromDay;
                        CASE GetManagement_Cuttings_Criterion() OF
                             IntDay : CutInfoRecord1.IntervalInfo := CutInfoRecord2.IntervalInfo;
                             IntGDD : CutInfoRecord1.IntervalGDD := CutInfoRecord2.IntervalGDD;
                             DryB,DryY,FreshY : CutInfoRecord1.MassInfo := CutInfoRecord2.MassInfo;
                             end;
                        CutInfoRecord1.NoMoreInfo := false;
                        END
                   ELSE BEGIN // complete CutInfoRecord1
                        CutInfoRecord1.ToDay := CutInfoRecord2.FromDay - 1;
                        CutInfoRecord1.NoMoreInfo := false;
                        IF (GetManagement_Cuttings_NrDays() <> undef_int) THEN
                           BEGIN
                           IF (CutInfoRecord1.ToDay > (GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1)) THEN
                              BEGIN
                              CutInfoRecord1.ToDay := GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1;
                              CutInfoRecord1.NoMoreInfo := true;
                              END;
                           END;
                        InfoLoaded := true;
                        END;
                END
           ELSE BEGIN // Eof(fCuts)
                IF (NrCut > 0) THEN
                   BEGIN  // CutInfoRecord2 becomes CutInfoRecord1
                   CutInfoRecord1.FromDay := CutInfoRecord2.FromDay;
                   CASE GetManagement_Cuttings_Criterion() OF
                        IntDay : CutInfoRecord1.IntervalInfo := CutInfoRecord2.IntervalInfo;
                        IntGDD : CutInfoRecord1.IntervalGDD := CutInfoRecord2.IntervalGDD;
                        DryB,DryY,FreshY : CutInfoRecord1.MassInfo := CutInfoRecord2.MassInfo;
                        end;
                   END;
                CutInfoRecord1.ToDay := Crop.DaysToHarvest;
                IF (GetManagement_Cuttings_NrDays() <> undef_int) THEN
                   BEGIN
                   IF (CutInfoRecord1.ToDay > (GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1)) THEN
                      CutInfoRecord1.ToDay := GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1;
                   END;
                CutInfoRecord1.NoMoreInfo := true;
                InfoLoaded := true;
                END;
        UNTIL (InfoLoaded = true);
        END;
 end;
END; (* GetNextHarvest *)




PROCEDURE OpenHarvestInfo(VAR fCuts : text);
VAR totalname : string;
    i : ShortInt;
BEGIN
IF (getManFile() <> '(None)')
   THEN totalname := GetManFileFull()
   ELSE totalname := CONCAT(PathNameSimul,'Cuttings.AqC');
Assign(fCuts,totalname);
Reset(fCuts);
READLN(fCuts); // description
READLN(fCuts); // AquaCrop version
IF (GetManFile() <> '(None)') THEN For i:= 1 to 10 DO READLN(fCuts); // management info
FOR i := 1 TO 12 DO READLN(fCuts);  // cuttings info (already loaded)
GetNextHarvest;
END; (* OpenHarvestInfo *)


// extra for output of daily results  -----------------------------
PROCEDURE DetermineGrowthStage(Dayi : LongInt;
                               CCiPrev : Double;
                               VAR Code : ShortInt);
VAR VirtualDay : INTEGER;
BEGIN
VirtualDay := Dayi - Simulation.DelayedDays - Crop.Day1;
IF (VirtualDay < 0)
   THEN Code := 0 // before cropping period
   ELSE BEGIN
        IF (VirtualDay < Crop.DaysToGermination)
           THEN Code := 1 //sown --> emergence OR transplant recovering
           ELSE BEGIN
                Code := 2; // vegetative development
                IF ((Crop.subkind = Grain) AND (VirtualDay >= Crop.DaysToFlowering))
                   THEN BEGIN
                        IF (VirtualDay < (Crop.DaysToFlowering + Crop.LengthFlowering))
                           THEN Code := 3 // flowering
                           ELSE Code := 4; // yield formation
                        END;
                IF ((Crop.subkind = Tuber) AND (VirtualDay >= Crop.DaysToFlowering))
                   THEN Code := 4; // yield formation
                IF ((VirtualDay > Crop.DaysToGermination) AND (CCiPrev <= 0))
                   THEN Code := Undef_int;  // no growth stage
                IF (VirtualDay >= (Crop.Length[1]+Crop.Length[2]+Crop.Length[3]+Crop.Length[4]))
                   THEN Code := 0; // after cropping period
                END;
        END;
END; (* DetermineGrowthStage *)





PROCEDURE InitializeSimulationRun;
VAR VAL100 : double;
    TempString : STRING;
    i,tHImax,DNr1,DNr2,Dayi,DayCC : integer;
    WPi,ValDouble,SumGDDforDayCC : double;
    DayNrCut : LongInt;
    CCiniMin,CCiniMax,RatDGDD,fWeed,BRatioNoWeeds,fi : double;
    Cweed : ShortInt;

    Day1,Month1,Year1 : INTEGER;
    FertStress : shortint;
    
BEGIN
//1. Adjustments at start
//1.1 Adjust soil water and salt content if water table IN soil profile
CheckForWaterTableInProfile((ZiAqua/100),Compartment,WaterTableInProfile);
IF WaterTableInProfile THEN AdjustForWatertable;
IF (NOT SimulParam.ConstGwt) THEN GetGwtSet(Simulation.FromDayNr,GwTable);

// 1.2 Check if FromDayNr simulation needs to be adjusted from previous run if Keep initial SWC
IF ((GetSWCIniFile() = 'KeepSWC') AND (NextSimFromDayNr <> undef_int)) THEN
   BEGIN  // assign the adjusted DayNr defined in previous run
   IF (NextSimFromDayNr <= Crop.Day1) THEN Simulation.FromDayNr := NextSimFromDayNr;
   END;
NextSimFromDayNr := undef_int;

// 2. initial settings for Crop
Crop.pActStom := Crop.pdef;
Crop.pSenAct := Crop.pSenescence;
Crop.pLeafAct := Crop.pLeafDefUL;
EvapoEntireSoilSurface := true;
Simulation.EvapLimitON := false;
Simulation.EvapWCsurf := 0;
Simulation.EvapZ := (EvapZmin/100);
Simulation.SumEToStress := 0;
CCxWitheredTpot := 0; // for calculation Maximum Biomass and considering soil fertility stress
CCxWitheredTpotNoS := 0; //  for calculation Maximum Biomass unlimited soil fertility
Simulation.DayAnaero := 0; // days of anaerobic condictions in global root zone
// germination
IF ((Crop.Planting = Seed) AND (Simulation.FromDayNr <= Crop.Day1))
   THEN Simulation.Germinate := false
   ELSE BEGIN
        Simulation.Germinate := true;
        // since already germinated no protection required
        Simulation.ProtectedSeedling := false;
        END;
// delayed germination
Simulation.DelayedDays := 0;

// 3. create temperature file covering crop cycle
IF (TemperatureFile <> '(None)') THEN
   BEGIN
   IF (Simulation.ToDayNr < Crop.DayN)
      THEN TemperatureFileCoveringCropPeriod(Crop.Day1,Simulation.TodayNr)
      ELSE TemperatureFileCoveringCropPeriod(Crop.Day1,Crop.DayN);
   END;

// 4. CO2 concentration during cropping period
DNr1 := Simulation.FromDayNr;
IF (Crop.Day1 > Simulation.FromDayNr) THEN DNr1 := Crop.Day1;
DNr2 := Simulation.ToDayNr;
IF (Crop.DayN < Simulation.ToDayNr) THEN DNr2 := Crop.DayN;
CO2i := CO2ForSimulationPeriod(DNr1,DNr2);

// 5. seasonals stress coefficients
Simulation.SalinityConsidered := (((Crop.ECemin <> undef_int) AND (Crop.ECemax <> undef_int)) AND (Crop.ECemin < Crop.ECemax));
IF (IrriMode = Inet) THEN Simulation.SalinityConsidered := false;
StressTot.NrD := undef_int;
StressTot.Salt := 0;
StressTot.Temp := 0;
StressTot.Exp := 0;
StressTot.Sto := 0;
StressTot.Weed := 0;

// 6. Soil fertility stress
// Coefficients for soil fertility - biomass relationship
  // AND for Soil salinity - CCx/KsSto relationship
RelationshipsForFertilityAndSaltStress(Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt);

// No soil fertility stress
IF (GetManagement_FertilityStress() <= 0) THEN SetManagement_FertilityStress(0);

// Reset soil fertility parameters to selected value in management
CropStressParametersSoilFertility(Crop.StressResponse,GetManagement_FertilityStress(),Simulation.EffectStress);
FertStress := GetManagement_FertilityStress();
TimeToMaxCanopySF(Crop.CCo,Crop.CGC,Crop.CCx,Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,
                    Crop.DaysToFlowering,Crop.LengthFlowering,Crop.DeterminancyLinked,
                    Crop.DaysToFullCanopySF,Simulation.EffectStress.RedCGC,
                    Simulation.EffectStress.RedCCX,FertStress);
SetManagement_FertilityStress(FertStress);
PreviousStressLevel := GetManagement_FertilityStress();
StressSFadjNEW := GetManagement_FertilityStress();
// soil fertility and GDDays
IF (Crop.ModeCycle = GDDays) THEN
   BEGIN
   IF (GetManagement_FertilityStress() <> 0)
      THEN Crop.GDDaysToFullCanopySF := GrowingDegreeDays(Crop.DaysToFullCanopySF,Crop.Day1,Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax)
      ELSE Crop.GDDaysToFullCanopySF := Crop.GDDaysToFullCanopy;
   END;

// Maximum sum Kc (for reduction WP in season if soil fertility stress)
SumKcTop := SeasonalSumOfKcPot(Crop.DaysToCCini,Crop.GDDaysToCCini,
                 Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,Crop.DaysToHarvest,
                 Crop.GDDaysToGermination,Crop.GDDaysToFullCanopy,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                 Crop.CCo,Crop.CCx,Crop.CGC,Crop.GDDCGC,Crop.CDC,Crop.GDDCDC,
                 Crop.KcTop,Crop.KcDecline,Crop.CCEffectEvapLate,
                 Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,Crop.GDtranspLow,CO2i,
                 Crop.ModeCycle);
SumKcTopStress := SumKcTop * FracBiomassPotSF;
SumKci := 0;

// 7. weed infestation and self-thinning of herbaceous perennial forage crops
// CC expansion due to weed infestation and/or CC decrease as a result of self-thinning
// 7.1 initialize
Simulation.RCadj := GetManagement_WeedRC();
Cweed := 0;
IF (Crop.subkind = Forage)
   THEN fi := MultiplierCCxSelfThinning(Simulation.YearSeason,Crop.YearCCx,Crop.CCxRoot)
   ELSE fi := 1;
// 7.2 fweed
IF (GetManagement_WeedRC() > 0)
   THEN BEGIN
        fWeedNoS := CCmultiplierWeed(GetManagement_WeedRC(),Crop.CCx,GetManagement_WeedShape());
        CCxCropWeedsNoSFstress := ROUND(((100* Crop.CCx * fWeedNoS) + 0.49))/100; // reference for plot with weed
        IF (GetManagement_FertilityStress() > 0)
           THEN BEGIN
                fWeed := 1;
                IF ((fi > 0) AND (Crop.subkind = Forage)) THEN
                   BEGIN
                   Cweed := 1;
                   IF (fi > 0.005)
                      THEN BEGIN // calculate the adjusted weed cover
                           Simulation.RCadj := ROUND(GetManagement_WeedRC()
                                 //+ Cweed*(1-fi)*Crop.CCx*(1-Simulation.EffectStress.RedCCX/100)*Management.WeedAdj);
                                 + Cweed*(1-fi)*Crop.CCx*(1-Simulation.EffectStress.RedCCX/100)*GetManagement_WeedAdj()/100);
                           IF (Simulation.RCadj < (100 * (1- fi/(fi + (1-fi)*(GetManagement_WeedAdj()/100)))))
                              THEN Simulation.RCadj := ROUND(100 * (1- fi/(fi + (1-fi)*(GetManagement_WeedAdj()/100))));
                           IF (Simulation.RCadj > 100) THEN Simulation.RCadj := 98;
                           END
                      ELSE Simulation.RCadj := 100;
                   END;
                END
           ELSE BEGIN
                IF (Crop.subkind = Forage)
                   THEN fweed := CCmultiplierWeedAdjusted(GetManagement_WeedRC(),Crop.CCx,GetManagement_WeedShape(),
                                        fi,Simulation.YearSeason,GetManagement_WeedAdj(),Simulation.RCadj)
                   ELSE fWeed := fWeedNoS;
                END;
        END
   ELSE BEGIN
        fWeedNoS := 1;
        fWeed := 1;
        CCxCropWeedsNoSFstress := Crop.CCx;
        END;
// 7.3 CC total due to weed infestation
CCxTotal := fWeed * Crop.CCx * (fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100);
CDCTotal := Crop.CDC * (fWeed*Crop.CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29)/
                       (Crop.CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29);
GDDCDCTotal := Crop.GDDCDC * (fWeed*Crop.CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29)/
                       (Crop.CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29);
IF (Crop.subkind = Forage)
   THEN fi := MultiplierCCoSelfThinning(Simulation.YearSeason,Crop.YearCCx,Crop.CCxRoot)
   ELSE fi := 1;
CCoTotal := fWeed * Crop.CCo * (fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100);

// 8. prepare output files
// Not applicable

// 9. first day
StartMode := true;
PreDay := (NOT Simulation.ResetIniSWC);
DayNri := Simulation.FromDayNr;
DetermineDate(Simulation.FromDayNr,Day1,Month1,Year1); // start simulation run
NoYear := (Year1 = 1901);  // for output file


// 10. Climate
// create climate files
CreateDailyClimFiles(Simulation.FromDayNr,Simulation.ToDayNr);
// climatic data for first day
OpenClimFilesAndGetDataFirstDay(DayNri,fEToSIM,fRainSIM,fTempSIM);

// Sum of GDD before start of simulation
Simulation.SumGDD := 0;
Simulation.SumGDDfromDay1 := 0;
IF ((Crop.ModeCycle = GDDays) AND (Crop.Day1 < DayNri))
   THEN GetSumGDDBeforeSimulation(Simulation.SumGDD,Simulation.SumGDDfromDay1); // GDDays before start of simulation
SumGDDPrev := Simulation.SumGDDfromDay1;

// Sum of GDD at end of first day
GDDayi := DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
IF (DayNri >= Crop.Day1)
   THEN BEGIN
        IF (DayNri = Crop.Day1) THEN Simulation.SumGDD := Simulation.SumGDD + GDDayi;
        Simulation.SumGDDfromDay1 := Simulation.SumGDDfromDay1 + GDDayi;
        END;
// Reset cummulative sums of ETo and GDD for Run output
SumETo := 0;
SumGDD := 0;


// 11. Irrigation
IrriInterval := 1;
GlobalIrriECw := true; // In Versions < 3.2 - Irrigation water quality is not yet recorded on file
OpenIrrigationFile(fIrri);


// 12. Adjusted time when starting as regrowth
IF (Crop.DaysToCCini <> 0)
   THEN BEGIN  // regrowth
        GDDTadj := undef_int;
        GDDayFraction := undef_int;
        IF (Crop.DaysToCCini = undef_int)
           THEN Tadj := Crop.DaysToFullCanopy - Crop.DaysToGermination
           ELSE Tadj := Crop.DaysToCCini;
        DayFraction := (Crop.DaysToSenescence-Crop.DaysToFullCanopy)/(Tadj + Crop.DaysToGermination + (Crop.DaysToSenescence-Crop.DaysToFullCanopy) );
        IF (Crop.ModeCycle = GDDays) THEN
           BEGIN
           IF (Crop.GDDaysToCCini = undef_int)
              THEN GDDTadj := Crop.GDDaysToFullCanopy - Crop.GDDaysToGermination
              ELSE GDDTadj := Crop.GDDaysToCCini;
           GDDayFraction := (Crop.GDDaysToSenescence-Crop.GDDaysToFullCanopy)/(GDDTadj + Crop.GDDaysToGermination + (Crop.GDDaysToSenescence-Crop.GDDaysToFullCanopy));
           END;
        END
   ELSE BEGIN // sowing or transplanting
        Tadj := 0;
        GDDTadj := 0;
        DayFraction := undef_int;
        GDDayFraction := undef_int;
        END;


// 13. Initial canopy cover
// 13.1 default value
// 13.1a RatDGDD for simulation of CanopyCoverNoStressSF (CCi with decline)
RatDGDD := 1;
IF (Crop.ModeCycle = GDDays) THEN
   BEGIN
   IF (Crop.GDDaysToFullCanopySF < Crop.GDDaysToSenescence)
      THEN RatDGDD := (Crop.DaysToSenescence-Crop.DaysToFullCanopySF)/(Crop.GDDaysToSenescence-Crop.GDDaysToFullCanopySF);
   END;
// 13.1b DayCC for initial canopy cover
Dayi := DayNri - Crop.Day1;
IF (Crop.DaysToCCini = 0)
   THEN BEGIN // sowing or transplant
        DayCC := Dayi;
        DayFraction := undef_int;
        END
   ELSE BEGIN
        // adjust time (calendar days) for regrowth
        DayCC := Dayi + Tadj + Crop.DaysToGermination; // adjusted time scale
        IF (DayCC > Crop.DaysToHarvest) THEN DayCC := Crop.DaysToHarvest; // special case where L123 > L1234
        IF (DayCC > Crop.DaysToFullCanopy) THEN
           BEGIN
           IF (Dayi <= Crop.DaysToSenescence)
              THEN DayCC := Crop.DaysToFullCanopy  + ROUND(DayFraction * (Dayi+Tadj+Crop.DaysToGermination - Crop.DaysToFullCanopy)) // slow down
              ELSE DayCC := Dayi; // switch time scale
           END;
        END;
// 13.1c SumGDDayCC for initial canopy cover
SumGDDforDayCC := undef_int;
IF (Crop.ModeCycle = GDDays) THEN
   BEGIN
   IF (Crop.GDDaysToCCini = 0)
      THEN SumGDDforDayCC := Simulation.SumGDDfromDay1 - GDDayi
      ELSE BEGIN
           // adjust time (Growing Degree Days) for regrowth
           SumGDDforDayCC := Simulation.SumGDDfromDay1 - GDDayi + GDDTadj + Crop.GDDaysToGermination;
           IF (SumGDDforDayCC > Crop.GDDaysToHarvest) THEN SumGDDforDayCC := Crop.GDDaysToHarvest; // special case where L123 > L1234
           IF (SumGDDforDayCC > Crop.GDDaysToFullCanopy) THEN
              BEGIN
              IF (Simulation.SumGDDfromDay1 <= Crop.GDDaysToSenescence)
                 THEN SumGDDforDayCC := Crop.GDDaysToFullCanopy + ROUND(GDDayFraction * (Simulation.SumGDDfromDay1+GDDTadj+Crop.GDDaysToGermination-Crop.GDDaysToFullCanopy)) // slow down
                 ELSE SumGDDforDayCC := Simulation.SumGDDfromDay1 - GDDayi; // switch time scale
              END;
           END;
   END;
// 13.1d CCi at start of day (is CCi at end of previous day)
IF (DayNri <= Crop.Day1)
   THEN BEGIN
        IF (Crop.DaysToCCini <> 0)
           THEN BEGIN  // regrowth which starts on 1st day
                IF (DayNri = Crop.Day1)
                   THEN BEGIN
                        CCiPrev := CCiNoWaterStressSF(DayCC,
                                      Crop.DaysToGermination,
                                      Crop.DaysToFullCanopySF,Crop.DaysToSenescence,Crop.DaysToHarvest,
                                      Crop.GDDaysToGermination,Crop.GDDaysToFullCanopySF,
                                      Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                                      CCoTotal,CCxTotal,Crop.CGC,Crop.GDDCGC,CDCTotal,GDDCDCTotal,
                                      SumGDDforDayCC,RatDGDD,
                                      Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX,
                                      Simulation.EffectStress.CDecline,Crop.ModeCycle);
                        END
                   ELSE CCiPrev := 0;
                END
           ELSE BEGIN // sowing or transplanting
                CCiPrev := 0;
                IF (DayNri = (Crop.Day1+Crop.DaysToGermination)) THEN CCiPrev := CCoTotal;
                END;
        END
   ELSE BEGIN
        IF (DayNri > Crop.DayN)
           THEN CCiPrev := 0  // after cropping period
           ELSE BEGIN
                CCiPrev := CCiNoWaterStressSF(DayCC,
                              Crop.DaysToGermination,
                              Crop.DaysToFullCanopySF,Crop.DaysToSenescence,Crop.DaysToHarvest,
                              Crop.GDDaysToGermination,Crop.GDDaysToFullCanopySF,
                              Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                              CCoTotal,CCxTotal,Crop.CGC,Crop.GDDCGC,CDCTotal,GDDCDCTotal,
                              SumGDDforDayCC,RatDGDD,
                              Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX,
                              Simulation.EffectStress.CDecline,Crop.ModeCycle);
                END;
        END;
// 13.2 specified CCini (%)
IF ((Simulation.CCini > 0) AND (ROUND(10000*CCiPrev) > 0) AND (ROUND(Simulation.CCini) <> ROUND(100*CCiPrev)))
   THEN BEGIN
        // 13.2a Minimum CC
        CCiniMin := 100 * (Crop.SizeSeedling/10000)*(Crop.PlantingDens/10000);
        IF (CCiniMin - INT(CCiniMin*100)/100) >= 0.00001
           THEN CCiniMin := INT(CCiniMin*100 + 1)/100
           ELSE CCiniMin := INT(CCiniMin*100)/100;
        // 13.2b Maximum CC
        CCiniMax := 100 * CCiPrev;
        CCiniMax := INT(CCiniMax*100)/100;
        // 13.2c accept specified CCini
        IF ((Simulation.CCini >= CCiniMin) AND (Simulation.CCini <= CCiniMax)) THEN CCiPrev := Simulation.CCini/100;
        END;
// 13.3
Crop.CCxAdjusted := CCxTotal;
Crop.CCoAdjusted := CCoTotal;
TimeSenescence := 0;
Crop.CCxWithered := 0;
NoMoreCrop := false;

CCiActual := CCiPrev;


// 14. Biomass and re-setting of GlobalZero
IF (ROUND(1000*Simulation.Bini) > 0) THEN // overwrite settings in GlobalZero (in Global)
   WITH SumWabal DO
        BEGIN
        Biomass := Simulation.Bini;
        BiomassPot := Simulation.Bini;
        BiomassUnlim := Simulation.Bini;
        BiomassTot :=  Simulation.Bini;
        END;


// 15. Transfer of assimilates
IF ((Crop.subkind = Forage) // only valid for perennial herbaceous forage crops
   AND (Trim(GetCropFileFull()) = Trim(Simulation.Storage.CropString)) // only for the same crop
   AND (Simulation.YearSeason > 1) // mobilization not possible in season 1
   AND (Simulation.YearSeason = (Simulation.Storage.Season + 1))) // season next to season in which storage took place
   THEN BEGIN
        // mobilization of assimilates
        Transfer.ToMobilize := Simulation.Storage.Btotal * Crop.Assimilates.Mobilized/100;
        IF (ROUND(1000 * Transfer.ToMobilize) > 0)  // minimum 1 kg
           THEN Transfer.Mobilize := true
           ELSE Transfer.Mobilize := false;
        END
   ELSE BEGIN
        Simulation.Storage.CropString := GetCropFileFull();
        // no mobilization of assimilates
        Transfer.ToMobilize := 0;
        Transfer.Mobilize := false;
        END;
// Storage is off and zero at start of season
Simulation.Storage.Season := Simulation.YearSeason;
Simulation.Storage.Btotal := 0;
Transfer.Store := false;
// Nothing yet mobilized at start of season
Transfer.Bmobilized := 0;



// 16. Initial rooting depth
// 16.1 default value
IF (DayNri <= Crop.Day1)
   THEN Ziprev := undef_int
   ELSE BEGIN
        IF (DayNri > Crop.DayN)
           THEN Ziprev := undef_int
           ELSE BEGIN
                ZiPrev := ActualRootingDepth((DayNri-Crop.Day1),Crop.DaysToGermination,Crop.DaysToMaxRooting,
                            Crop.DaysToHarvest,Crop.GDDaysToGermination,Crop.GDDaysToMaxRooting,Crop.GDDaysToHarvest,
                            SumGDDPrev,Crop.RootMin,Crop.RootMax,Crop.RootShape,Crop.ModeCycle);
                END;
        END;
// 16.2 specified or default Zrini (m)
IF ((Simulation.Zrini > 0) AND (Ziprev > 0) AND (Simulation.Zrini <= Ziprev))
   THEN BEGIN
        IF ((Simulation.Zrini >= Crop.RootMin) AND (Simulation.Zrini <= Crop.RootMax))
           THEN Ziprev := Simulation.Zrini
           ELSE BEGIN
                IF (Simulation.Zrini < Crop.RootMin)
                   THEN Ziprev := Crop.RootMin
                   ELSE Ziprev := Crop.RootMax;
                END;
        IF ((ROUND(Soil.RootMax*1000) < ROUND(Crop.RootMax*1000))
           AND (Ziprev > Soil.RootMax))
           THEN Ziprev := Soil.RootMax;
        RootingDepth := Ziprev;  // NOT NEEDED since RootingDepth is calculated in the RUN by ocnsidering ZiPrev
        END
   ELSE RootingDepth := ActualRootingDepth((DayNri-Crop.Day1+1),Crop.DaysToGermination,Crop.DaysToMaxRooting,
                      Crop.DaysToHarvest,Crop.GDDaysToGermination,Crop.GDDaysToMaxRooting,Crop.GDDaysToHarvest,
                      SumGDDPrev,Crop.RootMin,Crop.RootMax,Crop.RootShape,Crop.ModeCycle);



// 17. Multiple cuttings
NrCut := 0;
SumInterval := 0;
SumGDDcuts := 0;
BprevSum := 0;
YprevSum := 0;
CutInfoRecord1.IntervalInfo := 0;
CutInfoRecord2.IntervalInfo := 0;
CutInfoRecord1.MassInfo := 0;
CutInfoRecord2.MassInfo := 0;
DayLastCut:= 0;
CGCref := Crop.CGC;
GDDCGCref := Crop.GDDCGC;
IF GetManagement_Cuttings_Considered() THEN OpenHarvestInfo(fCuts);
CGCadjustmentAfterCutting := false;


// 18. Tab sheets

// 19. Labels, Plots and displays
IF (GetManagement_BundHeight() < 0.01) THEN
   BEGIN
   SurfaceStorage := 0;
   ECStorage := 0;
   END;
IF (RootingDepth > 0) THEN // salinity in root zone
   BEGIN
   DetermineRootZoneSaltContent(RootingDepth,RootZoneSalt.ECe, RootZoneSalt.ECsw,RootZoneSalt.ECswFC,RootZoneSalt.KsSalt);
   StressTot.Salt := ((StressTot.NrD - 1)*StressTot.Salt + 100*(1-RootZoneSalt.KsSalt))/StressTot.NrD;
   END;
// Harvest Index
Simulation.HIfinal := Crop.HI;
HItimesBEF := undef_int;
HItimesAT1 := 1;
HItimesAT2 := 1;
HItimesAT := 1;
alfaHI := undef_int;
alfaHIAdj := 0;
IF (Simulation.FromDayNr <= (Simulation.DelayedDays + Crop.Day1 + Crop.DaysToFlowering))
   THEN BEGIN // not yet flowering
        ScorAT1 := 0;
        ScorAT2 := 0;
        END
   ELSE BEGIN
        // water stress affecting leaf expansion
        // NOTE: time to reach end determinancy  is tHImax (i.e. flowering/2 or senescence)
        IF Crop.DeterminancyLinked
           THEN tHImax := ROUND(Crop.LengthFlowering/2)
           ELSE tHImax := (Crop.DaysToSenescence - Crop.DaysToFlowering);
        IF ((Simulation.FromDayNr <= (Simulation.DelayedDays + Crop.Day1 + Crop.DaysToFlowering + tHImax)) // not yet end period
             AND (tHImax > 0))
           THEN BEGIN // not yet end determinancy
                ScorAT1 := 1/tHImax;
                ScorAT1 := ScorAT1 * (Simulation.FromDayNr - (Simulation.DelayedDays + Crop.Day1 + Crop.DaysToFlowering));
                IF (ScorAT1 > 1) THEN ScorAT1 := 1;
                END
           ELSE ScorAT1 := 1;  // after period of effect
        // water stress affecting stomatal closure
        // period of effect is yield formation
        IF (Crop.dHIdt > 99)
           THEN tHImax := 0
           ELSE tHImax := ROUND(Crop.HI/Crop.dHIdt);
        IF ((Simulation.FromDayNr <= (Simulation.DelayedDays + Crop.Day1 + Crop.DaysToFlowering + tHImax)) // not yet end period
              AND (tHImax > 0))
           THEN BEGIN // not yet end yield formation
                ScorAT2 := 1/tHImax;
                ScorAT2 := ScorAT2 * (Simulation.FromDayNr - (Simulation.DelayedDays + Crop.Day1 + Crop.DaysToFlowering));
                IF (ScorAT2 > 1) THEN ScorAT2 := 1;
                END
           ELSE ScorAT2 := 1;  // after period of effect
        END;

IF OutDaily THEN DetermineGrowthStage(DayNri,CCiPrev,StageCode);

// 20. Settings for start
StartMode := true;
StressLeaf := undef_int;
StressSenescence := undef_int;
END; (* InitializeSimulationRun *)




// WRITING RESULTS section ================================================= START ====================


PROCEDURE WriteTheResults(ANumber : ShortInt;
                         Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
                         RPer,EToPer,GDDPer,
                         IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer,
                         SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer : double;
                         TheProjectFile : string;
                         VAR fRun : text);
VAR BrSF,RatioE,RatioT : INTEGER;
    WPy,HI : double;
    TempString : string;

BEGIN
// start
IF NoYear THEN
   BEGIN
   Year1 := 9999;
   YearN := 9999;
   END;
IF (ANumber = undef_int) // intermediate results
   THEN BEGIN
        CASE OutputAggregate OF
             1 : WRITE(fRun,'      Day',Day1:9,Month1:9,Year1:9);
             2 : WRITE(fRun,'    10Day',Day1:9,Month1:9,Year1:9);
             3 : WRITE(fRun,'    Month',Day1:9,Month1:9,Year1:9);
             end;
        END
   ELSE BEGIN
        Str(ANumber:9,TempString);
        TempString := CONCAT('Tot(',Trim(TempString),')');
        WHILE (Length(TempString) < 9) DO TempString := CONCAT(' ',TempString);
        WRITE(fRun,TempString,Day1:9,Month1:9,Year1:9);
        END;
// Climatic conditions
WRITE(fRun,Rper:9:1,EToPer:9:1,GDDPer:9:1,CO2i:9:2);
// Soil water parameters
IF (ExPer > 0)
   THEN RatioE := ROUND(100*EPer/ExPer)
   ELSE RatioE := undef_int;
IF (TrxPer > 0)
   THEN RatioT := ROUND(100*TrPer/TrxPer)
   ELSE RatioT := undef_int;
WRITE(fRun,IrriPer:9:1,InfiltPer:9:1,ROPer:9:1,DrainPer:9:1,CRwPer:9:1,
           EPer:9:1,RatioE:9,TrPer:9:1,TrWPer:9:1,RatioT:9);
// Soil Salinity
WRITE(fRun,SalInPer:10:3,SalOutPer:10:3,SalCRPer:10:3,TotalSaltContent.EndDay:10:3);
// seasonal stress
WRITE(fRun,StressTot.NrD:9,StressTot.Salt:9:0,GetManagement_FertilityStress():9,StressTot.Weed:9:0,
        StressTot.Temp:9:0,StressTot.Exp:9:0,StressTot.Sto:9:0);
// Biomass production
IF ((BiomassPer > 0) AND (BUnlimPer > 0))
   THEN BEGIN
        BrSF := ROUND(100*BiomassPer/BUnlimPer);
        IF (BrSF > 100) THEN BrSF := 100;
        END
   ELSE BrSF := undef_int;
WRITE(fRun,BiomassPer:10:3,BrSF:9);
// Crop yield
IF (ANumber <> undef_int) // end of simulation run
   THEN BEGIN
        // Water Use Efficiency yield
        IF (((SumWabal.Tact > 0) OR (SumWabal.ECropCycle > 0)) AND (SumWaBal.YieldPart > 0))
           THEN WPy := (SumWaBal.YieldPart*1000)/((SumWabal.Tact+SumWaBal.ECropCycle)*10)
           ELSE WPy := 0.0;
        // Harvest Index
        IF ((SumWabal.Biomass > 0) AND (SumWabal.YieldPart > 0))
           THEN HI := 100*(SumWabal.YieldPart)/(SumWabal.Biomass)
           ELSE HI := undef_double;
        // Fresh yield
        IF ((Crop.DryMatter = undef_int) OR (Crop.DryMatter = 0))
           THEN WRITE(fRun,HI:9:1,SumWabal.YieldPart:9:3,undef_double:9:3,WPy:9:2)
           ELSE WRITE(fRun,HI:9:1,SumWabal.YieldPart:9:3,(SumWabal.YieldPart/(Crop.DryMatter/100)):9:3,WPy:9:2);
        // Transfer of assimilates
        WRITE(fRun,Transfer.Bmobilized:9:3,Simulation.Storage.Btotal:9:3);
        END
   ELSE WRITE(fRun,undef_int:9,undef_int:9,undef_int:9,undef_int:9,BmobPer:9:3,BstoPer:9:3);
// End
WRITE(fRun,DayN:9,MonthN:9,YearN:9);
// Project
WRITELN(fRun,'  ',TheProjectFile);
END; (* WriteTheResults *)


PROCEDURE WriteIntermediatePeriod(TheProjectFile : string);
VAR Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
    RPer,EToPer,GDDPer,IrriPer,InfiltPer,EPer,ExPer,TrPer,TrWPer,TrxPer,DrainPer,BiomassPer,BUnlimPer : double;
    ROPer,CRwPer,SalInPer,SalOutPer,SalCRPer,BmobPer,BstoPer : double;

BEGIN
// determine intermediate results
DetermineDate((PreviousDayNr+1),Day1,Month1,Year1);
DetermineDate(DayNri,DayN,MonthN,YearN);
RPer := SumWabal.Rain - PreviousSum.Rain;
EToPer := SumETo - PreviousSumETo;
GDDPer := SumGDD - PreviousSumGDD;
IrriPer := SumWabal.Irrigation - PreviousSum.Irrigation;
InfiltPer := SumWabal.Infiltrated - PreviousSum.Infiltrated;
EPer := SumWabal.Eact - PreviousSum.Eact;
ExPer := SumWabal.Epot - PreviousSum.Epot;
TrPer := SumWabal.Tact - PreviousSum.Tact;
TrWPer := SumWabal.TrW - PreviousSum.TrW;
TrxPer := SumWabal.Tpot - PreviousSum.Tpot;
DrainPer := SumWabal.Drain - PreviousSum.Drain;
BiomassPer := SumWabal.Biomass - PreviousSum.Biomass;
BUnlimPer := SumWabal.BiomassUnlim - PreviousSum.BiomassUnlim;

ROPer := SumWabal.Runoff - PreviousSum.Runoff;
CRwPer := SumWabal.CRwater - PreviousSum.CRwater;
SalInPer := SumWabal.SaltIn - PreviousSum.SaltIn;
SalOutPer := SumWabal.SaltOut - PreviousSum.SaltOut;
SalCRPer := SumWabal.CRsalt - PreviousSum.CRsalt;

BmobPer := Transfer.Bmobilized - PreviousBmob;
BstoPer := Simulation.Storage.Btotal - PreviousBsto;

// write
WriteTheResults((undef_int),Day1,Month1,Year1,DayN,MonthN,YearN,
                RPer,EToPer,GDDPer,
                IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                EPer,ExPer,TrPer,TrWPer,TrxPer,
                SalInPer,SalOutPer,SalCRPer,
                BiomassPer,BUnlimPer,BmobPer,BstoPer,
                TheProjectFile,fRun);

// reset previous sums
PreviousDayNr := DayNri;
PreviousSum.Rain := SumWabal.Rain;
PreviousSumETo := SumETo;
PreviousSumGDD := SumGDD;
PreviousSum.Irrigation := SumWabal.Irrigation;
PreviousSum.Infiltrated := SumWabal.Infiltrated;
PreviousSum.Eact := SumWabal.Eact;
PreviousSum.Epot := SumWabal.Epot;
PreviousSum.Tact := SumWabal.Tact;
PreviousSum.TrW := SumWabal.TrW;
PreviousSum.Tpot := SumWabal.Tpot;
PreviousSum.Drain := SumWabal.Drain;
PreviousSum.Biomass := SumWabal.Biomass;
PreviousSum.BiomassPot := SumWabal.BiomassPot;
PreviousSum.BiomassUnlim := SumWabal.BiomassUnlim;

PreviousSum.Runoff := SumWabal.Runoff;
PreviousSum.CRwater := SumWabal.CRwater;
PreviousSum.SaltIn := SumWabal.SaltIn;
PreviousSum.SaltOut := SumWabal.SaltOut;
PreviousSum.CRsalt := SumWabal.CRsalt;

PreviousBmob := Transfer.Bmobilized;
PreviousBsto := Simulation.Storage.Btotal;
END; (* WriteIntermediatePeriod *)


PROCEDURE WriteSimPeriod(NrRun : ShortInt;
                         TheProjectFile : string);
VAR Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
BEGIN
DetermineDate(Simulation.FromDayNr,Day1,Month1,Year1); // Start simulation run
DetermineDate(Simulation.ToDayNr,DayN,MonthN,YearN); // End simulation run
WriteTheResults(NrRun,Day1,Month1,Year1,DayN,MonthN,YearN,
               SumWabal.Rain,SumETo,SumGDD,
               SumWabal.Irrigation,SumWabal.Infiltrated,SumWabal.Runoff,SumWabal.Drain,SumWabal.CRwater,
               SumWabal.Eact,SumWabal.Epot,SumWabal.Tact,SumWabal.TrW,SumWabal.Tpot,
               SumWabal.SaltIn,SumWabal.SaltOut,SumWabal.CRsalt,
               SumWabal.Biomass,SumWaBal.BiomassUnlim,Transfer.Bmobilized,Simulation.Storage.Btotal,
               TheProjectFile,fRun);
END; (* WriteSimPeriod *)



PROCEDURE CheckForPrint(TheProjectFile : string);
VAR DayN,MonthN,YearN,DayEndM : INTEGER;
    SaltIn,SaltOut,CRsalt,BiomassDay,BUnlimDay : double;
    WriteNow : BOOLEAN;

BEGIN
DetermineDate(DayNri,DayN,MonthN,YearN);
CASE OutputAggregate OF
  1 :   BEGIN // daily output
        BiomassDay := SumWabal.Biomass - PreviousSum.Biomass;
        BUnlimDay := SumWabal.BiomassUnlim - PreviousSum.BiomassUnlim;
        SaltIn := SumWabal.SaltIn - PreviousSum.SaltIn;
        SaltOut := SumWabal.SaltOut - PreviousSum.SaltOut;
        CRsalt := SumWabal.CRsalt - PreviousSum.CRsalt;
        WriteTheResults((undef_int),DayN,MonthN,YearN,DayN,MonthN,YearN,
                       Rain,ETo,GDDayi,
                       Irrigation,Infiltrated,Runoff,Drain,CRwater,
                       Eact,Epot,Tact,TactWeedInfested,Tpot,
                       SaltIn,SaltOut,CRsalt,
                       BiomassDay,BUnlimDay,Bin,Bout,
                       TheProjectFile,fRun);
        PreviousSum.Biomass := SumWabal.Biomass;
        PreviousSum.BiomassUnlim := SumWabal.BiomassUnlim;
        PreviousSum.SaltIn := SumWabal.SaltIn;
        PreviousSum.SaltOut := SumWabal.SaltOut;
        PreviousSum.CRsalt := SumWabal.CRsalt;
        END;
  2,3 : BEGIN  // 10-day or monthly output
        WriteNow := false;
        DayEndM := DaysInMonth[MonthN];
        IF (LeapYear(YearN) AND (MonthN = 2)) THEN DayEndM := 29;
        IF (DayN = DayEndM) THEN WriteNow := true;  // 10-day and month
        IF ((OutputAggregate = 2) AND ((DayN = 10) OR (DayN = 20))) THEN WriteNow := true; // 10-day
        IF WriteNow THEN WriteIntermediatePeriod(TheProjectFile);
        END;
    end;
END; (* CheckForPrint *)


PROCEDURE WriteDailyResults(DAP : INTEGER;
                            StageCode : ShortInt;
                            WPi : double;
                            VAR fDaily : text);
CONST NoValD = undef_double;
      NoValI = undef_int;
VAR Di,Mi,Yi,StrExp,StrSto,StrSalt,StrTr,StrW,Brel,Nr : INTEGER;
    Ratio1,Ratio2,Ratio3,KsTr,HI,KcVal,WPy,SaltVal : double;
BEGIN
DetermineDate(DayNri,Di,Mi,Yi);
IF (ClimRecord.FromY = 1901) THEN Yi := Yi - 1901 + 1;
IF (StageCode = 0) THEN DAP := undef_int; // before or after cropping

// 0. info day
WRITE(fDaily,Di:6,Mi:6,Yi:6,DAP:6,StageCode:6);

// 1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF (ZiAqua = undef_int)
      THEN WRITE(fDaily,TotalWaterContent.EndDay:10:1,Rain:8:1,Irrigation:9:1,
               SurfaceStorage:7:1,Infiltrated:7:1,Runoff:7:1,Drain:9:1,CRwater:9:1,undef_double:8:2)
      ELSE  WRITE(fDaily,TotalWaterContent.EndDay:10:1,Rain:8:1,Irrigation:9:1,
               SurfaceStorage:7:1,Infiltrated:7:1,Runoff:7:1,Drain:9:1,CRwater:9:1,(ZiAqua/100):8:2);
   IF (Tpot > 0) THEN Ratio1 := 100*Tact/Tpot
                 ELSE Ratio1 := 100.0;
   IF ((Epot+Tpot) > 0) THEN Ratio2 := 100*(Eact+Tact)/(Epot+Tpot)
                        ELSE Ratio2 := 100.0;
   IF (Epot > 0) THEN Ratio3 := 100*Eact/Epot
                 ELSE Ratio3 := 100;
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,Epot:9:1,Eact:9:1,Ratio3:7:0,Tpot:9:1,Tact:9:1,Ratio1:6:0,(Epot+Tpot):9:1,(Eact+Tact):8:1,Ratio2:8:0)
      ELSE WRITELN(fDaily,Epot:9:1,Eact:9:1,Ratio3:7:0,Tpot:9:1,Tact:9:1,Ratio1:6:0,(Epot+Tpot):9:1,(Eact+Tact):8:1,Ratio2:8:0);
   END;

// 2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   //1. relative transpiration
   IF (Tpot > 0) THEN Ratio1 := 100*Tact/Tpot
                 ELSE Ratio1 := 100.0;
   //2. Water stresses
   IF (StressLeaf < 0)
      THEN StrExp := undef_int
      ELSE StrExp := ROUND(StressLeaf);
   IF (Tpot <= 0)
      THEN StrSto := undef_int
      ELSE StrSto := ROUND(100 *(1 - Tact/Tpot));
   //3. Salinity stress
   IF (RootZoneSalt.KsSalt < 0)
      THEN StrSalt := undef_int
      ELSE StrSalt := ROUND(100 * (1 - RootZoneSalt.KsSalt));
   //4. Air temperature stress
   IF (CCiActual <= 0.0000001)
      THEN KsTr := 1
      ELSE KsTr := KsTemperature((0),Crop.GDtranspLow,GDDayi);
   IF (KsTr < 1)
      THEN StrTr := ROUND((1-KsTr)*100)
      ELSE StrTr := 0;
   //5. Relative cover of weeds
   IF (CCiActual <= 0.0000001)
      THEN StrW := undef_int
      ELSE StrW := Round(WeedRCi);
   //6. WPi adjustemnt
   IF (SumWabal.Biomass <= 0.000001) THEN WPi := 0;
   //7. Harvest Index
   IF ((SumWabal.Biomass > 0) AND (SumWabal.YieldPart > 0))
      THEN HI := 100*(SumWabal.YieldPart)/(SumWabal.Biomass)
      ELSE HI := undef_double;
   //8. Relative Biomass
   IF ((SumWaBal.Biomass > 0) AND (SumWaBal.BiomassUnlim > 0))
      THEN BEGIN
           Brel := ROUND(100*SumWaBal.Biomass/SumWaBal.BiomassUnlim);
           IF (Brel > 100) THEN Brel := 100;
           END
      ELSE Brel := undef_int;
   //9. Kc coefficient
   IF ((ETo > 0) AND (Tpot > 0) AND (StrTr < 100))
      THEN KcVal := Tpot/(ETo*KsTr)
      ELSE KcVal := undef_int;
   //10. Water Use Efficiency yield
   IF (((SumWabal.Tact > 0) OR (SumWabal.ECropCycle > 0)) AND (SumWaBal.YieldPart > 0))
      THEN WPy := (SumWaBal.YieldPart*1000)/((SumWabal.Tact+SumWabal.ECropCycle)*10)
      ELSE WPy := 0.0;
   // write
   WRITE(fDaily,GDDayi:9:1,RootingDepth:8:2,StrExp:7,StrSto:7,StressSenescence:7:0,StrSalt:7,StrW:7,
         (CCiActual*100):8:1,(CCiActualWeedInfested*100):8:1,StrTr:7,KcVal:9:2,Tpot:9:1,Tact:9:1,
         TactWeedInfested:9:1,Ratio1:6:0,(100*WPi):8:1,SumWabal.Biomass:10:3,HI:8:1,SumWabal.YieldPart:9:3);
   // Fresh yield
   IF ((Crop.DryMatter = undef_int) OR (Crop.DryMatter = 0))
      THEN WRITE(fDaily,undef_double:9:3)
      ELSE WRITE(fDaily,(SumWabal.YieldPart/(Crop.DryMatter/100)):9:3);
   // finalize
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,Brel:8,WPy:12:2,Bin:9:3,Bout:9:3)
      ELSE WRITELN(fDaily,Brel:8,WPy:12:2,Bin:9:3,Bout:9:3);
   END;

// 3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   WRITE(fDaily,TotalWaterContent.EndDay:10:1);
   IF (RootingDepth <= 0)
      THEN SetRootZoneWC_Actual(undef_double)
      ELSE BEGIN
           IF (ROUND(Soil.RootMax*1000) = ROUND(Crop.RootMax*1000))
              THEN DetermineRootZoneWC(Crop.RootMax,Simulation.SWCtopSoilConsidered)
              ELSE DetermineRootZoneWC(Soil.RootMax,Simulation.SWCtopSoilConsidered);
           END;
   WRITE(fDaily,GetRootZoneWC().actual:9:1,RootingDepth:8:2);
   IF (RootingDepth <= 0)
      THEN BEGIN
           SetRootZoneWC_Actual(undef_double);
           SetRootZoneWC_FC(undef_double);
           SetRootZoneWC_WP(undef_double);
           SetRootZoneWC_SAT(undef_double);
           SetRootZoneWC_Thresh(undef_double);
           SetRootZoneWC_Leaf(undef_double);
           SetRootZoneWC_Sen(undef_double);
           END
      ELSE DetermineRootZoneWC(RootingDepth,Simulation.SWCtopSoilConsidered);
   WRITE(fDaily,GetRootZoneWC().actual:8:1,GetRootZoneWC().SAT:10:1,GetRootZoneWC().FC:10:1,GetRootZoneWC().Leaf:10:1,
      GetRootZoneWC().Thresh:10:1,GetRootZoneWC().Sen:10:1);
   IF ((Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,GetRootZoneWC().WP:10:1)
      ELSE WRITELN(fDaily,GetRootZoneWC().WP:10:1);
   END;
   
// 4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   WRITE(fDaily,SaltInfiltr:9:3,(Drain*ECdrain*Equiv/100):10:3,(CRsalt/100):10:3,TotalSaltContent.EndDay:10:3);
   IF (RootingDepth <= 0)
      THEN BEGIN
           SaltVal := undef_int;
           RootZoneSalt.ECe := undef_int;
           RootZoneSalt.ECsw := undef_int;
           RootZoneSalt.KsSalt := 1;
           END
      ELSE SaltVal := (GetRootZoneWC().SAT*RootZoneSalt.ECe*Equiv)/100;
   IF (ZiAqua = undef_int)
      THEN WRITE(fDaily,SaltVal:10:3,RootingDepth:8:2,RootZoneSalt.ECe:9:2,RootZoneSalt.ECsw:8:2,
                 (100*(1-RootZoneSalt.KsSalt)):7:0,undef_double:8:2)
      ELSE WRITE(fDaily,SaltVal:10:3,RootingDepth:8:2,RootZoneSalt.ECe:9:2,RootZoneSalt.ECsw:8:2,
                 (100*(1-RootZoneSalt.KsSalt)):7:0,(ZiAqua/100):8:2);
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,ECiAqua:8:2)
      ELSE WRITELN(fDaily,ECiAqua:8:2);
   END;

// 5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
   WRITE(fDaily,(Compartment[1].theta*100):11:1);
   FOR Nr := 2 TO (NrCompartments-1) DO WRITE(fDaily,(Compartment[Nr].theta*100):11:1);
   IF ((Out6CompEC = true) OR (Out7Clim = true))
      THEN WRITE(fDaily,(Compartment[NrCompartments].theta*100):11:1)
      ELSE WRITELN(fDaily,(Compartment[NrCompartments].theta*100):11:1);
   END;

// 6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   SaltVal := ECeComp(Compartment[1]);
   WRITE(fDaily,SaltVal:11:1);
   FOR Nr := 2 TO (NrCompartments-1) DO
       BEGIN
       SaltVal := ECeComp(Compartment[Nr]);
       WRITE(fDaily,SaltVal:11:1);
       END;
   SaltVal := ECeComp(Compartment[NrCompartments]);
   IF (Out7Clim = true)
      THEN WRITE(fDaily,SaltVal:11:1)
      ELSE WRITELN(fDaily,SaltVal:11:1);
   END;

// 7. Climate input parameters
IF Out7Clim THEN
   BEGIN
   Ratio1 := (Tmin + Tmax)/2;
   WRITELN(fDaily,Rain:9:1,ETo:10:1,Tmin:10:1,Ratio1:10:1,Tmax:10:1,CO2i:10:2);
   END;
END; (* WriteDailyResults *)



PROCEDURE WriteEvaluationData(DAP : INTEGER;
                              StageCode : ShortInt;
                              VAR fEval : text);
                              
VAR SWCi,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd : double;
    Nr,Di,Mi,Yi : INTEGER;

    FUNCTION SWCZsoil(Zsoil : double) : double;
    VAR compi : INTEGER;
        CumDepth,Factor,frac_value,SWCact : double;
    BEGIN
    CumDepth := 0;
    compi := 0;
    SWCact := 0;
    REPEAT
      compi := compi + 1;
      CumDepth := CumDepth + Compartment[compi].Thickness;
      IF (CumDepth <= Zsoil)
         THEN Factor := 1
         ELSE BEGIN
              frac_value := Zsoil - (CumDepth - Compartment[compi].Thickness);
              IF (frac_value > 0)
                 THEN Factor := frac_value/Compartment[compi].Thickness
                 ELSE Factor := 0;
              END;
      SWCact := SWCact + Factor * 10 * (Compartment[compi].theta*100) * Compartment[compi].Thickness;

    UNTIL ((ROUND(100*CumDepth) >= ROUND(100*ZSoil)) OR (compi = NrCompartments));
    SWCZsoil := SWCact;
    END; (* SWCZsoil *)

BEGIN
//1. Prepare field data
CCfield := undef_int;
CCstd := undef_int;
Bfield := undef_int;
Bstd := undef_int;
SWCfield := undef_int;
SWCstd := undef_int;
IF ((LineNrEval <> undef_int) AND (DayNrEval = DayNri)) THEN
   BEGIN
   // read field data
   Reset(fObs);
   FOR Nr := 1 TO (LineNrEval -1) DO READLN(fObs);
   READLN(fObs,Nr,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd);
   // get Day Nr for next field data
   IF (Eof(fObs))
      THEN BEGIN
           LineNrEval := undef_int;
           Close(fObs);
           END
      ELSE BEGIN
           LineNrEval := LineNrEval + 1;
           READ(fObs,DayNrEval);
           DayNrEval := DayNr1Eval + DayNrEval -1;
           END;
   END;
//2. Date
DetermineDate(DayNri,Di,Mi,Yi);
IF (ClimRecord.FromY = 1901) THEN Yi := Yi - 1901 + 1;
IF (StageCode = 0) THEN DAP := undef_int; // before or after cropping
//3. Write simulation results and field data
SWCi := SWCZsoil(Zeval);
WRITELN(fEval,Di:6,Mi:6,Yi:6,DAP:6,StageCode:5,(CCiActual*100):8:1,CCfield:8:1,CCstd:8:1,
           SumWabal.Biomass:10:3,Bfield:10:3,Bstd:10:3,SWCi:8:1,SWCfield:8:1,SWCstd:8:1);
END; (* WriteEvaluationData *)





// WRITING RESULTS section ================================================= END ====================



PROCEDURE FileManagement(NrRun : ShortInt;
                         TheProjectFile : string;
                         TheProjectType : repTypeProject;
                         VAR fEToSIM,fRainSIM,fTempSIM,fIrri,fCuts : text);
VAR RepeatToDay : LongInt;
    StringREAD : ShortString;
    i,T1 : INTEGER;
    temp_str : string;
    PotValSF,KsTr,WPi,TESTVALY,PreIrri,StressStomata,FracAssim : double;
    HarvestNow : BOOLEAN;
    VirtualTimeCC,DayInSeason : INTEGER;
    SumGDDadjCC,RatDGDD : double;


    PROCEDURE GetZandECgwt(DayNri : LongInt;
                       VAR ZiAqua : INTEGER;
                       VAR ECiAqua : double);
    VAR ZiIN : INTEGER;
    BEGIN
    ZiIN := ZiAqua;
    IF (GwTable.DNr1 = GwTable.DNr2)
       THEN BEGIN
            ZiAqua := GwTable.Z1;
            ECiAqua := GwTable.EC1;
            END
       ELSE BEGIN
            ZiAqua := GwTable.Z1 + ROUND((DayNri - GwTable.DNr1)*(GwTable.Z2 - GwTable.Z1)/(GwTable.DNr2 - GwTable.DNr1));
            ECiAqua := GwTable.EC1 + (DayNri - GwTable.DNr1)*(GwTable.EC2 - GwTable.EC1)/(GwTable.DNr2 - GwTable.DNr1);
            END;
    IF (ZiAqua <> ZiIN) THEN CalculateAdjustedFC((ZiAqua/100),Compartment);
    END; (* GetZandECgwt *)


    FUNCTION IrriOutSeason(Dayi : LongInt) : INTEGER;
    VAR DNr, Nri : INTEGER;
        IrriEvents : rep_IrriOutSeasonEvents;
        TheEnd : BOOLEAN;
    BEGIN
    DNr := Dayi - Simulation.FromDayNr + 1;
    IrriEvents := IrriBeforeSeason;
    IF (Dayi > Crop.DayN) THEN
       BEGIN
       DNr := Dayi - Crop.DayN;
       IrriEvents := IrriAfterSeason;
       END;
    IF (DNr < 1)
       THEN IrriOutSeason := 0
       ELSE BEGIN
            TheEnd := false;
            Nri := 0;
            REPEAT
              Nri := Nri + 1;
              IF (IrriEvents[Nri].DayNr = DNr)
                 THEN BEGIN
                      IrriOutSeason := IrriEvents[Nri].Param;
                      TheEnd := true;
                      END
                 ELSE IrriOutSeason := 0;
            UNTIL ((Nri = 5) OR (IrriEvents[Nri].DayNr = 0)
               OR (IrriEvents[Nri].DayNr > DNr)
               OR TheEnd);
            END;
    END; (* IrriOutSeason *)



    FUNCTION IrriManual(Dayi : LongInt) : INTEGER;
    VAR DNr : INTEGER;
        StringREAD : ShortString;
        Ir1,Ir2 : double;
    BEGIN
    IF (IrriFirstDayNr = undef_int)
       THEN DNr := Dayi - Crop.Day1 + 1
       ELSE DNr := Dayi - IrriFirstDayNr + 1;
    IF (IrriInfoRecord1.NoMoreInfo)
       THEN IrriManual := 0
       ELSE BEGIN
            IrriManual := 0;
            IF (IrriInfoRecord1.TimeInfo = DNr) THEN
               BEGIN
               IrriManual := IrriInfoRecord1.DepthInfo;
               IF Eof(fIrri)
                  THEN IrriInfoRecord1.NoMoreInfo := true
                  ELSE BEGIN
                       IrriInfoRecord1.NoMoreInfo := false;
                       READLN(fIrri,StringREAD);
                       IF GlobalIrriECw // Versions before 3.2
                          THEN SplitStringInTwoParams(StringREAD,Ir1,Ir2)
                          ELSE SplitStringInThreeParams(StringREAD,Ir1,Ir2,Simulation.IrriECw);
                       IrriInfoRecord1.TimeInfo := ROUND(Ir1);
                       IrriInfoRecord1.DepthInfo := ROUND(Ir2);
                       END;
               END;
            END;
    END; (* IrriManual *)



    PROCEDURE GetIrriParam;
    VAR DayInSeason : Integer;

    BEGIN
    TargetTimeVal := -999;
    TargetDepthVal := -999;
    IF ((DayNri < Crop.Day1) OR (DayNri > Crop.DayN))
       THEN Irrigation := IrriOutSeason(DayNri)
       ELSE IF (IrriMode = Manual) THEN Irrigation := IrriManual(DayNri);
    IF ((IrriMode = Generate) AND ((DayNri >= Crop.Day1) AND (DayNri <= Crop.DayN))) THEN
       BEGIN
       // read next line if required
       DayInSeason := DayNri - Crop.Day1 + 1;
       IF (DayInSeason > IrriInfoRecord1.ToDay) THEN // read next line
          BEGIN
          IrriInfoRecord1 := IrriInfoRecord2;
          IF Eof(fIrri)
             THEN IrriInfoRecord1.ToDay := Crop.DayN - Crop.Day1 + 1
             ELSE BEGIN
                  IrriInfoRecord2.NoMoreInfo := false;
                  IF GlobalIrriECw // Versions before 3.2
                     THEN READLN(fIrri,IrriInfoRecord2.FromDay,IrriInfoRecord2.TimeInfo,IrriInfoRecord2.DepthInfo)
                     ELSE READLN(fIrri,IrriInfoRecord2.FromDay,IrriInfoRecord2.TimeInfo,
                                 IrriInfoRecord2.DepthInfo,Simulation.IrriEcw);
                  IrriInfoRecord1.ToDay := IrriInfoRecord2.FromDay - 1;
                  END;
          END;
       // get TargetValues
       TargetDepthVal := IrriInfoRecord1.DepthInfo;
       CASE GenerateTimeMode OF
          AllDepl : TargetTimeVal := IrriInfoRecord1.TimeInfo;
          AllRAW  : TargetTimeVal := IrriInfoRecord1.TimeInfo;
          FixInt  : BEGIN
                    TargetTimeVal := IrriInfoRecord1.TimeInfo;
                    IF (TargetTimeVal > IrriInterval) // do not yet irrigate
                       THEN TargetTimeVal := 0
                       ELSE IF (TargetTimeVal = IrriInterval) // irrigate
                               THEN TargetTimeVal := 1
                               ELSE BEGIN  // still to solve
                                    TargetTimeVal := 1; // voorlopige oplossing
                                    END;
                    IF ((TargetTimeVal = 1) AND (GenerateDepthMode = FixDepth)) THEN Irrigation := TargetDepthVal;
                    END;
          WaterBetweenBunds : BEGIN
                              TargetTimeVal := IrriInfoRecord1.TimeInfo;
                              IF  ((GetManagement_BundHeight() >= 0.01)
                               AND (GenerateDepthMode = FixDepth)
                               AND (TargetTimeVal < (1000 * GetManagement_BundHeight()))
                               AND (TargetTimeVal >= ROUND(SurfaceStorage)))
                                   THEN Irrigation := TargetDepthVal
                                   ELSE Irrigation := 0;
                              TargetTimeVal := -999; // no need for check in SIMUL
                              END;
          end;
       END;
    END; (* GetIrriParam *)


    PROCEDURE AdjustSWCRootZone(VAR PreIrri : double);
    VAR compi,layeri : ShortInt;
        SumDepth,ThetaPercRaw : double;
    BEGIN
    compi := 0;
    SumDepth := 0;
    PreIrri := 0;
    REPEAT
      compi := compi + 1;
      SumDepth := SumDepth + Compartment[compi].Thickness;
      layeri := Compartment[compi].Layer;
      ThetaPercRaw := SoilLayer[layeri].FC/100 - SimulParam.PercRAW/100*Crop.pdef*(SoilLayer[layeri].FC/100-SoilLayer[layeri].WP/100);
      IF (Compartment[compi].Theta < ThetaPercRaw) THEN
         BEGIN
         PreIrri := PreIrri + (ThetaPercRaw - Compartment[compi].Theta)*1000*Compartment[compi].Thickness;
         Compartment[compi].Theta := ThetaPercRaw;
         END;
    UNTIL ((SumDepth >= RootingDepth) OR (compi = NrCompartments))
    END; (* AdjustSWCRootZone *)


    PROCEDURE InitializeTransferAssimilates(NoMoreCrop : BOOLEAN;
                                            VAR Bin,Bout,AssimToMobilize,AssimMobilized,FracAssim : double;
                                            VAR StorageOn,MobilizationOn : BOOLEAN);
    BEGIN
    Bin := 0;
    Bout := 0;
    FracAssim := 0;
    IF (Crop.subkind = Forage) THEN // only for perennial herbaceous forage crops
      BEGIN
      FracAssim := 0;
      IF (NoMoreCrop = true)
         THEN BEGIN
              StorageOn := false;
              MobilizationOn := false;
              END
         ELSE BEGIN
              // Start of storage period ?
              //IF ((DayNri - Simulation.DelayedDays - Crop.Day1) = (Crop.DaysToHarvest - Crop.Assimilates.Period + 1)) THEN
              IF ((DayNri - Simulation.DelayedDays - Crop.Day1 + 1) = (Crop.DaysToHarvest - Crop.Assimilates.Period + 1)) THEN
                 BEGIN
                 // switch storage on
                 StorageOn := true;
                 // switch mobilization off
                 IF (MobilizationOn = true) THEN AssimToMobilize := AssimMobilized;
                 MobilizationOn := false;
                 END;
              // Fraction of assimilates transferred
              IF (MobilizationOn = true) THEN FracAssim := (AssimToMobilize-AssimMobilized)/AssimToMobilize;
              IF ((StorageOn = true) AND (Crop.Assimilates.Period > 0))
                 THEN FracAssim := (Crop.Assimilates.Stored/100) *
                 //(((DayNri - Simulation.DelayedDays - Crop.Day1)-(Crop.DaysToHarvest-Crop.Assimilates.Period))/Crop.Assimilates.Period);
                 (((DayNri - Simulation.DelayedDays - Crop.Day1 + 1)-(Crop.DaysToHarvest-Crop.Assimilates.Period))/Crop.Assimilates.Period);
              IF (FracAssim < 0) THEN FracAssim := 0;
              IF (FracAssim > 1) THEN FracAssim := 1;
              END;
      END;
    END;  (* InitializeTransferAssimilates *)



    PROCEDURE RecordHarvest(NrCut : INTEGER;
                        DayNri : LongInt;
                        DayInSeason,SumInterval : INTEGER;
                        BprevSum,YprevSum : double;
                        VAR fHarvest : text);
    VAR Dayi,Monthi,Yeari : INTEGER;
        NoYear : BOOLEAN;
    BEGIN
    Append(fHarvest);
    DetermineDate(Crop.Day1,Dayi,Monthi,Yeari);
    NoYear := (Yeari = 1901);
    DetermineDate(DayNri,Dayi,Monthi,Yeari);
    IF NoYear THEN Yeari := 9999;
    IF (NrCut = 9999)
       THEN BEGIN
            // last line at end of season
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,SumWabal.Biomass:34:3);
            IF (Crop.DryMatter = undef_int)
               THEN WRITELN(fHarvest,SumWabal.YieldPart:20:3)
               ELSE WRITELN(fHarvest,SumWabal.YieldPart:20:3,(SumWabal.YieldPart/(Crop.DryMatter/100)):20:3);
            END
       ELSE BEGIN
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,DayInSeason:6,SumInterval:6,(SumWabal.Biomass-BprevSum):12:3,
                  SumWabal.Biomass:10:3,(SumWabal.YieldPart-YprevSum):10:3);
            IF (Crop.DryMatter = undef_int)
               THEN WRITELN(fHarvest,SumWabal.YieldPart:10:3)
               ELSE WRITELN(fHarvest,SumWabal.YieldPart:10:3,((SumWabal.YieldPart-YprevSum)/(Crop.DryMatter/100)):10:3,
                         (SumWabal.YieldPart/(Crop.DryMatter/100)):10:3);
            END;
    END; (* RecordHarvest *)



    PROCEDURE GetPotValSF(DAP : INTEGER;
                      VAR PotValSF : double);
    VAR RatDGDD : double;
    BEGIN (* GetPotValSF *)
    RatDGDD := 1;
    IF ((Crop.ModeCycle = GDDays) AND (Crop.GDDaysToFullCanopySF < Crop.GDDaysToSenescence))
       THEN RatDGDD := (Crop.DaysToSenescence-Crop.DaysToFullCanopySF)/(Crop.GDDaysToSenescence-Crop.GDDaysToFullCanopySF);
    PotValSF := CCiNoWaterStressSF(DAP,Crop.DaysToGermination,Crop.DaysToFullCanopySF,Crop.DaysToSenescence,Crop.DaysToHarvest,
        Crop.GDDaysToGermination,Crop.GDDaysToFullCanopySF,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
        CCoTotal,CCxTotal,Crop.CGC,Crop.GDDCGC,CDCTotal,GDDCDCTotal,SumGDDadjCC,RatDGDD,
        Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX,Simulation.EffectStress.CDecline,Crop.ModeCycle);
    PotValSF := 100 * (1/CCxCropWeedsNoSFstress) * PotValSF;
    END; (* GetPotValSF *)




BEGIN (* FileManagement *)
RepeatToDay := Simulation.ToDayNr;

REPEAT
(* 1. Get ETo *)
IF (GetEToFile() = '(None)') THEN ETo := 5;

(* 2. Get Rain *)
IF (GetRainFile() = '(None)') THEN Rain := 0;

(* 3. Start mode *)
IF StartMode THEN StartMode := false;

(* 4. Get depth and quality of the groundwater*)
IF (NOT SimulParam.ConstGwt) THEN
   BEGIN
   IF (DayNri > GwTable.DNr2) THEN GetGwtSet(DayNri,GwTable);
   GetZandECgwt(DayNri,ZiAqua,ECiAqua);
   CheckForWaterTableInProfile((ZiAqua/100),Compartment,WaterTableInProfile);
   IF WaterTableInProfile THEN AdjustForWatertable;
   END;

(* 5. Get Irrigation *)
Irrigation := 0;
GetIrriParam;

(* 6. get virtual time for CC development *)
SumGDDadjCC := undef_int;
IF (Crop.DaysToCCini <> 0)
   THEN BEGIN // regrowth
        IF (DayNri >= Crop.Day1)
           THEN BEGIN
                // time setting for canopy development
                VirtualTimeCC := (DayNri - Simulation.DelayedDays - Crop.Day1) + Tadj + Crop.DaysToGermination; // adjusted time scale
                IF (VirtualTimeCC > Crop.DaysToHarvest) THEN VirtualTimeCC := Crop.DaysToHarvest; // special case where L123 > L1234
                IF (VirtualTimeCC > Crop.DaysToFullCanopy) THEN
                   BEGIN
                   IF ((DayNri - Simulation.DelayedDays - Crop.Day1) <= Crop.DaysToSenescence)
                      THEN VirtualTimeCC := Crop.DaysToFullCanopy + ROUND(DayFraction *
                            ( (DayNri - Simulation.DelayedDays - Crop.Day1)+Tadj+Crop.DaysToGermination - Crop.DaysToFullCanopy)) // slow down
                      ELSE VirtualTimeCC := (DayNri - Simulation.DelayedDays - Crop.Day1); // switch time scale
                   END;
                IF (Crop.ModeCycle = GDDays) THEN
                   BEGIN
                   SumGDDadjCC := Simulation.SumGDDfromDay1 + GDDTadj + Crop.GDDaysToGermination;
                   IF (SumGDDadjCC > Crop.GDDaysToHarvest) THEN SumGDDadjCC := Crop.GDDaysToHarvest; // special case where L123 > L1234
                   IF (SumGDDadjCC > Crop.GDDaysToFullCanopy) THEN
                      BEGIN
                      IF (Simulation.SumGDDfromDay1 <= Crop.GDDaysToSenescence)
                         THEN SumGDDadjCC := Crop.GDDaysToFullCanopy
                           + ROUND(GDDayFraction * (Simulation.SumGDDfromDay1+GDDTadj+Crop.GDDaysToGermination-Crop.GDDaysToFullCanopy)) // slow down
                         ELSE SumGDDadjCC := Simulation.SumGDDfromDay1 // switch time scale
                      END
                   END;
                // CC initial (at the end of previous day) when simulation starts before regrowth,
                IF ((DayNri = Crop.Day1) AND (DayNri > Simulation.FromDayNr)) THEN
                   BEGIN
                   RatDGDD := 1;
                   IF ((Crop.ModeCycle = GDDays) AND (Crop.GDDaysToFullCanopySF < Crop.GDDaysToSenescence)) THEN
                      RatDGDD := (Crop.DaysToSenescence-Crop.DaysToFullCanopySF)/(Crop.GDDaysToSenescence-Crop.GDDaysToFullCanopySF);
                   CropStressParametersSoilFertility(Crop.StressResponse,StressSFAdjNEW,Simulation.EffectStress);
                   CCiPrev := CCiniTotalFromTimeToCCini(Crop.DaysToCCini,Crop.GDDaysToCCini,
                                  Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToFullCanopySF,
                                  Crop.DaysToSenescence,Crop.DaysToHarvest,
                                  Crop.GDDaysToGermination,Crop.GDDaysToFullCanopy,Crop.GDDaysToFullCanopySF,
                                  Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                                  Crop.CCo,Crop.CCx,Crop.CGC,Crop.GDDCGC,Crop.CDC,Crop.GDDCDC,RatDGDD,
                                  Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX,
                                  Simulation.EffectStress.CDecline,(CCxTotal/Crop.CCx),Crop.ModeCycle);  // (CCxTotal/Crop.CCx) = fWeed
                   END;
                END
           ELSE BEGIN // before start crop
                VirtualTimeCC := DayNri - Simulation.DelayedDays - Crop.Day1;
                IF (Crop.ModeCycle = GDDays) THEN SumGDDadjCC := Simulation.SumGDD;
                END;
        END
   ELSE BEGIN // sown or transplanted
        VirtualTimeCC := DayNri - Simulation.DelayedDays - Crop.Day1;
        IF (Crop.ModeCycle = GDDays) THEN SumGDDadjCC := Simulation.SumGDD;
        // CC initial (at the end of previous day) when simulation starts before sowing/transplanting,
        IF ((DayNri = (Crop.Day1 + Crop.DaysToGermination)) AND (DayNri > Simulation.FromDayNr))
           THEN CCiPrev := CCoTotal;
        END;


(* 7. Rooting depth AND Inet day 1*)
IF (((Crop.ModeCycle = CalendarDays) AND ((DayNri-Crop.Day1+1) < Crop.DaysToHarvest))
              OR ((Crop.ModeCycle = GDDays) AND (Simulation.SumGDD < Crop.GDDaysToHarvest)))
   THEN BEGIN
        IF (((DayNri-Simulation.DelayedDays) >= Crop.Day1) AND ((DayNri-Simulation.DelayedDays) <= Crop.DayN))
           THEN BEGIN // rooting depth at DAP (at Crop.Day1, DAP = 1)
                RootingDepth := AdjustedRootingDepth(PlotVarCrop.ActVal,PlotVarCrop.PotVal,Tpot,Tact,StressLeaf,StressSenescence,
                                (DayNri-Crop.Day1+1),Crop.DaysToGermination,Crop.DaysToMaxRooting,Crop.DaysToHarvest,
                                Crop.GDDaysToGermination,Crop.GDDaysToMaxRooting,Crop.GDDaysToHarvest,(SumGDDPrev),
                                (Simulation.SumGDD),Crop.RootMin,Crop.RootMax,Ziprev,Crop.RootShape,
                                Crop.ModeCycle);
                ZiPrev := RootingDepth;  // IN CASE rootzone drops below groundwate table
                IF ((ZiAqua >= 0) AND (RootingDepth > (ZiAqua/100)) AND (Crop.AnaeroPoint > 0)) THEN
                   BEGIN
                   RootingDepth := ZiAqua/100;
                   IF (RootingDepth < Crop.RootMin) THEN RootingDepth := Crop.RootMin;
                   END;
                END
           ELSE RootingDepth := 0;
        END
   ELSE RootingDepth := Ziprev;
IF ((RootingDepth > 0) AND (DayNri = Crop.Day1))
   THEN BEGIN //initial root zone depletion day1 (for WRITE Output)
        DetermineRootZoneWC(RootingDepth,Simulation.SWCtopSoilConsidered);
        IF (IrriMode = Inet) THEN AdjustSWCRootZone(PreIrri);  // required to start germination
        END;

(* 8. Transfer of Assimilates  *)
InitializeTransferAssimilates(NoMoreCrop,Bin,Bout,Transfer.ToMobilize,Transfer.Bmobilized,FracAssim,
                              Transfer.Store,Transfer.Mobilize);

(* 9. RUN Soil water balance and actual Canopy Cover *)
BUDGET_module(DayNri,TargetTimeVal,TargetDepthVal,VirtualTimeCC,SumInterval,DayLastCut,StressTot.NrD,
              Tadj,GDDTadj,
              GDDayi,CGCref,GDDCGCref,CO2i,CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
              Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTot.Salt,
              DayFraction,GDDayFraction,FracAssim,
              StressSFadjNEW,Transfer.Store,Transfer.Mobilize,
              StressLeaf,StressSenescence,TimeSenescence,NoMoreCrop,CGCadjustmentAfterCutting,TESTVAL);

// consider Pre-irrigation (6.) if IrriMode = Inet
IF ((RootingDepth > 0) AND (DayNri = Crop.Day1) AND (IrriMode = Inet)) THEN
   BEGIN
   Irrigation := Irrigation + PreIrri;
   SumWabal.Irrigation := SumWabal.Irrigation + PreIrri;
   PreIrri := 0;
   END;

// total number of days in the season
IF (CCiActual > 0) THEN
   BEGIN
   IF (StressTot.NrD < 0)
      THEN StressTot.NrD := 1
      ELSE StressTot.NrD := StressTot.NrD +1;
   END;


(* 10. Potential biomass *)
DeterminePotentialBiomass(VirtualTimeCC,SumGDDadjCC,CO2i,GDDayi,CCxWitheredTpotNoS,SumWaBal.BiomassUnlim);

(* 11. Biomass and yield *)
IF ((RootingDepth > 0) AND (NoMoreCrop = false))
   THEN BEGIN
        DetermineRootZoneWC(RootingDepth,Simulation.SWCtopSoilConsidered);
        // temperature stress affecting crop transpiration
        IF (CCiActual <= 0.0000001)
           THEN KsTr := 1
           ELSE KsTr := KsTemperature((0),Crop.GDtranspLow,GDDayi);
        StressTot.Temp := ((StressTot.NrD - 1)*StressTot.Temp + 100*(1-KsTr))/StressTot.NrD;
        // soil salinity stress
        DetermineRootZoneSaltContent(RootingDepth,RootZoneSalt.ECe, RootZoneSalt.ECsw,RootZoneSalt.ECswFC,RootZoneSalt.KsSalt);
        StressTot.Salt := ((StressTot.NrD - 1)*StressTot.Salt + 100*(1-RootZoneSalt.KsSalt))/StressTot.NrD;
        // Biomass and yield
        DetermineBiomassAndYield(DayNri,ETo,Tmin,Tmax,CO2i,GDDayi,Tact,SumKcTop,CGCref,GDDCGCref,
                                 Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF,
                                 Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTot.Salt,SumGDDadjCC,CCiActual,FracAssim,
                                 VirtualTimeCC,SumInterval,
                                 SumWaBal.Biomass,SumWaBal.BiomassPot,SumWaBal.BiomassUnlim,SumWaBal.BiomassTot,
                                 SumWabal.YieldPart,WPi,HItimesBEF,ScorAT1,ScorAT2,HItimesAT1,HItimesAT2,
                                 HItimesAT,alfaHI,alfaHIAdj,SumKcTopStress,SumKci,CCxWitheredTpot,CCxWitheredTpotNoS,
                                 WeedRCi,CCiActualWeedInfested,TactWeedInfested,
                                 StressSFadjNEW,PreviousStressLevel,
                                 Transfer.Store,Transfer.Mobilize,
                                 Transfer.ToMobilize,Transfer.Bmobilized,Bin,Bout,
                                 TESTVALY);
        END
   ELSE BEGIN
        SenStage := undef_int;
        WeedRCi := undef_int; // no crop and no weed infestation
        CCiActualWeedInfested := 0.0; // no crop
        TactWeedInfested := 0.0; // no crop
        END;

(* 12. Reset after RUN *)
IF (PreDay = false) THEN PreviousDayNr := Simulation.FromDayNr - 1;
PreDay := true;
IF (DayNri >= Crop.Day1) THEN
   BEGIN
   CCiPrev := CCiActual;
   IF (ZiPrev < RootingDepth) THEN Ziprev := RootingDepth; // IN CASE groundwater table does not affect root development
   SumGDDPrev := Simulation.SumGDD;
   END;
IF (TargetTimeVal = 1) THEN IrriInterval := 0;

(* 13. Cuttings *)
IF GetManagement_Cuttings_Considered() THEN
   BEGIN
   HarvestNow := false;
   DayInSeason := DayNri - Crop.Day1 + 1;
   SumInterval := SumInterval + 1;
   SumGDDcuts := SumGDDcuts + GDDayi;
   CASE GetManagement_Cuttings_Generate() OF
        false : BEGIN
                IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) // adjust DayInSeason
                   THEN DayInSeason := DayNri - GetManagement_Cuttings_FirstDayNr() + 1;
                IF ((DayInSeason >= CutInfoRecord1.FromDay) AND (CutInfoRecord1.NoMoreInfo = false))
                   THEN BEGIN
                        HarvestNow := true;
                        GetNextHarvest;
                        END;
                 IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) // reset DayInSeason
                   THEN DayInSeason := DayNri - Crop.Day1 + 1;
                END;
        true  : BEGIN
                IF ((DayInSeason > CutInfoRecord1.ToDay) AND (CutInfoRecord1.NoMoreInfo = false))
                   THEN GetNextHarvest;
                CASE GetManagement_Cuttings_Criterion() OF
                     IntDay : BEGIN
                              IF ((SumInterval >= CutInfoRecord1.IntervalInfo)
                                   AND (DayInSeason >= CutInfoRecord1.FromDay)
                                   AND (DayInSeason <= CutInfoRecord1.ToDay))
                                 THEN HarvestNow := true;
                              END;
                     IntGDD : BEGIN
                              IF ((SumGDDcuts >= CutInfoRecord1.IntervalGDD)
                                   AND (DayInSeason >= CutInfoRecord1.FromDay)
                                   AND (DayInSeason <= CutInfoRecord1.ToDay))
                                 THEN HarvestNow := true;
                              END;
                     DryB   : BEGIN
                              IF (((SumWabal.Biomass - BprevSum) >= CutInfoRecord1.MassInfo)
                                                 AND (DayInSeason >= CutInfoRecord1.FromDay)
                                                 AND (DayInSeason <= CutInfoRecord1.ToDay))
                                 THEN HarvestNow := true;
                              END;
                     DryY   : BEGIN
                              IF (((SumWabal.YieldPart - YprevSum) >= CutInfoRecord1.MassInfo)
                                                   AND (DayInSeason >= CutInfoRecord1.FromDay)
                                                   AND (DayInSeason <= CutInfoRecord1.ToDay))
                                 THEN HarvestNow := true;
                              END;
                     FreshY : BEGIN
                              // OK if Crop.DryMatter = undef_int (not specified) HarvestNow remains false
                              IF ((((SumWabal.YieldPart - YprevSum)/(Crop.DryMatter/100)) >= CutInfoRecord1.MassInfo)
                                                                          AND (DayInSeason >= CutInfoRecord1.FromDay)
                                                                          AND (DayInSeason <= CutInfoRecord1.ToDay))
                                 THEN HarvestNow := true;

                              END;
                     end;

                END;
        end;
   IF (HarvestNow = true) THEN
      BEGIN
      NrCut := NrCut + 1;
      DayLastCut := DayInSeason;
      CGCadjustmentAfterCutting := false; // adjustement CGC
      IF (CCiPrev > (GetManagement_Cuttings_CCcut()/100)) THEN
         BEGIN
         CCiPrev := GetManagement_Cuttings_CCcut()/100;
         // ook nog CCwithered
         Crop.CCxWithered := 0;  // or CCiPrev ??
         CCxWitheredTpot := 0; // for calculation Maximum Biomass but considering soil fertility stress
         CCxWitheredTpotNoS := 0; //  for calculation Maximum Biomass unlimited soil fertility
         Crop.CCxAdjusted := CCiPrev; // new
         // Increase of CGC
         CGCadjustmentAfterCutting := true; // adjustement CGC
         END;
      // Record harvest
      IF Part1Mult THEN RecordHarvest(NrCut,DayNri,DayInSeason,SumInterval,BprevSum,YprevSum,fHarvest);
      // Reset
      SumInterval := 0;
      SumGDDcuts := 0;
      BprevSum := SumWaBal.Biomass;
      YprevSum := SumWaBal.YieldPart;
      END;
   END;

(* 14. Write results *)
//14.a Summation
SumETo := SumETo + ETo;
SumGDD := SumGDD + GDDayi;
//14.b Stress totals
IF (CCiActual > 0) THEN
   BEGIN
   // leaf expansion growth
   IF (StressLeaf > - 0.000001) THEN
      StressTot.Exp := ((StressTot.NrD - 1)*StressTot.Exp + StressLeaf)/StressTot.NrD;
   // stomatal closure
   IF (Tpot > 0) THEN
      BEGIN
      StressStomata := 100 *(1 - Tact/Tpot);
      IF (StressStomata > - 0.000001) THEN
         StressTot.Sto := ((StressTot.NrD - 1)*StressTot.Sto + StressStomata)/StressTot.NrD;
      END;
   END;
// weed stress
IF (WeedRCi > - 0.000001) THEN
   StressTot.Weed := ((StressTot.NrD - 1)*StressTot.Weed + WeedRCi)/StressTot.NrD;
//14.c Assign crop parameters
PlotVarCrop.ActVal := CCiActual/CCxCropWeedsNoSFstress * 100;
PlotVarCrop.PotVal := 100 * (1/CCxCropWeedsNoSFstress) *
                              CanopyCoverNoStressSF((VirtualTimeCC+Simulation.DelayedDays + 1),Crop.DaysToGermination,
                              Crop.DaysToSenescence,Crop.DaysToHarvest,
                              Crop.GDDaysToGermination,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                              (fWeedNoS*Crop.CCo),(fWeedNoS*Crop.CCx),CGCref,
                              (Crop.CDC*(fWeedNoS*Crop.CCx + 2.29)/(Crop.CCx + 2.29)),
                              GDDCGCref,(Crop.GDDCDC*(fWeedNoS*Crop.CCx + 2.29)/(Crop.CCx + 2.29)),
                              SumGDDadjCC,Crop.ModeCycle,
                              (0),(0));
IF ((VirtualTimeCC+Simulation.DelayedDays + 1) <= Crop.DaysToFullCanopySF)
   THEN BEGIN // not yet canopy decline with soil fertility stress
        PotValSF := 100 * (1/CCxCropWeedsNoSFstress) *
                         CanopyCoverNoStressSF((VirtualTimeCC+Simulation.DelayedDays + 1),Crop.DaysToGermination,
                         Crop.DaysToSenescence,Crop.DaysToHarvest,
                         Crop.GDDaysToGermination,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,
                         CCoTotal,CCxTotal,Crop.CGC,
                         CDCTotal,Crop.GDDCGC,GDDCDCTotal,
                         SumGDDadjCC,Crop.ModeCycle,
                         Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX);
        END
   ELSE GetPotValSF((VirtualTimeCC+Simulation.DelayedDays + 1),PotValSF);
//14.d Print ---------------------------------------
IF (OutputAggregate > 0) THEN CheckForPrint(TheProjectFile);
IF OutDaily THEN WriteDailyResults((DayNri-Simulation.DelayedDays-Crop.Day1+1),StageCode,WPi,fDaily);
IF (Part2Eval AND (ObservationsFile <> '(None)')) THEN WriteEvaluationData((DayNri-Simulation.DelayedDays-Crop.Day1+1),StageCode,fEval);

(* 15. Prepare Next day *)
//15.a Date
DayNri := DayNri + 1;
//15.b Irrigation
IF (DayNri = Crop.Day1)
   THEN IrriInterval := 1
   ELSE IrriInterval := IrriInterval + 1;
//15.c Rooting depth
//15.bis extra line for standalone
IF OutDaily THEN DetermineGrowthStage(DayNri,CCiPrev,StageCode);
// 15.extra - reset ageing of Kc at recovery after full senescence
IF (Simulation.SumEToStress >= 0.1) THEN DayLastCut := DayNri;
//15.d Read Climate next day, Get GDDays and update SumGDDays
IF (DayNri <= Simulation.ToDayNr) THEN
   BEGIN
   IF (GetEToFile() <> '(None)') THEN READLN(fEToSIM,ETo);
   IF (GetRainFile() <> '(None)') THEN READLN(fRainSIM,Rain);
   IF (TemperatureFile = '(None)')
      THEN BEGIN
           Tmin := SimulParam.Tmin;
           Tmax := SimulParam.Tmax;
           END
      ELSE READLN(fTempSIM,Tmin,Tmax);
   GDDayi := DegreesDay(Crop.Tbase,Crop.Tupper,Tmin,Tmax,SimulParam.GDDMethod);
   IF (DayNri >= Crop.Day1) THEN
      BEGIN
      Simulation.SumGDD := Simulation.SumGDD + GDDayi;
      Simulation.SumGDDfromDay1 := Simulation.SumGDDfromDay1 + GDDayi;
      END;
   END;

UNTIL ((DayNri-1) = RepeatToDay);  // END REPEAT

(* 16. Finalise *)
IF  ((DayNri-1) = Simulation.ToDayNr) THEN
    BEGIN
    // multiple cuttings
    IF Part1Mult THEN
       BEGIN
       IF (GetManagement_Cuttings_HarvestEnd() = true) THEN
          BEGIN  // final harvest at crop maturity
          NrCut := NrCut + 1;
          RecordHarvest(NrCut,DayNri,(DayNri-Crop.Day1+1),SumInterval,BprevSum,YprevSum,fHarvest);
          END;
       RecordHarvest((9999),DayNri,(DayNri-Crop.Day1+1),SumInterval,BprevSum,YprevSum,fHarvest); // last line at end of season
       END;
    // intermediate results
    IF ((OutputAggregate = 2) OR (OutputAggregate = 3) // 10-day and monthly results
        AND ((DayNri-1) > PreviousDayNr)) THEN
        BEGIN
        DayNri := DayNri-1;
        WriteIntermediatePeriod(TheProjectFile);
        END;
    //
    WriteSimPeriod(NrRun,TheProjectFile);
    END;

END; (* FileManagement *)





PROCEDURE RunSimulation(TheProjectFile : string;
                        TheProjectType : repTypeProject);
VAR NrRun : ShortInt;


    PROCEDURE AdjustCompartments;
    VAR TotDepth : double;
        i : ShortInt;
    BEGIN
    //Adjust size of compartments if required
    TotDepth := 0;
    FOR i := 1 to NrCompartments DO TotDepth := TotDepth + Compartment[i].Thickness;
    IF Simulation.MultipleRunWithKeepSWC // Project with a sequence of simulation runs and KeepSWC
       THEN BEGIN
            IF (ROUND(Simulation.MultipleRunConstZrx*1000) > ROUND(TotDepth*1000))
               THEN AdjustSizeCompartments(Simulation.MultipleRunConstZrx);
            END
       ELSE BEGIN
            IF (ROUND(Crop.RootMax*1000) > ROUND(TotDepth*1000)) THEN
               BEGIN
               IF (ROUND(Soil.RootMax*1000) = ROUND(Crop.RootMax*1000))
                  THEN BEGIN // no restrictive soil layer
                       AdjustSizeCompartments(Crop.RootMax);
                       // adjust soil water content
                       CalculateAdjustedFC((ZiAqua/100),Compartment);
                       IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
                       END
                  ELSE BEGIN // restrictive soil layer
                       IF (ROUND(Soil.RootMax*1000) > ROUND(TotDepth*1000)) THEN
                          BEGIN
                          AdjustSizeCompartments(Soil.RootMax);
                          // adjust soil water content
                          CalculateAdjustedFC((ZiAqua/100),Compartment);
                          IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
                          END
                       END;
               END;
            END;
    END; (* AdjustCompartments *)

    PROCEDURE CloseClimateFiles(VAR fEToSIM,fRainSIM,fTempSIM : text);
    BEGIN
    IF (GetEToFile() <> '(None)') THEN Close(fEToSIM);
    IF (GetRainFile() <> '(None)') THEN Close(fRainSIM);
    IF (TemperatureFile <> '(None)') THEN Close(fTempSIM);
    END; (* CloseClimateFiles *)

    PROCEDURE CloseIrrigationFile(VAR fIrri : text);
    BEGIN
    IF ((IrriMode = Manual) OR (IrriMode = Generate)) THEN Close(fIrri);
    END; (* CloseIrrigationFile *)

    PROCEDURE CloseManagementFile(VAR fCuts : text);
    BEGIN
    IF GetManagement_Cuttings_Considered() THEN Close(fCuts);
    END; (* CloseManagementFile *)


    PROCEDURE CloseEvalDataPerformEvaluation (NrRun : ShortInt;
                                              VAR fEval : text);
    VAR totalnameEvalStat,StrNr : string;

    BEGIN  (* CloseEvalDataPerformEvaluation *)
    // 1. Close Evaluation data file  and file with observations
    Close(fEval);
    IF (LineNrEval <> undef_int) THEN Close(fObs);
    // 2. Specify File name Evaluation of simulation results - Statistics
    StrNr := '';
    IF (Simulation.MultipleRun AND (Simulation.NrRuns > 1)) THEN Str(NrRun:3,StrNr);
    CASE TheProjectType OF
      TypePRO : totalnameEvalStat := CONCAT(PathNameOutp,OutputName,'PROevaluation.OUT');
      TypePRM : BEGIN
                Str(NrRun:3,StrNr);
                totalnameEvalStat := CONCAT(PathNameOutp,OutputName,'PRM',Trim(StrNr),'evaluation.OUT');
                END;
      end;
    // 3. Create Evaluation statistics file
    WriteAssessmentSimulation(StrNr,totalnameEvalStat,TheProjectType,
                              Simulation.FromDayNr,Simulation.ToDayNr);
    // 4. Delete Evaluation data file
    Erase(fEval);
    END; (* CloseEvalDataPerformEvaluation *)


BEGIN (* RunSimulation *)
OpenOutputRun(TheProjectType,fRun); // open seasonal results .out
IF OutDaily THEN OpenOutputDaily(TheProjectType,fDaily);  // Open Daily results .OUT
IF Part1Mult THEN OpenPart1MultResults(TheProjectType,fHarvest); // Open Multiple harvests in season .OUT
CASE TheProjectType OF
     TypePRO : BEGIN
               LoadSimulationRunProject(ProjectFileFull,(1));
               AdjustCompartments;
               GlobalZero(SumWabal);
               ResetPreviousSum(PreviousSum,SumETo,SumGDD,PreviousSumETo,PreviousSumGDD,PreviousBmob,PreviousBsto);
               InitializeSimulationRun;
               IF OutDaily THEN WriteTitleDailyResults(TheProjectType,(1),fDaily);
               IF Part1Mult THEN WriteTitlePart1MultResults(TheProjectType,(1),fHarvest);
               IF (Part2Eval AND (ObservationsFile <> '(None)')) THEN CreateEvalData((1),fObs,fEval);
               FileManagement((1),TheProjectFile,TheProjectType,fEToSIM,fRainSIM,fTempSIM,fIrri,fCuts);
               CloseClimateFiles(fEToSIM,fRainSIM,fTempSIM);
               CloseIrrigationFile(fIrri);
               CloseManagementFile(fCuts);
               IF (Part2Eval AND (ObservationsFile <> '(None)')) THEN CloseEvalDataPerformEvaluation((1),fEval);
               END;
     TypePRM : BEGIN
               FOR NrRun := 1 TO Simulation.NrRuns DO
                   BEGIN
                   LoadSimulationRunProject(MultipleProjectFileFull,NrRun);
                   AdjustCompartments;
                   GlobalZero(SumWabal);
                   ResetPreviousSum(PreviousSum,SumETo,SumGDD,PreviousSumETo,PreviousSumGDD,PreviousBmob,PreviousBsto);
                   InitializeSimulationRun;
                   IF OutDaily THEN WriteTitleDailyResults(TheProjectType,NrRun,fDaily);
                   IF Part1Mult THEN WriteTitlePart1MultResults(TheProjectType,NrRun,fHarvest);
                   IF (Part2Eval AND (ObservationsFile <> '(None)')) THEN CreateEvalData(NrRun,fObs,fEval);
                   FileManagement(NrRun,TheProjectFile,TheProjectType,fEToSIM,fRainSIM,fTempSIM,fIrri,fCuts);
                   CloseClimateFiles(fEToSIM,fRainSIM,fTempSIM);
                   CloseIrrigationFile(fIrri);
                   CloseManagementFile(fCuts);
                   IF (Part2Eval AND (ObservationsFile <> '(None)')) THEN CloseEvalDataPerformEvaluation(NrRun,fEval);
                   END;
               END;
     else;
     end;
     
Close(fRun); // Close Run.out
IF OutDaily THEN Close(fDaily);  // Close Daily.OUT
IF Part1Mult THEN Close(fHarvest);  // Close Multiple harvests in season
END; (* RunSimulation *)





end.
