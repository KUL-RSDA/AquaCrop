unit Run;

interface

uses Global, interface_global, interface_run, interface_rootunit, interface_tempprocessing, interface_climprocessing, interface_simul, interface_inforesults;

PROCEDURE InitializeSimulation(TheProjectFile_ : string;
                               TheProjectType : repTypeProject);

PROCEDURE FinalizeSimulation();

PROCEDURE InitializeRun(NrRun : ShortInt; TheProjectType : repTypeProject);

PROCEDURE AdvanceOneTimeStep();

PROCEDURE FinalizeRun1(NrRun : ShortInt;
                       TheProjectFile : string;
                       TheProjectType : repTypeProject);
PROCEDURE FinalizeRun2(NrRun : ShortInt; TheProjectType : repTypeProject);

PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);

implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;


var  fHarvest : text;
     SumETo, SumGDD, Ziprev,SumGDDPrev,TESTVAL : double;
     WaterTableInProfile,StartMode,NoMoreCrop : BOOLEAN;
     SumKcTop,SumKcTopStress,SumKci,Zeval,CCxCropWeedsNoSFstress,fWeedNoS,
     CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,WeedRCi,CCiActualWeedInfested : double;

     TimeSenescence : double; // calendar days or GDDays
     CGCadjustmentAfterCutting : BOOLEAN;
     BprevSum,YprevSum,SumGDDcuts : double;
     HItimesBEF,ScorAT1,SCorAT2,HItimesAT1,HItimesAT2,HItimesAT,alfaHI,alfaHIAdj : double;
     TheProjectFile : string;

     // DelayedGermination
     NextSimFromDayNr : LongInt; // the Simulation.FromDayNr for next run if delayed germination and KeepSWC

     // Evaluation
     DayNr1Eval,DayNrEval : LongInt;
     LineNrEval : INTEGER;
     
// specific for StandAlone
     PreviousSumETo,PreviousSumGDD : double;
     PreviousBmob,PreviousBsto : double;
     NoYear : BOOLEAN;
     StageCode : ShortInt;
     PreviousDayNr : LongInt;
     



PROCEDURE OpenOutputRun(TheProjectType : repTypeProject);
VAR totalname : string;

BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(GetPathNameOutp(),GetOutputName(),'PROseason.OUT');
      TypePRM : totalname := CONCAT(GetPathNameOutp(),GetOutputName(),'PRMseason.OUT');
      end;
fRun_open(totalname, 'w');
fRun_write('AquaCrop 7.0 (October 2021) - Output created on (date) : ' + DateToStr(Date) + '   at (time) : ' + TimeToStr(Time));
fRun_write('');
fRun_write('    RunNr     Day1   Month1    Year1     Rain      ETo       GD     CO2' +
           '      Irri   Infilt   Runoff    Drain   Upflow        E     E/Ex       Tr      TrW   Tr/Trx' +
           '    SaltIn   SaltOut    SaltUp  SaltProf' +
           '     Cycle   SaltStr  FertStr  WeedStr  TempStr   ExpStr   StoStr' +
           '  BioMass  Brelative   HI    Y(dry)  Y(fresh)    WPet      Bin     Bout     DayN   MonthN    YearN');
fRun_write('                                           mm       mm  degC.day    ppm' +
           '        mm       mm       mm       mm       mm       mm        %       mm       mm        %' +
           '    ton/ha    ton/ha    ton/ha    ton/ha' +
           '      days       %        %        %        %        %        %  ' +
           '  ton/ha        %       %    ton/ha   ton/ha    kg/m3   ton/ha   ton/ha');
END; (* OpenOutputRun *)


PROCEDURE OpenOutputDaily(TheProjectType : repTypeProject);
VAR totalname, tempstring : string;
BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(GetPathNameOutp(),GetOutputName(),'PROday.OUT');
      TypePRM : totalname := CONCAT(getPathNameOutp(),GetOutputName(),'PRMday.OUT');
      end;
fDaily_open(totalname, 'w');
WriteStr(tempstring, 'AquaCrop 7.0 (October 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
fDaily_write(tempstring);
END; (* OpenOutputDaily *)


PROCEDURE OpenPart1MultResults(TheProjectType : repTypeProject;
                               VAR fHarvest : text);
VAR totalname : string;
BEGIN
CASE TheProjectType OF
      TypePRO : totalname := CONCAT(GetPathNameOutp(),GetOutputName(),'PROharvests.OUT');
      TypePRM : totalname := CONCAT(GetPathNameOutp(),GetOutputName(),'PRMharvests.OUT');
      end;
Assign(fHarvest,totalname);
Rewrite(fHarvest);
WRITELN(fHarvest,'AquaCrop 7.0 (October 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
WRITELN(fHarvest,'Biomass and Yield at Multiple cuttings');
END; (* OpenPart1MultResults *)



PROCEDURE WriteTitleDailyResults(TheProjectType : repTypeProject;
                                 TheNrRun : ShortInt);
VAR Str1,Str2, tempstring : string;
    NodeD,Zprof : double;
    Compi : INTEGER;
BEGIN
// A. Run number
fDaily_write('');
IF (TheProjectType = TypePRM) THEN
   BEGIN
   Str(TheNrRun:4, Str1);
   WriteStr(tempstring, '   Run:',Str1);
   fDaily_write(tempstring);
   END;

// B. thickness of soil profile and root zone
IF ((Out1Wabal) OR (Out3Prof = true) OR (Out4Salt = true)) THEN
   BEGIN
   Zprof := 0;
   FOR compi :=1 to GetNrCompartments() DO Zprof := Zprof + GetCompartment_Thickness(compi);
   Str(Zprof:4:2,Str1);
   IF (ROUND(GetSoil().RootMax*1000) = ROUND(GetCrop().RootMax*1000))
      THEN Str(GetCrop().RootMax:4:2,Str2)
      ELSE Str(GetSoil().RootMax:4:2,Str2);
   END;

// C. 1st line title
//WriteStr(tempstring, '   Day Month  Year   DAP Stage');
fDaily_write('   Day Month  Year   DAP Stage', false);

// C1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) 
        THEN BEGIN
            WriteStr(tempstring, '   WC(',Str1,')   Rain     Irri   Surf   Infilt   RO    Drain       CR    Zgwt',
               '       Ex       E     E/Ex     Trx       Tr  Tr/Trx    ETx      ET  ET/ETx');
            fDaily_write(tempstring, False);
            END
        ELSE BEGIN
            WriteStr(tempstring, '   WC(',Str1,')   Rain     Irri   Surf   Infilt   RO    Drain       CR    Zgwt',
               '       Ex       E     E/Ex     Trx       Tr  Tr/Trx    ETx      ET  ET/ETx');
            fDaily_write(tempstring);
        END;
   END;
// C2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, '      GD       Z     StExp  StSto  StSen StSalt StWeed   CC      CCw     StTr  Kc(Tr)     Trx       Tr      TrW  Tr/Trx   WP',
        '    Biomass     HI    Y(dry)  Y(fresh)  Brelative    WPet      Bin     Bout');
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN 
      WriteStr(tempstring, '      GD       Z     StExp  StSto  StSen StSalt StWeed   CC      CCw     StTr  Kc(Tr)     Trx       Tr      TrW  Tr/Trx   WP',
        '    Biomass     HI    Y(dry)  Y(fresh)  Brelative    WPet      Bin     Bout');
      fDaily_write(tempstring);
      END;
   END;
// C3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   IF ((Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring,'  WC(',Str1,') Wr(',Str2,')     Z       Wr    Wr(SAT)    Wr(FC)   Wr(exp)   Wr(sto)   Wr(sen)   Wr(PWP)');
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring,'  WC(',Str1,') Wr(',Str2,')     Z       Wr    Wr(SAT)    Wr(FC)   Wr(exp)   Wr(sto)   Wr(sen)   Wr(PWP)');
      fDaily_write(tempstring);
      END;
   END;
// C4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN 
      BEGIN 
      WriteStr(tempstring,'    SaltIn    SaltOut   SaltUp   Salt(',Str1,')  SaltZ     Z       ECe    ECsw   StSalt  Zgwt    ECgw');
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring,'    SaltIn    SaltOut   SaltUp   Salt(',Str1,')  SaltZ     Z       ECe    ECsw   StSalt  Zgwt    ECgw');
      fDaily_write(tempstring);
      END;
   END;
// C5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
    fDaily_write('       WC01', false);
   FOR Compi := 2 TO (GetNrCompartments()-1) DO
       BEGIN
       Str(Compi:2,Str1);
       WriteStr(tempstring,'       WC',Str1);
       fDaily_write(tempstring, False);
       END;
   Str(GetNrCompartments():2,Str1);
   IF ((Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN 
      WriteStr(tempstring,'       WC',Str1);
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring, '       WC',Str1);
      fDaily_write(tempstring);
      END;
   END;
// C6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   fDaily_write('      ECe01', false);
   FOR Compi := 2 TO (GetNrCompartments()-1) DO
       BEGIN
       Str(Compi:2,Str1);
       WriteStr(tempstring,'      ECe',Str1);
       fDaily_write(tempstring, False);
       END;
   Str(GetNrCompartments():2,Str1);
   IF (Out7Clim = true) THEN
      BEGIN
      WriteStr(tempstring,'      ECe',Str1);
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring,'      ECe',Str1);
      fDaily_write(tempstring);
      END;
   END;
// C7. Climate input parameters
IF Out7Clim THEN fDaily_write('     Rain       ETo      Tmin      Tavg      Tmax      CO2');

fDaily_write('                              ', false);
// D1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN 
        BEGIN 
            WriteStr(tempstring, '        mm      mm       mm     mm     mm     mm       mm       mm      m ',
               '       mm       mm     %        mm       mm    %        mm      mm       %');
            fDaily_write(tempstring, False);
        END
      ELSE BEGIN
            WriteStr(tempstring,'        mm      mm       mm     mm     mm     mm       mm       mm      m ',
               '       mm       mm     %        mm       mm    %        mm      mm       %');
            fDaily_write(tempstring);
      END;
   END;
// D2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, '  degC-day     m       %      %      %      %      %      %       %       %       -        mm       mm       mm    %     g/m2',
        '    ton/ha      %    ton/ha   ton/ha       %       kg/m3   ton/ha   ton/ha');
      fDaily_write(tempstring, False);   
      END
      ELSE BEGIN
        WriteStr(tempstring, '  degC-day     m       %      %      %      %      %      %       %       %       -        mm       mm       mm    %     g/m2',
        '    ton/ha      %    ton/ha   ton/ha       %       kg/m3   ton/ha   ton/ha');
       fDaily_write(tempstring);  
      END;
   END;
// D3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   IF ((Out4Salt) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      fDaily_write('      mm       mm       m       mm        mm        mm        mm        mm        mm         mm', false)
      ELSE fDaily_write('      mm       mm       m       mm        mm        mm        mm        mm        mm        mm');
   END;
// D4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      fDaily_write('    ton/ha    ton/ha    ton/ha    ton/ha    ton/ha     m      dS/m    dS/m      %     m      dS/m', false)
      ELSE fDaily_write('    ton/ha    ton/ha    ton/ha    ton/ha    ton/ha     m      dS/m    dS/m      %     m      dS/m');
   END;
// D5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
   NodeD := GetCompartment_Thickness(1)/2;
   WriteStr(tempstring, NodeD:11:2);
   fDaily_write(tempstring, False);
   FOR Compi := 2 TO (GetNrCompartments()-1) DO
       BEGIN
       NodeD := NodeD + GetCompartment_Thickness(Compi-1)/2 + GetCompartment_Thickness(Compi)/2;
       WriteStr(tempstring, NodeD:11:2);
       fDaily_write(tempstring, False);
       END;
   NodeD := NodeD + GetCompartment_Thickness(GetNrCompartments()-1)/2 + GetCompartment_Thickness(GetNrCompartments())/2;
   IF ((Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, NodeD:11:2);
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring, NodeD:11:2);
      fDaily_write(tempstring);
      END;
   END;
// D6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   NodeD := GetCompartment_Thickness(1)/2;
   WriteStr(tempstring, NodeD:11:2);
   fDaily_write(tempstring, False);
   FOR Compi := 2 TO (GetNrCompartments()-1) DO
       BEGIN
       NodeD := NodeD + GetCompartment_Thickness(Compi-1)/2 + GetCompartment_Thickness(compi)/2;
       WriteStr(tempstring, NodeD:11:2);
       fDaily_write(tempstring, False);
       END;
   NodeD := NodeD + GetCompartment_Thickness(GetNrCompartments()-1)/2 + GetCompartment_Thickness(GetNrCompartments())/2;
   IF (Out7Clim = true) THEN
      BEGIN
      WriteStr(tempstring, NodeD:11:2);
      fDaily_write(tempstring, False);
      END
      ELSE BEGIN
      WriteStr(tempstring, NodeD:11:2);
      fDaily_write(tempstring);
      END;
   END;
// D7. Climate input parameters
IF Out7Clim THEN fDaily_write('       mm        mm     degC      degC      degC       ppm');
//end;
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
DetermineDate(GetCrop().Day1,Dayi,Monthi,Yeari);
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



PROCEDURE CreateEvalData(NrRun : ShortInt);
VAR dayi, monthi, yeari : INTEGER;
    StrNr,TempString : string;

BEGIN
// open input file with field data
fObs_open(GetObservationsFilefull(), 'r'); // Observations recorded in File
fObs_read(); // description
fObs_read(); // AquaCrop Version number
TempString := fObs_read();
ReadStr(TempString, Zeval); //  depth of sampled soil profile
TempString := fObs_read();
ReadStr(TempString, dayi);
TempString := fObs_read();
ReadStr(TempString, monthi);
TempString := fObs_read();
ReadStr(TempString, yeari);
DetermineDayNr(dayi,monthi,yeari,DayNr1Eval);
fObs_read(); // title
fObs_read(); // title
fObs_read(); // title
fObs_read(); // title
LineNrEval := undef_int;
TempString := fObs_read();
IF (NOT fObs_eof()) THEN
   BEGIN
   LineNrEval := 11;
   ReadStr(TempString, DayNrEval);
   DayNrEval := DayNr1Eval + DayNrEval -1;
   WHILE ((DayNrEval < GetSimulation_FromDayNr()) AND (LineNrEval <> undef_int)) DO
      BEGIN
      TempString := fObs_read();
      IF (fObs_eof())
         THEN LineNrEval := undef_int
         ELSE BEGIN
              LineNrEval := LineNrEval + 1;
              ReadStr(TempString, DayNrEval);
              DayNrEval := DayNr1Eval + DayNrEval -1;
              END;
      END;
   END;
IF (LineNrEval = undef_int) THEN fObs_close();
// open file with simulation results, field data
IF (GetSimulation_MultipleRun() AND (GetSimulation_NrRuns() > 1))
   THEN Str(NrRun:3,StrNr)
   ELSE StrNr := '';
SetfEval_filename(CONCAT(GetPathNameSimul(),'EvalData',Trim(StrNr),'.OUT'));
fEval_open(GetfEval_filename(), 'w');
fEval_write('AquaCrop 7.0 (June 2021) - Output created on (date) : ' + DateToStr(Date) + '   at (time) : ' + TimeToStr(Time));
fEval_write('Evaluation of simulation results - Data');
Str(Zeval:5:2,TempString);
fEval_write('                                                                                     for soil depth: ' + Trim(TempString) + ' m');
fEval_write('   Day Month  Year   DAP Stage   CCsim   CCobs   CCstd    Bsim      Bobs      Bstd   SWCsim  SWCobs   SWstd');
fEval_write('                                   %       %       %     ton/ha    ton/ha    ton/ha    mm       mm      mm');
END; (* CreateEvalData *)




PROCEDURE CreateDailyClimFiles(FromSimDay,ToSimDay : LongInt);
VAR totalname,totalnameOUT : string;
    fETo,fRain,fTemp,fEToS,fRainS,fTempS : text;
    StringREAD : ShortString;
    i : INTEGER;
    RunningDay : LongInt;
    tmpRain : double;
    ETo_temp : double;
    TminDataSet_temp, TmaxDataSet_temp : rep_SimulationEventsDbl;
    Tmin_temp, Tmax_temp : double;
    EToDataSet_temp, RainDataSet_temp : rep_SimulationEventsDbl;
BEGIN
// 1. ETo file
IF (GetEToFile() <> '(None)')
   THEN BEGIN
        totalname := GetEToFilefull();
        IF FileExists(totalname)
           THEN BEGIN
                // open file and find first day of simulation period
                CASE GetEToRecord_DataType() OF
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
                            FOR i := GetEToRecord_FromDayNr() TO (FromSimDay - 1) DO READLN(fETo);
                            READLN(fETo,ETo_temp);
                            SetETo(ETo_temp);
                            END;
                  Decadely: BEGIN
                            EToDataSet_temp := GetEToDataSet();
                            GetDecadeEToDataSet(FromSimDay,EToDataSet_temp);
                            SetEToDataSet(EToDataSet_temp);
                            i := 1;
                            While (GetEToDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                            SetETo(GetEToDataSet_i(i).Param);
                            END;
                  Monthly : BEGIN
                            EToDataSet_temp := GetEToDataSet();
                            GetMonthlyEToDataSet(FromSimDay,EToDataSet_temp);
                            SetEToDataSet(EToDataSet_temp);
                            i := 1;
                            While (GetEToDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                            SetETo(GetEToDataSet_i(i).Param);
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(GetPathNameSimul(),'EToData.SIM');
                Assign(fEToS,totalnameOUT);
                Rewrite(fEToS);
                WRITELN(fEToS,GetETo():10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE GetEToRecord_DataType() OF
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
                                           READLN(fETo,ETo_temp);
                                           SetETo(ETo_temp);
                                           END
                                      ELSE BEGIN
                                           READLN(fETo,ETo_temp);
                                           SetETo(ETo_temp);
                                           END;
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > GetEToDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        EToDataSet_temp := GetEToDataSet();
                                        GetDecadeEToDataSet(RunningDay,EToDataSet_temp);
                                        SetEToDataSet(EToDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetEToDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                   SetETo(GetEToDataSet_i(i).Param);
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > GetEToDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        EToDataSet_temp := GetEToDataSet();
                                        GetMonthlyEToDataSet(RunningDay,EToDataSet_temp);
                                        SetEToDataSet(EToDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetEToDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                   SetETo(GetEToDataSet_i(i).Param);
                                   END;
                         end;
                    WRITELN(fEToS,GetETo():10:4);
                    END;
                // Close files
                IF (GetEToRecord_DataType() = Daily) THEN Close(fETo);
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
             CASE GetRainRecord_DataType() OF
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
                            FOR i := GetRainRecord_FromDayNr() TO (FromSimDay - 1) DO READLN(fRain);
                               READLN(fRain,tmpRain);
                               SetRain(tmpRain);
                            END;
                  Decadely: BEGIN
                            RainDataSet_temp := GetRainDataSet();
                            GetDecadeRainDataSet(RunningDay,RainDataSet_temp);
                            SetRainDataSet(RainDataSet_temp);
                            i := 1;
                            While (GetRainDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                               SetRain(GetRainDataSet_i(i).Param);
                            END;
                  Monthly : BEGIN
                            RainDataSet_temp := GetRainDataSet();
                            GetMonthlyRainDataSet(RunningDay,RainDataSet_temp);
                            SetRainDataSet(RainDataSet_temp);
                            i := 1;
                            While (GetRainDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                               SetRain(GetRainDataSet_i(i).Param);
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(GetPathNameSimul(),'RainData.SIM');
                Assign(fRainS,totalnameOUT);
                Rewrite(fRainS);
                WRITELN(fRainS,GetRain():10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE GetRainRecord_DataType() OF
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
                                           READLN(fRain,tmpRain);
                                           SetRain(tmpRain);
                                           END
                                      ELSE BEGIN
                                         READLN(fRain,tmpRain);
                                         SetRain(tmpRain);
                                         END;
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > GetRainDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        RainDataSet_temp := GetRainDataSet();
                                        GetDecadeRainDataSet(RunningDay,RainDataSet_temp);
                                        SetRainDataSet(RainDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetRainDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                      SetRain(GetRainDataSet_i(i).Param);
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > GetRainDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        RainDataSet_temp := GetRainDataSet();
                                        GetMonthlyRainDataSet(RunningDay,RainDataSet_temp);
                                        SetRainDataSet(RainDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetRainDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                      SetRain(GetRainDataSet_i(i).Param);
                                   END;
                         end;
                    WRITELN(fRainS,GetRain():10:4);
                    END;
             // Close files
             IF (GetRainRecord_DataType() = Daily) THEN Close(fRain);
             Close(fRainS);
             END
        ELSE BEGIN
             //
             END;
        END;

// 3. Temperature file
IF (GetTemperatureFile() <> '(None)')
   THEN BEGIN
        totalname := GetTemperatureFilefull();
        IF FileExists(totalname)
           THEN BEGIN
                // open file and find first day of simulation period
                CASE GetTemperatureRecord().DataType OF
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
                            FOR i := GetTemperatureRecord().FromDayNr TO (FromSimDay - 1) DO READLN(fTemp);
                            READLN(fTemp,StringREAD);  // i.e. DayNri
                            Tmin_temp := GetTmin();
                            Tmax_temp := GetTmax();
                            SplitStringInTwoParams(StringREAD,Tmin_temp,Tmax_temp);
                            SetTmin(Tmin_temp);
                            SetTmax(Tmax_temp);
                            END;
                  Decadely: BEGIN
                            TminDataSet_temp := GetTminDataSet();
                            TmaxDataSet_temp := GetTmaxDataSet();
                            GetDecadeTemperatureDataSet(FromSimDay,TminDataSet_temp,TmaxDataSet_temp);
                            SetTminDataSet(TminDataSet_temp);
                            SetTmaxDataSet(TmaxDataSet_temp);
                            i := 1;
                            While (GetTminDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                            SetTmin(GetTminDataSet_i(i).Param);
                            SetTmax(GetTmaxDataSet_i(i).Param);
                            END;
                  Monthly : BEGIN
                            TminDataSet_temp := GetTminDataSet();
                            TmaxDataSet_temp := GetTmaxDataSet();
                            GetMonthlyTemperatureDataSet(FromSimDay,TminDataSet_temp,TmaxDataSet_temp);
                            SetTminDataSet(TminDataSet_temp);
                            SetTmaxDataSet(TmaxDataSet_temp);
                            i := 1;
                            While (GetTminDataSet_i(i).DayNr <> FromSimDay) Do i := i+1;
                            SetTmin(GetTminDataSet_i(i).Param);
                            SetTmax(GetTmaxDataSet_i(i).Param);
                            END;
                  end;
                // create SIM file and record first day
                totalnameOUT := CONCAT(GetPathNameSimul(),'TempData.SIM');
                Assign(fTempS,totalnameOUT);
                Rewrite(fTempS);
                WRITELN(fTempS,GetTmin():10:4,GetTmax():10:4);
                // next days of simulation period
                FOR RunningDay := (FromSimDay + 1) TO ToSimDay DO
                    BEGIN
                    CASE GetTemperatureRecord().DataType OF
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
                                           Tmin_temp := GetTmin();
                                           Tmax_temp := GetTmax();
                                           SplitStringInTwoParams(StringREAD,Tmin_temp,Tmax_temp);
                                           SetTmin(Tmin_temp);
                                           SetTmax(Tmax_temp);
                                           END
                                      ELSE BEGIN
                                           READLN(fTemp,Tmin_temp,Tmax_temp);
                                           SetTmin(Tmin_temp);
                                           SetTmax(Tmax_temp);
                                           END
                                   END;
                         Decadely: BEGIN
                                   IF (RunningDay > GetTminDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        TminDataSet_temp := GetTminDataSet();
                                        TmaxDataSet_temp := GetTmaxDataSet();
                                        GetDecadeTemperatureDataSet(FromSimDay,TminDataSet_temp,TmaxDataSet_temp);
                                        SetTminDataSet(TminDataSet_temp);
                                        SetTmaxDataSet(TmaxDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetTminDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                   SetTmin(GetTminDataSet_i(i).Param);
                                   SetTmax(GetTmaxDataSet_i(i).Param);
                                   END;
                         Monthly : BEGIN
                                   IF (RunningDay > GetTminDataSet_i(31).DayNr) THEN
                                        BEGIN
                                        TminDataSet_temp := GetTminDataSet();
                                        TmaxDataSet_temp := GetTmaxDataSet();
                                        GetMonthlyTemperatureDataSet(FromSimDay,TminDataSet_temp,TmaxDataSet_temp);
                                        SetTminDataSet(TminDataSet_temp);
                                        SetTmaxDataSet(TmaxDataSet_temp);
                                        END;
                                   i := 1;
                                   While (GetTminDataSet_i(i).DayNr <> RunningDay) Do i := i+1;
                                   SetTmin(GetTminDataSet_i(i).Param);
                                   SetTmax(GetTmaxDataSet_i(i).Param);
                                   END;
                         end;
                    WRITELN(fTempS,GetTmin():10:4,GetTmax():10:4);
                    END;
                // Close files
                IF (GetTemperatureRecord().DataType = Daily) THEN Close(fTemp);
                Close(fTempS);
                END
           ELSE BEGIN
                //
                END;
        END;
END; (* CreateDailyClimFiles *)


PROCEDURE OpenClimFilesAndGetDataFirstDay(FirstDayNr : LongInt);
VAR totalname : string;
    i : LongInt;
    tmpRain, ETo_temp : double;
    Tmin_temp, Tmax_temp : double;
    TempString : string;

BEGIN
// ETo file
IF (GetEToFile() <> '(None)') THEN
   BEGIN
   totalname := CONCAT(GetPathNameSimul(),'EToData.SIM');
   fEToSIM_open(totalname, 'r');
   IF (FirstDayNr = GetSimulation_FromDayNr())
      THEN BEGIN
           TempString := fEToSIM_read();
           ReadStr(TempString, ETo_temp);
           SetETo(ETo_temp);
           END
      ELSE BEGIN
           FOR i := GetSimulation_FromDayNr() TO (FirstDayNr - 1) DO 
                BEGIN 
                TempString := fEToSIM_read();
                ReadStr(TempString, ETo_temp);
                SetETo(ETo_temp);
                END;
           TempString := fEToSIM_read();
           ReadStr(TempString, ETo_temp);
           SetETo(ETo_temp);
           END;
   END;
// Rain file
IF (GetRainFile() <> '(None)') THEN
   BEGIN
   totalname := CONCAT(GetPathNameSimul(),'RainData.SIM');
   fRainSIM_open(totalname, 'r');
   IF (FirstDayNr = GetSimulation_FromDayNr())
      THEN BEGIN
         TempString := fRainSIM_read();
         ReadStr(TempString, tmpRain);
         SetRain(tmpRain);
         END
      ELSE BEGIN
           FOR i := GetSimulation_FromDayNr() TO (FirstDayNr - 1) DO
              BEGIN
                 TempString := fRainSIM_read();
                 ReadStr(TempString, tmpRain);
                 SetRain(tmpRain);
              END;
           TempString := fRainSIM_read();
           ReadStr(TempString, tmpRain);
           SetRain(tmpRain);
           END;
   END;
// Temperature file
IF (GetTemperatureFile() <> '(None)')
   THEN BEGIN
        totalname := CONCAT(GetPathNameSimul(),'TempData.SIM');
        fTempSIM_open(totalname, 'r');
        IF (FirstDayNr = GetSimulation_FromDayNr())
           THEN BEGIN
                TempString := fTempSIM_read();
                ReadStr(TempString, Tmin_temp, Tmax_temp);
                SetTmin(Tmin_temp);
                SetTmax(Tmax_temp);
                END
           ELSE BEGIN
                FOR i := GetSimulation_FromDayNr() TO (FirstDayNr - 1) DO 
                    BEGIN
                    TempString := fTempSIM_read();
                    ReadStr(TempString, Tmin_temp, Tmax_temp);
                    SetTmin(Tmin_temp);
                    SetTmax(Tmax_temp);
                    END;
                TempString := fTempSIM_read();
                ReadStr(TempString, Tmin_temp, Tmax_temp);
                SetTmin(Tmin_temp);
                SetTmax(Tmax_temp);
                END;
        END
   ELSE BEGIN
        SetTmin(GetSimulParam_Tmin());
        SetTmax(GetSimulParam_Tmax());
        END;
END; (* OpenClimFilesAndGetDataFirstDay *)



PROCEDURE GetNextHarvest;
VAR InfoLoaded : BOOLEAN;
    DayNrXX : LongInt;
    FromDay_temp : integer;
    IntervalInfo_temp, IntervalGDD_temp, MassInfo_temp : double;
    TempString : string;
BEGIN
CASE GetManagement_Cuttings_Generate() OF
 false: BEGIN
        TempString := fCuts_read();
        IF (NOT fCuts_eof())
           THEN BEGIN
                ReadStr(TempString, FromDay_temp);
                SetCutInfoRecord1_FromDay(FromDay_temp);
                SetCutInfoRecord1_NoMoreInfo(false);
                IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) THEN
                   BEGIN // scroll to start growing cycle
                   DayNrXX := GetManagement_Cuttings_FirstDayNr() + GetCutInfoRecord1_FromDay() -1;
                   WHILE ((DayNrXX < GetCrop().Day1) AND (GetCutInfoRecord1_NoMoreInfo() = false)) DO
                     BEGIN
                     TempString := fCuts_read();
                     IF (NOT fCuts_eof())
                        THEN BEGIN
                             ReadStr(TempString, FromDay_temp);
                             SetCutInfoRecord1_FromDay(FromDay_temp);
                             DayNrXX := GetManagement_Cuttings_FirstDayNr() + GetCutInfoRecord1_FromDay() -1;
                             END
                        ELSE SetCutInfoRecord1_NoMoreInfo(true);
                     END; // while loop
                   END; // scroll to start growing cycle
                END
           ELSE SetCutInfoRecord1_NoMoreInfo(true);
        END;
 true : BEGIN
        IF (GetNrCut() = 0) THEN
           BEGIN
           CASE GetManagement_Cuttings_Criterion() OF
                IntDay :             BEGIN
                                     TempString := fCuts_read();
                                     ReadStr(TempString, FromDay_temp, IntervalInfo_temp);
                                     SetCutInfoRecord1_FromDay(FromDay_temp);
                                     SetCutInfoRecord1_IntervalInfo(IntervalInfo_temp)
                                     END;
                IntGDD :             BEGIN
                                     TempString := fCuts_read();
                                     ReadStr(TempString, FromDay_temp, IntervalGDD_temp);
                                     SetCutInfoRecord1_FromDay(FromDay_temp);
                                     SetCutInfoRecord1_IntervalGDD(IntervalGDD_temp)
                                     END;
                DryB,DryY,FreshY :   BEGIN
                                     TempString := fCuts_read();
                                     ReadStr(TempString, FromDay_temp, MassInfo_temp);
                                     SetCutInfoRecord1_FromDay(FromDay_temp);
                                     SetCutInfoRecord1_MassInfo(MassInfo_temp)
                                     END;
                end;
           IF (GetCutInfoRecord1_FromDay() < GetManagement_Cuttings_Day1()) THEN SetCutInfoRecord1_FromDay(GetManagement_Cuttings_Day1());
           END;
        InfoLoaded := false;
        REPEAT
        TempString := fCuts_read();
        IF (NOT fCuts_eof())
           THEN BEGIN
                CASE GetManagement_Cuttings_Criterion() OF
                     IntDay :           BEGIN
                                        ReadStr(TempString, FromDay_temp, IntervalInfo_temp);
                                        SetCutInfoRecord2_FromDay(FromDay_temp);
                                        SetCutInfoRecord2_IntervalInfo(IntervalInfo_temp);
                                        END;
                     IntGDD :           BEGIN
                                        ReadStr(TempString, FromDay_temp, IntervalGDD_temp);
                                        SetCutInfoRecord2_FromDay(FromDay_temp);
                                        SetCutInfoRecord2_IntervalGDD(IntervalGDD_temp);
                                        END;
                     DryB,DryY,FreshY : BEGIN
                                        ReadStr(TempString, FromDay_temp, MassInfo_temp);
                                        SetCutInfoRecord2_FromDay(FromDay_temp);
                                        SetCutInfoRecord2_MassInfo(MassInfo_temp);
                                        END
                     end;
                IF (GetCutInfoRecord2_FromDay() < GetManagement_Cuttings_Day1()) THEN SetCutInfoRecord2_FromDay(GetManagement_Cuttings_Day1());
                IF (GetCutInfoRecord2_FromDay() <= GetCutInfoRecord1_FromDay())
                   THEN BEGIN // CutInfoRecord2 becomes CutInfoRecord1
                        SetCutInfoRecord1_FromDay(GetCutInfoRecord2_FromDay());
                        CASE GetManagement_Cuttings_Criterion() OF
                             IntDay : SetCutInfoRecord1_IntervalInfo(GetCutInfoRecord2_IntervalInfo());
                             IntGDD : SetCutInfoRecord1_IntervalGDD(GetCutInfoRecord2_IntervalGDD());
                             DryB,DryY,FreshY : SetCutInfoRecord1_MassInfo(GetCutInfoRecord2_MassInfo());
                             end;
                        SetCutInfoRecord1_NoMoreInfo(false);
                        END
                   ELSE BEGIN // complete CutInfoRecord1
                        SetCutInfoRecord1_ToDay(GetCutInfoRecord2_FromDay() - 1);
                        SetCutInfoRecord1_NoMoreInfo(false);
                        IF (GetManagement_Cuttings_NrDays() <> undef_int) THEN
                           BEGIN
                           IF (GetCutInfoRecord1_ToDay() > (GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1)) THEN
                              BEGIN
                              SetCutInfoRecord1_ToDay(GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1);
                              SetCutInfoRecord1_NoMoreInfo(true);
                              END;
                           END;
                        InfoLoaded := true;
                        END;
                END
           ELSE BEGIN // Eof(fCuts)
                IF (GetNrCut() > 0) THEN
                   BEGIN  // CutInfoRecord2 becomes CutInfoRecord1
                   SetCutInfoRecord1_FromDay(GetCutInfoRecord2_FromDay());
                   CASE GetManagement_Cuttings_Criterion() OF
                        IntDay : SetCutInfoRecord1_IntervalInfo(GetCutInfoRecord2_IntervalInfo());
                        IntGDD : SetCutInfoRecord1_IntervalGDD(GetCutInfoRecord2_IntervalGDD());
                        DryB,DryY,FreshY : SetCutInfoRecord1_MassInfo(GetCutInfoRecord2_MassInfo());
                        end;
                   END;
                SetCutInfoRecord1_ToDay(GetCrop().DaysToHarvest);
                IF (GetManagement_Cuttings_NrDays() <> undef_int) THEN
                   BEGIN
                   IF (GetCutInfoRecord1_ToDay() > (GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1)) THEN
                      SetCutInfoRecord1_ToDay(GetManagement_Cuttings_Day1() + GetManagement_Cuttings_NrDays() -1);
                   END;
                SetCutInfoRecord1_NoMoreInfo(true);
                InfoLoaded := true;
                END;
        UNTIL (InfoLoaded = true);
        END;
 end;
END; (* GetNextHarvest *)




PROCEDURE OpenHarvestInfo();
VAR totalname : string;
    i : ShortInt;
BEGIN
IF (getManFile() <> '(None)')
   THEN totalname := GetManFileFull()
   ELSE totalname := CONCAT(GetPathNameSimul(),'Cuttings.AqC');
fCuts_open(totalname, 'r');
fCuts_read(); // description
fCuts_read(); // AquaCrop version
IF (GetManFile() <> '(None)') THEN For i:= 1 to 10 DO fCuts_read(); // management info
FOR i := 1 TO 12 DO fCuts_read();  // cuttings info (already loaded)
GetNextHarvest;
END; (* OpenHarvestInfo *)


PROCEDURE InitializeSimulationRun;
VAR tHImax,DNr1,DNr2,Dayi,DayCC : integer;
    SumGDDforDayCC : double;
    CCiniMin,CCiniMax,RatDGDD, fWeed, fi : double;
    Cweed : ShortInt;
    Day1,Month1,Year1 : INTEGER;
    FertStress : shortint;
    ECe_temp, ECsw_temp, ECswFC_temp, KsSalt_temp : double;
    GwTable_temp : rep_GwTable;
    RedCGC_temp, RedCCX_temp, RCadj_temp : ShortInt; 
    EffectStress_temp : rep_EffectStress;
    SumGDD_temp, SumGDDFromDay1_temp, FracBiomassPotSF_temp : double;
    bool_temp : boolean;
    Crop_DaysToFullCanopySF_temp : integer;
    Coeffb0_temp, Coeffb1_temp, Coeffb2_temp : double;
    Coeffb0Salt_temp,Coeffb1Salt_temp,Coeffb2Salt_temp : double;

BEGIN
//1. Adjustments at start
//1.1 Adjust soil water and salt content if water table IN soil profile
CheckForWaterTableInProfile((GetZiAqua()/100),GetCompartment(),WaterTableInProfile);
IF WaterTableInProfile THEN AdjustForWatertable; 
IF (NOT GetSimulParam_ConstGwt()) THEN BEGIN
    GwTable_temp := GetGwTable();
    GetGwtSet(GetSimulation_FromDayNr(),GwTable_temp);
    SetGwTable(GwTable_temp);
    END;

// 1.2 Check if FromDayNr simulation needs to be adjusted from previous run if Keep initial SWC
IF ((GetSWCIniFile() = 'KeepSWC') AND (NextSimFromDayNr <> undef_int)) THEN
   BEGIN  // assign the adjusted DayNr defined in previous run
   IF (NextSimFromDayNr <= GetCrop().Day1) THEN SetSimulation_FromDayNr(NextSimFromDayNr);
   END;
NextSimFromDayNr := undef_int;

// 2. initial settings for Crop
SetCrop_pActStom(GetCrop().pdef);
SetCrop_pSenAct(GetCrop().pSenescence);
SetCrop_pLeafAct(GetCrop().pLeafDefUL);
SetEvapoEntireSoilSurface(true);
SetSimulation_EvapLimitON(false);
SetSimulation_EvapWCsurf(0);
SetSimulation_EvapZ(EvapZmin/100);
SetSimulation_SumEToStress(0);
SetCCxWitheredTpot(0); // for calculation Maximum Biomass and considering soil fertility stress
SetCCxWitheredTpotNoS(0); //  for calculation Maximum Biomass unlimited soil fertility
SetSimulation_DayAnaero(0); // days of anaerobic condictions in global root zone
// germination
IF ((GetCrop().Planting = Seed) AND (GetSimulation_FromDayNr() <= GetCrop().Day1))
   THEN SetSimulation_Germinate(false)
   ELSE BEGIN
        SetSimulation_Germinate(true);
        // since already germinated no protection required
        SetSimulation_ProtectedSeedling(false);
        END;
// delayed germination
SetSimulation_DelayedDays(0);

// 3. create temperature file covering crop cycle
IF (GetTemperatureFile() <> '(None)') THEN
   BEGIN
   IF (GetSimulation_ToDayNr() < GetCrop().DayN)
      THEN TemperatureFileCoveringCropPeriod(GetCrop().Day1,GetSimulation_TodayNr())
      ELSE TemperatureFileCoveringCropPeriod(GetCrop().Day1,GetCrop().DayN);
   END;

// 4. CO2 concentration during cropping period
DNr1 := GetSimulation_FromDayNr();
IF (GetCrop().Day1 > GetSimulation_FromDayNr()) THEN DNr1 := GetCrop().Day1;
DNr2 := GetSimulation_ToDayNr();
IF (GetCrop().DayN < GetSimulation_ToDayNr()) THEN DNr2 := GetCrop().DayN;
SetCO2i(CO2ForSimulationPeriod(DNr1,DNr2));

// 5. seasonals stress coefficients
bool_temp := ((GetCrop().ECemin <> undef_int) AND (GetCrop().ECemax <> undef_int)) AND (GetCrop().ECemin < GetCrop().ECemax);
SetSimulation_SalinityConsidered(bool_temp);
IF (GetIrriMode() = Inet) THEN SetSimulation_SalinityConsidered(false);
SetStressTot_NrD(undef_int);
SetStressTot_Salt(0);
SetStressTot_Temp(0);
SetStressTot_Exp(0);
SetStressTot_Sto(0);
SetStressTot_Weed(0);

// 6. Soil fertility stress
// Coefficients for soil fertility - biomass relationship
  // AND for Soil salinity - CCx/KsSto relationship
RelationshipsForFertilityAndSaltStress();

// No soil fertility stress
IF (GetManagement_FertilityStress() <= 0) THEN SetManagement_FertilityStress(0);

// Reset soil fertility parameters to selected value in management
EffectStress_temp := GetSimulation_EffectStress();
CropStressParametersSoilFertility(GetCrop_StressResponse(),GetManagement_FertilityStress(),EffectStress_temp);
SetSimulation_EffectStress(EffectStress_temp);
FertStress := GetManagement_FertilityStress();
RedCGC_temp := GetSimulation_EffectStress_RedCGC();
RedCCX_temp := GetSimulation_EffectStress_RedCCX();
Crop_DaysToFullCanopySF_temp := GetCrop().DaysToFullCanopySF;
TimeToMaxCanopySF(GetCrop().CCo,GetCrop().CGC,GetCrop().CCx,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,
                    GetCrop().DaysToFlowering,GetCrop().LengthFlowering,GetCrop().DeterminancyLinked,
                    Crop_DaysToFullCanopySF_temp,RedCGC_temp,
                    RedCCX_temp,FertStress);
SetCrop_DaysToFullCanopySF(Crop_DaysToFullCanopySF_temp);
SetManagement_FertilityStress(FertStress);
SetSimulation_EffectStress_RedCGC(RedCGC_temp);
SetSimulation_EffectStress_RedCCX(RedCCX_temp);
SetPreviousStressLevel(GetManagement_FertilityStress());
SetStressSFadjNEW(GetManagement_FertilityStress());
// soil fertility and GDDays
IF (GetCrop_ModeCycle() = GDDays) THEN
   BEGIN
   IF (GetManagement_FertilityStress() <> 0)
      THEN SetCrop_GDDaysToFullCanopySF(GrowingDegreeDays(GetCrop().DaysToFullCanopySF,GetCrop().Day1,GetCrop().Tbase,GetCrop().Tupper,GetSimulParam_Tmin(),GetSimulParam_Tmax()))
      ELSE SetCrop_GDDaysToFullCanopySF(GetCrop().GDDaysToFullCanopy);
   END;

// Maximum sum Kc (for reduction WP in season if soil fertility stress)
SumKcTop := SeasonalSumOfKcPot(GetCrop().DaysToCCini,GetCrop().GDDaysToCCini,
                 GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                 GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopy,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                 GetCrop().CCo,GetCrop().CCx,GetCrop().CGC,GetCrop().GDDCGC,GetCrop().CDC,GetCrop().GDDCDC,
                 GetCrop().KcTop,GetCrop().KcDecline,GetCrop().CCEffectEvapLate,
                 GetCrop().Tbase,GetCrop().Tupper,GetSimulParam_Tmin(),GetSimulParam_Tmax(),GetCrop().GDtranspLow,GetCO2i(),
                 GetCrop_ModeCycle());
SumKcTopStress := SumKcTop * GetFracBiomassPotSF();
SumKci := 0;

// 7. weed infestation and self-thinning of herbaceous perennial forage crops
// CC expansion due to weed infestation and/or CC decrease as a result of self-thinning
// 7.1 initialize
SetSimulation_RCadj(GetManagement_WeedRC());
Cweed := 0;
IF (GetCrop_subkind() = Forage)
   THEN fi := MultiplierCCxSelfThinning(GetSimulation_YearSeason(),GetCrop().YearCCx,GetCrop().CCxRoot)
   ELSE fi := 1;
// 7.2 fweed
IF (GetManagement_WeedRC() > 0)
   THEN BEGIN
        fWeedNoS := CCmultiplierWeed(GetManagement_WeedRC(),GetCrop().CCx,GetManagement_WeedShape());
        CCxCropWeedsNoSFstress := ROUND(((100* GetCrop().CCx * fWeedNoS) + 0.49))/100; // reference for plot with weed
        IF (GetManagement_FertilityStress() > 0)
           THEN BEGIN
                fWeed := 1;
                IF ((fi > 0) AND (GetCrop_subkind() = Forage)) THEN
                   BEGIN
                   Cweed := 1;
                   IF (fi > 0.005)
                      THEN BEGIN // calculate the adjusted weed cover
                           SetSimulation_RCadj(ROUND(GetManagement_WeedRC()
                                 //+ Cweed*(1-fi)*Crop.CCx*(1-Simulation.EffectStress.RedCCX/100)*Management.WeedAdj);
                                 + Cweed*(1-fi)*GetCrop().CCx*(1-GetSimulation_EffectStress_RedCCX()/100)*GetManagement_WeedAdj()/100));
                           IF (GetSimulation_RCadj() < (100 * (1- fi/(fi + (1-fi)*(GetManagement_WeedAdj()/100)))))
                              THEN SetSimulation_RCadj(ROUND(100 * (1- fi/(fi + (1-fi)*(GetManagement_WeedAdj()/100)))));
                           IF (GetSimulation_RCadj() > 100) THEN SetSimulation_RCadj(98);
                           END
                      ELSE SetSimulation_RCadj(100);
                   END;
                END
           ELSE BEGIN
                IF (GetCrop_subkind() = Forage)
                   THEN BEGIN
                        RCadj_temp := GetSimulation_RCadj();
                        fweed := CCmultiplierWeedAdjusted(GetManagement_WeedRC(),GetCrop().CCx,GetManagement_WeedShape(),
                                        fi,GetSimulation_YearSeason(),GetManagement_WeedAdj(),RCadj_temp);
                        SetSimulation_RCadj(RCadj_temp);
                        END
                   ELSE fWeed := fWeedNoS;
                END;
        END
   ELSE BEGIN
        fWeedNoS := 1;
        fWeed := 1;
        CCxCropWeedsNoSFstress := GetCrop().CCx;
        END;
// 7.3 CC total due to weed infestation
CCxTotal := fWeed * GetCrop().CCx * (fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100);
CDCTotal := GetCrop().CDC * (fWeed*GetCrop().CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29)/
                       (GetCrop().CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29);
GDDCDCTotal := GetCrop().GDDCDC * (fWeed*GetCrop().CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29)/
                       (GetCrop().CCx*(fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100) + 2.29);
IF (GetCrop_subkind() = Forage)
   THEN fi := MultiplierCCoSelfThinning(GetSimulation_YearSeason(),GetCrop().YearCCx,GetCrop().CCxRoot)
   ELSE fi := 1;
CCoTotal := fWeed * GetCrop().CCo * (fi+Cweed*(1-fi)*GetManagement_WeedAdj()/100);

// 8. prepare output files
// Not applicable

// 9. first day
StartMode := true;
bool_temp := (NOT GetSimulation_ResetIniSWC());
SetPreDay(bool_temp);
SetDayNri(GetSimulation_FromDayNr());
DetermineDate(GetSimulation_FromDayNr(),Day1,Month1,Year1); // start simulation run
NoYear := (Year1 = 1901);  // for output file


// 10. Climate
// create climate files
CreateDailyClimFiles(GetSimulation_FromDayNr(),GetSimulation_ToDayNr());
// climatic data for first day
OpenClimFilesAndGetDataFirstDay(GetDayNri());

// Sum of GDD before start of simulation
SetSimulation_SumGDD(0);
SetSimulation_SumGDDfromDay1(0);
IF ((GetCrop_ModeCycle() = GDDays) AND (GetCrop().Day1 < GetDayNri()))
   THEN BEGIN
        SumGDD_temp := GetSimulation_SumGDD();
        SumGDDfromDay1_temp := GetSimulation_SumGDDfromDay1();
        GetSumGDDBeforeSimulation(SumGDD_temp,SumGDDfromDay1_temp); // GDDays before start of simulation
        SetSimulation_SumGDD(SumGDD_temp);
        SetSimulation_SumGDDFromDay1(SumGDDFromDay1_temp);
        END;
SumGDDPrev := GetSimulation_SumGDDfromDay1();

// Sum of GDD at end of first day
SetGDDayi(DegreesDay(GetCrop().Tbase,GetCrop().Tupper,GetTmin(),GetTmax(),GetSimulParam_GDDMethod()));
IF (GetDayNri() >= GetCrop().Day1)
   THEN BEGIN
        IF (GetDayNri() = GetCrop().Day1) THEN SetSimulation_SumGDD(GetSimulation_SumGDD() + GetGDDayi());
        SetSimulation_SumGDDfromDay1(GetSimulation_SumGDDfromDay1() + GetGDDayi());
        END;
// Reset cummulative sums of ETo and GDD for Run output
SumETo := 0;
SumGDD := 0;


// 11. Irrigation
SetIrriInterval(1);
SetGlobalIrriECw(true); // In Versions < 3.2 - Irrigation water quality is not yet recorded on file
OpenIrrigationFile();


// 12. Adjusted time when starting as regrowth
IF (GetCrop().DaysToCCini <> 0)
   THEN BEGIN  // regrowth
        SetGDDTadj(undef_int);
        SetGDDayFraction(undef_int);
        IF (GetCrop().DaysToCCini = undef_int)
           THEN SetTadj(GetCrop().DaysToFullCanopy - GetCrop().DaysToGermination)
           ELSE SetTadj(GetCrop().DaysToCCini);
        SetDayFraction((GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopy)/(GetTadj() + GetCrop().DaysToGermination + (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopy) ));
        IF (GetCrop_ModeCycle() = GDDays) THEN
           BEGIN
           IF (GetCrop().GDDaysToCCini = undef_int)
              THEN SetGDDTadj(GetCrop().GDDaysToFullCanopy - GetCrop().GDDaysToGermination)
              ELSE SetGDDTadj(GetCrop().GDDaysToCCini);
           SetGDDayFraction((GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopy)/(GetGDDTadj() + GetCrop().GDDaysToGermination + (GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopy)));
           END;
        END
   ELSE BEGIN // sowing or transplanting
        SetTadj(0);
        SetGDDTadj(0);
        SetDayFraction(undef_int);
        SetGDDayFraction(undef_int);
        END;


// 13. Initial canopy cover
// 13.1 default value
// 13.1a RatDGDD for simulation of CanopyCoverNoStressSF (CCi with decline)
RatDGDD := 1;
IF (GetCrop_ModeCycle() = GDDays) THEN
   BEGIN
   IF (GetCrop().GDDaysToFullCanopySF < GetCrop().GDDaysToSenescence)
      THEN RatDGDD := (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF)/(GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF);
   END;
// 13.1b DayCC for initial canopy cover
Dayi := GetDayNri() - GetCrop().Day1;
IF (GetCrop().DaysToCCini = 0)
   THEN BEGIN // sowing or transplant
        DayCC := Dayi;
        SetDayFraction( undef_int );
        END
   ELSE BEGIN
        // adjust time (calendar days) for regrowth
        DayCC := Dayi + GetTadj() + GetCrop().DaysToGermination; // adjusted time scale
        IF (DayCC > GetCrop().DaysToHarvest) THEN DayCC := GetCrop().DaysToHarvest; // special case where L123 > L1234
        IF (DayCC > GetCrop().DaysToFullCanopy) THEN
           BEGIN
           IF (Dayi <= GetCrop().DaysToSenescence)
              THEN DayCC := GetCrop().DaysToFullCanopy  + ROUND(GetDayFraction() * (Dayi+GetTadj()+GetCrop().DaysToGermination - GetCrop().DaysToFullCanopy)) // slow down
              ELSE DayCC := Dayi; // switch time scale
           END;
        END;
// 13.1c SumGDDayCC for initial canopy cover
SumGDDforDayCC := undef_int;
IF (GetCrop_ModeCycle() = GDDays) THEN
   BEGIN
   IF (GetCrop().GDDaysToCCini = 0)
      THEN SumGDDforDayCC := GetSimulation_SumGDDfromDay1() - GetGDDayi()
      ELSE BEGIN
           // adjust time (Growing Degree Days) for regrowth
           SumGDDforDayCC := GetSimulation_SumGDDfromDay1() - GetGDDayi() + GetGDDTadj() + GetCrop().GDDaysToGermination;
           IF (SumGDDforDayCC > GetCrop().GDDaysToHarvest) THEN SumGDDforDayCC := GetCrop().GDDaysToHarvest; // special case where L123 > L1234
           IF (SumGDDforDayCC > GetCrop().GDDaysToFullCanopy) THEN
              BEGIN
              IF (GetSimulation_SumGDDfromDay1() <= GetCrop().GDDaysToFullCanopy)
                 THEN SumGDDforDayCC := GetCrop().GDDaysToFullCanopy + ROUND(GetGDDayFraction() * (GetSimulation_SumGDDfromDay1()+GetGDDTadj()+GetCrop().GDDaysToGermination-GetCrop().GDDaysToFullCanopy)) // slow down
                 ELSE SumGDDforDayCC := GetSimulation_SumGDDfromDay1() - GetGDDayi(); // switch time scale
              END;
           END;
   END;
// 13.1d CCi at start of day (is CCi at end of previous day)
IF (GetDayNri() <= GetCrop().Day1)
   THEN BEGIN
        IF (GetCrop().DaysToCCini <> 0)
           THEN BEGIN  // regrowth which starts on 1st day
                IF (GetDayNri() = GetCrop().Day1)
                   THEN BEGIN
                        SetCCiPrev(CCiNoWaterStressSF(DayCC,
                                      GetCrop().DaysToGermination,
                                      GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                      GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopySF,
                                      GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                      CCoTotal,CCxTotal,GetCrop().CGC,GetCrop().GDDCGC,CDCTotal,GDDCDCTotal,
                                      SumGDDforDayCC,RatDGDD,
                                      GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),
                                      GetSimulation_EffectStress_CDecline(),GetCrop_ModeCycle()));
                        END
                   ELSE SetCCiPrev(0);
                END
           ELSE BEGIN // sowing or transplanting
                SetCCiPrev(0);
                IF (GetDayNri() = (GetCrop().Day1+GetCrop().DaysToGermination)) THEN SetCCiPrev(CCoTotal);
                END;
        END
   ELSE BEGIN
        IF (GetDayNri() > GetCrop().DayN)
           THEN SetCCiPrev(0)  // after cropping period
           ELSE BEGIN
                SetCCiPrev(CCiNoWaterStressSF(DayCC,
                              GetCrop().DaysToGermination,
                              GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                              GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopySF,
                              GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                              CCoTotal,CCxTotal,GetCrop().CGC,GetCrop().GDDCGC,CDCTotal,GDDCDCTotal,
                              SumGDDforDayCC,RatDGDD,
                              GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),
                              GetSimulation_EffectStress_CDecline(),GetCrop_ModeCycle()));
                END;
        END;
// 13.2 specified CCini (%)
IF ((GetSimulation_CCini() > 0) AND (ROUND(10000*GetCCiPrev()) > 0) AND (ROUND(GetSimulation_CCini()) <> ROUND(100*GetCCiPrev())))
   THEN BEGIN
        // 13.2a Minimum CC
        CCiniMin := 100 * (GetCrop().SizeSeedling/10000)*(GetCrop().PlantingDens/10000);
        IF (CCiniMin - INT(CCiniMin*100)/100) >= 0.00001
           THEN CCiniMin := INT(CCiniMin*100 + 1)/100
           ELSE CCiniMin := INT(CCiniMin*100)/100;
        // 13.2b Maximum CC
        CCiniMax := 100 * GetCCiPrev();
        CCiniMax := INT(CCiniMax*100)/100;
        // 13.2c accept specified CCini
        IF ((GetSimulation_CCini() >= CCiniMin) AND (GetSimulation_CCini() <= CCiniMax)) THEN SetCCiPrev(GetSimulation_CCini()/100);
        END;
// 13.3
SetCrop_CCxAdjusted(CCxTotal);
SetCrop_CCoAdjusted(CCoTotal);
TimeSenescence := 0;
SetCrop_CCxWithered(0);
NoMoreCrop := false;
SetCCiActual(GetCCiPrev());


// 14. Biomass and re-setting of GlobalZero
IF (ROUND(1000*GetSimulation_Bini()) > 0) THEN BEGIN // overwrite settings in GlobalZero (in Global)
    SetSumWaBal_Biomass(GetSimulation_Bini());
    SetSumWaBal_BiomassPot(GetSimulation_Bini());
    SetSumWaBal_BiomassUnlim(GetSimulation_Bini());
    SetSumWaBal_BiomassTot(GetSimulation_Bini());
    END;


// 15. Transfer of assimilates
IF ((GetCrop_subkind() = Forage) // only valid for perennial herbaceous forage crops
   AND (Trim(GetCropFileFull()) = Trim(GetSimulation_Storage_CropString())) // only for the same crop
   AND (GetSimulation_YearSeason() > 1) // mobilization not possible in season 1
   AND (GetSimulation_YearSeason() = (GetSimulation_Storage_Season() + 1))) // season next to season in which storage took place
   THEN BEGIN
        // mobilization of assimilates
        SetTransfer_ToMobilize(GetSimulation_Storage_Btotal() * GetCrop_Assimilates().Mobilized/100);
        IF (ROUND(1000 * GetTransfer_ToMobilize()) > 0)  // minimum 1 kg
           THEN SetTransfer_Mobilize(true)
           ELSE SetTransfer_Mobilize(false);
        END
   ELSE BEGIN
        SetSimulation_Storage_CropString(GetCropFileFull());
        // no mobilization of assimilates
        SetTransfer_ToMobilize(0);
        SetTransfer_Mobilize(false);
        END;
// Storage is off and zero at start of season
SetSimulation_Storage_Season(GetSimulation_YearSeason());
SetSimulation_Storage_Btotal(0);
SetTransfer_Store(false);
// Nothing yet mobilized at start of season
SetTransfer_Bmobilized(0);



// 16. Initial rooting depth
// 16.1 default value
IF (GetDayNri() <= GetCrop().Day1)
   THEN Ziprev := undef_int
   ELSE BEGIN
        IF (GetDayNri() > GetCrop().DayN)
           THEN Ziprev := undef_int
           ELSE BEGIN
                ZiPrev := ActualRootingDepth(GetDayNri()-GetCrop().Day1,
                                             GetCrop().DaysToGermination,
                                             GetCrop().DaysToMaxRooting,
                                             GetCrop().DaysToHarvest,
                                             GetCrop().GDDaysToGermination,
                                             GetCrop().GDDaysToMaxRooting,
                                             SumGDDPrev,
                                             GetCrop().RootMin,
                                             GetCrop().RootMax,
                                             GetCrop().RootShape,
                                             GetCrop_ModeCycle());
                END;
        END;
// 16.2 specified or default Zrini (m)
IF ((GetSimulation_Zrini() > 0) AND (Ziprev > 0) AND (GetSimulation_Zrini() <= Ziprev))
   THEN BEGIN
        IF ((GetSimulation_Zrini() >= GetCrop().RootMin) AND (GetSimulation_Zrini() <= GetCrop().RootMax))
           THEN Ziprev := GetSimulation_Zrini()
           ELSE BEGIN
                IF (GetSimulation_Zrini() < GetCrop().RootMin)
                   THEN Ziprev := GetCrop().RootMin
                   ELSE Ziprev := GetCrop().RootMax;
                END;
        IF ((ROUND(GetSoil().RootMax*1000) < ROUND(GetCrop().RootMax*1000))
           AND (Ziprev > GetSoil().RootMax))
           THEN Ziprev := GetSoil().RootMax;
        SetRootingDepth(Ziprev);  // NOT NEEDED since RootingDepth is calculated in the RUN by ocnsidering ZiPrev
        END
   ELSE SetRootingDepth(ActualRootingDepth(GetDayNri()-GetCrop().Day1+1,
                                           GetCrop().DaysToGermination,
                                           GetCrop().DaysToMaxRooting,
                                           GetCrop().DaysToHarvest,
                                           GetCrop().GDDaysToGermination,
                                           GetCrop().GDDaysToMaxRooting,
                                           SumGDDPrev,
                                           GetCrop().RootMin,
                                           GetCrop().RootMax,
                                           GetCrop().RootShape,
                                           GetCrop_ModeCycle()));

// 17. Multiple cuttings
SetNrCut(0);
SetSumInterval(0);
SumGDDcuts := 0;
BprevSum := 0;
YprevSum := 0;
SetCutInfoRecord1_IntervalInfo(0);
SetCutInfoRecord2_IntervalInfo(0);
SetCutInfoRecord1_MassInfo(0);
SetCutInfoRecord2_MassInfo(0);
SetDayLastCut(0);
SetCGCref( GetCrop().CGC);
SetGDDCGCref(GetCrop().GDDCGC);
IF GetManagement_Cuttings_Considered() THEN OpenHarvestInfo();
CGCadjustmentAfterCutting := false;


// 18. Tab sheets

// 19. Labels, Plots and displays
IF (GetManagement_BundHeight() < 0.01) THEN
   BEGIN
   SetSurfaceStorage(0);
   SetECStorage(0);
   END;
IF (GetRootingDepth() > 0) THEN // salinity in root zone
   BEGIN
   ECe_temp := GetRootZoneSalt().ECe;
   ECsw_temp := GetRootZoneSalt().ECsw;
   ECswFC_temp := GetRootZoneSalt().ECswFC;
   KsSalt_temp := GetRootZoneSalt().KsSalt;
   DetermineRootZoneSaltContent(GetRootingDepth(),ECe_temp,ECsw_temp,ECswFC_temp,KsSalt_temp);
   SetRootZoneSalt_ECe(ECe_temp);
   SetRootZoneSalt_ECsw(ECsw_temp);
   SetRootZoneSalt_ECswFC(ECswFC_temp);
   SetRootZoneSalt_KsSalt(KsSalt_temp);
   SetStressTot_Salt(((GetStressTot_NrD() - 1)*GetStressTot_Salt() + 100*(1-GetRootZoneSalt().KsSalt))/GetStressTot_NrD());
   END;
// Harvest Index
SetSimulation_HIfinal(GetCrop().HI);
HItimesBEF := undef_int;
HItimesAT1 := 1;
HItimesAT2 := 1;
HItimesAT := 1;
alfaHI := undef_int;
alfaHIAdj := 0;
IF (GetSimulation_FromDayNr() <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering))
   THEN BEGIN // not yet flowering
        ScorAT1 := 0;
        ScorAT2 := 0;
        END
   ELSE BEGIN
        // water stress affecting leaf expansion
        // NOTE: time to reach end determinancy  is tHImax (i.e. flowering/2 or senescence)
        IF GetCrop().DeterminancyLinked
           THEN tHImax := ROUND(GetCrop().LengthFlowering/2)
           ELSE tHImax := (GetCrop().DaysToSenescence - GetCrop().DaysToFlowering);
        IF ((GetSimulation_FromDayNr() <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + tHImax)) // not yet end period
             AND (tHImax > 0))
           THEN BEGIN // not yet end determinancy
                ScorAT1 := 1/tHImax;
                ScorAT1 := ScorAT1 * (GetSimulation_FromDayNr() - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering));
                IF (ScorAT1 > 1) THEN ScorAT1 := 1;
                END
           ELSE ScorAT1 := 1;  // after period of effect
        // water stress affecting stomatal closure
        // period of effect is yield formation
        IF (GetCrop().dHIdt > 99)
           THEN tHImax := 0
           ELSE tHImax := ROUND(GetCrop().HI/GetCrop().dHIdt);
        IF ((GetSimulation_FromDayNr() <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + tHImax)) // not yet end period
              AND (tHImax > 0))
           THEN BEGIN // not yet end yield formation
                ScorAT2 := 1/tHImax;
                ScorAT2 := ScorAT2 * (GetSimulation_FromDayNr() - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering));
                IF (ScorAT2 > 1) THEN ScorAT2 := 1;
                END
           ELSE ScorAT2 := 1;  // after period of effect
        END;

IF OutDaily THEN DetermineGrowthStage(GetDayNri(),GetCCiPrev(),StageCode);

// 20. Settings for start
StartMode := true;
SetStressLeaf(undef_int);
SetStressSenescence(undef_int);

END; (* InitializeSimulationRun *)




// WRITING RESULTS section ================================================= START ====================


PROCEDURE WriteTheResults(ANumber : ShortInt;
                         Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
                         RPer,EToPer,GDDPer,
                         IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer,
                         SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer : double;
                         TheProjectFile : string);
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
        CASE GetOutputAggregate() OF
             1 : WriteStr(TempString,'      Day',Day1:9,Month1:9,Year1:9);
             2 : WriteStr(TempString,'    10Day',Day1:9,Month1:9,Year1:9);
             3 : WriteStr(TempString,'    Month',Day1:9,Month1:9,Year1:9);
             end;
        fRun_write(TempString, False);
        END
   ELSE BEGIN
        WriteStr(TempString, ANumber:9);
        TempString := CONCAT('Tot(', Trim(TempString), ')');
        WHILE (Length(TempString) < 9) DO TempString := CONCAT(' ',TempString);
        fRun_write(TempString, False);
        WriteStr(TempString, Day1:9,Month1:9,Year1:9);
        fRun_write(TempString, False);
        END;

// Climatic conditions
WriteStr(TempString, Rper:9:1,EToPer:9:1,GDDPer:9:1,GetCO2i():9:2);
fRun_write(TempString, False);
// Soil water parameters
IF (ExPer > 0)
   THEN RatioE := ROUND(100*EPer/ExPer)
   ELSE RatioE := undef_int;
IF (TrxPer > 0)
   THEN RatioT := ROUND(100*TrPer/TrxPer)
   ELSE RatioT := undef_int;

WriteStr(TempString, IrriPer:9:1,InfiltPer:9:1,ROPer:9:1,DrainPer:9:1,CRwPer:9:1,
         EPer:9:1,RatioE:9,TrPer:9:1,TrWPer:9:1,RatioT:9);
fRun_write(TempString, False);
// Soil Salinity
WriteStr(TempString, SalInPer:10:3,SalOutPer:10:3,SalCRPer:10:3,GetTotalSaltContent().EndDay:10:3);
fRun_write(TempString, False);
// seasonal stress
WriteStr(TempString, GetStressTot_NrD():9,GetStressTot_Salt():9:0,GetManagement_FertilityStress():9,GetStressTot_Weed():9:0,
         GetStressTot_Temp():9:0,GetStressTot_Exp():9:0,GetStressTot_Sto():9:0);
fRun_write(TempString, False);

// Biomass production
IF ((BiomassPer > 0) AND (BUnlimPer > 0))
   THEN BEGIN
        BrSF := ROUND(100*BiomassPer/BUnlimPer);
        IF (BrSF > 100) THEN BrSF := 100;
        END
   ELSE BrSF := undef_int;

WriteStr(TempString, BiomassPer:10:3, BrSF:9);
fRun_write(TempString, False);

// Crop yield
IF (ANumber <> undef_int) // end of simulation run
   THEN BEGIN
        // Water Use Efficiency yield
        IF (((GetSumWaBal_Tact() > 0) OR (GetSumWaBal_ECropCycle() > 0)) AND (GetSumWaBal_YieldPart() > 0))
           THEN WPy := (GetSumWaBal_YieldPart()*1000)/((GetSumWaBal_Tact()+GetSumWaBal_ECropCycle())*10)
           ELSE WPy := 0.0;
        // Harvest Index
        IF ((GetSumWaBal_Biomass() > 0) AND (GetSumWaBal_YieldPart() > 0))
           THEN HI := 100*(GetSumWaBal_YieldPart())/(GetSumWaBal_Biomass())
           ELSE HI := undef_double;
        // Fresh yield
        IF ((GetCrop().DryMatter = undef_int) OR (GetCrop().DryMatter = 0))
           THEN WriteStr(TempString, HI:9:1,GetSumWaBal_YieldPart():9:3,undef_double:9:3,WPy:9:2)
           ELSE WriteStr(TempString, HI:9:1,GetSumWaBal_YieldPart():9:3,(GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):9:3,WPy:9:2);
        fRun_write(TempString, False);
        // Transfer of assimilates
        WriteStr(TempString, GetTransfer_Bmobilized():9:3,GetSimulation_Storage_Btotal():9:3);
        fRun_write(TempString, False);
        END
   ELSE BEGIN
        WriteStr(TempString, undef_int:9,undef_int:9,undef_int:9,undef_int:9,BmobPer:9:3,BstoPer:9:3);
        fRun_write(TempString, False);
        END;
// End
WriteStr(TempString, DayN:9,MonthN:9,YearN:9);
fRun_write(TempString, False);

// Project
fRun_write('  ' + TheProjectFile, True);
END; (* WriteTheResults *)


PROCEDURE WriteIntermediatePeriod(TheProjectFile : string);
VAR Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
    RPer,EToPer,GDDPer,IrriPer,InfiltPer,EPer,ExPer,TrPer,TrWPer,TrxPer,DrainPer,BiomassPer,BUnlimPer : double;
    ROPer,CRwPer,SalInPer,SalOutPer,SalCRPer,BmobPer,BstoPer : double;

BEGIN
// determine intermediate results
DetermineDate((PreviousDayNr+1),Day1,Month1,Year1);
DetermineDate(GetDayNri(),DayN,MonthN,YearN);
RPer := GetSumWaBal_Rain() - GetPreviousSum_Rain();
EToPer := SumETo - PreviousSumETo;
GDDPer := SumGDD - PreviousSumGDD;
IrriPer := GetSumWaBal_Irrigation() - GetPreviousSum_Irrigation();
InfiltPer := GetSumWaBal_Infiltrated() - GetPreviousSum_Infiltrated();
EPer := GetSumWaBal_Eact() - GetPreviousSum_Eact();
ExPer := GetSumWaBal_Epot() - GetPreviousSum_Epot();
TrPer := GetSumWaBal_Tact() - GetPreviousSum_Tact();
TrWPer := GetSumWaBal_TrW() - GetPreviousSum_TrW();
TrxPer := GetSumWaBal_Tpot() - GetPreviousSum_Tpot();
DrainPer := GetSumWaBal_Drain() - GetPreviousSum_Drain();
BiomassPer := GetSumWaBal_Biomass() - GetPreviousSum_Biomass();
BUnlimPer := GetSumWaBal_BiomassUnlim() - GetPreviousSum_BiomassUnlim();

ROPer := GetSumWaBal_Runoff() - GetPreviousSum_Runoff();
CRwPer := GetSumWaBal_CRwater() - GetPreviousSum_CRwater();
SalInPer := GetSumWaBal_SaltIn() - GetPreviousSum_SaltIn();
SalOutPer := GetSumWaBal_SaltOut() - GetPreviousSum_SaltOut();
SalCRPer := GetSumWaBal_CRsalt() - GetPreviousSum_CRsalt();

BmobPer := GetTransfer_Bmobilized() - PreviousBmob;
BstoPer := GetSimulation_Storage_Btotal() - PreviousBsto;

// write
WriteTheResults((undef_int),Day1,Month1,Year1,DayN,MonthN,YearN,
                RPer,EToPer,GDDPer,
                IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                EPer,ExPer,TrPer,TrWPer,TrxPer,
                SalInPer,SalOutPer,SalCRPer,
                BiomassPer,BUnlimPer,BmobPer,BstoPer,
                TheProjectFile);

// reset previous sums
PreviousDayNr := GetDayNri();
SetPreviousSum_Rain(GetSumWaBal_Rain());
PreviousSumETo := SumETo;
PreviousSumGDD := SumGDD;
SetPreviousSum_Irrigation(GetSumWaBal_Irrigation());
SetPreviousSum_Infiltrated(GetSumWaBal_Infiltrated());
SetPreviousSum_Eact(GetSumWaBal_Eact());
SetPreviousSum_Epot(GetSumWaBal_Epot());
SetPreviousSum_Tact(GetSumWaBal_Tact());
SetPreviousSum_TrW(GetSumWaBal_TrW());
SetPreviousSum_Tpot(GetSumWaBal_Tpot());
SetPreviousSum_Drain(GetSumWaBal_Drain());
SetPreviousSum_Biomass(GetSumWaBal_Biomass());
SetPreviousSum_BiomassPot(GetSumWaBal_BiomassPot());
SetPreviousSum_BiomassUnlim(GetSumWaBal_BiomassUnlim());

SetPreviousSum_Runoff(GetSumWaBal_Runoff());
SetPreviousSum_CRwater(GetSumWaBal_CRwater());
SetPreviousSum_SaltIn(GetSumWaBal_SaltIn());
SetPreviousSum_SaltOut(GetSumWaBal_SaltOut());
SetPreviousSum_CRsalt(GetSumWaBal_CRsalt());

PreviousBmob := GetTransfer_Bmobilized();
PreviousBsto := GetSimulation_Storage_Btotal();
END; (* WriteIntermediatePeriod *)


PROCEDURE WriteSimPeriod(NrRun : ShortInt;
                         TheProjectFile : string);
VAR Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
BEGIN
DetermineDate(GetSimulation_FromDayNr(),Day1,Month1,Year1); // Start simulation run
DetermineDate(GetSimulation_ToDayNr(),DayN,MonthN,YearN); // End simulation run
WriteTheResults(NrRun,Day1,Month1,Year1,DayN,MonthN,YearN,
               GetSumWaBal_Rain(),SumETo,SumGDD,
               GetSumWaBal_Irrigation(),GetSumWaBal_Infiltrated(),GetSumWaBal_Runoff(),GetSumWaBal_Drain(),GetSumWaBal_CRwater(),
               GetSumWaBal_Eact(),GetSumWaBal_Epot(),GetSumWaBal_Tact(),GetSumWaBal_TrW(),GetSumWaBal_Tpot(),
               GetSumWaBal_SaltIn(),GetSumWaBal_SaltOut(),GetSumWaBal_CRsalt(),
               GetSumWaBal_Biomass(),GetSumWaBal_BiomassUnlim(),GetTransfer_Bmobilized(),GetSimulation_Storage_Btotal(),
               TheProjectFile);
END; (* WriteSimPeriod *)



PROCEDURE CheckForPrint(TheProjectFile : string);
VAR DayN,MonthN,YearN,DayEndM : INTEGER;
    SaltIn,SaltOut,CRsalt,BiomassDay,BUnlimDay : double;
    WriteNow : BOOLEAN;

BEGIN
DetermineDate(GetDayNri(),DayN,MonthN,YearN);
CASE GetOutputAggregate() OF
  1 :   BEGIN // daily output
        BiomassDay := GetSumWaBal_Biomass() - GetPreviousSum_Biomass();
        BUnlimDay := GetSumWaBal_BiomassUnlim() - GetPreviousSum_BiomassUnlim();
        SaltIn := GetSumWaBal_SaltIn() - GetPreviousSum_SaltIn();
        SaltOut := GetSumWaBal_SaltOut() - GetPreviousSum_SaltOut();
        CRsalt := GetSumWaBal_CRsalt() - GetPreviousSum_CRsalt();
        WriteTheResults((undef_int),DayN,MonthN,YearN,DayN,MonthN,YearN,
                       GetRain(),GetETo(),GetGDDayi(),
                       GetIrrigation(),GetInfiltrated(),GetRunoff(),GetDrain(),GetCRwater(),
                       GetEact(),GetEpot(),GetTact(),GetTactWeedInfested(),GetTpot(),
                       SaltIn,SaltOut,CRsalt,
                       BiomassDay,BUnlimDay,GetBin(),GetBout(),
                       TheProjectFile);
        SetPreviousSum_Biomass(GetSumWaBal_Biomass());
        SetPreviousSum_BiomassUnlim(GetSumWaBal_BiomassUnlim());
        SetPreviousSum_SaltIn(GetSumWaBal_SaltIn());
        SetPreviousSum_SaltOut(GetSumWaBal_SaltOut());
        SetPreviousSum_CRsalt(GetSumWaBal_CRsalt());
        END;
  2,3 : BEGIN  // 10-day or monthly output
        WriteNow := false;
        DayEndM := DaysInMonth[MonthN];
        IF (LeapYear(YearN) AND (MonthN = 2)) THEN DayEndM := 29;
        IF (DayN = DayEndM) THEN WriteNow := true;  // 10-day and month
        IF ((GetOutputAggregate() = 2) AND ((DayN = 10) OR (DayN = 20))) THEN WriteNow := true; // 10-day
        IF WriteNow THEN WriteIntermediatePeriod(TheProjectFile);
        END;
    end;
END; (* CheckForPrint *)


PROCEDURE WriteDailyResults(DAP : INTEGER;
                            StageCode : ShortInt;
                            WPi : double);
CONST NoValD = undef_double;
      NoValI = undef_int;
VAR Di,Mi,Yi,StrExp,StrSto,StrSalt,StrTr,StrW,Brel,Nr : INTEGER;
    Ratio1,Ratio2,Ratio3,KsTr,HI,KcVal,WPy,SaltVal : double;
    SWCtopSoilConsidered_temp : boolean;
    tempstring : string;
BEGIN
DetermineDate(GetDayNri(),Di,Mi,Yi);
IF (GetClimRecord_FromY() = 1901) THEN Yi := Yi - 1901 + 1;
IF (StageCode = 0) THEN DAP := undef_int; // before or after cropping

// 0. info day
writeStr(tempstring,Di:6,Mi:6,Yi:6,DAP:6,StageCode:6);
fDaily_write(tempstring, false);

// 1. Water balance
IF Out1Wabal THEN
   BEGIN
   IF (GetZiAqua() = undef_int) THEN
      BEGIN
      WriteStr(tempstring, GetTotalWaterContent().EndDay:10:1,GetRain():8:1,GetIrrigation():9:1,
               GetSurfaceStorage():7:1,GetInfiltrated():7:1,GetRunoff():7:1,GetDrain():9:1,GetCRwater():9:1,undef_double:8:2);
      fDaily_write(tempstring, false);
      END
      ELSE  BEGIN 
      WriteStr(tempstring, GetTotalWaterContent().EndDay:10:1,GetRain():8:1,GetIrrigation():9:1,
               GetSurfaceStorage():7:1,GetInfiltrated():7:1,GetRunoff():7:1,GetDrain():9:1,GetCRwater():9:1,(GetZiAqua()/100):8:2);
      fDaily_write(tempstring, false);
      END;
   IF (GetTpot() > 0) THEN Ratio1 := 100*GetTact()/GetTpot()
                 ELSE Ratio1 := 100.0;
   IF ((GetEpot()+GetTpot()) > 0) THEN Ratio2 := 100*(GetEact()+GetTact())/(GetEpot()+GetTpot())
                        ELSE Ratio2 := 100.0;
   IF (GetEpot() > 0) THEN Ratio3 := 100*GetEact()/GetEpot()
                 ELSE Ratio3 := 100;
   IF ((Out2Crop = true) OR (Out3Prof = true) OR (Out4Salt = true)
      OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, GetEpot():9:1,GetEact():9:1,Ratio3:7:0,GetTpot():9:1,GetTact():9:1,Ratio1:6:0,(GetEpot()+GetTpot()):9:1,(GetEact()+GetTact()):8:1,Ratio2:8:0);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, GetEpot():9:1,GetEact():9:1,Ratio3:7:0,GetTpot():9:1,GetTact():9:1,Ratio1:6:0,(GetEpot()+GetTpot()):9:1,(GetEact()+GetTact()):8:1,Ratio2:8:0);
      fDaily_write(tempstring);
      END;
   END;

// 2. Crop development and yield
IF Out2Crop THEN
   BEGIN
   //1. relative transpiration
   IF (GetTpot() > 0) THEN Ratio1 := 100*GetTact()/GetTpot()
                 ELSE Ratio1 := 100.0;
   //2. Water stresses
   IF (GetStressLeaf() < 0)
      THEN StrExp := undef_int
      ELSE StrExp := ROUND(GetStressLeaf());
   IF (GetTpot() <= 0)
      THEN StrSto := undef_int
      ELSE StrSto := ROUND(100 *(1 - GetTact()/GetTpot()));
   //3. Salinity stress
   IF (GetRootZoneSalt().KsSalt < 0)
      THEN StrSalt := undef_int
      ELSE StrSalt := ROUND(100 * (1 - GetRootZoneSalt().KsSalt));
   //4. Air temperature stress
   IF (GetCCiActual() <= 0.0000001)
      THEN KsTr := 1
      ELSE KsTr := KsTemperature((0),GetCrop().GDtranspLow,GetGDDayi());
   IF (KsTr < 1)
      THEN StrTr := ROUND((1-KsTr)*100)
      ELSE StrTr := 0;
   //5. Relative cover of weeds
   IF (GetCCiActual() <= 0.0000001)
      THEN StrW := undef_int
      ELSE StrW := Round(WeedRCi);
   //6. WPi adjustemnt
   IF (GetSumWaBal_Biomass() <= 0.000001) THEN WPi := 0;
   //7. Harvest Index
   IF ((GetSumWaBal_Biomass() > 0) AND (GetSumWaBal_YieldPart() > 0))
      THEN HI := 100*(GetSumWaBal_YieldPart())/(GetSumWaBal_Biomass())
      ELSE HI := undef_double;
   //8. Relative Biomass
   IF ((GetSumWaBal_Biomass() > 0) AND (GetSumWaBal_BiomassUnlim() > 0))
      THEN BEGIN
           Brel := ROUND(100*GetSumWaBal_Biomass()/GetSumWaBal_BiomassUnlim());
           IF (Brel > 100) THEN Brel := 100;
           END
      ELSE Brel := undef_int;
   //9. Kc coefficient
   IF ((GetETo() > 0) AND (GetTpot() > 0) AND (StrTr < 100))
      THEN KcVal := GetTpot()/(GetETo()*KsTr)
      ELSE KcVal := undef_int;
   //10. Water Use Efficiency yield
   IF (((GetSumWaBal_Tact() > 0) OR (GetSumWaBal_ECropCycle() > 0)) AND (GetSumWaBal_YieldPart() > 0))
      THEN WPy := (GetSumWaBal_YieldPart()*1000)/((GetSumWaBal_Tact()+GetSumWaBal_ECropCycle())*10)
      ELSE WPy := 0.0;
   // write
   WriteStr(tempstring, GetGDDayi():9:1,GetRootingDepth():8:2,StrExp:7,StrSto:7,GetStressSenescence():7:0,StrSalt:7,StrW:7,
         (GetCCiActual()*100):8:1,(CCiActualWeedInfested*100):8:1,StrTr:7,KcVal:9:2,GetTpot():9:1,GetTact():9:1,
         GetTactWeedInfested():9:1,Ratio1:6:0,(100*WPi):8:1,GetSumWaBal_Biomass():10:3,HI:8:1,GetSumWaBal_YieldPart():9:3);
   fDaily_write(tempstring, false);
   // Fresh yield
   IF ((GetCrop().DryMatter = undef_int) OR (GetCrop().DryMatter = 0)) THEN
      BEGIN
      WriteStr(tempstring, undef_double:9:3);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, (GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):9:3);
      fDaily_write(tempstring, false);
      END;
   // finalize
   IF ((Out3Prof = true) OR (Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, Brel:8,WPy:12:2,GetBin():9:3,GetBout():9:3);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, Brel:8,WPy:12:2,GetBin():9:3,GetBout():9:3);
      fDaily_write(tempstring);
      END;
   END;

// 3. Profile/Root zone - Soil water content
IF Out3Prof THEN
   BEGIN
   WriteStr(tempstring, GetTotalWaterContent().EndDay:10:1);
   fDaily_write(tempstring, false);
   IF (GetRootingDepth() <= 0)
      THEN SetRootZoneWC_Actual(undef_double)
      ELSE BEGIN
           IF (ROUND(GetSoil().RootMax*1000) = ROUND(GetCrop().RootMax*1000))
              THEN BEGIN
                   SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
                   DetermineRootZoneWC(GetCrop().RootMax,SWCtopSoilConsidered_temp);
                   SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
                   END
              ELSE BEGIN
                   SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
                   DetermineRootZoneWC(GetCrop().RootMax,SWCtopSoilConsidered_temp);
                   SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
                   END;
           END;
   WriteStr(tempstring, GetRootZoneWC().actual:9:1,GetRootingDepth():8:2);
   fDaily_write(tempstring, false);
   IF (GetRootingDepth() <= 0)
      THEN BEGIN
           SetRootZoneWC_Actual(undef_double);
           SetRootZoneWC_FC(undef_double);
           SetRootZoneWC_WP(undef_double);
           SetRootZoneWC_SAT(undef_double);
           SetRootZoneWC_Thresh(undef_double);
           SetRootZoneWC_Leaf(undef_double);
           SetRootZoneWC_Sen(undef_double);
           END
      ELSE BEGIN
           SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
           DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
           SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
           END; 
   WriteStr(tempstring, GetRootZoneWC().actual:8:1,GetRootZoneWC().SAT:10:1,GetRootZoneWC().FC:10:1,GetRootZoneWC().Leaf:10:1,
      GetRootZoneWC().Thresh:10:1,GetRootZoneWC().Sen:10:1);
   fDaily_write(tempstring, false);
   IF ((Out4Salt = true) OR (Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true)) THEN
      BEGIN
      WriteStr(tempstring, GetRootZoneWC().WP:10:1);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, GetRootZoneWC().WP:10:1);
      fDaily_write(tempstring);
      END;
   END;
   
// 4. Profile/Root zone - soil salinity
IF Out4Salt THEN
   BEGIN
   WriteStr(tempstring, GetSaltInfiltr():9:3,(GetDrain()*GetECdrain()*Equiv/100):10:3,(GetCRsalt()/100):10:3,GetTotalSaltContent().EndDay:10:3);
   fDaily_write(tempstring, false);
   IF (GetRootingDepth() <= 0)
      THEN BEGIN
           SaltVal := undef_int;
           SetRootZoneSalt_ECe(undef_int);
           SetRootZoneSalt_ECsw(undef_int);
           SetRootZoneSalt_KsSalt(1);
           END
      ELSE SaltVal := (GetRootZoneWC().SAT*GetRootZoneSalt().ECe*Equiv)/100;
   IF (GetZiAqua() = undef_int)
      THEN BEGIN
      WriteStr(tempstring, SaltVal:10:3,GetRootingDepth():8:2,GetRootZoneSalt().ECe:9:2,GetRootZoneSalt().ECsw:8:2,
                 (100*(1-GetRootZoneSalt().KsSalt)):7:0,undef_double:8:2);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, SaltVal:10:3,GetRootingDepth():8:2,GetRootZoneSalt().ECe:9:2,GetRootZoneSalt().ECsw:8:2,
                 (100*(1-GetRootZoneSalt().KsSalt)):7:0,(GetZiAqua()/100):8:2);
      fDaily_write(tempstring, false);
      END;
   IF ((Out5CompWC = true) OR (Out6CompEC = true) OR (Out7Clim = true))
      THEN BEGIN
      WriteStr(tempstring, GetECiAqua():8:2);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring,GetECiAqua():8:2);
      fDaily_write(tempstring);
      END;
   END;

// 5. Compartments - Soil water content
IF Out5CompWC THEN
   BEGIN
   WriteStr(tempstring, (GetCompartment_Theta(1)*100):11:1);
   fDaily_write(tempstring, false);
   FOR Nr := 2 TO (GetNrCompartments()-1) DO 
    BEGIN WriteStr(tempstring, (GetCompartment_Theta(Nr)*100):11:1);
          fDaily_write(tempstring, false);
    END;
   IF ((Out6CompEC = true) OR (Out7Clim = true))
      THEN BEGIN
      WriteStr(tempstring, (GetCompartment_Theta(GetNrCompartments())*100):11:1);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring, (GetCompartment_Theta(GetNrCompartments())*100):11:1);
      fDaily_write(tempstring);
      END;
   END;

// 6. Compartmens - Electrical conductivity of the saturated soil-paste extract
IF Out6CompEC THEN
   BEGIN
   SaltVal := ECeComp(GetCompartment_i(1));
   WriteStr(tempstring, SaltVal:11:1);
   fDaily_write(tempstring, false);
   FOR Nr := 2 TO (GetNrCompartments()-1) DO
       BEGIN
       SaltVal := ECeComp(GetCompartment_i(Nr));
       WriteStr(tempstring,SaltVal:11:1);
       fDaily_write(tempstring, false);
       END;
   SaltVal := ECeComp(GetCompartment_i(GetNrCompartments()));
   IF (Out7Clim = true)
      THEN BEGIN
      WriteStr(tempstring, SaltVal:11:1);
      fDaily_write(tempstring, false);
      END
      ELSE BEGIN
      WriteStr(tempstring,SaltVal:11:1);
      fDaily_write(tempstring);
      END;
   END;

// 7. Climate input parameters
IF Out7Clim THEN
   BEGIN
   Ratio1 := (GetTmin() + GetTmax())/2;
   WriteStr(tempstring,GetRain():9:1,GetETo():10:1,GetTmin():10:1,Ratio1:10:1,GetTmax():10:1,GetCO2i():10:2);
   fDaily_write(tempstring);
   END;
END; (* WriteDailyResults *)



PROCEDURE WriteEvaluationData(DAP : INTEGER;
                              StageCode : ShortInt);
                              
VAR SWCi,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd : double;
    Nr,Di,Mi,Yi : INTEGER;
    TempString : string;

    FUNCTION SWCZsoil(Zsoil : double) : double;
    VAR compi : INTEGER;
        CumDepth,Factor,frac_value,SWCact : double;
    BEGIN
    CumDepth := 0;
    compi := 0;
    SWCact := 0;
    REPEAT
      compi := compi + 1;
      CumDepth := CumDepth + GetCompartment_Thickness(compi);
      IF (CumDepth <= Zsoil)
         THEN Factor := 1
         ELSE BEGIN
              frac_value := Zsoil - (CumDepth - GetCompartment_Thickness(compi));
              IF (frac_value > 0)
                 THEN Factor := frac_value/GetCompartment_Thickness(compi)
                 ELSE Factor := 0;
              END;
      SWCact := SWCact + Factor * 10 * (GetCompartment_Theta(compi)*100) * GetCompartment_Thickness(compi);

    UNTIL ((ROUND(100*CumDepth) >= ROUND(100*ZSoil)) OR (compi = GetNrCompartments()));
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
IF ((LineNrEval <> undef_int) AND (DayNrEval = GetDayNri())) THEN
   BEGIN
   // read field data
   fObs_rewind();
   FOR Nr := 1 TO (LineNrEval -1) DO fObs_read();
   TempString := fObs_read();
   ReadStr(TempString,Nr,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd);
   // get Day Nr for next field data
   fObs_read();
   IF (fObs_eof())
      THEN BEGIN
           LineNrEval := undef_int;
           fObs_close();
           END
      ELSE BEGIN
           LineNrEval := LineNrEval + 1;
           ReadStr(TempString,DayNrEval);
           DayNrEval := DayNr1Eval + DayNrEval -1;
           END;
   END;
//2. Date
DetermineDate(GetDayNri(),Di,Mi,Yi);
IF (GetClimRecord_FromY() = 1901) THEN Yi := Yi - 1901 + 1;
IF (StageCode = 0) THEN DAP := undef_int; // before or after cropping
//3. Write simulation results and field data
SWCi := SWCZsoil(Zeval);
WriteStr(TempString, Di:6,Mi:6,Yi:6,DAP:6,StageCode:5,(GetCCiActual()*100):8:1,CCfield:8:1,CCstd:8:1,
           GetSumWaBal_Biomass:10:3,Bfield:10:3,Bstd:10:3,SWCi:8:1,SWCfield:8:1,SWCstd:8:1);
fEval_write(TempString);
END; (* WriteEvaluationData *)





// WRITING RESULTS section ================================================= END ====================

PROCEDURE AdvanceOneTimeStep();

VAR PotValSF,KsTr,WPi,TESTVALY,PreIrri,StressStomata,FracAssim : double;
    HarvestNow : BOOLEAN;
    VirtualTimeCC,DayInSeason : INTEGER;
    SumGDDadjCC,RatDGDD : double;
    Biomass_temp, BiomassPot_temp, BiomassUnlim_temp, BiomassTot_temp : double;
    YieldPart_temp : double;
    ECe_temp, ECsw_temp, ECswFC_temp, KsSalt_temp : double;
    FromDay_temp, TimeInfo_temp, DepthInfo_temp : integer;
    GwTable_temp : rep_GwTable;
    Store_temp, Mobilize_temp : boolean;
    ToMobilize_temp, Bmobilized_temp, ETo_tmp : double;
    EffectStress_temp : rep_EffectStress;
    SWCtopSOilConsidered_temp : boolean;
    ZiAqua_temp : integer;
    ECiAqua_temp : double;
    tmpRain : double;
    TactWeedInfested_temp : double;
    Tmin_temp, Tmax_temp : double;
    Bin_temp, Bout_temp : double;
    TempString : string;
    TargetTimeVal, TargetDepthVal : Integer;
    PreviousStressLevel_temp, StressSFadjNEW_temp : shortint;
    CCxWitheredTpot_temp, CCxWitheredTpotNoS_temp : double;
    StressLeaf_temp,StressSenescence_temp : double;

    PROCEDURE GetZandECgwt(DayNri : LongInt;
                       VAR ZiAqua : INTEGER;
                       VAR ECiAqua : double);
    VAR ZiIN : INTEGER;
        Comp_temp : rep_comp;
    BEGIN
    ZiIN := ZiAqua;
    IF (GetGwTable_DNr1() = GetGwTable_DNr2())
       THEN BEGIN
            ZiAqua := GetGwTable_Z1();
            ECiAqua := GetGwTable_EC1();
            END
       ELSE BEGIN
            ZiAqua := GetGwTable_Z1() + ROUND((DayNri - GetGwTable_DNr1())*(GetGwTable_Z2() - GetGwTable_Z1())/(GetGwTable_DNr2() - GetGwTable_DNr1()));
            ECiAqua := GetGwTable_EC1() + (DayNri - GetGwTable_DNr1())*(GetGwTable_EC2() - GetGwTable_EC1())/(GetGwTable_DNr2() - GetGwTable_DNr1());
            END;
    IF (ZiAqua <> ZiIN) THEN BEGIN
                             Comp_temp := GetCompartment();
                             CalculateAdjustedFC((ZiAqua/100),Comp_temp);
                             SetCompartment(Comp_temp);
                             END;
    END; (* GetZandECgwt *)


    FUNCTION IrriOutSeason(Dayi : LongInt) : INTEGER;
    VAR DNr, Nri : INTEGER;
        IrriEvents : rep_IrriOutSeasonEvents;
        TheEnd : BOOLEAN;
    BEGIN
    DNr := Dayi - GetSimulation_FromDayNr() + 1;
    IrriEvents := GetIrriBeforeSeason();
    IF (Dayi > GetCrop().DayN) THEN
       BEGIN
       DNr := Dayi - GetCrop().DayN;
       IrriEvents := GetIrriAfterSeason();
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
        IrriECw_temp : double;
    BEGIN
    IF (GetIrriFirstDayNr() = undef_int)
       THEN DNr := Dayi - GetCrop().Day1 + 1
       ELSE DNr := Dayi - GetIrriFirstDayNr() + 1;
    IF (GetIrriInfoRecord1_NoMoreInfo())
       THEN IrriManual := 0
       ELSE BEGIN
            IrriManual := 0;
            IF (GetIrriInfoRecord1_TimeInfo() = DNr) THEN
               BEGIN
               IrriManual := GetIrriInfoRecord1_DepthInfo();
               StringREAD := fIrri_read();
               IF fIrri_eof()
                  THEN SetIrriInfoRecord1_NoMoreInfo(true)
                  ELSE BEGIN
                       SetIrriInfoRecord1_NoMoreInfo(false);
                       IF GetGlobalIrriECw() // Versions before 3.2
                          THEN SplitStringInTwoParams(StringREAD,Ir1,Ir2)
                          ELSE BEGIN
                               IrriECw_temp := GetSimulation_IrriECw();
                               SplitStringInThreeParams(StringREAD,Ir1,Ir2,IrriECw_temp);
                               SetSimulation_IrriECw(IrriECw_temp);
                               END;
                       SetIrriInfoRecord1_TimeInfo(ROUND(Ir1));
                       SetIrriInfoRecord1_DepthInfo(ROUND(Ir2));
                       END;
               END;
            END;
    END; (* IrriManual *)



    PROCEDURE GetIrriParam (VAR TargetTimeVal, TargetDepthVal : integer);
    VAR DayInSeason : Integer;
        IrriECw_temp : double;
        TempString : string;

    BEGIN
    TargetTimeVal := -999;
    TargetDepthVal := -999;
    IF ((GetDayNri() < GetCrop().Day1) OR (GetDayNri() > GetCrop().DayN))
       THEN SetIrrigation(IrriOutSeason(GetDayNri()))
       ELSE IF (GetIrriMode() = Manual) THEN SetIrrigation(IrriManual(GetDayNri()));
    IF ((GetIrriMode() = Generate) AND ((GetDayNri() >= GetCrop().Day1) AND (GetDayNri() <= GetCrop().DayN))) THEN
       BEGIN
       // read next line if required
       DayInSeason := GetDayNri() - GetCrop().Day1 + 1;
       IF (DayInSeason > GetIrriInfoRecord1_ToDay()) THEN // read next line
          BEGIN
          SetIrriInfoRecord1(GetIrriInfoRecord2());

          TempString := fIrri_read();
          IF fIrri_eof()
             THEN SetIrriInfoRecord1_ToDay(GetCrop().DayN - GetCrop().Day1 + 1)
             ELSE BEGIN
                  SetIrriInfoRecord2_NoMoreInfo(false);
                  IF GetGlobalIrriECw() // Versions before 3.2
                     THEN BEGIN
                          ReadStr(TempString,FromDay_temp,TimeInfo_temp,DepthInfo_temp);
                          SetIrriInfoRecord2_FromDay(FromDay_temp);
                          SetIrriInfoRecord2_TimeInfo(TimeInfo_temp);
                          SetIrriInfoRecord2_DepthInfo(DepthInfo_temp);
                          END
                     ELSE BEGIN
                          ReadStr(TempString,FromDay_temp,TimeInfo_temp, DepthInfo_temp,IrriEcw_temp);
                          SetIrriInfoRecord2_FromDay(FromDay_temp);
                          SetIrriInfoRecord2_TimeInfo(TimeInfo_temp);
                          SetIrriInfoRecord2_DepthInfo(DepthInfo_temp);
                          SetSimulation_IrriEcw(IrriEcw_temp);
                          END;
                  SetIrriInfoRecord1_ToDay(GetIrriInfoRecord2_FromDay() - 1);
                  END;
          END;
       // get TargetValues
       TargetDepthVal := GetIrriInfoRecord1_DepthInfo();
       CASE GetGenerateTimeMode() OF
          AllDepl : TargetTimeVal := GetIrriInfoRecord1_TimeInfo();
          AllRAW  : TargetTimeVal := GetIrriInfoRecord1_TimeInfo();
          FixInt  : BEGIN
                    TargetTimeVal := GetIrriInfoRecord1_TimeInfo();
                    IF (TargetTimeVal > GetIrriInterval()) // do not yet irrigate
                       THEN TargetTimeVal := 0
                       ELSE IF (TargetTimeVal = GetIrriInterval()) // irrigate
                               THEN TargetTimeVal := 1
                               ELSE BEGIN  // still to solve
                                    TargetTimeVal := 1; // voorlopige oplossing
                                    END;
                    IF ((TargetTimeVal = 1) AND (GetGenerateDepthMode() = FixDepth)) THEN SetIrrigation(TargetDepthVal);
                    END;
          WaterBetweenBunds : BEGIN
                              TargetTimeVal := GetIrriInfoRecord1_TimeInfo();
                              IF  ((GetManagement_BundHeight() >= 0.01)
                               AND (GetGenerateDepthMode() = FixDepth)
                               AND (TargetTimeVal < (1000 * GetManagement_BundHeight()))
                               AND (TargetTimeVal >= ROUND(GetSurfaceStorage())))
                                   THEN SetIrrigation(TargetDepthVal)
                                   ELSE SetIrrigation(0);
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
      SumDepth := SumDepth + GetCompartment_Thickness(compi);
      layeri := GetCompartment_Layer(compi);
      ThetaPercRaw := GetSoilLayer_i(layeri).FC/100 - GetSimulParam_PercRAW()/100*GetCrop().pdef*(GetSoilLayer_i(layeri).FC/100-GetSoilLayer_i(layeri).WP/100);
      IF (GetCompartment_Theta(compi) < ThetaPercRaw) THEN
         BEGIN
         PreIrri := PreIrri + (ThetaPercRaw - GetCompartment_Theta(compi))*1000*GetCompartment_Thickness(compi);
         SetCompartment_Theta(compi, ThetaPercRaw);
         END;
    UNTIL ((SumDepth >= GetRootingDepth()) OR (compi = GetNrCompartments()))
    END; (* AdjustSWCRootZone *)


    PROCEDURE InitializeTransferAssimilates(NoMoreCrop : BOOLEAN;
                                            VAR Bin,Bout,AssimToMobilize,AssimMobilized,FracAssim : double;
                                            VAR StorageOn,MobilizationOn : BOOLEAN);
    BEGIN
    Bin := 0;
    Bout := 0;
    FracAssim := 0;
    IF (GetCrop_subkind() = Forage) THEN // only for perennial herbaceous forage crops
      BEGIN
      FracAssim := 0;
      IF (NoMoreCrop = true)
         THEN BEGIN
              StorageOn := false;
              MobilizationOn := false;
              END
         ELSE BEGIN
              // Start of storage period ?
              //IF ((GetDayNri() - Simulation.DelayedDays - Crop.Day1) = (Crop.DaysToHarvest - Crop.Assimilates.Period + 1)) THEN
              IF ((GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1 + 1) = (GetCrop().DaysToHarvest - GetCrop_Assimilates().Period + 1)) THEN
                 BEGIN
                 // switch storage on
                 StorageOn := true;
                 // switch mobilization off
                 IF (MobilizationOn = true) THEN AssimToMobilize := AssimMobilized;
                 MobilizationOn := false;
                 END;
              // Fraction of assimilates transferred
              IF (MobilizationOn = true) THEN FracAssim := (AssimToMobilize-AssimMobilized)/AssimToMobilize;
              IF ((StorageOn = true) AND (GetCrop_Assimilates().Period > 0))
                 THEN FracAssim := (GetCrop_Assimilates().Stored/100) *
                 //(((GetDayNri() - Simulation.DelayedDays - Crop.Day1)-(Crop.DaysToHarvest-Crop.Assimilates.Period))/Crop.Assimilates.Period);
                 (((GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1 + 1)-(GetCrop().DaysToHarvest-GetCrop_Assimilates().Period))/GetCrop_Assimilates().Period);
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
    DetermineDate(GetCrop().Day1,Dayi,Monthi,Yeari);
    NoYear := (Yeari = 1901);
    DetermineDate(DayNri,Dayi,Monthi,Yeari);
    IF NoYear THEN Yeari := 9999;
    IF (NrCut = 9999)
       THEN BEGIN
            // last line at end of season
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,GetSumWaBal_Biomass():34:3);
            IF (GetCrop().DryMatter = undef_int)
               THEN WRITELN(fHarvest,GetSumWaBal_YieldPart():20:3)
               ELSE WRITELN(fHarvest,GetSumWaBal_YieldPart():20:3,(GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):20:3);
            END
       ELSE BEGIN
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,DayInSeason:6,SumInterval:6,(GetSumWaBal_Biomass()-BprevSum):12:3,
                  GetSumWaBal_Biomass():10:3,(GetSumWaBal_YieldPart()-YprevSum):10:3);
            IF (GetCrop().DryMatter = undef_int)
               THEN WRITELN(fHarvest,GetSumWaBal_YieldPart():10:3)
               ELSE WRITELN(fHarvest,GetSumWaBal_YieldPart():10:3,((GetSumWaBal_YieldPart()-YprevSum)/(GetCrop().DryMatter/100)):10:3,
                         (GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):10:3);
            END;
    END; (* RecordHarvest *)



    PROCEDURE GetPotValSF(DAP : INTEGER;
                      VAR PotValSF : double);
    VAR RatDGDD : double;
    BEGIN (* GetPotValSF *)
    RatDGDD := 1;
    IF ((GetCrop_ModeCycle() = GDDays) AND (GetCrop().GDDaysToFullCanopySF < GetCrop().GDDaysToSenescence))
       THEN RatDGDD := (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF)/(GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF);
    PotValSF := CCiNoWaterStressSF(DAP,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
        GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopySF,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
        CCoTotal,CCxTotal,GetCrop().CGC,GetCrop().GDDCGC,CDCTotal,GDDCDCTotal,SumGDDadjCC,RatDGDD,
        GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),GetSimulation_EffectStress_CDecline(),GetCrop_ModeCycle());
    PotValSF := 100 * (1/CCxCropWeedsNoSFstress) * PotValSF;
    END; (* GetPotValSF *)

BEGIN (* AdvanceOneTimeStep *)

(* 1. Get ETo *)
IF (GetEToFile() = '(None)') THEN SetETo(5);

(* 2. Get Rain *)
IF (GetRainFile() = '(None)') THEN SetRain(0);

(* 3. Start mode *)
IF StartMode THEN StartMode := false;

(* 4. Get depth and quality of the groundwater*)
IF (NOT GetSimulParam_ConstGwt()) THEN
   BEGIN
   IF (GetDayNri() > GetGwTable_DNr2()) THEN BEGIN
        GwTable_temp := GetGwTable();
        GetGwtSet(GetDayNri(),GwTable_temp);
        SetGwTable(GwTable_temp);
        END;
   ZiAqua_temp := GetZiAqua();
   ECiAqua_temp := GetECiAqua();
   GetZandECgwt(GetDayNri(),ZiAqua_temp,ECiAqua_temp);
   SetZiAqua(ZiAqua_temp);
   SetECiAqua(ECiAqua_temp);
   CheckForWaterTableInProfile((GetZiAqua()/100),GetCompartment(),WaterTableInProfile);
   IF WaterTableInProfile THEN AdjustForWatertable;
   END;

(* 5. Get Irrigation *)
SetIrrigation(0);
GetIrriParam(TargetTimeVal, TargetDepthVal);

(* 6. get virtual time for CC development *)
SumGDDadjCC := undef_int;
IF (GetCrop().DaysToCCini <> 0)
   THEN BEGIN // regrowth
        IF (GetDayNri() >= GetCrop().Day1)
           THEN BEGIN
                // time setting for canopy development
                VirtualTimeCC := (GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1) + GetTadj() + GetCrop().DaysToGermination; // adjusted time scale
                IF (VirtualTimeCC > GetCrop().DaysToHarvest) THEN VirtualTimeCC := GetCrop().DaysToHarvest; // special case where L123 > L1234
                IF (VirtualTimeCC > GetCrop().DaysToFullCanopy) THEN
                   BEGIN
                   IF ((GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1) <= GetCrop().DaysToSenescence)
                      THEN VirtualTimeCC := GetCrop().DaysToFullCanopy + ROUND(GetDayFraction() *
                            ( (GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1)+GetTadj()+GetCrop().DaysToGermination - GetCrop().DaysToFullCanopy)) // slow down
                      ELSE VirtualTimeCC := (GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1); // switch time scale
                   END;
                IF (GetCrop_ModeCycle() = GDDays) THEN
                   BEGIN
                   SumGDDadjCC := GetSimulation_SumGDDfromDay1() + GetGDDTadj() + GetCrop().GDDaysToGermination;
                   IF (SumGDDadjCC > GetCrop().GDDaysToHarvest) THEN SumGDDadjCC := GetCrop().GDDaysToHarvest; // special case where L123 > L1234
                   IF (SumGDDadjCC > GetCrop().GDDaysToFullCanopy) THEN
                      BEGIN
                      IF (GetSimulation_SumGDDfromDay1() <= GetCrop().GDDaysToSenescence)
                         THEN SumGDDadjCC := GetCrop().GDDaysToFullCanopy
                           + ROUND(GetGDDayFraction() * (GetSimulation_SumGDDfromDay1()+GetGDDTadj()+GetCrop().GDDaysToGermination-GetCrop().GDDaysToFullCanopy)) // slow down
                         ELSE SumGDDadjCC := GetSimulation_SumGDDfromDay1() // switch time scale
                      END
                   END;
                // CC initial (at the end of previous day) when simulation starts before regrowth,
                IF ((GetDayNri() = GetCrop().Day1) AND (GetDayNri() > GetSimulation_FromDayNr())) THEN
                   BEGIN
                   RatDGDD := 1;
                   IF ((GetCrop().ModeCycle = GDDays) AND (GetCrop().GDDaysToFullCanopySF < GetCrop().GDDaysToSenescence)) THEN
                      RatDGDD := (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF)/(GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF);
                   EffectStress_temp := GetSimulation_EffectStress();
                   CropStressParametersSoilFertility(GetCrop().StressResponse,GetStressSFadjNEW(),EffectStress_temp);
                   SetSimulation_EffectStress(EffectStress_temp);
                   SetCCiPrev(CCiniTotalFromTimeToCCini(GetCrop().DaysToCCini,GetCrop().GDDaysToCCini,
                                  GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToFullCanopySF,
                                  GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                  GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopy,GetCrop().GDDaysToFullCanopySF,
                                  GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                  GetCrop().CCo,GetCrop().CCx,GetCrop().CGC,GetCrop().GDDCGC,GetCrop().CDC,GetCrop().GDDCDC,RatDGDD,
                                  GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),
                                  GetSimulation_EffectStress_CDecline(),(CCxTotal/GetCrop().CCx),GetCrop().ModeCycle));  // (CCxTotal/Crop.CCx) = fWeed
                   END;
                END
           ELSE BEGIN // before start crop
                VirtualTimeCC := GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1;
                IF (GetCrop().ModeCycle = GDDays) THEN SumGDDadjCC := GetSimulation_SumGDD();
                END;
        END
   ELSE BEGIN // sown or transplanted
        VirtualTimeCC := GetDayNri() - GetSimulation_DelayedDays() - GetCrop().Day1;
        IF (GetCrop().ModeCycle = GDDays) THEN SumGDDadjCC := GetSimulation_SumGDD();
        // CC initial (at the end of previous day) when simulation starts before sowing/transplanting,
        IF ((GetDayNri() = (GetCrop().Day1 + GetCrop().DaysToGermination)) AND (GetDayNri() > GetSimulation_FromDayNr()))
           THEN SetCCiPrev(CCoTotal);
        END;


(* 7. Rooting depth AND Inet day 1*)
IF (((GetCrop().ModeCycle = CalendarDays) AND ((GetDayNri()-GetCrop().Day1+1) < GetCrop().DaysToHarvest))
              OR ((GetCrop().ModeCycle = GDDays) AND (GetSimulation_SumGDD() < GetCrop().GDDaysToHarvest)))
   THEN BEGIN
        IF (((GetDayNri()-GetSimulation_DelayedDays()) >= GetCrop().Day1) AND ((GetDayNri()-GetSimulation_DelayedDays()) <= GetCrop().DayN))
           THEN BEGIN // rooting depth at DAP (at Crop.Day1, DAP = 1)
                SetRootingDepth(AdjustedRootingDepth(GetPlotVarCrop().ActVal,GetPlotVarCrop().PotVal,GetTpot(),GetTact(),GetStressLeaf(),GetStressSenescence(),
                                (GetDayNri()-GetCrop().Day1+1),GetCrop().DaysToGermination,GetCrop().DaysToMaxRooting,GetCrop().DaysToHarvest,
                                GetCrop().GDDaysToGermination,GetCrop().GDDaysToMaxRooting,GetCrop().GDDaysToHarvest,(SumGDDPrev),
                                (GetSimulation_SumGDD()),GetCrop().RootMin,GetCrop().RootMax,Ziprev,GetCrop().RootShape,
                                GetCrop().ModeCycle));
                ZiPrev := GetRootingDepth();  // IN CASE rootzone drops below groundwate table
                IF ((GetZiAqua() >= 0) AND (GetRootingDepth() > (GetZiAqua()/100)) AND (GetCrop().AnaeroPoint > 0)) THEN
                   BEGIN
                   SetRootingDepth(GetZiAqua()/100);
                   IF (GetRootingDepth() < GetCrop().RootMin) THEN SetRootingDepth(GetCrop().RootMin);
                   END;
                END
           ELSE SetRootingDepth(0);
        END
   ELSE SetRootingDepth(Ziprev);
IF ((GetRootingDepth() > 0) AND (GetDayNri() = GetCrop().Day1))
   THEN BEGIN //initial root zone depletion day1 (for WRITE Output)
        SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
        DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
        SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
        IF (GetIrriMode() = Inet) THEN AdjustSWCRootZone(PreIrri);  // required to start germination
        END;

(* 8. Transfer of Assimilates  *)
ToMobilize_temp := GetTransfer_ToMobilize();
Bmobilized_temp := GetTransfer_Bmobilized();
Store_temp := GetTransfer_Store();
Mobilize_temp := GetTransfer_Mobilize();
Bin_temp := GetBin();
Bout_temp := GetBout();
InitializeTransferAssimilates(NoMoreCrop,Bin_temp,Bout_temp,ToMobilize_temp,Bmobilized_temp,FracAssim,
                              Store_temp,Mobilize_temp);
SetTransfer_ToMobilize(ToMobilize_temp);
SetTransfer_Bmobilized(Bmobilized_temp);
SetTransfer_Store(Store_temp);
SetTransfer_Mobilize(Mobilize_temp);
SetBin(Bin_temp);
SetBout(Bout_temp);

(* 9. RUN Soil water balance and actual Canopy Cover *)
StressLeaf_temp := GetStressLeaf();
StressSenescence_temp := GetStressSenescence();
BUDGET_module(GetDayNri(),TargetTimeVal,TargetDepthVal,VirtualTimeCC,GetSumInterval(),GetDayLastCut(),GetStressTot_NrD(),
              GetTadj(),GetGDDTadj(),
              GetGDDayi(),GetCGCref(),GetGDDCGCref(),GetCO2i(),CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
              GetCoeffb0Salt(),GetCoeffb1Salt(),GetCoeffb2Salt(),GetStressTot_Salt(),
              GetDayFraction(),GetGDDayFraction(),FracAssim,
              GetStressSFadjNEW(),GetTransfer_Store(),GetTransfer_Mobilize(),
              StressLeaf_temp,StressSenescence_temp,TimeSenescence,NoMoreCrop,CGCadjustmentAfterCutting,TESTVAL);
SetStressLeaf(StressLeaf_temp);
SetStressSenescence(StressSenescence_temp);

// consider Pre-irrigation (6.) if IrriMode = Inet
IF ((GetRootingDepth() > 0) AND (GetDayNri() = GetCrop().Day1) AND (GetIrriMode() = Inet)) THEN
   BEGIN
   SetIrrigation(GetIrrigation() + PreIrri);
   SetSumWabal_Irrigation(GetSumWaBal_Irrigation() + PreIrri);
   PreIrri := 0;
   END;

// total number of days in the season
IF (GetCCiActual() > 0) THEN
   BEGIN
   IF (GetStressTot_NrD() < 0)
      THEN SetStressTot_NrD(1)
      ELSE SetStressTot_NrD(GetStressTot_NrD() +1);
   END;


(* 10. Potential biomass *)
BiomassUnlim_temp := GetSumWaBal_BiomassUnlim();
CCxWitheredTpotNoS_temp := GetCCxWitheredTpotNoS();
DeterminePotentialBiomass(VirtualTimeCC,SumGDDadjCC,GetCO2i(),GetGDDayi(),CCxWitheredTpotNoS_temp,BiomassUnlim_temp);
SetCCxWitheredTpotNoS(CCxWitheredTpotNoS_temp);
SetSumWaBal_BiomassUnlim(BiomassUnlim_temp);

(* 11. Biomass and yield *)
IF ((GetRootingDepth() > 0) AND (NoMoreCrop = false))
   THEN BEGIN
        SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
        DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
        SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
        // temperature stress affecting crop transpiration
        IF (GetCCiActual() <= 0.0000001)
           THEN KsTr := 1
           ELSE KsTr := KsTemperature((0),GetCrop().GDtranspLow,GetGDDayi());
        SetStressTot_Temp(((GetStressTot_NrD() - 1)*GetStressTot_Temp() + 100*(1-KsTr))/GetStressTot_NrD());
        // soil salinity stress
        ECe_temp := GetRootZoneSalt().ECe;
        ECsw_temp := GetRootZoneSalt().ECsw;
        ECswFC_temp := GetRootZoneSalt().ECswFC;
        KsSalt_temp := GetRootZoneSalt().KsSalt;
        DetermineRootZoneSaltContent(GetRootingDepth(),ECe_temp,ECsw_temp,ECswFC_temp,KsSalt_temp);
        SetRootZoneSalt_ECe(ECe_temp);
        SetRootZoneSalt_ECsw(ECsw_temp);
        SetRootZoneSalt_ECswFC(ECswFC_temp);
        SetRootZoneSalt_KsSalt(KsSalt_temp);
        SetStressTot_Salt(((GetStressTot_NrD() - 1)*GetStressTot_Salt() + 100*(1-GetRootZoneSalt().KsSalt))/GetStressTot_NrD());
        // Biomass and yield
        Store_temp := GetTransfer_Store(); 
        Mobilize_temp := GetTransfer_Mobilize(); 
        ToMobilize_temp := GetTransfer_ToMobilize(); 
        Bmobilized_temp := GetTransfer_Bmobilized(); 
        Biomass_temp := GetSumWaBal_Biomass();
        BiomassPot_temp := GetSumWaBal_BiomassPot();
        BiomassUnlim_temp := GetSumWaBal_BiomassUnlim();
        BiomassTot_temp := GetSumWaBal_BiomassTot();
        YieldPart_temp := GetSumWaBal_YieldPart();
        TactWeedInfested_temp := GetTactWeedInfested();
        PreviousStressLevel_temp := GetPreviousStressLevel();
        StressSFadjNEW_temp := GetStressSFadjNEW();
        CCxWitheredTpot_temp := GetCCxWitheredTpot();
        CCxWitheredTpotNoS_temp := GetCCxWitheredTpotNoS();
        Bin_temp := GetBin();
        Bout_temp := GetBout();
        DetermineBiomassAndYield(GetDayNri(),GetETo(),GetTmin(),GetTmax(),GetCO2i(),GetGDDayi(),GetTact(),SumKcTop,GetCGCref(),GetGDDCGCref(),
                                 GetCoeffb0(),GetCoeffb1(),GetCoeffb2(),GetFracBiomassPotSF(),                             GetCoeffb0Salt(),GetCoeffb1Salt(),GetCoeffb2Salt(),GetStressTot_Salt(),SumGDDadjCC,GetCCiActual(),FracAssim,
                                 VirtualTimeCC,GetSumInterval(),
                                 Biomass_temp,BiomassPot_temp,BiomassUnlim_temp,BiomassTot_temp,
                                 YieldPart_temp,WPi,HItimesBEF,ScorAT1,ScorAT2,HItimesAT1,HItimesAT2,
                                 HItimesAT,alfaHI,alfaHIAdj,SumKcTopStress,SumKci,CCxWitheredTpot_temp,CCxWitheredTpotNoS_temp,
                                 WeedRCi,CCiActualWeedInfested,TactWeedInfested_temp,
                                 StressSFadjNEW_temp,PreviousStressLevel_temp,
                                 Store_temp,Mobilize_temp,
                                 ToMobilize_temp,Bmobilized_temp,Bin_temp,Bout_temp,
                                 TESTVALY);
        SetTransfer_Store(Store_temp);
        SetTransfer_Mobilize(Mobilize_temp);
        SetTransfer_ToMobilize(ToMobilize_temp);
        SetTransfer_Bmobilized(Bmobilized_temp);
        SetSumWaBal_Biomass(Biomass_temp);
        SetSumWaBal_BiomassPot(BiomassPot_temp);
        SetSumWaBal_BiomassUnlim(BiomassUnlim_temp);
        SetSumWaBal_BiomassTot(BiomassTot_temp);
        SetSumWaBal_YieldPart(YieldPart_temp);
        SetTactWeedInfested(TactWeedInfested_temp);
        SetBin(Bin_temp);
        SetBout(Bout_temp);
        SetPreviousStressLevel(PreviousStressLevel_temp);
        SetStressSFadjNEW(StressSFadjNEW_temp);
        SetCCxWitheredTpot(CCxWitheredTpot_temp);
        SetCCxWitheredTpotNoS(CCxWitheredTpotNoS_temp);
        END
   ELSE BEGIN
        SenStage := undef_int;
        WeedRCi := undef_int; // no crop and no weed infestation
        CCiActualWeedInfested := 0.0; // no crop
        SetTactWeedInfested(0.0); // no crop
        END;

(* 12. Reset after RUN *)
IF (GetPreDay() = false) THEN PreviousDayNr := GetSimulation_FromDayNr() - 1;
SetPreDay(true);
IF (GetDayNri() >= GetCrop().Day1) THEN
   BEGIN
   SetCCiPrev(GetCCiActual());
   IF (ZiPrev < GetRootingDepth()) THEN Ziprev := GetRootingDepth(); // IN CASE groundwater table does not affect root development
   SumGDDPrev := GetSimulation_SumGDD();
   END;
IF (TargetTimeVal = 1) THEN SetIrriInterval(0);

(* 13. Cuttings *)
IF GetManagement_Cuttings_Considered() THEN
   BEGIN
   HarvestNow := false;
   DayInSeason := GetDayNri() - GetCrop().Day1 + 1;
   SetSumInterval(GetSumInterval() + 1);
   SumGDDcuts := SumGDDcuts + GetGDDayi();
   CASE GetManagement_Cuttings_Generate() OF
        false : BEGIN
                IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) // adjust DayInSeason
                   THEN DayInSeason := GetDayNri() - GetManagement_Cuttings_FirstDayNr() + 1;
                IF ((DayInSeason >= GetCutInfoRecord1_FromDay()) AND (GetCutInfoRecord1_NoMoreInfo() = false))
                   THEN BEGIN
                        HarvestNow := true;
                        GetNextHarvest;
                        END;
                 IF (GetManagement_Cuttings_FirstDayNr() <> undef_int) // reset DayInSeason
                   THEN DayInSeason := GetDayNri() - GetCrop().Day1 + 1;
                END;
        true  : BEGIN
                IF ((DayInSeason > GetCutInfoRecord1_ToDay()) AND (GetCutInfoRecord1_NoMoreInfo() = false))
                   THEN GetNextHarvest;
                CASE GetManagement_Cuttings_Criterion() OF
                     IntDay : BEGIN
                              IF ((GetSumInterval() >= GetCutInfoRecord1_IntervalInfo())
                                   AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                   AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     IntGDD : BEGIN
                              IF ((SumGDDcuts >= GetCutInfoRecord1_IntervalGDD())
                                   AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                   AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     DryB   : BEGIN
                              IF (((GetSumWabal_Biomass() - BprevSum) >= GetCutInfoRecord1_MassInfo())
                                                 AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                                 AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     DryY   : BEGIN
                              IF (((GetSumWabal_YieldPart() - YprevSum) >= GetCutInfoRecord1_MassInfo())
                                                   AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                                   AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     FreshY : BEGIN
                              // OK if Crop.DryMatter = undef_int (not specified) HarvestNow remains false
                              IF ((((GetSumWaBal_YieldPart() - YprevSum)/(GetCrop().DryMatter/100)) >= GetCutInfoRecord1_MassInfo())
                                                                          AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                                                          AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;

                              END;
                     end;

                END;
        end;
   IF (HarvestNow = true) THEN
      BEGIN
      SetNrCut(GetNrCut() + 1);
      SetDayLastCut(DayInSeason);
      CGCadjustmentAfterCutting := false; // adjustement CGC
      IF (GetCCiPrev() > (GetManagement_Cuttings_CCcut()/100)) THEN
         BEGIN
         SetCCiPrev(GetManagement_Cuttings_CCcut()/100);
         // ook nog CCwithered
         SetCrop_CCxWithered(0);  // or CCiPrev ??
         SetCCxWitheredTpot(0); // for calculation Maximum Biomass but considering soil fertility stress
         SetCCxWitheredTpotNoS(0); //  for calculation Maximum Biomass unlimited soil fertility
         SetCrop_CCxAdjusted(GetCCiPrev()); // new
         // Increase of CGC
         CGCadjustmentAfterCutting := true; // adjustement CGC
         END;
      // Record harvest
      IF Part1Mult THEN RecordHarvest(GetNrCut(),GetDayNri(),DayInSeason,GetSumInterval(),BprevSum,YprevSum,fHarvest);
      // Reset
      SetSumInterval(0);
      SumGDDcuts := 0;
      BprevSum := GetSumWaBal_Biomass();
      YprevSum := GetSumWaBal_YieldPart();
      END;
   END;

(* 14. Write results *)
//14.a Summation
SumETo := SumETo + GetETo();
SumGDD := SumGDD + GetGDDayi();
//14.b Stress totals
IF (GetCCiActual() > 0) THEN
   BEGIN
   // leaf expansion growth
   IF (GetStressLeaf() > - 0.000001) THEN
      SetStressTot_Exp(((GetStressTot_NrD() - 1)*GetStressTot_Exp() + GetStressLeaf())/GetStressTot_NrD());
   // stomatal closure
   IF (GetTpot() > 0) THEN
      BEGIN
      StressStomata := 100 *(1 - GetTact()/GetTpot());
      IF (StressStomata > - 0.000001) THEN
         SetStressTot_Sto(((GetStressTot_NrD() - 1)*GetStressTot_Sto() + StressStomata)/GetStressTot_NrD());
      END;
   END;
// weed stress
IF (WeedRCi > - 0.000001) THEN
   SetStressTot_Weed(((GetStressTot_NrD() - 1)*GetStressTot_Weed() + WeedRCi)/GetStressTot_NrD());
//14.c Assign crop parameters
SetPlotVarCrop_ActVal(GetCCiActual()/CCxCropWeedsNoSFstress * 100);
SetPlotVarCrop_PotVal(100 * (1/CCxCropWeedsNoSFstress) *
                              CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1),GetCrop().DaysToGermination,
                              GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                              GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                              (fWeedNoS*GetCrop().CCo),(fWeedNoS*GetCrop().CCx),GetCGCref(),
                              (GetCrop().CDC*(fWeedNoS*GetCrop().CCx + 2.29)/(GetCrop().CCx + 2.29)),
                              GetGDDCGCref(),(GetCrop().GDDCDC*(fWeedNoS*GetCrop().CCx + 2.29)/(GetCrop().CCx + 2.29)),
                              SumGDDadjCC,GetCrop().ModeCycle,
                              (0),(0)));
IF ((VirtualTimeCC+GetSimulation_DelayedDays() + 1) <= GetCrop().DaysToFullCanopySF)
   THEN BEGIN // not yet canopy decline with soil fertility stress
        PotValSF := 100 * (1/CCxCropWeedsNoSFstress) *
                         CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1),GetCrop().DaysToGermination,
                         GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                         GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                         CCoTotal,CCxTotal,GetCrop().CGC,
                         CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                         SumGDDadjCC,GetCrop().ModeCycle,
                         GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
        END
   ELSE GetPotValSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1),PotValSF);
//14.d Print ---------------------------------------
IF (GetOutputAggregate() > 0) THEN CheckForPrint(TheProjectFile);
IF OutDaily THEN WriteDailyResults((GetDayNri()-GetSimulation_DelayedDays()-GetCrop().Day1+1),StageCode,WPi);
IF (Part2Eval AND (GetObservationsFile() <> '(None)')) THEN WriteEvaluationData((GetDayNri()-GetSimulation_DelayedDays()-GetCrop().Day1+1),StageCode);

(* 15. Prepare Next day *)
//15.a Date
SetDayNri(GetDayNri() + 1);
//15.b Irrigation
IF (GetDayNri() = GetCrop().Day1)
   THEN SetIrriInterval(1)
   ELSE SetIrriInterval(GetIrriInterval() + 1);
//15.c Rooting depth
//15.bis extra line for standalone
IF OutDaily THEN DetermineGrowthStage(GetDayNri(),GetCCiPrev(),StageCode);
// 15.extra - reset ageing of Kc at recovery after full senescence
IF (GetSimulation_SumEToStress() >= 0.1) THEN SetDayLastCut(GetDayNri());
//15.d Read Climate next day, Get GDDays and update SumGDDays
IF (GetDayNri() <= GetSimulation_ToDayNr()) THEN
   BEGIN
   IF (GetEToFile() <> '(None)') THEN
        BEGIN
        TempString := fEToSIM_read();
        ReadStr(TempString, ETo_tmp);
        SetETo(ETo_tmp);
        END;
   IF (GetRainFile() <> '(None)') THEN
   BEGIN
      TempString := fRainSIM_read();
      ReadStr(TempString, tmpRain);
      SetRain(tmpRain);
      END;
   IF (GetTemperatureFile() = '(None)')
      THEN BEGIN
           SetTmin(GetSimulParam_Tmin());
           SetTmax(GetSimulParam_Tmax());
           END
      ELSE BEGIN
           TempString := fTempSIM_read();
           ReadStr(TempString, Tmin_temp, Tmax_temp);
           SetTmin(Tmin_temp);
           SetTmax(Tmax_temp);
           END;
   SetGDDayi(DegreesDay(GetCrop().Tbase,GetCrop().Tupper,GetTmin(),GetTmax(),GetSimulParam_GDDMethod()));
   IF (GetDayNri() >= GetCrop().Day1) THEN
      BEGIN
      SetSimulation_SumGDD(GetSimulation_SumGDD() + GetGDDayi());
      SetSimulation_SumGDDfromDay1(GetSimulation_SumGDDfromDay1() + GetGDDayi());
      END;
   END;

END; (* AdvanceOneTimeStep *)

PROCEDURE FileManagement();
VAR RepeatToDay : LongInt;

BEGIN (* FileManagement *)
RepeatToDay := GetSimulation_ToDayNr();
REPEAT
  AdvanceOneTimeStep()
UNTIL ((GetDayNri()-1) = RepeatToDay);
END; // FileManagement


PROCEDURE InitializeSimulation(TheProjectFile_ : string;
                               TheProjectType : repTypeProject);
BEGIN
TheProjectFile := TheProjectFile_;
OpenOutputRun(TheProjectType); // open seasonal results .out
IF OutDaily THEN OpenOutputDaily(TheProjectType);  // Open Daily results .OUT
IF Part1Mult THEN OpenPart1MultResults(TheProjectType,fHarvest); // Open Multiple harvests in season .OUT
END;  // InitializeSimulation


PROCEDURE FinalizeSimulation();
BEGIN
fRun_close(); // Close Run.out
IF OutDaily THEN fDaily_close();  // Close Daily.OUT
IF Part1Mult THEN Close(fHarvest);  // Close Multiple harvests in season
END;  // FinalizeSimulation


PROCEDURE InitializeRun(NrRun : ShortInt; TheProjectType : repTypeProject);
VAR SumWaBal_temp, PreviousSum_temp : rep_sum;

    PROCEDURE AdjustCompartments;
    VAR TotDepth : double;
        i : ShortInt;
        Comp_temp : rep_Comp;
    BEGIN
    //Adjust size of compartments if required
    TotDepth := 0;
    FOR i := 1 to GetNrCompartments() DO TotDepth := TotDepth + GetCompartment_Thickness(i);
    IF GetSimulation_MultipleRunWithKeepSWC() // Project with a sequence of simulation runs and KeepSWC
       THEN BEGIN
            IF (ROUND(GetSimulation_MultipleRunConstZrx()*1000) > ROUND(TotDepth*1000))
               THEN AdjustSizeCompartments(GetSimulation_MultipleRunConstZrx());
            END
       ELSE BEGIN
            IF (ROUND(GetCrop().RootMax*1000) > ROUND(TotDepth*1000)) THEN
               BEGIN
               IF (ROUND(GetSoil().RootMax*1000) = ROUND(GetCrop().RootMax*1000))
                  THEN BEGIN // no restrictive soil layer
                       AdjustSizeCompartments(GetCrop().RootMax);
                       // adjust soil water content
                       Comp_temp := GetCompartment();

                       CalculateAdjustedFC((GetZiAqua()/100),Comp_temp);
                       SetCompartment(Comp_temp);
                       IF GetSimulation_IniSWC_AtFC() THEN ResetSWCToFC;
                       END
                  ELSE BEGIN // restrictive soil layer
                       IF (ROUND(GetSoil().RootMax*1000) > ROUND(TotDepth*1000)) THEN
                          BEGIN
                          AdjustSizeCompartments(GetSoil().RootMax);
                          // adjust soil water content
                          Comp_temp := GetCompartment();
                          CalculateAdjustedFC((GetZiAqua()/100),Comp_temp);
                          SetCompartment(Comp_temp);
                          IF GetSimulation_IniSWC_AtFC() THEN ResetSWCToFC;
                          END
                       END;
               END;
            END;
    END; // AdjustCompartments

BEGIN
LoadSimulationRunProject(GetMultipleProjectFileFull(),NrRun);
AdjustCompartments;
SumWaBal_temp := GetSumWaBal();
GlobalZero(SumWabal_temp);
SetSumWaBal(SumWaBal_temp);
ResetPreviousSum(SumETo,SumGDD,PreviousSumETo,PreviousSumGDD,PreviousBmob,PreviousBsto);
InitializeSimulationRun;
IF OutDaily THEN WriteTitleDailyResults(TheProjectType,NrRun);
IF Part1Mult THEN WriteTitlePart1MultResults(TheProjectType,NrRun,fHarvest);
IF (Part2Eval AND (GetObservationsFile() <> '(None)')) THEN CreateEvalData(NrRun);
END; // InitializeRun


PROCEDURE FinalizeRun1(NrRun : ShortInt;
                       TheProjectFile : string;
                       TheProjectType : repTypeProject);

    PROCEDURE RecordHarvest(NrCut : INTEGER;
                        DayNri : LongInt;
                        DayInSeason,SumInterval : INTEGER;
                        BprevSum,YprevSum : double;
                        VAR fHarvest : text);
    VAR Dayi,Monthi,Yeari : INTEGER;
        NoYear : BOOLEAN;
    BEGIN
    Append(fHarvest);
    DetermineDate(GetCrop().Day1,Dayi,Monthi,Yeari);
    NoYear := (Yeari = 1901);
    DetermineDate(DayNri,Dayi,Monthi,Yeari);
    IF NoYear THEN Yeari := 9999;
    IF (NrCut = 9999)
       THEN BEGIN
            // last line at end of season
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,GetSumWaBal_Biomass():34:3);
            IF (GetCrop().DryMatter = undef_int)
               THEN WRITELN(fHarvest,GetSumWaBal_YieldPart():20:3)
               ELSE WRITELN(fHarvest,GetSumWaBal_YieldPart():20:3,(GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):20:3);
            END
       ELSE BEGIN
            WRITE(fHarvest,NrCut:6,Dayi:6,Monthi:6,Yeari:6,DayInSeason:6,SumInterval:6,(GetSumWaBal_Biomass()-BprevSum):12:3,
                  GetSumWaBal_Biomass():10:3,(GetSumWaBal_YieldPart()-YprevSum):10:3);
            IF (GetCrop().DryMatter = undef_int)
               THEN WRITELN(fHarvest,GetSumWaBal_YieldPart():10:3)
               ELSE WRITELN(fHarvest,GetSumWaBal_YieldPart():10:3,((GetSumWaBal_YieldPart()-YprevSum)/(GetCrop().DryMatter/100)):10:3,
                         (GetSumWaBal_YieldPart()/(GetCrop().DryMatter/100)):10:3);
            END;
    END; // RecordHarvest

BEGIN

(* 16. Finalise *)
IF  ((GetDayNri()-1) = GetSimulation_ToDayNr()) THEN
    BEGIN
    // multiple cuttings
    IF Part1Mult THEN
       BEGIN
       IF (GetManagement_Cuttings_HarvestEnd() = true) THEN
          BEGIN  // final harvest at crop maturity
          SetNrCut(GetNrCut() + 1);
          RecordHarvest(GetNrCut(),GetDayNri(),(GetDayNri()-GetCrop().Day1+1),GetSumInterval(),BprevSum,YprevSum,fHarvest);
          END;
       RecordHarvest((9999),GetDayNri(),(GetDayNri()-GetCrop().Day1+1),GetSumInterval(),BprevSum,YprevSum,fHarvest); // last line at end of season
       END;
    // intermediate results
    IF ((GetOutputAggregate() = 2) OR (GetOutputAggregate() = 3) // 10-day and monthly results
        AND ((GetDayNri()-1) > PreviousDayNr)) THEN
        BEGIN
        SetDayNri(GetDayNri()-1);
        WriteIntermediatePeriod(TheProjectFile);
        END;
    //
    WriteSimPeriod(NrRun,TheProjectFile);
    END;
END; // FinalizeRun1


PROCEDURE FinalizeRun2(NrRun : ShortInt; TheProjectType : repTypeProject);

    PROCEDURE CloseEvalDataPerformEvaluation (NrRun : ShortInt);
    VAR totalnameEvalStat,StrNr : string;

    BEGIN  // CloseEvalDataPerformEvaluation
    // 1. Close Evaluation data file  and file with observations
    fEval_close();
    IF (LineNrEval <> undef_int) THEN fObs_close();
    // 2. Specify File name Evaluation of simulation results - Statistics
    StrNr := '';
    IF (GetSimulation_MultipleRun() AND (GetSimulation_NrRuns() > 1)) THEN Str(NrRun:3,StrNr);
    CASE TheProjectType OF
      TypePRO : totalnameEvalStat := CONCAT(GetPathNameOutp(),GetOutputName(),'PROevaluation.OUT');
      TypePRM : BEGIN
                Str(NrRun:3,StrNr);
                totalnameEvalStat := CONCAT(GetPathNameOutp(),GetOutputName(),'PRM',Trim(StrNr),'evaluation.OUT');
                END;
      end;
    // 3. Create Evaluation statistics file
    WriteAssessmentSimulation(StrNr,totalnameEvalStat,TheProjectType,
                              GetSimulation_FromDayNr(),GetSimulation_ToDayNr());
    // 4. Delete Evaluation data file
    fEval_erase();
    END; // CloseEvalDataPerformEvaluation


    PROCEDURE CloseClimateFiles();
    BEGIN
    IF (GetEToFile() <> '(None)') THEN fEToSIM_close();
    IF (GetRainFile() <> '(None)') THEN fRainSIM_close();
    IF (GetTemperatureFile() <> '(None)') THEN fTempSIM_close();
    END; // CloseClimateFiles


    PROCEDURE CloseIrrigationFile();
    BEGIN
    IF ((GetIrriMode() = Manual) OR (GetIrriMode() = Generate)) THEN fIrri_close();
    END; // CloseIrrigationFile


    PROCEDURE CloseManagementFile();
    BEGIN
    IF GetManagement_Cuttings_Considered() THEN fCuts_close();
    END; // CloseManagementFile

BEGIN
CloseClimateFiles();
CloseIrrigationFile();
CloseManagementFile();
IF (Part2Eval AND (GetObservationsFile() <> '(None)')) THEN CloseEvalDataPerformEvaluation(NrRun);
END; // FinalizeRun2


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
   FinalizeRun1(NrRun, TheProjectFile, TheProjectType);
   FinalizeRun2(NrRun, TheProjectType);
END;

FinalizeSimulation();
END; // RunSimulation


end.
