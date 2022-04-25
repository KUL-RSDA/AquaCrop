unit Run;

interface

uses Global, interface_global, interface_run, interface_rootunit, interface_tempprocessing, interface_climprocessing, interface_simul, interface_inforesults;


PROCEDURE FinalizeRun1(NrRun : ShortInt;
                       TheProjectFile : string;
                       TheProjectType : repTypeProject);
PROCEDURE FinalizeRun2(NrRun : ShortInt; TheProjectType : repTypeProject);

PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);

implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;


// WRITING RESULTS section ================================================= START ====================




PROCEDURE WriteDailyResults(DAP : INTEGER;
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
IF (GetStageCode() = 0) THEN DAP := undef_int; // before or after cropping

// 0. info day
writeStr(tempstring,Di:6,Mi:6,Yi:6,DAP:6,GetStageCode():6);
fDaily_write(tempstring, false);

// 1. Water balance
IF GetOut1Wabal() THEN
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
   IF ((GetOut2Crop() = true) OR (GetOut3Prof() = true) OR (GetOut4Salt() = true)
      OR (GetOut5CompWC() = true) OR (GetOut6CompEC() = true) OR (GetOut7Clim() = true)) THEN
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
IF GetOut2Crop() THEN
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
      ELSE StrW := Round(GetWeedRCi());
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
         (GetCCiActual()*100):8:1,(GetCCiActualWeedInfested()*100):8:1,StrTr:7,KcVal:9:2,GetTpot():9:1,GetTact():9:1,
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
   IF ((GetOut3Prof() = true) OR (GetOut4Salt() = true) OR (GetOut5CompWC() = true) OR (GetOut6CompEC() = true) OR (GetOut7Clim() = true)) THEN
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
IF GetOut3Prof() THEN
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
   IF ((GetOut4Salt() = true) OR (GetOut5CompWC() = true) OR (GetOut6CompEC() = true) OR (GetOut7Clim() = true)) THEN
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
IF GetOut4Salt() THEN
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
   IF ((GetOut5CompWC() = true) OR (GetOut6CompEC() = true) OR (GetOut7Clim() = true))
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
IF GetOut5CompWC() THEN
   BEGIN
   WriteStr(tempstring, (GetCompartment_Theta(1)*100):11:1);
   fDaily_write(tempstring, false);
   FOR Nr := 2 TO (GetNrCompartments()-1) DO 
    BEGIN WriteStr(tempstring, (GetCompartment_Theta(Nr)*100):11:1);
          fDaily_write(tempstring, false);
    END;
   IF ((GetOut6CompEC() = true) OR (GetOut7Clim() = true))
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
IF GetOut6CompEC() THEN
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
   IF (GetOut7Clim = true)
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
IF GetOut7Clim() THEN
   BEGIN
   Ratio1 := (GetTmin() + GetTmax())/2;
   WriteStr(tempstring,GetRain():9:1,GetETo():10:1,GetTmin():10:1,Ratio1:10:1,GetTmax():10:1,GetCO2i():10:2);
   fDaily_write(tempstring);
   END;
END; (* WriteDailyResults *)


// WRITING RESULTS section ================================================= END ====================


PROCEDURE FileManagement();
VAR RepeatToDay : LongInt;

BEGIN (* FileManagement *)
RepeatToDay := GetSimulation_ToDayNr();
REPEAT
  AdvanceOneTimeStep()
UNTIL ((GetDayNri()-1) = RepeatToDay);
END; // FileManagement


PROCEDURE FinalizeRun1(NrRun : ShortInt;
                       TheProjectFile : string;
                       TheProjectType : repTypeProject);
BEGIN

(* 16. Finalise *)
IF  ((GetDayNri()-1) = GetSimulation_ToDayNr()) THEN
    BEGIN
    // multiple cuttings
    IF GetPart1Mult() THEN
       BEGIN
       IF (GetManagement_Cuttings_HarvestEnd() = true) THEN
          BEGIN  // final harvest at crop maturity
          SetNrCut(GetNrCut() + 1);
          RecordHarvest(GetNrCut(),(GetDayNri()-GetCrop().Day1+1));
          END;
       RecordHarvest((9999),(GetDayNri()-GetCrop().Day1+1)); // last line at end of season
       END;
    // intermediate results
    IF ((GetOutputAggregate() = 2) OR (GetOutputAggregate() = 3) // 10-day and monthly results
        AND ((GetDayNri()-1) > GetPreviousDayNr())) THEN
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
    IF (GetLineNrEval() <> undef_int) THEN fObs_close();
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
IF (GetPart2Eval() AND (GetObservationsFile() <> '(None)')) THEN CloseEvalDataPerformEvaluation(NrRun);
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
   FinalizeRun1(NrRun, GetTheProjectFile(), TheProjectType);
   FinalizeRun2(NrRun, TheProjectType);
END;

FinalizeSimulation();
END; // RunSimulation


end.
