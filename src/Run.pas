unit Run;

interface

uses Global, interface_global, interface_run, interface_rootunit, interface_tempprocessing, interface_climprocessing, interface_simul, interface_inforesults;

PROCEDURE InitializeSimulation(TheProjectFile_ : string;
                               TheProjectType : repTypeProject);

PROCEDURE FinalizeSimulation();

PROCEDURE AdvanceOneTimeStep();

PROCEDURE FinalizeRun1(NrRun : ShortInt;
                       TheProjectFile : string;
                       TheProjectType : repTypeProject);
PROCEDURE FinalizeRun2(NrRun : ShortInt; TheProjectType : repTypeProject);

PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);

implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;

var  TheProjectFile : string;




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



PROCEDURE WriteEvaluationData(DAP : INTEGER);
                              
VAR SWCi,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd : double;
    Nr,Di,Mi,Yi: INTEGER;
    TempString : string;
    DayNrEval_temp : INTEGER;

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
IF ((GetLineNrEval() <> undef_int) AND (GetDayNrEval() = GetDayNri())) THEN
   BEGIN
   // read field data
   fObs_rewind();
   FOR Nr := 1 TO (GetLineNrEval() -1) DO fObs_read();
   TempString := fObs_read();
   ReadStr(TempString,Nr,CCfield,CCstd,Bfield,Bstd,SWCfield,SWCstd);
   // get Day Nr for next field data
   fObs_read();
   IF (fObs_eof())
      THEN BEGIN
           SetLineNrEval(undef_int);
           fObs_close();
           END
      ELSE BEGIN
           SetLineNrEval(GetLineNrEval() + 1);
           ReadStr(TempString,DayNrEval_temp);
           SetDayNrEval(DayNrEval_temp);
           SetDayNrEval(GetDayNr1Eval() + GetDayNrEval() -1);
           END;
   END;
//2. Date
DetermineDate(GetDayNri(),Di,Mi,Yi);
IF (GetClimRecord_FromY() = 1901) THEN Yi := Yi - 1901 + 1;
IF (GetStageCode() = 0) THEN DAP := undef_int; // before or after cropping
//3. Write simulation results and field data
SWCi := SWCZsoil(GetZeval());
WriteStr(TempString, Di:6,Mi:6,Yi:6,DAP:6,GetStageCode():5,(GetCCiActual()*100):8:1,CCfield:8:1,CCstd:8:1,
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
    StressLeaf_temp,StressSenescence_temp, TimeSenescence_temp : double;
    SumKcTopStress_temp, SumKci_temp, WeedRCi_temp, CCiActualWeedInfested_temp : double;
    HItimesBEF_temp, ScorAT1_temp,ScorAT2_temp : double; 
    HItimesAT1_temp, HItimesAT2_temp, HItimesAT_temp : double;
    alfaHI_temp, alfaHIAdj_temp : double;
    TESTVAL : double;
    WaterTableInProfile_temp, NoMoreCrop_temp, CGCadjustmentAfterCutting_temp : boolean;

BEGIN (* AdvanceOneTimeStep *)

(* 1. Get ETo *)
IF (GetEToFile() = '(None)') THEN SetETo(5);

(* 2. Get Rain *)
IF (GetRainFile() = '(None)') THEN SetRain(0);

(* 3. Start mode *)
IF GetStartMode() THEN SetStartMode(false);

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
   GetZandECgwt(ZiAqua_temp,ECiAqua_temp);
   SetZiAqua(ZiAqua_temp);
   SetECiAqua(ECiAqua_temp);
   WaterTableInProfile_temp := GetWaterTableInProfile();
   CheckForWaterTableInProfile((GetZiAqua()/100),GetCompartment(),WaterTableInProfile_temp);
   SetWaterTableInProfile(WaterTableInProfile_temp);
   IF GetWaterTableInProfile() THEN AdjustForWatertable;
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
                                  GetSimulation_EffectStress_CDecline(),(GetCCxTotal()/GetCrop().CCx),GetCrop().ModeCycle));  // (CCxTotal/Crop.CCx) = fWeed
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
           THEN SetCCiPrev(GetCCoTotal());
        END;


(* 7. Rooting depth AND Inet day 1*)
IF (((GetCrop().ModeCycle = CalendarDays) AND ((GetDayNri()-GetCrop().Day1+1) < GetCrop().DaysToHarvest))
              OR ((GetCrop().ModeCycle = GDDays) AND (GetSimulation_SumGDD() < GetCrop().GDDaysToHarvest)))
   THEN BEGIN
        IF (((GetDayNri()-GetSimulation_DelayedDays()) >= GetCrop().Day1) AND ((GetDayNri()-GetSimulation_DelayedDays()) <= GetCrop().DayN))
           THEN BEGIN // rooting depth at DAP (at Crop.Day1, DAP = 1)
                SetRootingDepth(AdjustedRootingDepth(GetPlotVarCrop().ActVal,GetPlotVarCrop().PotVal,GetTpot(),GetTact(),GetStressLeaf(),GetStressSenescence(),
                                (GetDayNri()-GetCrop().Day1+1),GetCrop().DaysToGermination,GetCrop().DaysToMaxRooting,GetCrop().DaysToHarvest,
                                GetCrop().GDDaysToGermination,GetCrop().GDDaysToMaxRooting,GetCrop().GDDaysToHarvest,GetSumGDDPrev(),
                                (GetSimulation_SumGDD()),GetCrop().RootMin,GetCrop().RootMax,GetZiprev(),GetCrop().RootShape,
                                GetCrop().ModeCycle));
                SetZiprev(GetRootingDepth());  // IN CASE rootzone drops below groundwate table
                IF ((GetZiAqua() >= 0) AND (GetRootingDepth() > (GetZiAqua()/100)) AND (GetCrop().AnaeroPoint > 0)) THEN
                   BEGIN
                   SetRootingDepth(GetZiAqua()/100);
                   IF (GetRootingDepth() < GetCrop().RootMin) THEN SetRootingDepth(GetCrop().RootMin);
                   END;
                END
           ELSE SetRootingDepth(0);
        END
   ELSE SetRootingDepth(GetZiprev());
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
InitializeTransferAssimilates(Bin_temp,Bout_temp,ToMobilize_temp,Bmobilized_temp,FracAssim,
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
TimeSenescence_temp := GetTimeSenescence();
NoMoreCrop_temp := GetNoMoreCrop();
CGCadjustmentAfterCutting_temp := GetCGCadjustmentAfterCutting();
BUDGET_module(GetDayNri(),TargetTimeVal,TargetDepthVal,VirtualTimeCC,GetSumInterval(),GetDayLastCut(),GetStressTot_NrD(),
              GetTadj(),GetGDDTadj(),
              GetGDDayi(),GetCGCref(),GetGDDCGCref(),GetCO2i(),GetCCxTotal(),GetCCoTotal(),GetCDCTotal(),GetGDDCDCTotal(),SumGDDadjCC,
              GetCoeffb0Salt(),GetCoeffb1Salt(),GetCoeffb2Salt(),GetStressTot_Salt(),
              GetDayFraction(),GetGDDayFraction(),FracAssim,
              GetStressSFadjNEW(),GetTransfer_Store(),GetTransfer_Mobilize(),
              StressLeaf_temp,StressSenescence_temp,TimeSenescence_temp,NoMoreCrop_temp,CGCadjustmentAfterCutting_temp,TESTVAL);
SetStressLeaf(StressLeaf_temp);
SetStressSenescence(StressSenescence_temp);
SetTimeSenescence(TimeSenescence_temp);
SetNoMoreCrop(NoMoreCrop_temp);
SetCGCadjustmentAfterCutting(CGCadjustmentAfterCutting_temp);

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
IF ((GetRootingDepth() > 0) AND (GetNoMoreCrop() = false))
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
        SumKcTopStress_temp := GetSumKcTopStress();
        SumKci_temp := GetSumKci();
        WeedRCi_temp := GetWeedRCi();
        CCiActualWeedInfested_temp := GetCCiActualWeedInfested();
        HItimesBEF_temp := GetHItimesBEF();
        ScorAT1_temp := GetScorAT1();
        ScorAT2_temp := GetScorAT2();
        HItimesAT1_temp := GetHItimesAT1();
        HItimesAT2_temp := GetHItimesAT2();
        HItimesAT_temp := GetHItimesAT();
        alfaHI_temp := GetalfaHI(); 
        alfaHIAdj_temp := GetalfaHIAdj();
        DetermineBiomassAndYield(GetDayNri(),GetETo(),GetTmin(),GetTmax(),GetCO2i(),GetGDDayi(),GetTact(),GetSumKcTop(),GetCGCref(),GetGDDCGCref(),
                                 GetCoeffb0(),GetCoeffb1(),GetCoeffb2(),GetFracBiomassPotSF(),                             GetCoeffb0Salt(),GetCoeffb1Salt(),GetCoeffb2Salt(),GetStressTot_Salt(),SumGDDadjCC,GetCCiActual(),FracAssim,
                                 VirtualTimeCC,GetSumInterval(),
                                 Biomass_temp,BiomassPot_temp,BiomassUnlim_temp,BiomassTot_temp,
                                 YieldPart_temp,WPi,HItimesBEF_temp,ScorAT1_temp,ScorAT2_temp,HItimesAT1_temp,HItimesAT2_temp,
                                 HItimesAT_temp,alfaHI_temp,alfaHIAdj_temp,SumKcTopStress_temp,SumKci_temp,CCxWitheredTpot_temp,CCxWitheredTpotNoS_temp,
                                 WeedRCi_temp,CCiActualWeedInfested_temp,TactWeedInfested_temp,
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
        SetSumKcTopStress(SumKcTopStress_temp);
        SetSumKci(SumKci_temp);
        SetWeedRCi(WeedRCi_temp);
        SetCCiActualWeedInfested(CCiActualWeedInfested_temp); 
        SetHItimesBEF(HItimesBEF_temp);
        SetScorAT1(ScorAT1_temp);
        SetScorAT2(ScorAT2_temp);
        SetHItimesAT1(HItimesAT1_temp);
        SetHItimesAT2(HItimesAT2_temp);
        SetHItimesAT(HItimesAT_temp);
        SetalfaHI(alfaHI_temp);
        SetalfaHIAdj(alfaHIAdj_temp);
        END
   ELSE BEGIN
        SenStage := undef_int;
        SetWeedRCi(undef_int); // no crop and no weed infestation
        SetCCiActualWeedInfested(0.0); // no crop
        SetTactWeedInfested(0.0); // no crop
        END;

(* 12. Reset after RUN *)
IF (GetPreDay() = false) THEN SetPreviousDayNr(GetSimulation_FromDayNr() - 1);
SetPreDay(true);
IF (GetDayNri() >= GetCrop().Day1) THEN
   BEGIN
   SetCCiPrev(GetCCiActual());
   IF (GetZiprev() < GetRootingDepth()) THEN SetZiprev(GetRootingDepth()); // IN CASE groundwater table does not affect root development
   SetSumGDDPrev(GetSimulation_SumGDD());
   END;
IF (TargetTimeVal = 1) THEN SetIrriInterval(0);

(* 13. Cuttings *)
IF GetManagement_Cuttings_Considered() THEN
   BEGIN
   HarvestNow := false;
   DayInSeason := GetDayNri() - GetCrop().Day1 + 1;
   SetSumInterval(GetSumInterval() + 1);
   SetSumGDDcuts( GetSumGDDcuts() + GetGDDayi());
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
                              IF ((GetSumGDDcuts() >= GetCutInfoRecord1_IntervalGDD())
                                   AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                   AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     DryB   : BEGIN
                              IF (((GetSumWabal_Biomass() - GetBprevSum()) >= GetCutInfoRecord1_MassInfo())
                                                 AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                                 AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     DryY   : BEGIN
                              IF (((GetSumWabal_YieldPart() - GetYprevSum()) >= GetCutInfoRecord1_MassInfo())
                                                   AND (DayInSeason >= GetCutInfoRecord1_FromDay())
                                                   AND (DayInSeason <= GetCutInfoRecord1_ToDay()))
                                 THEN HarvestNow := true;
                              END;
                     FreshY : BEGIN
                              // OK if Crop.DryMatter = undef_int (not specified) HarvestNow remains false
                              IF ((((GetSumWaBal_YieldPart() - GetYprevSum())/(GetCrop().DryMatter/100)) >= GetCutInfoRecord1_MassInfo())
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
      SetCGCadjustmentAfterCutting(false); // adjustement CGC
      IF (GetCCiPrev() > (GetManagement_Cuttings_CCcut()/100)) THEN
         BEGIN
         SetCCiPrev(GetManagement_Cuttings_CCcut()/100);
         // ook nog CCwithered
         SetCrop_CCxWithered(0);  // or CCiPrev ??
         SetCCxWitheredTpot(0); // for calculation Maximum Biomass but considering soil fertility stress
         SetCCxWitheredTpotNoS(0); //  for calculation Maximum Biomass unlimited soil fertility
         SetCrop_CCxAdjusted(GetCCiPrev()); // new
         // Increase of CGC
         SetCGCadjustmentAfterCutting(true); // adjustement CGC
         END;
      // Record harvest
      IF GetPart1Mult() THEN RecordHarvest(GetNrCut(),DayInSeason);
      // Reset
      SetSumInterval(0);
      SetSumGDDcuts(0);
      SetBprevSum(GetSumWaBal_Biomass());
      SetYprevSum(GetSumWaBal_YieldPart());
      END;
   END;

(* 14. Write results *)
//14.a Summation
SetSumETo( GetSumETo() + GetETo());
SetSumGDD( GetSumGDD() + GetGDDayi());
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
IF (GetWeedRCi() > - 0.000001) THEN
   SetStressTot_Weed(((GetStressTot_NrD() - 1)*GetStressTot_Weed() + GetWeedRCi())/GetStressTot_NrD());
//14.c Assign crop parameters
SetPlotVarCrop_ActVal(GetCCiActual()/GetCCxCropWeedsNoSFstress() * 100);
SetPlotVarCrop_PotVal(100 * (1/GetCCxCropWeedsNoSFstress()) *
                              CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1),GetCrop().DaysToGermination,
                              GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                              GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                              (GetfWeedNoS()*GetCrop().CCo),(GetfWeedNoS()*GetCrop().CCx),GetCGCref(),
                              (GetCrop().CDC*(GetfWeedNoS()*GetCrop().CCx + 2.29)/(GetCrop().CCx + 2.29)),
                              GetGDDCGCref(),(GetCrop().GDDCDC*(GetfWeedNoS()*GetCrop().CCx + 2.29)/(GetCrop().CCx + 2.29)),
                              SumGDDadjCC,GetCrop().ModeCycle,
                              (0),(0)));
IF ((VirtualTimeCC+GetSimulation_DelayedDays() + 1) <= GetCrop().DaysToFullCanopySF)
   THEN BEGIN // not yet canopy decline with soil fertility stress
        PotValSF := 100 * (1/GetCCxCropWeedsNoSFstress()) *
                         CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1),GetCrop().DaysToGermination,
                         GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                         GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                         GetCCoTotal(),GetCCxTotal(),GetCrop().CGC,
                         GetCDCTotal(),GetCrop().GDDCGC,GetGDDCDCTotal(),
                         SumGDDadjCC,GetCrop().ModeCycle,
                         GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
        END
   ELSE GetPotValSF((VirtualTimeCC+GetSimulation_DelayedDays() + 1), SumGDDAdjCC, PotValSF);
//14.d Print ---------------------------------------
IF (GetOutputAggregate() > 0) THEN CheckForPrint(TheProjectFile);
IF GetOutDaily() THEN WriteDailyResults((GetDayNri()-GetSimulation_DelayedDays()-GetCrop().Day1+1),WPi);
IF (GetPart2Eval() AND (GetObservationsFile() <> '(None)')) THEN WriteEvaluationData((GetDayNri()-GetSimulation_DelayedDays()-GetCrop().Day1+1));

(* 15. Prepare Next day *)
//15.a Date
SetDayNri(GetDayNri() + 1);
//15.b Irrigation
IF (GetDayNri() = GetCrop().Day1)
   THEN SetIrriInterval(1)
   ELSE SetIrriInterval(GetIrriInterval() + 1);
//15.c Rooting depth
//15.bis extra line for standalone
IF GetOutDaily() THEN DetermineGrowthStage(GetDayNri(),GetCCiPrev());
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
IF GetOutDaily() THEN OpenOutputDaily(TheProjectType);  // Open Daily results .OUT
IF GetPart1Mult() THEN OpenPart1MultResults(TheProjectType); // Open Multiple harvests in season .OUT
END;  // InitializeSimulation


PROCEDURE FinalizeSimulation();
BEGIN
fRun_close(); // Close Run.out
IF GetOutDaily() THEN fDaily_close();  // Close Daily.OUT
IF GetPart1Mult() THEN fHarvest_close();  // Close Multiple harvests in season
END;  // FinalizeSimulation



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
   FinalizeRun1(NrRun, TheProjectFile, TheProjectType);
   FinalizeRun2(NrRun, TheProjectType);
END;

FinalizeSimulation();
END; // RunSimulation


end.
