unit Run;

interface

uses Global, interface_global, interface_run, interface_rootunit, interface_tempprocessing, interface_climprocessing, interface_simul, interface_inforesults;



PROCEDURE AdvanceOneTimeStep();

PROCEDURE RunSimulation(TheProjectFile_ : string;
                        TheProjectType : repTypeProject);

implementation

uses SysUtils,TempProcessing,ClimProcessing,RootUnit,Simul,StartUnit,InfoResults;


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
IF (GetOutputAggregate() > 0) THEN CheckForPrint(GetTheProjectFile());
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
