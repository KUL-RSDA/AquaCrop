unit Simul;

interface

uses Global, interface_global, Math, TempProcessing, interface_tempprocessing, interface_simul;


PROCEDURE DetermineBiomassAndYield(dayi : LongInt;
                                   ETo,TminOnDay,TmaxOnDay,CO2i,GDDayi,Tact,SumKcTop,
                                   CGCref,GDDCGCref,Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF,
                                   Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,AverageSaltStress,SumGDDadjCC,CCtot,FracAssim : double;
                                   VirtualTimeCC,SumInterval : INTEGER;
                                   VAR Biomass,BiomassPot,BiomassUnlim,BiomassTot,YieldPart,WPi,HItimesBEF,
                                       ScorAT1,ScorAT2,HItimesAT1,HItimesAT2,HItimesAT,alfa,alfaMax,SumKcTopStress,SumKci,CCxWitheredTpot,
                                       CCxWitheredTpotNoS,WeedRCi,CCw,TrW : double;
                                   VAR StressSFadjNEW,PreviousStressLevel : ShortInt;
                                   VAR StoreAssimilates,MobilizeAssimilates : BOOLEAN;
                                   VAR AssimToMobilize, AssimMobilized,Bin,Bout : double;
                                   VAR TESTVAL : double);

PROCEDURE BUDGET_module(dayi : LongInt;
                        TargetTimeVal, TargetDepthVal, VirtualTimeCC, SumInterval, DayLastCut,NrDayGrow,
                        Tadj, GDDTadj : Integer;
                        GDDayi,CGCref,GDDCGCref,CO2i,CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
                        Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTotSaltPrev,
                        DayFraction,GDDayFraction,FracAssim : double;
                        StressSFadjNEW : ShortInt;
                        StorageON,MobilizationON : BOOLEAN;
                        VAR StressLeaf,StressSenescence : double;
                        VAR TimeSenescence : double;
                        VAR NoMoreCrop,CGCadjustmentAfterCutting : BOOLEAN;
                        VAR TESTVAL : double);


implementation



PROCEDURE AdjustpStomatalToETo(MeanETo : double;
                               VAR pStomatULAct : double);
BEGIN
CASE GetCrop().pMethod OF
     NoCorrection      : pStomatULAct := GetCrop().pdef ;
     FAOCorrection     : BEGIN
                         pStomatULAct := GetCrop().pdef + GetSimulParam_pAdjFAO() * (0.04*(5-MeanETo))*log10(10-9*GetCrop().pdef);
                         IF pStomatULAct > 1 THEN pStomatULAct := 1;
                         IF pStomatULAct < 0 THEN pStomatULAct := 0;
                         END;
     end;
END; (* AdjustpStomatalToETo *)






PROCEDURE DetermineBiomassAndYield(dayi : LongInt;
                                   ETo,TminOnDay,TmaxOnDay,CO2i,GDDayi,Tact,SumKcTop,
                                   CGCref,GDDCGCref,Coeffb0,Coeffb1,Coeffb2,FracBiomassPotSF,
                                   Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,AverageSaltStress,SumGDDadjCC,CCtot,FracAssim : double;
                                   VirtualTimeCC,SumInterval : INTEGER;
                                   VAR Biomass,BiomassPot,BiomassUnlim,BiomassTot,YieldPart,WPi,HItimesBEF,
                                       ScorAT1,ScorAT2,HItimesAT1,HItimesAT2,HItimesAT,alfa,alfaMax,SumKcTopStress,SumKci,CCxWitheredTpot,
                                       CCxWitheredTpotNoS,WeedRCi,CCw,TrW : double;
                                   VAR StressSFadjNEW,PreviousStressLevel : ShortInt;
                                   VAR StoreAssimilates,MobilizeAssimilates : BOOLEAN;
                                   VAR AssimToMobilize, AssimMobilized,Bin,Bout : double;
                                   VAR TESTVAL : double);
Const TempRange = 5;
      k = 2;
VAR RatioBM,RBM,HItimesTotal,
    pLeafULAct,pLeafLLAct,pStomatULAct,pLL,Ksleaf,Ksstomatal,KsPolWS,KsPolCs,KsPolHs,KsPol,
    Wrel,Dcor,fFlor,fSwitch,fCCx : double;
    tmax1,tmax2,DayCor,DayiAfterFlowering,DaysYieldFormation : INTEGER;
    PercentLagPhase : ShortInt;
    WPsf, WPunlim, BioAdj,CCtotStar,CCwStar : double;
    wdrc_temp, HIfinal_temp : integer;
    SWCtopSoilConsidered_temp : boolean;

    FUNCTION FractionFlowering(dayi : LongInt) : double;
    VAR f1,f2,F : double;
        DiFlor : INTEGER;

        FUNCTION FractionPeriod(DiFlor : INTEGER) : double;
        VAR fi,TimePerc : double;
        BEGIN
        IF (DiFlor <= 0)
           THEN fi := 0
           ELSE BEGIN
                TimePerc := 100 * (DiFlor/GetCrop().LengthFlowering);
                IF (TimePerc > 100)
                   THEN fi := 1
                   ELSE BEGIN
                        fi := 0.00558 * exp(0.63*Ln(TimePerc)) - 0.000969 * TimePerc - 0.00383;
                        IF (fi < 0) THEN fi := 0;
                        END;
                END;
        FractionPeriod := fi;
        END; (* FractionPeriod *)

    BEGIN
    IF (GetCrop().LengthFlowering <=1)
       THEN F := 1
       ELSE BEGIN
            DiFlor := ROUND(dayi - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering));
            f2 := FractionPeriod(DiFlor);
            DiFlor := ROUND((dayi-1) - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering));
            f1 := FractionPeriod(DiFlor);
            IF (ABS(f1-f2) < 0.0000001)
               THEN F := 0
               ELSE F := 100 * ((f1+f2)/2)/GetCrop().LengthFlowering;
            END;
    FractionFlowering := F;
    END; (* FractionFlowering *)


    FUNCTION YearWeighingFactor(CropFirstDayNr : LongInt) : INTEGER;
    Var Dayi,Monthi,Yeari : INTEGER;
    BEGIN
    DetermineDate(CropFirstDayNr,Dayi,Monthi,Yeari);
    YearWeighingFactor := Yeari;
    END; (* YearWeighingFactor *)




BEGIN
TESTVAL := undef_int;

// 0. Reference HarvestIndex for that day (alfa in percentage) + Information on PercentLagPhase (for estimate WPi)
IF ((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain) OR (GetCrop().Subkind = Vegetative) OR (GetCrop().Subkind = Forage))
   THEN BEGIN //DaysToFlowering corresponds with Tuberformation
        IF  (((GetCrop().Subkind = Vegetative) AND (GetCrop().Planting = Regrowth)
           OR (GetCrop().Subkind = Forage) AND (GetCrop().Planting = Regrowth)))
           THEN alfa := GetCrop().HI
           ELSE BEGIN
                HIfinal_temp := GetSimulation_HIfinal();
                alfa := HarvestIndexDay((dayi-GetCrop().Day1),GetCrop().DaysToFlowering,GetCrop().HI,GetCrop().dHIdt,GetCCiActual(),
                              GetCrop().CCxAdjusted,GetSimulParam_PercCCxHIfinal(),GetCrop().Planting,
                              PercentLagPhase,HIfinal_temp);
                SetSimulation_HIfinal(HIfinal_temp);
                END;
        END;

//WPi := undef_int; // for the case ETo is zero, WPi is not determined and hence not displayed
WPi := (GetCrop().WP/100);

// 1. biomass
IF (ETo > 0) THEN
   BEGIN
   // 1.1 WPi for that day
   // 1.1a - given WPi
   WPi := (GetCrop().WP/100);
   // 1.1b - adjustment WPi for reproductive stage (works with calendar days)
   IF (((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain)) AND (alfa > 0)) THEN // WPi switch to WP for reproductive stage
      BEGIN
      fSwitch := 1;
      DaysYieldFormation := ROUND(GetCrop().HI/GetCrop().dHIdt);
      IF (DaysYieldFormation > 0) THEN
         BEGIN
         IF GetCrop().DeterminancyLinked
            THEN fSwitch := PercentLagPhase/100
            ELSE BEGIN
                 DayiAfterFlowering := dayi - GetSimulation_DelayedDays() - GetCrop().Day1 - GetCrop().DaysToFlowering;
                 IF (DayiAfterFlowering < (DaysYieldFormation/3))
                    THEN fSwitch := DayiAfterFlowering/(DaysYieldFormation/3);
                 END;
         END;
      WPi :=  WPi * (1 - (1-GetCrop().WPy/100)*fSwitch)  // switch in Lag Phase
      END;

   // 1.1c - adjustment WPi for CO2
   IF ROUND(100*CO2i) <> ROUND(100*CO2Ref) THEN
      BEGIN
      WPi := WPi * fAdjustedForCO2(CO2i,GetCrop().WP,GetCrop().AdaptedToCO2);
      END;


   // 1.1d - adjustment WPi for Soil Fertility
   WPsf := WPi; // no water stress, but fertility stress
   WPunlim := WPi; // no water stress, no fertiltiy stress
   IF (GetSimulation_EffectStress_RedWP() > 0)  // Reductions are zero if no fertility stress
      THEN BEGIN // water stress and fertility stress
           IF ((SumKci/SumKcTopStress) < 1)
              THEN BEGIN
                   IF (ETo > 0) THEN SumKci := SumKci + Tact/ETo;
                   IF (SumKci > 0) THEN WPi := WPi * (1 - (GetSimulation_EffectStress_RedWP()/100) * exp(k*ln(SumKci/SumKcTopStress)) );
                   END
              ELSE WPi := WPi * (1 - GetSimulation_EffectStress_RedWP()/100);
           END
      ELSE IF (ETo > 0) THEN SumKci := SumKci + Tact/ETo;



   // 1.2 actual biomass
   //IF (Management.WeedRC > 0)
   //IF ((Management.WeedRC > 0) AND (ROUND(CCtot*10000) > 0))
   IF ((GetSimulation_RCadj() > 0) AND (ROUND(CCtot*10000) > 0))
      THEN BEGIN // weed infestation
           // green canopy cover of the crop in weed-infested field
           IF (GetManagement_WeedDeltaRC() <> 0)
              THEN BEGIN
                   IF (GetCrop().subkind = Forage)
                      THEN fCCx := MultiplierCCxSelfThinning(GetSimulation_YearSeason(),GetCrop().YearCCx,GetCrop().CCxRoot)
                      ELSE fCCx := 1;
                   wdrc_temp := GetManagement_WeedDeltaRC();
                   WeedRCi := GetWeedRC(VirtualTimeCC,SumGDDadjCC,fCCx,
                                 //Management.WeedRC,Management.WeedAdj,
                                 GetSimulation_RCadj(),GetManagement_WeedAdj(),
                                 wdrc_temp,
                                 GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,
                                 GetCrop().GDDaysToFullCanopySF,GetCrop().GDDaysToSenescence,
                                 GetCrop_ModeCycle());
                   SetManagement_WeedDeltaRC(wdrc_temp);
                   END
              //ELSE WeedRCi := Management.WeedRC;
              ELSE WeedRCi := GetSimulation_RCadj();
           CCw := CCtot * (1-WeedRCi/100);
           // correction for micro-advection
           CCtotStar := 1.72*CCtot - 1*(CCtot*CCtot) + 0.30*(CCtot*CCtot*CCtot);
           IF (CCtotStar < 0) THEN CCtotStar := 0;
           IF (CCtotStar > 1) THEN CCtotStar := 1;
           IF (CCw > 0.0001)
              THEN CCwStar := CCw + (CCtotStar - CCtot)
              ELSE CCwStar := 0;
           // crop transpiration in weed-infested field
           IF (CCtotStar <= 0.0001)
              THEN TrW := 0
              ELSE TrW := Tact * (CCwStar/CCtotStar);
           // crop biomass in weed-infested field
           Biomass := Biomass + WPi *(TrW/ETo);  (* ton/ha*)
           END
      ELSE BEGIN
           WeedRCi := 0.0;
           CCw := CCtot;
           TrW := Tact;
           Biomass := Biomass + WPi *(Tact/ETo);  (* ton/ha*)
           END;

   // Transfer of assimilates
   IF (GetCrop_subkind() = Forage) THEN // only for perennial herbaceous forage crops
      BEGIN
      // 1. Mobilize assimilates at start of season
      IF (MobilizeAssimilates = true) THEN
         BEGIN
         // mass to mobilize
         IF (FracAssim < 0.05) THEN FracAssim := 0.05;
         Bin := FracAssim * WPi *(TrW/ETo);  (* ton/ha*)
         IF ((AssimMobilized + Bin) > AssimToMobilize) THEN Bin := AssimToMobilize - AssimMobilized;
         // cumulative mass mobilized
         AssimMobilized := AssimMobilized + Bin;
         // switch mobilize off when all mass is transfered
         IF (ROUND(1000*AssimToMobilize) <= ROUND(1000*AssimMobilized))
            THEN MobilizeAssimilates := false;
         END;
      // 2. Store assimilates at end of season
      IF (StoreAssimilates = true) THEN
         BEGIN
         // mass to store
         Bout := FracAssim * WPi *(TrW/ETo);  (* ton/ha*)
         // cumulative mass stored
         SetSimulation_Storage_Btotal(GetSimulation_Storage_Btotal() + Bout);
         END;
      TESTVAL := FracAssim;
      END;

   Biomass := Biomass + Bin - Bout;  (* ton/ha*) // correction for transferred assimilates

   // actual total biomass (crop and weeds)
   BiomassTot := BiomassTot + WPi *(Tact/ETo);  (* ton/ha*)  // for dynamic adjustment of soil fertility stress
   BiomassTot := BiomassTot + Bin - Bout; // correction for transferred assimilates

   // 1.3 potential biomass - unlimited soil fertiltiy
   BiomassUnlim := BiomassUnlim + Bin - Bout; // correction for transferred assimilates
   END; // (ETo > 0)

//1.4 potential biomass for given soil fertility
BiomassPot :=  FracBiomassPotSF * BiomassUnlim; (* ton/ha*)

// 2. yield
tmax1 := undef_int;
IF ((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain)) THEN
   BEGIN  //DaysToFlowering corresponds with Tuberformation
   IF (dayi > (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering)) THEN
      BEGIN  // calculation starts when flowering has started

      // 2.2 determine HImultiplier at the start of flowering
      // effect of water stress before flowering (HItimesBEF)
      IF (HItimesBEF < - 0.1) THEN   // i.e. undefined at the start of flowering
         BEGIN
         IF (BiomassPot < 0.0001)
            THEN HItimesBEF := 1
            ELSE BEGIN
                 RatioBM := Biomass/BiomassPot;
                 // Not correct if weed infestation and no fertility stress
                 // for that case BiomassPot might be larger (but cannot be calculated since WP is unknown)
                 IF (RatioBM > 1) THEN RatioBM := 1;
                 RBM := BMRange(GetCrop().HIincrease);
                 HItimesBEF := HImultiplier(RatioBM,RBM,GetCrop().HIincrease);
                 END;
         IF (GetCCiActual() <= 0.01) THEN HItimesBEF := 0; // no green canopy cover left at start of flowering;
         END;

      // 2.3 Relative water content for that day
      SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
      DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
      SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
      IF (GetSimulation_SWCtopSoilConsidered() = true) // top soil is relative wetter than total root zone
         THEN Wrel := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP) // top soil
         ELSE Wrel := (GetRootZoneWC().FC - GetRootZoneWC().Actual)/(GetRootZoneWC().FC - GetRootZoneWC().WP); // total root zone

      // 2.4 Failure of Pollination during flowering (alfaMax in percentage)
      IF (GetCrop().Subkind = grain) // - only valid for fruit/grain crops (flowers)
         THEN BEGIN
              IF ((dayi <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + GetCrop().LengthFlowering)) // calculation limited to flowering period
                 AND ((GetCCiActual()*100) > GetSimulParam_PercCCxHIfinal())) THEN // sufficient green canopy remains
                 BEGIN
                 // 2.4a - Fraction of flowers which are flowering on day  (fFlor)
                 fFlor := FractionFlowering(dayi);
                 // 2.4b - Ks(pollination) water stress
                 pLL := 1;
                 KsPolWS := KsAny(Wrel,GetCrop().pPollination,pLL,(0));
                 // 2.4c - Ks(pollination) cold stress
                 KsPolCS := KsTemperature((GetCrop().Tcold-TempRange),GetCrop().Tcold,TminOnDay);
                 // 2.4d - Ks(pollination) heat stress
                 KsPolHS := KsTemperature((GetCrop().Theat+TempRange),GetCrop().Theat,TmaxOnDay);
                 // 2.4e - Adjust alfa
                 KsPol := KsPolWS;
                 IF (KsPol > KsPolCS) THEN KsPol := KsPolCS;
                 IF (KsPol > KsPolHS) THEN KsPol := KsPolHS;
                 alfaMax := alfaMax + (KsPol * (1 + GetCrop().fExcess/100) * fFlor * GetCrop().HI);
                 IF (alfaMax > GetCrop().HI) THEN alfaMax := GetCrop().HI;
                 END;
              END
         ELSE alfaMax := GetCrop().HI; // for Tuber crops (no flowering)

      // 2.5 determine effect of water stress affecting leaf expansion after flowering
             //from start flowering till end of determinancy
      IF GetCrop().DeterminancyLinked
         THEN tmax1 := ROUND(GetCrop().LengthFlowering/2)
         ELSE tmax1 := (GetCrop().DaysToSenescence - GetCrop().DaysToFlowering);
      IF ((HItimesBEF > 0.99) // there is green canopy cover at start of flowering;
          AND (dayi <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + tmax1)) // and not yet end period
          AND (tmax1 > 0) // otherwise no effect
          AND (ROUND(GetCrop().aCoeff) <> Undef_int) // otherwise no effect
          AND (GetCCiActual() > 0.001))  // and as long as green canopy cover remains (for correction to stresses)
         THEN BEGIN
              // determine KsLeaf
              AdjustpLeafToETo(ETo,pLeafULAct,pLeafLLAct);
              Ksleaf := KsAny(Wrel,pLeafULAct,pLeafLLAct,GetCrop().KsShapeFactorLeaf);
              // daily correction
              Dcor := (1 + (1-Ksleaf)/GetCrop().aCoeff);
              // weighted correction
              ScorAT1 := ScorAT1 + Dcor/tmax1;
              DayCor := dayi - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering);
              HItimesAT1  := (tmax1/DayCor) * ScorAT1;
              END;

      // 2.6 determine effect of water stress affecting stomatal closure after flowering
             //during yield formation
      IF (GetCrop().dHIdt > 99)
         THEN tmax2 := 0
         ELSE tmax2 := ROUND(GetCrop().HI/GetCrop().dHIdt);
      IF ((HItimesBEF > 0.99) // there is green canopy cover at start of flowering;
          AND (dayi <= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + tmax2)) // and not yet end period
          AND (tmax2 > 0) // otherwise no effect
          AND (ROUND(GetCrop().bCoeff) <> Undef_int) // otherwise no effect
          AND (GetCCiActual() > 0.001))  // and as long as green canopy cover remains (for correction to stresses)
         THEN BEGIN
              // determine KsStomatal
              AdjustpStomatalToETo(ETo,pStomatULAct);
              pLL := 1;
              Ksstomatal := KsAny(Wrel,pStomatULAct,pLL,GetCrop().KsShapeFactorStomata);
              // daily correction
              IF (Ksstomatal > 0.001)
                   THEN Dcor := (Exp(0.10*Ln(Ksstomatal))) * (1-(1-Ksstomatal)/GetCrop().bCoeff)
                   ELSE Dcor := 0;
              // weighted correction
              ScorAT2 := ScorAT2 + Dcor/tmax2;
              DayCor := dayi - (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering);
              HItimesAT2  := (tmax2/DayCor) * ScorAT2;
              END;

      // 2.7 total multiplier after flowering
      IF ((tmax2 = 0) AND (tmax1 = 0))
         THEN HItimesAT := 1
         ELSE BEGIN
              IF (tmax2 = 0)
                 THEN HItimesAT := HItimesAT1
                 ELSE BEGIN
                      IF (tmax1 = 0)
                         THEN HItimesAT := HItimesAT2
                         ELSE IF (tmax1 <= tmax2)
                                 THEN BEGIN
                                      HItimesAT := HItimesAT2 * ((tmax1*HItimesAT1 + (tmax2-tmax1))/tmax2);
                                      IF (ROUND(GetCrop().bCoeff) = Undef_int) THEN HItimesAT := HItimesAT1;
                                      IF (ROUND(GetCrop().aCoeff) = Undef_int) THEN HItimesAT := HItimesAT2;
                                      END
                                 ELSE BEGIN
                                      HItimesAT := HItimesAT1 * ((tmax2*HItimesAT2 + (tmax1-tmax2))/tmax1);
                                      IF (ROUND(GetCrop().bCoeff) = Undef_int) THEN HItimesAT := HItimesAT1;
                                      IF (ROUND(GetCrop().aCoeff) = Undef_int) THEN HItimesAT := HItimesAT2;
                                      END;
                      END;
              END;

      // 2.8 Limit HI to allowable maximum increase
      HItimesTotal := HItimesBEF * HItimesAT;
      IF (HItimesTotal > (1+(GetCrop().DHImax/100))) THEN HItimesTotal := 1+(GetCrop().DHImax/100);

      // 2.9 Yield
      IF (alfaMax >= alfa)
         THEN YieldPart := Biomass * HItimesTotal*(alfa/100)
         ELSE YieldPart := Biomass * HItimesTotal*(alfaMax/100)
      END;
   END; // (Crop.subkind = Tuber) OR (Crop.Subkind = grain)

// 2bis. yield leafy vegetable crops
IF ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage)) THEN
   BEGIN
   IF (dayi >= (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering)) THEN
      BEGIN  // calculation starts at crop day 1 (since days to flowering is 0)
      //YieldPart := Biomass * (alfa/100);
      IF ((100*CCw) < GetSimulParam_PercCCxHIfinal()) THEN alfa := 0;
      //IF (Management.WeedRC > 0)
      IF (ROUND(100*ETo)> 0) THEN
         BEGIN  // with correction for transferred assimilates
         IF (GetSimulation_RCadj() > 0)
            //THEN YieldPart := YieldPart + WPi *(TrW/ETo) * (alfa/100)
            THEN YieldPart := YieldPart + (WPi*(TrW/ETo) + Bin - Bout) * (alfa/100)
            //ELSE YieldPart := YieldPart + WPi *(Tact/ETo) * (alfa/100);
            ELSE YieldPart := YieldPart + (WPi*(Tact/ETo) + Bin - Bout) * (alfa/100);
         END;
      END;
   END;



// 3. Dynamic adjustment of soil fertility stress
IF ((GetManagement_FertilityStress() > 0) AND (BiomassUnlim > 0.001) AND GetCrop_StressResponse().Calibrated)
   THEN BEGIN
        //BioAdj := 100 * (FracBiomassPotSF + (FracBiomassPotSF - Biomass/BiomassUnlim));
        BioAdj := 100 * (FracBiomassPotSF + (FracBiomassPotSF - BiomassTot/BiomassUnlim));
        IF (BioAdj >= 100)
           THEN StressSFadjNEW := 0
           ELSE BEGIN
                IF (BioAdj <= 0)
                   THEN StressSFadjNEW := 80
                   ELSE BEGIN
                        StressSFadjNEW := ROUND(Coeffb0 + Coeffb1*BioAdj + Coeffb2*BioAdj*BioAdj);
                        //IF (StressSFadjNEW < 0) THEN StressSFadjNEW := 0;
                        // since < 0 is unrealistic (= poor calibration)
                        IF (StressSFadjNEW < 0) THEN StressSFadjNEW := GetManagement_FertilityStress();
                        IF (StressSFadjNEW > 80) THEN StressSFadjNEW := 80;
                        END;
                IF (StressSFadjNEW > GetManagement_FertilityStress())
                   THEN StressSFadjNEW := GetManagement_FertilityStress();
                END;
        IF ((GetCrop().Subkind = grain) AND GetCrop().DeterminancyLinked
           AND (dayi > (GetSimulation_DelayedDays() + GetCrop().Day1 + GetCrop().DaysToFlowering + tmax1))) THEN
           BEGIN // potential vegetation period is exceeded
           IF (StressSFadjNEW < PreviousStressLevel) THEN StressSFadjNEW := PreviousStressLevel;
           IF (StressSFadjNEW > GetManagement_FertilityStress())
              THEN StressSFadjNEW := GetManagement_FertilityStress();
           END;
        END
   ELSE StressSFadjNEW := 0;
PreviousStressLevel := StressSFadjNEW;
SumKcTopStress := (1 - StressSFadjNEW/100) * SumKcTop;





END; (* DetermineBiomassAndYield *)






PROCEDURE BUDGET_module(dayi : LongInt;
                        TargetTimeVal, TargetDepthVal, VirtualTimeCC, SumInterval, DayLastCut,NrDayGrow,
                        Tadj, GDDTadj : Integer;
                        GDDayi,CGCref,GDDCGCref,CO2i,CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
                        Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTotSaltPrev,
                        DayFraction,GDDayFraction,FracAssim : double;
                        StressSFadjNEW : ShortInt;
                        StorageON,MobilizationON : BOOLEAN;
                        VAR StressLeaf,StressSenescence : double;
                        VAR TimeSenescence : double;
                        VAR NoMoreCrop,CGCadjustmentAfterCutting : BOOLEAN;
                        VAR TESTVAL : double);

VAR  control : rep_control;
     InfiltratedRain,
     InfiltratedIrrigation,
     InfiltratedStorage,
     EpotTot,SubDrain : double;
     DAP : INTEGER;
     ECInfilt : double ; //EC of the infiltrated water (surface storage)
     WaterTableInProfile : BOOLEAN;
     HorizontalWaterFlow,HorizontalSaltFlow : double;
     EffectStress_temp : rep_EffectStress;
     SWCtopSoilConsidered_temp : boolean;
     EvapWCsurf_temp, CRwater_temp, Tpot_temp, Epot_temp : double;
     Comp_temp : rep_Comp;
     Crop_pActStom_temp : double;
     CRsalt_temp, ECdrain_temp, Tact_temp : double;



PROCEDURE calculate_transpiration(Tpot : double);

VAR WtoExtract, theta_critical, alfa, sinkMM : double;
    compi, layeri, pre_layer : INTEGER;
    DeltaWC, InetThreshold : double;
    TpotMAX, RedFact, RedFactECsw : double;
    Wrel,WrelSalt,pStomatLLAct : double;
    CompiECe,CompiECsw,CompiECswFC : double;
    SWCtopSoilConsidered_temp : boolean;
    Comp_temp : rep_Comp;
    Compi_temp : CompartmentIndividual;



PROCEDURE calculate_theta_critical(layeri : INTEGER;
                                   VAR theta_critical : double);

VAR theta_TAW : double;

BEGIN
theta_TAW := GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100;
theta_critical := GetSoilLayer_i(layeri).FC/100 - theta_TAW * GetCrop().pActStom;
END; (*calculate_theta_critical*)



PROCEDURE calculate_rootfraction_compartment(RootingDepth : double;
                                             VAR Compartment : rep_Comp);

VAR       frac_value, cumdepth : double;
          compi,i : INTEGER;

BEGIN
cumdepth := 0;
compi := 0;
REPEAT
  compi := compi + 1;
  cumdepth := cumdepth + Compartment[compi].Thickness;
  IF (cumdepth <= RootingDepth)
     THEN Compartment[compi].WFactor := 1
     ELSE BEGIN
          frac_value := RootingDepth - (cumdepth - Compartment[compi].Thickness);
          IF (frac_value > 0)
             THEN Compartment[compi].WFactor := frac_value/Compartment[compi].Thickness
             ELSE Compartment[compi].WFactor := 0;
          END;
UNTIL (cumdepth >= RootingDepth) OR (compi = GetNrCompartments());
FOR i := compi+1 TO GetNrCompartments() DO Compartment[i].WFactor := 0;
END; (* calculate_rootfraction_compartment *)



PROCEDURE calculate_sink_values(Tpot,RootingDepth : double;
                                VAR Compartment : rep_Comp;
                                Crop : rep_crop);

VAR       sink_value, StopComp, SbotComp,
          cumdepth : double;
          compi, i : INTEGER;

BEGIN
IF (GetIrriMode() = Inet)
   THEN BEGIN
        sink_value := (GetCrop().SmaxTop + GetCrop().SmaxBot)/2;
        for compi := 1 to GetNrCompartments() DO Compartment[compi].Smax := sink_value;
        END
   ELSE BEGIN
        cumdepth := 0;
        compi := 0;
        SbotComp := GetCrop().SmaxTop;
        REPEAT
           compi := compi + 1;
           StopComp := SbotComp;
           cumdepth := cumdepth + Compartment[compi].Thickness;
           IF (cumdepth <= RootingDepth)
              THEN SbotComp := GetCrop().SmaxBot * GetSimulation_SCor() + (GetCrop().SmaxTop - GetCrop().SmaxBot*GetSimulation_SCor())
                                      * (RootingDepth - cumdepth)/RootingDepth
              ELSE SbotComp := GetCrop().SmaxBot*GetSimulation_SCor();
           Compartment[compi].Smax := ((StopComp + SbotComp)/2);
           IF (Compartment[compi].Smax > 0.06) THEN Compartment[compi].Smax := 0.06;
        UNTIL (cumdepth >= RootingDepth) OR (compi = GetNrCompartments());
        FOR i := (compi + 1) TO GetNrCompartments() DO Compartment[i].Smax := 0;
        END;
END; (* calculate_sink_values *)



PROCEDURE Correction_Anaeroby(VAR Comp : CompartmentIndividual;
                              VAR alfa : double);
VAR alfaAN : double;
    ini : INTEGER;
BEGIN
IF ((GetDaySubmerged() >= GetSimulParam_DelayLowOxygen()) AND (GetCrop().AnaeroPoint > 0))
   THEN alfaAN := 0
   ELSE IF (Comp.theta > (GetSoilLayer_i(Comp.Layer).SAT - GetCrop().AnaeroPoint)/100)
           THEN BEGIN
                Comp.DayAnaero := Comp.DayAnaero + 1;
                IF (Comp.DayAnaero >= GetSimulParam_DelayLowOxygen())
                   THEN BEGIN
                        ini := 0;
                        Comp.DayAnaero := GetSimulParam_DelayLowOxygen();
                        END
                   ELSE ini := 1;
                alfaAN := (GetSoilLayer_i(Comp.Layer).SAT/100 - Comp.theta)/(GetCrop().AnaeroPoint/100);
                IF (alfaAN < 0) THEN alfaAN := 0;
                IF (GetSimulParam_DelayLowOxygen() > 1)
                   THEN alfaAN := (ini+(Comp.DayAnaero-1)*alfaAN)/(ini+Comp.DayAnaero-1);
                END
           ELSE BEGIN
                alfaAN := 1;
                Comp.DayAnaero := 0;
                END;
IF (alfa > alfaAN) THEN alfa := alfaAN;
END; (* Correction_Anaeroby *)




PROCEDURE DetermineRootZoneAnaeroConditions(Wsat,Wact,AnaeVol,Zr : double;
                                            VAR RedFact : double);
VAR SATVol,ACTVol : double;
BEGIN
RedFact := 1;
IF ((AnaeVol > 0) AND (Zr > 0))
   THEN BEGIN
        SATVol := Wsat/(10*Zr);
        ACTVol := Wact/(10*Zr);
        IF (ACTVol > SATVol) THEN ACTVol := SATVol;
        IF (ActVol > (SatVol-AnaeVol))
           THEN BEGIN
                SetSimulation_DayAnaero(GetSimulation_DayAnaero() + 1);
                IF (GetSimulation_DayAnaero() > GetSimulParam_DelayLowOxygen()) THEN SetSimulation_DayAnaero(GetSimulParam_DelayLowOxygen());
                RedFact := 1 - (1-((SATVol - ACTVol)/AnaeVol))* (GetSimulation_DayAnaero()/GetSimulParam_DelayLowOxygen());
                END
           ELSE SetSimulation_DayAnaero(0);
        END
   ELSE SetSimulation_DayAnaero(0);
END; (* DetermineRootZoneAnaeroConditions *)



BEGIN (* calculate_transpiration *)
SetTact(0.0);

IF (Tpot > 0) THEN
   BEGIN
   // 1. maximum transpiration in actual root zone
   IF (GetIrriMode() = Inet)
      THEN BEGIN
           // salinity stress not considered
           TpotMAX := Tpot;
           END
      ELSE BEGIN // IrriMode = NOT Inet
           // 1.a effect of water stress and salinity stress
           SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
           DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
           SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);

           // --- 1. Effect of water stress and ECe (total rootzone)
           WrelSalt := (GetRootZoneWC().FC-GetRootZoneWC().Actual)/(GetRootZoneWC().FC-GetRootZoneWC().WP);

           // --- 2. Effect of water stress
           pStomatLLAct := 1;
           IF (GetSimulation_SWCtopSoilConsidered() = true)
              THEN BEGIN // top soil is relative wetter than total root zone
                   IF (GetRootZoneWC().ZtopAct < (0.999 * GetRootZoneWC().ZtopThresh))
                      THEN BEGIN
                           Wrel := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP);
                           RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,(0.0)); // where (0.0) is linear
                           END
                      ELSE RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                   END
              ELSE BEGIN // total root zone
                   IF (GetRootZoneWC().Actual < (0.999 * GetRootZoneWC().Thresh))
                      THEN BEGIN
                           // THE 3 LINES BELOW GIVE THE CORRECT WAY
                              //Wrel := (RootZoneWC.FC-RootZoneWC.Actual)/(RootZoneWC.FC-RootZoneWC.WP);
                              //pStomatLLAct := 1;
                              //RedFact := KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,GetCrop().KsShapeFactorStomata);
                           // THIS LINE IS THE OLD WAY SINCE RELEASE (version 3.0)
                              //RedFact := 1 - (RootZoneWC.Thresh - RootZoneWC.Actual)/(RootZoneWC.Thresh - RootZoneWC.WP);
                              //IF (RedFact < 0) THEN RedFact := 0;
                           // These lines are the new way following the old procedure (but adjusted for soil salinity)
                           Wrel := (GetRootZoneWC().FC-GetRootZoneWC().Actual)/(GetRootZoneWC().FC-GetRootZoneWC().WP);
                           RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,(0.0)); // where (0.0) is linear
                           END
                      ELSE RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                   END;

           IF (RedFact < 0) THEN RedFact := 0;
           IF (RedFact > 1) THEN RedFact := 1;

           // --- 3. Extra effect of ECsw (salt in total root zone is considered)
           IF GetSimulation_SalinityConsidered()
              THEN RedFactECsw := AdjustedKsStoToECsw(GetCrop().ECemin,GetCrop().ECemax,GetCrop().ResponseECsw,
                             GetRootZoneSalt().ECe,GetRootZoneSalt().ECsw,GetRootZoneSalt().ECswFC,
                             WrelSalt,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,RedFact)
              ELSE RedFactECsw := RedFact;

           // --- 4. Conclusion (adjustment of TpotMAX considering Water and Salt stress)
           TpotMAX := RedFactECsw * Tpot;

           // 1.b anaerobic conditions in root zone (total root zone is considered)
           DetermineRootZoneAnaeroConditions(GetRootZoneWC().SAT,GetRootZoneWC().Actual,GetCrop().AnaeroPoint,GetRootingDepth(),RedFact);
           TpotMAX := RedFact * TpotMax;
           END;

   // 2. extraction of TpotMax out of the compartments
   // 2.a initial settings
   Comp_temp := GetCompartment();
   calculate_rootfraction_compartment(GetRootingDepth(),Comp_temp);
   calculate_sink_values(TpotMAX,GetRootingDepth(),Comp_temp,GetCrop());
   SetCompartment(Comp_temp);
   compi := 0;
   pre_layer := 0;
   REPEAT
     compi := compi + 1;
     layeri := GetCompartment_Layer(compi);
     IF (layeri > pre_layer) THEN
        BEGIN
        calculate_theta_critical(layeri,theta_critical);
        pre_layer := layeri;
        END;
     // 2.b calculate alfa
     IF (GetIrriMode() = Inet)
        THEN alfa := 1
        ELSE BEGIN
             // effect of water stress and ECe
             IF (GetCompartment_theta(compi) >= (theta_critical))
                THEN alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100)
                ELSE IF (GetCompartment_theta(compi) > (GetSoilLayer_i(layeri).WP/100))
                        THEN BEGIN
                             IF (theta_critical > GetSoilLayer_i(layeri).WP/100)
                                THEN BEGIN
                                     Wrel := (GetSoilLayer_i(layeri).FC/100 - GetCompartment_theta(compi))
                                             /(GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100);
                                     pStomatLLAct := 1;
                                     alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,GetCrop().KsShapeFactorStomata);
                                     END
                                ELSE alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                             END
                        ELSE alfa := 0;
             // extra effect of ECsw
             IF GetSimulation_SalinityConsidered()
                THEN BEGIN
                     WrelSalt := (GetSoilLayer_i(layeri).FC/100 - GetCompartment_theta(compi))
                               /(GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100);
                     CompiECe := ECeComp(GetCompartment_i(compi));
                     CompiECsw := ECswComp(GetCompartment_i(compi),(false));
                     CompiECswFC := ECswComp(GetCompartment_i(compi),(true));
                     RedFactECsw := AdjustedKsStoToECsw(GetCrop().ECemin,GetCrop().ECemax,GetCrop().ResponseECsw,
                              CompiECe,CompiECsw,CompiECswFC,
                              WrelSalt,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,alfa);
                     END
                ELSE RedFactECsw := alfa;
             alfa := RedFactECsw;
             END;
     IF (GetCrop().AnaeroPoint > 0) THEN BEGIN
                                    Compi_temp := GetCompartment_i(compi);
                                    Correction_Anaeroby(Compi_temp,alfa);
                                    SetCompartment_i(compi, Compi_temp);
                                    END;
     // 2.c extract water
     //sink := alfa * Compartment[compi].WFactor * Compartment[compi].Smax;
     sinkMM := 1000 * (alfa * GetCompartment_WFactor(compi) * GetCompartment_Smax(compi)) * GetCompartment_Thickness(compi);
     //theta_to_extract := ((TpotMAX-GetTact())/1000)/Compartment[compi].Thickness;
     WtoExtract := TpotMAX-GetTact();
     //IF (theta_to_extract < sink) THEN sink := theta_to_extract;
     IF (WtoExtract < sinkMM) THEN sinkMM := WtoExtract;
     //Compartment[compi].theta := Compartment[compi].theta - sink;
     SetCompartment_theta(compi, GetCompartment_theta(compi)
          - sinkMM/(1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100)));
     //theta_to_extract := theta_to_extract - sink;
     WtoExtract := WtoExtract - sinkMM;
     //Tact := Tact + sink * 1000 * Compartment[compi].Thickness;
     SetTact(GetTact() + sinkMM);
   //UNTIL ((theta_to_extract <= 0) OR (compi = Nrcompartments));
   UNTIL ((WtoExtract <= 0) OR (compi = GetNrCompartments()));

   // 3. add net irrigation water requirement
   IF (GetIrriMode() = Inet) THEN
     BEGIN // total root zone is considered
     SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
     DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
     SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
     InetThreshold := GetRootZoneWC().FC - GetSimulParam_PercRAW()/100*(GetRootZoneWC().FC - GetRootZoneWC().Thresh);
     IF (GetRootZoneWC().Actual < InetThreshold) THEN
        BEGIN
        pre_layer := 0;
        FOR compi := 1 TO GetNrCompartments() DO
           BEGIN
           layeri := GetCompartment_Layer(compi);
           IF (layeri > pre_layer) THEN
              BEGIN
              calculate_theta_critical(layeri,theta_critical);
              InetThreshold := GetSoilLayer_i(layeri).FC/100 - GetSimulParam_PercRAW()/100*(GetSoilLayer_i(layeri).FC/100 - theta_critical);
              pre_layer := layeri;
              END;
           //DeltaWC := Compartment[compi].WFactor * (InetThreshold - Compartment[compi].Theta)*1000*Compartment[compi].Thickness;
           DeltaWC := GetCompartment_WFactor(compi) * (InetThreshold - GetCompartment_Theta(compi))
                      *1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100);
           //Compartment[compi].Theta := Compartment[compi].Theta + DeltaWC/(1000*Compartment[compi].Thickness);
           SetCompartment_Theta(compi, GetCompartment_theta(compi) + DeltaWC
                                       /(1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100)));
           SetIrrigation(GetIrrigation() + DeltaWC);
           END;
        END;
     END; // (Irrimode = Inet)
   END; // (Tpot > 0)
END; (* calculate_transpiration *)



PROCEDURE surface_transpiration;
VAR Textra, Part : double;
    compi : INTEGER;
    KsReduction,SaltSurface : double;
    Tact_temp : double;
BEGIN
SetDaySubmerged(GetDaySubmerged() + 1);
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    SetCompartment_DayAnaero(compi, GetCompartment_DayAnaero(compi) + 1);
    IF (GetCompartment_DayAnaero(compi) > GetSimulParam_DelayLowOxygen())
       THEN SetCompartment_DayAnaero(compi, GetSimulParam_DelayLowOxygen());
    END;
IF (GetCrop().AnaeroPoint > 0) THEN Part := (1-GetDaySubmerged()/GetSimulParam_DelayLowOxygen())
                          ELSE Part := 1;
KsReduction := KsSalinity(GetSimulation_SalinityConsidered(),GetCrop().ECemin,GetCrop().ECemax,GetECstorage(),(0.0));
SaltSurface := GetSurfaceStorage()*GetECstorage()*Equiv;
IF (GetSurfaceStorage() > KsReduction*Part*GetTpot())
   THEN BEGIN
        SetSurfaceStorage(GetSurfaceStorage() - KsReduction*Part*GetTpot());
        SetTact(KsReduction*Part*GetTpot());
        //NEW
        SetECstorage(SaltSurface/(GetSurfaceStorage()*Equiv)); //salinisation of surface storage layer
        END
   ELSE BEGIN
        SetTact(GetSurfaceStorage() -0.1);
        SetSurfaceStorage(0.1); // zero give error in already updated salt balance
        END;
IF (GetTact() < KsReduction*Part*GetTpot()) THEN
   BEGIN
   Tact_temp := GetTact(); (*Protect Tact from changes in the next routine*)
   calculate_transpiration((KsReduction*Part*GetTpot()-GetTact()));
   SetTact(Tact_temp + GetTact());
   END;
END; (* surface_transpiration *)


PROCEDURE FeedbackCC;
BEGIN
IF (((GetCCiActual() - GetCCiPrev()) > 0.005)  // canopy is still developing
    AND (GetTact() = 0))                  // due to aeration stress or ETo = 0
THEN SetCCiActual(GetCCiPrev());           // no transpiration, no crop developmentc
END; (* FeedbackCC *)



PROCEDURE HorizontalInflowGWTable(DepthGWTmeter : double);
Var Ztot, Zi, DeltaTheta, SaltAct,SaltAdj : double;
    compi,celli : INTEGER;
    Compi_temp : CompartmentIndividual;
BEGIN
Ztot := 0;
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    Ztot := Ztot + GetCompartment_Thickness(compi);
    Zi := Ztot - GetCompartment_Thickness(compi)/2;
    IF (Zi >= DepthGWTmeter) THEN
       BEGIN
       // soil water content is at saturation
       IF (GetCompartment_Theta(compi) < GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100) THEN
          BEGIN
          DeltaTheta := GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100 - GetCompartment_Theta(compi);
          SetCompartment_theta(compi, GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100);
          HorizontalWaterFlow := HorizontalWaterFlow + 1000 * DeltaTheta * GetCompartment_Thickness(compi)
                                 * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
          END;
       // ECe is equal to the EC of the groundwater table
       IF (Abs(ECeComp(GetCompartment_i(compi)) - GetECiAqua()) > 0.0001) THEN
          BEGIN
          SaltAct := 0;
          FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
              SaltAct := SaltAct + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100; // Mg/ha
          Compi_temp := GetCompartment_i(compi);
          DetermineSaltContent(GetECiAqua(),Compi_temp);
          SetCompartment_i(compi, Compi_temp);
          SaltAdj := 0;
          FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
              SaltAdj := SaltAdj + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100; // Mg/ha
          HorizontalSaltFlow := HorizontalSaltFlow + (SaltAdj - SaltAct);
          END;
       END;
    END;
END; (* HorizontalInflowGWTable *)




BEGIN (* BUDGET_module *)


// 1. Soil water balance
control := begin_day;
ECdrain_temp := GetECdrain();
CheckWaterSaltBalance(dayi, InfiltratedRain, control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain_temp,HorizontalWaterFlow, HorizontalSaltFlow, SubDrain);
SetECdrain(ECdrain_temp);

// 2. Adjustments in presence of Groundwater table
CheckForWaterTableInProfile((GetZiAqua()/100),GetCompartment(),WaterTableInProfile);
Comp_temp := GetCompartment();
CalculateAdjustedFC((GetZiAqua()/100),Comp_temp);
SetCompartment(Comp_temp);

// 3. Drainage
calculate_drainage;

// 4. Runoff
IF (GetManagement_Bundheight() < 0.001) THEN
   BEGIN
   SetDaySubmerged(0);
   IF ((GetManagement_RunoffON() = true) AND (GetRain() > 0.1)) THEN calculate_runoff(GetSimulParam_RunoffDepth());
   END;

// 5. Infiltration (Rain and Irrigation)
IF ((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   THEN CalculateEffectiveRainfall(SubDrain);
IF (((GetIrriMode() = Generate) AND (GetIrrigation() = 0)) AND (TargetTimeVal <> -999))
   THEN Calculate_irrigation(SubDrain, TargetTimeVal, TargetDepthVal);
IF (GetManagement_Bundheight() >= 0.01)
   THEN calculate_surfacestorage(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage,ECinfilt, SubDrain, dayi)
   ELSE calculate_Extra_runoff(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain);
calculate_infiltration(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain);

// 6. Capillary Rise
CRwater_temp := GetCRwater();
CRsalt_temp := GetCRsalt();
calculate_CapillaryRise(CRwater_temp,CRsalt_temp);
SetCRwater(CRwater_temp);
SetCRsalt(CRsalt_temp);

// 7. Salt balance
calculate_saltcontent(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain, dayi);


// 8. Check Germination
IF ((GetSimulation_Germinate() = false) AND (dayi >=GetCrop().Day1)) THEN CheckGermination;

// 9. Determine effect of soil fertiltiy and soil salinity stress
// EffectSoilFertilitySalinityStress(Simulation.EffectStress);
IF (NoMoreCrop = false) THEN EffectSoilFertilitySalinityStress(StressSFadjNEW, Coeffb0Salt, Coeffb1Salt,
                                            Coeffb2Salt, NrDayGrow, StressTotSaltPrev, VirtualTimeCC);


// 10. Canopy Cover (CC)
IF (NoMoreCrop = false) THEN
   BEGIN
   // determine water stresses affecting canopy cover
   SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
   DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
   SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
   // determine canopy cover
   CASE GetCrop_ModeCycle() OF
           GDDays : DetermineCCiGDD(CCxTotal,CCoTotal, StressLeaf, FracAssim, MobilizationON, StorageON, SumGDDAdjCC, VirtualTimeCC, StressSenescence, TimeSenescence, NoMoreCrop, CDCTotal, CGCAdjustmentAfterCutting, GDDayFraction, GDDayi, GDDCDCTotal, GDDTadj);
           else DetermineCCi(CCxTotal, CCoTotal, StressLeaf, FracAssim,MobilizationON, StorageON, Tadj, VirtualTimeCC, StressSenescence, TimeSenescence, NoMoreCrop, CDCTotal, CGCAdjustmentAfterCutting, DayFraction, GDDCDCTotal, TESTVAL);
           end;
   END;

// 11. Determine Tpot and Epot
// 11.1 Days after Planting
IF (GetCrop_ModeCycle() = Calendardays)
   THEN DAP := VirtualTimeCC
   ELSE BEGIN // growing degree days - to position correctly where in cycle
        DAP := SumCalendarDays(ROUND(SumGDDadjCC),
                      GetCrop().Day1,GetCrop().Tbase,GetCrop().Tupper,GetSimulParam_Tmin(),GetSimulParam_Tmax());
        DAP := DAP + GetSimulation_DelayedDays(); // are not considered when working with GDDays
        END;
// 11.2 Calculation
Tpot_temp := GetTpot();
CalculateETpot(DAP,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,DayLastCut,
               GetCCiActual(),GetETo(),GetCrop().KcTop,GetCrop().KcDecline,GetCrop().CCxAdjusted,GetCrop().CCxWithered,GetCrop().CCEffectEvapLate,CO2i,
               GDDayi,GetCrop().GDtranspLow,Tpot_temp,EpotTot);
SetTpot(Tpot_temp);
SetEpot(EpotTot);    // adjustment Epot for mulch and partial wetting in next step
Crop_pActStom_temp := GetCrop().pActStom;
AdjustpStomatalToETo(GetETo(),Crop_pActStom_temp);
SetCrop_pActStom(Crop_pActStom_temp);

// 12. Evaporation
IF (GetPreDay() = false) THEN PrepareStage2; // Initialize Simulation.EvapstartStg2 (REW is gone)
IF ((GetRain() > 0) OR
   ((GetIrrigation() > 0) AND (GetIrriMode() <> Inet)))
   THEN PrepareStage1;
EvapWCsurf_temp := GetSimulation_EvapWCsurf();
Epot_temp := GetEpot();
AdjustEpotMulchWettedSurface(dayi,EpotTot,Epot_temp,EvapWCsurf_temp);
SetEpot(Epot_temp);
SetSimulation_EvapWCsurf(EvapWCsurf_temp);
IF (((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   AND (GetSimulParam_EffectiveRain_RootNrEvap() > 0)) // reduction soil evaporation
 THEN SetEpot(GetEpot() * (exp((1/GetSimulParam_EffectiveRain_RootNrEvap())*ln((GetSoil().REW+1)/20))));
// actual evaporation
SetEact(0);
IF (GetEpot() > 0) THEN
   BEGIN
   // surface water
   IF (GetSurfaceStorage() > 0) THEN CalculateEvaporationSurfaceWater;
   // stage 1 evaporation
   IF ((ABS(GetEpot() - GetEact()) > 0.0000001) AND (GetSimulation_EvapWCsurf() > 0))
      THEN CalculateSoilEvaporationStage1;
   // stage 2 evaporation
   IF (ABS(GetEpot() - GetEact()) > 0.0000001) THEN CalculateSoilEvaporationStage2;
   END;
// Reset redcution Epot for 10-day or monthly rainfall data
IF (((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   AND (GetSimulParam_EffectiveRain_RootNrEvap() > 0))
 THEN SetEpot(GetEpot()/(exp((1/GetSimulParam_EffectiveRain_RootNrEvap())*ln((GetSoil().REW+1)/20))));


// 13. Transpiration
IF ((NoMoreCrop = false) AND (GetRootingDepth() > 0.0001)) THEN
   BEGIN
//   Tact_temp := GetTact();
   IF ((GetSurfaceStorage() > 0) AND
       ((GetCrop().AnaeroPoint = 0) OR (GetDaySubmerged() < GetSimulParam_DelayLowOxygen())))
       THEN surface_transpiration
       ELSE calculate_transpiration(GetTpot());
   END;
//   SetTact(Tact_temp);
IF (GetSurfaceStorage() <= 0) THEN SetDaySubmerged(0);
FeedbackCC;

// 14. Adjustment to groundwater table
IF WaterTableInProfile THEN HorizontalInflowGWTable(GetZiAqua()/100);

// 15. Salt concentration
ConcentrateSalts;

// 16. Soil water balance
control := end_day;
ECdrain_temp := GetECdrain();
CheckWaterSaltBalance(dayi,InfiltratedRain, control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain_temp,HorizontalWaterFlow,HorizontalSaltFlow, SubDrain);
SetECdrain(ECdrain_temp);
END; (* BUDGET_module *)




end.
