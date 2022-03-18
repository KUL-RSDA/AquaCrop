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
                                       CCxWitheredTpotNoS,WeedRCi,CCw,Trw : double;
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
                                       CCxWitheredTpotNoS,WeedRCi,CCw,Trw : double;
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
                alfa := HarvestIndexDay((dayi-GetCrop().Day1),GetCrop().DaysToFlowering,GetCrop().HI,GetCrop().dHIdt,CCiactual,
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
         IF (CCiActual <= 0.01) THEN HItimesBEF := 0; // no green canopy cover left at start of flowering;
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
                 AND ((CCiactual*100) > GetSimulParam_PercCCxHIfinal())) THEN // sufficient green canopy remains
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
          AND (CCiactual > 0.001))  // and as long as green canopy cover remains (for correction to stresses)
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
          AND (CCiactual > 0.001))  // and as long as green canopy cover remains (for correction to stresses)
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
TYPE rep_control = (begin_day,end_day);
     rep_WhichTheta = (AtSAT,AtFC,AtWP,AtAct);
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
     CRsalt_temp : double;



PROCEDURE CheckWaterSaltBalance(control: rep_control;
                                InfiltratedIrrigation, InfiltratedStorage : double;
                                VAR Surf0,ECInfilt,ECdrain : double);

VAR compi, layeri, celli :INTEGER;
VAR Surf1,ECw : double;
BEGIN (* CheckWaterSaltBalance *)

CASE control OF
     begin_day:BEGIN
               SetTotalWaterContent_BeginDay(0); // mm
               Surf0 := GetSurfaceStorage(); // mm
               SetTotalSaltContent_BeginDay(0); // Mg/ha
               FOR compi :=1 to GetNrCompartments() DO
                   BEGIN
                   SetTotalWaterContent_BeginDay(GetTotalWaterContent().BeginDay
                   + GetCompartment_theta(compi)*1000*GetCompartment_Thickness(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
                   SetCompartment_fluxout(compi, 0);
                   FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
                           SetTotalSaltContent_BeginDay(GetTotalSaltContent().BeginDay
                           + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100); // Mg/ha
                   END;
               SetDrain(0.0);
               SetRunoff(0.0);
               //Eact:=0.0; at the beginning of the evaporation process it is put at zero
               Tact:=0.0;
               SetInfiltrated(0.0);
               ECinfilt := 0.0;
               SubDrain := 0;
               ECdrain := 0;
               HorizontalWaterFlow := 0;
               HorizontalSaltFlow := 0;
               SetCRwater(0);
               SetCRsalt(0);
               END;

     end_day : BEGIN
               SetInfiltrated(InfiltratedRain+InfiltratedIrrigation+InfiltratedStorage);
               FOR layeri := 1 TO GetSoil().NrSoilLayers DO SetSoilLayer_WaterContent(layeri, 0);
               SetTotalWaterContent_EndDay(0);
               Surf1 := GetSurfaceStorage();
               SetTotalSaltContent_EndDay(0);

               // quality of irrigation water
               IF (dayi < GetCrop().Day1)
                  THEN ECw := GetIrriECw().PreSeason
                  ELSE BEGIN
                       ECw := GetSimulation_IrriECw();;
                       IF (dayi > GetCrop().DayN) THEN ECw := GetIrriECw().PostSeason;
                       END;

               FOR compi :=1 to GetNrCompartments() DO
                   BEGIN
                   SetTotalWaterContent_EndDay(GetTotalWaterContent().EndDay
                   + GetCompartment_theta(compi)*1000*GetCompartment_Thickness(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
                   SetSoilLayer_WaterContent(GetCompartment_Layer(compi), GetSoilLayer_i(GetCompartment_Layer(compi)).WaterContent
                   + GetCompartment_theta(compi)*1000*GetCompartment_theta(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
                   FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
                           SetTotalSaltContent_EndDay(GetTotalSaltContent().EndDay
                           + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100); // Mg/ha
                   END;
               SetTotalWaterContent_ErrorDay(GetTotalWaterContent().BeginDay + Surf0
                              -(GetTotalWaterContent().EndDay+GetDrain()+GetRunoff()+Eact+Tact+Surf1-GetRain()-GetIrrigation()-GetCRwater()-HorizontalWaterFlow));
               SetTotalSaltContent_ErrorDay(GetTotalSaltContent().BeginDay - GetTotalSaltContent().EndDay // Mg/ha
                                            + InfiltratedIrrigation*ECw*Equiv/100
                                            + InfiltratedStorage*ECinfilt*Equiv/100
                                            - GetDrain()*ECdrain*Equiv/100
                                            + GetCRsalt()/100
                                            + HorizontalSaltFlow);
               SetSumWaBal_Epot(GetSumWaBal_Epot() + GetEpot());
               SetSumWaBal_Tpot(GetSumWaBal_Tpot() + GetTpot());
               SetSumWaBal_Rain(GetSumWaBal_Rain() + GetRain());
               SetSumWaBal_Irrigation(GetSumWaBal_Irrigation() + GetIrrigation());
               SetSumWaBal_Infiltrated(GetSumWaBal_Infiltrated() + GetInfiltrated());
               SetSumWaBal_Runoff(GetSumWaBal_Runoff() + GetRunoff());
               SetSumWaBal_Drain(GetSumWaBal_Drain() + GetDrain());
               SetSumWaBal_Eact(GetSumWaBal_Eact() + Eact);
               SetSumWaBal_Tact(GetSumWaBal_Tact() + Tact);
               SetSumWaBal_TrW(GetSumWaBal_TrW() + TactWeedInfested);
               SetSumWaBal_CRwater(GetSumWaBal_CRwater() + GetCRwater());

               IF (((dayi-GetSimulation_DelayedDays()) >= GetCrop().Day1 ) AND ((dayi-GetSimulation_DelayedDays()) <= GetCrop().DayN)) THEN // in growing cycle
                  BEGIN
                  IF (GetSumWaBal_Biomass() > 0) // biomass was already produced (i.e. CC present)
                     THEN BEGIN // and still canopy cover
                          IF (CCiActual > 0) THEN SetSumWaBal_ECropCycle(GetSumWaBal_ECropCycle() + Eact);
                          END
                     ELSE SetSumWaBal_ECropCycle(GetSumWaBal_ECropCycle() + Eact); // before germination
                  END;
               SetSumWaBal_CRsalt(GetSumWaBal_CRsalt() + GetCRsalt()/100);
               SetSumWaBal_SaltIn(GetSumWaBal_SaltIn() + (InfiltratedIrrigation*ECw+InfiltratedStorage*ECinfilt)*Equiv/100);
               SetSumWaBal_SaltOut(GetSumWaBal_SaltOut() +  GetDrain()*ECdrain*Equiv/100);
               END;
     END;
END; (* CheckWaterSaltBalance *)




PROCEDURE calculate_CapillaryRise(VAR CRwater,CRsalt : double);
VAR Zbottom,MaxMM,DThetaMax,DTheta,LimitMM,CRcomp,SaltCRi,DrivingForce,ZtopNextLayer,
    Krel,ThetaThreshold  : double;
    compi,SCellAct,layeri : INTEGER;
BEGIN
Zbottom := 0;
FOR compi := 1 TO GetNrCompartments() DO Zbottom := Zbottom + GetCompartment_Thickness(compi);

// start at the bottom of the soil profile
compi := GetNrCompartments();
MaxMM := MaxCRatDepth(GetSoilLayer_i(GetCompartment_Layer(compi)).CRa,GetSoilLayer_i(GetCompartment_Layer(compi)).CRb,
                      GetSoilLayer_i(GetCompartment_Layer(compi)).InfRate,(Zbottom - GetCompartment_Thickness(compi)/2),(GetZiAqua()/100));

// check restrictions on CR from soil layers below
ZtopNextLayer := 0;
FOR layeri := 1 TO GetCompartment_Layer(GetNrCompartments()) DO ZtopNextLayer := ZtopNextLayer + GetSoilLayer_i(layeri).Thickness;
layeri := GetCompartment_Layer(GetNrCompartments());
WHILE ((ZtopNextLayer < (GetZiAqua()/100)) AND (layeri < GetSoil().NrSoilLayers)) DO
   BEGIN
   layeri := layeri + 1;
   LimitMM := MaxCRatDepth(GetSoilLayer_i(layeri).CRa,GetSoilLayer_i(layeri).CRb,GetSoilLayer_i(layeri).InfRate,ZtopNextLayer,(GetZiAqua()/100));
   IF (MaxMM > LimitMM) THEN MaxMM := LimitMM;
   ZtopNextLayer := ZtopNextLayer + GetSoilLayer_i(layeri).Thickness;
   END;

WHILE ((ROUND(MaxMM*1000) > 0) AND (compi > 0) AND (ROUND(GetCompartment_fluxout(compi)*1000) = 0)) DO
   BEGIN
   // Driving force
   IF ((GetCompartment_theta(compi) >= GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100) AND (GetSimulParam_RootNrDF() > 0))
      THEN DrivingForce := 1 - (exp(GetSimulParam_RootNrDF()*Ln(GetCompartment_theta(compi)-GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100))
                                     /exp(GetSimulParam_RootNrDF()*Ln(GetCompartment_FCadj(compi)/100-GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100)))
      ELSE DrivingForce := 1;
   // relative hydraulic conductivity
   ThetaThreshold := (GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100 + GetSoilLayer_i(GetCompartment_Layer(compi)).FC/100)/2;
   IF (GetCompartment_Theta(compi) < ThetaThreshold)
      THEN BEGIN
           IF ((GetCompartment_Theta(compi) <= GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100)
               OR (ThetaThreshold <= GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100))
              THEN Krel := 0
              ELSE Krel := (GetCompartment_Theta(compi) - GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100)/
                           (ThetaThreshold - GetSoilLayer_i(GetCompartment_Layer(compi)).WP/100);
           END
      ELSE Krel := 1;

   // room available to store water
   DTheta := GetCompartment_FCadj(compi)/100 - GetCompartment_Theta(compi);
   IF ((DTheta > 0) AND ((Zbottom - GetCompartment_Thickness(compi)/2) < (GetZiAqua()/100))) THEN
      BEGIN
      // water stored
      DThetaMax := Krel * DrivingForce * MaxMM/(1000*GetCompartment_Thickness(compi));
      IF (DTheta >= DThetaMax)
         THEN BEGIN
              SetCompartment_Theta(compi, GetCompartment_Theta(compi) + DThetaMax);
              CRcomp := DThetaMax*1000*GetCompartment_Thickness(compi)
                        * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
              MaxMM := 0;
              END
         ELSE BEGIN
              SetCompartment_Theta(compi, GetCompartment_FCadj(compi)/100);
              CRcomp := DTheta*1000*GetCompartment_Thickness(compi)
                        * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
              MaxMM := Krel * MaxMM - CRcomp;
              END;
      CRwater := CRwater + CRcomp;
      // salt stored
      SCellAct := ActiveCells(GetCompartment_i(compi));
      SaltCRi := Equiv * CRcomp * GetECiAqua(); // gram/m2
      SetCompartment_Salt(compi, SCellAct, GetCompartment_Salt(compi, SCellAct) + SaltCRi);
      CRsalt := CRsalt + SaltCRi;
      END;
   Zbottom := Zbottom - GetCompartment_Thickness(compi);
   compi := compi - 1;
   IF (compi > 0) THEN
      BEGIN
      LimitMM := MaxCRatDepth(GetSoilLayer_i(GetCompartment_Layer(compi)).CRa,GetSoilLayer_i(GetCompartment_Layer(compi)).CRb,
                              GetSoilLayer_i(GetCompartment_Layer(compi)).InfRate,(Zbottom - GetCompartment_Thickness(compi)/2),(GetZiAqua()/100));
      IF (MaxMM > LimitMM) THEN MaxMM := LimitMM;
      END;
   END;
END; (* calculate_CapillaryRise *)



PROCEDURE calculate_saltcontent(InfiltratedRain, InfiltratedIrrigation, InfiltratedStorage : double);
VAR   SaltIN, SaltOUT, mmIN, DeltaTheta, Theta, SAT, mm1, mm2, Dx, limit, Dif, UL : double;
      Zr, depthi, ECsubdrain, ECcel, DeltaZ,
      ECsw1, ECsw2, ECsw, SM1, SM2, DS1, DS2, DS : double;

      compi, celi, celiM1, Ni : INTEGER;
      ECw : double;
      Salt_temp, Salt2_temp, Depo_temp, Depo2_temp : double;
      Compi_temp : CompartmentIndividual;



    PROCEDURE Mixing (Dif,mm1,mm2 : double;
                      VAR Salt1, Salt2, Depo1, Depo2 : double);
    VAR EC1, EC2, ECmix : double;

    BEGIN
    SaltSolutionDeposit(mm1,Salt1,Depo1);
    EC1 := Salt1/(mm1*Equiv);
    SaltSolutionDeposit(mm2,Salt2,Depo2);
    EC2 := Salt2/(mm2*Equiv);
    ECmix := (EC1*mm1+EC2*mm2)/(mm1+mm2);
    //removed    IN/OUT Ratio
    //IF (EC1 > EC2) THEN DifAdjusted := Dif * 1/SimulParam.SaltRatio
    //               ELSE DifAdjusted := Dif;
    EC1 := EC1 + (ECmix-EC1)*Dif;
    EC2 := EC2 + (ECmix-EC2)*Dif;
    Salt1 := EC1*mm1*Equiv;
    SaltSolutionDeposit(mm1,Salt1,Depo1);
    Salt2 := EC2*mm2*Equiv;
    SaltSolutionDeposit(mm2,Salt2,Depo2);
    END; (* Mixing *)


    PROCEDURE MoveSaltTo(VAR Compx : CompartmentIndividual;
                         celx : INTEGER;
                         DS : double);
    VAR mmx : double;
    BEGIN
    IF (DS >= 0)
       THEN BEGIN
            Compx.Salt[celx] := Compx.Salt[celx] + DS;
            mmx := GetSoilLayer_i(Compx.Layer).Dx*1000*Compx.Thickness
                  * (1 - GetSoilLayer_i(Compx.Layer).GravelVol/100);
            IF (celx = GetSoilLayer_i(Compx.Layer).SCP1) THEN mmx := 2*mmx;
            SaltSolutionDeposit(mmx,Compx.Salt[celx],Compx.Depo[celx]);
            END
       ELSE BEGIN
            celx := GetSoilLayer_i(Compx.Layer).SCP1;
            Compx.Salt[celx] := Compx.Salt[celx] + DS;
            mmx := 2*GetSoilLayer_i(Compx.Layer).Dx*1000*Compx.Thickness
                   * (1 - GetSoilLayer_i(Compx.Layer).GravelVol/100);
            SaltSolutionDeposit(mmx,Compx.Salt[celx],Compx.Depo[celx]);
            mmx := mmx/2;
            WHILE (Compx.Salt[celx] < 0) DO
                  BEGIN
                  Compx.Salt[celx-1] := Compx.Salt[celx-1] + Compx.Salt[celx];
                  Compx.Salt[celx] := 0;
                  celx := celx - 1;
                  SaltSolutionDeposit(mmx,Compx.Salt[celx],Compx.Depo[celx]);
                  END;
            END;
    END; (* MoveSaltTo *)



BEGIN (* calculate_saltcontent *)
mmIN := InfiltratedRain + InfiltratedIrrigation + InfiltratedStorage;

// quality of irrigation water
IF (dayi < GetCrop().Day1)
   THEN ECw := GetIrriECw().PreSeason
   ELSE BEGIN
        ECw := GetSimulation_IrriECw();
        IF (dayi > GetCrop().DayN) THEN ECw := GetIrriECw().PostSeason;
        END;

// initialise salt balance
SaltIN := InfiltratedIrrigation*ECw*Equiv + InfiltratedStorage*GetECstorage()*Equiv;
SaltInfiltr := SaltIN/100; (* salt infiltrated in soil profile kg/ha *)
SaltOut:= 0;


FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    //0. Set compartment parameters
    SAT := (GetSoilLayer_i(GetCompartment_Layer(compi)).SAT)/100;  (* m3/m3 *)
    UL := GetSoilLayer_i(GetCompartment_Layer(compi)).UL; (* m3/m3 *)  (* Upper limit of SC salt cel *)
    Dx := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx;  (* m3/m3 *) (* Size of salts cel (expect last one) *)

    //1. Initial situation before drain and infiltration
    DeltaTheta := mmIN/
                (1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
    Theta := GetCompartment_theta(compi)-DeltaTheta+GetCompartment_fluxout(compi)/(1000*GetCompartment_Thickness(compi));

    //2. Determine active SaltCels and Add IN
    Theta := Theta + DeltaTheta;
    IF (Theta <= UL)
       THEN BEGIN
            celi := 0;
            WHILE (Theta > Dx*celi) DO celi := celi + 1;
            END
       ELSE celi := GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1;
    IF (celi = 0) THEN celi := 1;  // XXX would be best to avoid celi=0 to begin with 
    IF (DeltaTheta > 0) THEN SetCompartment_Salt(compi, celi, GetCompartment_Salt(compi, celi) + SaltIN);

    //3. Mixing
    IF (celi > 1) THEN
       BEGIN
       FOR Ni := 1 TO (celi-1) DO
           BEGIN
           mm1 := Dx*1000*GetCompartment_Thickness(compi)
                  * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           IF (Ni < GetSoilLayer_i(GetCompartment_Layer(compi)).SC)
              THEN mm2 := mm1
              ELSE IF (Theta > SAT)
                      THEN mm2 := (Theta-UL)*1000*GetCompartment_Thickness(compi)
                                  * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100)
                      ELSE mm2 := (SAT-UL)*1000*GetCompartment_Thickness(compi)
                                  * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           Dif := GetSoilLayer_i(GetCompartment_Layer(compi)).SaltMobility[Ni];
           Salt_temp := GetCompartment_Salt(compi, Ni);
           Salt2_temp := GetCompartment_Salt(compi, Ni+1);
           Depo_temp := GetCompartment_Depo(compi, Ni);
           Depo2_temp := GetCompartment_Depo(compi, Ni+1);
           Mixing(Dif,mm1,mm2,Salt_temp,Salt2_temp,Depo_temp,Depo2_temp);
           SetCompartment_Salt(compi, Ni, Salt_temp);
           SetCompartment_Salt(compi, Ni+1, Salt2_temp);
           SetCompartment_Depo(compi, Ni, Depo_temp);
           SetCompartment_Depo(compi, Ni+1, Depo2_temp);
           END;
       END;

    //4. Drain
    SaltOut := 0;
    IF (GetCompartment_fluxout(compi) > 0)
       THEN BEGIN
            DeltaTheta := GetCompartment_fluxout(compi)/
                       (1000*GetCompartment_Thickness(compi)*(1-GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
            WHILE (DeltaTheta > 0) DO
              BEGIN
              IF (celi < GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1) THEN limit := (celi-1)*Dx
                                                                   ELSE limit := UL;
              IF (Theta - DeltaTheta) < limit
                 THEN BEGIN
                      SaltOut := SaltOut + GetCompartment_Salt(compi, celi)
                                         + GetCompartment_Depo(compi, celi);
                      SetCompartment_Salt(compi, celi, 0);
                      mm1 := (Theta - limit)*1000*GetCompartment_Thickness(compi)
                             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
                      IF SaltOut > (GetSimulParam_SaltSolub() * mm1)
                         THEN BEGIN
                              SetCompartment_Depo(compi, celi, SaltOut - (GetSimulParam_SaltSolub() * mm1));
                              SaltOut := (GetSimulParam_SaltSolub() * mm1);
                              END
                         ELSE SetCompartment_Depo(compi, celi, 0);
                      DeltaTheta := DeltaTheta - (Theta-limit);
                      Theta := limit;
                      celi := celi - 1;
                      END
                 ELSE BEGIN
                      SaltOut := SaltOut + (GetCompartment_Salt(compi, celi)
                            + GetCompartment_Depo(compi, celi))*(DeltaTheta/(Theta-limit));
                      SetCompartment_Salt(compi, celi, GetCompartment_Salt(compi, celi) *(1-DeltaTheta/(Theta-limit)));
                      SetCompartment_Depo(compi, celi, GetCompartment_Depo(compi, celi) *(1-DeltaTheta/(Theta-limit)));
                      mm1 := DeltaTheta*1000*GetCompartment_Thickness(compi)
                             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
                      IF SaltOut > (GetSimulParam_SaltSolub() * mm1) THEN
                         BEGIN
                         SetCompartment_Depo(compi, celi, GetCompartment_Depo(compi, celi) + (SaltOut - GetSimulParam_SaltSolub() * mm1));
                         SaltOut := (GetSimulParam_SaltSolub() * mm1);
                         END;
                      DeltaTheta := 0;
                      mm1 := GetSoilLayer_i(GetCompartment_Layer(compi)).DX*1000*GetCompartment_Thickness(compi)
                             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
                      IF (celi = GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1) THEN mm1 := 2*mm1;
                      Salt_temp := GetCompartment_Salt(compi, celi);
                      Depo_temp := GetCompartment_Depo(compi, celi);
                      SaltSolutionDeposit(mm1,Salt_temp,Depo_temp);
                      SetCompartment_Salt(compi, celi, Salt_temp);
                      SetCOmpartment_Depo(compi, celi, Depo_temp);
                      END;
              END;
            END;

    mmIN := GetCompartment_fluxout(compi);
    SaltIN := SaltOUT;
    END;

IF (GetDrain() > 0.001) THEN ECdrain := SaltOUT/(GetDrain()*Equiv);


//5. vertical salt diffusion
celi := ActiveCells(GetCompartment_i(1));
SM2 := GetSoilLayer_i(GetCompartment_Layer(1)).SaltMobility[celi]/4;
ECsw2 := ECswComp(GetCompartment_i(1),(false)); // not at FC
mm2 := GetCompartment_Theta(1)*1000*GetCompartment_Thickness(1)
       * (1 - GetSoilLayer_i(GetCompartment_Layer(1)).GravelVol/100);
FOR compi := 2 TO GetNrCompartments() DO
    BEGIN
    celiM1 := celi;
    SM1 := SM2;
    ECsw1 := ECsw2;
    mm1 := mm2;
    celi :=  ActiveCells(GetCompartment_i(compi));
    SM2 := GetSoilLayer_i(GetCompartment_Layer(compi)).SaltMobility[celi]/4;
    ECsw2 := ECswComp(GetCompartment_i(compi),(false)); // not at FC
    mm2 := GetCompartment_Theta(compi)*1000*GetCompartment_Thickness(compi)
           * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
    ECsw := (ECsw1*mm1+ECsw2*mm2)/(mm1+mm2);
    DS1 := (ECsw1 - (ECsw1+(ECsw-ECsw1)*SM1))*mm1*Equiv;
    DS2 := (ECsw2 - (ECsw2+(ECsw-ECsw2)*SM2))*mm2*Equiv;
    IF (Abs(DS2) < Abs(DS1)) THEN DS := Abs(DS2)
                             ELSE DS := Abs(DS1);
    IF (DS > 0) THEN
       BEGIN
       IF (ECsw1 > ECsw) THEN DS := DS*(-1);
       Compi_temp := GetCompartment_i(compi-1);
       MoveSaltTo(Compi_temp,celiM1,DS);
       SetCompartment_i(compi-1, Compi_temp);
       DS := DS*(-1);
       Compi_temp := GetCompartment_i(compi);
       MoveSaltTo(Compi_temp,celi,DS);
       SetCompartment_i(compi, Compi_temp);
       END;
    END;




//6. Internal salt movement as a result of SubDrain
//SubDrain part of non-effective rainfall (10-day & monthly input)
IF (SubDrain > 0) THEN
   BEGIN
   Zr := GetRootingDepth();
   IF (Zr >= 0) THEN Zr := (GetSimulParam_EvapZmax()/100); // in meter
   compi := 0;
   depthi := 0;
   ECsubdrain := 0;

   //extract
   REPEAT
     compi := compi + 1;
     depthi := depthi + GetCompartment_Thickness(compi);
     If (depthi <= Zr)
        THEN DeltaZ := GetCompartment_Thickness(compi)
        ELSE DeltaZ := GetCompartment_Thickness(compi) - (depthi-Zr);
     celi := ActiveCells(GetCompartment_i(compi));
     IF (celi < GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1)
        THEN mm1 := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx*1000*GetCompartment_Thickness(compi)
                    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100)
        ELSE mm1 := 2*GetSoilLayer_i(GetCompartment_Layer(compi)).Dx*1000*GetCompartment_Thickness(compi)
                    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
     ECcel := GetCompartment_Salt(compi, celi)/(mm1*Equiv);
     ECsubdrain := (ECcel*mm1*(DeltaZ/GetCompartment_Thickness(compi))+ECsubdrain*SubDrain)
                   /(mm1*(DeltaZ/GetCompartment_Thickness(compi))+SubDrain);
     SetCompartment_Salt(compi, celi, (1-(DeltaZ/GetCompartment_Thickness(compi)))*GetCompartment_Salt(compi,celi)
                                      + (DeltaZ/GetCompartment_Thickness(compi))*ECsubdrain*mm1*Equiv);
     Salt_temp := GetCompartment_Salt(compi, celi);
     Depo_temp := GetCompartment_Depo(compi, celi);
     SaltSolutionDeposit(mm1,Salt_temp,Depo_temp);
     SetCompartment_Salt(compi, celi, Salt_temp);
     SetCompartment_Depo(compi, celi, Depo_temp);
   UNTIl (depthi >= Zr) OR (compi >= GetNrCompartments());

   //dump
   IF (compi >= GetNrCompartments())
      THEN BEGIN
           SaltOUT := ECdrain*(GetDrain()*Equiv) + ECsubdrain*SubDrain*Equiv;
           ECdrain := SaltOUT/(GetDrain()*Equiv);
           END
      ELSE BEGIN
           compi := compi + 1;
           celi := ActiveCells(GetCompartment_i(compi));
           IF (celi < GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1)
              THEN mm1 := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx*1000*GetCompartment_Thickness(compi)
                          * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100)
              ELSE mm1 := 2*GetSoilLayer_i(GetCompartment_Layer(compi)).Dx*1000*GetCompartment_Thickness(compi)
                          * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           SetCompartment_Salt(compi, celi, GetCompartment_Salt(compi, celi) + ECsubdrain*SubDrain*Equiv);
           Salt_temp := GetCompartment_Salt(compi, celi);
           Depo_temp := GetCompartment_Depo(compi, celi);
           SaltSolutionDeposit(mm1,Salt_temp,Depo_temp);
           SetCompartment_Salt(compi, celi, Salt_temp);
           SetCompartment_Depo(compi, celi, Depo_temp);
           END;
   END;
END; (* calculate_saltcontent *)





PROCEDURE CheckGermination;
VAR Zroot, WCGermination : double;
    SWCtopSoilConsidered_temp : boolean;

BEGIN
// total root zone is considered
Zroot := GetCrop().RootMin;
SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
DetermineRootZoneWC(Zroot,SWCtopSoilConsidered_temp);
SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
WCGermination := GetRootZoneWC().WP + (GetRootZoneWC().FC - GetRootZoneWC().WP) * (GetSimulParam_TAWGermination()/100);
IF (GetRootZoneWC().Actual < WCGermination)
   THEN BEGIN
        SetSimulation_DelayedDays(GetSimulation_DelayedDays() + 1);
        SetSimulation_SumGDD(0);
        END
   ELSE BEGIN
        SetSimulation_Germinate(true);
        IF (GetCrop().Planting = Seed)
           THEN SetSimulation_ProtectedSeedling(true)
           ELSE SetSimulation_ProtectedSeedling(false);
        END;
END; (* CheckGermination *)




PROCEDURE DetermineCCi(CCxTotal,CCoTotal : double;
                       VAR CCiActual : double);

Const CCdormant = 0.05;

VAR pLeafLLAct , CGCadjusted, CDCadjusted, CCiSen, tTemp,CCxSF,CGCSF,CCxSFCD,KsRED,CCibis : double;
    tFinalCCx : INTEGER;
    WithBeta : BOOLEAN;
    TheSenescenceON : BOOLEAN;
    // test Version 6.2
    KsSen : double;
    Crop_pLeafAct_temp : double;
    Crop_pSenAct_temp : double;
    Crop_CCxAdjusted_temp : double;


PROCEDURE DetermineCGCadjusted(VAR CGCadjusted : double);
VAR Wrelative,MaxVal : double;
    KsLeaf : double;
    SWCeffectiveRootZone,FCeffectiveRootZone,WPeffectiveRootZone : double;

BEGIN
// determine FC and PWP
IF (GetSimulation_SWCtopSoilConsidered() = true)
   THEN BEGIN // top soil is relative wetter than total root zone
        SWCeffectiveRootZone := GetRootZoneWC().ZtopAct;
        Wrelative := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP);
        FCeffectiveRootZone := GetRootZoneWC().ZtopFC;
        WPeffectiveRootZone := GetRootZoneWC().ZtopWP;
        END
   ELSE BEGIN // total rootzone is wetter than top soil
        SWCeffectiveRootZone := GetRootZoneWC().Actual;
        Wrelative := (GetRootZoneWC().FC - GetRootZoneWC().Actual)/(GetRootZoneWC().FC - GetRootZoneWC().WP);
        FCeffectiveRootZone := GetRootZoneWC().FC;
        WPeffectiveRootZone := GetRootZoneWC().WP;
        END;

// Canopy stress and effect of soil water stress on CGC
IF (SWCeffectiveRootZone >= FCeffectiveRootZone)
   THEN BEGIN
        CGCadjusted := CGCSF;
        StressLeaf := 0;
        END
   ELSE IF (SWCeffectiveRootZone <= WPeffectiveRootZone)
           THEN BEGIN
                CGCadjusted := 0;
                StressLeaf := 100;
                END
           ELSE BEGIN
                IF (Wrelative <= GetCrop().pLeafAct)
                   THEN BEGIN
                        CGCadjusted := CGCSF;
                        StressLeaf := 0;
                        END
                   ELSE IF (Wrelative >= pLeafLLAct)
                           THEN BEGIN
                                CGCadjusted := 0;
                                StressLeaf := 100;
                                END
                           ELSE BEGIN
                                KsLeaf := KsAny(Wrelative,GetCrop().pLeafAct,pLeafLLAct,GetCrop().KsShapeFactorLeaf);
                                CGCadjusted := CGCSF * KsLeaf;
                                StressLeaf := 100 * (1 - KsLeaf);
                                END;
                END;

// effect of transfer of assimilates on CGC
IF (CGCadjusted > 0.000001) // CGC can be adjusted
   AND (  ((GetCrop_subkind() = Forage) AND ((StorageON = true) OR (MobilizationON = true))) // transfer assimilates
        OR (CGCadjustmentAfterCutting = true)) // increase of Canopy development after Cutting
   THEN BEGIN
        // decrease CGC during storage
        IF ((GetCrop_subkind() = Forage) AND (StorageON = true))
           THEN CGCadjusted := CGCadjusted * (1 - FracAssim);
        // increase CGC after cutting
        IF ((CGCadjustmentAfterCutting = true) AND (StorageON = false))
           THEN CGCadjusted := CGCadjusted * (1 + GetManagement_Cuttings_CGCPlus()/100);
        // increase CGC during mobilization
        IF ((GetCrop_subkind() = Forage) AND (MobilizationON = true) AND (CGCadjustmentAfterCutting = false))
           THEN BEGIN
                IF ((CCxSFCD <=0) OR (CCiPrev >= 0.9*CCxSFCD))
                   THEN MaxVal := 0
                   ELSE BEGIN
                        MaxVal := (1- CCiPrev/(0.9*CCxSFCD));
                        IF (MaxVal > 1) THEN MaxVal := 1;
                        IF (MaxVal < 0) THEN MaxVal := 0;
                        END;
                IF (MaxVal > (FracAssim/2)) THEN MaxVal := FracAssim/2;
                CGCadjusted := CGCadjusted * (1 + Maxval);
                END;
        END;

END; (* DetermineCGCadjusted *)


PROCEDURE DetermineCDCadjustedWaterStress(VAR CDCadjusted,KsSen : double);
VAR Wrelative : double;
    //KsSen : double;
    pSenLL : double;
    pSenAct : double;
    WithBeta : BOOLEAN;

BEGIN
pSenLL := 0.999; //WP
IF (GetSimulation_SWCtopSoilConsidered() = true) // top soil is relative wetter than total root zone
   THEN Wrelative := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP) // top soil
   ELSE Wrelative := (GetRootZoneWC().FC - GetRootZoneWC().Actual)/(GetRootZoneWC().FC - GetRootZoneWC().WP); // total root zone
WithBeta := false;
AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,pSenAct);
IF (Wrelative <= pSenAct)
   THEN BEGIN
        CDCadjusted := 0.001; // extreme small decline
        StressSenescence := 0;
        KsSen := 1;
        END
   ELSE IF (Wrelative >= pSenLL)
           THEN BEGIN
                //CDCadjusted := CDCTotal*CCxSFCD/CCxTotal; // full speed
                CDCadjusted := CDCTotal*(CCxSFCD+2.29)/(CCxTotal+2.29); // full speed
                StressSenescence := 100;
                KsSen := 0;
                END
           ELSE BEGIN
                KsSen := KsAny(Wrelative,pSenAct,pSenLL,GetCrop().KsShapeFactorSenescence);
                IF (KsSen > 0.000001)
                   THEN BEGIN
                        //CDCadjusted := CDCTotal * (CCxSFCD/CCxTotal) * (1 - Exp(8*Ln(KsSen)));
                        CDCadjusted := CDCTotal * ((CCxSFCD+2.29)/(CCxTotal+2.29)) * (1 - Exp(8*Ln(KsSen)));
                        StressSenescence := 100 * (1 - KsSen);
                        END
                   ELSE BEGIN
                        CDCadjusted := 0;
                        StressSenescence := 0;
                        END;
                END;
END; (* DetermineCDCadjustedWaterStress *)





FUNCTION RequiredTimeNew(CCiToFind,CCo,CCx,CGCadjusted : double) : double;
VAR CGCx : double;
BEGIN
// Only when VirtualTime > 1
// and CCx < CCiToFind
//1. CGCx to reach CCiToFind on previous day (= VirtualTime -1 )
IF (CCiToFind <= CCx/2)
   THEN CGCx := (Ln(CCiToFind/CCo))/VirtualTimeCC
   ELSE CGCx := (Ln((0.25*CCx*CCx/CCo)/(CCx-CCiToFind)))/VirtualTimeCC;
//2. Required time
RequiredTimeNew := VirtualTimeCC * CGCx/CGCadjusted;
END; (* RequiredTimeNew *)



FUNCTION CCatTime(tfictive : double;
                  CCoGiven,CGCGiven,CCxGiven : double)  : double;
VAR CCi : double;
BEGIN
CCi := CCoGiven * EXP(CGCGiven * tfictive);
IF (CCi > CCxGiven/2) THEN CCi := CCxGiven - 0.25 * (CCxGiven/CCoGiven) * CCxGiven * EXP(-CGCGiven*tfictive);
CCatTime := CCi;
END; (* CCatTime *)


PROCEDURE DetermineCCxAdjusted(VAR CCxAdjusted : double);  //canopy development
VAR tfictive : double;
BEGIN
//1. find time (tfictive) required to reach CCiPrev (CCi of previous day) with CGCadjusted
tfictive := RequiredTimeNew(CCiprev,GetCrop().CCoAdjusted,CCxSF,CGCadjusted);

//2. Get CCxadjusted (reached at end of stretched crop development)
IF (tfictive > 0)
   THEN BEGIN
        tfictive := tfictive + (tFinalCCx - VirtualTimeCC);
        CCxAdjusted := CCatTime(tfictive,GetCrop().CCoAdjusted,CGCadjusted,CCxSF);
        END
   ELSE CCxAdjusted := undef_double; // this means CCiActual := CCiPrev
END; (* DetermineCCxAdjusted *)


PROCEDURE GetNewCCxandCDC(CCiPrev,CDC,CCx : double;
                          VAR CCxAdjusted,CDCadjusted : double);
BEGIN
//CCxAdjusted := CCiPrev/(1-0.05*(exp((VirtualTimeCC-GetCrop().DaysToSenescence)*CDC/CCX)-1));
CCxAdjusted := CCiPrev/(1-0.05*(exp((VirtualTimeCC-GetCrop().DaysToSenescence)*CDC*3.33/(CCX+2.29))-1));
//CDCadjusted := CDC * CCxAdjusted/CCx;
CDCadjusted := CDC * (CCxAdjusted+2.29)/(CCx+2.29);
END; (* GetNewCCxandCDC *)



BEGIN (* DetermineCCi *)
IF ((VirtualTimeCC < GetCrop().DaysToGermination) OR (VirtualTimeCC > (GetCrop().DayN-GetCrop().Day1)))
   THEN CCiActual := 0
   ELSE BEGIN // growing season (once germinated)
        //1. find some parameters
        CGCSF := GetCrop().CGC*(1-GetSimulation_EffectStress_RedCGC()/100);
        CGCadjusted := CGCSF;
        CCxSF := CCxTotal*(1-GetSimulation_EffectStress_RedCCX()/100);
        // maximum canopy cover than can be reached (considering soil fertility/salinity, weed stress)
        IF (VirtualTimeCC <= GetCrop().DaysToFullCanopySF)
           THEN CCxSFCD := CCxSF // no correction before maximum canopy is reached
           ELSE BEGIN
                IF (VirtualTimeCC < GetCrop().DaysToSenescence)
                   THEN BEGIN
                        CCxSFCD := CCiNoWaterStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                            GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,GetCrop().GDDaysToGermination,
                            GetCrop().GDDaysToFullCanopySF,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                            CCoTotal,CCxTotal,GetCrop().CGC,GetCrop().GDDCGC,CDCTotal,GDDCDCTotal,GetSimulation_SumGDD(),(1),
                            GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),GetSimulation_EffectStress_CDecline(),
                            GetCrop().ModeCycle);
                        END
                   ELSE CCxSFCD := CCxSF - (GetSimulation_EffectStress_CDecline()/100) * (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF);
                IF (CCxSFCD < 0) THEN CCxSFCD := 0;
                END;
        StressLeaf := undef_int;
        IF (VirtualTimeCC = GetCrop().DaysToGermination) THEN CCiPrev := CCoTotal;

        // time of potentional vegetative growth
        tFinalCCx := GetCrop().DaysToSenescence; // undeterminant crop
        IF ((GetCrop_subkind() = Grain) AND (GetCrop().DeterminancyLinked = true)) THEN  // determinant crop
           BEGIN //reduce tFinalCC in f(determinancy of crop)
           IF (GetCrop().DaysToCCini <> 0)
              THEN BEGIN  // regrowth  (adjust to slower time)
                   tFinalCCx := GetCrop().DaysToFullCanopy
                      + ROUND(DayFraction * ( (GetCrop().DaysToFlowering + (GetCrop().LengthFlowering/2)
                      - GetSimulation_DelayedDays())+Tadj+GetCrop().DaysToGermination - GetCrop().DaysToFullCanopy));
                   END
              ELSE BEGIN // sown or transplant
                   tFinalCCx := GetCrop().DaysToFlowering + ROUND(GetCrop().LengthFlowering/2);
                   END;
           IF (tFinalCCx > GetCrop().DaysToSenescence) THEN tFinalCCx := GetCrop().DaysToSenescence;
           END;

        // Crop.pLeafAct and Crop.pSenAct for plotting root zone depletion in RUN
        Crop_pLeafAct_temp := GetCrop().pLeafAct;
        AdjustpLeafToETo(GetETo(),Crop_pLeafAct_temp,pLeafLLAct);
        SetCrop_pLeafAct(Crop_pLeafAct_temp);
        WithBeta := true;
        Crop_pSenAct_temp := GetCrop().pSenAct;
        AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,Crop_pSenAct_temp);
        SetCrop_pSenAct(Crop_pSenAct_temp);

        //2. Canopy can still develop (stretched to tFinalCCx)
        IF (VirtualTimeCC < tFinalCCx)
           THEN BEGIN //Canopy can stil develop (stretched to tFinalCCx)
                IF ((CCiPrev <= GetCrop().CCoAdjusted)
                   OR (VirtualTimeCC <= 1)
                   OR ((GetSimulation_ProtectedSeedling() = true) AND (CCiPrev <= (1.25 * CCoTotal))))
                   //2.a first day or very small CC as a result of senescence (no adjustment for leaf stress)
                   THEN BEGIN
                        CGCadjustmentAfterCutting := false;
                        IF (GetSimulation_ProtectedSeedling() = true)
                           THEN BEGIN
                                CCiActual :=
                                CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                                       GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                                       GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                                       CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                                                       GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                                       GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                IF (CCiActual > (1.25 * CCoTotal)) THEN SetSimulation_ProtectedSeedling(false);
                                END
                           ELSE BEGIN
                                // this results in CC increase when during senescence CC becomes smaller than CCini)
                                IF (VirtualTimeCC = 1)
                                   THEN CCiActual := GetCrop().CCoAdjusted*Exp(CGCSF*2)
                                   ELSE CCiActual := GetCrop().CCoAdjusted*Exp(CGCSF*1);
                                END;
                        //CCiActual := GetCrop().CCoAdjusted*Exp(CGCSF*1);
                        END
                   //2.b CC > CCo
                   ELSE BEGIN
                        IF (CCiPrev < 0.97999*CCxSF)
                           THEN BEGIN
                                DetermineCGCadjusted(CGCadjusted);
                                IF (CGCadjusted > 0.00000001)
                                   THEN BEGIN // CGCSF or CGCadjusted > 0
                                        Crop_CCxAdjusted_temp := GetCrop().CCxAdjusted;
                                        DetermineCCxAdjusted(Crop_CCxAdjusted_temp);
                                        SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp);
                                        IF (GetCrop().CCxAdjusted < 0)
                                           THEN CCiActual := CCiPrev
                                           ELSE IF (Abs(CCiPrev - 0.97999*CCxSF) < 0.001)
                                                   //THEN CCiActual := CCxSF // not correct since this will become CCxWithered and Transpiration will drop
                                                   THEN CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                                                         GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                                                         GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                                                         CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                                                                         GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                                                         GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX())
                                                   ELSE BEGIN
                                                        tTemp := RequiredTimeNew(CCiprev,GetCrop().CCoAdjusted,GetCrop().CCxAdjusted,CGCadjusted);
                                                        IF (tTemp < 0)
                                                           THEN CCiActual := CCiPrev
                                                           ELSE BEGIN
                                                                tTemp := tTemp+1;
                                                                CCiActual := CCatTime(tTemp,GetCrop().CCoAdjusted,CGCadjusted,GetCrop().CCxAdjusted);
                                                                END;
                                                        END;
                                        END
                                   ELSE BEGIN // CGCadjusted = 0 - too dry for leaf expansion
                                        CCiActual := CCiPrev;
                                        IF (CCiActual > GetCrop().CCoAdjusted)
                                           THEN SetCrop_CCoAdjusted(CCoTotal)
                                           ELSE SetCrop_CCoAdjusted(CCiActual);
                                        END;
                                END
                           ELSE BEGIN
                                CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                                     GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                SetCrop_CCoAdjusted(CCoTotal);
                                StressLeaf := -33; // maximum canopy is reached;
                                CGCadjustmentAfterCutting := false; // no increase anymore of CGC after cutting
                                END;
                        IF (CCiActual > CCxSFCD) THEN
                           BEGIN
                           CCiActual := CCxSFCD;
                           StressLeaf := -33; // maximum canopy is reached;
                           CGCadjustmentAfterCutting := false; // no increase anymore of CGC after cutting
                           END;
                        END;
                SetCrop_CCxAdjusted(CCiActual);
                END

        //3. Canopy can no longer develop (Mid-season (from tFinalCCx) or Late season stage)
           ELSE BEGIN
                StressLeaf := -33; // maximum canopy is reached;
                CGCadjustmentAfterCutting := false; // no increase anymore of CGC after cutting
                IF (GetCrop().CCxAdjusted < 0)
                   THEN SetCrop_CCxAdjusted(CCiPrev);

                IF (VirtualTimeCC < GetCrop().DaysToSenescence)
                // mid-season
                   THEN BEGIN
                        IF (GetCrop().CCxAdjusted > 0.97999*CCxSF)
                           THEN BEGIN
                                CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                                     GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                SetCrop_CCxAdjusted(CCiActual);
                                END
                           ELSE CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,(GetCrop().CCxAdjusted/(1-GetSimulation_EffectStress_RedCCx()/100)),GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCTotal,
                                     GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                        IF (CCiActual > CCxSFCD) THEN CCiActual := CCxSFCD;
                        END
                // late season
                   ELSE BEGIN
                        StressSenescence := undef_int; // to avoid display of zero stress in late season
                        IF (GetCrop().CCxAdjusted > CCxSFCD) THEN SetCrop_CCxAdjusted(CCxSFCD);
                        IF (GetCrop().CCxAdjusted < 0.01)
                           THEN CCiActual := 0
                           ELSE BEGIN  // calculate CC in late season
                                // CCibis = CC which canopy declines (soil fertility/salinity stress) further in late season
                                CCibis := CCxSF - (GetSimulation_EffectStress_CDecline()/100)
                                          * (exp(2*Ln((VirtualTimeCC+GetSimulation_DelayedDays()+1) - GetCrop().DaysToFullCanopySF))
                                                /(GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF));
                                IF (CCibis < 0)
                                   THEN CCiActual := 0
                                   ELSE BEGIN
                                        // CCiActual = CC with natural senescence in late season
                                        Crop_CCxAdjusted_temp := GetCrop().CCxAdjusted;
                                        CDCadjusted := GetCDCadjustedNoStressNew(CCxTotal,CDCTotal,Crop_CCxAdjusted_temp);
                                        SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp);
                                        IF ((VirtualTimeCC+GetSimulation_DelayedDays()+1)
                                             < (GetCrop().DaysToSenescence + LengthCanopyDecline(GetCrop().CCxAdjusted,CDCadjusted)))
                                            THEN BEGIN
                                                 CCiActual := GetCrop().CCxAdjusted *
                                                   (1 - 0.05 * (exp(((VirtualTimeCC+GetSimulation_DelayedDays()+1)-GetCrop().DaysToSenescence)
                                                                 *3.33* CDCadjusted/(GetCrop().CCxAdjusted + 2.29))-1));
                                                 // CCiActual becomes CCibis, when canopy decline is more severe
                                                 IF (CCibis < CCiActual) THEN CCiActual := CCibis;
                                                 END
                                            ELSE CCiActual := 0;
                                        END;
                                END;
                        END; // late season
                END; //3. Canopy can no longer develop (Mid-season (from tFinalCCx) or Late season stage)



        //4. Canopy senescence due to water stress ?
        IF ((VirtualTimeCC < GetCrop().DaysToSenescence) // not yet late season stage
           OR (TimeSenescence > 0)) // in late season with ongoing early senesence (TimeSenescence in days)
           THEN BEGIN
                StressSenescence := 0;
                WithBeta := true;
                Crop_pSenAct_temp := GetCrop().pSenAct;
                AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,Crop_pSenAct_temp);
                SetCrop_pSenAct(Crop_pSenAct_temp);
                KsRED := 1;  // effect of soil salinity on the threshold for senescence
                IF (GetSimulation_SWCtopSoilConsidered() = true)
                   THEN BEGIN // top soil is relative wetter than total root zone
                           IF ((GetRootZoneWC().ZtopAct < (GetRootZoneWC().ZtopFC - GetCrop().pSenAct*KsRED*(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP)))
                           AND (GetSimulation_ProtectedSeedling() = false))
                               THEN TheSenescenceON := true
                               ELSE TheSenescenceON := false;
                        END
                   ELSE BEGIN
                        IF ((GetRootZoneWC().Actual < (GetRootZoneWC().FC - GetCrop().pSenAct*KsRED*(GetRootZoneWC().FC - GetRootZoneWC().WP)))
                            AND (GetSimulation_ProtectedSeedling() = false))
                               THEN TheSenescenceON := true
                               ELSE TheSenescenceON := false;
                        END;

                IF TheSenescenceON
                   THEN BEGIN // CanopySenescence
                        CGCadjustmentAfterCutting := false;
                        SetSimulation_EvapLimitON(true); // consider withered crop when not yet in late season
                        IF (TimeSenescence = 0) THEN CCiTopEarlySen := CCiActual; // CC before canopy decline
                        TimeSenescence := TimeSenescence + 1;  // add 1 day
                        DetermineCDCadjustedWaterStress(CDCadjusted,KsSen);
                        IF (CCiTopEarlySen < 0.001)
                           THEN BEGIN
                                IF ((GetSimulation_SumEToStress() > GetCrop().SumEToDelaySenescence)
                                   OR (GetCrop().SumEToDelaySenescence = 0))
                                   THEN CCiSen := 0 // no crop anymore
                                   ELSE BEGIN
                                        IF (CCdormant > GetCrop().CCo)
                                           THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                           ELSE CCiSen := GetCrop().CCo;
                                        END;
                                END
                           ELSE BEGIN
                                IF (((TimeSenescence*CDCTotal*3.33)/(CCiTopEarlySen+2.29) > 100) // e power too large and in any case CCisen << 0
                                       OR (CCiprev >= 1.05 * CCiTopEarlySen)) // Ln of negative or zero value
                                      THEN BEGIN
                                           IF ((GetSimulation_SumEToStress() > GetCrop().SumEToDelaySenescence)
                                              OR (GetCrop().SumEToDelaySenescence = 0))
                                              THEN CCiSen := 0 // no crop anymore
                                              ELSE BEGIN
                                                   IF (CCdormant > GetCrop().CCo)
                                                      THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                                      ELSE CCiSen := GetCrop().CCo;
                                                   END;
                                           END
                                      ELSE BEGIN
                                           // CDC is adjusted to degree of stress
                                           // time required to reach CCiprev with CDCadjusted
                                           tTemp := (LN(1 + (1-CCiprev/CCiTopEarlySen)/0.05))/(CDCadjusted*3.33/(CCiTopEarlySen+2.29));
                                           // add 1 day to tTemp and calculate CCiSen with CDCadjusted
                                           CCiSen := CCiTopEarlySen * (1 - 0.05*(exp((tTemp+1)*CDCadjusted*3.33/(CCiTopEarlySen+2.29))-1));
                                           END;

                                IF (CCiSen < 0) THEN CCiSen := 0;
                                IF ((GetCrop().SumEToDelaySenescence > 0) AND (GetSimulation_SumEToStress() <= GetCrop().SumEToDelaySenescence)) THEN
                                   BEGIN
                                   IF ((CCiSen < GetCrop().CCo) OR (CCiSen < CCdormant)) THEN
                                      BEGIN
                                      IF (CCdormant > GetCrop().CCo)
                                         THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                         ELSE CCiSen := GetCrop().CCo;
                                      END;
                                   END;
                                END;
                        IF (VirtualTimeCC < GetCrop().DaysToSenescence)
                           THEN BEGIN // before late season
                                IF (CCiSen > CCxSFCD) THEN CCiSen := CCxSFCD;
                                CCiActual := CCiSen;
                                IF (CCiActual > CCiPrev) THEN CCiActual := CCiPrev; // to avoid jump in CC
                                                         // when CGCadjusted increases as a result of watering
                                SetCrop_CCxAdjusted(CCiActual);
                                IF (CCiActual < CCoTotal)
                                   THEN SetCrop_CCoAdjusted(CCiActual)
                                   ELSE SetCrop_CCoAdjusted(CCoTotal);
                                END
                           ELSE BEGIN // in late season
                                IF (CCiSen < CCiActual) THEN CCiActual := CCiSen;
                                   //SetCrop_CCxAdjusted(CCiTopEarlySen);
                                   //SetCrop_CCxAdjusted(CCiActual);
                                END;
                        //IF (CCiSen <= GetCrop().CCo) THEN Simulation.SumEToStress := Simulation.SumEToStress + ETo;
                        IF ((ROUND(10000*CCiSen) <= (10000*CCdormant))
                           OR (ROUND(10000*CCiSen) <= ROUND(10000*GetCrop().CCo)))
                              THEN SetSimulation_SumEToStress(GetSimulation_SumEToStress() + GetETo());
                        END
                   ELSE BEGIN // no water stress, resulting in canopy senescence
                        TimeSenescence := 0;  // No early senescence or back to normal
                        StressSenescence := 0;
                        SetSimulation_SumEToStress(0);
                        IF ((VirtualTimeCC > GetCrop().DaysToSenescence) AND (CCiActual > CCiprev)) THEN
                           BEGIN // result of a rewatering in late season of an early declining canopy
                           Crop_CCxAdjusted_temp := GetCrop().CCxAdjusted;
                           GetNewCCxandCDC(CCiprev,CDCTotal,CCxSF,Crop_CCxAdjusted_temp,CDCadjusted);
                           SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp);
                           CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,(GetCrop().CCxAdjusted/(1-GetSimulation_EffectStress_RedCCx()/100)),
                                     GetCrop().CGC,CDCadjusted,GetCrop().GDDCGC,GDDCDCTotal,
                                     GetSimulation_SumGDD(),GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                           //IF (CCiActual > CCxSFCD) THEN CCiActual := CCxSFCD; // added March 2017
                           END;
                        END;
                END;

        //5. Adjust GetCrop().CCxWithered - required for correction of Transpiration of dying green canopy
        IF (CCiActual > GetCrop().CCxWithered) THEN SetCrop_CCxWithered(CCiActual);
        //IF (GetCrop().CCxWithered > CCxSFCD) THEN SetCrop_CCxWithered(CCxSFCD); - OUT 15/10/2008

        // 6. correction for late-season stage for rounding off errors
         IF (VirtualTimeCC > GetCrop().DaysToSenescence) THEN
            IF (CCiActual > CCiprev) THEN CCiActual := CCiprev;

        // 7. no crop as a result of fertiltiy and/or water stress
        IF (ROUND(1000*CCiActual) <= 0) THEN NoMoreCrop := true;

        // test
        TESTVAL := CGCadjusted;
        END;
END; (* DetermineCCi *)





PROCEDURE DetermineCCiGDD(CCxTotal,CCoTotal : double;
                          VAR CCiActual : double);
Const CCdormant = 0.05;

VAR pLeafLLAct , GDDCGCadjusted, GDDCDCadjusted, CCiSen, GDDtTemp,
    CCxSF,CGCGDDSF,CCxSFCD,RatDGDD,KsRED,CCibis : double;
    GDDtFinalCCx : INTEGER;
    WithBeta : BOOLEAN;
    TheSenescenceON : BOOLEAN;

    // test Version 6.2
    KsSen : double;
    Crop_pLeafAct_temp : double;
    Crop_pSenAct_temp : double;
    Crop_CCxAdjusted_temp : double;


PROCEDURE DetermineGDDCGCadjusted(VAR GDDCGCadjusted : double);
VAR Wrelative : double;
    KsLeaf,MaxVal : double;
    SWCeffectiveRootZone,FCeffectiveRootZone,WPeffectiveRootZone : double;
BEGIN
// determine FC and PWP
IF (GetSimulation_SWCtopSoilConsidered() = true)
   THEN BEGIN // top soil is relative wetter than total root zone
        SWCeffectiveRootZone := GetRootZoneWC().ZtopAct;
        Wrelative := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP); // top soil
        FCeffectiveRootZone := GetRootZoneWC().ZtopFC;
        WPeffectiveRootZone := GetRootZoneWC().ZtopWP;
        END
   ELSE BEGIN
        SWCeffectiveRootZone := GetRootZoneWC().Actual;
        Wrelative := (GetRootZoneWC().FC - GetRootZoneWC().Actual)/(GetRootZoneWC().FC - GetRootZoneWC().WP); // total root zone
        FCeffectiveRootZone := GetRootZoneWC().FC;
        WPeffectiveRootZone := GetRootZoneWC().WP;
        END;

// Canopy stress and effect of water stress on CGCGDD
IF (SWCeffectiveRootZone >= FCeffectiveRootZone)
   THEN BEGIN
        GDDCGCadjusted := CGCGDDSF;
        StressLeaf := 0;
        END
   ELSE BEGIN
        IF (SWCeffectiveRootZone <= WPeffectiveRootZone)
           THEN BEGIN
                GDDCGCadjusted := 0;
                StressLeaf := 100;
                END
           ELSE BEGIN
                IF (Wrelative <= GetCrop().pLeafAct)
                   THEN BEGIN
                        GDDCGCadjusted := CGCGDDSF;
                        StressLeaf := 0;
                        END
                   ELSE IF (Wrelative >= pLeafLLAct)
                           THEN BEGIN
                                GDDCGCadjusted := 0;
                                StressLeaf := 100;
                                END
                           ELSE BEGIN
                                KsLeaf := KsAny(Wrelative,GetCrop().pLeafAct,pLeafLLAct,GetCrop().KsShapeFactorLeaf);
                                GDDCGCadjusted := CGCGDDSF * KsLeaf;
                                StressLeaf := 100 * (1 - KsLeaf);
                                END;
                END;
        END;

// effect of transfer of assimilates on CGCGDD
IF (GDDCGCadjusted > 0.000001) // CGCGDD can be adjusted
   AND (  ((GetCrop_subkind() = Forage) AND ((StorageON = true) OR (MobilizationON = true))) // transfer assimilates
        OR (CGCadjustmentAfterCutting = true)) // increase of Canopy development after Cutting
   THEN BEGIN
        // decrease CGC during storage
        IF ((GetCrop_subkind() = Forage) AND (StorageON = true))
           THEN GDDCGCadjusted := GDDCGCadjusted * (1 - FracAssim);
        // increase CGC after cutting
        IF ((CGCadjustmentAfterCutting = true) AND (StorageON = false))
           THEN GDDCGCadjusted := GDDCGCadjusted * (1 + GetManagement_Cuttings_CGCPlus()/100);
        // increase CGC during mobilization
        IF ((GetCrop_subkind() = Forage) AND (MobilizationON = true) AND (CGCadjustmentAfterCutting = false))
           THEN BEGIN
                IF ((CCxSFCD <=0) OR (CCiPrev >= 0.9*CCxSFCD))
                   THEN MaxVal := 0
                   ELSE BEGIN
                        MaxVal := (1- CCiPrev/(0.9*CCxSFCD));
                        IF (MaxVal > 1) THEN MaxVal := 1;
                        IF (MaxVal < 0) THEN MaxVal := 0;
                        END;
                IF (MaxVal > (FracAssim/2)) THEN MaxVal := FracAssim/2;
                GDDCGCadjusted := GDDCGCadjusted * (1 + Maxval);
                END;
        END;
END; (* DetermineGDDCGCadjusted *)




FUNCTION RequiredGDD(CCiToFind,CCo,CCx,GDDCGCadjusted : double) : double;
VAR GDDCGCx : double;
BEGIN
// Only when SumGDDadj > GDDayi
// and CCx < CCiToFind
//1. GDDCGCx to reach CCiToFind on previous day (= SumGDDadj - GDDayi )
IF (CCiToFind <= CCx/2)
   THEN GDDCGCx := (Ln(CCiToFind/CCo))/(SumGDDadjCC-GDDayi)
   ELSE GDDCGCx := (Ln((0.25*CCx*CCx/CCo)/(CCx-CCiToFind)))/(SumGDDadjCC-GDDayi);
//2. Required GDD
RequiredGDD := (SumGDDadjCC-GDDayi) * GDDCGCx/GDDCGCadjusted;
END; (* RequiredGDD *)



FUNCTION CCatGDDTime(GDDtfictive : double;
                     CCoGiven,GDDCGCGiven,CCxGiven : double)  : double;
VAR CCi : double;
BEGIN
CCi := CCoGiven * EXP(GDDCGCGiven * GDDtfictive);
IF (CCi > CCxGiven/2) THEN CCi := CCxGiven - 0.25 * (CCxGiven/CCoGiven) * CCxGiven * EXP(-GDDCGCGiven*GDDtfictive);
CCatGDDTime := CCi;
END; (* CCatGDDTime *)


PROCEDURE DetermineCCxAdjusted(VAR CCxAdjusted : double);  //canopy development
VAR GDDtfictive : double;
BEGIN
//1. find time (GDDtfictive) required to reach CCiPrev (CCi of previous day) with GDDCGCadjusted
GDDtfictive := RequiredGDD(CCiprev,GetCrop().CCoAdjusted,CCxSF,GDDCGCadjusted);

//2. Get CCxadjusted (reached at end of stretched crop development)
IF (GDDtfictive > 0)
   THEN BEGIN
        GDDtfictive := GDDtfictive + (GDDtFinalCCx - SumGDDadjCC) + GDDayi;
        CCxAdjusted := CCatGDDTime(GDDtfictive,GetCrop().CCoAdjusted,GDDCGCadjusted,CCxSF);
        END
   ELSE CCxAdjusted := undef_double; // this means CCiActual := CCiPrev
END; (* DetermineCCxAdjusted *)



FUNCTION GetGDDCDCadjustedNoStress(CCx,GDDCDC,CCxAdjusted : double) : double;
VAR GDDCDCadjusted : double;
BEGIN
//GDDCDCadjusted := GDDCDC * (CCxadjusted/CCx);
GDDCDCadjusted := GDDCDC * ((CCxadjusted+2.29)/(CCx+2.29));
GetGDDCDCadjustedNoStress := GDDCDCadjusted;
END; (* GetGDDCDCadjustedNoStress *)


PROCEDURE DetermineGDDCDCadjustedWaterStress(VAR GDDCDCadjusted,KsSen : double);
VAR Wrelative : double;
    //KsSen : double;
    pSenLL : double;
    pSenAct : double;
    WithBeta : BOOLEAN;

BEGIN
pSenLL := 0.999; //WP
IF (GetSimulation_SWCtopSoilConsidered() = true) // top soil is relative wetter than total root zone
   THEN Wrelative := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP) // top soil
   ELSE Wrelative := (GetRootZoneWC().FC - GetRootZoneWC().Actual)/(GetRootZoneWC().FC - GetRootZoneWC().WP); // total root zone

WithBeta := false;
AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,pSenAct);
IF (Wrelative <= pSenAct)
   THEN BEGIN
        GDDCDCadjusted := 0.0001; // extreme small decline
        StressSenescence := 0;
        KsSen := 1;
        END
   ELSE IF (Wrelative >= pSenLL)
           THEN BEGIN
                //GDDCDCadjusted := GDDCDCTotal * (CCxSFCD/CCxTotal); // full speed
                GDDCDCadjusted := GDDCDCTotal * ((CCxSFCD+2.29)/(CCxTotal+2.29)); // full speed
                StressSenescence := 100;
                KsSen := 0;
                END
           ELSE BEGIN
                KsSen := KsAny(Wrelative,pSenAct,pSenLL,GetCrop().KsShapeFactorSenescence);
                IF (KsSen > 0.000001)
                   THEN BEGIN
                        //GDDCDCadjusted := GDDCDCTotal * (CCxSFCD/CCxTotal) * (1 - Exp(8*Ln(KsSen)));
                        GDDCDCadjusted := GDDCDCTotal * ((CCxSFCD+2.29)/(CCxTotal+2.29)) * (1 - Exp(8*Ln(KsSen)));
                        StressSenescence := 100 * (1-KsSen);
                        END
                   ELSE BEGIN
                        GDDCDCadjusted := 0;
                        StressSenescence := 0;
                        END;
                END;
END; (* DetermineGDDCDCadjustedWaterStress *)




PROCEDURE GetNewCCxandGDDCDC(CCiPrev,GDDCDC,CCx : double;
                             VAR CCxAdjusted,GDDCDCadjusted : double);
BEGIN
//CCxAdjusted := CCiPrev/(1-0.05*(exp((SumGDDadjCC-GDDayi-GetCrop().GDDaysToSenescence)*GDDCDC/CCX)-1));
CCxAdjusted := CCiPrev/(1-0.05*(exp((SumGDDadjCC-GDDayi-GetCrop().GDDaysToSenescence)*GDDCDC*3.33/(CCX+2.29))-1));
//GDDCDCadjusted := GDDCDC * CCxAdjusted/CCx;
GDDCDCadjusted := GDDCDC * (CCxAdjusted+2.29)/(CCx+2.29);
END; (* GetNewCCxandCDC *)



BEGIN (* DetermineCCiGDD *)
IF ((SumGDDadjCC <= GetCrop().GDDaysToGermination) OR (ROUND(SumGDDadjCC) > GetCrop().GDDaysToHarvest))
   THEN CCiActual := 0
   ELSE BEGIN // growing season (once germinated)
        //1. find some parameters
        CGCGDDSF := GetCrop().GDDCGC *(1-GetSimulation_EffectStress_RedCGC()/100);
        GDDCGCadjusted := CGCGDDSF;

        RatDGDD := 1;
        IF (GetCrop().GDDaysToFullCanopySF < GetCrop().GDDaysToSenescence)
           THEN RatDGDD := (GetCrop().DaysToSenescence-GetCrop().DaysToFullCanopySF)/(GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF);

        CCxSF := CCxTotal*(1-GetSimulation_EffectStress_RedCCX()/100);
        // maximum canopy cover than can be reached (considering soil fertility/salinity, weed stress)
        IF (SumGDDadjCC <= GetCrop().GDDaysToFullCanopySF)
           THEN CCxSFCD := CCxSF // no canopy decline before max canopy can be reached
           ELSE BEGIN // canopy decline due to soil fertility
                IF (SumGDDadjCC < GetCrop().GDDaysToSenescence)
                   THEN BEGIN
                        CCxSFCD := CCiNoWaterStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                            GetCrop().DaysToFullCanopySF,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,GetCrop().GDDaysToGermination,
                            GetCrop().GDDaysToFullCanopySF,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                            CCoTotal,CCxTotal,GetCrop().CGC,GetCrop().GDDCGC,CDCTotal,GDDCDCTotal,SumGDDadjCC,RatDGDD,
                            GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX(),GetSimulation_EffectStress_CDecline(),
                            GetCrop().ModeCycle);
                        END
                   ELSE CCxSFCD := CCxSF - (RatDGDD*GetSimulation_EffectStress_CDecline()/100) * (GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF);
                IF (CCxSFCD < 0) THEN CCxSFCD := 0;
                END;
        StressLeaf := undef_int;
        IF ((SumGDDadjCC = GetCrop().GDDaysToGermination) AND (GetCrop().DaysToCCini = 0)) THEN CCiPrev := CCoTotal;

        // time of potential vegetative growth
        GDDtFinalCCx := GetCrop().GDDaysToSenescence; // non determinant crop
        IF ((GetCrop_subkind() = Grain) AND (GetCrop().DeterminancyLinked = true)) THEN // determinancy
           BEGIN
           //reduce GDDtFinalCCx in f(determinancy of crop)
           IF (GetCrop().DaysToCCini <> 0)
              THEN BEGIN // regrowth
                   GDDtFinalCCx := GetCrop().GDDaysToFullCanopy
                       + ROUND(GDDayFraction * (GetCrop().GDDaysToFlowering + (GetCrop().GDDLengthFlowering/2)
                          +GDDTadj+GetCrop().GDDaysToGermination-GetCrop().GDDaysToFullCanopy)) // slow down
                   END
              ELSE BEGIN // sown or transplant
                   GDDtFinalCCx := GetCrop().GDDaysToFlowering + ROUND(GetCrop().GDDLengthFlowering/2);
                   END;
           IF (GDDtFinalCCx > GetCrop().GDDaysToSenescence) THEN GDDtFinalCCx := GetCrop().GDDaysToSenescence;
           END;

        //Crop.pLeafAct and Crop.pSenAct for plotting root zone depletion in RUN
        Crop_pLeafAct_temp := GetCrop().pLeafAct;
        AdjustpLeafToETo(GetETo(),Crop_pLeafAct_temp,pLeafLLAct);
        SetCrop_pLeafAct(Crop_pLeafAct_temp);
        WithBeta := true;
        Crop_pSenAct_temp := GetCrop().pSenAct;
        AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,Crop_pSenAct_temp);
        SetCrop_pSenAct(Crop_pSenAct_temp);

        //2. Canopy can still develop (stretched to GDDtFinalCCx)
        IF (SumGDDadjCC < GDDtFinalCCx)
           THEN BEGIN //Canopy can stil develop (stretched to GDDtFinalCCx)
                IF ((CCiPrev <= GetCrop().CCoAdjusted) OR (SumGDDadjCC <= GDDayi)
                   OR ((GetSimulation_ProtectedSeedling() = true) AND (CCiPrev <= (1.25 * CCoTotal))))
                   //2.a First day or very small CC as a result of senescence (no adjustment for leaf stress)
                   THEN BEGIN
                        CGCadjustmentAfterCutting := false;
                         IF (GetSimulation_ProtectedSeedling() = true)
                           THEN BEGIN
                                CCiActual :=
                                CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                                       GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                                       GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                                       CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                                       SumGDDadjCC,GetCrop().ModeCycle,
                                                       GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                IF (CCiActual > (1.25 * CCoTotal)) THEN SetSimulation_ProtectedSeedling(false);
                                END
                           ELSE BEGIN
                                CCiActual := GetCrop().CCoAdjusted*Exp(CGCGDDSF * GDDayi);
                                END;
                        END
                   //2.b CC > CCo
                   ELSE BEGIN
                        IF (CCiPrev < (0.97999*CCxSF))
                           THEN BEGIN
                                DetermineGDDCGCadjusted(GDDCGCadjusted);
                                IF (GDDCGCadjusted > 0.00000001)
                                   THEN BEGIN // Crop.GDDCGC or GDDCGCadjusted > 0
                                        Crop_CCxAdjusted_temp := GetCrop().CCxAdjusted;
                                        DetermineCCxAdjusted(Crop_CCxAdjusted_temp);
                                        SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp);
                                        IF (GetCrop().CCxAdjusted < 0)
                                           THEN CCiActual := CCiPrev
                                           ELSE IF (ABS(CCiPrev - 0.97999*CCxSF) < 0.001)
                                                   //THEN CCiActual := CCxSF
                                                   THEN CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                                                         GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                                                         GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                                                         CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                                                         SumGDDadjCC,GetCrop().ModeCycle,
                                                                         GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX())
                                                   ELSE BEGIN
                                                        GDDtTemp := RequiredGDD(CCiprev,GetCrop().CCoAdjusted,GetCrop().CCxAdjusted,GDDCGCadjusted);
                                                        IF (GDDtTemp < 0)
                                                           THEN CCiActual := CCiPrev
                                                           ELSE BEGIN
                                                                GDDtTemp := GDDtTemp + GDDayi;
                                                                CCiActual := CCatGDDTime(GDDtTemp,GetCrop().CCoAdjusted,GDDCGCadjusted,GetCrop().CCxAdjusted)
                                                                END;
                                                        END;
                                        END
                                   ELSE BEGIN // GDDCGCadjusted = 0 - too dry for leaf expansion
                                        CCiActual := CCiPrev;
                                        IF (CCiActual > GetCrop().CCoAdjusted)
                                           THEN SetCrop_CCoAdjusted(CCoTotal)
                                           ELSE SetCrop_CCoAdjusted(CCiActual);
                                        END;
                                END
                           ELSE BEGIN
                                CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                     SumGDDadjCC,GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                SetCrop_CCoAdjusted(CCoTotal);
                                StressLeaf := -33; // maximum canopy is reached;
                                CGCadjustmentAfterCutting := false;   // no increase of Canopy development after Cutting
                                END;
                        IF (CCiActual > CCxSFCD) THEN
                           BEGIN
                           CCiActual := CCxSFCD;
                           StressLeaf := -33; // maximum canopy is reached;
                           CGCadjustmentAfterCutting := false;  // no increase of Canopy development after Cutting
                           END;
                        END;
                SetCrop_CCxAdjusted(CCiActual);
                END

        //3. Canopy can no longer develop (Mid-season (from tFinalCCx) or Late season stage)
           ELSE BEGIN
                StressLeaf := -33; // maximum canopy is reached;
                CGCadjustmentAfterCutting := false;   // no increase of Canopy development after Cutting
                IF (GetCrop().CCxAdjusted < 0) THEN SetCrop_CCxAdjusted(CCiPrev);

                IF (SumGDDadjCC < GetCrop().GDDaysToSenescence)
                // mid-season
                   THEN BEGIN
                        IF (GetCrop().CCxAdjusted > 0.97999*CCxSF)
                           THEN BEGIN
                                CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,CCxTotal,GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                     SumGDDadjCC,GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                                SetCrop_CCxAdjusted(CCiActual);
                                END
                           ELSE CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,(GetCrop().CCxAdjusted/(1-GetSimulation_EffectStress_RedCCx()/100)),
                                     GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                     SumGDDadjCC,GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                        IF (CCiActual > CCxSFCD) THEN CCiActual := CCxSFCD;
                        END
                // late season
                   ELSE BEGIN
                        (*
                        StressSenescence := undef_int; // to avoid display of zero stress in late season
                        IF (GetCrop().CCxAdjusted > CCxSFCD) THEN SetCrop_CCxAdjusted(CCxSFCD);
                        GDDCDCadjusted := GetGDDCDCadjustedNoStress(CCxTotal,GDDCDCTotal,GetCrop().CCxAdjusted);
                        CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+Simulation.DelayedDays+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,(GetCrop().CCxAdjusted/(1-Simulation.EffectStress.RedCCx/100)),
                                     GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                     SumGDDadjCC,GetCrop_ModeCycle(),
                                     Simulation.EffectStress.RedCGC,Simulation.EffectStress.RedCCX);
                        //IF (CCiActual > CCxSFCD) THEN CCiActual := CCxSFCD; // added March 2017 *)

                        StressSenescence := undef_int; // to avoid display of zero stress in late season
                        IF (GetCrop().CCxAdjusted > CCxSFCD) THEN SetCrop_CCxAdjusted(CCxSFCD);
                        IF (GetCrop().CCxAdjusted < 0.01)
                           THEN CCiActual := 0
                           ELSE BEGIN  // calculate CC in late season
                                // CCibis = CC which canopy declines (soil fertility/salinity stress) further in late season
                                CCibis := CCxSF - (RatDGDD*GetSimulation_EffectStress_CDecline()/100)
                                          * (exp(2*Ln(SumGDDadjCC - GetCrop().GDDaysToFullCanopySF))
                                                /(GetCrop().GDDaysToSenescence-GetCrop().GDDaysToFullCanopySF));
                                IF (CCibis < 0)
                                   THEN CCiActual := 0
                                   ELSE BEGIN
                                        // CCiActual = CC with natural senescence in late season
                                        GDDCDCadjusted := GetGDDCDCadjustedNoStress(CCxTotal,GDDCDCTotal,GetCrop().CCxAdjusted);
                                        IF (SumGDDadjCC < (GetCrop().GDDaysToSenescence + LengthCanopyDecline(GetCrop().CCxAdjusted,GDDCDCadjusted)))
                                            THEN BEGIN
                                                 CCiActual := GetCrop().CCxAdjusted *
                                                   (1 - 0.05 * (exp((SumGDDadjCC-GetCrop().GDDaysToSenescence)
                                                                 *3.33* GDDCDCadjusted/(GetCrop().CCxAdjusted + 2.29))-1));
                                                 // CCiActual becomes CCibis, when canopy decline is more severe
                                                 IF (CCibis < CCiActual) THEN CCiActual := CCibis;
                                                 END
                                            ELSE CCiActual := 0;
                                        END;
                                END;
                        END; // late season
                END; //3. Canopy can no longer develop (Mid-season (from tFinalCCx) or Late season stage)


        //4. Canopy senescence due to water stress ?
        IF ((SumGDDadjCC < GetCrop().GDDaysToSenescence) // not yet late season stage
           OR (TimeSenescence > 0)) // in late season with ongoing early senesence  (TimeSenescence in GDD)
           THEN BEGIN
                StressSenescence := 0;
                WithBeta := true;
                Crop_pSenAct_temp := GetCrop().pSenAct;
                AdjustpSenescenceToETo(GetETo(),TimeSenescence,WithBeta,Crop_pSenAct_temp);
                SetCrop_pSenAct(Crop_pSenAct_temp);
                KsRED := 1; // effect of soil salinity on the threshold for senescence
                IF (GetSimulation_SWCtopSoilConsidered() = true)
                   THEN BEGIN // top soil is relative wetter than total root zone
                           IF ((GetRootZoneWC().ZtopAct < (GetRootZoneWC().ZtopFC - GetCrop().pSenAct*KsRED*(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP)))
                           AND (GetSimulation_ProtectedSeedling() = false))
                               THEN TheSenescenceON := true
                               ELSE TheSenescenceON := false;
                        END
                   ELSE BEGIN
                        IF ((GetRootZoneWC().Actual < (GetRootZoneWC().FC - GetCrop().pSenAct*KsRED*(GetRootZoneWC().FC - GetRootZoneWC().WP)))
                           AND (GetSimulation_ProtectedSeedling() = false))
                               THEN TheSenescenceON := true
                               ELSE TheSenescenceON := false;
                        END;

                IF TheSenescenceON
                   THEN BEGIN // CanopySenescence
                        CGCadjustmentAfterCutting := false;
                        SetSimulation_EvapLimitON(true); // consider withered crop when not yet in late season
                        IF (TimeSenescence = 0) THEN CCiTopEarlySen := CCiActual; // CC before canopy decline
                        TimeSenescence := TimeSenescence + GDDayi;
                        DetermineGDDCDCadjustedWaterStress(GDDCDCadjusted,KsSen);
                        IF (CCiTopEarlySen < 0.001)
                           THEN BEGIN
                                IF ((GetSimulation_SumEToStress() > GetCrop().SumEToDelaySenescence)
                                   OR (GetCrop().SumEToDelaySenescence = 0))
                                   THEN CCiSen := 0 // no crop anymore
                                   ELSE BEGIN
                                        IF (CCdormant > GetCrop().CCo)
                                           THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                           ELSE CCiSen := GetCrop().CCo;
                                        END;
                                END
                           ELSE BEGIN
                                IF (((TimeSenescence*GDDCDCadjusted*3.33)/(CCiTopEarlySen+2.29) > 100) // e power too large and in any case CCisen << 0
                                      OR (CCiprev >= 1.05 * CCiTopEarlySen)) // Ln of negative or zero value
                                      THEN BEGIN
                                           IF ((GetSimulation_SumEToStress() > GetCrop().SumEToDelaySenescence)
                                              OR (GetCrop().SumEToDelaySenescence = 0))
                                              THEN CCiSen := 0 // no crop anymore
                                              //ELSE CCiSen := GetCrop().CCo;
                                              ELSE BEGIN
                                                   IF (CCdormant > GetCrop().CCo)
                                                      THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                                      ELSE CCiSen := GetCrop().CCo;
                                                   END;
                                           END
                                      ELSE BEGIN
                                           // GDDCDC is adjusted to degree of stress
                                           // time required to reach CCiprev with GDDCDCadjusted
                                           GDDtTemp := (LN(1 + (1-CCiprev/CCiTopEarlySen)/0.05))/(GDDCDCadjusted*3.33/(CCiTopEarlySen+2.29));
                                           // add 1 day to tTemp and calculate CCiSen with CDCadjusted
                                           CCiSen := CCiTopEarlySen * (1 - 0.05*(exp((GDDtTemp+GDDayi)*GDDCDCadjusted*3.33/(CCiTopEarlySen+2.29))-1));
                                           END;
                                IF (CCiSen < 0) THEN CCiSen := 0;
                                IF ((GetCrop().SumEToDelaySenescence > 0) AND (GetSimulation_SumEToStress() <= GetCrop().SumEToDelaySenescence)) THEN
                                   BEGIN
                                   IF ((CCiSen < GetCrop().CCo) OR (CCiSen < CCdormant)) THEN
                                      BEGIN
                                      IF (CCdormant > GetCrop().CCo)
                                         THEN CCiSen := GetCrop().CCo + (1 - GetSimulation_SumEToStress()/GetCrop().SumEToDelaySenescence)*(CCdormant - GetCrop().CCo)
                                         ELSE CCiSen := GetCrop().CCo;
                                      END;
                                   END;
                                END;
                        IF (SumGDDadjCC < GetCrop().GDDaysToSenescence)
                           THEN BEGIN // before late season
                                IF (CCiSen > CCxSFCD) THEN CCiSen := CCxSFCD;
                                CCiActual := CCiSen;
                                IF (CCiActual > CCiPrev) THEN CCiActual := CCiPrev; // to avoid jump in CC
                                                         // when GDDCGCadjusted increases as a result of watering
                                SetCrop_CCxAdjusted(CCiActual);
                                IF (CCiActual < CCoTotal)
                                   THEN SetCrop_CCoAdjusted(CCiActual)
                                   ELSE SetCrop_CCoAdjusted(CCoTotal);
                                END
                           ELSE BEGIN // in late season
                                IF (CCiSen < CCiActual) THEN CCiActual := CCiSen;
                                //SetCrop_CCxAdjusted(CCiTopEarlySen);
                                //SetCrop_CCxAdjusted(CCiActual);
                                END;
                        //IF (CCiSen <= GetCrop().CCo) THEN Simulation.SumEToStress := Simulation.SumEToStress + ETo;
                        IF ((ROUND(10000*CCiSen) <= (10000*CCdormant))
                           OR (ROUND(10000*CCiSen) <= ROUND(10000*GetCrop().CCo)))
                              THEN SetSimulation_SumEToStress(GetSimulation_SumEToStress() + GetETo());
                        END
                   ELSE BEGIN // no water stress, resulting in canopy senescence
                        IF ((TimeSenescence > 0) AND (SumGDDadjCC > GetCrop().GDDaysToSenescence)) THEN
                           BEGIN  //rewatering in late season of an early declining canopy
                           Crop_CCxAdjusted_temp := GetCrop().CCxAdjusted;
                           GetNewCCxandGDDCDC(CCiprev,GDDCDCTotal,CCxSF,Crop_CCxAdjusted_temp,GDDCDCadjusted);
                           SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp);
                           CCiActual := CanopyCoverNoStressSF((VirtualTimeCC+GetSimulation_DelayedDays()+1),GetCrop().DaysToGermination,
                                     GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,
                                     GetCrop().GDDaysToGermination,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,
                                     CCoTotal,(GetCrop().CCxAdjusted/(1-GetSimulation_EffectStress_RedCCx()/100)),
                                     GetCrop().CGC,CDCTotal,GetCrop().GDDCGC,GDDCDCadjusted,
                                     SumGDDadjCC,GetCrop().ModeCycle,
                                     GetSimulation_EffectStress_RedCGC(),GetSimulation_EffectStress_RedCCX());
                           //IF (CCiActual > CCxSFCD) THEN CCiActual := CCxSFCD; // added March 2017
                           END;
                        TimeSenescence := 0;  // No early senescence or back to normal
                        StressSenescence := 0;
                        SetSimulation_SumEToStress(0);
                        END;
                END;

        //5. Adjust Crop.CCxWithered - required for correction of Transpiration of dying green canopy
        IF (CCiActual > GetCrop().CCxWithered) THEN SetCrop_CCxWithered(CCiActual);
        //IF (Crop.CCxWithered > CCxSFCD) THEN SetCrop_CCxWithered(CCxSFCD); - OUT 15/10/2008

        // 6. correction for late-season stage for rounding off errors
         IF (SumGDDadjCC > GetCrop().GDDaysToSenescence) THEN
            IF (CCiActual > CCiprev) THEN CCiActual := CCiprev;

        // 7. no crop as a result of fertiltiy and/or water stress
        IF (ROUND(1000*CCiActual) <= 0) THEN NoMoreCrop := true;

        END;
END; (* DetermineCCiGDD *)


PROCEDURE PrepareStage1;
BEGIN
IF (GetSurfaceStorage() > 0.0000001)
   THEN SetSimulation_EvapWCsurf(GetSoil().REW)
   ELSE BEGIN
        SetSimulation_EvapWCsurf(GetRain() + GetIrrigation() - GetRunOff());
        IF (GetSimulation_EvapWCsurf() > GetSoil().REW) THEN SetSimulation_EvapWCsurf(GetSoil().REW);
        END;
SetSimulation_EvapStartStg2(undef_Int);
SetSimulation_EvapZ(EvapZmin/100);
END; (* PrepareStage1 *)



PROCEDURE AdjustEpotMulchWettedSurface(dayi: INTEGER;
                                       EpotTot: DOUBLE;
                                       VAR Epot : Double;
                                       VAR EvapWCsurface : Double);
Var EpotIrri : double;
BEGIN
// 1. Mulches (reduction of EpotTot to Epot)
IF (GetSurfaceStorage() <= 0.000001)
   THEN BEGIN
        IF (dayi < GetCrop().Day1) // before season
           THEN Epot := EpotTot * (1 - (GetManagement_EffectMulchOffS()/100)*(GetManagement_SoilCoverBefore()/100))
           ELSE BEGIN
                IF (dayi < GetCrop().Day1+GetCrop().DaysToHarvest) // in season
                   THEN Epot := EpotTot * (1 - (GetManagement_EffectMulchInS()/100)*(GetManagement_Mulch()/100))
                   ELSE Epot := EpotTot * (1 - (GetManagement_EffectMulchOffS()/100)*(GetManagement_SoilCoverAfter()/100));
                END;
        END
   ELSE Epot := EpotTot; // flooded soil surface

// 2a. Entire soil surface wetted ?
IF (GetIrrigation() > 0) THEN
   BEGIN
   // before season
   IF ((dayi < GetCrop().Day1) AND (GetSimulParam_IrriFwOffSeason() < 100))
      THEN EvapoEntireSoilSurface := false;
   // in season
   IF ((dayi >= GetCrop().Day1) AND (dayi < GetCrop().Day1+GetCrop().DaysToHarvest) AND (GetSimulParam_IrriFwInSeason() < 100))
      THEN EvapoEntireSoilSurface := false;
   // after season
   IF ((dayi >= GetCrop().Day1+GetCrop().DaysToHarvest)AND (GetSimulParam_IrriFwOffSeason() < 100))
      THEN EvapoEntireSoilSurface := false;
   END;
IF ((GetRain() > 1) OR (GetSurfaceStorage() > 0)) THEN EvapoEntireSoilSurface := true;
IF ((dayi >= GetCrop().Day1) AND (dayi < GetCrop().Day1+GetCrop().DaysToHarvest) AND (GetIrriMode() = Inet))
   THEN EvapoEntireSoilSurface := true;

// 2b. Correction for Wetted surface by Irrigation
IF (EvapoEntireSoilSurface = false) THEN
   BEGIN
   IF ((dayi >= GetCrop().Day1) AND (dayi < GetCrop().Day1+GetCrop().DaysToHarvest))
      THEN BEGIN // in season
           EvapWCsurface := EvapWCsurface * (GetSimulParam_IrriFwInSeason()/100);
           EpotIrri := EpotTot * (GetSimulParam_IrriFwInSeason()/100);
           END
      ELSE BEGIN // off-season
           EvapWCsurface := EvapWCsurface * (GetSimulParam_IrriFwOffSeason()/100);
           EpotIrri := EpotTot * (GetSimulParam_IrriFwOffSeason()/100);
           END;
   IF (Eact > EpotIrri) THEN EpotIrri := Eact;  // Eact refers to the previous day
   IF (EpotIrri < Epot) THEN Epot := Epotirri;
   END;
END; (* AdjustEpotMulchWettedSurface *)


FUNCTION WCEvapLayer(Zlayer : double;
                     AtTheta : rep_WhichTheta) : double;
VAR Ztot,Wx,fracZ : double;
    compi : ShortInt;
BEGIN
Wx := 0.0;
Ztot := 0.0;
compi := 0;
WHILE ((ABS(Zlayer-Ztot) > 0.0001) AND (compi < GetNrCompartments())) DO
      BEGIN
      compi := compi + 1;
      IF ((Ztot + GetCompartment_Thickness(compi)) > Zlayer)
         THEN fracZ := (Zlayer - Ztot)/(GetCompartment_Thickness(compi))
         ELSE fracZ := 1;
      CASE AtTheta OF
           AtSAT  : Wx := Wx + 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).SAT * fracZ * GetCompartment_Thickness(compi)
                    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           AtFC   : Wx := Wx + 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).FC * fracZ * GetCompartment_Thickness(compi)
                    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           AtWP   : Wx := Wx + 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).WP * fracZ * GetCompartment_Thickness(compi)
                    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           else Wx := Wx + 1000 * GetCompartment_Theta(compi) * fracZ * GetCompartment_Thickness(compi)
                      * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
           end;
      Ztot := Ztot + fracZ * GetCompartment_Thickness(compi);
      END;
WCEvapLayer := Wx;
END; (* WCEvapLayer *)



PROCEDURE PrepareStage2;
VAR AtTheta : rep_WhichTheta;
    WSAT,WFC,Wact : double;
BEGIN
SetSimulation_EvapZ(EvapZmin/100);
AtTheta := AtSat;
WSAT := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
AtTheta := AtFC;
WFC := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
AtTheta := AtAct;
Wact := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
SetSimulation_EvapStartStg2(ROUND(100 * (Wact - (WFC-GetSoil().REW))/(WSAT-(WFC-GetSoil().REW))));
IF (GetSimulation_EvapStartStg2() < 0)
   THEN SetSimulation_EvapStartStg2(0);
END; (* PrepareStage2 *)



PROCEDURE CalculateEvaporationSurfaceWater;
VAR SaltSurface : double;
BEGIN
IF (GetSurfaceStorage() > GetEpot())
   THEN BEGIN
        SaltSurface := GetSurfaceStorage()*GetECstorage()*Equiv;
        Eact := GetEpot();
        SetSurfaceStorage(GetSurfaceStorage() - Eact);
        SetECstorage(SaltSurface/(GetSurfaceStorage()*Equiv)); //salinisation of surface storage layer
        END
   ELSE BEGIN
        Eact := GetSurfaceStorage();
        SetSurfaceStorage(0);
        SetSimulation_EvapWCsurf(GetSoil().REW);
        SetSimulation_EvapZ(EvapZmin/100);
        IF (GetSimulation_EvapWCsurf() < 0.0001)
           THEN PrepareStage2
           ELSE SetSimulation_EvapStartStg2(undef_int);
        END;
END; (* CalculateEvaporationSurfaceWater *)




PROCEDURE ExtractWaterFromEvapLayer(EvapToLose : double;
                                    Zact : double;
                                    Stg1 : Boolean);
VAR EvapLost,Wx,Wairdry,AvailableW,Ztot,fracZ,StillToExtract : double;
    compi : ShortInt;

BEGIN
EvapLost := 0;
compi := 0;
Ztot := 0;
REPEAT
  compi := compi + 1;
  IF ((Ztot + GetCompartment_Thickness(compi)) > Zact)
     THEN fracZ := (Zact-Ztot)/GetCompartment_Thickness(compi)
     ELSE fracZ := 1;
  Wairdry := 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).WP/2 * GetCompartment_Thickness(compi)
             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
  Wx := 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
        * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
  AvailableW := (Wx-Wairdry)*fracZ;
  StillToExtract := (EvapToLose-EvapLost);
  IF (AvailableW > 0) THEN
     BEGIN
     IF (AvailableW > StillToExtract)
        THEN BEGIN
             Eact := Eact + StillToExtract;
             EvapLost := EvapLost + StillToExtract;
             Wx := Wx - StillToExtract;
             END
        ELSE BEGIN
             Eact := Eact + AvailableW;
             EvapLost := EvapLost + AvailableW;
             Wx := Wx - AvailableW;
             END;
     SetCompartment_Theta(compi, Wx/
        (1000*GetCompartment_Thickness(compi)*(1-GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100)));
     END;
  Ztot := Ztot + fracZ * (GetCompartment_Thickness(compi));
UNTIL ((Compi >= GetNrCompartments())
       OR (Abs(StillToExtract) < 0.0000001)
       OR (Ztot >= 0.999999*Zact));
IF Stg1 THEN
   BEGIN
   SetSimulation_EvapWCsurf(GetSimulation_EvapWCsurf() - EvapLost);
   IF (Abs(EvapToLose-EvapLost) > 0.0001) // not enough water left in the compartment to store WCsurf
         THEN SetSimulation_EvapWCsurf(0);
   END;
END; (* ExtractWaterFromEvapLayer *)




PROCEDURE CalculateSoilEvaporationStage1;
VAR Eremaining : double;
    Stg1 : BOOLEAN;
BEGIN
Stg1 := true;
Eremaining := GetEpot() - Eact;
IF (GetSimulation_EvapWCsurf() > Eremaining)
   THEN ExtractWaterFromEvapLayer(Eremaining,EvapZmin,Stg1)
   ELSE ExtractWaterFromEvapLayer(GetSimulation_EvapWCsurf(),EvapZmin,Stg1);
IF (GetSimulation_EvapWCsurf() <0.0000001) THEN PrepareStage2;
END; (* CalculateSoilEvaporationStage1 *)



PROCEDURE CalculateSoilEvaporationStage2;
CONST NrOfStepsInDay = 20;
      FractionWtoExpandZ = 0.4;
TYPE repArrayD = Array[2..12] of double;
     repArraySI = Array[2..12] of ShortInt;
VAR AtTheta : rep_WhichTheta;
    Wupper, Wlower, Wact, Eremaining, Wrel, Kr, Elost,
    MaxSaltExDepth, SX, Zi, SaltDisplaced, UL, DeltaX  : double;
    i, compi, SCell1, SCellEnd  : ShortInt;
    Stg1, BoolCell : BOOLEAN;
    ThetaIniEvap : repArrayD;
    SCellIniEvap : repArraySI;

    PROCEDURE GetLimitsEvapLayer(xProc : double;
                                 VAR Wupper,Wlower : double);
    VAR AtTheta : rep_WhichTheta;
        WSAT,WFC : double;
    BEGIN
    AtTheta := AtSat;
    WSAT := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    AtTheta := AtFC;
    WFC := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    Wupper := (xProc/100) * (WSAT - (WFC-GetSoil().REW)) + (WFC-GetSoil().REW);
    AtTheta := AtWP;
    Wlower := WCEvapLayer(GetSimulation_EvapZ(),AtTheta)/2;
    END; (* GetLimitsEvapLayer *)


    FUNCTION SaltTransportFactor(theta : double) : double;
    VAR x : double ;
    BEGIN
    IF (theta <= GetSoilLayer_i(1).WP/200)
       THEN SaltTransportFactor := 0
       ELSE BEGIN
            x := (theta*100 - GetSoilLayer_i(1).WP/2)/(GetSoilLayer_i(1).SAT - GetSoilLayer_i(1).WP/2);
            SaltTransportFactor := EXP(x*LN(10)+LN(x/10));
            END;
    END; (* SaltTransportFactor *)



BEGIN (* CalculateSoilEvaporationStage2 *)

// Step 1. Conditions before soil evaporation
compi := 1;
MaxSaltExDepth := GetCompartment_Thickness(1);
WHILE ((MaxSaltExDepth < GetSimulParam_EvapZmax()) AND (compi < GetNrCompartments())) DO
  BEGIN
  compi := compi + 1;
  ThetaIniEvap[compi] := GetCompartment_Theta(compi);
  SCellIniEvap[compi] := ActiveCells(GetCompartment_i(compi));
  MaxSaltExDepth := MaxSaltExDepth + GetCompartment_Thickness(compi);
  END;

// Step 2. Soil evaporation
Stg1 := false;
Eremaining := GetEpot()-Eact;
GetLimitsEvapLayer(GetSimulation_EvapStartStg2(),Wupper,Wlower);
FOR i := 1 TO NrOfStepsInDay DO
    BEGIN
    AtTheta := AtAct;
    Wact := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    Wrel := (Wact-Wlower)/(Wupper-Wlower);
    IF (GetSimulParam_EvapZmax() > EvapZmin) THEN
       WHILE ((Wrel < (FractionWtoExpandZ*(GetSimulParam_EvapZmax()-(100*GetSimulation_EvapZ()))/(GetSimulParam_EvapZmax()-EvapZmin)))
          AND (GetSimulation_EvapZ() < GetSimulParam_EvapZmax()/100)) DO
          BEGIN
          SetSimulation_EvapZ(GetSimulation_EvapZ() + 0.001); // add 1 mm
          GetLimitsEvapLayer(GetSimulation_EvapStartStg2(),Wupper,Wlower);
          AtTheta := AtAct;
          Wact := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
          Wrel := (Wact-Wlower)/(Wupper-Wlower);
          END;
    Kr := SoilEvaporationReductionCoefficient(Wrel,GetSimulParam_EvapDeclineFactor());
    IF (Abs(getETo() - 5) > 0.01) THEN // correction for evaporative demand
       BEGIN
       // adjustment of Kr (not considered yet)
       END;
    Elost := Kr * (Eremaining/NrOfStepsInDay);
    ExtractWaterFromEvapLayer(Elost,GetSimulation_EvapZ(),Stg1);
    END;

// Step 3. Upward salt transport
SX := SaltTransportFactor(Getcompartment_Theta(1));
IF (SX > 0.01) THEN
   BEGIN
   SCell1 := ActiveCells(GetCompartment_i(1));
   compi := 2;
   Zi := GetCompartment_Thickness(1) + GetCompartment_Thickness(2);
   WHILE ((ROUND(Zi*100) <= ROUND(MaxSaltExDepth*100)) AND (compi <= GetNrCompartments())
          AND (ROUND(ThetaIniEvap[compi]*100000) <> ROUND(GetCompartment_theta(compi)*100000))) DO
      BEGIN  // move salt to compartment 1
      SCellEnd := ActiveCells(GetCompartment_i(compi));
      BoolCell := false;
      UL := GetSoilLayer_i(GetCompartment_Layer(compi)).UL;
      DeltaX := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx;
      REPEAT
      IF (SCellEnd < SCellIniEvap[compi])
         THEN BEGIN
              SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi]);
              SetCompartment_Salt(compi, SCellIniEvap[compi], GetCompartment_Salt(compi, SCellIniEvap[compi]) - SaltDisplaced);
              SCellIniEvap[compi] := SCellIniEvap[compi] - 1;
              ThetaIniEvap[compi] := DeltaX * SCellIniEvap[compi];
              END
         ELSE BEGIN
              BoolCell := true;
              IF (SCellEnd = GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1)
                 THEN SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi])
                      * (ThetaIniEvap[compi] - GetCompartment_theta(compi))/(ThetaIniEvap[compi]-UL)
                 ELSE SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi])
                      * (ThetaIniEvap[compi] - GetCompartment_theta(compi))/(ThetaIniEvap[compi]-(DeltaX*(SCellEnd-1)));
              SetCompartment_Salt(compi, SCellIniEvap[compi], GetCompartment_Salt(compi, SCellIniEvap[compi]) - SaltDisplaced);
              END;
      SetCompartment_Salt(1, SCell1, GetCompartment_Salt(1, SCell1) + SaltDisplaced);
      UNTIL BoolCell;
      compi := compi + 1;
      IF (compi <= GetNrCompartments()) THEN Zi := Zi + GetCompartment_Thickness(compi);
      END;
   END;

END; (* CalculateSoilEvaporationStage2 *)



PROCEDURE calculate_transpiration(Tpot : double; VAR Tact : double);

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
Tact := 0.0;

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
     //theta_to_extract := ((TpotMAX-Tact)/1000)/Compartment[compi].Thickness;
     WtoExtract := TpotMAX-Tact;
     //IF (theta_to_extract < sink) THEN sink := theta_to_extract;
     IF (WtoExtract < sinkMM) THEN sinkMM := WtoExtract;
     //Compartment[compi].theta := Compartment[compi].theta - sink;
     SetCompartment_theta(compi, GetCompartment_theta(compi)
          - sinkMM/(1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100)));
     //theta_to_extract := theta_to_extract - sink;
     WtoExtract := WtoExtract - sinkMM;
     //Tact := Tact + sink * 1000 * Compartment[compi].Thickness;
     Tact := Tact + sinkMM;
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
        Tact := KsReduction*Part*GetTpot();
        //NEW
        SetECstorage(SaltSurface/(GetSurfaceStorage()*Equiv)); //salinisation of surface storage layer
        END
   ELSE BEGIN
        Tact := GetSurfaceStorage() -0.1;
        SetSurfaceStorage(0.1); // zero give error in already updated salt balance
        END;
IF (Tact < KsReduction*Part*GetTpot()) THEN
   BEGIN
   calculate_transpiration((KsReduction*Part*GetTpot()-Tact),Textra);
   Tact := Tact + Textra;
   END;
END; (* surface_transpiration *)


PROCEDURE FeedbackCC;
BEGIN
IF (((CCiActual - CCiPrev) > 0.005)  // canopy is still developing
    AND (Tact = 0))                  // due to aeration stress or ETo = 0
THEN CCiActual := CCiPrev;           // no transpiration, no crop development
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



PROCEDURE ConcentrateSalts;
Var compi, celWet, celi : INTEGER;
    SaltTot, mm : Double;
    Salt_temp, Depo_temp : double;
BEGIN
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    SaltTot := 0;
    celWet := ActiveCells(GetCompartment_i(compi));
    IF (celWet < GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1) THEN FOR celi := (celWet+1) TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
       BEGIN
       SaltTot := SaltTot + GetCompartment_Salt(compi, celi) + GetCompartment_Depo(compi, celi);
       SetCompartment_Salt(compi, celi, 0);
       SetCompartment_Depo(compi, celi, 0);
       END;
    IF (SaltTot > 0) THEN
       BEGIN
       SetCompartment_Salt(compi, celWet, GetCompartment_Salt(compi, celWet) + SaltTot);
       mm := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx*1000*GetCompartment_Thickness(compi)
             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
       Salt_temp := GetCompartment_Salt(compi, celWet);
       Depo_temp := GetCompartment_Depo(compi, celWet);
       SaltSolutionDeposit(mm,Salt_temp, Depo_temp);
       SetCompartment_Salt(compi, celWet, Salt_temp);
       SetCompartment_Depo(compi, celWet, Depo_temp);
       END;
    END;
END; (* ConcentrateSalts *)



BEGIN (* BUDGET_module *)


// 1. Soil water balance
control := begin_day;
CheckWaterSaltBalance(control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain);

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
calculate_saltcontent(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage);


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
           GDDays : DetermineCCiGDD(CCxTotal,CCoTotal,CCiActual);
           else DetermineCCi(CCxTotal,CCoTotal,CCiActual);
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
               CCiActual,GetETo(),GetCrop().KcTop,GetCrop().KcDecline,GetCrop().CCxAdjusted,GetCrop().CCxWithered,GetCrop().CCEffectEvapLate,CO2i,
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
Eact := 0;
IF (GetEpot() > 0) THEN
   BEGIN
   // surface water
   IF (GetSurfaceStorage() > 0) THEN CalculateEvaporationSurfaceWater;
   // stage 1 evaporation
   IF ((ABS(GetEpot() - Eact) > 0.0000001) AND (GetSimulation_EvapWCsurf() > 0))
      THEN CalculateSoilEvaporationStage1;
   // stage 2 evaporation
   IF (ABS(GetEpot() - Eact) > 0.0000001) THEN CalculateSoilEvaporationStage2;
   END;
// Reset redcution Epot for 10-day or monthly rainfall data
IF (((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   AND (GetSimulParam_EffectiveRain_RootNrEvap() > 0))
 THEN SetEpot(GetEpot()/(exp((1/GetSimulParam_EffectiveRain_RootNrEvap())*ln((GetSoil().REW+1)/20))));


// 13. Transpiration
IF ((NoMoreCrop = false) AND (GetRootingDepth() > 0.0001)) THEN
   BEGIN
   IF ((GetSurfaceStorage() > 0) AND
       ((GetCrop().AnaeroPoint = 0) OR (GetDaySubmerged() < GetSimulParam_DelayLowOxygen())))
       THEN surface_transpiration
       ELSE calculate_transpiration(GetTpot(),Tact);
   END;
IF (GetSurfaceStorage() <= 0) THEN SetDaySubmerged(0);
FeedbackCC;

// 14. Adjustment to groundwater table
IF WaterTableInProfile THEN HorizontalInflowGWTable(GetZiAqua()/100);

// 15. Salt concentration
ConcentrateSalts;

// 16. Soil water balance
control := end_day;
CheckWaterSaltBalance(control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain);
END; (* BUDGET_module *)




end.
