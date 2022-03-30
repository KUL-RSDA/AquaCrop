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




end.
