unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;


PROCEDURE StressBiomassRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    CropSResp : rep_Shapes;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    VAR b0,b1,b2 : double;
                                    VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);

PROCEDURE CCxSaltStressRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    TheCCsaltDistortion : ShortInt;
                                    VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
                                    VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);

FUNCTION BiomassRatio(TempDaysToCCini,TempGDDaysToCCini : INTEGER;
                      TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
                      TempKc,TempKcDecline,TempCCeffect,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
                      TempModeCycle : rep_modeCycle;
                      SFInfo : rep_EffectStress;
                      SFInfoStress,WeedStress : ShortInt;
                      DeltaWeedStress : INTEGER;
                      DeterminantCropType,FertilityStressOn : BOOLEAN) : double;


implementation


PROCEDURE StressBiomassRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    CropSResp : rep_Shapes;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    VAR b0,b1,b2 : double;
                                    VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);

CONST EToStandard = 5;
      k = 2;


TYPE StressIndexes = Record
       StressProc : ShortInt;
       BioMProc,
       BioMSquare : double;
       END;

VAR StressMatrix : ARRAY[0..7] of StressIndexes;
    Si : ShortInt;
    L12SF,GDDL12SF : INTEGER;
    StressResponse : rep_EffectStress;
    RatDGDD,BNor,BNor100,Yavg,X1avg,X2avg,y,x1,x2,x1y,x2y,x1Sq,
    x2Sq,x1x2,SUMx1y,SUMx2y,SUMx1Sq,SUMx2Sq,SUMx1x2 : double;
    SiPr : ShortInt;
    SumKcTop,HIGC,HIGClinear : double;
    DaysYieldFormation,tSwitch : INTEGER;

BEGIN
// 1. initialize
SetSimulation_DelayedDays(0); // required for CalculateETpot
L12SF := L12; // to calculate SumKcTop (no stress)
GDDL12SF := GDDL12; // to calculate SumKcTop (no stress)
// Maximum sum Kc (no stress)
SumKcTop := SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                               L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234,
                               CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                               Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,CO2Given,
                               TheModeCycle);

//Get PercentLagPhase (for estimate WPi during yield formation)
IF ((TheCropType = Tuber) OR (TheCropType = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(RefHI/RatedHIdt);
   IF CropDeterm
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(RefHI,RatedHIdt);
           GetDaySwitchToLinear(RefHI,RatedHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

// 2. Biomass production for various stress levels
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    // stress effect
    SiPr := 10*Si;
    StressMatrix[Si].StressProc := SiPr;
    CropStressParametersSoilFertility(CropSResp,SiPr,StressResponse);
    // adjusted length of Max canopy cover
    RatDGDD := 1;
    IF ((StressResponse.RedCCX = 0) AND (StressResponse.RedCGC = 0))
       THEN BEGIN
            L12SF := L12;
            GDDL12SF := GDDL12;
            END
       ELSE BEGIN
            TimeToMaxCanopySF(CCo,CGC,CCx,L0,L12,L123,LFlor,LengthFlor,CropDeterm,
                              L12SF,StressResponse.RedCGC,StressResponse.RedCCX,SiPr);
            IF (TheModeCycle = GDDays) THEN GDDL12SF := GrowingDegreeDays(L12SF,CropDNr1,Tbase,Tupper,TDayMin,TDayMax);
            IF ((TheModeCycle = GDDays) AND (GDDL12SF < GDDL123))
               THEN RatDGDD := (L123-L12SF)/(GDDL123-GDDL12SF);
            END;
    // biomass production
    BNor := Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                        L0,L12,L12SF,L123,L1234,LFlor,
                        GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                        CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                        KcTop,KcDeclAgeing,CCeffectProcent,WPveg,CO2Given,
                        Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,
                        SiPr,StressResponse.RedCGC,StressResponse.RedCCX,StressResponse.RedWP,StressResponse.RedKsSto,(0),(0),
                        StressResponse.CDecline,(-0.01),TheModeCycle,(true),(false));
    IF (Si = 0)
       THEN BEGIN
            BNor100 := BNor;
            StressMatrix[0].BioMProc := 100;
            END
       ELSE BEGIN
            IF (BNor100 > 0.00001)
               THEN StressMatrix[Si].BioMProc := 100 * BNor/BNor100
               ELSE StressMatrix[Si].BioMProc := 100;
            END;
    StressMatrix[Si].BioMSquare := StressMatrix[Si].BioMProc * StressMatrix[Si].BioMProc;
    END; // end stress level

// 5. Stress - Biomass relationship
Yavg := 0;
X1avg := 0;
X2avg := 0;
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    Yavg := Yavg + StressMatrix[Si].StressProc;
    X1avg := X1avg + StressMatrix[Si].BioMProc;
    X2avg := X2avg + StressMatrix[Si].BioMSquare;
    END;
Yavg  := Yavg/8;
X1avg := X1avg/8;
X2avg := X2avg/8;
SUMx1y  := 0;
SUMx2y  := 0;
SUMx1Sq := 0;
SUMx2Sq := 0;
SUMx1x2 := 0;
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    y     := StressMatrix[Si].StressProc - Yavg;
    x1    := StressMatrix[Si].BioMProc - X1avg;
    x2    := StressMatrix[Si].BioMSquare - X2avg;
    x1y   := x1 * y;
    x2y   := x2 * y;
    x1Sq  := x1 * x1;
    x2Sq  := x2 * x2;
    x1x2  := x1 * x2;
    SUMx1y  := SUMx1y + x1y;
    SUMx2y  := SUMx2y + x2y;
    SUMx1Sq := SUMx1Sq + x1Sq;
    SUMx2Sq := SUMx2Sq + x2Sq;
    SUMx1x2 := SUMx1x2 + x1x2;
    END;

IF (abs(ROUND(SUMx1x2*1000)) <> 0)
   THEN BEGIN
        b2 := (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/(SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2);
        b1 := (SUMx1y - b2 * SUMx1x2)/SUMx1Sq;
        b0 := Yavg - b1*X1avg - b2*X2avg;

        BM10 :=  StressMatrix[1].BioMProc;
        BM20 :=  StressMatrix[2].BioMProc;
        BM30 :=  StressMatrix[3].BioMProc;
        BM40 :=  StressMatrix[4].BioMProc;
        BM50 :=  StressMatrix[5].BioMProc;
        BM60 :=  StressMatrix[6].BioMProc;
        BM70 :=  StressMatrix[7].BioMProc;
        END
   ELSE BEGIN
        b2 := undef_int;
        b1 := undef_int;
        b0 := undef_int;
        END;
END;  (* StressBiomassRelationship *)



PROCEDURE CCxSaltStressRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    TheCCsaltDistortion : ShortInt;
                                    VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
                                    VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);

TYPE StressIndexes = Record
       CCxReduction : ShortInt;
       SaltProc,
       SaltSquare : double;
       END;

VAR L12SS,GDDL12SS,DaysYieldFormation,tSwitch : INTEGER;
    SumKcTop,HIGC,HIGClinear,CCToReach : double;
    Si,SiPr : ShortInt;
    StressMatrix : ARRAY[0..9] of StressIndexes;
    StressResponse : rep_EffectStress;
    RatDGDD,BNor,BNor100,BioMProc : double;
    Yavg,X1avg,X2avg,SUMx1y,SUMx2y,SUMx1Sq,SUMx2Sq,SUMx1x2,y,x1,x2,x1y,x2y,x1Sq,x2Sq,x1x2  : double;

BEGIN
// 1. initialize
SetSimulation_DelayedDays(0); // required for CalculateETpot
GDDL12SS := GDDL12; // to calculate SumKcTop (no stress)
BNor100 := undef_int;
// Maximum sum Kc (no stress)
SumKcTop := SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                               L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234,
                               CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                               Tbase,Tupper,TDayMin,TDayMax,GDbioLow,CO2Given,
                               TheModeCycle);
//Get PercentLagPhase (for estimate WPi during yield formation)
IF ((TheCropType = Tuber) OR (TheCropType = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(RefHI/RatedHIdt);
   IF CropDeterm
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(RefHI,RatedHIdt);
           GetDaySwitchToLinear(RefHI,RatedHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

// 2. Biomass production (or Salt stress) for various CCx reductions
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    // CCx reduction
    SiPr := 10*Si;
    StressMatrix[Si].CCxReduction := SiPr;
    // adjustment CC
    CropStressParametersSoilSalinity(SiPr,TheCCsaltDistortion,CCo,CCx,CGC,GDDCGC,CropDeterm,L12,LFlor,LengthFlor,L123,
                                     GDDL12,GDDFlor,GDDLengthFlor,GDDL123,TheModeCycle,StressResponse);
    // adjusted length of Max canopy cover
    RatDGDD := 1;
    IF ((StressResponse.RedCCX = 0) AND (StressResponse.RedCGC = 0))
       THEN BEGIN
            L12SS := L12;
            GDDL12SS := GDDL12;
            END
       ELSE BEGIN
            CCToReach := 0.98*(1-StressResponse.RedCCX/100)*CCx;
            L12SS := DaysToReachCCwithGivenCGC(CCToReach,CCo,((1-StressResponse.RedCCX/100)*CCx),
                     (CGC*(1-(StressResponse.RedCGC)/100)),L0);
            IF (TheModeCycle = GDDays) THEN GDDL12SS := GrowingDegreeDays(L12SS,CropDNr1,Tbase,Tupper,TDayMin,TDayMax);
            IF ((TheModeCycle = GDDays) AND (GDDL12SS < GDDL123))
               THEN RatDGDD := (L123-L12SS)/(GDDL123-GDDL12SS);
            END;

    // biomass production
    BNor := Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                        L0,L12,L12SS,L123,L1234,LFlor,
                        GDDL0,GDDL12,GDDL12SS,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                        CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                        KcTop,KcDeclAgeing,CCeffectProcent,WPveg,CO2Given,
                        Tbase,Tupper,TDayMin,TDayMax,GDbioLow,RatDGDD,SumKcTop,
                        SiPr,StressResponse.RedCGC,StressResponse.RedCCX,StressResponse.RedWP,StressResponse.RedKsSto,(0),(0),
                        StressResponse.CDecline,(-0.01),TheModeCycle,(false),(false));
    IF (Si = 0)
       THEN BEGIN
            BNor100 := BNor;
            BioMProc := 100;
            StressMatrix[0].SaltProc := 0;
            END
       ELSE BEGIN
            IF (BNor100 > 0.00001)
               THEN BEGIN
                    BioMProc := 100 * BNor/BNor100;
                    StressMatrix[Si].SaltProc := 100 - BioMProc;
                    END
               ELSE StressMatrix[Si].SaltProc := 0;
            END;
    StressMatrix[Si].SaltSquare := StressMatrix[Si].SaltProc * StressMatrix[Si].SaltProc;
    END; // end stress level

// 3. CCx - Salt stress relationship
Yavg := 0;
X1avg := 0;
X2avg := 0;
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    Yavg := Yavg + StressMatrix[Si].CCxReduction;
    X1avg := X1avg + StressMatrix[Si].SaltProc;
    X2avg := X2avg + StressMatrix[Si].SaltSquare;
    END;
Yavg  := Yavg/10;
X1avg := X1avg/10;
X2avg := X2avg/10;
SUMx1y  := 0;
SUMx2y  := 0;
SUMx1Sq := 0;
SUMx2Sq := 0;
SUMx1x2 := 0;
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    y     := StressMatrix[Si].CCxReduction - Yavg;
    x1    := StressMatrix[Si].SaltProc - X1avg;
    x2    := StressMatrix[Si].SaltSquare - X2avg;
    x1y   := x1 * y;
    x2y   := x2 * y;
    x1Sq  := x1 * x1;
    x2Sq  := x2 * x2;
    x1x2  := x1 * x2;
    SUMx1y  := SUMx1y + x1y;
    SUMx2y  := SUMx2y + x2y;
    SUMx1Sq := SUMx1Sq + x1Sq;
    SUMx2Sq := SUMx2Sq + x2Sq;
    SUMx1x2 := SUMx1x2 + x1x2;
    END;

IF (abs(ROUND(SUMx1x2*1000)) <> 0)
   THEN BEGIN
        Coeffb2Salt := (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/(SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2);
        Coeffb1Salt := (SUMx1y - Coeffb2Salt * SUMx1x2)/SUMx1Sq;
        Coeffb0Salt := Yavg - Coeffb1Salt*X1avg - Coeffb2Salt*X2avg;

        Salt10 :=  StressMatrix[1].SaltProc;
        Salt20 :=  StressMatrix[2].SaltProc;
        Salt30 :=  StressMatrix[3].SaltProc;
        Salt40 :=  StressMatrix[4].SaltProc;
        Salt50 :=  StressMatrix[5].SaltProc;
        Salt60 :=  StressMatrix[6].SaltProc;
        Salt70 :=  StressMatrix[7].SaltProc;
        Salt80 :=  StressMatrix[8].SaltProc;
        Salt90 :=  StressMatrix[9].SaltProc;
        END
   ELSE BEGIN
        Coeffb2Salt := undef_int;
        Coeffb1Salt := undef_int;
        Coeffb0Salt := undef_int;
        END;
END; (* CCxSaltStressRelationship *)


FUNCTION BiomassRatio(TempDaysToCCini,TempGDDaysToCCini : INTEGER;
                      TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
                      TempKc,TempKcDecline,TempCCeffect,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
                      TempModeCycle : rep_modeCycle;
                      SFInfo : rep_EffectStress;
                      SFInfoStress,WeedStress : ShortInt;
                      DeltaWeedStress : INTEGER;
                      DeterminantCropType,FertilityStressOn : BOOLEAN) : double;

Const EToStandard = 5;
      k = 2;
      CO2iLocal = 369.41;

VAR SumKcTop,HIGC,HIGClinear : double;
    RatDGDD,SumBPot,SumBSF : double;
    tSwitch,DaysYieldFormation : INTEGER;

BEGIN
//1. Initialize
//1 - a. Maximum sum Kc
SumKcTop := SeasonalSumOfKcPot(TempDaysToCCini,TempGDDaysToCCini,
                               TempL0,TempL12,TempL123,TempHarvest,TempGDDL0,TempGDDL12,TempGDDL123,TempGDDHarvest,
                               TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                               TempKc,TempKcDecline,TempCCeffect,
                               TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,CO2iLocal,
                               TempModeCycle);
//1 - b. Prepare for growing degree days
RatDGDD := 1;
//IF ((TempModeCycle = GDDays) AND (GDDL12SF < TempGDDL123))
IF ((TempModeCycle = GDDays) AND (SFInfoStress > 0) AND (GDDL12SF < TempGDDL123))
   THEN RatDGDD := (TempL123-L12SF)/(TempGDDL123-GDDL12SF);
//1 - c. Get PercentLagPhase (for estimate WPi during yield formation)
DaysYieldFormation := undef_int;
IF ((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(TempHI/TempdHIdt);
   IF DeterminantCropType
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(TempHI,TempdHIdt);
           GetDaySwitchToLinear(TempHI,TempdHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

//2. potential biomass - no soil fertiltiy stress - no weed stress
SumBPot := Bnormalized(TempDaysToCCini,TempGDDaysToCCini,
                       TempL0,TempL12,TempL12,TempL123,TempHarvest,TempFlower,
                       TempGDDL0,TempGDDL12,TempGDDL12,TempGDDL123,TempGDDHarvest,TempWPy,DaysYieldFormation,tSwitch,
                       TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                       TempKc,TempKcDecline,TempCCeffect,TempWP,CO2iLocal,
                       //TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,RatDGDD,SumKcTop,
                       TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,(1),SumKcTop,
                       (0),(0),(0),(0),(0),(0),(0),(0),(-0.01),TempModeCycle,FertilityStressOn,(false));

//3. potential biomass - soil fertiltiy stress and weed stress
SumBSF := Bnormalized(TempDaysToCCini,TempGDDaysToCCini,
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,TempGDDL12,GDDL12SF,TempGDDL123,TempGDDHarvest,TempWPy,DaysYieldFormation,tSwitch,
                      TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                      TempKc,TempKcDecline,TempCCeffect,TempWP,CO2iLocal,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,RatDGDD,SumKcTop,
                      SFInfoStress,SFInfo.RedCGC,SFInfo.RedCCX,SFInfo.RedWP,SFInfo.RedKsSto,WeedStress,
                      DeltaWeedStress,SFInfo.CDecline,ShapeFweed,TempModeCycle,FertilityStressOn,(false));

BiomassRatio := SumBSF/SumBPot;
END; (* BiomassRatio *)


end.
