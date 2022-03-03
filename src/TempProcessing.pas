unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;


PROCEDURE BTransferPeriod(TheDaysToCCini,TheGDDaysToCCini,
                          L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
                          CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                          KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                          Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD : double;
                          TheModeCycle : rep_modeCycle;
                          TempAssimPeriod : INTEGER;
                          TempAssimStored : ShortInt;
                          VAR SumBtot,SumBstored : double);

FUNCTION Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L12SF,L123,L1234,LFlor,
                     GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
                     CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
                     StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
                     DeltaWeedStress : INTEGER;
                     StrResCDecline,ShapeFweed : Double;
                     TheModeCycle : rep_modeCycle;
                     FertilityStressOn : BOOLEAN;
                     TestRecord : BOOLEAN) : DOUBLE;

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


PROCEDURE BTransferPeriod(TheDaysToCCini,TheGDDaysToCCini,
                          L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
                          CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                          KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                          Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD : double;
                          TheModeCycle : rep_modeCycle;
                          TempAssimPeriod : INTEGER;
                          TempAssimStored : ShortInt;
                          VAR SumBtot,SumBstored : double);

CONST EToStandard = 5;
                          
VAR fTemp : textFile;
    SumGDDfromDay1,SumGDDforPlot,SumGDD,DayFraction,GDDayFraction,CCinitial,
    Tndayi,Txdayi,GDDi,CCi,CCxWitheredForB,TpotForB,EpotTotForB : double;
    GrowthON : BOOLEAN;
    GDDTadj,Tadj,DayCC,Dayi,StartStorage : INTEGER;

BEGIN

//1. Open Temperature file
IF (GetTemperatureFile() <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(GetPathNameSimul(),'TCrop.SIM'));
   Reset(fTemp);
   END;

// 2. initialize
SetSimulation_DelayedDays(0); // required for CalculateETpot
SumBtot := 0;
SumBstored := 0;
SumGDDforPlot := undef_int;
SumGDD := undef_int;
SumGDDfromDay1 := 0;
GrowthON := false;
GDDTadj := undef_int;
DayFraction := undef_int;
GDDayFraction := undef_int;
StartStorage := L1234 - TempAssimPeriod + 1;
CCxWitheredForB := 0;

// 3. Initialise 1st day
IF (TheDaysToCCini <> 0)
   THEN BEGIN  // regrowth which starts on 1st day
        GrowthON := true;
        IF (TheDaysToCCini = undef_int)
           THEN BEGIN // CCx on 1st day
                Tadj := L12 - L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := GDDL12 - GDDL0;
                   SumGDD := GDDL12;
                   END;
                CCinitial := CCx;
                END
           ELSE BEGIN // CC on 1st day is < CCx
                Tadj := TheDaysToCCini;
                DayCC := Tadj + L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := TheGDDaysToCCini;
                   SumGDD := GDDL0 + TheGDDaysToCCini;
                   SumGDDforPlot := SumGDD;
                   END;
                CCinitial := CanopyCoverNoStressSF(DayCC,L0,L123,L1234,
                                  GDDL0,GDDL123,GDDL1234,CCo,CCx,CGC,CDC,
                                  GDDCGC,GDDCDC,SumGDDforPlot,TheModeCycle,(0),(0));
                END;
        // Time reduction for days between L12 and L123
        DayFraction := (L123-L12)/(Tadj + L0 + (L123-L12) );
        IF (TheModeCycle = GDDays)
           THEN GDDayFraction := (GDDL123-GDDL12)/(GDDTadj + GDDL0 + (GDDL123-GDDL12));
        END
   ELSE BEGIN  // growth starts after germination/recover
        Tadj := 0;
        IF (TheModeCycle = GDDays) THEN
           BEGIN
           GDDTadj := 0;
           SumGDD := 0;
           END;
        CCinitial := CCo;
        END;

//4. Calculate Biomass
FOR Dayi := 1 TO L1234 DO
    BEGIN
    //4.1 growing degrees for dayi
    IF (GetTemperatureFile() <> '(None)')
       THEN BEGIN
            READLN(fTemp,Tndayi,Txdayi);
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,GetSimulParam_GDDMethod());
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,GetSimulParam_GDDMethod());
    IF (TheModeCycle = GDDays) THEN
       BEGIN
       SumGDD := SumGDD + GDDi;
       SumGDDfromDay1 := SumGDDfromDay1 + GDDi;
       END;

    //4.2 green Canopy Cover (CC)
    DayCC := Dayi;
    IF (GrowthON = false)
       THEN BEGIN // not yet canopy development
            CCi := 0;
            IF (TheDaysToCCini <> 0)
               THEN BEGIN // regrowth
                    CCi := CCinitial;
                    GrowthON := true;
                    END
               ELSE BEGIN // sowing or transplanting
                    IF (TheModeCycle = CalendarDays)
                       THEN BEGIN
                            IF (Dayi = (L0+1)) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END
                       ELSE BEGIN
                            IF (SumGDD > GDDL0) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END;
                    END;
            END
       ELSE BEGIN
            IF (TheDaysToCCini = 0)
               THEN DayCC := Dayi
               ELSE BEGIN
                    DayCC := Dayi + Tadj + L0; // adjusted time scale
                    IF (DayCC > L1234) THEN DayCC := L1234; // special case where L123 > L1234
                    IF (DayCC > L12) THEN
                       BEGIN
                       IF (Dayi <= L123)
                          THEN DayCC := L12 + ROUND(DayFraction * (Dayi+Tadj+L0 - L12)) // slow down
                          ELSE DayCC := Dayi; // switch time scale
                       END;
                    END;
            IF (TheModeCycle = GDDays)
               THEN BEGIN
                    IF (TheGDDaysToCCini = 0)
                       THEN SumGDDforPlot := SumGDDfromDay1
                       ELSE BEGIN
                            SumGDDforPlot := SumGDD;
                            IF (SumGDDforPlot > GDDL1234) THEN SumGDDforPlot := GDDL1234; // special case where L123 > L1234
                            IF (SumGDDforPlot > GDDL12) THEN
                               BEGIN
                               IF (SumGDDfromDay1 <= GDDL123)
                                  THEN SumGDDforPlot := GDDL12 + ROUND(GDDayFraction * (SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12)) // slow down
                                  ELSE SumGDDforPlot := SumGDDfromDay1 // switch time scale
                               END
                            END;
                    END;
            CCi := CCiNoWaterStressSF(DayCC,L0,L12,L123,L1234,
                              GDDL0,GDDL12,GDDL123,GDDL1234,
                              CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDDforPlot,RatDGDD,
                              (0),(0),(0),TheModeCycle);
            END;
    IF (CCi > CCxWitheredForB) THEN CCxWitheredForB := CCi;

    //4.3 potential transpiration (TpotForB)
    IF (CCi > 0.0001)
       THEN BEGIN
            //5.3 potential transpiration of total canopy cover
            CalculateETpot(DayCC,L0,L12,L123,L1234,(0),CCi,EToStandard,KcTop,KcDeclAgeing,
                           CCx,CCxWitheredForB,CCeffectProcent,TheCO2,GDDi,GDtranspLow,TpotForB,EpotTotForB);
            END
       ELSE TpotForB := 0;

    //4.4 Biomass (B)
    IF (Dayi >= StartStorage) THEN
       BEGIN
       SumBtot := SumBtot +  WPbio * (TpotForB/EToStandard);
       SumBstored := SumBstored + WPbio*(TpotForB/EToStandard)*(0.01*TempAssimStored)*((Dayi-StartStorage+1)/TempAssimPeriod);
       END;
    END;


//5. Close Temperature file
IF (GetTemperatureFile() <> '(None)') THEN Close(fTemp);
END; (* BTransferPeriod *)



FUNCTION Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L12SF,L123,L1234,LFlor,
                     GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
                     CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
                     StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
                     DeltaWeedStress : INTEGER;
                     StrResCDecline,ShapeFweed : Double;
                     TheModeCycle : rep_modeCycle;
                     FertilityStressOn : BOOLEAN;
                     TestRecord : BOOLEAN) : DOUBLE;

CONST EToStandard = 5;
      k = 2;

VAR  fTemp,fOUT : textFile;
     SumGDD,Tndayi,Txdayi,GDDi,CCi,CCxWitheredForB,
     TpotForB,EpotTotForB,SumKCi,fSwitch,WPi,SumBnor,SumKcTopSF,fCCx : double;
     Dayi,DayCC,Tadj,GDDTadj : INTEGER;
     CCoadj,CCxadj,CDCadj,GDDCDCadj,CCw,CCtotStar,CCwStar : double;
     SumGDDfromDay1,SumGDDforPlot,CCinitial,DayFraction,GDDayFraction,fWeed,WeedCorrection : double;
     GrowthON : BOOLEAN;

BEGIN
//1. Adjustment for weed infestation
IF (WeedStress > 0)
   THEN BEGIN
        IF (StressInPercent > 0) // soil fertility stress
           THEN fWeed := 1  //no expansion of canopy cover possible
           ELSE fWeed := CCmultiplierWeed(WeedStress,CCx,ShapeFweed);
        CCoadj := CCo*fWeed;
        CCxadj := CCx*fWeed;
        CDCadj := CDC*(fWeed*CCx + 2.29)/(CCx + 2.29);
        GDDCDCadj := GDDCDC*(fWeed*CCx + 2.29)/(CCx + 2.29);
        END
   ELSE BEGIN
        CCoadj := CCo;
        CCxadj := CCx;
        CDCadj := CDC;
        GDDCDCadj := GDDCDC;
        END;

// TEST
IF (TestRecord = true) THEN
   BEGIN
   Assign(fOUT,CONCAT(GetPathNameSimul(),'TestBio.SIM'));
   Rewrite(fOUT);
   END;


// 2. Open Temperature file
IF (GetTemperatureFile() <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(GetPathNameSimul(),'TCrop.SIM'));
   Reset(fTemp);
   END;


//3. Initialize
SumKcTopSF := (1 - StressInPercent/100) * SumKcTop;   // only required for soil fertility stress

// test
IF (TestRecord = true) THEN WRITELN(fOUT,SumKcTopSF:10:1);


SetSimulation_DelayedDays(0); // required for CalculateETpot
SumKci := 0;
SumBnor := 0;
SumGDDforPlot := undef_int;
SumGDD := undef_int;
SumGDDfromDay1 := 0;
GrowthON := false;
GDDTadj := undef_int;
DayFraction := undef_int;
GDDayFraction := undef_int;
CCxWitheredForB := 0;

// 4. Initialise 1st day
IF (TheDaysToCCini <> 0)
   THEN BEGIN  // regrowth which starts on 1st day
        GrowthON := true;
        IF (TheDaysToCCini = undef_int)
           THEN BEGIN // CCx on 1st day
                Tadj := L12 - L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := GDDL12 - GDDL0;
                   SumGDD := GDDL12;
                   END;
                CCinitial := CCxadj * (1-StrResRedCCX/100);
                END
           ELSE BEGIN // CC on 1st day is < CCx
                Tadj := TheDaysToCCini;
                DayCC := Tadj + L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := TheGDDaysToCCini;
                   SumGDD := GDDL0 + TheGDDaysToCCini;
                   SumGDDforPlot := SumGDD;
                   END;
                CCinitial := CanopyCoverNoStressSF(DayCC,L0,L123,L1234,
                                  GDDL0,GDDL123,GDDL1234,CCoadj,CCxadj,CGC,CDCadj,
                                  GDDCGC,GDDCDCadj,SumGDDforPlot,TheModeCycle,StrResRedCGC,StrResRedCCX);
                END;
        // Time reduction for days between L12 and L123
        DayFraction := (L123-L12)/(Tadj + L0 + (L123-L12) );
        IF (TheModeCycle = GDDays)
           THEN GDDayFraction := (GDDL123-GDDL12)/(GDDTadj + GDDL0 + (GDDL123-GDDL12));
        END
   ELSE BEGIN  // growth starts after germination/recover
        Tadj := 0;
        IF (TheModeCycle = GDDays) THEN
           BEGIN
           GDDTadj := 0;
           SumGDD := 0;
           END;
        CCinitial := CCoadj;
        END;

//5. Calculate Bnormalized
FOR Dayi := 1 TO L1234 DO
    BEGIN
    //5.1 growing degrees for dayi
    IF (GetTemperatureFile() <> '(None)')
       THEN BEGIN
            READLN(fTemp,Tndayi,Txdayi);
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,GetSimulParam_GDDMethod());
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,GetSimulParam_GDDMethod());
    IF (TheModeCycle = GDDays) THEN
       BEGIN
       SumGDD := SumGDD + GDDi;
       SumGDDfromDay1 := SumGDDfromDay1 + GDDi;
       END;


    //5.2 green Canopy Cover (CC)
    DayCC := Dayi;
    IF (GrowthON = false)
       THEN BEGIN // not yet canopy development
            CCi := 0;
            IF (TheDaysToCCini <> 0)
               THEN BEGIN // regrowth
                    CCi := CCinitial;
                    GrowthON := true;
                    END
               ELSE BEGIN // sowing or transplanting
                    IF (TheModeCycle = CalendarDays)
                       THEN BEGIN
                            IF (Dayi = (L0+1)) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END
                       ELSE BEGIN
                            IF (SumGDD > GDDL0) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END;
                    END;
            END
       ELSE BEGIN
            IF (TheDaysToCCini = 0)
               THEN DayCC := Dayi
               ELSE BEGIN
                    DayCC := Dayi + Tadj + L0; // adjusted time scale
                    IF (DayCC > L1234) THEN DayCC := L1234; // special case where L123 > L1234
                    IF (DayCC > L12) THEN
                       BEGIN
                       IF (Dayi <= L123)
                          THEN DayCC := L12 + ROUND(DayFraction * (Dayi+Tadj+L0 - L12)) // slow down
                          ELSE DayCC := Dayi; // switch time scale
                       END;
                    END;
            IF (TheModeCycle = GDDays)
               THEN BEGIN
                    IF (TheGDDaysToCCini = 0)
                       THEN SumGDDforPlot := SumGDDfromDay1
                       ELSE BEGIN
                            SumGDDforPlot := SumGDD;
                            IF (SumGDDforPlot > GDDL1234) THEN SumGDDforPlot := GDDL1234; // special case where L123 > L1234
                            IF (SumGDDforPlot > GDDL12) THEN
                               BEGIN
                               IF (SumGDDfromDay1 <= GDDL123)
                                  THEN SumGDDforPlot := GDDL12 + ROUND(GDDayFraction * (SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12)) // slow down
                                  ELSE SumGDDforPlot := SumGDDfromDay1 // switch time scale
                               END
                            END;
                    END;
            CCi := CCiNoWaterStressSF(DayCC,L0,L12SF,L123,L1234,
                              GDDL0,GDDL12SF,GDDL123,GDDL1234,
                              CCoadj,CCxadj,CGC,GDDCGC,CDCadj,GDDCDCadj,SumGDDforPlot,RatDGDD,
                              StrResRedCGC,StrResRedCCX,StrResCDecline,TheModeCycle);
            END;
    //CCxWitheredForB := CCi;
    IF (CCi > CCxWitheredForB) THEN CCxWitheredForB := CCi;
    IF (DayCC >= L12SF) THEN CCxWitheredForB := CCxadj*(1-StrResRedCCX/100);
    CCw := CCi;

    IF (CCi > 0.0001)
       THEN BEGIN
            //5.3 potential transpiration of total canopy cover (crop and weed)
            CalculateETpot(DayCC,L0,L12,L123,L1234,(0),CCi,EToStandard,KcTop,KcDeclAgeing,
                           CCxadj,CCxWitheredForB,CCeffectProcent,TheCO2,GDDi,GDtranspLow,TpotForB,EpotTotForB);

            //5.4 Sum of Kc (only required for soil fertility stress)
            SumKci := SumKci + (TpotForB/EToStandard);

            //5.5 potential transpiration of crop canopy cover (without weed)
            IF (WeedStress > 0) THEN
               BEGIN
               // green canopy cover of the crop (CCw) in weed-infested field (CCi is CC of crop and weeds)
               fCCx := 1.0; // only for non perennials (no self-thinning)
               IF (DeltaWeedStress <> 0)
                  THEN WeedCorrection := GetWeedRC(DayCC,SumGDDforPlot,fCCx,WeedStress,GetManagement_WeedAdj(),
                  DeltaWeedStress,L12SF,L123,GDDL12SF,GDDL123,TheModeCycle)
                  ELSE WeedCorrection := WeedStress;
               CCw := CCi * (1 - WeedCorrection/100);
               // correction for micro-advection
               CCtotStar := 1.72*CCi - 1*(CCi*CCi) + 0.30*(CCi*CCi*CCi);
               IF (CCtotStar < 0) THEN CCtotStar := 0;
               IF (CCtotStar > 1) THEN CCtotStar := 1;
               IF (CCw > 0.0001)
                  THEN CCwStar := CCw + (CCtotStar - CCi)
                  ELSE CCwStar := 0;
               // crop transpiration in weed-infested field
               IF (CCtotStar <= 0.0001)
                  THEN TpotForB := 0
                  ELSE TpotForB := TpotForB * (CCwStar/CCtotStar);
               END;
            END
       ELSE TpotForB := 0;

    //5.6 biomass water productivity (WP)
     WPi := WPbio; // vegetative stage
    // 5.6a. vegetative versus yield formation stage
    IF (((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain)) AND (WPyield < 100) AND (Dayi > LFlor)) THEN
       BEGIN // yield formation stage
       fSwitch := 1;
       IF ((DaysYieldFormation > 0) AND (tSwitch > 0)) THEN
          BEGIN
          fSwitch := (Dayi-LFlor)/tSwitch;
          IF (fSwitch > 1) THEN fSwitch := 1;
          END;
       WPi := WPi * (1 - (1-WPyield/100)*fSwitch);
       END;

     //5.7 Biomass (B)
    IF FertilityStressOn
       THEN BEGIN
            //5.7a - reduction for soil fertiltiy
            IF ((StrResRedWP > 0) AND (SumKci > 0) AND (SumKcTopSF > 0)) THEN
               BEGIN
               IF (SumKci < SumKcTopSF)
                  THEN BEGIN
                       IF (SumKci > 0) THEN
                          WPi := WPi * (1 - (StrResRedWP/100) * EXP(k*Ln(SumKci/SumKcTopSF)));
                       END
                  ELSE WPi := WPi * (1 - StrResRedWP/100);
               END;
            //5.7b - Biomass (B)
            SumBnor := SumBnor +  WPi * (TpotForB/EToStandard);
            END
       ELSE SumBnor := SumBnor +  WPi * (1 - StrResRedKsSto/100) * (TpotForB/EToStandard); // for salinity stress


    // test
    IF (TestRecord = true) THEN
       WRITELN(fOUT,Dayi:10,(100*CCi):10:1,(100*CCw):10:1,WeedCorrection:10:2,(TpotForB):10:2,WPi:10:2,SumKci:10:1);

    END;


//4. Close Temperature file
IF (GetTemperatureFile() <> '(None)') THEN Close(fTemp);

IF (TestRecord = true) THEN Close(fOUT);

//5. Export
Bnormalized := SumBnor;
END; (* Bnormalized *)



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
