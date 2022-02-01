unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;


FUNCTION GrowingDegreeDays(ValPeriod : INTEGER;
                           FirstDayPeriod : LongInt;
                           Tbase,Tupper,TDayMin,TDayMax : double) : integer;

FUNCTION SumCalendarDays(ValGDDays : INTEGER;
                      FirstDayCrop : LongInt;
                      Tbase,Tupper,TDayMin,TDayMax : double) : integer;


FUNCTION MaxAvailableGDD(FromDayNr : LongInt;
                         Tbase,Tupper,TDayMin,TDayMax : double) : Double;

PROCEDURE GDDCDCToCDC(PlantDayNr : LongInt;
                      D123,GDDL123,GDDHarvest : INTEGER;
                      CCx,GDDCDC,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
                      VAR CDC : double);

PROCEDURE AdjustCalendarDays(PlantDayNr : LongInt;
                             InfoCropType : rep_subkind;
                             Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
                             GDDL0,GDDL12,GDDFlor,GDDLengthFlor,GDDL123,GDDHarvest,GDDLZmax : INTEGER;
                             VAR GDDHImax : INTEGER;
                             GDDCGC,GDDCDC,CCo,CCx : double;
                             IsCGCGiven : BOOLEAN;
                             HIndex : INTEGER;
                             TheDaysToCCini : INTEGER;
                             ThePlanting : rep_planting;
                             VAR D0,D12,DFlor,LengthFlor,D123,DHarvest,DLZmax,LHImax : INTEGER;
                             VAR StLength : rep_int_array;
                             VAR CGC,CDC,dHIdt : double;
                             VAR Succes : BOOLEAN);


FUNCTION ResetCropDay1(CropDay1IN : LongInt;
         SwitchToYear1 : BOOLEAN) : LongInt;


PROCEDURE AdjustCalendarCrop(FirstCropDay : LongInt);

PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);

PROCEDURE TemperatureFileCoveringCropPeriod(CropFirstDay,CropLastDay : LongInt);

Function RoundedOffGDD(PeriodGDD,PeriodDay : INTEGER;
                       FirstDayPeriod : LongInt;
                       TempTbase,TempTupper,TempTmin,TempTmax : double) : INTEGER;

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

PROCEDURE CropStressParametersSoilSalinity(CCxRed,CCdistortion : ShortInt;
                                           CCo,CCx,CGC,GDDCGC : double;
                                           CropDeterm : BOOLEAN;
                                           L12,LFlor,LengthFlor,L123 : INTEGER;
                                           GDDL12,GDDLFlor,GDDLengthFlor,GDDL123 : INTEGER;
                                           TheModeCycle : rep_modeCycle;
                                           VAR StressResponse : rep_EffectStress);
                                                                   
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

PROCEDURE AdjustCropFileParameters(TheCropFileSet : rep_CropFileSet;
                                   LseasonDays : INTEGER;
                                   TheCropDay1 : LongInt;
                                   TheModeCycle  : rep_modeCycle;
                                   TheTbase,TheTupper  : double;
                                   VAR L123,L1234,GDD123,GDD1234 : INTEGER);



implementation


FUNCTION GrowingDegreeDays(ValPeriod : INTEGER;
                           FirstDayPeriod : LongInt;
                           Tbase,Tupper,TDayMin,TDayMax : double) : integer;
VAR i,RemainingDays : INTEGER;
    totalname : string;
    fTemp : TextFile;
    DayNri : LongInt;
    GDDays,DayGDD : double;
    TminDataSet,TmaxDataSet : rep_SimulationEventsDbl;
    StringREAD : ShortString;
    AdjustDayNri : BOOLEAN;
BEGIN
GDDays := 0;
IF (ValPeriod > 0) THEN
   BEGIN
   IF (GetTemperatureFile() = '(None)')
      THEN BEGIN  // given average Tmin and Tmax
           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
           GDDays := ROUND(ValPeriod * DayGDD)
           END
      ELSE BEGIN   // temperature file
           DayNri := FirstDayPeriod;
           IF  FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,
                                   GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM)
               THEN BEGIN
                    AdjustDayNri := true;
                    SetDayNrToYundef(DayNri);
                    END
               ELSE AdjustDayNri := false;
           totalname := GetTemperatureFilefull();
           IF (FileExists(totalname) AND (GetTemperatureRecord().ToDayNr > DayNri)
                                     AND (GetTemperatureRecord().FromDayNr <= DayNri))
                THEN BEGIN
                     (*AdjustDayNri := FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,
                                           GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM);*)
                     RemainingDays := ValPeriod;
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
                                    FOR i := GetTemperatureRecord().FromDayNr TO (DayNri - 1) DO READLN(fTemp);
                                    READLN(fTemp,StringREAD);
                                    SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                    //READLN(fTemp,TDayMin,TDayMax);
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    GDDays := GDDays + DayGDD;
                                    RemainingDays := RemainingDays - 1;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                       BEGIN
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
                                               SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                               //READLN(fTemp,TDayMin,TDayMax);
                                               END
                                          ELSE BEGIN
                                               READLN(fTemp,StringREAD);
                                               SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                               //READLN(fTemp,TDayMin,TDayMax);
                                               END;
                                       DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                       GDDays := GDDays + DayGDD;
                                       RemainingDays := RemainingDays - 1;
                                       DayNri := DayNri + 1;
                                       END;
                                    IF (RemainingDays > 0) THEN GDDays := undef_int;
                                    Close(fTemp);
                                    END;
                          Decadely: BEGIN
                                    GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                    i := 1;
                                    While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                    TDaymin := TminDataSet[i].Param;
                                    TDaymax := TmaxDataSet[i].Param;
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    GDDays := GDDays + DayGDD;
                                    RemainingDays := RemainingDays - 1;
                                    DayNri := DayNri + 1;
                                    WHILE((RemainingDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                      BEGIN
                                      IF (DayNri > TminDataSet[31].DayNr) THEN GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                      i := 1;
                                      While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                      TDayMin := TminDataSet[i].Param;
                                      TDayMax := TmaxDataSet[i].Param;
                                      DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                      GDDays := GDDays + DayGDD;
                                      RemainingDays := RemainingDays - 1;
                                      DayNri := DayNri + 1;
                                      END;
                                    IF (RemainingDays > 0) THEN GDDays := undef_int;
                                    END;
                          Monthly : BEGIN
                                    GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                    i := 1;
                                    While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                    TDayMin := TminDataSet[i].Param;
                                    TDayMax := TmaxDataSet[i].Param;
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    GDDays := GDDays + DayGDD;
                                    RemainingDays := RemainingDays - 1;
                                    DayNri := DayNri + 1;
                                    WHILE((RemainingDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                      BEGIN
                                      IF (DayNri > TminDataSet[31].DayNr) THEN GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                      i := 1;
                                      While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                      TDayMin := TminDataSet[i].Param;
                                      TDayMax := TmaxDataSet[i].Param;
                                      DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                      GDDays := GDDays + DayGDD;
                                      RemainingDays := RemainingDays - 1;
                                      DayNri := DayNri + 1;
                                      END;
                                    IF (RemainingDays > 0) THEN GDDays := undef_int;
                                    END;
                          end;
                     END
                ELSE BEGIN
                     GDDays := undef_int;
                     END;
             END;
   END;
GrowingDegreeDays := ROUND(GDDays);
END; (* GrowingDegreeDays *)


FUNCTION SumCalendarDays(ValGDDays : INTEGER;
                        FirstDayCrop : LongInt;
                        Tbase,Tupper,TDayMin,TDayMax : double) : integer;
VAR i : INTEGER;
    fTemp : TextFile;
    NrCDays : INTEGER;
    totalname : string;
    RemainingGDDays,DayGDD : double;
    DayNri : LongInt;
    TminDataSet,TmaxDataSet : rep_SimulationEventsDbl;
    AdjustDayNri : BOOLEAN;
    StringREAD : ShortString;

BEGIN
NrCdays := 0;
IF (ValGDDays > 0) THEN
   BEGIN
   IF (GetTemperatureFile() = '(None)')
      THEN BEGIN  // given average Tmin and Tmax
           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
           IF (DayGDD = 0)
              THEN NrCDays := -9
              ELSE NrCDays := ROUND(ValGDDays/DayGDD);
           END
      ELSE BEGIN
           DayNri := FirstDayCrop;
           IF  FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,
                                   GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM)
               THEN BEGIN
                    AdjustDayNri := true;
                    SetDayNrToYundef(DayNri);
                    END
               ELSE AdjustDayNri := false;
           totalname := GetTemperatureFilefull();
           IF (FileExists(totalname) AND (GetTemperatureRecord().ToDayNr > DayNri)
                                     AND (GetTemperatureRecord().FromDayNr <= DayNri))
                THEN BEGIN
                     (*
                     AdjustDayNri := FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,
                                           GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM);
                                           *)
                     RemainingGDDays := ValGDDays;
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
                                    FOR i := GetTemperatureRecord().FromDayNr TO (DayNri - 1) DO READLN(fTemp);
                                    READLN(fTemp,StringREAD);
                                    SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                    //READLN(fTemp,TDayMin,TDayMax);
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    NrCDays := NrCDays + 1;
                                    RemainingGDDays := RemainingGDDays - DayGDD;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingGDDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                       BEGIN
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
                                               SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                               //READLN(fTemp,TDayMin,TDayMax);
                                               END
                                          ELSE BEGIN
                                               READLN(fTemp,StringREAD);
                                               SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                               //READLN(fTemp,TDayMin,TDayMax);
                                               END;
                                       DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                       NrCDays := NrCDays + 1;
                                       RemainingGDDays := RemainingGDDays - DayGDD;
                                       DayNri := DayNri + 1;
                                       END;
                                    IF (RemainingGDDays > 0) THEN NrCDays := undef_int;
                                    Close(fTemp);
                                    END;
                          Decadely: BEGIN
                                    GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                    i := 1;
                                    While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                    TDaymin := TminDataSet[i].Param;
                                    TDaymax := TmaxDataSet[i].Param;
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    NrCDays := NrCDays + 1;
                                    RemainingGDDays := RemainingGDDays - DayGDD;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingGDDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                      BEGIN
                                      IF (DayNri > TminDataSet[31].DayNr) THEN GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                      i := 1;
                                      While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                      TDayMin := TminDataSet[i].Param;
                                      TDayMax := TmaxDataSet[i].Param;
                                      DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                      NrCDays := NrCDays + 1;
                                      RemainingGDDays := RemainingGDDays - DayGDD;
                                      DayNri := DayNri + 1;
                                      END;
                                    IF (RemainingGDDays > 0) THEN NrCDays := undef_int;
                                    END;
                          Monthly : BEGIN
                                    GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                    i := 1;
                                    While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                    TDayMin := TminDataSet[i].Param;
                                    TDayMax := TmaxDataSet[i].Param;
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    NrCDays := NrCDays + 1;
                                    RemainingGDDays := RemainingGDDays - DayGDD;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingGDDays > 0)
                                          AND ((DayNri < GetTemperatureRecord().ToDayNr) or AdjustDayNri)) DO
                                      BEGIN
                                      IF (DayNri > TminDataSet[31].DayNr) THEN GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                                      i := 1;
                                      While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                                      TDayMin := TminDataSet[i].Param;
                                      TDayMax := TmaxDataSet[i].Param;
                                      DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                      NrCDays := NrCDays + 1;
                                      RemainingGDDays := RemainingGDDays - DayGDD;
                                      DayNri := DayNri + 1;
                                      END;
                                    IF (RemainingGDDays > 0) THEN NrCDays := undef_int;
                                    END;
                          end;
                     END
                ELSE BEGIN
                     NrCDays := undef_int;
                     END;
           END;
   END;
SumCalendarDays := NrCDays;
END; (* SumCalendarDays *)




FUNCTION MaxAvailableGDD(FromDayNr : LongInt;
                         Tbase,Tupper,TDayMin,TDayMax : double) : Double;
VAR i : INTEGER;
    MaxGDDays, DayGDD : double;
    totalname : string;
    fTemp : TextFile;
    DayNri : Longint;
    TminDataSet,TmaxDataSet : rep_SimulationEventsDbl;
    StringREAD : ShortString;

BEGIN
MaxGDDays := 100000;
IF (GetTemperatureFile() = '(None)')
   THEN BEGIN
        DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
        If (DayGDD <= 0) THEN MaxGDDays := 0;
        END
   ELSE BEGIN
        MaxGDDays := 0;
        IF FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,
           GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM)
           THEN FromDayNr := GetTemperatureRecord().FromDayNr  // since we have 365 days anyway
           ELSE BEGIN
             //DetermineDate(CropDay1,dayi,monthi,yeari);
             //yeari := GetTemperatureRecord().FromY;
             //DetermineDayNr(dayi,monthi,yeari,CropDay1);
                END;
           DayNri := FromDayNr;
           totalname := GetTemperatureFilefull();
           IF (FileExists(totalname) AND (GetTemperatureRecord().ToDayNr > FromDayNr)
                             AND (GetTemperatureRecord().FromDayNr <= FromDayNr)) THEN
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
                          FOR i := GetTemperatureRecord().FromDayNr TO (FromDayNr - 1) DO READLN(fTemp);
                          READLN(fTemp,StringREAD);
                          SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                          DayNri := DayNri + 1;
                          DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                          MaxGDDays := MaxGDDays + DayGDD;
                          WHILE (DayNri < GetTemperatureRecord().ToDayNr) DO
                           BEGIN
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
                                   SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                   END
                              ELSE BEGIN
                                   READLN(fTemp,StringREAD);
                                   SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                   END;
                           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                           MaxGDDays := MaxGDDays + DayGDD;
                           DayNri := DayNri + 1;
                           END;
                          END;
                Decadely: BEGIN
                          GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                          i := 1;
                          While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                          TDaymin := TminDataSet[i].Param;
                          TDaymax := TmaxDataSet[i].Param;
                          DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                          MaxGDDays := MaxGDDays + DayGDD;
                          DayNri := DayNri + 1;
                          WHILE(DayNri < GetTemperatureRecord().ToDayNr) DO
                           BEGIN
                           IF (DayNri > TminDataSet[31].DayNr) THEN GetDecadeTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                           i := 1;
                           While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                           TDayMin := TminDataSet[i].Param;
                           TDayMax := TmaxDataSet[i].Param;
                           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                           MaxGDDays := MaxGDDays + DayGDD;
                           DayNri := DayNri + 1;
                           END;
                          END;
                Monthly : BEGIN
                          GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                          i := 1;
                          While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                          TDayMin := TminDataSet[i].Param;
                          TDayMax := TmaxDataSet[i].Param;
                          DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                          MaxGDDays := MaxGDDays + DayGDD;
                          DayNri := DayNri + 1;
                          WHILE(DayNri < GetTemperatureRecord().ToDayNr) DO
                               BEGIN
                               IF (DayNri > TminDataSet[31].DayNr) THEN GetMonthlyTemperatureDataSet(DayNri,TminDataSet,TmaxDataSet);
                               i := 1;
                               While (TminDataSet[i].DayNr <> DayNri) Do i := i+1;
                               TDayMin := TminDataSet[i].Param;
                               TDayMax := TmaxDataSet[i].Param;
                               DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                               MaxGDDays := MaxGDDays + DayGDD;
                               DayNri := DayNri + 1;
                               END;
                          END;
                end;
        END;
MaxAvailableGDD := MaxGDDays;
END; (* MaxAvailableGDD *)




PROCEDURE GDDCDCToCDC(PlantDayNr : LongInt;
                      D123,GDDL123,GDDHarvest : INTEGER;
                      CCx,GDDCDC,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
                      VAR CDC : double);
VAR ti,GDDi : INTEGER;
    CCi : double;
BEGIN
GDDi := LengthCanopyDecline(CCx,GDDCDC);
IF ((GDDL123+GDDi) <= GDDHarvest)
   THEN CCi := 0 // full decline
   ELSE BEGIN // partly decline
        IF (GDDL123 < GDDHarvest)
           THEN GDDi := GDDHarvest - GDDL123
           ELSE GDDi := 5;
        //CCi := CCx * (1 - 0.05 * (exp(GDDi*GDDCDC/CCx)-1) );  // CC at time ti
        CCi := CCx * (1 - 0.05 * (exp(GDDi*(GDDCDC*3.33)/(CCx+2.29))-1) );  // CC at time ti
        END;
ti := SumCalendarDays(GDDi,(PlantDayNr+D123),Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
IF (ti > 0)
   //THEN CDC := (CCx/ti) * Ln(1 + ((1-CCi/CCx)/0.05) )
   THEN CDC := (((CCx+2.29)/ti) * Ln(1 + ((1-CCi/CCx)/0.05) ))/3.33
   ELSE CDC := undef_int;
END; (* GDDCDCToCDC *)




PROCEDURE AdjustCalendarDays(PlantDayNr : LongInt;
                             InfoCropType : rep_subkind;
                             Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
                             GDDL0,GDDL12,GDDFlor,GDDLengthFlor,GDDL123,GDDHarvest,GDDLZmax : INTEGER;
                             VAR GDDHImax : INTEGER;
                             GDDCGC,GDDCDC,CCo,CCx : double;
                             IsCGCGiven : BOOLEAN;
                             HIndex : INTEGER;
                             TheDaysToCCini : INTEGER;
                             ThePlanting : rep_planting;
                             VAR D0,D12,DFlor,LengthFlor,D123,DHarvest,DLZmax,LHImax : INTEGER;
                             VAR StLength : rep_int_array;
                             VAR CGC,CDC,dHIdt : double;
                             VAR Succes : BOOLEAN);

BEGIN
Succes := true;
D0 := SumCalendarDays(GDDL0,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
D12 := SumCalendarDays(GDDL12,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
IF (InfoCropType <> Forage) THEN
   BEGIN
   D123 := SumCalendarDays(GDDL123,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
   DHarvest := SumCalendarDays(GDDHarvest,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
   END;
DLZmax := SumCalendarDays(GDDLZmax,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
CASE InfoCropType OF
     Grain,
     Tuber : BEGIN
             DFlor := SumCalendarDays(GDDFlor,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
             IF (DFlor <> undef_int)
                THEN BEGIN
                     IF (InfoCropType = Grain)
                        THEN LengthFlor := SumCalendarDays(GDDLengthFlor,(PlantDayNr+DFlor),Tbase,Tupper,NoTempFileTMin,NoTempFileTMax)
                        ELSE LengthFlor := 0;
                     LHImax := SumCalendarDays(GDDHImax,(PlantDayNr+DFlor),Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
                     IF ((LengthFlor = undef_int) OR (LHImax = undef_int)) THEN Succes := false;
                     END
                ELSE BEGIN
                     LengthFlor := undef_int;
                     LHImax := undef_int;
                     Succes := false;
                     END;
             END;
     Vegetative,
     Forage     : BEGIN
                  LHImax := SumCalendarDays(GDDHImax,PlantDayNr,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax);
                  END;
     end;

IF ((D0 = undef_int) OR (D12 = undef_int) OR (D123 = undef_int) OR (DHarvest = undef_int) OR
    (DLZmax = undef_int)) THEN Succes := false;

IF Succes THEN
   BEGIN
   CGC := (GDDL12/D12) * GDDCGC;
   GDDCDCToCDC(PlantDayNr,D123,GDDL123,GDDHarvest,CCx,GDDCDC,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax,CDC);
   DetermineLengthGrowthStages(CCo,CCx,CDC,D0,DHarvest,IsCGCGiven,
       TheDaysToCCini,ThePlanting,D123,StLength,D12,CGC);
   IF ((InfoCropType = Grain) OR (InfoCropType = Tuber)) THEN dHIdt := HIndex/LHImax;
   IF ((InfoCropType = Vegetative) OR (InfoCropType = Forage)) THEN
      BEGIN
      IF (LHImax > 0)
         THEN BEGIN
              IF (LHImax > DHarvest)
                 THEN dHIdt := HIndex/DHarvest
                 ELSE dHIdt := HIndex/LHImax;
              IF (dHIdt > 100) THEN
                 BEGIN
                 dHIdt := 100; // 100 is maximum TempdHIdt (See SetdHIdt)
                 LHImax := 0;
                 END;
              END
         ELSE BEGIN
              dHIdt := 100; // 100 is maximum TempdHIdt (See SetdHIdt)
              LHImax := 0;
              END;
      END;
   END;
END; (* AdjustCalendarDays *)



FUNCTION ResetCropDay1(CropDay1IN : LongInt;
         SwitchToYear1 : BOOLEAN) : LongInt;
VAR CropDay1OUT : LongInt;
    dayi,monthi,yeari : INTEGER;

BEGIN
DetermineDate(CropDay1IN,dayi,monthi,yeari);
IF (GetTemperatureRecord().FromY = 1901)
   THEN BEGIN
        yeari := 1901;
        DetermineDayNr(Dayi,Monthi,Yeari,CropDay1OUT);
        END
   ELSE BEGIN
        IF SwitchToYear1
           THEN DetermineDayNr(Dayi,Monthi,GetTemperatureRecord().FromY,CropDay1OUT)
           ELSE CropDay1OUT := CropDay1IN;
        END;
ResetCropDay1 := CropDay1OUT;
END; (* ResetCropDay1 *)


PROCEDURE AdjustCalendarCrop(FirstCropDay : LongInt);
VAR succes : BOOLEAN;
    CGCisGiven : BOOLEAN;
    Crop_GDDaysToHIo_temp : integer;
    Crop_DaysToGermination_temp, Crop_DaysToFullCanopy_temp, Crop_DaysToFlowering_temp, Crop_LengthFlowering_temp : integer;
    Crop_DaysToSenescence_temp, Crop_DaysToHarvest_temp, Crop_DaysToMaxRooting_temp, Crop_DaysToHIo_temp : integer;
    Crop_Length_temp : rep_int_array; 
    Crop_CGC_temp : double;
    Crop_CDC_temp : double;
    Crop_dHIdt_temp : double; 
BEGIN
CGCisGiven := true;
CASE GetCrop_ModeCycle() OF
     GDDays : BEGIN
              SetCrop_GDDaysToFullCanopy(GetCrop().GDDaysToGermination +
                 ROUND(LN((0.25*GetCrop().CCx*GetCrop().CCx/GetCrop().CCo)/(GetCrop().CCx-(0.98*GetCrop().CCx)))/GetCrop().GDDCGC));
              IF (GetCrop().GDDaysToFullCanopy > GetCrop().GDDaysToHarvest)
                 THEN SetCrop_GDDaysToFullCanopy(GetCrop().GDDaysToHarvest);
              Crop_GDDaysToHIo_temp := GetCrop().GDDaysToHIo;
              Crop_DaysToGermination_temp := GetCrop().DaysToGermination;
              Crop_DaysToFullCanopy_temp := GetCrop().DaysToFullCanopy;
              Crop_DaysToFlowering_temp := GetCrop().DaysToFlowering;
              Crop_LengthFlowering_temp := GetCrop().LengthFlowering;
              Crop_DaysToSenescence_temp := GetCrop().DaysToSenescence;
              Crop_DaysToHarvest_temp := GetCrop().DaysToHarvest;
              Crop_DaysToMaxRooting_temp := GetCrop().DaysToMaxRooting;
              Crop_DaysToHIo_temp := GetCrop().DaysToHIo;
              Crop_Length_temp := GetCrop().Length; 
              Crop_CGC_temp := GetCrop().CGC; 
              Crop_CDC_temp := GetCrop().CDC; 
              Crop_dHIdt_temp := GetCrop().dHIdt; 
              AdjustCalendarDays(FirstCropDay,GetCrop_subkind(),GetCrop().Tbase,GetCrop().Tupper,SimulParam.Tmin,SimulParam.Tmax,
                 GetCrop().GDDaysToGermination,GetCrop().GDDaysToFullCanopy,GetCrop().GDDaysToFlowering,
                 GetCrop().GDDLengthFlowering,GetCrop().GDDaysToSenescence,GetCrop().GDDaysToHarvest,GetCrop().GDDaysToMaxRooting,
                 Crop_GDDaysToHIo_temp,
                 GetCrop().GDDCGC,GetCrop().GDDCDC,GetCrop().CCo,GetCrop().CCx,CGCisGiven,GetCrop().HI,
                 GetCrop().DaysToCCini,GetCrop().Planting,
                 Crop_DaysToGermination_temp,Crop_DaysToFullCanopy_temp,Crop_DaysToFlowering_temp,Crop_LengthFlowering_temp,
                 Crop_DaysToSenescence_temp,Crop_DaysToHarvest_temp,Crop_DaysToMaxRooting_temp,Crop_DaysToHIo_temp,
                 Crop_Length_temp,Crop_CGC_temp,Crop_CDC_temp,Crop_dHIdt_temp,Succes);
              SetCrop_GDDaysToHIo(Crop_GDDaysToHIo_temp);
              SetCrop_DaysToGermination(Crop_DaysToGermination_temp);
              SetCrop_DaysToFullCanopy(Crop_DaysToFullCanopy_temp);
              SetCrop_DaysToFlowering(Crop_DaysToFlowering_temp);
              SetCrop_LengthFlowering(Crop_LengthFlowering_temp);
              SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp);
              SetCrop_DaysToHarvest(Crop_DaysToHarvest_temp);
              SetCrop_DaysToMaxRooting(Crop_DaysToMaxRooting_temp);
              SetCrop_DaysToHIo(Crop_DaysToHIo_temp);
              SetCrop_Length(Crop_Length_temp); 
              SetCrop_CGC(Crop_CGC_temp);
              SetCrop_CDC(Crop_CDC_temp);
              SetCrop_dHIdt(Crop_dHIdt_temp); 
              END;
     else Succes := true;
     end;
IF NOT Succes THEN ; // GEEN IDEE WAT TE DOEN
END; (* AdjustCalendarCrop *)



PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);
VAR f0,fClim : TextFile;
    TempString,TempString1,TempString2,observations_descr,eto_descr,CO2descr,rain_descr : string;
    TempSimDayNr1,TempSimDayNrN : LongInt;
    i,Runi : ShortInt;
    TotDepth : double;
    VersionNr : double;
    FertStress : shortint;
    temperature_record : rep_clim;
    TempInt : integer;
    Crop_Planting_temp : rep_Planting;
    Crop_RootMin_temp, Crop_SizePlant_temp, Crop_CCini_temp : double;
    Crop_DaysToCCini_temp, Crop_GDDaysToCCini_temp : integer;
    Crop_DaysToSenescence_temp, Crop_DaysToHarvest_temp : integer;
    Crop_GDDaysToSenescence_temp, Crop_GDDaysToHarvest_temp : integer;
    Crop_Day1_temp : INTEGER;
    Crop_DayN_temp : INTEGER;
    Crop_DaysToFullCanopySF_temp : INTEGER;

    PROCEDURE GetFileDescription(TheFileFullName : string;
                                 VAR TheDescription : string);
    VAR f0 : textfile;
    BEGIN
    Assign(f0,TheFileFullName);
    Reset(f0);
    Readln(f0,TheDescription);
    Close(f0);
    END; (* GetFileDescription *)

BEGIN
TempString := StringReplace(NameFileFull, '"', '', [rfReplaceAll]);
Assign(f0,TempString);
Reset(f0);
READLN(f0); //Description
READLN(f0,VersionNr);  // AquaCrop version Nr
IF (NrRun > 1) THEN
   BEGIN
   // Files previous runs
   FOR Runi := 1 TO (NrRun - 1) DO
       FOR i := 1 TO 47  DO Readln(f0); // 5 + 42 lines with files
   END;
// Year of cultivation and Simulation and Cropping period
READLN(f0,Simulation.YearSeason); // year number of cultivation (1 = seeding/planting year)
READLN(f0,TempSimDayNr1); //First day of simulation period
READLN(f0,TempSimDayNrN); //Last day of simulation period
READLN(f0,TempInt); //First day of cropping period
SetCrop_Day1(TempInt);
READLN(f0,TempInt); //Last day of cropping period
SetCrop_DayN(TempInt);


// 1. Climate
READLN(f0); // Info Climate
READLN(f0,TempString);  //ClimateFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetClimateFile(Trim(TempString));
IF (GetClimateFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathClimateFile
        SetClimateFileFull(GetClimateFile());
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathClimateFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetClimateFileFull(CONCAT(Trim(TempString),GetClimateFile()));
        Assign(fClim,GetClimateFileFull());
        Reset(fClim);
        // 1.0 Description
        READLN(fClim,TempString);
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        ClimateDescription := Trim(TempString);
        Close(fClim);
        END;
// 1.1 Temperature
READLN(f0); // Info Temperature
READLN(f0,TempString);  //TemperatureFile
SetTemperatureFile(Trim(TempString));
IF (GetTemperatureFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathTemperatureFile
        SetTemperatureFilefull(GetTemperatureFile());  (* no file *)
        Str(SimulParam.Tmin:8:1,TempString1);
        Str(SimulParam.Tmax:8:1,TempString2);
        TemperatureDescription := CONCAT('Default temperature data: Tmin = ',
                    trim(TempString1),' and Tmax = ',trim(TempString2),' °C');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathTemperatureFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetTemperatureFileFull(CONCAT(Trim(TempString),Trim(GetTemperatureFile())));
        temperature_record := GetTemperatureRecord();
        LoadClim(GetTemperatureFilefull(),TemperatureDescription,temperature_record);
        CompleteClimateDescription(temperature_record);
        SetTemperatureRecord(temperature_record);
        END;
// 1.2 ETo
READLN(f0); // Info ETo
READLN(f0,TempString);  //EToFile
SetEToFile(Trim(TempString));
IF (GetEToFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathETo
        SetEToFilefull(GetEToFile());  (* no file *)
        SetEToDescription('Specify ETo data when Running AquaCrop');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathETo
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetEToFilefull(CONCAT(Trim(TempString),GetEToFile()));
        eto_descr := GetEToDescription();
        LoadClim(GetEToFilefull(),eto_descr,EToRecord);
        SetEToDescription(eto_descr); 
        CompleteClimateDescription(EToRecord);
        END;
// 1.3 Rain
READLN(f0); // Info Rain
READLN(f0,TempString);  //RainFile
SetRainFile(Trim(TempString));
IF (GetRainFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathRain
        SetRainFilefull(GetRainFile());  (* no file *)
        SetRainDescription('Specify Rain data when Running AquaCrop');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathRain
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetRainFileFull(CONCAT(Trim(TempString),GetRainFile()));
        rain_descr := Getraindescription();
        LoadClim(GetRainFilefull(),rain_descr,RainRecord);
        SetRainDescription(rain_descr);
        CompleteClimateDescription(RainRecord);
        END;
// 1.4 CO2
READLN(f0); // Info CO2
READLN(f0,TempString);  //CO2File
SetCO2File(Trim(TempString));
IF (GetCO2File() = '(None)')
   THEN READLN(f0)  //PathCO2File
   ELSE BEGIN
        READLN(f0,TempString);  //PathCO2File
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetCO2FileFull(CONCAT(Trim(TempString),GetCO2File()));
        CO2descr :=  GetCO2Description();
        GenerateCO2Description(GetCO2FileFull(),CO2descr);
        SetCO2Description(CO2descr)
        END;
SetClimData;
AdjustOnsetSearchPeriod; // Set initial StartSearch and StopSearchDayNr

// 2. Calendar
READLN(f0); // Info calendar
READLN(f0,TempString);  //CalendarFile
SetCalendarFile(Trim(TempString));
IF (GetCalendarFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathCalendarFile
        CalendarDescription := 'No calendar for the Seeding/Planting year';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathCalendarFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetCalendarFilefull(CONCAT(Trim(TempString),GetCalendarFile()));
        GetFileDescription(GetCalendarFilefull(),CalendarDescription);
        END;

// 3. Crop
Simulation.LinkCropToSimPeriod := true;
READLN(f0); // Info Crop
READLN(f0,TempString);  //CropFile
SetCropFile(Trim(TempString));
READLN(f0,TempString);  //PathCropFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetCropFilefull(CONCAT(Trim(TempString),GetCropFile()));
LoadCrop(GetCropFilefull());

// Adjust crop parameters of Perennials
IF (GetCrop_subkind() = Forage) THEN
   BEGIN
   // adjust crop characteristics to the Year (Seeding/Planting or Non-seesing/Planting year)
   Crop_Planting_temp := GetCrop().Planting;
   Crop_RootMin_temp := GetCrop().RootMin;
   Crop_SizePlant_temp := GetCrop().SizePlant;
   Crop_CCini_temp := GetCrop().CCini;
   Crop_DaysToCCini_temp := GetCrop().DaysToCCini;
   Crop_GDDaysToCCini_temp := GetCrop().GDDaysToCCini;
   AdjustYearPerennials(Simulation.YearSeason,GetCrop().SownYear1,GetCrop_ModeCycle(),GetCrop().RootMax,GetCrop().RootMinYear1,
                     GetCrop().CCo,GetCrop().SizeSeedling,GetCrop().CGC,GetCrop().CCx,GetCrop().GDDCGC,GetCrop().PlantingDens,
                     Crop_Planting_temp,Crop_RootMin_temp,Crop_SizePlant_temp,Crop_CCini_temp,
                     Crop_DaysToCCini_temp,Crop_GDDaysToCCini_temp);
   SetCrop_Planting(Crop_Planting_temp);
   SetCrop_RootMin(Crop_RootMin_temp);
   SetCrop_SizePlant(Crop_SizePlant_temp);
   SetCrop_CCini(Crop_CCini_temp);
   SetCrop_DaysToCCini(Crop_DaysToCCini_temp);
   SetCrop_GDDaysToCCini(Crop_GDDaysToCCini_temp);
   // adjust length of season
   SetCrop_DaysToHarvest(GetCrop().DayN - GetCrop().Day1 + 1);
   Crop_DaysToSenescence_temp := GetCrop().DaysToSenescence;
   Crop_DaysToHarvest_temp := GetCrop().DaysToHarvest;
   Crop_GDDaysToSenescence_temp := GetCrop().GDDaysToSenescence;
   Crop_GDDaysToHarvest_temp := GetCrop().GDDaysToHarvest;
   AdjustCropFileParameters(GetCropFileSet(),(GetCrop().DaysToHarvest),GetCrop().Day1,GetCrop_ModeCycle(),GetCrop().Tbase,GetCrop().Tupper,
                                    Crop_DaysToSenescence_temp,Crop_DaysToHarvest_temp,
                                    Crop_GDDaysToSenescence_temp,Crop_GDDaysToHarvest_temp);
   SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp);
   SetCrop_DaysToHarvest(Crop_DaysToHarvest_temp);
   SetCrop_GDDaysToSenescence(Crop_GDDaysToSenescence_temp);
   SetCrop_GDDaysToHarvest(Crop_GDDaysToHarvest_temp);
   END;

AdjustCalendarCrop(GetCrop().Day1);
CompleteCropDescription;
//Onset.Off := true;
IF (GetClimFile() = '(None)')
   THEN BEGIN
       Crop_Day1_temp := GetCrop().Day1;
       Crop_DayN_temp := GetCrop().DayN;
       AdjustCropYearToClimFile(Crop_Day1_temp,Crop_DayN_temp); // adjusting GetCrop().Day1 and GetCrop().DayN to ClimFile
       SetCrop_Day1(Crop_Day1_temp);
       SetCrop_DayN(Crop_DayN_temp);
       END
   ELSE SetCrop_DayN(GetCrop().Day1 + GetCrop().DaysToHarvest - 1);

(* adjusting ClimRecord.'TO' for undefined year with 365 days *)
IF ((GetClimFile() <> '(None)') AND (ClimRecord.FromY = 1901)
   AND (ClimRecord.NrObs = 365)) THEN AdjustClimRecordTo(GetCrop().DayN);
(* adjusting simulation period *)
AdjustSimPeriod;

// 4. Irrigation
READLN(f0); // Info Irrigation
READLN(f0,TempString);  //IrriFile
SetIrriFile(Trim(TempString));
IF (GetIrriFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathIrriFile
        SetIrriFileFull(GetIrriFile());
        NoIrrigation;
        //IrriDescription := 'Rainfed cropping';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathIrriFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetIrriFileFull(CONCAT(Trim(TempString),GetIrriFile()));
        LoadIrriScheduleInfo(GetIrriFileFull());
        END;

// 5. Field Management
READLN(f0); // Info Field Management
READLN(f0,TempString);  //ManFile
SetManFile(Trim(TempString));
IF (GetManFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathManFile
        SetManFileFull(GetManFile());
        ManDescription := 'No specific field management';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathManFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetManFileFull(CONCAT(Trim(TempString),GetManFile()));
        LoadManagement(GetManFilefull());
        // reset canopy development to soil fertility
        FertStress := GetManagement_FertilityStress();
        Crop_DaysToFullCanopySF_temp := GetCrop().DaysToFullCanopySF;
        TimeToMaxCanopySF(GetCrop().CCo,GetCrop().CGC,GetCrop().CCx,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,
                          GetCrop().DaysToFlowering,GetCrop().LengthFlowering,GetCrop().DeterminancyLinked,
                          Crop_DaysToFullCanopySF_temp,Simulation.EffectStress.RedCGC,
                          Simulation.EffectStress.RedCCX,FertStress);
        SetCrop_DaysToFullCanopySF(Crop_DaysToFullCanopySF_temp);
        SetManagement_FertilityStress(FertStress);
        END;

// 6. Soil Profile
READLN(f0); // Info Soil
READLN(f0,TempString);  //ProfFile
SetProfFile(Trim(TempString));
READLN(f0,TempString);  //PathProfFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetProfFilefull(CONCAT(Trim(TempString),GetProfFile()));
// The load of profile is delayed to check if soil water profile need to be reset (see 8.)

// 7. Groundwater
READLN(f0); // Info Groundwater
READLN(f0,TempString);  //GroundWaterFile
SetGroundWaterFile(Trim(TempString));
IF (GetGroundWaterFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathGroundWaterFile
        SetGroundWaterFilefull(GetGroundWaterFile());
        GroundWaterDescription := 'no shallow groundwater table';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathGroundWaterFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetGroundWaterFilefull(CONCAT(Trim(TempString),GetGroundWaterFile()));
        // Loading the groundwater is done after loading the soil profile (see 9.)
        END;


// 8. Set simulation period
Simulation.FromDayNr := TempSimDayNr1;
Simulation.ToDayNr := TempSimDayNrN;
IF ((GetCrop().Day1 <> Simulation.FromDayNr) OR (GetCrop().DayN <> Simulation.ToDayNr))
   THEN Simulation.LinkCropToSimPeriod := false;

// 9. Initial conditions
READLN(f0); // Info Initial conditions
READLN(f0,TempString);  //SWCIniFile
IF (Trim(TempString) = 'KeepSWC')
   THEN BEGIN
        // No load of soil file (which reset thickness compartments and Soil water content to FC)
        SetSWCIniFile('KeepSWC');
        SWCIniDescription := 'Keep soil water profile of previous run';
        READLN(f0);  //PathSWCIniFile
        END
   ELSE BEGIN
        // start with load and complete profile description (see 5.) which reset SWC to FC by default
        LoadProfile(GetProfFilefull());
        CompleteProfileDescription;

        //Adjust size of compartments if required
        TotDepth := 0;
        FOR i := 1 to NrCompartments DO TotDepth := TotDepth + Compartment[i].Thickness;
        IF Simulation.MultipleRunWithKeepSWC // Project with a sequence of simulation runs and KeepSWC
           THEN BEGIN
                IF (ROUND(Simulation.MultipleRunConstZrx*1000) > ROUND(TotDepth*1000))
                   THEN AdjustSizeCompartments(Simulation.MultipleRunConstZrx);
                END
           ELSE BEGIN
                IF (ROUND(GetCrop().RootMax*1000) > ROUND(TotDepth*1000)) THEN
                   BEGIN
                   IF (ROUND(GetSoil().RootMax*1000) = ROUND(GetCrop().RootMax*1000))
                      THEN AdjustSizeCompartments(GetCrop().RootMax) // no restrictive soil layer
                      ELSE BEGIN // restrictive soil layer
                           IF (ROUND(GetSoil().RootMax*1000) > ROUND(TotDepth*1000))
                              THEN AdjustSizeCompartments(GetSoil().RootMax)
                           END;
                   END;
                END;

        SetSWCIniFile(Trim(TempString));
        IF (GetSWCIniFile() = '(None)')
           THEN BEGIN
                READLN(f0);  //PathSWCIniFile
                SetSWCiniFileFull(GetSWCiniFile()); (* no file *)
                SWCiniDescription := 'Soil water profile at Field Capacity';
                END
           ELSE BEGIN
                READLN(f0,TempString);  //PathSWCIniFile
                TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
                SetSWCiniFileFull(CONCAT(Trim(TempString),GetSWCIniFile()));
                LoadInitialConditions(GetSWCiniFileFull(),SurfaceStorage,Simulation.IniSWC);
                END;
        CASE Simulation.IniSWC.AtDepths OF
             true : TranslateIniPointsToSWProfile(Simulation.IniSWC.NrLoc,Simulation.IniSWC.Loc,Simulation.IniSWC.VolProc,
                                                  Simulation.IniSWC.SaltECe,NrCompartments,Compartment);
             else TranslateIniLayersToSWProfile(Simulation.IniSWC.NrLoc,Simulation.IniSWC.Loc,Simulation.IniSWC.VolProc,
                                                Simulation.IniSWC.SaltECe,NrCompartments,Compartment);
             end;


        IF Simulation.ResetIniSWC THEN // to reset SWC and SALT at end of simulation run
           BEGIN
           FOR i := 1 TO NrCompartments DO
               BEGIN
               Simulation.ThetaIni[i] := Compartment[i].Theta;
               Simulation.ECeIni[i] := ECeComp(Compartment[i]);
               END;
           // ADDED WHEN DESINGNING 4.0 BECAUSE BELIEVED TO HAVE FORGOTTEN - CHECK LATER
           IF (GetManagement_BundHeight() >= 0.01) THEN
              BEGIN
              Simulation.SurfaceStorageIni := SurfaceStorage;
              Simulation.ECStorageIni := ECStorage;
              END;
           END;
        END;

// 10. load the groundwater file if it exists (only possible for Version 4.0 and higher)
IF ((ROUND(10*VersionNr) >= 40) AND (GetGroundWaterFile() <> '(None)')) // the groundwater file is only available in Version 4.0 or higher
   THEN LoadGroundWater(GetGroundWaterFilefull(),Simulation.FromDayNr,ZiAqua,ECiAqua)
   ELSE BEGIN
        ZiAqua := undef_int;
        ECiAqua := undef_int;
        SimulParam.ConstGwt := true;
        END;
CalculateAdjustedFC((ZiAqua/100),Compartment);
//IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
IF (Simulation.IniSWC.AtFC AND (GetSWCIniFile() <> 'KeepSWC')) THEN ResetSWCToFC;

// 11. Off-season conditions
READLN(f0); // Info Off-season conditions
READLN(f0,TempString);  //OffSeasonFile
SetOffSeasonFile(Trim(TempString));
IF (GetOffSeasonFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathOffSeasonFile
        SetOffSeasonFileFull(GetOffSeasonFile());
        OffSeasonDescription := 'No specific off-season conditions';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathOffSeasonFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetOffSeasonFileFull(CONCAT(Trim(TempString),GetOffSeasonFile()));
        LoadOffSeason(GetOffSeasonFilefull());
        END;

// 12. Field data
READLN(f0); // Info Field data
READLN(f0,TempString);  //Field dataFile
SetObservationsFile(Trim(TempString));
IF (GetObservationsFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //Path Field data File
        SetObservationsFileFull(GetObservationsFile());
        SetObservationsDescription('No field observations');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //Path Field data File
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetObservationsFileFull(CONCAT(Trim(TempString),GetObservationsFile()));
        observations_descr := GetObservationsDescription();
        GetFileDescription(GetObservationsFileFull(),observations_descr);
        SetObservationsDescription(observations_descr);
        END;

Close(f0);
END; (* LoadSimulationRunProject *)




PROCEDURE TemperatureFileCoveringCropPeriod(CropFirstDay,CropLastDay : LongInt);
VAR totalname,totalnameOUT : STRING;
    fTemp: text;
    f2 : textFile;
    i,RunningDay : INTEGER;
    StringREAD : ShortString;
    TminDataSet,TmaxDataSet : rep_SimulationEventsDbl;
    Tlow,Thigh : double;

BEGIN
totalname := GetTemperatureFilefull();
IF FileExists(totalname)
   THEN BEGIN
        // open file and find first day of cropping period
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
                       FOR i := GetTemperatureRecord().FromDayNr TO (CropFirstDay - 1) DO READLN(fTemp);
                       READLN(fTemp,StringREAD);  // i.e. Crop.Day1
                       SplitStringInTwoParams(StringREAD,Tlow,Thigh);
                       END;
             Decadely: BEGIN
                       GetDecadeTemperatureDataSet(CropFirstDay,TminDataSet,TmaxDataSet);
                       i := 1;
                       While (TminDataSet[i].DayNr <> CropFirstDay) Do i := i+1;
                       Tlow := TminDataSet[i].Param;
                       Thigh := TmaxDataSet[i].Param;
                       END;
             Monthly : BEGIN
                       GetMonthlyTemperatureDataSet(CropFirstDay,TminDataSet,TmaxDataSet);
                       i := 1;
                       While (TminDataSet[i].DayNr <> CropFirstDay) Do i := i+1;
                       Tlow := TminDataSet[i].Param;
                       Thigh := TmaxDataSet[i].Param;
                       END;
             end;
        // create SIM file and record first day
        totalnameOUT := CONCAT(GetPathNameSimul(),'TCrop.SIM');
        Assign(f2,totalnameOUT);
        Rewrite(f2);
        WRITELN(f2,Tlow:10:4,Thigh:10:4);
        // next days of simulation period
        FOR RunningDay := (CropFirstDay + 1) TO CropLastDay DO
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
                                   SplitStringInTwoParams(StringREAD,Tlow,Thigh);
                                   END
                              ELSE READLN(fTemp,Tlow,Thigh);
                                   END;
                 Decadely: BEGIN
                           IF (RunningDay > TminDataSet[31].DayNr) THEN GetDecadeTemperatureDataSet(RunningDay,TminDataSet,TmaxDataSet);
                           i := 1;
                           While (TminDataSet[i].DayNr <> RunningDay) Do i := i+1;
                           Tlow := TminDataSet[i].Param;
                           Thigh := TmaxDataSet[i].Param;
                           END;
                 Monthly : BEGIN
                           IF (RunningDay > TminDataSet[31].DayNr) THEN GetMonthlyTemperatureDataSet(RunningDay,TminDataSet,TmaxDataSet);
                           i := 1;
                           While (TminDataSet[i].DayNr <> RunningDay) Do i := i+1;
                           Tlow := TminDataSet[i].Param;
                           Thigh := TmaxDataSet[i].Param;
                           END;
                 end;
            WRITELN(f2,Tlow:10:4,Thigh:10:4);
            END;
        // Close files
        IF (GetTemperatureRecord().DataType = Daily) THEN Close(fTemp);
        Close(f2);
        END
   ELSE BEGIN
        // fatal error if no air temperature file
        END;
END; (* TemperatureFileCoveringCropPeriod *)




Function RoundedOffGDD(PeriodGDD,PeriodDay : INTEGER;
                       FirstDayPeriod : LongInt;
                       TempTbase,TempTupper,TempTmin,TempTmax : double) : INTEGER;
VAR DayMatch,PeriodUpdatedGDD : INTEGER;
BEGIN
IF (PeriodGDD > 0)
   THEN BEGIN
        DayMatch := SumCalendarDays(PeriodGDD,FirstDayPeriod,TempTbase,TempTupper,TempTmin,TempTmax);
        PeriodUpdatedGDD := GrowingDegreeDays(PeriodDay,FirstDayPeriod,TempTbase,TempTupper,TempTmin,TempTmax);
        IF (PeriodDay = DayMatch)
           THEN RoundedOffGDD := PeriodGDD
           ELSE RoundedOffGDD := PeriodUpdatedGDD;
        END
   ELSE RoundedOffGDD := GrowingDegreeDays(PeriodDay,FirstDayPeriod,TempTbase,TempTupper,TempTmin,TempTmax);
END; (* RoundedOffGDD *)


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
Simulation.DelayedDays := 0; // required for CalculateETpot
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
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,SimulParam.GDDMethod);
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
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


Simulation.DelayedDays := 0; // required for CalculateETpot
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
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,SimulParam.GDDMethod);
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
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
Simulation.DelayedDays := 0; // required for CalculateETpot
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



PROCEDURE CropStressParametersSoilSalinity(CCxRed,CCdistortion : ShortInt;
                                           CCo,CCx,CGC,GDDCGC : double;
                                           CropDeterm : BOOLEAN;
                                           L12,LFlor,LengthFlor,L123 : INTEGER;
                                           GDDL12,GDDLFlor,GDDLengthFlor,GDDL123 : INTEGER;
                                           TheModeCycle : rep_modeCycle;
                                           VAR StressResponse : rep_EffectStress);
VAR CCToReach,CCxAdj,L12Double,L12SS,CGCadjMax,CGCAdjMin,L12SSmax,CGCadj,CCxFinal,
    GDDL12Double,GDDCGCadjMax,GDDL12SSmax,GDDCGCAdjMin,GDDCGCadj : double;
BEGIN
// initialize
StressResponse.RedCCX := CCxRed;
StressResponse.RedWP := 0;
L12Double := L12;
L12SSmax := L12;
GDDL12Double := GDDL12;

// CGC reduction
CCToReach := 0.98 * CCx;
IF ((CCo > CCToReach) OR (CCo >= CCx) OR (CCxRed = 0))
   THEN StressResponse.RedCGC := 0
   ELSE BEGIN
        StressResponse.RedCGC := undef_int;
        // reference for no salinity stress
        IF (TheModeCycle = CalendarDays)
           THEN BEGIN
                L12Double := LN((0.25*CCx*CCx/CCo)/(CCx-CCToReach))/CGC;
                IF (L12Double <= 0) THEN StressResponse.RedCGC := 0;
                END
           ELSE BEGIN
                GDDL12Double := LN((0.25*CCx*CCx/CCo)/(CCx-CCToReach))/GDDCGC;
                IF (GDDL12Double <= 0) THEN StressResponse.RedCGC := 0;
                END;
        // with salinity stress
        CCxAdj := 0.90 * CCx * (1 - CCxRed/100);
        CCToReach := 0.98 * CCxAdj;
        IF ((StressResponse.RedCGC <> 0) AND ((CCxAdj-CCToReach) >= 0.0001))
           THEN BEGIN
                IF (TheModeCycle = CalendarDays)
                   THEN BEGIN
                        CGCadjMax := LN((0.25*CCxAdj*CCxAdj/CCo)/(CCxAdj-CCToReach))/L12Double;
                        L12SSmax := L12 + (L123 - L12)/2;
                        IF (CropDeterm AND (L12SSmax > (LFlor + ROUND(LengthFlor/2))))
                           THEN L12SSmax := LFlor + ROUND(LengthFlor/2);
                        IF (L12SSmax > L12Double)
                           THEN CGCAdjMin := LN((0.25*CCxAdj*CCxAdj/CCo)/(CCxAdj-CCToReach))/L12SSmax
                           ELSE CGCAdjMin := CGCadjMax;
                        IF (CCxRed < 10) // smooth start required
                           THEN CGCadj := CGCadjMax - (CGCadjMax-CGCAdjMin)*(exp(CCxRed*LN(1.5))/exp(10*LN(1.5)))*(CCdistortion/100)
                           ELSE CGCadj := CGCadjMax - (CGCadjMax-CGCAdjMin)*(CCdistortion/100);
                        StressResponse.RedCGC := ROUND(100*(CGC-CGCadj)/CGC);
                        END
                   ELSE BEGIN
                        GDDCGCadjMax := LN((0.25*CCxAdj*CCxAdj/CCo)/(CCxAdj-CCToReach))/GDDL12Double;
                        GDDL12SSmax := GDDL12 + (GDDL123 - GDDL12)/2;
                        IF (CropDeterm AND (GDDL12SSmax > (GDDLFlor + ROUND(LengthFlor/2))))
                           THEN GDDL12SSmax := GDDLFlor + ROUND(GDDLengthFlor/2);
                        IF (GDDL12SSmax > GDDL12Double)
                           THEN GDDCGCAdjMin := LN((0.25*CCxAdj*CCxAdj/CCo)/(CCxAdj-CCToReach))/GDDL12SSmax
                           ELSE GDDCGCAdjMin := GDDCGCadjMax;
                        IF (CCxRed < 10)  // smooth start required
                           THEN GDDCGCadj := GDDCGCadjMax - (GDDCGCadjMax-GDDCGCAdjMin)*(exp(CCxRed)/exp(10))*(CCdistortion/100)
                           ELSE GDDCGCadj := GDDCGCadjMax - (GDDCGCadjMax-GDDCGCAdjMin)*(CCdistortion/100);
                        StressResponse.RedCGC := ROUND(100*(GDDCGC-GDDCGCadj)/GDDCGC);
                        END;
                END
           ELSE StressResponse.RedCGC := 0;
        END;

// Canopy decline
IF (CCxRed = 0)
   THEN StressResponse.CDecline := 0
   ELSE BEGIN
        CCxAdj := 0.98*CCx*(1 - CCxRed/100);
        L12SS := L12SSmax - (L12SSmax-L12Double) * (CCdistortion/100);
        IF ((L123 > L12SS) AND (CCdistortion > 0))
           THEN BEGIN
                IF (CCxRed < 10)  // smooth start required
                   THEN CCxFinal := CCxAdj - (exp(CCxRed*LN(1.5))/exp(10*LN(1.5)))*(0.5*CCdistortion/100)*(CCxAdj - CCo)
                   ELSE CCxFinal := CCxAdj - (0.5*CCdistortion/100)*(CCxAdj - CCo);
                IF (CCxFinal < CCo) THEN CCxFinal := CCo;
                StressResponse.CDecline := 100*(CCxAdj - CCxFinal)/(L123 - L12SS);
                IF (StressResponse.CDecline > 1) THEN StressResponse.CDecline := 1.0;
                IF (StressResponse.CDecline <= 0) THEN StressResponse.CDecline := 0.001;
                END
           ELSE StressResponse.CDecline := 0.001; // no shift of maturity
        END;

// Stomata closure
StressResponse.RedKsSto := CCxRed;
END; (* CropStressParametersSoilSalinity *)



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
Simulation.DelayedDays := 0; // required for CalculateETpot
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


PROCEDURE AdjustCropFileParameters(TheCropFileSet : rep_CropFileSet;
                                   LseasonDays : INTEGER;
                                   TheCropDay1 : LongInt;
                                   TheModeCycle  : rep_modeCycle;
                                   TheTbase,TheTupper  : double;
                                   VAR L123,L1234,GDD123,GDD1234 : INTEGER);
BEGIN  // Adjust some crop parameters (CROP.*) as specified by the generated length season (LseasonDays)
// time to maturity
L1234 := LseasonDays; // days
IF (TheModeCycle = GDDays)
   THEN GDD1234 := GrowingDegreeDays(LseasonDays,TheCropDay1,TheTbase,TheTupper,SimulParam.Tmin,SimulParam.Tmax)
   ELSE GDD1234 := undef_int;

// time to senescence  (reference is given in TheCropFileSet
IF (TheModeCycle = GDDays)
   THEN BEGIN
        GDD123 := GDD1234 - TheCropFileSet.GDDaysFromSenescenceToEnd;
        L123 := SumCalendarDays(GDD123,TheCropDay1,TheTbase,TheTupper,SimulParam.Tmin,SimulParam.Tmax);
        END
   ELSE BEGIN
        L123 := L1234 - TheCropFileSet.DaysFromSenescenceToEnd;
        GDD123 := undef_int;
        END;
END; (* AdjustCropFileParameters *)


end.
