unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;

PROCEDURE GetDecadeTemperatureDataSet(DayNri : LongInt;
                                      VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
PROCEDURE GetMonthlyTemperatureDataSet(DayNri : LongInt;
                                       VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);



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

PROCEDURE HIadjColdHeat(TempHarvest,TempFlower,TempLengthFlowering,TempHI : INTEGER;
                        TempTmin,TempTmax : double;
                        TempTcold,TempTheat : shortInt;
                        TempfExcess : smallInt;
                        VAR HIadjusted : double;
                        VAR ColdStress,HeatStress : BOOLEAN);

PROCEDURE AdjustCropFileParameters(TheCropFileSet : rep_CropFileSet;
                                   LseasonDays : INTEGER;
                                   TheCropDay1 : LongInt;
                                   TheModeCycle  : rep_modeCycle;
                                   TheTbase,TheTupper  : double;
                                   VAR L123,L1234,GDD123,GDD1234 : INTEGER);



implementation


PROCEDURE GetDecadeTemperatureDataSet(DayNri : LongInt;
                                      VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);

VAR Nri,ni,Dayi,Deci,Monthi,Yeari,DayN : INTEGER;
    DNR : LongInt;
    C1Min,C1Max,C2Min,C2Max,C3Min,C3Max : Double;
    UlMin,LLMin,MidMin,
    UlMax,LLMax,MidMax : double;




PROCEDURE GetSetofThree(DayN,Deci,Monthi,Yeari : INTEGER;
                        VAR C1Min,C1Max,C2Min,C2Max,C3Min,C3Max : double);
VAR fTemp : textfile;
    DecFile,Mfile,Yfile,Nri,Obsi : INTEGER;
    OK3 : BOOLEAN;
    StringREAD : ShortString;

// 1 = previous decade, 2 = Actual decade, 3 = Next decade;
BEGIN
Assign(fTemp,TemperatureFilefull);
Reset(fTemp);
READLN(fTemp); // description
READLN(fTemp); // time step
READLN(fTemp); // day
READLN(fTemp); // month
READLN(fTemp); // year
READLN(fTemp);
READLN(fTemp);
READLN(fTemp);
IF (TemperatureRecord.FromD > 20)
   THEN DecFile := 3
   ELSE IF (TemperatureRecord.FromD > 10)
           THEN DecFile := 2
           ELSE DecFile := 1;
Mfile := TemperatureRecord.FromM;
IF (TemperatureRecord.FromY = 1901) THEN Yfile := Yeari
                                    ELSE Yfile := TemperatureRecord.FromY;
OK3 := false;

IF (TemperatureRecord.NrObs <= 2) THEN
   BEGIN
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C1Min,C1Max);
   //READLN(fTemp,C1Min,C1Max);
   CASE TemperatureRecord.NrObs OF
     1 : BEGIN
         C2Min := C1Min;
         C2Max := C2Max;
         C3Min := C1Min;
         C3Max := C1Max;
         END;
     2 : BEGIN
         DecFile := DecFile + 1;
         IF (DecFile > 3) THEN AdjustDecadeMONTHandYEAR(DecFile,Mfile,Yfile);
         READLN(fTemp,StringREAD);
         SplitStringInTwoParams(StringREAD,C3Min,C3Max);
         //READLN(fTemp,C3Min,C3Max);
         IF (Deci = DecFile)
            THEN BEGIN
                 C2Min := C3Min;
                 C2Max := C3Max;
                 C3Min := C2Min+(C2Min-C1Min)/4;
                 C3Max := C2Max+(C2Max-C1Max)/4;
                 END
            ELSE BEGIN
                 C2Min := C1Min;
                 C2Max := C1Max;
                 C1Min := C2Min + (C2Min-C3Min)/4;
                 C1Max := C2Max + (C2Max-C3Max)/4;
                 END;
         END;
     END;
   OK3 := true;
   END;
IF ((NOT OK3) AND ((Deci = DecFile) AND (Monthi = Mfile) AND (Yeari = Yfile))) THEN
   BEGIN
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C1Min,C1Max);
   //READLN(fTemp,C1Min,C1Max);
   C2Min := C1Min;
   C2Max := C1Max;
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C3Min,C3Max);
   //READLN(fTemp,C3Min,C3Max);
   C1Min := C2Min + (C2Min-C3Min)/4;
   C1Max := C2Max + (C2Max-C3Max)/4;
   OK3 := true;
   END;
IF ((NOT OK3) AND ((DayN = TemperatureRecord.ToD) AND (Monthi = TemperatureRecord.ToM))) THEN
   IF ((TemperatureRecord.FromY = 1901) OR (Yeari = TemperatureRecord.ToY)) THEN
      BEGIN
      FOR Nri := 1 TO (TemperatureRecord.NrObs-2) DO READLN(fTemp);
      READLN(fTemp,StringREAD);
      SplitStringInTwoParams(StringREAD,C1Min,C1Max);
      //READLN(fTemp,C1Min,C1Max);
      READLN(fTemp,StringREAD);
      SplitStringInTwoParams(StringREAD,C2Min,C2Max);
      //READLN(fTemp,C2Min,C2Max);
      C3Min := C2Min+(C2Min-C1Min)/4;
      C3Max := C2Max+(C2Max-C1Max)/4;
      OK3 := true;
      END;
IF (NOT OK3) THEN
   BEGIN
   Obsi := 1;
   REPEAT
     IF ((Deci = DecFile) AND (Monthi = Mfile) AND (Yeari = Yfile))
        THEN OK3 := true
        ELSE BEGIN
             DecFile := DecFile + 1;
             IF (DecFile > 3) THEN AdjustDecadeMONTHandYEAR(DecFile,Mfile,Yfile);
             Obsi := Obsi + 1;
             END;
   UNTIL (OK3);
   IF (TemperatureRecord.FromD > 20)
      THEN DecFile := 3
      ELSE IF (TemperatureRecord.FromD > 10)
              THEN DecFile := 2
              ELSE DecFile := 1;
   FOR Nri := 1 TO (Obsi-2) DO Readln(fTemp);
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C1Min,C1Max);
   //READLN(fTemp,C1Min,C1Max);
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C2Min,C2Max);
   //READLN(fTemp,C2Min,C2Max);
   READLN(fTemp,StringREAD);
   SplitStringInTwoParams(StringREAD,C3Min,C3Max);
   //READLN(fTemp,C3Min,C3Max);
   END;
Close(fTemp);
END; (* GetSetofThree *)


PROCEDURE GetParameters(C1,C2,C3 : double;
                        VAR UL,LL,Mid : double);
BEGIN
UL := (C1+C2)/2;
LL := (C2+C3)/2;
Mid := 2*C2 - (UL+LL)/2;
// --previous decade-->/UL/....... Mid ......../LL/<--next decade--
END; (* GetParameters *)


BEGIN (* GetDecadeTemperatureDataSet *)
DetermineDate(DayNri,Dayi,Monthi,Yeari);
IF (Dayi > 20)
   THEN BEGIN
        Deci := 3;
        Dayi := 21;
        DayN := DaysInMonth[Monthi];
        IF ((Monthi = 2) AND LeapYear(Yeari)) THEN DayN := DayN + 1;
        ni := DayN - Dayi + 1;
        END
   ELSE IF (Dayi > 10)
           THEN BEGIN
                Deci := 2;
                Dayi := 11;
                DayN := 20;
                ni := 10;
                END
           ELSE BEGIN
                Deci := 1;
                Dayi := 1;
                DayN := 10;
                ni := 10;
                END;
GetSetofThree(DayN,Deci,Monthi,Yeari,C1Min,C1Max,C2Min,C2Max,C3Min,C3Max);
DetermineDayNr(Dayi,Monthi,Yeari,DNR);

GetParameters(C1Min,C2Min,C3Min,ULMin,LLMin,MidMin);
For Nri := 1 TO ni DO
    BEGIN
    TMinDataSet[Nri].DayNr := DNR+Nri-1;
    IF (Nri <= (ni/2+0.01))
       THEN TMinDataSet[Nri].Param := (2*ULMin + (MidMin-ULMin)*(2*Nri-1)/(ni/2))/2
       ELSE BEGIN
            IF (((ni = 11) OR (ni = 9)) AND (Nri < (ni+1.01)/2))
               THEN TminDataSet[Nri].Param := MidMin
               ELSE TminDataSet[Nri].Param := (2*MidMin + (LLMin-MidMin)*(2*Nri-(ni+1))/(ni/2))/2;
            END;
    //IF (EToDataSet[Nri].Param < 0) THEN EToDataSet[Nri].Param := 0;
    END;

GetParameters(C1Max,C2Max,C3Max,ULMax,LLMax,MidMax);
For Nri := 1 TO ni DO
    BEGIN
    TMaxDataSet[Nri].DayNr := DNR+Nri-1;
    IF (Nri <= (ni/2+0.01))
       THEN TMaxDataSet[Nri].Param := (2*ULMax + (MidMax-ULMax)*(2*Nri-1)/(ni/2))/2
       ELSE BEGIN
            IF (((ni = 11) OR (ni = 9)) AND (Nri < (ni+1.01)/2))
               THEN TmaxDataSet[Nri].Param := MidMax
               ELSE TmaxDataSet[Nri].Param := (2*MidMax + (LLMax-MidMax)*(2*Nri-(ni+1))/(ni/2))/2;
            END;
    //IF (EToDataSet[Nri].Param < 0) THEN EToDataSet[Nri].Param := 0;
    END;


FOR Nri := (ni+1) TO 31 DO
    BEGIN
    TminDataSet[Nri].DayNr := DNR+ni-1;
    TminDataSet[Nri].Param := 0;
    TmaxDataSet[Nri].DayNr := DNR+ni-1;
    TmaxDataSet[Nri].Param := 0;
    END;
END; (* GetDecadeTemperatureDataSet *)



PROCEDURE GetMonthlyTemperatureDataSet(DayNri : LongInt;
                                       VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
VAR Dayi,Monthi,Yeari,DayN : INTEGER;
    DNR : LongInt;
    X1,X2,X3,t1,t2 : INTEGER;
    C1Min,C2Min,C3Min : Double;
    C1Max,C2Max,C3Max : Double;
    aOver3Min,bOver2Min,cMin : extended;
    aOver3Max,bOver2Max,cMax : extended;


PROCEDURE GetSetofThreeMonths(Monthi,Yeari : INTEGER;
                        VAR C1Min,C2Min,C3Min,
                            C1Max,C2Max,C3Max : double;
                        VAR X1,X2,X3,t1 : INTEGER);
VAR fTemp : textfile;
    Mfile,Yfile,n1,n2,n3,Nri,Obsi : INTEGER;
    OK3 : BOOLEAN;
    StringREAD : ShortString;



    PROCEDURE ReadMonth(Mfile,Yfile : INTEGER;
                        VAR ni : INTEGER;
                        VAR CiMin,CiMax : double);
    BEGIN
    READLN(fTemp,StringREAD);
    SplitStringInTwoParams(StringREAD,CiMin,CiMax);
    //READLN(fTemp,CiMin,CiMax);
    ni := 30; // simplification give better results for all cases
    //ni := DaysInMonth[Mfile];
    //IF ((Mfile = 2) AND LeapYear(Yfile)) THEN ni := ni + 1;
    CiMin := CiMin * ni;
    CiMax := CiMax * ni;
    END; (* ReadMonth *)

BEGIN
//1. Prepare record
Assign(fTemp,TemperatureFilefull);
Reset(fTemp);
READLN(fTemp); // description
READLN(fTemp); // time step
READLN(fTemp); // day
READLN(fTemp); // month
READLN(fTemp); // year
READLN(fTemp);
READLN(fTemp);
READLN(fTemp);
Mfile := TemperatureRecord.FromM;
IF (TemperatureRecord.FromY = 1901) THEN Yfile := Yeari
                                    ELSE Yfile := TemperatureRecord.FromY;
OK3 := false;

//2. IF 3 or less records
IF (TemperatureRecord.NrObs <= 3) THEN
   BEGIN
   ReadMonth(Mfile,Yfile,n1,C1Min,C1Max);
   X1 := n1;
   CASE TemperatureRecord.NrObs OF
     1 : BEGIN
         t1 := X1;
         X2 := X1 + n1;
         C2Min := C1Min;
         C2Max := C1Max;
         X3 := X2 + n1;
         C3Min := C1Min;
         C3Max := C1Max;
         END;
     2 : BEGIN
         t1 := X1;
         Mfile := Mfile + 1;
         IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
         ReadMonth(Mfile,Yfile,n3,C3Min,C3Max);
         IF (Monthi = Mfile)
            THEN BEGIN
                 C2Min := C3Min;
                 C2Max := C3Max;
                 X2 := X1 + n3;
                 X3 := X2 + n3;
                 END
            ELSE BEGIN
                 C2Min := C1Min;
                 C2Max := C1Max;
                 X2 := X1 + n1;
                 X3 := X2 + n3;
                 END;
         END;
     3 : BEGIN
         IF (Monthi = Mfile) THEN t1 := 0;
         Mfile := Mfile + 1;
         IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
         ReadMonth(Mfile,Yfile,n2,C2Min,C2Max);
         X2 := X1 + n2;
         IF (Monthi = Mfile) THEN t1 := X1;
         Mfile := Mfile + 1;
         IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
         ReadMonth(Mfile,Yfile,n3,C3Min,C3Max);
         X3 := X2 + n3;
         IF (Monthi = Mfile) THEN t1 := X2;
         END;
     END;
   OK3 := true;
   END;

//3. If first observation
IF ((NOT OK3) AND ((Monthi = Mfile) AND (Yeari = Yfile))) THEN
   BEGIN
   t1 := 0;
   ReadMonth(Mfile,Yfile,n1,C1Min,C1Max);
   X1 := n1;
   Mfile := Mfile + 1;
   IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
   ReadMonth(Mfile,Yfile,n2,C2Min,C2Max);
   X2 := X1 + n2;
   Mfile := Mfile + 1;
   IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
   ReadMonth(Mfile,Yfile,n3,C3Min,C3Max);
   X3 := X2 + n3;
   OK3 := true;
   END;

//4. If last observation
IF ((NOT OK3) AND (Monthi = TemperatureRecord.ToM)) THEN
   IF ((TemperatureRecord.FromY = 1901) OR (Yeari = TemperatureRecord.ToY)) THEN
      BEGIN
      FOR Nri := 1 TO (TemperatureRecord.NrObs-3) DO
          BEGIN
          READLN(fTemp);
          Mfile := Mfile + 1;
          IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
          END;
      ReadMonth(Mfile,Yfile,n1,C1Min,C1Max);
      X1 := n1;
      Mfile := Mfile + 1;
      IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
      ReadMonth(Mfile,Yfile,n2,C2Min,C2Max);
      X2 := X1 + n2;
      t1 := X2;
      Mfile := Mfile + 1;
      IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
      ReadMonth(Mfile,Yfile,n3,C3Min,C3Max);
      X3 := X2 + n3;
      OK3 := true;
      END;

//5. IF not previous cases
IF (NOT OK3) THEN
   BEGIN
   Obsi := 1;
   REPEAT
     IF ((Monthi = Mfile) AND (Yeari = Yfile))
        THEN OK3 := true
        ELSE BEGIN
             Mfile := Mfile + 1;
             IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
             Obsi := Obsi + 1;
             END;
   UNTIL (OK3);
   Mfile := TemperatureRecord.FromM;
   FOR Nri := 1 TO (Obsi-2) DO
       BEGIN
       Readln(fTemp);
       Mfile := Mfile + 1;
       IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
       END;
   ReadMonth(Mfile,Yfile,n1,C1Min,C1Max);
   X1 := n1;
   t1 := X1;
   Mfile := Mfile + 1;
   IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
   ReadMonth(Mfile,Yfile,n2,C2Min,C2Max);
   X2 := X1 + n2;
   Mfile := Mfile + 1;
   IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
   ReadMonth(Mfile,Yfile,n3,C3Min,C3Max);
   X3 := X2 + n3;
   END;

Close(fTemp);
END; (* GetSetofThreeMonths *)


PROCEDURE GetInterpolationParameters(C1,C2,C3 : double;
                                     X1,X2,X3 : INTEGER;
                                     VAR aOver3,bOver2,c : extended);
BEGIN //n1=n2=n3=30 --> better parabola
aOver3 := (C1-2*C2+C3)/(6*30*30*30);
bOver2 := (-6*C1+9*C2-3*C3)/(6*30*30);
c := (11*C1-7*C2+2*C3)/(6*30);
END; (* GetInterpolationParameters *)


BEGIN (* GetMonthlyTemperatureDataSet *)
DetermineDate(DayNri,Dayi,Monthi,Yeari);
GetSetofThreeMonths(Monthi,Yeari,C1Min,C2Min,C3Min,C1Max,C2Max,C3Max,X1,X2,X3,t1);

Dayi := 1;
DetermineDayNr(Dayi,Monthi,Yeari,DNR);
DayN := DaysInMonth[Monthi];
IF ((Monthi = 2) AND LeapYear(Yeari)) THEN DayN := DayN + 1;

GetInterpolationParameters(C1Min,C2Min,C3Min,X1,X2,X3,aOver3Min,bOver2Min,cMin);
GetInterpolationParameters(C1Max,C2Max,C3Max,X1,X2,X3,aOver3Max,bOver2Max,cMax);
For Dayi := 1 TO DayN DO
    BEGIN
    t2 := t1 + 1;
    TminDataSet[Dayi].DayNr := DNR+Dayi-1;
    TmaxDataSet[Dayi].DayNr := DNR+Dayi-1;
    TminDataSet[Dayi].Param := aOver3Min*(t2*t2*t2-t1*t1*t1)
                              + bOver2Min*(t2*t2-t1*t1) + cMin*(t2-t1);
    TmaxDataSet[Dayi].Param := aOver3Max*(t2*t2*t2-t1*t1*t1)
                              + bOver2Max*(t2*t2-t1*t1) + cMax*(t2-t1);
    //IF (EToDataSet[Dayi].Param < 0) THEN EToDataSet[Dayi].Param := 0;
    t1 := t2;
    END;
FOR Dayi := (DayN+1) TO 31 DO
    BEGIN
    TminDataSet[Dayi].DayNr := DNR+DayN-1;
    TmaxDataSet[Dayi].DayNr := DNR+DayN-1;
    TminDataSet[Dayi].Param := 0;
    TmaxDataSet[Dayi].Param := 0;
    END;
END; (* GetMonthlyTemperatureDataSet *)





PROCEDURE SetDayNrToYundef(VAR DayNri : LongInt);
VAR Dayi,Monthi,Yeari : INTEGER;
BEGIN
DetermineDate(DayNri,Dayi,Monthi,Yeari);
Yeari := 1901;
DetermineDayNr(Dayi,Monthi,Yeari,DayNri);
END; (* SetDayNrToYundef *)



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
   IF (TemperatureFile = '(None)')
      THEN BEGIN  // given average Tmin and Tmax
           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
           GDDays := ROUND(ValPeriod * DayGDD)
           END
      ELSE BEGIN   // temperature file
           DayNri := FirstDayPeriod;
           IF  FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,
                                   TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM)
               THEN BEGIN
                    AdjustDayNri := true;
                    SetDayNrToYundef(DayNri);
                    END
               ELSE AdjustDayNri := false;
           totalname := TemperatureFilefull;
           IF (FileExists(totalname) AND (TemperatureRecord.ToDayNr > DayNri)
                                     AND (TemperatureRecord.FromDayNr <= DayNri))
                THEN BEGIN
                     (*AdjustDayNri := FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,
                                           TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM);*)
                     RemainingDays := ValPeriod;
                     CASE TemperatureRecord.DataType OF
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
                                    FOR i := TemperatureRecord.FromDayNr TO (DayNri - 1) DO READLN(fTemp);
                                    READLN(fTemp,StringREAD);
                                    SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                    //READLN(fTemp,TDayMin,TDayMax);
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    GDDays := GDDays + DayGDD;
                                    RemainingDays := RemainingDays - 1;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingDays > 0)
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
   IF (TemperatureFile = '(None)')
      THEN BEGIN  // given average Tmin and Tmax
           DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
           IF (DayGDD = 0)
              THEN NrCDays := -9
              ELSE NrCDays := ROUND(ValGDDays/DayGDD);
           END
      ELSE BEGIN
           DayNri := FirstDayCrop;
           IF  FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,
                                   TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM)
               THEN BEGIN
                    AdjustDayNri := true;
                    SetDayNrToYundef(DayNri);
                    END
               ELSE AdjustDayNri := false;
           totalname := TemperatureFilefull;
           IF (FileExists(totalname) AND (TemperatureRecord.ToDayNr > DayNri)
                                     AND (TemperatureRecord.FromDayNr <= DayNri))
                THEN BEGIN
                     (*
                     AdjustDayNri := FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,
                                           TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM);
                                           *)
                     RemainingGDDays := ValGDDays;
                     CASE TemperatureRecord.DataType OF
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
                                    FOR i := TemperatureRecord.FromDayNr TO (DayNri - 1) DO READLN(fTemp);
                                    READLN(fTemp,StringREAD);
                                    SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                                    //READLN(fTemp,TDayMin,TDayMax);
                                    DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                                    NrCDays := NrCDays + 1;
                                    RemainingGDDays := RemainingGDDays - DayGDD;
                                    DayNri := DayNri + 1;
                                    WHILE ((RemainingGDDays > 0)
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
                                          AND ((DayNri < TemperatureRecord.ToDayNr) or AdjustDayNri)) DO
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
IF (TemperatureFile = '(None)')
   THEN BEGIN
        DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
        If (DayGDD <= 0) THEN MaxGDDays := 0;
        END
   ELSE BEGIN
        MaxGDDays := 0;
        IF FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,
           TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM)
           THEN FromDayNr := TemperatureRecord.FromDayNr  // since we have 365 days anyway
           ELSE BEGIN
             //DetermineDate(CropDay1,dayi,monthi,yeari);
             //yeari := TemperatureRecord.FromY;
             //DetermineDayNr(dayi,monthi,yeari,CropDay1);
                END;
           DayNri := FromDayNr;
           totalname := TemperatureFilefull;
           IF (FileExists(totalname) AND (TemperatureRecord.ToDayNr > FromDayNr)
                             AND (TemperatureRecord.FromDayNr <= FromDayNr)) THEN
           CASE TemperatureRecord.DataType OF
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
                          FOR i := TemperatureRecord.FromDayNr TO (FromDayNr - 1) DO READLN(fTemp);
                          READLN(fTemp,StringREAD);
                          SplitStringInTwoParams(StringREAD,TDayMin,TDayMax);
                          DayNri := DayNri + 1;
                          DayGDD := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
                          MaxGDDays := MaxGDDays + DayGDD;
                          WHILE (DayNri < TemperatureRecord.ToDayNr) DO
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
                          WHILE(DayNri < TemperatureRecord.ToDayNr) DO
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
                          WHILE(DayNri < TemperatureRecord.ToDayNr) DO
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
IF (TemperatureRecord.FromY = 1901)
   THEN BEGIN
        yeari := 1901;
        DetermineDayNr(Dayi,Monthi,Yeari,CropDay1OUT);
        END
   ELSE BEGIN
        IF SwitchToYear1
           THEN DetermineDayNr(Dayi,Monthi,TemperatureRecord.FromY,CropDay1OUT)
           ELSE CropDay1OUT := CropDay1IN;
        END;
ResetCropDay1 := CropDay1OUT;
END; (* ResetCropDay1 *)


PROCEDURE AdjustCalendarCrop(FirstCropDay : LongInt);
VAR succes : BOOLEAN;
    CGCisGiven : BOOLEAN;
BEGIN
CGCisGiven := true;
CASE Crop.ModeCycle OF
     GDDays : BEGIN
              Crop.GDDaysToFullCanopy := Crop.GDDaysToGermination +
                 ROUND(LN((0.25*Crop.CCx*Crop.CCx/Crop.CCo)/(Crop.CCx-(0.98*Crop.CCx)))/Crop.GDDCGC);
              IF (Crop.GDDaysToFullCanopy > Crop.GDDaysToHarvest)
                 THEN Crop.GDDaysToFullCanopy := Crop.GDDaysToHarvest;
              AdjustCalendarDays(FirstCropDay,Crop.subkind,Crop.Tbase,Crop.Tupper,SimulParam.Tmin,SimulParam.Tmax,
                 Crop.GDDaysToGermination,Crop.GDDaysToFullCanopy,Crop.GDDaysToFlowering,
                 Crop.GDDLengthFlowering,Crop.GDDaysToSenescence,Crop.GDDaysToHarvest,Crop.GDDaysToMaxRooting,
                 Crop.GDDaysToHIo,
                 Crop.GDDCGC,Crop.GDDCDC,Crop.CCo,Crop.CCx,CGCisGiven,Crop.HI,
                 Crop.DaysToCCini,Crop.Planting,
                 Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToFlowering,Crop.LengthFlowering,
                 Crop.DaysToSenescence,Crop.DaysToHarvest,Crop.DaysToMaxRooting,Crop.DaysToHIo,
                 Crop.Length,Crop.CGC,Crop.CDC,Crop.dHIdt,Succes);
              END;
     else Succes := true;
     end;
IF NOT Succes THEN ; // GEEN IDEE WAT TE DOEN
END; (* AdjustCalendarCrop *)



PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);
VAR f0,fClim : TextFile;
    TempString,TempString1,TempString2 : string;
    TempSimDayNr1,TempSimDayNrN : LongInt;
    i,Runi : ShortInt;
    TotDepth : double;
    VersionNr : double;

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
READLN(f0,Crop.Day1); //First day of cropping period
READLN(f0,Crop.DayN); //Last day of cropping period


// 1. Climate
READLN(f0); // Info Climate
READLN(f0,TempString);  //ClimateFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetClimateFile(Trim(TempString));
IF (GetClimateFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathClimateFile
        ClimateFileFull := GetClimateFile();
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathClimateFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        ClimateFileFull := CONCAT(Trim(TempString),GetClimateFile());
        Assign(fClim,ClimateFileFull);
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
TemperatureFile:= Trim(TempString);
IF (TemperatureFile = '(None)')
   THEN BEGIN
        READLN(f0);  //PathTemperatureFile
        TemperatureFilefull := TemperatureFile;  (* no file *)
        Str(SimulParam.Tmin:8:1,TempString1);
        Str(SimulParam.Tmax:8:1,TempString2);
        TemperatureDescription := CONCAT('Default temperature data: Tmin = ',
                    trim(TempString1),' and Tmax = ',trim(TempString2),' °C');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathTemperatureFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        TemperatureFileFull := CONCAT(Trim(TempString),Trim(TemperatureFile));
        LoadClim(TemperatureFilefull,TemperatureDescription,TemperatureRecord);
        CompleteClimateDescription(TemperatureRecord);
        END;
// 1.2 ETo
READLN(f0); // Info ETo
READLN(f0,TempString);  //EToFile
SetEToFile(Trim(TempString));
IF (GetEToFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathETo
        SetEToFilefull(GetEToFile());  (* no file *)
        EToDescription := 'Specify ETo data when Running AquaCrop';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathETo
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetEToFilefull(CONCAT(Trim(TempString),GetEToFile()));
        LoadClim(GetEToFilefull(),EToDescription,EToRecord);
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
        RainDescription := 'Specify Rain data when Running AquaCrop';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathRain
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetRainFileFull(CONCAT(Trim(TempString),GetRainFile()));
        LoadClim(GetRainFilefull(),RainDescription,RainRecord);
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
        CO2FileFull := CONCAT(Trim(TempString),GetCO2File());
        GetCO2Description(CO2FileFull,CO2Description);
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
        CalendarFilefull := CONCAT(Trim(TempString),GetCalendarFile());
        GetFileDescription(CalendarFilefull,CalendarDescription);
        END;

// 3. Crop
Simulation.LinkCropToSimPeriod := true;
READLN(f0); // Info Crop
READLN(f0,TempString);  //CropFile
SetCropFile(Trim(TempString));
READLN(f0,TempString);  //PathCropFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
CropFilefull := CONCAT(Trim(TempString),GetCropFile());
LoadCrop(CropFilefull);

// Adjust crop parameters of Perennials
IF (Crop.subkind = Forage) THEN
   BEGIN
   // adjust crop characteristics to the Year (Seeding/Planting or Non-seesing/Planting year)
   AdjustYearPerennials(Simulation.YearSeason,Crop.SownYear1,Crop.ModeCycle,Crop.RootMax,Crop.RootMinYear1,
                     Crop.CCo,Crop.SizeSeedling,Crop.CGC,Crop.CCx,Crop.GDDCGC,Crop.PlantingDens,
                     Crop.Planting,Crop.RootMin,Crop.SizePlant,Crop.CCini,
                     Crop.DaysToCCini,Crop.GDDaysToCCini);
   // adjust length of season
   Crop.DaysToHarvest := Crop.DayN - Crop.Day1 + 1;
   AdjustCropFileParameters(GetCropFileSet(),(Crop.DaysToHarvest),Crop.Day1,Crop.ModeCycle,Crop.Tbase,Crop.Tupper,
                                    Crop.DaysToSenescence,Crop.DaysToHarvest,
                                    Crop.GDDaysToSenescence,Crop.GDDaysToHarvest);
   END;

AdjustCalendarCrop(Crop.Day1);
CompleteCropDescription;
//Onset.Off := true;
IF (GetClimFile() = '(None)')
   THEN AdjustCropYearToClimFile(Crop.Day1,Crop.DayN) // adjusting Crop.Day1 and Crop.DayN to ClimFile
   ELSE Crop.DayN := Crop.Day1 + Crop.DaysToHarvest - 1;
(* adjusting ClimRecord.'TO' for undefined year with 365 days *)
IF ((GetClimFile() <> '(None)') AND (ClimRecord.FromY = 1901)
   AND (ClimRecord.NrObs = 365)) THEN AdjustClimRecordTo(Crop.DayN);
(* adjusting simulation period *)
AdjustSimPeriod;

// 4. Irrigation
READLN(f0); // Info Irrigation
READLN(f0,TempString);  //IrriFile
SetIrriFile(Trim(TempString));
IF (GetIrriFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathIrriFile
        IrriFileFull := GetIrriFile();
        NoIrrigation;
        //IrriDescription := 'Rainfed cropping';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathIrriFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        IrriFilefull := CONCAT(Trim(TempString),GetIrriFile());
        LoadIrriScheduleInfo(IrriFilefull);
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
        TimeToMaxCanopySF(Crop.CCo,Crop.CGC,Crop.CCx,Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,
                          Crop.DaysToFlowering,Crop.LengthFlowering,Crop.DeterminancyLinked,
                          Crop.DaysToFullCanopySF,Simulation.EffectStress.RedCGC,
                          Simulation.EffectStress.RedCCX,Management.FertilityStress);
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
GroundWaterFile := Trim(TempString);
IF (GroundWaterFile = '(None)')
   THEN BEGIN
        READLN(f0);  //PathGroundWaterFile
        GroundWaterFilefull := GroundWaterFile;
        GroundWaterDescription := 'no shallow groundwater table';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathGroundWaterFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        GroundWaterFilefull := CONCAT(Trim(TempString),GroundWaterFile);
        // Loading the groundwater is done after loading the soil profile (see 9.)
        END;


// 8. Set simulation period
Simulation.FromDayNr := TempSimDayNr1;
Simulation.ToDayNr := TempSimDayNrN;
IF ((Crop.Day1 <> Simulation.FromDayNr) OR (Crop.DayN <> Simulation.ToDayNr))
   THEN Simulation.LinkCropToSimPeriod := false;

// 9. Initial conditions
READLN(f0); // Info Initial conditions
READLN(f0,TempString);  //SWCIniFile
IF (Trim(TempString) = 'KeepSWC')
   THEN BEGIN
        // No load of soil file (which reset thickness compartments and Soil water content to FC)
        SWCIniFile := 'KeepSWC';
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
                IF (ROUND(Crop.RootMax*1000) > ROUND(TotDepth*1000)) THEN
                   BEGIN
                   IF (ROUND(Soil.RootMax*1000) = ROUND(Crop.RootMax*1000))
                      THEN AdjustSizeCompartments(Crop.RootMax) // no restrictive soil layer
                      ELSE BEGIN // restrictive soil layer
                           IF (ROUND(Soil.RootMax*1000) > ROUND(TotDepth*1000))
                              THEN AdjustSizeCompartments(Soil.RootMax)
                           END;
                   END;
                END;

        SWCIniFile := Trim(TempString);
        IF (SWCIniFile = '(None)')
           THEN BEGIN
                READLN(f0);  //PathSWCIniFile
                SWCiniFileFull := SWCiniFile; (* no file *)
                SWCiniDescription := 'Soil water profile at Field Capacity';
                END
           ELSE BEGIN
                READLN(f0,TempString);  //PathSWCIniFile
                TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
                SWCiniFileFull := CONCAT(Trim(TempString),SWCIniFile);
                LoadInitialConditions(SWCiniFileFull,SurfaceStorage,Simulation.IniSWC);
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
           IF (Management.BundHeight >= 0.01) THEN
              BEGIN
              Simulation.SurfaceStorageIni := SurfaceStorage;
              Simulation.ECStorageIni := ECStorage;
              END;
           END;
        END;

// 10. load the groundwater file if it exists (only possible for Version 4.0 and higher)
IF ((ROUND(10*VersionNr) >= 40) AND (GroundWaterFile <> '(None)')) // the groundwater file is only available in Version 4.0 or higher
   THEN LoadGroundWater(GroundWaterFilefull,Simulation.FromDayNr,ZiAqua,ECiAqua)
   ELSE BEGIN
        ZiAqua := undef_int;
        ECiAqua := undef_int;
        SimulParam.ConstGwt := true;
        END;
CalculateAdjustedFC((ZiAqua/100),Compartment);
//IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
IF (Simulation.IniSWC.AtFC AND (SWCIniFile <> 'KeepSWC')) THEN ResetSWCToFC;

// 11. Off-season conditions
READLN(f0); // Info Off-season conditions
READLN(f0,TempString);  //OffSeasonFile
OffSeasonFile := Trim(TempString);
IF (OffSeasonFile = '(None)')
   THEN BEGIN
        READLN(f0);  //PathOffSeasonFile
        OffSeasonFileFull := OffSeasonFile;
        OffSeasonDescription := 'No specific off-season conditions';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathOffSeasonFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        OffSeasonFileFull := CONCAT(Trim(TempString),OffSeasonFile);
        LoadOffSeason(OffSeasonFilefull);
        END;

// 12. Field data
READLN(f0); // Info Field data
READLN(f0,TempString);  //Field dataFile
ObservationsFile := Trim(TempString);
IF (ObservationsFile = '(None)')
   THEN BEGIN
        READLN(f0);  //Path Field data File
        ObservationsFileFull := ObservationsFile;
        ObservationsDescription := 'No field observations';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //Path Field data File
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        ObservationsFileFull := CONCAT(Trim(TempString),ObservationsFile);
        GetFileDescription(ObservationsFileFull,ObservationsDescription);
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
totalname := TemperatureFilefull;
IF FileExists(totalname)
   THEN BEGIN
        // open file and find first day of cropping period
        CASE TemperatureRecord.DataType OF
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
                       FOR i := TemperatureRecord.FromDayNr TO (CropFirstDay - 1) DO READLN(fTemp);
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
        totalnameOUT := CONCAT(PathNameSimul,'TCrop.SIM');
        Assign(f2,totalnameOUT);
        Rewrite(f2);
        WRITELN(f2,Tlow:10:4,Thigh:10:4);
        // next days of simulation period
        FOR RunningDay := (CropFirstDay + 1) TO CropLastDay DO
            BEGIN
            CASE TemperatureRecord.DataType OF
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
        IF (TemperatureRecord.DataType = Daily) THEN Close(fTemp);
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
IF (TemperatureFile <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(PathNameSimul,'TCrop.SIM'));
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
    IF (TemperatureFile <> '(None)')
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
IF (TemperatureFile <> '(None)') THEN Close(fTemp);
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
     CCiAdjusted : double;

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
   Assign(fOUT,CONCAT(PathNameSimul,'TestBio.SIM'));
   Rewrite(fOUT);
   END;


//2. Open Temperature file
IF (TemperatureFile <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(PathNameSimul,'TCrop.SIM'));
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
    IF (TemperatureFile <> '(None)')
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
                  THEN WeedCorrection := GetWeedRC(DayCC,SumGDDforPlot,fCCx,WeedStress,Management.WeedAdj,
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
    IF (((Crop.subkind = Tuber) OR (Crop.Subkind = grain)) AND (WPyield < 100) AND (Dayi > LFlor)) THEN
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
IF (TemperatureFile <> '(None)') THEN Close(fTemp);

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

VAR fTemp : textFile;
    StressMatrix : ARRAY[0..7] of StressIndexes;
    Si,Stepi : ShortInt;
    L12SF,GDDL12SF,Dayi : INTEGER;
    StressResponse : rep_EffectStress;
    RatDGDD,SumGDD,T0dayi,TXdayi,GDDi,SumTporNor,CCi,
    CCxWitheredForB,TpotForB,EpotTotForB,KsB,SumTpot,TpotSeason,BNor,WPi,BNor100,
    Yavg,X1avg,X2avg,y,x1,x2,x1y,x2y,x1Sq,x2Sq,x1x2,SUMx1y,SUMx2y,SUMx1Sq,SUMx2Sq,SUMx1x2 : double;
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

VAR SumKcTop,HIGC,HIGClinear,fSwitch : double;
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
IF ((Crop.subkind = Tuber) OR (Crop.Subkind = grain)) THEN //DaysToFlowering corresponds with Tuberformation
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


PROCEDURE HIadjColdHeat(TempHarvest,TempFlower,TempLengthFlowering,TempHI : INTEGER;
                        TempTmin,TempTmax : double;
                        TempTcold,TempTheat : shortInt;
                        TempfExcess : smallInt;
                        VAR HIadjusted : double;
                        VAR ColdStress,HeatStress : BOOLEAN);
CONST TempRange = 5;
VAR  fTemp : textFile;
     Dayi : INTEGER;
     Tndayi,Txdayi,KsPol,KsPolCS,KsPolHS,fFlor : double;


 FUNCTION FractionFlowering(Dayi : INTEGER) : double;
    VAR f1,f2,F : double;
        DiFlor : INTEGER;

        FUNCTION FractionPeriod(DiFlor : INTEGER) : double;
        VAR fi,TimePerc : double;
        BEGIN
        IF (DiFlor <= 0)
           THEN fi := 0
           ELSE BEGIN
                TimePerc := 100 * (DiFlor/TempLengthFlowering);
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
    IF (TempLengthFlowering <=1)
       THEN F := 1
       ELSE BEGIN
            DiFlor := Dayi;
            f2 := FractionPeriod(DiFlor);
            DiFlor := Dayi-1;
            f1 := FractionPeriod(DiFlor);
            IF (ABS(f1-f2) < 0.0000001)
               THEN F := 0
               ELSE F := ((f1+f2)/2)* 100/TempLengthFlowering;
            END;
    FractionFlowering := F;
    END; (* FractionFlowering *)



BEGIN
//1. Open Temperature file
IF (TemperatureFile <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(PathNameSimul,'TCrop.SIM'));
   Reset(fTemp);
   FOR Dayi := 1 TO (TempFlower-1) DO READLN(fTemp);
   END;

//2. Initialize
HIadjusted := 0;
ColdStress := false;
HeatStress := false;

//3. Cold or Heat stress affecting pollination
FOR Dayi := 1 TO TempLengthFlowering DO
    BEGIN
    // 3.1 Read air temperature
    IF (TemperatureFile <> '(None)')
       THEN READLN(fTemp,Tndayi,Txdayi)
       ELSE BEGIN
            Tndayi := TempTmin;
            Txdayi := TempTmax;
            END;
    // 3.2 Fraction of flowers which are flowering on day  (fFlor)
    fFlor := FractionFlowering(dayi);
    // 3.3 Ks(pollination) cold stress
    KsPolCS := KsTemperature((TempTcold-TempRange),TempTcold,Tndayi);
    IF (ROUND(10000*KsPolCS) < 10000) THEN ColdStress := true;
    // 3.4 Ks(pollination) heat stress
    KsPolHS := KsTemperature((TempTheat+TempRange),TempTheat,Txdayi);
    IF (ROUND(10000*KsPolHS) < 10000) THEN HeatStress := true;
    // 3.5 Adjust HI
    KsPol := 1;
    IF (KsPol > KsPolCS) THEN KsPol := KsPolCS;
    IF (KsPol > KsPolHS) THEN KsPol := KsPolHS;
    HIadjusted := HIadjusted + (KsPol * (1 + TempfExcess/100) * fFlor * TempHI);
    IF (HIadjusted > TempHI) THEN HIadjusted := TempHI;
    END;

//3. Close Temperature file
IF (TemperatureFile <> '(None)') THEN Close(fTemp);

END; (* HIadjColdHeat *)


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
