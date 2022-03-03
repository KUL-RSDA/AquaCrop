unit ClimProcessing;

interface

uses Global, interface_global, interface_climprocessing;


PROCEDURE GetDecadeEToDataSet(DayNri : LongInt;
                              VAR EToDataSet : rep_SimulationEventsDbl);
PROCEDURE GetDecadeRainDataSet(DayNri : LongInt;
                               VAR RainDataSet : rep_SimulationEventsDbl);
PROCEDURE GetMonthlyRainDataSet(DayNri : LongInt;
                                VAR RainDataSet : rep_SimulationEventsDbl);


implementation



PROCEDURE GetDecadeEToDataSet(DayNri : LongInt;
                              VAR EToDataSet : rep_SimulationEventsDbl);
VAR Nri,ni,Dayi,Deci,Monthi,Yeari,DayN : INTEGER;
    DNR : LongInt;
    C1,C2,C3 : Double;
    Ul,LL,Mid : double;


PROCEDURE GetSetofThree(DayN,Deci,Monthi,Yeari : INTEGER;
                        VAR C1,C2,C3 : double);
VAR fETo : textfile;
    DecFile,Mfile,Yfile,Nri,Obsi : INTEGER;
    OK3 : BOOLEAN;


// 1 = previous decade, 2 = Actual decade, 3 = Next decade;
BEGIN
Assign(fETo,GetEToFilefull());
Reset(fETo);
READLN(fETo); // description
READLN(fETo); // time step
READLN(fETo); // day
READLN(fETo); // month
READLN(fETo); // year
READLN(fETo);
READLN(fETo);
READLN(fETo);

IF (GetEToRecord_FromD() > 20)
   THEN DecFile := 3
   ELSE IF (GetEToRecord_FromD() > 10)
           THEN DecFile := 2
           ELSE DecFile := 1;
Mfile := GetEToRecord_FromM();
IF (GetEToRecord_FromY() = 1901) THEN Yfile := Yeari
                            ELSE Yfile := GetEToRecord_FromY();
OK3 := false;

IF (GetEToRecord_NrObs() <= 2) THEN
   BEGIN
   READLN(fETo,C1);
   CASE GetEToRecord_NrObs() OF
     1 : BEGIN
         C2 := C1;
         C3 := C1;
         END;
     2 : BEGIN
         DecFile := DecFile + 1;
         IF (DecFile > 3) THEN AdjustDecadeMONTHandYEAR(DecFile,Mfile,Yfile);
         READLN(fETo,C3);
         IF (Deci = DecFile)
            THEN BEGIN
                 C2 := C3;
                 C3 := C2+(C2-C1)/4;
                 END
            ELSE BEGIN
                 C2 := C1;
                 C1 := C2 + (C2-C3)/4;
                 END;
         END;
     END;
   OK3 := true;
   END;
IF ((NOT OK3) AND ((Deci = DecFile) AND (Monthi = Mfile) AND (Yeari = Yfile))) THEN
   BEGIN
   READLN(fETo,C1);
   C2 := C1;
   READLN(fETo,C3);
   C1 := C2 + (C2-C3)/4;
   OK3 := true;
   END;
IF ((NOT OK3) AND ((DayN = GetEToRecord_ToD()) AND (Monthi = GetEToRecord_ToM()))) THEN
   IF ((GetEToRecord_FromY() = 1901) OR (Yeari = GetEToRecord_ToY())) THEN
      BEGIN
      FOR Nri := 1 TO (GetEToRecord_NrObs()-2) DO READLN(fETo);
      READLN(fETo,C1);
      READLN(fETo,C2);
      C3 := C2+(C2-C1)/4;
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
   IF (GetEToRecord_FromD() > 20)
      THEN DecFile := 3
      ELSE IF (GetEToRecord_FromD() > 10)
              THEN DecFile := 2
              ELSE DecFile := 1;
   FOR Nri := 1 TO (Obsi-2) DO Readln(fETo);
   READLN(fETo,C1);
   READLN(fETo,C2);
   READLN(fETo,C3);
   END;
Close(fETo);
END; (* GetSetofThree *)


BEGIN (* GetDecadeEToDataSet *)
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
GetSetofThree(DayN,Deci,Monthi,Yeari,C1,C2,C3);
DetermineDayNr(Dayi,Monthi,Yeari,DNR);
IF (C2 = 0)
   THEN BEGIN
        For Nri := 1 TO ni DO
          BEGIN
          EToDataSet[Nri].DayNr := DNR+Nri-1;
          EToDataSet[Nri].Param := 0;
          END;
        END
   ELSE BEGIN
        GetParameters(C1,C2,C3,UL,LL,Mid);
        For Nri := 1 TO ni DO
          BEGIN
          EToDataSet[Nri].DayNr := DNR+Nri-1;
          IF (Nri <= (ni/2+0.01))
             THEN EToDataSet[Nri].Param := (2*UL + (Mid-UL)*(2*Nri-1)/(ni/2))/2
             ELSE BEGIN
                  IF (((ni = 11) OR (ni = 9)) AND (Nri < (ni+1.01)/2))
                     THEN EToDataSet[Nri].Param := Mid
                     ELSE EToDataSet[Nri].Param := (2*Mid + (LL-Mid)*(2*Nri-(ni+1))/(ni/2))/2;
                  END;
          IF (EToDataSet[Nri].Param < 0) THEN EToDataSet[Nri].Param := 0;
          END;
        END;

FOR Nri := (ni+1) TO 31 DO
    BEGIN
    EToDataSet[Nri].DayNr := DNR+ni-1;
    EToDataSet[Nri].Param := 0;
    END;
END; (* GetDecadeEToDataSet *)





PROCEDURE GetDecadeRainDataSet(DayNri : LongInt;
                               VAR RainDataSet : rep_SimulationEventsDbl);
VAR Nri,Day1,Deci,Monthi,Yeari,ni,DecFile,Mfile,Yfile : INTEGER;
    DNR : LongInt;
    fRain : textfile;
    OKRain : BOOLEAN;
    C : double;

BEGIN
DetermineDate(DayNri,Day1,Monthi,Yeari);

//0. Set Monthly Parameters

// 1. Which decade ?
IF (Day1 > 20)
   THEN BEGIN
        Deci := 3;
        Day1 := 21;
        ni := DaysInMonth[Monthi] - Day1 + 1;
        IF ((Monthi = 2) AND LeapYear(Yeari)) THEN ni := ni + 1;
        END
   ELSE IF (Day1 > 10)
           THEN BEGIN
                Deci := 2;
                Day1 := 11;
                ni := 10;
                END
           ELSE BEGIN
                Deci := 1;
                Day1 := 1;
                ni := 10;
                END;

// 2. Load datafile
Assign(fRain,GetRainFilefull());
Reset(fRain);
READLN(fRain); // description
READLN(fRain); // time step
READLN(fRain); // day
READLN(fRain); // month
READLN(fRain); // year
READLN(fRain);
READLN(fRain);
READLN(fRain);
IF (GetRainRecord_FromD() > 20)
   THEN DecFile := 3
   ELSE IF (GetRainRecord_FromD() > 10)
           THEN DecFile := 2
           ELSE DecFile := 1;
Mfile := GetRainRecord_FromM();
IF (GetRainRecord_FromY() = 1901) THEN Yfile := Yeari
                             ELSE Yfile := GetRainRecord_FromY();

// 3. Find decade
OKRain := false;
C := 999;
REPEAT
 IF ((Deci = DecFile) AND (Monthi = Mfile) AND (Yeari = Yfile))
    THEN BEGIN
         READLN(fRain,C);
         OKRain := true;
         END
    ELSE BEGIN
         READLN(fRain);
         DecFile := DecFile + 1;
         IF (DecFile > 3) THEN AdjustDecadeMONTHandYEAR(DecFile,Mfile,Yfile);
         END;
UNTIL (OKRain);
Close(fRain);

// 4. Process data
DetermineDayNr(Day1,Monthi,Yeari,DNR);
For Nri := 1 TO ni DO
    BEGIN
    RainDataSet[Nri].DayNr := DNR+Nri-1;
    RainDataSet[Nri].Param := C/ni;
    END;
FOR Nri := (ni+1) TO 31 DO
    BEGIN
    RainDataSet[Nri].DayNr := DNR+ni-1;
    RainDataSet[Nri].Param := 0;
    END;

END; (* GetDecadeRainDataSet *)




PROCEDURE GetMonthlyRainDataSet(DayNri : LongInt;
                                VAR RainDataSet : rep_SimulationEventsDbl);
VAR Dayi,DayN,Monthi,Yeari : INTEGER;
    C1,C2,C3,RainDec1,RainDec2,RainDec3 : double;
    DNR : LongInt;


PROCEDURE GetSetofThreeMonths(Monthi,Yeari : INTEGER;
                        VAR C1,C2,C3 : double);
VAR fRain : textfile;
    Mfile,Yfile,Nri,Obsi : INTEGER;
    OK3 : BOOLEAN;

BEGIN
//1. Prepare record
Assign(fRain,GetRainFilefull());
Reset(fRain);
READLN(fRain); // description
READLN(fRain); // time step
READLN(fRain); // day
READLN(fRain); // month
READLN(fRain); // year
READLN(fRain);
READLN(fRain);
READLN(fRain);
Mfile := GetRainRecord_FromM();
IF (GetRainRecord_FromY() = 1901) THEN Yfile := Yeari
                             ELSE Yfile := GetRainRecord_FromY();
OK3 := false;

//2. IF 2 or less records
IF (GetRainRecord_NrObs() <= 2) THEN
   BEGIN
   READLN(fRain,C1);
   CASE GetRainRecord_NrObs() OF
     1 : BEGIN
         C2 := C1;
         C3 := C1;
         END;
     2 : BEGIN
         Mfile := Mfile + 1;
         IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
         READLN(fRain,C3);
         IF (Monthi = Mfile)
            THEN C2 := C3
            ELSE C2 := C1;
         END;
     END;
   OK3 := true;
   END;

//3. If first observation
IF ((NOT OK3) AND ((Monthi = Mfile) AND (Yeari = Yfile))) THEN
   BEGIN
   READLN(fRain,C1);
   C2 := C1;
   READLN(fRain,C3);
   OK3 := true;
   END;

//4. If last observation
IF ((NOT OK3) AND (Monthi = GetRainRecord_ToM())) THEN
   IF ((GetRainRecord_FromY() = 1901) OR (Yeari = GetRainRecord_ToY())) THEN
      BEGIN
//      FOR Nri := 1 TO (GetRainRecord_NrObs()-3) DO READLN(fRain);
//      READLN(fRain,C1);
//      READLN(fRain,C2);
//      READLN(fRain,C3);
      FOR Nri := 1 TO (GetRainRecord_NrObs()-2) DO READLN(fRain);
      READLN(fRain,C1);
      READLN(fRain,C2);
      C3 := C2;
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
   Mfile := GetRainRecord_FromM();
   FOR Nri := 1 TO (Obsi-2) DO
       BEGIN
       Readln(fRain);
       Mfile := Mfile + 1;
       IF (Mfile > 12) THEN AdjustMONTHandYEAR(Mfile,Yfile);
       END;
   READLN(fRain,C1);
   READLN(fRain,C2);
   READLN(fRain,C3);
   END;

Close(fRain);
END; (* GetSetofThreeMonths *)


BEGIN
DetermineDate(DayNri,Dayi,Monthi,Yeari);

//Set Monthly Parameters

GetSetofThreeMonths(Monthi,Yeari,C1,C2,C3);

Dayi := 1;
DetermineDayNr(Dayi,Monthi,Yeari,DNR);
DayN := DaysInMonth[Monthi];
IF ((Monthi = 2) AND LeapYear(Yeari)) THEN DayN := DayN + 1;
IF (C2 > 0)
   THEN BEGIN
        RainDec1 := (5*C1 + 26*C2 - 4*C3)/(27*3); // mm/dec
        RainDec2 := (-C1 + 29*C2 - C3)/(27*3);
        RainDec3 := (-4*C1 + 26*C2 + 5*C3)/(27*3);
        For Dayi := 1 TO 10 DO
          BEGIN
          RainDataSet[Dayi].DayNr := DNR+Dayi-1;
          RainDataSet[Dayi].Param := RainDec1/10;
          IF (RainDataSet[Dayi].Param < 0) THEN RainDataSet[Dayi].Param := 0;
          END;
        For Dayi := 11 TO 20 DO
          BEGIN
          RainDataSet[Dayi].DayNr := DNR+Dayi-1;
          RainDataSet[Dayi].Param := RainDec2/10;
          IF (RainDataSet[Dayi].Param < 0) THEN RainDataSet[Dayi].Param := 0;
          END;
        For Dayi := 21 TO DayN DO
          BEGIN
          RainDataSet[Dayi].DayNr := DNR+Dayi-1;
          RainDataSet[Dayi].Param := RainDec3/(DayN-21+1);
          IF (RainDataSet[Dayi].Param < 0) THEN RainDataSet[Dayi].Param := 0;
          END;
        END
   ELSE For Dayi := 1 TO DayN DO
            BEGIN
            RainDataSet[Dayi].DayNr := DNR+Dayi-1;
            RainDataSet[Dayi].Param := 0;
            END;

FOR Dayi := (DayN+1) TO 31 DO
    BEGIN
    RainDataSet[Dayi].DayNr := DNR+DayN-1;
    RainDataSet[Dayi].Param := 0;
    END;
END; (* GetMonthlyRainDataSet *)



end.
