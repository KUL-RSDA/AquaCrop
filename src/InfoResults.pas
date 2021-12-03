unit InfoResults;

interface

uses Global;

PROCEDURE WriteAssessmentSimulation(StrNr,totalnameEvalStat : string;
                                    TheProjectType : repTypeProject;
                                    RangeMin,RangeMax : LongInt);
                                      


implementation

Uses SysUtils;

TYPE rep_EventObsSim = Record
        Obsi : double;
        StdObsi : double;
        Simi : double;
        DDi : ShortInt;
        MMi : shortInt;
        YYYYi : INTEGER;
        end;

    rep_ArrayObsSim = ARRAY[1..100] OF rep_EventObsSim;



PROCEDURE StatisticAnalysis(TypeObsSim : rep_TypeObsSim;
                            RangeObsMin,RangeObsMax : LongInt;
                            StrNr : string;
                            VAR Nobs : INTEGER;
                            VAR ObsAver,SimAver,PearsonCoeff,RMSE,NRMSE,NScoeff,IndexAg : double;
                            VAR ArrayObsSim : rep_ArrayObsSim);
VAR Nri : INTEGER;
    dDeNom,R2tel,SumSqrDobs,SumSqrDsim,SumSqrDiv,DeNom  : double;



    PROCEDURE GetObsSim(RangeObsMin,RangeObsMax : LongInt;
                        StrNr : string;
                        VAR Nobs : INTEGER;
                        Var ArrayObsSim : rep_ArrayObsSim;
                        VAR ObsAver,SimAver : double);
    VAR OutputName : string;
        fOut : TextFile;
        Dayi,Monthi : ShortInt;
        SkipLines,NCi,NCobs,Yeari : INTEGER;
        i : LongInt;
        VarObsi,VarSimi,VarStdi : double;
    BEGIN
    Nobs := 0;
    ObsAver := 0;
    SimAver := 0;

    // open file
    OutputName := CONCAT(PathNameSimul,'EvalData',Trim(StrNr),'.OUT');
    Assign(fOut,OutputName);
    Reset(fOut);
    READLN(fOut); //AquaCrop Version - Date and Time
    READLN(fOut); //title
    READLN(fOut); //
    READLN(fOut); //list of variables
    READLN(fOut); //units
    // find first day
    //SkipLines := RangeObsMin - Simulation.FromDayNr - 1;
    SkipLines := RangeObsMin - Simulation.FromDayNr;
    FOR i := 1 to SkipLines DO READLN(fOut);

    // get Sim and Obs in range
    CASE TypeObsSim OF
       ObsSimCC  : NCobs := 2;
       ObsSimB   : NCobs := 5;
       ObsSimSWC : NCobs := 8;
       else NCobs := 5;
       end;
    FOR i := RangeObsMin TO RangeObsMax DO
        BEGIN
        IF NOT (Eof(fout)) THEN
           BEGIN
           READ(fOut,Dayi,Monthi,Yeari);
           FOR NCi := 1 TO NCobs DO READ(fOut,VarSimi);
           READLN(fOut,VarSimi,VarObsi,VarStdi);
           IF ((ROUND(VarObsi) <> undef_int) AND (Nobs < 100)) THEN
              BEGIN
              Nobs := Nobs + 1;
              ArrayObsSim[Nobs].DDi := Dayi;
              ArrayObsSim[Nobs].MMi := Monthi;
              ArrayObsSim[Nobs].YYYYi := Yeari;
              ArrayObsSim[Nobs].Simi := VarSimi;
              ArrayObsSim[Nobs].Obsi := VarObsi;
              ArrayObsSim[Nobs].StdObsi := VarStdi;
              SimAver := SimAver + VarSimi;
              ObsAver := ObsAver + VarObsi;
              END;
           END;
        END;
    // close file
    Close(fOut);

    // calculate averages
    IF (Nobs > 0) THEN
       BEGIN
       ObsAver := ObsAver/Nobs;
       SimAver := SimAver/Nobs;
       END;
    END; (* GetObsSim *)


BEGIN (* StatisticAnalysis *)
// get data
GetObsSim(RangeObsMin,RangeObsMax,StrNr,Nobs,ArrayObsSim,ObsAver,SimAver);
// statistical evaluation
IF (Nobs > 1)
   THEN BEGIN
        R2tel := 0;
        SumSqrDobs := 0;
        SumSqrDsim := 0;
        SumSqrDiv := 0;
        dDeNom := 0;
        FOR Nri := 1 TO Nobs DO
            BEGIN
            R2tel := R2tel + (ArrayObsSim[Nri].Obsi-ObsAver)*(ArrayObsSim[Nri].Simi-SimAver);
            SumSqrDobs := SumSqrDobs + SQR(ArrayObsSim[Nri].Obsi-ObsAver);
            SumSqrDsim := SumSqrDsim + SQR(ArrayObsSim[Nri].Simi-SimAver);
            SumSqrDiv := SumSqrDiv + SQR(ArrayObsSim[Nri].Obsi-ArrayObsSim[Nri].Simi);
            dDeNom := dDeNom + SQR(abs(ArrayObsSim[Nri].Simi-ObsAver) + abs(ArrayObsSim[Nri].Obsi-ObsAver));
            END;
        //R2
        DeNom := SQRT(SumSqrDobs*SumSqrDsim);
        IF (DeNom > 0)
           THEN PearsonCoeff := R2tel/DeNom
           ELSE PearsonCoeff := undef_int;
        //RMSE
        RMSE := SQRT(SumSqrDiv/NObs);
        //NRMSE
        IF (ObsAver > 0)
           THEN NRMSE := 100*(RMSE/ObsAver)
           ELSE NRMSE := undef_int;
        //Nash-Sutcliffe coefficient (EF)
        IF (SumSqrDobs > 0)
           THEN NScoeff := 1 - (SumSqrDiv/SumSqrDobs)
           ELSE NScoeff := undef_int;
        //Index of agreement (d)
        IF (dDeNom > 0)
           THEN IndexAg := 1 - (SumSqrDiv/dDeNom)
           ELSE IndexAg := undef_int;
        END
   ELSE BEGIN
        ObsAver := undef_int;
        SimAver := undef_int;
        PearsonCoeff := undef_int;
        RMSE := undef_int;
        NRMSE := undef_int;
        NScoeff := undef_int;
        IndexAg := undef_int;
        END;
END; (* StatisticAnalysis *)





PROCEDURE WriteAssessmentSimulation(StrNr,totalnameEvalStat : string;
                                    TheProjectType : repTypeProject;
                                    RangeMin,RangeMax : LongInt);
VAR fAssm : text;
    TypeObsSim : rep_TypeObsSim;
    Nobs,Nri : INTEGER;
    ObsAver,SimAver,PearsonCoeff,RMSE,NRMSE,NScoeff,IndexAg : double;
    ArrayObsSim : rep_ArrayObsSim;
    YearString : string;
    
BEGIN
// 1. Open file for assessment
Assign(fAssm,totalnameEvalStat);
Rewrite(fAssm);
WRITELN(fAssm,'AquaCrop 7.0 (June 2021) - Output created on (date) : ',DateToStr(Date),'   at (time) : ',TimeToStr(Time));
WRITELN(fAssm,'Evaluation of simulation results - Statistics');
IF (TheProjectType = TypePRM) THEN WRITELN(fAssm,'** Run number:',StrNr);
WRITELN(fAssm);

// 2. Run analysis

// 2.1 Canopy Cover
TypeObsSim := ObsSimCC;
StatisticAnalysis(TypeObsSim,RangeMin,RangeMax,StrNr,Nobs,ObsAver,SimAver,
                  PearsonCoeff,RMSE,NRMSE,NScoeff,IndexAg,ArrayObsSim);
                  WRITELN(fAssm);
WRITELN(fAssm,'  ASSESSMENT OF CANOPY COVER --------------------------------------');
IF (Nobs > 1)
   THEN BEGIN
        WRITELN(fAssm,'              --------- Canopy Cover (%) ---------');
        WRITELN(fAssm,'    Nr        Observed    +/- St Dev     Simulated    Date');
        WRITELN(fAssm,'  ----------------------------------------------------------------');
        FOR Nri := 1 to Nobs DO
            BEGIN
            IF (ArrayObsSim[Nri].YYYYi <= 1901)
               THEN YearString :=''
               ELSE Str((ArrayObsSim[Nri].YYYYi):4,YearString);
            WRITELN(fAssm,Nri:6,(ArrayObsSim[Nri].Obsi):14:1,(ArrayObsSim[Nri].StdObsi):14:1,
                         (ArrayObsSim[Nri].Simi):14:1,'      ',(ArrayObsSim[Nri].DDi):2,' ',
                         NameMonth[ArrayObsSim[Nri].MMi],' ',YearString);
            END;
        WRITELN(fAssm);
        WRITELN(fAssm,'  Valid observations/simulations sets (n) ....... : ',Nobs:5);
        WRITELN(fAssm,'  Average of observed Canopy Cover .............. : ',ObsAver:7:1,'   %');
        WRITELN(fAssm,'  Average of simulated Canopy Cover ............. : ',SimAver:7:1,'   %');
        WRITELN(fAssm);
        WRITELN(fAssm,'  Pearson Correlation Coefficient (r) ........... : ',PearsonCoeff:8:2);
        WRITELN(fAssm,'  Root mean square error (RMSE) ................. : ',RMSE:7:1,'   % CC');
        WRITELN(fAssm,'  Normalized root mean square error  CV(RMSE).... : ',NRMSE:7:1,'   %');
        WRITELN(fAssm,'  Nash-Sutcliffe model efficiency coefficient (EF): ',NScoeff:8:2);
        WRITELN(fAssm,'  Willmotts index of agreement (d) .............. : ',IndexAg:8:2);
        END
   ELSE WRITELN(fAssm,'  No statistic analysis (insufficient data)');
WRITELN(fAssm,'  ----------------------------------------------------------------');

//2.2 Biomass production
TypeObsSim := ObsSimB;
StatisticAnalysis(TypeObsSim,RangeMin,RangeMax,StrNr,Nobs,ObsAver,SimAver,
                  PearsonCoeff,RMSE,NRMSE,NScoeff,IndexAg,ArrayObsSim);
WRITELN(fAssm);
WRITELN(fAssm);
WRITELN(fAssm,'  ASSESSMENT OF BIOMASS PRODUCTION --------------------------------');
IF (Nobs > 1)
   THEN BEGIN
        WRITELN(fAssm,'              --------- Biomass (ton/ha) ---------');
        WRITELN(fAssm,'    Nr        Observed    +/- St Dev     Simulated    Date');
        WRITELN(fAssm,'  ----------------------------------------------------------------');
        FOR Nri := 1 to Nobs DO
            BEGIN
            IF (ArrayObsSim[Nri].YYYYi <= 1901)
               THEN YearString :=''
               ELSE Str((ArrayObsSim[Nri].YYYYi):4,YearString);
            WRITELN(fAssm,Nri:6,(ArrayObsSim[Nri].Obsi):16:3,(ArrayObsSim[Nri].StdObsi):14:3,
                          (ArrayObsSim[Nri].Simi):14:3,'      ',(ArrayObsSim[Nri].DDi):2,' ',
                          NameMonth[ArrayObsSim[Nri].MMi],' ',YearString);
            END;
        WRITELN(fAssm);
        WRITELN(fAssm,'  Valid observations/simulations sets (n) ....... : ',Nobs:5);
        WRITELN(fAssm,'  Average of observed Biomass production ........ : ',ObsAver:9:3,'   ton/ha');
        WRITELN(fAssm,'  Average of simulated Biomass production ....... : ',SimAver:9:3,'   ton/ha');
        WRITELN(fAssm);
        WRITELN(fAssm,'  Pearson Correlation Coefficient (r) ........... : ',PearsonCoeff:8:2);
        WRITELN(fAssm,'  Root mean square error (RMSE) ................. : ',RMSE:9:3,'   ton/ha');
        WRITELN(fAssm,'  Normalized root mean square error  CV(RMSE).... : ',NRMSE:7:1,'   %');
        WRITELN(fAssm,'  Nash-Sutcliffe model efficiency coefficient (EF): ',NScoeff:8:2);
        WRITELN(fAssm,'  Willmotts index of agreement (d) .............. : ',IndexAg:8:2);
        END
   ELSE WRITELN(fAssm,'  No statistic analysis (insufficient data)');
WRITELN(fAssm,'  ----------------------------------------------------------------');

//2.3 Soil Water Content
TypeObsSim := ObsSimSWC;
StatisticAnalysis(TypeObsSim,RangeMin,RangeMax,StrNr,Nobs,ObsAver,SimAver,
                  PearsonCoeff,RMSE,NRMSE,NScoeff,IndexAg,ArrayObsSim);
WRITELN(fAssm);
WRITELN(fAssm);
WRITELN(fAssm,'  ASSESSMENT OF SOIL WATER CONTENT --------------------------------');
IF (Nobs > 1)
   THEN BEGIN
        WRITELN(fAssm,'              ------ Soil water content (mm) -----');
        WRITELN(fAssm,'    Nr        Observed    +/- St Dev     Simulated    Date');
        WRITELN(fAssm,'  ----------------------------------------------------------------');
        FOR Nri := 1 to Nobs DO
            BEGIN
            IF (ArrayObsSim[Nri].YYYYi <= 1901)
               THEN YearString :=''
               ELSE Str((ArrayObsSim[Nri].YYYYi):4,YearString);
            WRITELN(fAssm,Nri:6,(ArrayObsSim[Nri].Obsi):14:1,(ArrayObsSim[Nri].StdObsi):14:1,
                      (ArrayObsSim[Nri].Simi):14:1,'      ',(ArrayObsSim[Nri].DDi):2,' ',
                      NameMonth[ArrayObsSim[Nri].MMi],' ',YearString);
            END;
        WRITELN(fAssm);
        WRITELN(fAssm,'  Valid observations/simulations sets (n) ....... : ',Nobs:5);
        WRITELN(fAssm,'  Average of observed Soil water content ........ : ',ObsAver:7:1,'   mm');
        WRITELN(fAssm,'  Average of simulated Soil water content ....... : ',SimAver:7:1,'   mm');
        WRITELN(fAssm);
        WRITELN(fAssm,'  Pearson Correlation Coefficient (r) ........... : ',PearsonCoeff:8:2);
        WRITELN(fAssm,'  Root mean square error (RMSE) ................. : ',RMSE:7:1,'   mm');
        WRITELN(fAssm,'  Normalized root mean square error  CV(RMSE).... : ',NRMSE:7:1,'   %');
        WRITELN(fAssm,'  Nash-Sutcliffe model efficiency coefficient (EF): ',NScoeff:8:2);
        WRITELN(fAssm,'  Willmotts index of agreement (d) .............. : ',IndexAg:8:2);
        END
   ELSE WRITELN(fAssm,'  No statistic analysis (insufficient data)');
WRITELN(fAssm,'  ----------------------------------------------------------------');
WRITELN(fAssm);

// 3. Close file for assessment
Close(fAssm);
END; (* WriteAssessmentSimulation *)



end.
