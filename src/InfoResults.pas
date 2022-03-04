unit InfoResults;

interface

uses Global, interface_global, interface_inforesults;

PROCEDURE WriteAssessmentSimulation(StrNr,totalnameEvalStat : string;
                                    TheProjectType : repTypeProject;
                                    RangeMin,RangeMax : LongInt);



implementation

Uses SysUtils;





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
