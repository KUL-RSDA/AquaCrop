unit interface_inforesults;

interface

uses Global, interface_global;

type 
    rep_EventObsSim = Record
        Obsi : double;
        StdObsi : double;
        Simi : double;
        DDi : ShortInt;
        MMi : shortInt;
        YYYYi : INTEGER;
        end;

    rep_ArrayObsSim = ARRAY[1..100] OF rep_EventObsSim;

procedure StatisticAnalysis(
                    constref TypeObsSim : rep_TypeObsSim;
                    constref RangeObsMin, RangeObsMax : integer;
                    constref StrNr : string;
                    var Nobs : integer;
                    var ObsAver, SimAver, PearsonCoeff, RMSE : double;
                    var NRMSE, NScoeff, IndexAg : double;
                    var ArrayObsSim : rep_ArrayObsSim);

procedure StatisticAnalysis_wrap(
                    constref TypeObsSim : integer;
                    constref RangeObsMin, RangeObsMax : integer;
                    constref StrNr : PChar;
                    constref strlen : integer;
                    var Nobs : integer;
                    var ObsAver, SimAver, PearsonCoeff, RMSE : double;
                    var NRMSE, NScoeff, IndexAg : double;
                    var ArrayObsSim : rep_ArrayObsSim);
    external 'aquacrop' name '__ac_interface_inforesults_MOD_statisticanalysis_wrap';

procedure WriteAssessmentSimulation(constref StrNr, totalnameEvalStat : string;
                                    constref TheProjectType : repTypeProject;
                                    constref RangeMin, RangeMax : integer);

procedure WriteAssessmentSimulation_wrap(
                                    //constref p1, p2 : PChar;
                                    //constref strlen1, strlen2 : integer;
                                    constref p2 : PChar;
                                    constref strlen2 : integer;
                                    constref TheProjectType : ShortInt;
                                    constref RangeMin, RangeMax : integer);
    external 'aquacrop' name '__ac_interface_inforesults_MOD_writeassessmentsimulation_wrap';


implementation


procedure StatisticAnalysis(
                    constref TypeObsSim : rep_TypeObsSim;
                    constref RangeObsMin, RangeObsMax : integer;
                    constref StrNr : string;
                    var Nobs : integer;
                    var ObsAver, SimAver, PearsonCoeff, RMSE : double;
                    var NRMSE, NScoeff, IndexAg : double;
                    var ArrayObsSim : rep_ArrayObsSim);
var
    int_typeObsSim : integer;
    p : PChar;
    strlen : integer;
begin
    int_typeObsSim := ord(TypeObsSim);
    p := PChar(StrNr);
    strlen := Length(StrNr);
    StatisticAnalysis_wrap(int_TypeObsSim, RangeObsMin, RangeObsMax, p, strlen, 
                           Nobs, ObsAver, SimAver, PearsonCoeff, RMSE, NRMSE,
                           NScoeff, IndexAg, ArrayObsSim);
end;


procedure WriteAssessmentSimulation(constref StrNr, totalnameEvalStat : string;
                                    constref TheProjectType : repTypeProject;
                                    constref RangeMin, RangeMax : integer);
var
    int_projecttype : ShortInt;
    //p1 : PChar;
    p2 : PChar;
    //strlen1 : integer;
    strlen2 : integer;
begin
    int_projecttype := ord(TheProjectType);
    //p1 := PChar(StrNr);
    p2 := PChar(totalnameEvalStat);
    //strlen1 := Length(StrNr);
    strlen2 := Length(totalnameEvalStat);
    writeln('PPP :', StrNr, ':', totalnameEvalStat, ': ', strlen2);
    //WriteAssessmentSimulation_wrap(p1, p2, strlen1, strlen2,
    //                             int_projecttype, RangeMin, RangeMax)
    WriteAssessmentSimulation_wrap(p2, strlen2, int_projecttype, RangeMin, RangeMax)
end;


initialization


end.

