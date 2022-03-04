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
    external 'aquacrop' name '__ac_inforesults_MOD_statisticanalysis';


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


initialization


end.

