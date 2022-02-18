unit interface_rootunit;

interface

uses Global, interface_global;

function __AdjustedRootingDepth(
        constref CCAct :  double;
        constref CCpot :  double;
        constref Tpot :  double;
        constref Tact :  double;
        constref StressLeaf :  double;
        constref StressSenescence :  double;
        constref DAP :  integer;
        constref L0 :  integer;
        constref LZmax :  integer;
        constref L1234 :  integer;
        constref GDDL0 :  integer;
        constref GDDLZmax :  integer;
        constref GDDL1234 :  integer;
        constref SumGDDPrev : double;
        constref SumGDD : double;
        constref Zmin : double;
        constref Zmax : double;
        constref Ziprev : double;
        constref ShapeFactor : shortint;
        constref TypeDays : integer) : double;
    external 'aquacrop' name '__ac_rootunit_MOD_adjustedrootingdepth';

function AdjustedRootingDepth(
        constref CCAct :  double;
        constref CCpot :  double;
        constref Tpot :  double;
        constref Tact :  double;
        constref StressLeaf :  double;
        constref StressSenescence :  double;
        constref DAP :  integer;
        constref L0 :  integer;
        constref LZmax :  integer;
        constref L1234 :  integer;
        constref GDDL0 :  integer;
        constref GDDLZmax :  integer;
        constref GDDL1234 :  integer;
        constref SumGDDPrev : double;
        constref SumGDD : double;
        constref Zmin : double;
        constref Zmax : double;
        constref Ziprev : double;
        constref ShapeFactor : shortint;
        constref TypeDays : rep_modeCycle) : double;


implementation

function AdjustedRootingDepth(
        constref CCAct :  double;
        constref CCpot :  double;
        constref Tpot :  double;
        constref Tact :  double;
        constref StressLeaf :  double;
        constref StressSenescence :  double;
        constref DAP :  integer;
        constref L0 :  integer;
        constref LZmax :  integer;
        constref L1234 :  integer;
        constref GDDL0 :  integer;
        constref GDDLZmax :  integer;
        constref GDDL1234 :  integer;
        constref SumGDDPrev : double;
        constref SumGDD : double;
        constref Zmin : double;
        constref Zmax : double;
        constref Ziprev : double;
        constref ShapeFactor : shortint;
        constref TypeDays : rep_modeCycle) : double;
var
   int_TypeDays:  integer;

begin
    int_TypeDays := ord(TypeDays);
    AdjustedRootingDepth := __AdjustedRootingDepth(CCAct, CCpot, Tpot, Tact,
        StressLeaf, StressSenescence, DAP, L0, LZmax, L1234, GDDL0, GDDLZmax,
        GDDL1234, SumGDDPrev, SumGDD, Zmin, Zmax, Ziprev, ShapeFactor, int_TypeDays);
end;

initialization


end.
