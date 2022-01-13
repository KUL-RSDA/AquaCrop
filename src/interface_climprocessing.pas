unit interface_climprocessing;

interface

uses Global, interface_global;


procedure AdjustDecadeMONTHandYEAR(
            var DecFile, Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustdecademonthandyear';

procedure AdjustMONTHandYEAR(
            var Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustmonthandyear';

procedure GetParameters(
            constref C1, C2, C3 : double;
            var UL, LL, Mid : double);
         external 'aquacrop' name '__ac_climprocessing_MOD_getparameters';

implementation



initialization


end.

