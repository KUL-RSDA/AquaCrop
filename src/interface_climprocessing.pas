unit interface_climprocessing;

interface

uses Global, interface_global;


procedure AdjustDecadeMONTHandYEAR(
            var DecFile, Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustdecademonthandyear';

procedure AdjustMONTHandYEAR(
            var Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustmonthandyear';


implementation



initialization


end.

