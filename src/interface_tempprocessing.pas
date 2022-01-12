unit interface_tempprocessing;

interface

uses Global, interface_global;


PROCEDURE AdjustMONTHandYEAR(
            VAR Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustmonthandyear';


implementation


initialization


end.

