unit interface_tempprocessing;

interface


PROCEDURE AdjustMONTHandYEAR(
            VAR Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustmonthandyear';


implementation


initialization


end.

