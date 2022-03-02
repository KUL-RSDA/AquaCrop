unit interface_simul;

interface

uses Global, interface_global, TempProcessing, interface_tempprocessing;

function GetCDCadjustedNoStressNew(
            constref CCx, CDC, CCxAdjusted): double;
     external 'aquacrop' name '__ac_simul_MOD_getcdcadjustednostressnew';

procedure AdjustpLeafToETo(
            constref EToMean : double;
            VAR pLeafULAct : double ;
            VAR pLeafLLAct : double);
     external 'aquacrop' name '__ac_simul_MOD_adjustpleaftoeto';

procedure DeterminePotentialBiomass(
            constref VirtualTimeCC : integer;
            constref cumGDDadjCC : double;
            constref CO2i : double;
            constref GDDayi : double;
            VAR CCxWitheredTpotNoS : double;
            VAR BiomassUnlim : double);
     external 'aquacrop' name '__ac_simul_MOD_determinepotentialbiomass';

implementation


initialization


end.

