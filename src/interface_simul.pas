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

function calculate_delta_theta(
             constref theta, thetaAdjFC : double;
             constref NrLayer : shortint): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_delta_theta';

function calculate_theta(
             constref delta_theta, thetaAdjFC : double;
             constref NrLayer : shortint): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_theta';

implementation


initialization


end.

