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

//-----------------------------------------------------------------------------
// BUDGET_module
//-----------------------------------------------------------------------------


function calculate_delta_theta(
             constref theta, thetaAdjFC : double;
             constref NrLayer : integer): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_delta_theta';

function calculate_theta(
             constref delta_theta, thetaAdjFC : double;
             constref NrLayer : integer): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_theta';

procedure calculate_drainage();
     external 'aquacrop' name '__ac_simul_MOD_calculate_drainage';

procedure calculate_runoff(constref MaxDepth : double );
     external 'aquacrop' name '__ac_simul_MOD_calculate_runoff';

procedure Caluclate_irrigation(var SubDrain : double;
                               var TargetTimeVal : integer);
    external 'aquacrop' name '__ac_simul_MOD_calculate_irrigation'; 

//-----------------------------------------------------------------------------
// end BUDGET_module
//-----------------------------------------------------------------------------


implementation


initialization


end.

