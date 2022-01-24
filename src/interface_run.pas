unit interface_run;

interface

uses Global, interface_global;

TYPE rep_GwTable = RECORD
     DNr1, DNr2 : LongInt;
     Z1, Z2 : INTEGER;  // cm
     EC1, EC2 : double; // dS/m
     end;

TYPE rep_plotPar = RECORD
       PotVal, ActVal : double;
       end;

TYPE repIrriInfoRecord = Record
       NoMoreInfo : BOOLEAN;
       FromDay,
       ToDay,
       TimeInfo,
       DepthInfo  : INTEGER;
       end;

TYPE rep_StressTot = RECORD
       Salt,
       Temp,
       Exp,
       Sto,
       Weed : double;
       NrD  : INTEGER;
       end;

TYPE repCutInfoRecord = Record
       NoMoreInfo : BOOLEAN;
       FromDay,
       ToDay,
       IntervalInfo : INTEGER;
       IntervalGDD,
       MassInfo  : double;
       end;

TYPE rep_Transfer = Record
       Store      : BOOLEAN; // transfer of assimilates from above ground parts to root system is active
       Mobilize   : BOOLEAN; // transfer of assimialtes from root system to above ground parts is active
       ToMobilize : double;  // Total mass of assimilates (ton/ha) to mobilize at start of the season
       Bmobilized : double;  // Cumulative sum of assimilates (ton/ha) mobilized form root system
       end;

function GetGwTable(): rep_GwTable;
        external 'aquacrop' name '__ac_run_MOD_getgwtable';

procedure SetGwTable_DNr1(constref DNr1 : integer);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_dnr1';

procedure SetGwTable_DNr2(constref DNr2 : integer);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_dnr2';

procedure SetGwTable_Z1(constref Z1 : integer);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_z1';

procedure SetGwTable_Z2(constref Z2 : integer);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_z2';

procedure SetGwTable_EC1(constref EC1 : double);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_ec1';

procedure SetGwTable_EC2(constref EC2 : double);
        external 'aquacrop' name '__ac_run_MOD_setgwtable_ec2';

function GetPlotVarCrop(): rep_plotPar;
        external 'aquacrop' name '__ac_run_MOD_getPlotVarCrop';

procedure SetPlotVarCrop_PotVal(constref PotVal : double);
        external 'aquacrop' name '__ac_run_MOD_setplotvarcrop_potval';

procedure SetPlotVarCrop_ActVal(constref ActVal : double);
        external 'aquacrop' name '__ac_run_MOD_setplotvarcrop_actval';

function GetIrriInfoRecord1_NoMoreInfo(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getirriinforecord1_nomoreinfo_wrap';

function GetIrriInfoRecord1_FromDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord1_fromday';

function GetIrriInfoRecord1_ToDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord1_today';

function GetIrriInfoRecord1_TimeInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord1_timeinfo';

function GetIrriInfoRecord1_DepthInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord1_depthinfo';

procedure SetIrriInfoRecord1_NoMoreInfo(constref NoMoreInfo : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setirriinforecord1_nomoreinfo_wrap';

procedure SetIrriInfoRecord1_FromDay(constref FromDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord1_fromday';

procedure SetIrriInfoRecord1_ToDay(constref ToDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord1_today';

procedure SetIrriInfoRecord1_TimeInfo(constref TimeInfo : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord1_timeinfo';

procedure SetIrriInfoRecord1_DepthInfo(constref DepthInfo : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord1_depthinfo';

function GetIrriInfoRecord2_NoMoreInfo(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getirriinforecord2_nomoreinfo_wrap';

function GetIrriInfoRecord2_FromDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord2_fromday';

function GetIrriInfoRecord2_ToDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord2_today';

function GetIrriInfoRecord2_TimeInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord2_timeinfo';

function GetIrriInfoRecord2_DepthInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getirriinforecord2_depthinfo';

procedure SetIrriInfoRecord2_NoMoreInfo(constref NoMoreInfo : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setirriinforecord2_nomoreinfo_wrap';

procedure SetIrriInfoRecord2_FromDay(constref FromDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord2_fromday';

procedure SetIrriInfoRecord2_ToDay(constref ToDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord2_today';

procedure SetIrriInfoRecord2_TimeInfo(constref TimeInfo : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord2_timeinfo';

procedure SetIrriInfoRecord2_DepthInfo(constref DepthInfo : integer);
        external 'aquacrop' name '__ac_run_MOD_setirriinforecord2_depthinfo';

function GetStressTot(): rep_StressTot;
        external 'aquacrop' name '__ac_run_MOD_getstresstot';

procedure SetStressTot_Salt(constref Salt : double);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_salt';

procedure SetStressTot_Temp(constref Temp : double);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_temp';

procedure SetStressTot_Exp(constref Exp : double);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_exp';

procedure SetStressTot_Sto(constref Sto : double);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_sto';

procedure SetStressTot_Weed(constref Weed : double);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_weed';

procedure SetStressTot_NrD(constref NrD : integer);
        external 'aquacrop' name '__ac_run_MOD_setstresstot_nrd';

function GetCutInfoRecord1_NoMoreInfo(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getcutinforecord1_nomoreinfo_wrap';

function GetCutInfoRecord1_FromDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord1_fromday';

function GetCutInfoRecord1_ToDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord1_today';

function GetCutInfoRecord1_IntervalInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord1_intervalinfo';

function GetCutInfoRecord1_IntervalGDD(): double;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord1_intervalgdd';

function GetCutInfoRecord1_MassInfo(): double;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord1_massinfo';


implementation


initialization


end.

