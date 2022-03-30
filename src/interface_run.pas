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

function GetGwTable_DNr1() : integer;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_dnr1';

function GetGwTable_DNr2() : integer;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_dnr2';

function GetGwTable_Z1() : integer;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_z1';

function GetGwTable_Z2(): integer;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_z2';

function GetGwTable_EC1() : double;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_ec1';

function GetGwTable_EC2() : double;
        external 'aquacrop' name '__ac_run_MOD_getgwtable_ec2';

procedure SetGwTable(constref GwTable : rep_GwTable);

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
        external 'aquacrop' name '__ac_run_MOD_getplotvarcrop';

procedure SetPlotVarCrop_PotVal(constref PotVal : double);
        external 'aquacrop' name '__ac_run_MOD_setplotvarcrop_potval';

procedure SetPlotVarCrop_ActVal(constref ActVal : double);
        external 'aquacrop' name '__ac_run_MOD_setplotvarcrop_actval';

function GetIrriInfoRecord1() : repIrriInfoRecord;

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

procedure SetIrriInfoRecord1(constref IrriInfoRecord1 : repIrriInfoRecord);

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

function GetIrriInfoRecord2() : repIrriInfoRecord;

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

procedure SetIrriInfoRecord2(constref IrriInfoRecord2 : repIrriInfoRecord);

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

function GetStressTot_Salt() : double;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_salt';

function GetStressTot_Temp() : double;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_temp';

function GetStressTot_Exp() : double;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_exp';

function GetStressTot_Sto(): double;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_sto';

function GetStressTot_Weed(): double;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_weed';

function GetStressTot_NrD() : integer;
        external 'aquacrop' name '__ac_run_MOD_getstresstot_nrd';

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

function GetCutInfoRecord1() : repCutInfoRecord;

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

procedure SetCutInfoRecord1(constref CutInfoRecord1 : repCutInfoRecord);

procedure SetCutInfoRecord1_NoMoreInfo(constref NoMoreInfo : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setcutinforecord1_nomoreinfo_wrap';

procedure SetCutInfoRecord1_FromDay(constref FromDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord1_fromday';

procedure SetCutInfoRecord1_ToDay(constref ToDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord1_today';

procedure SetCutInfoRecord1_IntervalInfo(constref IntervalInfo : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord1_intervalinfo';

procedure SetCutInfoRecord1_IntervalGDD(constref IntervalGDD : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord1_intervalgdd';

procedure SetCutInfoRecord1_MassInfo(constref MassInfo : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord1_massinfo';

function GetCutInfoRecord2() : repCutInfoRecord;

function GetCutInfoRecord2_NoMoreInfo(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getcutinforecord2_nomoreinfo_wrap';

function GetCutInfoRecord2_FromDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord2_fromday';

function GetCutInfoRecord2_ToDay(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord2_today';

function GetCutInfoRecord2_IntervalInfo(): integer;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord2_intervalinfo';

function GetCutInfoRecord2_IntervalGDD(): double;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord2_intervalgdd';

function GetCutInfoRecord2_MassInfo(): double;
        external 'aquacrop' name '__ac_run_MOD_getcutinforecord2_massinfo';

procedure SetCutInfoRecord2(constref CutInfoRecord2 : repCutInfoRecord);

procedure SetCutInfoRecord2_NoMoreInfo(constref NoMoreInfo : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setcutinforecord2_nomoreinfo_wrap';

procedure SetCutInfoRecord2_FromDay(constref FromDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord2_fromday';

procedure SetCutInfoRecord2_ToDay(constref ToDay : integer);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord2_today';

procedure SetCutInfoRecord2_IntervalInfo(constref IntervalInfo : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord2_intervalinfo';

procedure SetCutInfoRecord2_IntervalGDD(constref IntervalGDD : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord2_intervalgdd';

procedure SetCutInfoRecord2_MassInfo(constref MassInfo : double);
        external 'aquacrop' name '__ac_run_MOD_setcutinforecord2_massinfo';

function GetTransfer_Store(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_gettransfer_store_wrap';

function GetTransfer_Mobilize(): boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_gettransfer_mobilize_wrap';

function GetTransfer_ToMobilize(): double;
        external 'aquacrop' name '__ac_run_MOD_gettransfer_tomobilize';

function GetTransfer_Bmobilized(): double;
        external 'aquacrop' name '__ac_run_MOD_gettransfer_bmobilized';

procedure SetTransfer_Store(constref Store : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_settransfer_store_wrap';

procedure SetTransfer_Mobilize(constref Mobilize : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_settransfer_mobilize_wrap';

procedure SetTransfer_ToMobilize(constref ToMobilize : double);
        external 'aquacrop' name '__ac_run_MOD_settransfer_tomobilize';

procedure SetTransfer_Bmobilized(constref Bmobilized : double);
        external 'aquacrop' name '__ac_run_MOD_settransfer_bmobilized';

procedure AdjustForWatertable;
        external 'aquacrop' name '__ac_run_MOD_adjustforwatertable';

procedure ResetPreviousSum(VAR PreviousSum : rep_sum;
                           VAR SumETo,SumGDD,PreviousSumETo,PreviousSumGDD,
                               PreviousBmob,PreviousBsto : double);
    external 'aquacrop' name '__ac_run_MOD_resetprevioussum';

procedure GetGwtSet(constref DayNrIN : LongInt;
                    VAR GwT : rep_GwTable);
        external 'aquacrop' name '__ac_run_MOD_getgwtset';

procedure RelationshipsForFertilityAndSaltStress(
                    VAR Coeffb0 : double;
                    VAR Coeffb1 : double;
                    VAR Coeffb2 : double;
                    VAR FracBiomassPotSF : double;
                    VAR Coeffb0Salt : double;
                    VAR Coeffb1Salt : double;
                    VAR Coeffb2Salt : double);
        external 'aquacrop' name '__ac_run_MOD_relationshipsforfertilityandsaltstress';

implementation

function GetGwTable() : rep_GwTable;
begin;
    GetGwTable.DNr1 := GetGwTable_DNr1();
    GetGwTable.DNr2 := GetGwTable_DNr2();
    GetGwTable.Z1 := GetGwTable_Z1();
    GetGwTable.Z2 := GetGwTable_Z2();
    GetGwTable.EC1 := GetGwTable_EC1();
    GetGwTable.EC2 := GetGwTable_EC2();
end; 

procedure SetGwTable(constref GwTable : rep_GwTable);
begin;
    SetGwTable_DNr1(GwTable.DNr1);
    SetGwTable_DNr2(GwTable.DNr2);
    SetGwTable_Z1(GwTable.Z1);
    SetGwTable_Z2(GwTable.Z2);
    SetGwTable_EC1(GwTable.EC1);
    SetGwTable_EC2(GwTable.EC2);
end;

function GetIrriInfoRecord1() : repIrriInfoRecord;
begin;
    GetIrriInfoRecord1.NoMoreInfo := GetIrriInfoRecord1_NoMoreInfo();
    GetIrriInfoRecord1.FromDay := GetIrriInfoRecord1_FromDay();
    GetIrriInfoRecord1.ToDay := GetIrriInfoRecord1_ToDay();
    GetIrriInfoRecord1.TimeInfo := GetIrriInfoRecord1_TimeInfo();
    GetIrriInfoRecord1.DepthInfo := GetIrriInfoRecord1_DepthInfo();
end; 

function GetIrriInfoRecord2() : repIrriInfoRecord;
begin;
    GetIrriInfoRecord2.NoMoreInfo := GetIrriInfoRecord2_NoMoreInfo();
    GetIrriInfoRecord2.FromDay := GetIrriInfoRecord2_FromDay();
    GetIrriInfoRecord2.ToDay := GetIrriInfoRecord2_ToDay();
    GetIrriInfoRecord2.TimeInfo := GetIrriInfoRecord2_TimeInfo();
    GetIrriInfoRecord2.DepthInfo := GetIrriInfoRecord2_DepthInfo();
end;

function GetCutInfoRecord1() : repCutInfoRecord;
begin;
    GetCutInfoRecord1.NoMoreInfo := GetCutInfoRecord1_NoMoreInfo();
    GetCutInfoRecord1.FromDay := GetCutInfoRecord1_FromDay();
    GetCutInfoRecord1.ToDay := GetCutInfoRecord1_ToDay();
    GetCutInfoRecord1.IntervalInfo := GetCutInfoRecord1_IntervalInfo();
    GetCutInfoRecord1.IntervalGDD := GetCutInfoRecord1_IntervalGDD();
    GetCutInfoRecord1.MassInfo := GetCutInfoRecord1_MassInfo();
end; 

function GetCutInfoRecord2() : repCutInfoRecord;
begin;
    GetCutInfoRecord2.NoMoreInfo := GetCutInfoRecord2_NoMoreInfo();
    GetCutInfoRecord2.FromDay := GetCutInfoRecord2_FromDay();
    GetCutInfoRecord2.ToDay := GetCutInfoRecord2_ToDay();
    GetCutInfoRecord2.IntervalInfo := GetCutInfoRecord2_IntervalInfo();
    GetCutInfoRecord2.IntervalGDD := GetCutInfoRecord2_IntervalGDD();
    GetCutInfoRecord2.MassInfo := GetCutInfoRecord2_MassInfo();
end;

procedure SetIrriInfoRecord1(constref IrriInfoRecord1 : repIrriInfoRecord);
begin;
    SetIrriInfoRecord1_NoMoreInfo(IrriInfoRecord1.NoMoreInfo);
    SetIrriInfoRecord1_FromDay(IrriInfoRecord1.FromDay);
    SetIrriInfoRecord1_ToDay(IrriInfoRecord1.ToDay);
    SetIrriInfoRecord1_TimeInfo(IrriInfoRecord1.TimeInfo);
    SetIrriInfoRecord1_DepthInfo(IrriInfoRecord1.DepthInfo);
end;

procedure SetIrriInfoRecord2(constref IrriInfoRecord2 : repIrriInfoRecord);
begin;
    SetIrriInfoRecord2_NoMoreInfo(IrriInfoRecord2.NoMoreInfo);
    SetIrriInfoRecord2_FromDay(IrriInfoRecord2.FromDay);
    SetIrriInfoRecord2_ToDay(IrriInfoRecord2.ToDay);
    SetIrriInfoRecord2_TimeInfo(IrriInfoRecord2.TimeInfo);
    SetIrriInfoRecord2_DepthInfo(IrriInfoRecord2.DepthInfo);
end;

procedure SetCutInfoRecord1(constref CutInfoRecord1 : repCutInfoRecord);
begin;
    SetCutInfoRecord1_NoMoreInfo(CutInfoRecord1.NoMoreInfo);
    SetCutInfoRecord1_FromDay(CutInfoRecord1.FromDay);
    SetCutInfoRecord1_ToDay(CutInfoRecord1.ToDay);
    SetCutInfoRecord1_IntervalInfo(CutInfoRecord1.IntervalInfo);
    SetCutInfoRecord1_IntervalGDD(CutInfoRecord1.IntervalGDD);
    SetCutInfoRecord1_MassInfo(CutInfoRecord1.MassInfo);
end;

procedure SetCutInfoRecord2(constref CutInfoRecord2 : repCutInfoRecord);
begin;
    SetCutInfoRecord2_NoMoreInfo(CutInfoRecord2.NoMoreInfo);
    SetCutInfoRecord2_FromDay(CutInfoRecord2.FromDay);
    SetCutInfoRecord2_ToDay(CutInfoRecord2.ToDay);
    SetCutInfoRecord2_IntervalInfo(CutInfoRecord2.IntervalInfo);
    SetCutInfoRecord2_IntervalGDD(CutInfoRecord2.IntervalGDD);
    SetCutInfoRecord2_MassInfo(CutInfoRecord2.MassInfo);
end;

initialization


end.

