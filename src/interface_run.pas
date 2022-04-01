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

function GetTminDataSet() : rep_SimulationEventsDbl;

function GetTminDataSet_i(constref i : integer) : rep_DayEventDbl;

function GetTminDataSet_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_run_MOD_gettmindataset_daynr';

function GetTminDataSet_Param(constref i : integer) : double;
    external 'aquacrop' name '__ac_run_MOD_gettmindataset_param';

procedure SetTminDataSet(constref TminDataSet_in : rep_SimulationEventsDbl);

procedure SetTminDataSet_i(constref i : integer;
                           constref TminDataSet_i : rep_DayEventDbl);

procedure SetTminDataSet_DayNr(constref i : integer;
                               constref  DayNr_in : integer);
    external 'aquacrop' name '__ac_run_MOD_settmindataset_daynr';

procedure SetTminDataSet_Param(constref i : integer;
                               constref Param_in : double);
    external 'aquacrop' name '__ac_run_MOD_settmindataset_param';

function GetTmaxDataSet() : rep_SimulationEventsDbl;

function GetTmaxDataSet_i(constref i : integer) : rep_DayEventDbl;

function GetTmaxDataSet_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_run_MOD_gettmaxdataset_daynr';

function GetTmaxDataSet_Param(constref i : integer) : double;
    external 'aquacrop' name '__ac_run_MOD_gettmaxdataset_param';

procedure SetTmaxDataSet(constref TmaxDataSet_in : rep_SimulationEventsDbl);

procedure SetTmaxDataSet_i(constref i : integer;
                           constref TmaxDataSet_i : rep_DayEventDbl);

procedure SetTmaxDataSet_DayNr(constref i, DayNr_in: integer);
    external 'aquacrop' name '__ac_run_MOD_settmaxdataset_daynr';

procedure SetTmaxDataSet_Param(constref i : integer;
                               constref Param_in : double);
    external 'aquacrop' name '__ac_run_MOD_settmaxdataset_param';

procedure GetSumGDDBeforeSimulation(var SumGDDtillDay, SumGDDtillDayM1 : double);
    external 'aquacrop' name '__ac_run_MOD_getsumgddbeforesimulation';

function GetDayNri() : LongInt;
    external 'aquacrop' name '__ac_run_MOD_getdaynri';

procedure SetDayNri(constref DayNri_in : LongInt);
    external 'aquacrop' name '__ac_run_MOD_setdaynri';

procedure fRun_open(constref filename : string; constref mode : string);

procedure fRun_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_frun_open_wrap';

procedure fRun_write(constref line : string; constref advance : boolean = True);

procedure fRun_write_wrap(
            constref line_ptr : PChar;
            constref line_len : integer;
            constref advance : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_frun_write_wrap';

procedure fRun_close();
        external 'aquacrop' name '__ac_run_MOD_frun_close';

procedure fIrri_open(constref filename : string; constref mode : string);

procedure fIrri_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_firri_open_wrap';

function fIrri_read() : string;

function fIrri_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_firri_read_wrap';

function fIrri_eof() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_firri_eof_wrap';

procedure fIrri_close();
        external 'aquacrop' name '__ac_run_MOD_firri_close';

function GetFracBiomassPotSF() : double;
    external 'aquacrop' name '__ac_run_MOD_getfracbiomasspotsf';

procedure SetFracBiomassPotSF(constref FracBiomassPotSF_in : double);
    external 'aquacrop' name '__ac_run_MOD_setfracbiomasspotsf';

function GetCO2i() : double;
    external 'aquacrop' name '__ac_run_MOD_getco2i';

procedure SetCO2i(constref CO2i_in : double);
    external 'aquacrop' name '__ac_run_MOD_setco2i';


procedure RelationshipsForFertilityAndSaltStress(
                    VAR Coeffb0 : double;
                    VAR Coeffb1 : double;
                    VAR Coeffb2 : double;
                    VAR FracBiomassPotSF : double;
                    VAR Coeffb0Salt : double;
                    VAR Coeffb1Salt : double;
                    VAR Coeffb2Salt : double);
        external 'aquacrop' name '__ac_run_MOD_relationshipsforfertilityandsaltstress';

function GetEToDataSet() : rep_SimulationEventsDbl;

function GetEToDataSet_i(constref i : integer) : rep_DayEventDbl;

function GetEToDataSet_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_run_MOD_getetodataset_daynr';

function GetEToDataSet_Param(constref i : integer) : double;
    external 'aquacrop' name '__ac_run_MOD_getetodataset_param';

procedure SetEToDataSet(constref EToDataSet_in : rep_SimulationEventsDbl);

procedure SetEToDataSet_i(constref i : integer;
                           constref EToDataSet_i : rep_DayEventDbl);

procedure SetEToDataSet_DayNr(constref i : integer;
                               constref  DayNr_in : integer);
    external 'aquacrop' name '__ac_run_MOD_setetodataset_daynr';

procedure SetEToDataSet_Param(constref i : integer;
                               constref Param_in : double);
    external 'aquacrop' name '__ac_run_MOD_setetodataset_param';

function GetRainDataSet() : rep_SimulationEventsDbl;

function GetRainDataSet_i(constref i : integer) : rep_DayEventDbl;

function GetRainDataSet_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_run_MOD_getraindataset_daynr';

function GetRainDataSet_Param(constref i : integer) : double;
    external 'aquacrop' name '__ac_run_MOD_getraindataset_param';

procedure SetRainDataSet(constref RainDataSet_in : rep_SimulationEventsDbl);

procedure SetRainDataSet_i(constref i : integer;
                           constref RainDataSet_i : rep_DayEventDbl);

procedure SetRainDataSet_DayNr(constref i : integer;
                               constref  DayNr_in : integer);
    external 'aquacrop' name '__ac_run_MOD_setraindataset_daynr';

procedure SetRainDataSet_Param(constref i : integer;
                               constref Param_in : double);
    external 'aquacrop' name '__ac_run_MOD_setraindataset_param';

procedure fTempSIM_open(constref filename : string; constref mode : string);

procedure fTempSIM_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_ftempsim_open_wrap';

function fTempSIM_read() : string;

function fTempSIM_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_ftempsim_read_wrap';

procedure fTempSIM_close();
        external 'aquacrop' name '__ac_run_MOD_ftempsim_close';

procedure fCuts_open(constref filename : string; constref mode : string);

procedure fCuts_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_fcuts_open_wrap';

function fCuts_read() : string;

function fCuts_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_fcuts_read_wrap';

function fCuts_eof() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_fcuts_eof_wrap';

procedure fCuts_close();
        external 'aquacrop' name '__ac_run_MOD_fcuts_close';


implementation


function GetTminDataSet() : rep_SimulationEventsDbl;
var
    i : integer;
begin
    for i := 1 to 31 do GetTminDataSet[i] := GetTminDataSet_i(i);
end;

function GetTminDataSet_i(constref i : integer) : rep_DayEventDbl;
begin
    GetTminDataSet_i.DayNr := GetTminDataSet_DayNr(i);
    GetTminDataSet_i.Param := GetTminDataSet_Param(i);
end;

procedure SetTminDataSet(constref TminDataSet_in : rep_SimulationEventsDbl);
var
    i : integer;
begin
    for i := 1 to 31 do SetTminDataSet_i(i, TminDataSet_in[i]);
end;

procedure SetTminDataSet_i(constref i : integer;
                          constref TminDataSet_i : rep_DayEventDbl);
begin
    SetTminDataSet_DayNr(i, TminDataSet_i.DayNr);
    SetTminDataSet_Param(i, TminDataSet_i.Param);
end;

function GetTmaxDataSet() : rep_SimulationEventsDbl;
var
    i : integer;
begin
    for i := 1 to 31 do GetTmaxDataSet[i] := GetTmaxDataSet_i(i);
end;

function GetTmaxDataSet_i(constref i : integer) : rep_DayEventDbl;
begin
    GetTmaxDataSet_i.DayNr := GetTmaxDataSet_DayNr(i);
    GetTmaxDataSet_i.Param := GetTmaxDataSet_Param(i);
end;

procedure SetTmaxDataSet(constref TmaxDataSet_in : rep_SimulationEventsDbl);
var
    i : integer;
begin
    for i := 1 to 31 do SetTmaxDataSet_i(i, TmaxDataSet_in[i]);
end;

procedure SetTmaxDataSet_i(constref i : integer;
                          constref TmaxDataSet_i : rep_DayEventDbl);
begin
    SetTmaxDataSet_DayNr(i, TmaxDataSet_i.DayNr);
    SetTmaxDataSet_Param(i, TmaxDataSet_i.Param);
end;


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

procedure fRun_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fRun_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

procedure fRun_write(constref line : string; constref advance : boolean = True);
var
     line_ptr : PChar;
     line_len : integer;
begin;
     line_ptr := PChar(line);
     line_len := Length(line);
     fRun_write_wrap(line_ptr, line_len, advance);
end;

procedure fIrri_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fIrri_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

function fIrri_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fIrri_read_wrap();
     fIrri_read := AnsiString(line_ptr);
end;


procedure fTempSIM_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fTempSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

function fTempSIM_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fTempSIM_read_wrap();
     fTempSIM_read := AnsiString(line_ptr);
end;


procedure fCuts_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fCuts_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

function fCuts_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fCuts_read_wrap();
     fCuts_read := AnsiString(line_ptr);
end;


function GetEToDataSet() : rep_SimulationEventsDbl;
var
    i : integer;
begin
    for i := 1 to 31 do GetEToDataSet[i] := GetEToDataSet_i(i);
end;

function GetEToDataSet_i(constref i : integer) : rep_DayEventDbl;
begin
    GetEToDataSet_i.DayNr := GetEToDataSet_DayNr(i);
    GetEToDataSet_i.Param := GetEToDataSet_Param(i);
end;

procedure SetEToDataSet(constref EToDataSet_in : rep_SimulationEventsDbl);
var
    i : integer;
begin
    for i := 1 to 31 do SetEToDataSet_i(i, EToDataSet_in[i]);
end;

procedure SetEToDataSet_i(constref i : integer;
                          constref EToDataSet_i : rep_DayEventDbl);
begin
    SetEToDataSet_DayNr(i, EToDataSet_i.DayNr);
    SetEToDataSet_Param(i, EToDataSet_i.Param);
end;



function GetRainDataSet() : rep_SimulationEventsDbl;
var
    i : integer;
begin
    for i := 1 to 31 do GetRainDataSet[i] := GetRainDataSet_i(i);
end;

function GetRainDataSet_i(constref i : integer) : rep_DayEventDbl;
begin
    GetRainDataSet_i.DayNr := GetRainDataSet_DayNr(i);
    GetRainDataSet_i.Param := GetRainDataSet_Param(i);
end;

procedure SetRainDataSet(constref RainDataSet_in : rep_SimulationEventsDbl);
var
    i : integer;
begin
    for i := 1 to 31 do SetRainDataSet_i(i, RainDataSet_in[i]);
end;

procedure SetRainDataSet_i(constref i : integer;
                          constref RainDataSet_i : rep_DayEventDbl);
begin
    SetRainDataSet_DayNr(i, RainDataSet_i.DayNr);
    SetRainDataSet_Param(i, RainDataSet_i.Param);
end;

initialization


end.

