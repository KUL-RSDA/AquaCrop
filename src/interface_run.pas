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

procedure ResetPreviousSum;
    external 'aquacrop' name '__ac_run_MOD_resetprevioussum';

procedure GetGwtSet(constref DayNrIN : LongInt;
                    VAR GwT : rep_GwTable);
        external 'aquacrop' name '__ac_run_MOD_getgwtset';

procedure fDaily_open(constref filename : string; constref mode : string);

procedure fDaily_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_fdaily_open_wrap';

procedure fDaily_write(constref line : string; constref advance : boolean = True);

procedure fDaily_write_wrap(
            constref line_ptr : PChar;
            constref line_len : integer;
            constref advance : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_fdaily_write_wrap';

procedure fDaily_close();
        external 'aquacrop' name '__ac_run_MOD_fdaily_close';

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

function GetBin(): double;
        external 'aquacrop' name '__ac_run_MOD_getbin';

function GetBout(): double;
        external 'aquacrop' name '__ac_run_MOD_getbout';

procedure SetBin(constref Bin_in : double);
        external 'aquacrop' name '__ac_run_MOD_setbin';

procedure SetBout(constref Bout_in : double);
        external 'aquacrop' name '__ac_run_MOD_setbout';

function GetGDDayi(): double;
        external 'aquacrop' name '__ac_run_MOD_getgddayi';

procedure SetGDDayi(constref GDDayi_in : double);
        external 'aquacrop' name '__ac_run_MOD_setgddayi';

function GetCO2i() : double;
    external 'aquacrop' name '__ac_run_MOD_getco2i';

procedure SetCO2i(constref CO2i_in : double);
    external 'aquacrop' name '__ac_run_MOD_setco2i';

function GetPreviousSum() : rep_sum;

function GetPreviousSum_Epot() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_epot';

function GetPreviousSum_Tpot() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_tpot';

function GetPreviousSum_Rain() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_rain';

function GetPreviousSum_Irrigation() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_irrigation';

function GetPreviousSum_Infiltrated() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_infiltrated';

function GetPreviousSum_Runoff() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_runoff';

function GetPreviousSum_Drain() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_drain';

function GetPreviousSum_Eact() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_eact';

function GetPreviousSum_Tact() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_tact';

function GetPreviousSum_TrW() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_trw';

function GetPreviousSum_ECropCycle() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_ecropcycle';

function GetPreviousSum_CRwater() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_crwater';

function GetPreviousSum_Biomass() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_biomass';

function GetPreviousSum_YieldPart() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_yieldpart';

function GetPreviousSum_BiomassPot() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_biomasspot';

function GetPreviousSum_BiomassUnlim() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_biomassunlim';

function GetPreviousSum_BiomassTot() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_biomasstot';

function GetPreviousSum_SaltIn() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_saltin';

function GetPreviousSum_SaltOut() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_saltout';

function GetPreviousSum_CRsalt() : double;
        external 'aquacrop' name '__ac_run_MOD_getprevioussum_crsalt';

procedure SetPreviousSum(constref PreviousSum : rep_sum);

procedure SetPreviousSum_Epot(constref Epot : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_epot';

procedure SetPreviousSum_Tpot(constref Tpot : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_tpot';

procedure SetPreviousSum_Rain(constref Rain : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_rain';

procedure SetPreviousSum_Irrigation(constref Irrigation : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_irrigation';

procedure SetPreviousSum_Infiltrated(constref Infiltrated : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_infiltrated';

procedure SetPreviousSum_Runoff(constref Runoff : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_runoff';

procedure SetPreviousSum_Drain(constref Drain : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_drain';

procedure SetPreviousSum_Eact(constref Eact : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_eact';

procedure SetPreviousSum_Tact(constref Tact : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_tact';

procedure SetPreviousSum_TrW(constref TrW : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_trw';

procedure SetPreviousSum_ECropCycle(constref ECropCycle : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_ecropcycle';

procedure SetPreviousSum_CRwater(constref CRwater : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_crwater';

procedure SetPreviousSum_Biomass(constref Biomass : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_biomass';

procedure SetPreviousSum_yieldpart(constref yieldpart : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_yieldpart';

procedure SetPreviousSum_BiomassPot(constref BiomassPot : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_biomasspot';

procedure SetPreviousSum_BiomassUnlim(constref BiomassUnlim : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_biomassunlim';

procedure SetPreviousSum_BiomassTot(constref BiomassTot : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_biomasstot';

procedure SetPreviousSum_SaltIn(constref SaltIn : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_saltin';

procedure SetPreviousSum_SaltOut(constref SaltOut : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_saltout';

procedure SetPreviousSum_CRsalt(constref CRsalt : double);
        external 'aquacrop' name '__ac_run_MOD_setprevioussum_crsalt';

procedure RelationshipsForFertilityAndSaltStress();
        external 'aquacrop' name '__ac_run_MOD_relationshipsforfertilityandsaltstress';

procedure WriteTitleDailyResults(constref TheProjectType : repTypeProject;
                          constref TheNrRun : Shortint);

procedure __WriteTitleDailyResults(constref TheProjectType : integer;
                           constref TheNrRun : shortint);
        external 'aquacrop' name '__ac_run_MOD_writetitledailyresults';

procedure fEToSIM_open(constref filename : string; constref mode : string);

procedure fEToSIM_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_fetosim_open_wrap';

function fEToSIM_read() : string;

function fEToSIM_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_fetosim_read_wrap';

procedure fEToSIM_close();
        external 'aquacrop' name '__ac_run_MOD_fetosim_close';

procedure fRainSIM_open(constref filename : string; constref mode : string);

procedure fRainSIM_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_frainsim_open_wrap';

function fRainSIM_read() : string;

function fRainSIM_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_frainsim_read_wrap';

procedure fRainSIM_close();
        external 'aquacrop' name '__ac_run_MOD_frainsim_close';

procedure DetermineGrowthStage(
                    constref Dayi : LongInt;
                    constref CCiPrev : double);
        external 'aquacrop' name '__ac_run_MOD_determinegrowthstage';

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

procedure OpenIrrigationFile();
        external 'aquacrop' name '__ac_run_MOD_openirrigationfile';

function GetGlobalIrriECw() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getglobalirriecw_wrap';

procedure SetGlobalIrriECw(constref GlobalIrriECw_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setglobalirriecw_wrap';

function GetWaterTableInProfile() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getwatertableinprofile_wrap';

procedure SetWaterTableInProfile(constref WaterTableInProfile_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setwatertableinprofile_wrap';

function GetStartMode() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getstartmode_wrap';

procedure SetStartMode(constref StartMode_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setstartmode_wrap';

function GetNoMoreCrop() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getnomorecrop_wrap';

procedure SetNoMoreCrop(constref NoMoreCrop_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setnomorecrop_wrap';

function GetCGCadjustmentAfterCutting() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getcgcadjustmentaftercutting_wrap';

procedure SetCGCadjustmentAfterCutting(constref CGCadjustmentAfterCutting_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setcgcadjustmentaftercutting_wrap';

procedure fObs_open(constref filename : string; constref mode : string);

procedure fObs_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_fobs_open_wrap';

function fObs_read() : string;

function fObs_read_wrap() : PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_fobs_read_wrap';

function fObs_eof() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_fobs_eof_wrap';

procedure fObs_close();
        external 'aquacrop' name '__ac_run_MOD_fobs_close';

procedure fObs_rewind();
        external 'aquacrop' name '__ac_run_MOD_fobs_rewind';

function GetIrriInterval() : integer;
    external 'aquacrop' name '__ac_run_MOD_getirriinterval';

procedure SetIrriInterval(constref IrriInterval : integer);
    external 'aquacrop' name '__ac_run_MOD_setirriinterval';

function GetTadj() : integer;
    external 'aquacrop' name '__ac_run_MOD_gettadj';

procedure SetTadj(constref Tadj : integer);
    external 'aquacrop' name '__ac_run_MOD_settadj';

function GetGDDTadj() : integer;
    external 'aquacrop' name '__ac_run_MOD_getgddtadj';

procedure SetGDDTadj(constref GDDTadj : integer);
    external 'aquacrop' name '__ac_run_MOD_setgddtadj';

function GetDayLastCut() : integer;
    external 'aquacrop' name '__ac_run_MOD_getdaylastcut';

procedure SetDayLastCut(constref DayLastCut : integer);
    external 'aquacrop' name '__ac_run_MOD_setdaylastcut';

function GetNrCut() : integer;
    external 'aquacrop' name '__ac_run_MOD_getnrcut';

procedure SetNrCut(constref NrCut : integer); 
    external 'aquacrop' name '__ac_run_MOD_setnrcut';

function GetSumInterval() : integer;
    external 'aquacrop' name '__ac_run_MOD_getsuminterval';

procedure SetSumInterval(constref SumInterval : integer);
    external 'aquacrop' name '__ac_run_MOD_setsuminterval';

function GetPreviousStressLevel() : shortint;
    external 'aquacrop' name '__ac_run_MOD_getpreviousstresslevel';

procedure SetPreviousStressLevel(constref PreviousStressLevel : shortint);
    external 'aquacrop' name '__ac_run_MOD_setpreviousstresslevel';

function GetStressSFadjNEW() : shortint;
    external 'aquacrop' name '__ac_run_MOD_getstresssfadjnew';

procedure SetStressSFadjNEW(constref StressSFadjNEW : shortint);
    external 'aquacrop' name '__ac_run_MOD_setstresssfadjnew';

procedure GetNextHarvest()
    external 'aquacrop' name '__ac_run_MOD_getnextharvest';     

function GetCCxWitheredTpot() : double;
    external 'aquacrop' name '__ac_run_MOD_getccxwitheredtpot';

procedure SetCCxWitheredTpot(constref CCxWitheredTpot : double);
    external 'aquacrop' name '__ac_run_MOD_setccxwitheredtpot';

function GetCCxWitheredTpotNoS() : double;
    external 'aquacrop' name '__ac_run_MOD_getccxwitheredtpotnos';

procedure SetCCxWitheredTpotNoS(constref CCxWitheredTpotNoS : double);
    external 'aquacrop' name '__ac_run_MOD_setccxwitheredtpotnos';

function GetCoeffb0() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb0';

procedure SetCoeffb0(constref Coeffb0 : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb0';

function GetCoeffb1() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb1';

procedure SetCoeffb1(constref Coeffb1 : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb1';

function GetCoeffb2() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb2';

procedure SetCoeffb2(constref Coeffb2 : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb2';

function GetCoeffb0Salt() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb0salt';

procedure SetCoeffb0Salt(constref Coeffb0Salt : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb0salt';

function GetCoeffb1Salt() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb1salt';

procedure SetCoeffb1Salt(constref Coeffb1Salt : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb1salt';

function GetCoeffb2Salt() : double;
    external 'aquacrop' name '__ac_run_MOD_getcoeffb2salt';

procedure SetCoeffb2Salt(constref Coeffb2Salt : double);
    external 'aquacrop' name '__ac_run_MOD_setcoeffb2salt';

function GetStressLeaf() : double;
    external 'aquacrop' name '__ac_run_MOD_getstressleaf';

procedure SetStressLeaf(constref StressLeaf : double);
    external 'aquacrop' name '__ac_run_MOD_setstressleaf';

function GetStressSenescence() : double;
    external 'aquacrop' name '__ac_run_MOD_getstresssenescence';

procedure SetStressSenescence(constref StressSenescence : double);
    external 'aquacrop' name '__ac_run_MOD_setstresssenescence';

function GetDayFraction() : double;
    external 'aquacrop' name '__ac_run_MOD_getdayfraction';

procedure SetDayFraction(constref DayFraction : double);
    external 'aquacrop' name '__ac_run_MOD_setdayfraction';

function GetGDDayFraction() : double;
    external 'aquacrop' name '__ac_run_MOD_getgddayfraction';

procedure SetGDDayFraction(constref GDDayFraction : double);
    external 'aquacrop' name '__ac_run_MOD_setgddayfraction';

function GetGDDCGCref() : double;
    external 'aquacrop' name '__ac_run_MOD_getgddcgcref';

procedure SetGDDCGCref(constref GDDCGCref : double);
    external 'aquacrop' name '__ac_run_MOD_setgddcgcref';

function GetCGCref() : double;
    external 'aquacrop' name '__ac_run_MOD_getcgcref';

procedure SetCGCref(constref CGCref : double);
    external 'aquacrop' name '__ac_run_MOD_setcgcref';

procedure OpenOutputRun(constref TheProjectType : repTypeProject);

procedure __OpenOutputRun(constref TheProjectType : integer);
    external 'aquacrop' name '__ac_run_MOD_openoutputrun';

procedure OpenOutputDaily(constref TheProjectType : repTypeProject);

procedure __OpenOutputDaily(constref TheProjectType : integer);
    external 'aquacrop' name '__ac_run_MOD_openoutputdaily';

function GetSumETo() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumeto';

procedure SetSumETo(constref SumETo : double);
    external 'aquacrop' name '__ac_run_MOD_setsumeto';

function GetSumGDD() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumgdd';

procedure SetSumGDD(constref SumGDD : double);
    external 'aquacrop' name '__ac_run_MOD_setsumgdd';

function GetTimeSenescence() : double;
    external 'aquacrop' name '__ac_run_MOD_gettimesenescence';

procedure SetTimeSenescence(constref TimeSenescence : double);
    external 'aquacrop' name '__ac_run_MOD_settimesenescence';

function GetSumKcTop() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumkctop';

procedure SetSumKcTop(constref SumKcTop : double);
    external 'aquacrop' name '__ac_run_MOD_setsumkctop';

function GetSumKcTopStress() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumkctopstress';

procedure SetSumKcTopStress(constref SumKcTopStress : double);
    external 'aquacrop' name '__ac_run_MOD_setsumkctopstress';

function GetSumKci() : double; 
    external 'aquacrop' name '__ac_run_MOD_getsumkci';

procedure SetSumKci(constref SumKci : double);
    external 'aquacrop' name '__ac_run_MOD_setsumkci';

function GetCCxCropWeedsNoSFstress() : double;
    external 'aquacrop' name '__ac_run_MOD_getccxcropweedsnosfstress';

procedure SetCCxCropWeedsNoSFstress(constref CCxCropWeedsNoSFstress : double);
    external 'aquacrop' name '__ac_run_MOD_setccxcropweedsnosfstress';

function GetZiprev() : double;
    external 'aquacrop' name '__ac_run_MOD_getziprev';

procedure SetZiprev(constref Ziprev : double);
    external 'aquacrop' name '__ac_run_MOD_setziprev';

function GetSumGDDPrev() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumgddprev';

procedure SetSumGDDPrev(constref SumGDDPrev : double);
    external 'aquacrop' name '__ac_run_MOD_setsumgddprev';

function GetPreviousSumETo() : double;
    external 'aquacrop' name '__ac_run_MOD_getprevioussumeto';

procedure SetPreviousSumETo(constref PreviousSumETo : double);
    external 'aquacrop' name '__ac_run_MOD_setprevioussumeto';

function GetPreviousSumGDD() : double;
    external 'aquacrop' name '__ac_run_MOD_getprevioussumgdd';

procedure SetPreviousSumGDD(constref PreviousSumGDD : double);
    external 'aquacrop' name '__ac_run_MOD_setprevioussumgdd';

function GetPreviousBmob() : double;
    external 'aquacrop' name '__ac_run_MOD_getpreviousbmob';

procedure SetPreviousBmob(constref PreviousBmob : double);
    external 'aquacrop' name '__ac_run_MOD_setpreviousbmob';

function GetPreviousBsto() : double;
    external 'aquacrop' name '__ac_run_MOD_getpreviousbsto';

procedure SetPreviousBsto(constref PreviousBsto : double);
    external 'aquacrop' name '__ac_run_MOD_setpreviousbsto';

function GetCCoTotal() : double;
    external 'aquacrop' name '__ac_run_MOD_getccototal';

procedure SetCCoTotal(constref CCoTotal : double);
    external 'aquacrop' name '__ac_run_MOD_setccototal';

function GetCCxTotal() : double;
    external 'aquacrop' name '__ac_run_MOD_getccxtotal';

procedure SetCCxTotal(constref CCxTotal : double);
    external 'aquacrop' name '__ac_run_MOD_setccxtotal';

function GetCDCTotal() : double;
    external 'aquacrop' name '__ac_run_MOD_getcdctotal';

procedure SetCDCTotal(constref CDCTotal : double);
    external 'aquacrop' name '__ac_run_MOD_setcdctotal';

function GetGDDCDCTotal() : double;
    external 'aquacrop' name '__ac_run_MOD_getgddcdctotal';

procedure SetGDDCDCTotal(constref GDDCDCTotal : double);
    external 'aquacrop' name '__ac_run_MOD_setgddcdctotal';

function GetWeedRCi() : double;
    external 'aquacrop' name '__ac_run_MOD_getweedrci';

procedure SetWeedRCi(constref WeedRCi : double);
    external 'aquacrop' name '__ac_run_MOD_setweedrci';

function GetCCiActualWeedInfested() : double;
    external 'aquacrop' name '__ac_run_MOD_getcciactualweedinfested';

procedure SetCCiActualWeedInfested(constref CCiActualWeedInfested : double);
    external 'aquacrop' name '__ac_run_MOD_setcciactualweedinfested';

function GetfWeedNoS() : double;
    external 'aquacrop' name '__ac_run_MOD_getfweednos';

procedure SetfWeedNoS(constref fWeedNoS : double);
    external 'aquacrop' name '__ac_run_MOD_setfweednos';

function GetZeval() : double;
    external 'aquacrop' name '__ac_run_MOD_getzeval';

procedure SetZeval(constref Zeval : double);
    external 'aquacrop' name '__ac_run_MOD_setzeval';

function GetBprevSum() : double;
    external 'aquacrop' name '__ac_run_MOD_getbprevsum';

procedure SetBprevSum(constref BprevSum : double);
    external 'aquacrop' name '__ac_run_MOD_setbprevsum';

function GetYprevSum() : double;
    external 'aquacrop' name '__ac_run_MOD_getyprevsum';

procedure SetYprevSum(constref YprevSum : double);
    external 'aquacrop' name '__ac_run_MOD_setyprevsum';

function GetSumGDDcuts() : double;
    external 'aquacrop' name '__ac_run_MOD_getsumgddcuts';

procedure SetSumGDDcuts(constref SumGDDcuts : double);
    external 'aquacrop' name '__ac_run_MOD_setsumgddcuts';

function GetHItimesBEF() : double;
    external 'aquacrop' name '__ac_run_MOD_gethitimesbef';

procedure SetHItimesBEF(constref HItimesBEF : double);
    external 'aquacrop' name '__ac_run_MOD_sethitimesbef';

function GetScorAT1() : double;
    external 'aquacrop' name '__ac_run_MOD_getscorat1';

procedure SetScorAT1(constref ScorAT1 : double);
    external 'aquacrop' name '__ac_run_MOD_setscorat1';

function GetScorAT2() : double;
    external 'aquacrop' name '__ac_run_MOD_getscorat2';

procedure SetScorAT2(constref ScorAT2 : double);
    external 'aquacrop' name '__ac_run_MOD_setscorat2';

function GetHItimesAT1() : double;
    external 'aquacrop' name '__ac_run_MOD_gethitimesat1';

procedure SetHItimesAT1(constref HItimesAT1 : double);
    external 'aquacrop' name '__ac_run_MOD_sethitimesat1';

function GetHItimesAT2() : double;
    external 'aquacrop' name '__ac_run_MOD_gethitimesat2';

procedure SetHItimesAT2(constref HItimesAT2 : double);
    external 'aquacrop' name '__ac_run_MOD_sethitimesat2';

function GetHItimesAT() : double;
    external 'aquacrop' name '__ac_run_MOD_gethitimesat';

procedure SetHItimesAT(constref HItimesAT : double);
    external 'aquacrop' name '__ac_run_MOD_sethitimesat';

function GetalfaHI() : double;
    external 'aquacrop' name '__ac_run_MOD_getalfahi';

procedure SetalfaHI(constref alfaHI : double);
    external 'aquacrop' name '__ac_run_MOD_setalfahi';

function GetalfaHIAdj() : double;
    external 'aquacrop' name '__ac_run_MOD_getalfahiadj';

procedure SetalfaHIAdj(constref alfaHIAdj : double);
    external 'aquacrop' name '__ac_run_MOD_setalfahiadj';

function GetDayNr1Eval() : integer;
    external 'aquacrop' name '__ac_run_MOD_getdaynr1eval';

procedure SetDayNr1Eval(constref DayNr1Eval : integer);
    external 'aquacrop' name '__ac_run_MOD_setdaynr1eval';

function GetDayNrEval() : integer;
    external 'aquacrop' name '__ac_run_MOD_getdaynreval';

procedure SetDayNrEval(constref DayNrEval : integer);
    external 'aquacrop' name '__ac_run_MOD_setdaynreval';

function GetLineNrEval() : integer;
    external 'aquacrop' name '__ac_run_MOD_getlinenreval';

procedure SetLineNrEval(constref LineNrEval : integer);
    external 'aquacrop' name '__ac_run_MOD_setlinenreval';

function GetNextSimFromDayNr() : integer;
    external 'aquacrop' name '__ac_run_MOD_getnextsimfromdaynr';

procedure SetNextSimFromDayNr(constref NextSimFromDayNr : integer);
    external 'aquacrop' name '__ac_run_MOD_setnextsimfromdaynr';

function GetStageCode() : integer;
    external 'aquacrop' name '__ac_run_MOD_getstagecode';

procedure SetStageCode(constref StageCode : integer);
    external 'aquacrop' name '__ac_run_MOD_setstagecode';

function GetPreviousDayNr() : integer;
    external 'aquacrop' name '__ac_run_MOD_getpreviousdaynr';

procedure SetPreviousDayNr(constref PreviousDayNr : integer);
    external 'aquacrop' name '__ac_run_MOD_setpreviousdaynr';

function GetNoYear() : boolean;
        external 'aquacrop' name '__ac_interface_run_MOD_getnoyear_wrap';

procedure SetNoYear(constref NoYear_in : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_setnoyear_wrap';

function GetfEval_filename() : string;

function GetfEval_filename_wrap() : PChar;
    external 'aquacrop' name '__ac_interface_run_MOD_getfeval_filename_wrap';

procedure SetfEval_filename(constref filename : string);

procedure SetfEval_filename_wrap(constref filename_ptr : PChar;
                                 constref strlen : integer);
    external 'aquacrop' name '__ac_interface_run_MOD_setfeval_filename_wrap';

procedure fEval_open(constref filename : string; constref mode : string);

procedure fEval_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_feval_open_wrap';

procedure fEval_write(constref line : string; constref advance : boolean = True);

procedure fEval_write_wrap(
            constref line_ptr : PChar;
            constref line_len : integer;
            constref advance : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_feval_write_wrap';

procedure fEval_close();
        external 'aquacrop' name '__ac_run_MOD_feval_close';

procedure fEval_erase();
    external 'aquacrop' name '__ac_run_MOD_feval_erase';

function GetfHarvest_filename_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_run_MOD_getfharvest_filename_wrap';

function GetfHarvest_filename(): string;

procedure SetfHarvest_filename(constref str : string);

procedure SetfHarvest_filename_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_setfharvest_filename_wrap';

procedure fHarvest_open(constref filename : string; constref mode : string);

procedure fHarvest_open_wrap(
            constref filename_ptr : PChar;
            constref filename_len : integer;
            constref mode_ptr : PChar;
            constref mode_len : integer);
        external 'aquacrop' name '__ac_interface_run_MOD_fharvest_open_wrap';

procedure fHarvest_write(constref line : string; constref advance : boolean = True);

procedure fHarvest_write_wrap(
            constref line_ptr : PChar;
            constref line_len : integer;
            constref advance : boolean);
        external 'aquacrop' name '__ac_interface_run_MOD_fharvest_write_wrap';

procedure fHarvest_close();
        external 'aquacrop' name '__ac_run_MOD_fharvest_close';

procedure WriteTitlePart1MultResults(constref TheProjectType : repTypeProject;
                          constref TheNrRun : Shortint);

procedure __WriteTitlePart1MultResults(constref TheProjectType : integer;
                           constref TheNrRun : shortint);
    external 'aquacrop' name '__ac_run_MOD_writetitlepart1multresults';

procedure WriteTheResults(constref ANumber : ShortInt;
                         constref Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
                         constref RPer,EToPer,GDDPer,
                         IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer,
                         SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer : double;
                         constref TheProjectFile : string);

procedure WriteTheResults_wrap(constref ANumber : ShortInt;
                         constref Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
                         constref RPer,EToPer,GDDPer,
                         IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer,
                         SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer : double;
                         constref TheProjectFile_ptr : PChar;
                         constref strlen : integer);
    external 'aquacrop' name '__ac_interface_run_MOD_writetheresults_wrap';

procedure InitializeSimulationRun();
        external 'aquacrop' name '__ac_run_MOD_initializesimulationrun';

procedure CreateEvalData(NrRun : ShortInt);
    external 'aquacrop' name '__ac_run_MOD_createevaldata';

procedure OpenPart1MultResults(constref TheProjectType : repTypeProject);

procedure __OpenPart1MultResults(constref TheProjectType : integer);
    external 'aquacrop' name '__ac_run_MOD_openpart1multresults';

procedure CreateDailyClimFiles(constref FromSimDay,ToSimDay : LongInt);
    external 'aquacrop' name '__ac_run_MOD_createdailyclimfiles';

procedure openharvestinfo();
        external 'aquacrop' name '__ac_run_MOD_openharvestinfo';

procedure openclimfilesandgetdatafirstday(constref FirstDayNr : LongInt);
        external 'aquacrop' name '__ac_run_MOD_openclimfilesandgetdatafirstday';

procedure WriteSimPeriod(constref NrRun : ShortInt;
                         constref TheProjectFile : string);

procedure WriteSimPeriod_wrap(constref NrRun : ShortInt;
                         constref TheProjectFile_ptr : PChar;
                         constref strlen : integer);
    external 'aquacrop' name '__ac_interface_run_MOD_writesimperiod_wrap';

procedure GetZandECgwt(VAR ZiAqua : INTEGER;
                       VAR ECiAqua : double);
         external 'aquacrop' name '__ac_run_MOD_getzandecgwt';

function IrriOutSeason() : INTEGER;
         external 'aquacrop' name '__ac_run_MOD_irrioutseason';

function IrriManual() : INTEGER;
         external 'aquacrop' name '__ac_run_MOD_irrimanual';

procedure WriteIntermediatePeriod(TheProjectFile : string);

procedure WriteIntermediatePeriod_wrap( constref TheProjectFile_ptr : PChar;
                                        constref strlen : integer);
    external 'aquacrop' name '__ac_interface_run_MOD_writeintermediateperiod_wrap';


implementation

procedure WriteIntermediatePeriod(TheProjectFile : string);
var
    TheProjectFile_ptr : PChar;
    strlen : integer;
begin
    TheProjectFile_ptr := PChar(TheProjectFile);
    strlen := Length(TheProjectFile);
    WriteIntermediatePeriod_wrap(TheProjectFile_ptr, strlen);
end;



procedure WriteSimPeriod(constref NrRun : ShortInt;
                         constref TheProjectFile : string);
var
    TheProjectFile_ptr : PChar;
    strlen : integer;
begin
    TheProjectFile_ptr := PChar(TheProjectFile);
    strlen := Length(TheProjectFile);
    WriteSimPeriod_wrap(NrRun, TheProjectFile_ptr, strlen);
end;



procedure WriteTheResults(constref ANumber : ShortInt;
                         constref Day1,Month1,Year1,DayN,MonthN,YearN : INTEGER;
                         constref RPer,EToPer,GDDPer,
                         IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer,
                         SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer : double;
                         constref TheProjectFile : string);
var
    TheProjectFile_ptr : PChar;
    strlen : integer;
begin
    TheProjectFile_ptr := PChar(TheProjectFile);
    strlen := Length(TheProjectFile);
    WriteTheResults_wrap(ANumber, Day1, Month1, Year1, DayN, MonthN, YearN, 
                         RPer,EToPer,GDDPer, IrriPer,InfiltPer,ROPer,DrainPer,CRwPer,
                         EPer,ExPer,TrPer,TrWPer,TrxPer, SalInPer,SalOutPer,SalCRPer,
                         BiomassPer,BUnlimPer,BmobPer,BstoPer,
                         TheProjectFile_ptr, strlen);
end;
    


procedure OpenOutputRun(constref TheProjectType : repTypeProject);
var
    int_typeproject : integer;
begin
    int_typeproject := ord(TheProjectType);
    __OpenOutputRun(int_typeproject);
end;


procedure WriteTitlePart1MultResults(constref TheProjectType : repTypeProject;
                          constref TheNrRun : Shortint);
var
    int_typeproject : integer;
begin
    int_typeproject := ord(TheProjectType);
    __WriteTitlePart1MultResults(int_typeproject, TheNrRun);
end;


procedure OpenOutputDaily(constref TheProjectType : repTypeProject);
var
    int_typeproject : integer;
begin
    int_typeproject := ord(TheProjectType);
    __OpenOutputDaily(int_typeproject);
end;

procedure OpenPart1MultResults(constref TheProjectType : repTypeProject);
var
    int_typeproject : integer;
begin
    int_typeproject := ord(TheProjectType);
    __OpenPart1MultResults(int_typeproject);
end;


function GetfEval_filename() : string;
var
    filename_ptr : PChar;
begin
    filename_ptr := GetfEval_filename_wrap();
    GetfEval_filename := AnsiString(filename_ptr);
end;


procedure SetfEval_filename(constref filename : string);
var
    filename_ptr : PChar;
    strlen : integer;
begin
    filename_ptr := PChar(filename);
    strlen := Length(filename);
    SetfEval_filename_wrap(filename_ptr, strlen);
end;

procedure fEval_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fEval_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

procedure fEval_write(constref line : string; constref advance : boolean = True);
var
     line_ptr : PChar;
     line_len : integer;
begin;
     line_ptr := PChar(line);
     line_len := Length(line);
     fEval_write_wrap(line_ptr, line_len, advance);
end;


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

procedure fDaily_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fDaily_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

procedure fDaily_write(constref line : string; constref advance : boolean = True);
var
     line_ptr : PChar;
     line_len : integer;
begin;
     line_ptr := PChar(line);
     line_len := Length(line);
     fDaily_write_wrap(line_ptr, line_len, advance);
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



procedure fEToSIM_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fEToSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
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


function fEToSIM_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fEToSIM_read_wrap();
     fEToSIM_read := AnsiString(line_ptr);
end;


procedure fRainSIM_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fRainSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
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


function fRainSIM_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fRainSIM_read_wrap();
     fRainSIM_read := AnsiString(line_ptr);
end;


function fCuts_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fCuts_read_wrap();
     fCuts_read := AnsiString(line_ptr);
end;


procedure fObs_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fObs_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;

function fObs_read() : string;
var
     line_ptr : PChar;
begin;
     line_ptr := fObs_read_wrap();
     fObs_read := AnsiString(line_ptr);
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

procedure WriteTitleDailyResults(constref TheProjectType : repTypeProject;
                          constref TheNrRun : Shortint);
var
    int_typeproject : integer;
begin
    int_typeproject := ord(TheProjectType);
    __WriteTitleDailyResults(int_typeproject, TheNrRun);
end;

function GetPreviousSum() : rep_sum;
begin;
    GetPreviousSum.Epot := GetPreviousSum_Epot();
    GetPreviousSum.Tpot := GetPreviousSum_Tpot();
    GetPreviousSum.Rain := GetPreviousSum_Rain();
    GetPreviousSum.Irrigation := GetPreviousSum_Irrigation();
    GetPreviousSum.Infiltrated := GetPreviousSum_Infiltrated();
    GetPreviousSum.Runoff := GetPreviousSum_Runoff();
    GetPreviousSum.Drain := GetPreviousSum_Drain();
    GetPreviousSum.Eact := GetPreviousSum_Eact();
    GetPreviousSum.Tact := GetPreviousSum_Tact();
    GetPreviousSum.TrW := GetPreviousSum_TrW();
    GetPreviousSum.ECropCycle := GetPreviousSum_ECropCycle();
    GetPreviousSum.CRwater := GetPreviousSum_CRwater();
    GetPreviousSum.Biomass := GetPreviousSum_Biomass();
    GetPreviousSum.YieldPart := GetPreviousSum_YieldPart();
    GetPreviousSum.BiomassPot := GetPreviousSum_BiomassPot();
    GetPreviousSum.BiomassUnlim := GetPreviousSum_BiomassUnlim();
    GetPreviousSum.BiomassTot := GetPreviousSum_BiomassTot();
    GetPreviousSum.SaltIn := GetPreviousSum_SaltIn();
    GetPreviousSum.SaltOut := GetPreviousSum_SaltOut();
    GetPreviousSum.CRsalt := GetPreviousSum_CRsalt();
end;

procedure SetPreviousSum(constref PreviousSum : rep_sum);
begin;
    SetPreviousSum_Epot(PreviousSum.Epot);
    SetPreviousSum_Tpot(PreviousSum.Tpot);
    SetPreviousSum_Rain(PreviousSum.Rain);
    SetPreviousSum_Irrigation(PreviousSum.Irrigation);
    SetPreviousSum_Infiltrated(PreviousSum.Infiltrated);
    SetPreviousSum_Runoff(PreviousSum.Runoff);
    SetPreviousSum_Drain(PreviousSum.Drain);
    SetPreviousSum_Eact(PreviousSum.Eact);
    SetPreviousSum_Tact(PreviousSum.Tact);
    SetPreviousSum_TrW(PreviousSum.TrW);
    SetPreviousSum_ECropCycle(PreviousSum.ECropCycle);
    SetPreviousSum_CRwater(PreviousSum.CRwater);
    SetPreviousSum_Biomass(PreviousSum.Biomass);
    SetPreviousSum_YieldPart(PreviousSum.YieldPart);
    SetPreviousSum_BiomassPot(PreviousSum.BiomassPot);
    SetPreviousSum_BiomassUnlim(PreviousSum.BiomassUnlim);
    SetPreviousSum_BiomassTot(PreviousSum.BiomassTot);
    SetPreviousSum_SaltIn(PreviousSum.SaltIn);
    SetPreviousSum_SaltOut(PreviousSum.SaltOut);
    SetPreviousSum_CRsalt(PreviousSum.CRsalt);
end;


function GetfHarvest_filename(): string;
var
     p : PChar;
begin;
     p := GetfHarvest_filename_wrap();
     GetfHarvest_filename := AnsiString(p);
end;


procedure SetfHarvest_filename(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetfHarvest_filename_wrap(p, strlen);
end;


procedure fHarvest_open(constref filename : string; constref mode : string);
var
     filename_ptr, mode_ptr : PChar;
     filename_len, mode_len : integer;
begin;
     filename_ptr := PChar(filename);
     filename_len := Length(filename);
     mode_ptr := PChar(mode);
     mode_len := Length(mode);
     fHarvest_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len);
end;


procedure fHarvest_write(constref line : string; constref advance : boolean = True);
var
     line_ptr : PChar;
     line_len : integer;
begin;
     line_ptr := PChar(line);
     line_len := Length(line);
     fHarvest_write_wrap(line_ptr, line_len, advance);
end;




initialization


end.

