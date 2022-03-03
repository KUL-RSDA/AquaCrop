unit interface_tempprocessing;

interface

uses Global, interface_global;


procedure AdjustMONTHandYEAR(
            VAR Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustmonthandyear';

procedure AdjustDecadeMONTHandYEAR(
            VAR DecFile,Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustdecademonthandyear';

procedure SetDayNrToYundef(
            VAR DayNri : LongInt);
        external 'aquacrop' name '__ac_tempprocessing_MOD_setdaynrtoyundef';

procedure GetDecadeTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getdecadetemperaturedataset';

procedure GetMonthlyTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getmonthlytemperaturedataset';

function GrowingDegreeDays(
            constref ValPeriod : INTEGER;
            constref FirstDayPeriod : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : integer;
        external 'aquacrop' name '__ac_tempprocessing_MOD_growingdegreedays';

function SumCalendarDays(
            constref ValGDDays : INTEGER;
            constref FirstDayCrop : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : integer;
        external 'aquacrop' name '__ac_tempprocessing_MOD_sumcalendardays';

FUNCTION MaxAvailableGDD(
            constref FromDayNr : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : Double;
         external 'aquacrop' name '__ac_tempprocessing_MOD_maxavailablegdd';

procedure GDDCDCToCDC(
            constref PlantDayNr : LongInt;
            constref D123,GDDL123,GDDHarvest : INTEGER;
            constref CCx,GDDCDC,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
            VAR CDC : double);
        external 'aquacrop' name '__ac_tempprocessing_MOD_gddcdctocdc';

PROCEDURE AdjustCalendarDays(
            constref PlantDayNr : LongInt;
            constref InfoCropType : rep_subkind;
            constref Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
            constref GDDL0,GDDL12,GDDFlor,GDDLengthFlor,GDDL123,GDDHarvest,GDDLZmax : INTEGER;
            VAR GDDHImax : INTEGER;
            constref GDDCGC,GDDCDC,CCo,CCx : double;
            constref IsCGCGiven : BOOLEAN;
            constref HIndex : INTEGER;
            constref TheDaysToCCini : INTEGER;
            constref ThePlanting : rep_planting;
            VAR D0,D12,DFlor,LengthFlor,D123,DHarvest,DLZmax,LHImax : INTEGER;
            VAR StLength : rep_int_array;
            VAR CGC,CDC,dHIdt : double;
            VAR Succes : BOOLEAN);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcalendardays';

procedure AdjustCalendarCrop(
            constref FirstCropDay : LongInt);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcalendarcrop';

function RoundedOffGDD(
            constref PeriodGDD,PeriodDay : INTEGER;
            constref FirstDayPeriod : LongInt;
            constref TempTbase,TempTupper,TempTmin,TempTmax : double) : INTEGER;
         external 'aquacrop' name '__ac_tempprocessing_MOD_roundedoffgdd';

procedure HIadjColdHeat(
            constref TempHarvest,TempFlower,TempLengthFlowering,TempHI : INTEGER;
            constref TempTmin,TempTmax : double;
            constref TempTcold,TempTheat : shortInt;
            constref TempfExcess : smallInt;
            VAR HIadjusted : double;
            VAR ColdStress,HeatStress : BOOLEAN);
         external 'aquacrop' name '__ac_tempprocessing_MOD_hiadjcoldheat';

function ResetCropDay1(
            constref CropDay1IN : LongInt;
            constref SwitchToYear1 : BOOLEAN) : LongInt;
         external 'aquacrop' name '__ac_tempprocessing_MOD_resetcropday1';

procedure CropStressParametersSoilSalinity(
            constref CCxRed,CCdistortion : ShortInt;
            constref CCo,CCx,CGC,GDDCGC : double;
            constref CropDeterm : BOOLEAN;
            constref L12,LFlor,LengthFlor,L123 : INTEGER;
            constref GDDL12,GDDLFlor,GDDLengthFlor,GDDL123 : INTEGER;
            constref TheModeCycle : rep_modeCycle;
            VAR StressResponse : rep_EffectStress);
         external 'aquacrop' name '__ac_tempprocessing_MOD_cropstressparameterssoilsalinity';

procedure TemperatureFileCoveringCropPeriod(
            constref CropFirstDay,CropLastDay : LongInt);
         external 'aquacrop' name '__ac_tempprocessing_MOD_temperaturefilecoveringcropperiod';

procedure AdjustCropFileParameters(
            constref TheCropFileSet : rep_CropFileSet;
            constref LseasonDays : INTEGER;
            constref TheCropDay1 : LongInt;
            constref TheModeCycle  : rep_modeCycle;
            constref TheTbase,TheTupper  : double;
            VAR L123,L1234,GDD123,GDD1234 : INTEGER);
         external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcropfileparameters';

function Bnormalized_wrap(
            constref TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : integer;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;
         external 'aquacrop' name '__ac_interface_tempprocessing_MOD_bnormalized_wrap';

function Bnormalized(
            constref TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : rep_modeCycle;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;


implementation


function Bnormalized(
            constref TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : rep_modeCycle;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;
var
   int_modeCycle : integer;
begin
   int_modeCycle := ord(TheModeCycle);
   Bnormalized := Bnormalized_wrap(
                      TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                      CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,
                      StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress,
                      DeltaWeedStress,
                      StrResCDecline,ShapeFweed,
                      int_modeCycle,
                      FertilityStressOn, TestRecord); 
end;

initialization


end.

