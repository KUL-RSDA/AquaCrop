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

procedure TemperatureFileCoveringCropPeriod(
            constref CropFirstDay,CropLastDay : LongInt);
         external 'aquacrop' name '__ac_tempprocessing_MOD_temperaturefilecoveringcropperiod';

implementation


initialization


end.

