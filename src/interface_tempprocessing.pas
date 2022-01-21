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


implementation


initialization


end.

