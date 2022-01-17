unit interface_tempprocessing;

interface

uses Global, interface_global;


PROCEDURE AdjustMONTHandYEAR(
            VAR Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustmonthandyear';

PROCEDURE AdjustDecadeMONTHandYEAR(
            VAR DecFile,Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustdecademonthandyear';

PROCEDURE SetDayNrToYundef(
            VAR DayNri : LongInt);
        external 'aquacrop' name '__ac_tempprocessing_MOD_setdaynrtoyundef';

PROCEDURE GetDecadeTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getdecadetemperaturedataset';

PROCEDURE GetMonthlyTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getmonthlytemperaturedataset';

implementation


initialization


end.

