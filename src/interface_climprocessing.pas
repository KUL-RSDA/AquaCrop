unit interface_climprocessing;


interface


uses interface_global;


procedure AdjustDecadeMONTHandYEAR(
            var DecFile, Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustdecademonthandyear';

procedure AdjustMONTHandYEAR(
            var Mfile, Yfile : integer);
        external 'aquacrop' name '__ac_climprocessing_MOD_adjustmonthandyear';

procedure GetParameters(
            constref C1, C2, C3 : double;
            var UL, LL, Mid : double);
         external 'aquacrop' name '__ac_climprocessing_MOD_getparameters';

procedure GetInterpolationParameters(
            constref C1, C2, C3 : double;
            constref X1, X2, X3 : integer;
            var aOver3, bOver2, c : double);
         external 'aquacrop' name '__ac_climprocessing_MOD_getinterpolationparameters';

procedure GetMonthlyEToDataSet(constref DayNri : LongInt;
                               VAR EToDataSet : rep_SimulationEventsDbl);
         external 'aquacrop' name '__ac_climprocessing_MOD_getmonthlyetodataset';

procedure GetDecadeEToDataSet(constref DayNri : LongInt;
                              VAR EToDataSet : rep_SimulationEventsDbl);
         external 'aquacrop' name '__ac_climprocessing_MOD_getdecadeetodataset';

procedure GetDecadeRainDataSet(constref DayNri : LongInt;
                               VAR RainDataSet : rep_SimulationEventsDbl);
         external 'aquacrop' name '__ac_climprocessing_MOD_getdecaderaindataset';

procedure GetMonthlyRainDataSet(constref DayNri : LongInt;
                                VAR RainDataSet : rep_SimulationEventsDbl);
         external 'aquacrop' name '__ac_climprocessing_MOD_getmonthlyraindataset';

implementation



initialization


end.
