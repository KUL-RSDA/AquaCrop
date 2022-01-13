unit interface_climprocessing;

interface

uses Global, interface_global;


procedure GetInterpolationParameters(
            constref C1, C2, C3 : double;
            constref X1, X2, X3 : integer;
            var aOver3, bOver2, c : double);
         external 'aquacrop' name '__ac_climprocessing_MOD_getinterpolationparameters';

implementation


initialization


end.

