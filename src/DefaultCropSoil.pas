unit DefaultCropSoil;

interface

uses Global, interface_global, interface_defaultcropsoil;

PROCEDURE ResetDefaultSoil;

implementation

PROCEDURE ResetDefaultSoil;
var
    cra_temp, crb_temp : double;
BEGIN
SetProfDescription('deep loamy soil profile');
SetSoil_CNvalue(61); // for an initial abstraction of 0.05 S
SetSoil_REW(9);
SetSoil_NrSoilLayers(1);
//Soil.Zlimit := undef_int;   // to be removed
SetSoilLayer_Thickness(1, 4);
SetSoilLayer_SAT(1, 50.0);
SetSoilLayer_FC(1, 30.0);
SetSoilLayer_WP(1, 10.0);
SetSoilLayer_InfRate(1, 500.0);
SetSoilLayer_Penetrability(1, 100);
SetSoilLayer_GravelMass(1, 0);
SetSoilLayer_GravelVol(1, 0);
SetSoilLayer_Description(1, 'Loamy soil horizon');
SetSoilLayer_SoilClass(1, NumberSoilClass(GetSoilLayer_SAT(1),GetSoilLayer_FC(1),GetSoilLayer_WP(1),GetSoilLayer_InfRate(1)));
cra_temp := GetSoilLayer_CRa(1);
crb_temp := GetSoilLayer_CRb(1);
DetermineParametersCR(GetSoilLayer_SoilClass(1),GetSoilLayer_InfRate(1),cra_temp, crb_temp);
SetSoilLayer_CRa(1, cra_temp);
SetSoilLayer_CRb(1, crb_temp);
SetProfFilefull(CONCAT(GetPathNameSimul(),'DEFAULT.SOL'));
SaveProfile(GetProfFilefull());
END; (* ResetDefaultSoil *)


end.
