unit DefaultCropSoil;

interface

uses Global, interface_global, interface_defaultcropsoil;

PROCEDURE ResetDefaultCrop;
PROCEDURE ResetDefaultSoil;

implementation

PROCEDURE ResetDefaultCrop;
BEGIN
CropDescription := 'a generic crop';
Crop.subkind := Grain;
Crop.Planting := Seed;
Crop.SownYear1 := true; // for perennials
Crop.ModeCycle := CalendarDays;
Crop.pMethod := FAOCorrection;
Crop.Tbase := 5.5;  // Basal temperature (degC)
Crop.Tupper := 30.0; // Cut-off temperature (degC)
Crop.pLeafDefUL := 0.25; //Soil water depletion factor for leaf expansion (p-leaf) - Upper Limit
Crop.pLeafDefLL := 0.60; //Soil water depletion factor for leaf expansion (p-leaf) - Lower Limit
Crop.KsShapeFactorLeaf := 3; //Shape factor for Water stress coefficient Leaf expansion (0 = straight line)
Crop.pdef := 0.50; //Soil water depletion fraction for stomatal control (p - stomatal)
Crop.KsShapeFactorStomata := 3; //Shape factor for Water stress coefficient Stomatal Control (0 = straight line)
Crop.pSenescence := 0.85; //Soil water depletion factor for canopy senescence (p-senescence)
Crop.KsShapeFactorSenescence := 3; //Shape factor for Water stress coefficient Canopy Senescence (0 = straight line)
Crop.SumEToDelaySenescence := 50; //Sum(ETo) during stress period to be exceeded before senescence is triggered
Crop.pPollination := 0.90;
Crop.AnaeroPoint := 5; //Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)

Crop.StressResponse.Stress := 50;  //Soil fertility stress at calibration (%)
Crop.StressResponse.ShapeCGC := +2.16;  //Shape factor for response of Canopy Growth Coefficient to soil fertility stress
Crop.StressResponse.ShapeCCX := 0.79;  //Shape factor for response of Maximum Canopy Coefficient to soil fertility stress
Crop.StressResponse.ShapeWP := +1.67;  //Shape factor for response of Crop water productivity to soil fertility stress
Crop.StressResponse.ShapeCDecline := +1.67; //Shape factor for response of Canopy cover decline to soil fertility stress
Crop.StressResponse.Calibrated := true;

Crop.ECemin := 2; //Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)
Crop.ECemax := 12; //Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)
Crop.CCsaltDistortion := 25; //Calibrated distortion canopy cover for calibration of simulation of effect of salinity stress (%)
Crop.ResponseECsw := 100; //Calibrated response of Ks stomata to ECsw for calibration: From 0 (none) to +125 (very strong)
Crop.Tcold := 8; //Minimum air temperature below which pollination starts to fail (cold stress) (degC)
Crop.Theat := 40; //Maximum air temperature above which pollination starts to fail (heat stress) (degC)
Crop.GDtranspLow := 11.1; //Minimum growing degrees required for full crop transpiration (degC - day)
Crop.KcTop := 1.10; //Crop coefficient when complete cover and prior to senescence (Kc,top)
Crop.KcDecline := 0.150; //Decline crop coefficient (%/day) as a result of ageing, nitrogen defficiency, etc.
Crop.RootMin := 0.30; //Minimum rooting depth (m)
Crop.RootMax := 1.00; //Maximum rooting depth (m)
Crop.RootMinYear1 := Crop.RootMin; //Minimum rooting depth in first year in meter (for perennials)
Crop.RootShape := 15; //Shape factor describing root zone expansion
Crop.SmaxTopQuarter := 0.048; //Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone
Crop.SmaxBotQuarter := 0.012; //Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone
Crop.CCEffectEvapLate := 50; //Effect of canopy cover on reduction soil evap in late season stage
Crop.SizeSeedling := 6.50; //Canopy cover per seedling (cm2)
Crop.SizePlant := Crop.SizeSeedling; // Canopy cover when regrowth (cm2)
Crop.PlantingDens := 185000; //Number of plants per hectare
Crop.CCo := (Crop.SizeSeedling/10000) * (Crop.PlantingDens/10000); //Starting canopy size (CCo) in fraction
Crop.CCini := Crop.CCo;
Crop.CGC := 0.15; //Canopy growth coefficient (CGC): Increase in canopy cover (in fraction) per day
// Crop.CGCdx := undef_int; //Maximum decrease (%) of Canopy Growth Coefficient in and between seasons - Forage/Pasture Crops
// Crop.CGCns := undef_int; //Number of seasons at which Maximum decrease of Canopy Growth Coefficient is reached - Forage/Pasture Crops
// Crop.CGCroot := undef_int; //Shape factor for decrease Canopy Growth Coefficient - Forage/Pasture Crops
Crop.YearCCx := undef_int; // the number of years at which CCx declines to 90 % of its value due to self-thining - Perennials
Crop.CCxRoot := undef_int; // shape factor of the decline of CCx over the years due to self-thinning - Perennials
Crop.CCx := 0.80; //Maximum canopy cover (CCx) in fraction
Crop.CDC := 0.1275; //Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction) per day
Crop.DaysToCCini := 0;
Crop.DaysToGermination := 5; //Calendar Days: from sowing to germination
Crop.DaysToMaxRooting := 100; //Calendar Days: from sowing to maximum rooting depth
Crop.DaysToSenescence := 110; //Calendar Days: from sowing to start senescence
Crop.DaysToHarvest := 125; //Calendar Days: from sowing to maturity
Crop.DaysToFlowering := 70; //Calendar Days: from sowing to flowering
Crop.LengthFlowering := 10; //Length of the flowering stage (days)
Crop.DaysToHIo := 50;
Crop.DeterminancyLinked := true;
Crop.fExcess := 50; //Potential excess of fruits (%)
Crop.WP := 17.0; //(normalized) Water productivity (gram/m2)
Crop.WPy := 100; //(normalized) Water productivity during yield formation (Percent of WP)
Crop.AdaptedToCO2 := 50; // Percentage adapted to elevated atmospheric CO2 concentration
Crop.HI := 50; //HI harvest index (percentage)
Crop.DryMatter := 25; //dry matter content (%) of fresh yield
Crop.HIincrease := 5; //Possible increase (%) of HI due to water stress before flowering
Crop.aCoeff := 10; //Coefficient describing positive impact of restricted vegetative growth at flowering on HI
Crop.bCoeff := 8; //Coefficient describing reduction of impact of stomatal closure at flowering on HI
Crop.DHImax := 15; //Allowable maximum increase (%) of specified HI
Crop.dHIdt := -9; //Caluclated as Crop.HI/Crop.DaysToHIo
Crop.GDDaysToCCini := -9;
Crop.GDDaysToGermination := -9; //GDDays: from sowing to germination
Crop.GDDaysToMaxRooting := -9; //GDDays: from sowing to maximum rooting depth
Crop.GDDaysToSenescence := -9; //GDDays: from sowing to start senescence
Crop.GDDaysToHarvest := -9; //GDDays: from sowing to harvest
Crop.GDDaysToFlowering := -9; //GDDays: from sowing to flowering
Crop.GDDLengthFlowering := -9; //Length of the flowering stage (growing degree days)
Crop.GDDaysToHIo := -9;
Crop.GDDCGC := -9.000000; //CGC for GGDays: Increase in canopy cover (in fraction) per growing-degree day
Crop.GDDCDC := -9.000000; //CDC for GGDays: Decrease in canopy cover (in fraction) growing-degree day

Crop.Assimilates.On := false;  // transfer of assimilates from above ground parts to root system is NOT considered
Crop.Assimilates.Period := 0;  // Number of days before end of season at which storage starts
Crop.Assimilates.Stored := 0;  // Percentage of assimilates transferred to root system at end of season
Crop.Assimilates.Mobilized := 0; // Percentage stored assimilates, transferred to above ground parts in next season

SetCropFilefull(CONCAT(PathNameSimul,'DEFAULT.CRO'));
SaveCrop(GetCropFilefull());
END; (* ResetDefaultCrop *)



PROCEDURE ResetDefaultSoil;
BEGIN
ProfDescription := 'deep loamy soil profile';
SetSoil_CNvalue(61); // for an initial abstraction of 0.05 S
SetSoil_REW(9);
SetSoil_NrSoilLayers(1);
//Soil.Zlimit := undef_int;   // to be removed
SoilLayer[1].Thickness := 4;
SoilLayer[1].SAT := 50.0;
SoilLayer[1].FC := 30.0;
SoilLayer[1].WP := 10.0;
SoilLayer[1].InfRate := 500.0;
SoilLayer[1].Penetrability := 100;
SoilLayer[1].GravelMass := 0;
SoilLayer[1].GravelVol := 0;
SoilLayer[1].Description := 'Loamy soil horizon';
SoilLayer[1].SoilClass := NumberSoilClass(SoilLayer[1].SAT,SoilLayer[1].FC,SoilLayer[1].WP,SoilLayer[1].InfRate);
DetermineParametersCR(SoilLayer[1].SoilClass,SoilLayer[1].InfRate,SoilLayer[1].CRa,SoilLayer[1].CRb);
SetProfFilefull(CONCAT(PathNameSimul,'DEFAULT.SOL'));
SaveProfile(GetProfFilefull());
END; (* ResetDefaultSoil *)


end.
