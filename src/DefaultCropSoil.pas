unit DefaultCropSoil;

interface

uses Global, interface_global, interface_defaultcropsoil;

PROCEDURE ResetDefaultCrop;
PROCEDURE ResetDefaultSoil;

implementation

PROCEDURE ResetDefaultCrop;
BEGIN
CropDescription := 'a generic crop';
SetCrop_subkind(Grain);
SetCrop_Planting(Seed);
SetCrop_SownYear1(true); // for perennials
SetCrop_ModeCycle(CalendarDays);
SetCrop_pMethod(FAOCorrection);
SetCrop_Tbase(5.5);  // Basal temperature (degC)
SetCrop_Tupper(30.0); // Cut-off temperature (degC)
SetCrop_pLeafDefUL(0.25); //Soil water depletion factor for leaf expansion (p-leaf) - Upper Limit
SetCrop_pLeafDefLL(0.60); //Soil water depletion factor for leaf expansion (p-leaf) - Lower Limit
SetCrop_KsShapeFactorLeaf(3); //Shape factor for Water stress coefficient Leaf expansion (0 = straight line)
SetCrop_pdef(0.50); //Soil water depletion fraction for stomatal control (p - stomatal)
SetCrop_KsShapeFactorStomata(3); //Shape factor for Water stress coefficient Stomatal Control (0 = straight line)
SetCrop_pSenescence(0.85); //Soil water depletion factor for canopy senescence (p-senescence)
SetCrop_KsShapeFactorSenescence(3); //Shape factor for Water stress coefficient Canopy Senescence (0 = straight line)
SetCrop_SumEToDelaySenescence(50); //Sum(ETo) during stress period to be exceeded before senescence is triggered
SetCrop_pPollination(0.90);
SetCrop_AnaeroPoint(5); //Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)

SetCrop_StressResponse_Stress(50);  //Soil fertility stress at calibration (%)
SetCrop_StressResponse_ShapeCGC(+2.16);  //Shape factor for response of Canopy Growth Coefficient to soil fertility stress
SetCrop_StressResponse_ShapeCCX(0.79);  //Shape factor for response of Maximum Canopy Coefficient to soil fertility stress
SetCrop_StressResponse_ShapeWP(+1.67);  //Shape factor for response of Crop water productivity to soil fertility stress
SetCrop_StressResponse_ShapeCDecline(+1.67); //Shape factor for response of Canopy cover decline to soil fertility stress
SetCrop_StressResponse_Calibrated(true);

SetCrop_ECemin(2); //Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)
SetCrop_ECemax(12); //Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)
SetCrop_CCsaltDistortion(25); //Calibrated distortion canopy cover for calibration of simulation of effect of salinity stress (%)
SetCrop_ResponseECsw(100); //Calibrated response of Ks stomata to ECsw for calibration: From 0 (none) to +125 (very strong)
SetCrop_Tcold(8); //Minimum air temperature below which pollination starts to fail (cold stress) (degC)
SetCrop_Theat(40); //Maximum air temperature above which pollination starts to fail (heat stress) (degC)
SetCrop_GDtranspLow(11.1); //Minimum growing degrees required for full crop transpiration (degC - day)
SetCrop_KcTop(1.10); //Crop coefficient when complete cover and prior to senescence (Kc,top)
SetCrop_KcDecline(0.150); //Decline crop coefficient (%/day) as a result of ageing, nitrogen defficiency, etc.
SetCrop_RootMin(0.30); //Minimum rooting depth (m)
SetCrop_RootMax(1.00); //Maximum rooting depth (m)
SetCrop_RootMinYear1(GetCrop().RootMin); //Minimum rooting depth in first year in meter (for perennials)
SetCrop_RootShape(15); //Shape factor describing root zone expansion
SetCrop_SmaxTopQuarter(0.048); //Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone
SetCrop_SmaxBotQuarter(0.012); //Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone
SetCrop_CCEffectEvapLate(50); //Effect of canopy cover on reduction soil evap in late season stage
SetCrop_SizeSeedling(6.50); //Canopy cover per seedling (cm2)
SetCrop_SizePlant(GetCrop().SizeSeedling); // Canopy cover when regrowth (cm2)
SetCrop_PlantingDens(185000); //Number of plants per hectare
SetCrop_CCo((GetCrop().SizeSeedling/10000) * (GetCrop().PlantingDens/10000)); //Starting canopy size (CCo) in fraction
SetCrop_CCini(GetCrop().CCo);
SetCrop_CGC(0.15); //Canopy growth coefficient (CGC): Increase in canopy cover (in fraction) per day
// SetCrop_CGCdx(undef_int); //Maximum decrease (%) of Canopy Growth Coefficient in and between seasons - Forage/Pasture Crops
// SetCrop_CGCns(undef_int); //Number of seasons at which Maximum decrease of Canopy Growth Coefficient is reached - Forage/Pasture Crops
// SetCrop_CGCroot(undef_int); //Shape factor for decrease Canopy Growth Coefficient - Forage/Pasture Crops
SetCrop_YearCCx(undef_int); // the number of years at which CCx declines to 90 % of its value due to self-thining - Perennials
SetCrop_CCxRoot(undef_int); // shape factor of the decline of CCx over the years due to self-thinning - Perennials
SetCrop_CCx(0.80); //Maximum canopy cover (CCx) in fraction
SetCrop_CDC(0.1275); //Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction) per day
SetCrop_DaysToCCini(0);
SetCrop_DaysToGermination(5); //Calendar Days: from sowing to germination
SetCrop_DaysToMaxRooting(100); //Calendar Days: from sowing to maximum rooting depth
SetCrop_DaysToSenescence(110); //Calendar Days: from sowing to start senescence
SetCrop_DaysToHarvest(125); //Calendar Days: from sowing to maturity
SetCrop_DaysToFlowering(70); //Calendar Days: from sowing to flowering
SetCrop_LengthFlowering(10); //Length of the flowering stage (days)
SetCrop_DaysToHIo(50);
SetCrop_DeterminancyLinked(true);
SetCrop_fExcess(50); //Potential excess of fruits (%)
SetCrop_WP(17.0); //(normalized) Water productivity (gram/m2)
SetCrop_WPy(100); //(normalized) Water productivity during yield formation (Percent of WP)
SetCrop_AdaptedToCO2(50); // Percentage adapted to elevated atmospheric CO2 concentration
SetCrop_HI(50); //HI harvest index (percentage)
SetCrop_DryMatter(25); //dry matter content (%) of fresh yield
SetCrop_HIincrease(5); //Possible increase (%) of HI due to water stress before flowering
SetCrop_aCoeff(10); //Coefficient describing positive impact of restricted vegetative growth at flowering on HI
SetCrop_bCoeff(8); //Coefficient describing reduction of impact of stomatal closure at flowering on HI
SetCrop_DHImax(15); //Allowable maximum increase (%) of specified HI
SetCrop_dHIdt(-9); //Caluclated as SetCrop_HI/SetCrop_DaysToHIo
SetCrop_GDDaysToCCini(-9);
SetCrop_GDDaysToGermination(-9); //GDDays: from sowing to germination
SetCrop_GDDaysToMaxRooting(-9); //GDDays: from sowing to maximum rooting depth
SetCrop_GDDaysToSenescence(-9); //GDDays: from sowing to start senescence
SetCrop_GDDaysToHarvest(-9); //GDDays: from sowing to harvest
SetCrop_GDDaysToFlowering(-9); //GDDays: from sowing to flowering
SetCrop_GDDLengthFlowering(-9); //Length of the flowering stage (growing degree days)
SetCrop_GDDaysToHIo(-9);
SetCrop_GDDCGC(-9.000000); //CGC for GGDays: Increase in canopy cover (in fraction) per growing-degree day
SetCrop_GDDCDC(-9.000000); //CDC for GGDays: Decrease in canopy cover (in fraction) growing-degree day

SetCrop_Assimilates_On(false);  // transfer of assimilates from above ground parts to root system is NOT considered
SetCrop_Assimilates_Period(0);  // Number of days before end of season at which storage starts
SetCrop_Assimilates_Stored(0);  // Percentage of assimilates transferred to root system at end of season
SetCrop_Assimilates_Mobilized(0); // Percentage stored assimilates, transferred to above ground parts in next season

SetCropFilefull(CONCAT(GetPathNameSimul(),'DEFAULT.CRO'));
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
SetProfFilefull(CONCAT(GetPathNameSimul(),'DEFAULT.SOL'));
SaveProfile(GetProfFilefull());
END; (* ResetDefaultSoil *)


end.
