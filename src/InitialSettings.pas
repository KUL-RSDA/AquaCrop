unit InitialSettings;

interface


uses Global, interface_global, DefaultCropSoil, interface_defaultcropsoil, interface_initialsettings;

PROCEDURE InitializeSettings;

implementation

 PROCEDURE InitializeSettings;
 TYPE rep_string20 = string[20];
 VAR TempString1,TempString2,CO2descr : string;
     Nri : INTEGER;
     SumWaBal_temp : rep_sum;
     Crop_Day1_temp : INTEGER;
     Crop_DayN_temp : INTEGER;

 BEGIN
 // 1. Program settings
 // Settings of Program parameters
 // 1a. General.PAR

 SetSimulParam_PercRAW(50); //Threshold [% of RAW] for determination of Inet
 SetNrCompartments(12); //Number of soil compartments (maximum is 12) (not a program parameter)
 SetSimulParam_CompDefThick(0.10); //Default thickness of soil compartments [m]
 SetSimulParam_CropDay1(81); //DayNumber of first day cropping period (1..365)
 SetSimulParam_Tbase(10.0);  //Default base temperature (degC) below which no crop development
 SetSimulParam_Tupper(30.0); //Default upper temperature threshold for crop development
 SetSimulParam_IrriFwInSeason(100); //Percentage of soil surface wetted by irrigation in crop season
 SetSimulParam_IrriFwOffSeason(100); // Percentage of soil surface wetted by irrigation off-season

 // 1b. Soil.PAR - 6 parameters

 SetSimulParam_RunoffDepth(0.30); //considered depth (m) of soil profile for calculation of mean soil water content
 SetSimulParam_CNcorrection(true);
 SetSimulParam_SaltDiff(20); // salt diffusion factor (%)
 SetSimulParam_SaltSolub(100); // salt solubility (g/liter)
 SetSimulParam_RootNrDF(16); // shape factor capillary rise factor
 SetSimulParam_IniAbstract(5); // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions

 // 1c. Rainfall.PAR - 4 parameters
 SetSimulParam_EffectiveRain_Method(USDA);
 SetSimulParam_EffectiveRain_PercentEffRain(70); // IF Method is Percentage
 SetSimulParam_EffectiveRain_ShowersInDecade(2);  // For estimation of surface run-off
 SetSimulParam_EffectiveRain_RootNrEvap(5); // For reduction of soil evaporation

 // 1d. Crop.PAR  - 12 parameters
 SetSimulParam_EvapDeclineFactor(4); // evaporation decline factor in stage 2
 SetSimulParam_KcWetBare(1.10); //Kc wet bare soil [-]
 SetSimulParam_PercCCxHIfinal(5); // CC threshold below which HI no longer increase(% of 100)
 SetSimulParam_RootPercentZmin(70); //Starting depth of root sine function (% of Zmin)
 SetSimulParam_MaxRootZoneExpansion(5.00); // fixed at 5 cm/day
 SetSimulParam_KsShapeFactorRoot(-6); // Shape factor for effect water stress on rootzone expansion
 SetSimulParam_TAWGermination(20);  // Soil water content (% TAW) required at sowing depth for germination
 SetSimulParam_pAdjFAO(1); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
 SetSimulParam_DelayLowOxygen(3); //number of days for full effect of deficient aeration
 SetSimulParam_ExpFsen(1.00); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
 SetSimulParam_Beta(12); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
 SetSimulParam_ThicknessTopSWC(10); // Thickness top soil (cm) in which soil water depletion has to be determined

 // 1e. Field.PAR - 1 parameter
 SetSimulParam_EvapZmax(30); //maximum water extraction depth by soil evaporation [cm]

 // 1f. Temperature.PAR - 3 parameters
 SetSimulParam_Tmin(12.0);   //Default minimum temperature (degC) if no temperature file is specified
 SetSimulParam_Tmax(28.0);   //Default maximum temperature (degC) if no temperature file is specified
 SetSimulParam_GDDMethod(3); //Default method for GDD calculations



 SetPreDay(false);
 SetIniPercTAW(50); // Default Value for Percentage TAW for Display in Initial Soil Water Content Menu

 // Default for soil compartments
 IF (GetNrCompartments() > max_No_compartments) //Savety check of value in General.PAR;
    THEN SetNrCompartments(max_No_compartments);
 FOR Nri := 1 TO max_No_compartments DO  // required for formactivate ParamNew
     SetCompartment_Thickness(Nri, GetSimulParam_CompDefThick());
 // Default CropDay1 - Savety check of value in General.PAR
 WHILE (GetSimulParam_CropDay1() > 365) DO SetSimulParam_CropDay1(GetSimulParam_CropDay1()-365);
 If (GetSimulParam_CropDay1() < 1) THEN SetSimulParam_CropDay1(1);

 // 2a. Ground water table
 SetGroundWaterFile('(None)');
 SetGroundWaterFilefull(GetGroundWaterFile());  (* no file *)
 SetGroundWaterDescription('no shallow groundwater table');
 SetZiAqua(undef_int);
 SetECiAqua(undef_int);
 SetSimulParam_ConstGwt(true);

 // 2b. Soil profile and initial soil water content
 ResetDefaultSoil; // Reset the soil profile to its default values
 SetProfFile('DEFAULT.SOL');
 SetProfFilefull(CONCAT(getPathNameSimul(),GetProfFile()));
 // required for SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetCrop().RootMin,GetSoil().NrSoilLayers,SoilLayer)) in LoadProfile
 SetCrop_RootMin(0.30); //Minimum rooting depth (m)
 SetCrop_RootMax(1.00); //Maximum rooting depth (m)
 // Crop.RootMin, RootMax, and Soil.RootMax are correctly calculated in LoadCrop
 LoadProfile(GetProfFilefull());
 CompleteProfileDescription; // Simulation.ResetIniSWC AND specify_soil_layer whcih contains PROCEDURE DeclareInitialCondAtFCandNoSalt,
                             // in which SWCiniFile := '(None)', and settings for Soil water and Salinity content

 // 2c. Complete initial conditions (crop development)
 SetSimulation_CCini(undef_int);
 SetSimulation_Bini(0.000);
 SetSimulation_Zrini(undef_int);


 // 3. Crop characteristics and cropping period
 ResetDefaultCrop; // Reset the crop to its default values
 SetCropFile('DEFAULT.CRO');
 SetCropFilefull(CONCAT(GetPathNameSimul(),GetCropFile()));
 //LoadCrop ==============================
 SetCrop_CCo((GetCrop().PlantingDens/10000) * (GetCrop().SizeSeedling/10000));
 SetCrop_CCini((GetCrop().PlantingDens/10000) * (GetCrop().SizePlant/10000));
 // maximum rooting depth in given soil profile
 SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetSoil().NrSoilLayers,GetSoilLayer()));
 // determine miscellaneous
 SetCrop_Day1(GetSimulParam_CropDay1());
 CompleteCropDescription;
 SetSimulation_YearSeason(1);
 NoCropCalendar;

 // 4. Field Management
 SetManFile('(None)');
 SetManFilefull(GetManFile());  (* no file *)
 NoManagement;

 // 5. Climate
 // 5.1 Temperature
 SetTemperatureFile('(None)');
 SetTemperatureFilefull(GetTemperatureFile());  (* no file *)
 Str(GetSimulParam_Tmin():8:1,TempString1);
 Str(GetSimulParam_Tmax():8:1,TempString2);
 SetTemperatureDescription('');
 SetTemperatureRecord_DataType(Daily);
 SetTemperatureRecord_NrObs(0);
 SetTemperatureRecord_FromString('any date');
 SetTemperatureRecord_ToString('any date');
 SetTemperatureRecord_FromY(1901);

 // 5.2 ETo
 SetEToFile('(None)');
 SetEToFilefull(GetEToFile());  (* no file *)
 SetEToDescription('');
 SetEToRecord_DataType(Daily);
 SetEToRecord_NrObs(0);
 SetEToRecord_FromString('any date');
 SetEToRecord_ToString('any date');
 SetEToRecord_FromY(1901);

 // 5.3 Rain
 SetRainFile('(None)');
 SetRainFilefull(GetRainFile());  (* no file *)
 SetRainDescription('');
 SetRainRecord_DataType(Daily);
 SetRainRecord_NrObs(0);
 SetRainRecord_FromString('any date');
 SetRainRecord_ToString('any date');
 SetRainRecord_FromY(1901);

 // 5.4 CO2
 SetCO2File('MaunaLoa.CO2');
 setCO2FileFull(CONCAT(GetPathNameSimul(),GetCO2File()));
 CO2descr := GetCO2Description();
 GenerateCO2Description(GetCO2FileFull(),CO2descr);
 SetCO2Description(CO2descr);


 // 5.5 Climate file
 SetClimateFile('(None)');
 SetClimateFileFull(GetClimateFile());
 SetClimateDescription('');

 // 5.6 Set Climate and Simulation Period
 SetClimData;
 SetSimulation_LinkCropToSimPeriod(true);
(* adjusting Crop.Day1 and Crop.DayN to ClimFile *)
Crop_Day1_temp := GetCrop().Day1;
Crop_DayN_temp := GetCrop().DayN;
 AdjustCropYearToClimFile(Crop_Day1_temp,Crop_DayN_temp);
SetCrop_Day1(Crop_Day1_temp);
SetCrop_DayN(Crop_DayN_temp);
(* adjusting ClimRecord.'TO' for undefined year with 365 days *)
 IF ((GetClimFile() <> '(None)') AND (GetClimRecord_FromY() = 1901)
   AND (GetClimRecord_NrObs() = 365)) THEN AdjustClimRecordTo(GetCrop().DayN);
(* adjusting simulation period *)
 AdjustSimPeriod;

 // 6. irrigation
 SetIrriFile('(None)');
 SetIrriFilefull(GetIrriFile());  (* no file *)
 NoIrrigation;

 // 7. Off-season
 SetOffSeasonFile('(None)');
 SetOffSeasonFileFull(getOffSeasonFile());
 NoManagementOffSeason;

 // 8. Project and Multiple Project file
 SetProjectFile('(None)');
 SetProjectFileFull(GetProjectFile());
 ProjectDescription := 'No specific project';
 SetSimulation_MultipleRun(false); // No sequence of simulation runs in the project
 SetSimulation_NrRuns(1);
 SetSimulation_MultipleRunWithKeepSWC(false);
 SetSimulation_MultipleRunConstZrx(undef_int);
 SetMultipleProjectFile(GetProjectFile());
 SetMultipleProjectFileFull(GetProjectFileFull());
 MultipleProjectDescription := ProjectDescription;


 // 9. Observations file
 SetObservationsFile('(None)');
 SetObservationsFileFull(GetObservationsFile());
 SetObservationsDescription('No field observations');

 // 10. Output files
 SetOutputName('Project');

 // 11. Onset
 SetOnset_Criterion(RainPeriod);
 SetOnset_AirTCriterion(CumulGDD);
 AdjustOnsetSearchPeriod;

 // 12. Simulation run
 SetETo(5.0);
 SetRain(0);
 SetIrrigation(0);
 SurfaceStorage := 0;
 ECstorage := 0.0;
 SetDaySubmerged(0);
 SumWaBal_temp := GetSumWabal();
 GlobalZero(SumWabal_temp);
 SetSumWaBal(SumWaBal_temp);
 SetDrain(0.0); // added 4.0
 SetRunoff(0.0);// added 4.0
 SetInfiltrated(0.0); // added 4.0
 SetCRwater(0); // added 4.0
 SetCRsalt(0); // added 4.0
 SetSimulation_ResetIniSWC(true);
 SetSimulation_EvapLimitON(false);
 SetMaxPlotNew(50);
 SetMaxPlotTr(10);
 SetSimulation_InitialStep(10); // Length of period (days) for displaying intermediate results during simulation run
 SetSimulation_LengthCuttingInterval(40); // Default length of cutting interval (days)
 END; (* InitializeSettings *)


end.
