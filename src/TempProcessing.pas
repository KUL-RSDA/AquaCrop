unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;


PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);

PROCEDURE StressBiomassRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    CropSResp : rep_Shapes;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    VAR b0,b1,b2 : double;
                                    VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);

PROCEDURE CCxSaltStressRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    TheCCsaltDistortion : ShortInt;
                                    VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
                                    VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);

FUNCTION BiomassRatio(TempDaysToCCini,TempGDDaysToCCini : INTEGER;
                      TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
                      TempKc,TempKcDecline,TempCCeffect,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
                      TempModeCycle : rep_modeCycle;
                      SFInfo : rep_EffectStress;
                      SFInfoStress,WeedStress : ShortInt;
                      DeltaWeedStress : INTEGER;
                      DeterminantCropType,FertilityStressOn : BOOLEAN) : double;


implementation



PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);
VAR f0,fClim : TextFile;
    TempString,TempString1,TempString2,observations_descr,eto_descr,CO2descr,rain_descr,
    CalendarDescriptionLocal, TemperatureDescriptionLocal : string;
    TempSimDayNr1,TempSimDayNrN : LongInt;
    i,Runi : ShortInt;
    TotDepth : double;
    VersionNr : double;
    FertStress : shortint;
    temperature_record : rep_clim;
    rain_record : rep_clim;
    ETo_record : rep_clim;
    YearSeason_temp, RedCGC_temp, RedCCX_temp : ShortInt;
    Compartment_temp : rep_Comp;
    TempInt : integer;
    Crop_Planting_temp : rep_Planting;
    Crop_RootMin_temp, Crop_SizePlant_temp, Crop_CCini_temp : double;
    Crop_DaysToCCini_temp, Crop_GDDaysToCCini_temp : integer;
    Crop_DaysToSenescence_temp, Crop_DaysToHarvest_temp : integer;
    Crop_GDDaysToSenescence_temp, Crop_GDDaysToHarvest_temp : integer;
    Crop_Day1_temp : INTEGER;
    Crop_DayN_temp : INTEGER;
    Crop_DaysToFullCanopySF_temp : INTEGER;
    ZiAqua_temp : integer;
    ECiAqua_temp : double;

    PROCEDURE GetFileDescription(TheFileFullName : string;
                                 VAR TheDescription : string);
    VAR f0 : textfile;
    BEGIN
    Assign(f0,TheFileFullName);
    Reset(f0);
    Readln(f0,TheDescription);
    Close(f0);
    END; (* GetFileDescription *)

BEGIN
TempString := StringReplace(NameFileFull, '"', '', [rfReplaceAll]);
Assign(f0,TempString);
Reset(f0);
READLN(f0); //Description
READLN(f0,VersionNr);  // AquaCrop version Nr
IF (NrRun > 1) THEN
   BEGIN
   // Files previous runs
   FOR Runi := 1 TO (NrRun - 1) DO
       FOR i := 1 TO 47  DO Readln(f0); // 5 + 42 lines with files
   END;
// Year of cultivation and Simulation and Cropping period
READLN(f0,YearSeason_temp); // year number of cultivation (1 = seeding/planting year)
SetSimulation_YearSeason(YearSeason_temp);
READLN(f0,TempSimDayNr1); //First day of simulation period
READLN(f0,TempSimDayNrN); //Last day of simulation period
READLN(f0,TempInt); //First day of cropping period
SetCrop_Day1(TempInt);
READLN(f0,TempInt); //Last day of cropping period
SetCrop_DayN(TempInt);


// 1. Climate
READLN(f0); // Info Climate
READLN(f0,TempString);  //ClimateFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetClimateFile(Trim(TempString));
IF (GetClimateFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathClimateFile
        SetClimateFileFull(GetClimateFile());
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathClimateFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetClimateFileFull(CONCAT(Trim(TempString),GetClimateFile()));
        Assign(fClim,GetClimateFileFull());
        Reset(fClim);
        // 1.0 Description
        READLN(fClim,TempString);
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetClimateDescription(Trim(TempString));
        Close(fClim);
        END;
// 1.1 Temperature
READLN(f0); // Info Temperature
READLN(f0,TempString);  //TemperatureFile
SetTemperatureFile(Trim(TempString));
IF (GetTemperatureFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathTemperatureFile
        SetTemperatureFilefull(GetTemperatureFile());  (* no file *)
        Str(GetSimulParam_Tmin():8:1,TempString1);
        Str(GetSimulParam_Tmax():8:1,TempString2);
        SetTemperatureDescription(CONCAT('Default temperature data: Tmin = ',
                    trim(TempString1),' and Tmax = ',trim(TempString2),' °C'));
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathTemperatureFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetTemperatureFileFull(CONCAT(Trim(TempString),Trim(GetTemperatureFile())));
        temperature_record := GetTemperatureRecord();
        TemperatureDescriptionLocal := GetTemperatureDescription();
        LoadClim(GetTemperatureFileFull(),TemperatureDescriptionLocal,temperature_record);
        SetTemperatureDescription(TemperatureDescriptionLocal);
        CompleteClimateDescription(temperature_record);
        SetTemperatureRecord(temperature_record);
        END;
// 1.2 ETo
READLN(f0); // Info ETo
READLN(f0,TempString);  //EToFile
SetEToFile(Trim(TempString));
IF (GetEToFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathETo
        SetEToFilefull(GetEToFile());  (* no file *)
        SetEToDescription('Specify ETo data when Running AquaCrop');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathETo
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetEToFilefull(CONCAT(Trim(TempString),GetEToFile()));
        eto_descr := GetEToDescription();
        ETo_record := GetEToRecord();
        LoadClim(GetEToFilefull(),eto_descr,ETo_record);
        SetEToDescription(eto_descr); 
        CompleteClimateDescription(ETo_record);
        SetEToRecord(ETo_record);
        END;
// 1.3 Rain
READLN(f0); // Info Rain
READLN(f0,TempString);  //RainFile
SetRainFile(Trim(TempString));
IF (GetRainFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathRain
        SetRainFilefull(GetRainFile());  (* no file *)
        SetRainDescription('Specify Rain data when Running AquaCrop');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathRain
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetRainFileFull(CONCAT(Trim(TempString),GetRainFile()));
        rain_descr := Getraindescription();
        rain_record := GetRainRecord();
        LoadClim(GetRainFilefull(),rain_descr,rain_record);
        SetRainDescription(rain_descr);
        CompleteClimateDescription(rain_record);
        SetRainRecord(rain_record);
        END;
// 1.4 CO2
READLN(f0); // Info CO2
READLN(f0,TempString);  //CO2File
SetCO2File(Trim(TempString));
IF (GetCO2File() = '(None)')
   THEN READLN(f0)  //PathCO2File
   ELSE BEGIN
        READLN(f0,TempString);  //PathCO2File
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetCO2FileFull(CONCAT(Trim(TempString),GetCO2File()));
        CO2descr :=  GetCO2Description();
        GenerateCO2Description(GetCO2FileFull(),CO2descr);
        SetCO2Description(CO2descr)
        END;
SetClimData;
AdjustOnsetSearchPeriod; // Set initial StartSearch and StopSearchDayNr

// 2. Calendar
READLN(f0); // Info calendar
READLN(f0,TempString);  //CalendarFile
SetCalendarFile(Trim(TempString));
IF (GetCalendarFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathCalendarFile
        SetCalendarDescription('No calendar for the Seeding/Planting year');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathCalendarFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetCalendarFileFull(CONCAT(Trim(TempString),GetCalendarFile()));
        CalendarDescriptionLocal := GetCalendarDescription();
        GetFileDescription(GetCalendarFileFull(),CalendarDescriptionLocal);
        SetCalendarDescription(CalendarDescriptionLocal);
        END;

// 3. Crop
SetSimulation_LinkCropToSimPeriod(true);
READLN(f0); // Info Crop
READLN(f0,TempString);  //CropFile
SetCropFile(Trim(TempString));
READLN(f0,TempString);  //PathCropFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetCropFilefull(CONCAT(Trim(TempString),GetCropFile()));
LoadCrop(GetCropFilefull());

// Adjust crop parameters of Perennials
IF (GetCrop_subkind() = Forage) THEN
   BEGIN
   // adjust crop characteristics to the Year (Seeding/Planting or Non-seesing/Planting year)
   Crop_Planting_temp := GetCrop().Planting;
   Crop_RootMin_temp := GetCrop().RootMin;
   Crop_SizePlant_temp := GetCrop().SizePlant;
   Crop_CCini_temp := GetCrop().CCini;
   Crop_DaysToCCini_temp := GetCrop().DaysToCCini;
   Crop_GDDaysToCCini_temp := GetCrop_GDDaysToCCini();
   AdjustYearPerennials(GetSimulation_YearSeason(),GetCrop().SownYear1,GetCrop_ModeCycle(),GetCrop().RootMax,GetCrop().RootMinYear1,
                     GetCrop().CCo,GetCrop().SizeSeedling,GetCrop().CGC,GetCrop().CCx,GetCrop().GDDCGC,GetCrop().PlantingDens,
                     Crop_Planting_temp,Crop_RootMin_temp,Crop_SizePlant_temp,Crop_CCini_temp,
                     Crop_DaysToCCini_temp,Crop_GDDaysToCCini_temp);
   SetCrop_Planting(Crop_Planting_temp);
   SetCrop_RootMin(Crop_RootMin_temp);
   SetCrop_SizePlant(Crop_SizePlant_temp);
   SetCrop_CCini(Crop_CCini_temp);
   SetCrop_DaysToCCini(Crop_DaysToCCini_temp);
   SetCrop_GDDaysToCCini(Crop_GDDaysToCCini_temp);
   // adjust length of season
   SetCrop_DaysToHarvest(GetCrop().DayN - GetCrop().Day1 + 1);
   Crop_DaysToSenescence_temp := GetCrop().DaysToSenescence;
   Crop_DaysToHarvest_temp := GetCrop().DaysToHarvest;
   Crop_GDDaysToSenescence_temp := GetCrop().GDDaysToSenescence;
   Crop_GDDaysToHarvest_temp := GetCrop().GDDaysToHarvest;
   AdjustCropFileParameters(GetCropFileSet(),(GetCrop().DaysToHarvest),GetCrop().Day1,GetCrop_ModeCycle(),GetCrop().Tbase,GetCrop().Tupper,
                                    Crop_DaysToSenescence_temp,Crop_DaysToHarvest_temp,
                                    Crop_GDDaysToSenescence_temp,Crop_GDDaysToHarvest_temp);
   SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp);
   SetCrop_DaysToHarvest(Crop_DaysToHarvest_temp);
   SetCrop_GDDaysToSenescence(Crop_GDDaysToSenescence_temp);
   SetCrop_GDDaysToHarvest(Crop_GDDaysToHarvest_temp);
   END;

AdjustCalendarCrop(GetCrop().Day1);
CompleteCropDescription;
//Onset.Off := true;
IF (GetClimFile() = '(None)')
   THEN BEGIN
       Crop_Day1_temp := GetCrop().Day1;
       Crop_DayN_temp := GetCrop().DayN;
       AdjustCropYearToClimFile(Crop_Day1_temp,Crop_DayN_temp); // adjusting Crop.Day1 and Crop.DayN to ClimFile
       SetCrop_Day1(Crop_Day1_temp);
       SetCrop_DayN(Crop_DayN_temp);
       END
   ELSE SetCrop_DayN(GetCrop().Day1 + GetCrop().DaysToHarvest - 1);

(* adjusting ClimRecord.'TO' for undefined year with 365 days *)
IF ((GetClimFile() <> '(None)') AND (GetClimRecord_FromY() = 1901)
   AND (GetClimRecord_NrObs() = 365)) THEN AdjustClimRecordTo(GetCrop().DayN);
(* adjusting simulation period *)
AdjustSimPeriod;

// 4. Irrigation
READLN(f0); // Info Irrigation
READLN(f0,TempString);  //IrriFile
SetIrriFile(Trim(TempString));
IF (GetIrriFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathIrriFile
        SetIrriFileFull(GetIrriFile());
        NoIrrigation;
        //IrriDescription := 'Rainfed cropping';
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathIrriFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetIrriFileFull(CONCAT(Trim(TempString),GetIrriFile()));
        LoadIrriScheduleInfo(GetIrriFileFull());
        END;

// 5. Field Management
READLN(f0); // Info Field Management
READLN(f0,TempString);  //ManFile
SetManFile(Trim(TempString));
IF (GetManFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathManFile
        SetManFileFull(GetManFile());
        SetManDescription('No specific field management');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathManFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetManFileFull(CONCAT(Trim(TempString),GetManFile()));
        LoadManagement(GetManFilefull());
        // reset canopy development to soil fertility
        FertStress := GetManagement_FertilityStress();
        Crop_DaysToFullCanopySF_temp := GetCrop().DaysToFullCanopySF;
        RedCGC_temp := GetSimulation_EffectStress_RedCGC();
        RedCCX_temp := GetSimulation_EffectStress_RedCCX();
        TimeToMaxCanopySF(GetCrop().CCo,GetCrop().CGC,GetCrop().CCx,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,
                          GetCrop().DaysToFlowering,GetCrop().LengthFlowering,GetCrop().DeterminancyLinked,
                          Crop_DaysToFullCanopySF_temp,RedCGC_temp,
                          RedCCX_temp,FertStress);
        SetCrop_DaysToFullCanopySF(Crop_DaysToFullCanopySF_temp);
        SetManagement_FertilityStress(FertStress);
        SetSimulation_EffectStress_RedCGC(RedCGC_temp);
        SetSimulation_EffectStress_RedCCX(RedCCX_temp);
        END;

// 6. Soil Profile
READLN(f0); // Info Soil
READLN(f0,TempString);  //ProfFile
SetProfFile(Trim(TempString));
READLN(f0,TempString);  //PathProfFile
TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
SetProfFilefull(CONCAT(Trim(TempString),GetProfFile()));
// The load of profile is delayed to check if soil water profile need to be reset (see 8.)

// 7. Groundwater
READLN(f0); // Info Groundwater
READLN(f0,TempString);  //GroundWaterFile
SetGroundWaterFile(Trim(TempString));
IF (GetGroundWaterFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathGroundWaterFile
        SetGroundWaterFilefull(GetGroundWaterFile());
        SetGroundWaterDescription('no shallow groundwater table');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathGroundWaterFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetGroundWaterFilefull(CONCAT(Trim(TempString),GetGroundWaterFile()));
        // Loading the groundwater is done after loading the soil profile (see 9.)
        END;


// 8. Set simulation period
SetSimulation_FromDayNr(TempSimDayNr1);
SetSimulation_ToDayNr(TempSimDayNrN);
IF ((GetCrop().Day1 <> GetSimulation_FromDayNr()) OR (GetCrop().DayN <> GetSimulation_ToDayNr()))
   THEN SetSimulation_LinkCropToSimPeriod(false);

// 9. Initial conditions
READLN(f0); // Info Initial conditions
READLN(f0,TempString);  //SWCIniFile
IF (Trim(TempString) = 'KeepSWC')
   THEN BEGIN
        // No load of soil file (which reset thickness compartments and Soil water content to FC)
        SetSWCIniFile('KeepSWC');
        SetSWCIniDescription('Keep soil water profile of previous run');
        READLN(f0);  //PathSWCIniFile
        END
   ELSE BEGIN
        // start with load and complete profile description (see 5.) which reset SWC to FC by default
        LoadProfile(GetProfFilefull());
        CompleteProfileDescription;

        //Adjust size of compartments if required
        TotDepth := 0;
        FOR i := 1 to GetNrCompartments() DO TotDepth := TotDepth + GetCompartment_Thickness(i);
        IF GetSimulation_MultipleRunWithKeepSWC() // Project with a sequence of simulation runs and KeepSWC
           THEN BEGIN
                IF (ROUND(GetSimulation_MultipleRunConstZrx()*1000) > ROUND(TotDepth*1000))
                   THEN AdjustSizeCompartments(GetSimulation_MultipleRunConstZrx());
                END
           ELSE BEGIN
                IF (ROUND(GetCrop().RootMax*1000) > ROUND(TotDepth*1000)) THEN
                   BEGIN
                   IF (ROUND(GetSoil().RootMax*1000) = ROUND(GetCrop().RootMax*1000))
                      THEN AdjustSizeCompartments(GetCrop().RootMax) // no restrictive soil layer
                      ELSE BEGIN // restrictive soil layer
                           IF (ROUND(GetSoil().RootMax*1000) > ROUND(TotDepth*1000))
                              THEN AdjustSizeCompartments(GetSoil().RootMax)
                           END;
                   END;
                END;

        SetSWCIniFile(Trim(TempString));
        IF (GetSWCIniFile() = '(None)')
           THEN BEGIN
                READLN(f0);  //PathSWCIniFile
                SetSWCiniFileFull(GetSWCiniFile()); (* no file *)
                SetSWCiniDescription('Soil water profile at Field Capacity');
                END
           ELSE BEGIN
                READLN(f0,TempString);  //PathSWCIniFile
                TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
                SetSWCiniFileFull(CONCAT(Trim(TempString),GetSWCIniFile()));
                LoadInitialConditions(GetSWCiniFileFull(),SurfaceStorage);
                END;
        Compartment_temp := GetCompartment();
        CASE GetSimulation_IniSWC_AtDepths() OF
             true : TranslateIniPointsToSWProfile(GetSimulation_IniSWC_NrLoc(),GetSimulation_IniSWC_Loc(),GetSimulation_IniSWC_VolProc(),
                                                  GetSimulation_IniSWC_SaltECe(),GetNrCompartments(),Compartment_temp);
             else TranslateIniLayersToSWProfile(GetSimulation_IniSWC_NrLoc(),GetSimulation_IniSWC_Loc(),GetSimulation_IniSWC_VolProc(),
                                                GetSimulation_IniSWC_SaltECe(),GetNrCompartments(),Compartment_temp);
             end;
        SetCompartment(Compartment_temp);


        IF GetSimulation_ResetIniSWC() THEN // to reset SWC and SALT at end of simulation run
           BEGIN
           FOR i := 1 TO GetNrCompartments() DO
               BEGIN
               SetSimulation_ThetaIni_i(i,GetCompartment_Theta(i));
               SetSimulation_ECeIni_i(i,ECeComp(GetCompartment_i(i)));
               END;
           // ADDED WHEN DESINGNING 4.0 BECAUSE BELIEVED TO HAVE FORGOTTEN - CHECK LATER
           IF (GetManagement_BundHeight() >= 0.01) THEN
              BEGIN
              SetSimulation_SurfaceStorageIni(SurfaceStorage);
              SetSimulation_ECStorageIni(ECStorage);
              END;
           END;
        END;

// 10. load the groundwater file if it exists (only possible for Version 4.0 and higher)
IF ((ROUND(10*VersionNr) >= 40) AND (GetGroundWaterFile() <> '(None)')) // the groundwater file is only available in Version 4.0 or higher
   THEN BEGIN
        ZiAqua_temp := GetZiAqua();
        ECiAqua_temp := GetECiAqua();
        LoadGroundWater(GetGroundWaterFilefull(),GetSimulation_FromDayNr(),ZiAqua_temp,ECiAqua_temp);
        SetZiAqua(ZiAqua_temp);
        SetECiAqua(ECiAqua_temp);
        END
   ELSE BEGIN
        SetZiAqua(undef_int);
        SetECiAqua(undef_int);
        SetSimulParam_ConstGwt(true);
        END;
Compartment_temp := GetCompartment();
CalculateAdjustedFC((GetZiAqua()/100),Compartment_temp);
SetCompartment(Compartment_temp);
//IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
IF (GetSimulation_IniSWC_AtFC() AND (GetSWCIniFile() <> 'KeepSWC')) THEN ResetSWCToFC;

// 11. Off-season conditions
READLN(f0); // Info Off-season conditions
READLN(f0,TempString);  //OffSeasonFile
SetOffSeasonFile(Trim(TempString));
IF (GetOffSeasonFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //PathOffSeasonFile
        SetOffSeasonFileFull(GetOffSeasonFile());
        SetOffSeasonDescription('No specific off-season conditions');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //PathOffSeasonFile
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetOffSeasonFileFull(CONCAT(Trim(TempString),GetOffSeasonFile()));
        LoadOffSeason(GetOffSeasonFilefull());
        END;

// 12. Field data
READLN(f0); // Info Field data
READLN(f0,TempString);  //Field dataFile
SetObservationsFile(Trim(TempString));
IF (GetObservationsFile() = '(None)')
   THEN BEGIN
        READLN(f0);  //Path Field data File
        SetObservationsFileFull(GetObservationsFile());
        SetObservationsDescription('No field observations');
        END
   ELSE BEGIN
        READLN(f0,TempString);  //Path Field data File
        TempString := StringReplace(TempString, '"', '', [rfReplaceAll]);
        SetObservationsFileFull(CONCAT(Trim(TempString),GetObservationsFile()));
        observations_descr := GetObservationsDescription();
        GetFileDescription(GetObservationsFileFull(),observations_descr);
        SetObservationsDescription(observations_descr);
        END;

Close(f0);
END; (* LoadSimulationRunProject *)


PROCEDURE StressBiomassRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    CropSResp : rep_Shapes;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    VAR b0,b1,b2 : double;
                                    VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);

CONST EToStandard = 5;
      k = 2;


TYPE StressIndexes = Record
       StressProc : ShortInt;
       BioMProc,
       BioMSquare : double;
       END;

VAR StressMatrix : ARRAY[0..7] of StressIndexes;
    Si : ShortInt;
    L12SF,GDDL12SF : INTEGER;
    StressResponse : rep_EffectStress;
    RatDGDD,BNor,BNor100,Yavg,X1avg,X2avg,y,x1,x2,x1y,x2y,x1Sq,
    x2Sq,x1x2,SUMx1y,SUMx2y,SUMx1Sq,SUMx2Sq,SUMx1x2 : double;
    SiPr : ShortInt;
    SumKcTop,HIGC,HIGClinear : double;
    DaysYieldFormation,tSwitch : INTEGER;

BEGIN
// 1. initialize
SetSimulation_DelayedDays(0); // required for CalculateETpot
L12SF := L12; // to calculate SumKcTop (no stress)
GDDL12SF := GDDL12; // to calculate SumKcTop (no stress)
// Maximum sum Kc (no stress)
SumKcTop := SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                               L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234,
                               CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                               Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,CO2Given,
                               TheModeCycle);

//Get PercentLagPhase (for estimate WPi during yield formation)
IF ((TheCropType = Tuber) OR (TheCropType = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(RefHI/RatedHIdt);
   IF CropDeterm
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(RefHI,RatedHIdt);
           GetDaySwitchToLinear(RefHI,RatedHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

// 2. Biomass production for various stress levels
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    // stress effect
    SiPr := 10*Si;
    StressMatrix[Si].StressProc := SiPr;
    CropStressParametersSoilFertility(CropSResp,SiPr,StressResponse);
    // adjusted length of Max canopy cover
    RatDGDD := 1;
    IF ((StressResponse.RedCCX = 0) AND (StressResponse.RedCGC = 0))
       THEN BEGIN
            L12SF := L12;
            GDDL12SF := GDDL12;
            END
       ELSE BEGIN
            TimeToMaxCanopySF(CCo,CGC,CCx,L0,L12,L123,LFlor,LengthFlor,CropDeterm,
                              L12SF,StressResponse.RedCGC,StressResponse.RedCCX,SiPr);
            IF (TheModeCycle = GDDays) THEN GDDL12SF := GrowingDegreeDays(L12SF,CropDNr1,Tbase,Tupper,TDayMin,TDayMax);
            IF ((TheModeCycle = GDDays) AND (GDDL12SF < GDDL123))
               THEN RatDGDD := (L123-L12SF)/(GDDL123-GDDL12SF);
            END;
    // biomass production
    BNor := Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                        L0,L12,L12SF,L123,L1234,LFlor,
                        GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                        CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                        KcTop,KcDeclAgeing,CCeffectProcent,WPveg,CO2Given,
                        Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,
                        SiPr,StressResponse.RedCGC,StressResponse.RedCCX,StressResponse.RedWP,StressResponse.RedKsSto,(0),(0),
                        StressResponse.CDecline,(-0.01),TheModeCycle,(true),(false));
    IF (Si = 0)
       THEN BEGIN
            BNor100 := BNor;
            StressMatrix[0].BioMProc := 100;
            END
       ELSE BEGIN
            IF (BNor100 > 0.00001)
               THEN StressMatrix[Si].BioMProc := 100 * BNor/BNor100
               ELSE StressMatrix[Si].BioMProc := 100;
            END;
    StressMatrix[Si].BioMSquare := StressMatrix[Si].BioMProc * StressMatrix[Si].BioMProc;
    END; // end stress level

// 5. Stress - Biomass relationship
Yavg := 0;
X1avg := 0;
X2avg := 0;
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    Yavg := Yavg + StressMatrix[Si].StressProc;
    X1avg := X1avg + StressMatrix[Si].BioMProc;
    X2avg := X2avg + StressMatrix[Si].BioMSquare;
    END;
Yavg  := Yavg/8;
X1avg := X1avg/8;
X2avg := X2avg/8;
SUMx1y  := 0;
SUMx2y  := 0;
SUMx1Sq := 0;
SUMx2Sq := 0;
SUMx1x2 := 0;
FOR Si := 0 TO 7 DO  // various stress levels
    BEGIN
    y     := StressMatrix[Si].StressProc - Yavg;
    x1    := StressMatrix[Si].BioMProc - X1avg;
    x2    := StressMatrix[Si].BioMSquare - X2avg;
    x1y   := x1 * y;
    x2y   := x2 * y;
    x1Sq  := x1 * x1;
    x2Sq  := x2 * x2;
    x1x2  := x1 * x2;
    SUMx1y  := SUMx1y + x1y;
    SUMx2y  := SUMx2y + x2y;
    SUMx1Sq := SUMx1Sq + x1Sq;
    SUMx2Sq := SUMx2Sq + x2Sq;
    SUMx1x2 := SUMx1x2 + x1x2;
    END;

IF (abs(ROUND(SUMx1x2*1000)) <> 0)
   THEN BEGIN
        b2 := (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/(SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2);
        b1 := (SUMx1y - b2 * SUMx1x2)/SUMx1Sq;
        b0 := Yavg - b1*X1avg - b2*X2avg;

        BM10 :=  StressMatrix[1].BioMProc;
        BM20 :=  StressMatrix[2].BioMProc;
        BM30 :=  StressMatrix[3].BioMProc;
        BM40 :=  StressMatrix[4].BioMProc;
        BM50 :=  StressMatrix[5].BioMProc;
        BM60 :=  StressMatrix[6].BioMProc;
        BM70 :=  StressMatrix[7].BioMProc;
        END
   ELSE BEGIN
        b2 := undef_int;
        b1 := undef_int;
        b0 := undef_int;
        END;
END;  (* StressBiomassRelationship *)



PROCEDURE CCxSaltStressRelationship(TheDaysToCCini,TheGDDaysToCCini : INTEGER;
                                    L0,L12,L123,L1234,
                                    LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                                    GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
                                    CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                                    KcTop,KcDeclAgeing,CCeffectProcent,
                                    Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
                                    CropDNr1 : LongInt;
                                    CropDeterm : BOOLEAN;
                                    TheCropType : rep_subkind;
                                    TheModeCycle : rep_modeCycle;
                                    TheCCsaltDistortion : ShortInt;
                                    VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
                                    VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);

TYPE StressIndexes = Record
       CCxReduction : ShortInt;
       SaltProc,
       SaltSquare : double;
       END;

VAR L12SS,GDDL12SS,DaysYieldFormation,tSwitch : INTEGER;
    SumKcTop,HIGC,HIGClinear,CCToReach : double;
    Si,SiPr : ShortInt;
    StressMatrix : ARRAY[0..9] of StressIndexes;
    StressResponse : rep_EffectStress;
    RatDGDD,BNor,BNor100,BioMProc : double;
    Yavg,X1avg,X2avg,SUMx1y,SUMx2y,SUMx1Sq,SUMx2Sq,SUMx1x2,y,x1,x2,x1y,x2y,x1Sq,x2Sq,x1x2  : double;

BEGIN
// 1. initialize
SetSimulation_DelayedDays(0); // required for CalculateETpot
GDDL12SS := GDDL12; // to calculate SumKcTop (no stress)
BNor100 := undef_int;
// Maximum sum Kc (no stress)
SumKcTop := SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                               L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234,
                               CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                               Tbase,Tupper,TDayMin,TDayMax,GDbioLow,CO2Given,
                               TheModeCycle);
//Get PercentLagPhase (for estimate WPi during yield formation)
IF ((TheCropType = Tuber) OR (TheCropType = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(RefHI/RatedHIdt);
   IF CropDeterm
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(RefHI,RatedHIdt);
           GetDaySwitchToLinear(RefHI,RatedHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

// 2. Biomass production (or Salt stress) for various CCx reductions
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    // CCx reduction
    SiPr := 10*Si;
    StressMatrix[Si].CCxReduction := SiPr;
    // adjustment CC
    CropStressParametersSoilSalinity(SiPr,TheCCsaltDistortion,CCo,CCx,CGC,GDDCGC,CropDeterm,L12,LFlor,LengthFlor,L123,
                                     GDDL12,GDDFlor,GDDLengthFlor,GDDL123,TheModeCycle,StressResponse);
    // adjusted length of Max canopy cover
    RatDGDD := 1;
    IF ((StressResponse.RedCCX = 0) AND (StressResponse.RedCGC = 0))
       THEN BEGIN
            L12SS := L12;
            GDDL12SS := GDDL12;
            END
       ELSE BEGIN
            CCToReach := 0.98*(1-StressResponse.RedCCX/100)*CCx;
            L12SS := DaysToReachCCwithGivenCGC(CCToReach,CCo,((1-StressResponse.RedCCX/100)*CCx),
                     (CGC*(1-(StressResponse.RedCGC)/100)),L0);
            IF (TheModeCycle = GDDays) THEN GDDL12SS := GrowingDegreeDays(L12SS,CropDNr1,Tbase,Tupper,TDayMin,TDayMax);
            IF ((TheModeCycle = GDDays) AND (GDDL12SS < GDDL123))
               THEN RatDGDD := (L123-L12SS)/(GDDL123-GDDL12SS);
            END;

    // biomass production
    BNor := Bnormalized(TheDaysToCCini,TheGDDaysToCCini,
                        L0,L12,L12SS,L123,L1234,LFlor,
                        GDDL0,GDDL12,GDDL12SS,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                        CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                        KcTop,KcDeclAgeing,CCeffectProcent,WPveg,CO2Given,
                        Tbase,Tupper,TDayMin,TDayMax,GDbioLow,RatDGDD,SumKcTop,
                        SiPr,StressResponse.RedCGC,StressResponse.RedCCX,StressResponse.RedWP,StressResponse.RedKsSto,(0),(0),
                        StressResponse.CDecline,(-0.01),TheModeCycle,(false),(false));
    IF (Si = 0)
       THEN BEGIN
            BNor100 := BNor;
            BioMProc := 100;
            StressMatrix[0].SaltProc := 0;
            END
       ELSE BEGIN
            IF (BNor100 > 0.00001)
               THEN BEGIN
                    BioMProc := 100 * BNor/BNor100;
                    StressMatrix[Si].SaltProc := 100 - BioMProc;
                    END
               ELSE StressMatrix[Si].SaltProc := 0;
            END;
    StressMatrix[Si].SaltSquare := StressMatrix[Si].SaltProc * StressMatrix[Si].SaltProc;
    END; // end stress level

// 3. CCx - Salt stress relationship
Yavg := 0;
X1avg := 0;
X2avg := 0;
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    Yavg := Yavg + StressMatrix[Si].CCxReduction;
    X1avg := X1avg + StressMatrix[Si].SaltProc;
    X2avg := X2avg + StressMatrix[Si].SaltSquare;
    END;
Yavg  := Yavg/10;
X1avg := X1avg/10;
X2avg := X2avg/10;
SUMx1y  := 0;
SUMx2y  := 0;
SUMx1Sq := 0;
SUMx2Sq := 0;
SUMx1x2 := 0;
FOR Si := 0 TO 9 DO  // various CCx reduction
    BEGIN
    y     := StressMatrix[Si].CCxReduction - Yavg;
    x1    := StressMatrix[Si].SaltProc - X1avg;
    x2    := StressMatrix[Si].SaltSquare - X2avg;
    x1y   := x1 * y;
    x2y   := x2 * y;
    x1Sq  := x1 * x1;
    x2Sq  := x2 * x2;
    x1x2  := x1 * x2;
    SUMx1y  := SUMx1y + x1y;
    SUMx2y  := SUMx2y + x2y;
    SUMx1Sq := SUMx1Sq + x1Sq;
    SUMx2Sq := SUMx2Sq + x2Sq;
    SUMx1x2 := SUMx1x2 + x1x2;
    END;

IF (abs(ROUND(SUMx1x2*1000)) <> 0)
   THEN BEGIN
        Coeffb2Salt := (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/(SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2);
        Coeffb1Salt := (SUMx1y - Coeffb2Salt * SUMx1x2)/SUMx1Sq;
        Coeffb0Salt := Yavg - Coeffb1Salt*X1avg - Coeffb2Salt*X2avg;

        Salt10 :=  StressMatrix[1].SaltProc;
        Salt20 :=  StressMatrix[2].SaltProc;
        Salt30 :=  StressMatrix[3].SaltProc;
        Salt40 :=  StressMatrix[4].SaltProc;
        Salt50 :=  StressMatrix[5].SaltProc;
        Salt60 :=  StressMatrix[6].SaltProc;
        Salt70 :=  StressMatrix[7].SaltProc;
        Salt80 :=  StressMatrix[8].SaltProc;
        Salt90 :=  StressMatrix[9].SaltProc;
        END
   ELSE BEGIN
        Coeffb2Salt := undef_int;
        Coeffb1Salt := undef_int;
        Coeffb0Salt := undef_int;
        END;
END; (* CCxSaltStressRelationship *)


FUNCTION BiomassRatio(TempDaysToCCini,TempGDDaysToCCini : INTEGER;
                      TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
                      TempKc,TempKcDecline,TempCCeffect,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
                      TempModeCycle : rep_modeCycle;
                      SFInfo : rep_EffectStress;
                      SFInfoStress,WeedStress : ShortInt;
                      DeltaWeedStress : INTEGER;
                      DeterminantCropType,FertilityStressOn : BOOLEAN) : double;

Const EToStandard = 5;
      k = 2;
      CO2iLocal = 369.41;

VAR SumKcTop,HIGC,HIGClinear : double;
    RatDGDD,SumBPot,SumBSF : double;
    tSwitch,DaysYieldFormation : INTEGER;

BEGIN
//1. Initialize
//1 - a. Maximum sum Kc
SumKcTop := SeasonalSumOfKcPot(TempDaysToCCini,TempGDDaysToCCini,
                               TempL0,TempL12,TempL123,TempHarvest,TempGDDL0,TempGDDL12,TempGDDL123,TempGDDHarvest,
                               TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                               TempKc,TempKcDecline,TempCCeffect,
                               TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,CO2iLocal,
                               TempModeCycle);
//1 - b. Prepare for growing degree days
RatDGDD := 1;
//IF ((TempModeCycle = GDDays) AND (GDDL12SF < TempGDDL123))
IF ((TempModeCycle = GDDays) AND (SFInfoStress > 0) AND (GDDL12SF < TempGDDL123))
   THEN RatDGDD := (TempL123-L12SF)/(TempGDDL123-GDDL12SF);
//1 - c. Get PercentLagPhase (for estimate WPi during yield formation)
DaysYieldFormation := undef_int;
IF ((GetCrop_subkind() = Tuber) OR (GetCrop().Subkind = grain)) THEN //DaysToFlowering corresponds with Tuberformation
   BEGIN
   DaysYieldFormation := ROUND(TempHI/TempdHIdt);
   IF DeterminantCropType
      THEN BEGIN
           HIGC := HarvestIndexGrowthCoefficient(TempHI,TempdHIdt);
           GetDaySwitchToLinear(TempHI,TempdHIdt,HIGC,tSwitch,HIGClinear);
           END
      ELSE tSwitch := ROUND(DaysYieldFormation/3);
   END;

//2. potential biomass - no soil fertiltiy stress - no weed stress
SumBPot := Bnormalized(TempDaysToCCini,TempGDDaysToCCini,
                       TempL0,TempL12,TempL12,TempL123,TempHarvest,TempFlower,
                       TempGDDL0,TempGDDL12,TempGDDL12,TempGDDL123,TempGDDHarvest,TempWPy,DaysYieldFormation,tSwitch,
                       TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                       TempKc,TempKcDecline,TempCCeffect,TempWP,CO2iLocal,
                       //TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,RatDGDD,SumKcTop,
                       TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,(1),SumKcTop,
                       (0),(0),(0),(0),(0),(0),(0),(0),(-0.01),TempModeCycle,FertilityStressOn,(false));

//3. potential biomass - soil fertiltiy stress and weed stress
SumBSF := Bnormalized(TempDaysToCCini,TempGDDaysToCCini,
                      TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                      TempGDDL0,TempGDDL12,GDDL12SF,TempGDDL123,TempGDDHarvest,TempWPy,DaysYieldFormation,tSwitch,
                      TempCCo,TempCCx,TempCGC,TempGDDCGC,TempCDC,TempGDDCDC,
                      TempKc,TempKcDecline,TempCCeffect,TempWP,CO2iLocal,
                      TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,RatDGDD,SumKcTop,
                      SFInfoStress,SFInfo.RedCGC,SFInfo.RedCCX,SFInfo.RedWP,SFInfo.RedKsSto,WeedStress,
                      DeltaWeedStress,SFInfo.CDecline,ShapeFweed,TempModeCycle,FertilityStressOn,(false));

BiomassRatio := SumBSF/SumBPot;
END; (* BiomassRatio *)


end.
