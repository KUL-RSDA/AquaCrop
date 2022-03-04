unit TempProcessing;

interface

uses Global, interface_global, SysUtils, interface_tempprocessing;


PROCEDURE LoadSimulationRunProject(NameFileFull : string;
                                   NrRun : INTEGER);


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


end.
