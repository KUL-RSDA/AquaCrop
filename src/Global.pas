unit Global;

interface

uses SysUtils, interface_global;


Const max_No_compartments = 12;
      Equiv = 0.64; // conversion factor: 1 dS/m = 0.64 g/l

      NameMonth : ARRAY[1..12] of string = ('January','February','March','April',
          'May','June','July','August','September','October','November','December');

      DaysInMonth : ARRAY[1..12] of integer = (31,28,31,30,31,30,31,31,30,31,30,31);
      EvapZmin = 15; //cm  minimum soil depth for water extraction by evaporation

TYPE
     repstring17 = string[17]; (* Date string *)
     rep_string3  = string[3];  (* Read/Write ProfFile *)

TYPE
     CompartmentIndividual = Record
         Thickness : double;  (* meter *)
         theta     : double;  (* m3/m3 *)
         fluxout   : double;  (* mm/day *)
         Layer     : INTEGER;
         Smax      : double;  (* Maximum root extraction m3/m3.day *)
         FCadj     : double;  (* Vol % at Field Capacity adjusted to Aquifer *)
         DayAnaero : INTEGER; (* number of days under anaerobic conditions *)
         WFactor   : double;  (* weighting factor 0 ... 1
                               Importance of compartment in calculation of
                               - relative wetness (RUNOFF)
                               - evaporation process
                               - transpiration process *)
         // salinity factors
         Salt      : rep_salt; // salt content in solution in cells (g/m2)
         Depo      : rep_salt; // salt deposit in cells (g/m2)
         END;

     rep_soil = Record
         REW            : ShortInt; (* Readily evaporable water mm *)
         NrSoilLayers   : ShortInt;
         CNvalue        : ShortInt;
         RootMax        : Single; // maximum rooting depth in soil profile for selected crop
         end;

     rep_Comp = ARRAY[1.. max_No_compartments] of CompartmentIndividual;

     rep_Content = Record  // total water (mm) or salt (Mg/ha) content
         BeginDay  : double; //at the beginning of the day
         EndDay    : double; //at the end of the day
         ErrorDay  : double; //error on WaterContent or SaltContent over the day
         END;

     rep_subkind = (Vegetative,Grain,Tuber,Forage);
     rep_pMethod = (NoCorrection,FAOCorrection);
     

     rep_Assimilates = Record
         On          : Boolean;
         Period      : INTEGER; (* Number of days at end of season during which assimilates are stored in root system *)
         Stored      : ShortInt; (* Percentage of assimilates, transferred to root system at last day of season *)
         Mobilized   : ShortInt; (* Percentage of stored assimilates, transferred to above ground parts in next season *)
         end;

     rep_Crop = Record
         subkind       : rep_subkind;
         ModeCycle     : rep_modeCycle;
         Planting      : rep_Planting; // 1 = sown, 0 = transplanted, -9 = regrowth
         pMethod       : rep_pMethod;
         pdef : double;  // soil water depletion fraction for no stomatal stress as defined (ETo = 5 mm/day)
         pActStom : double; // actual p for no stomatal stress for ETo of the day
         KsShapeFactorLeaf,KsShapeFactorStomata,KsShapeFactorSenescence : Double;
         pLeafDefUL,pLeafDefLL: double; //soil water depletion fraction for leaf expansion (ETo = 5 mm/day)
         pLeafAct    : double; //actual p for upper limit leaf expansion for ETo of the day
         pSenescence : double; //soil water depletion fraction for canopys senescence (ETo = 5 mm/day)
         pSenAct     : double; //actual p for canopy senescence for ETo of the day
         pPollination : double; //soil water depletion fraction for failure of pollination
         SumEToDelaySenescence : INTEGER;
         AnaeroPoint : INTEGER; (* (SAT - [vol%]) at which deficient aeration *)
         StressResponse : rep_Shapes; // is reponse to soil fertility stress
         ECemin,                // lower threshold for salinity stress (dS/m)
         ECemax,                // upper threshold for salinity stress (dS/m)
         CCsaltDistortion : ShortInt;  // distortion canopy cover for calibration for simulation of effect of salinity stress (%)
         ResponseECsw : INTEGER; // Response of Ks stomata to ECsw for calibration: From 0 (none) to +200 (very strong)
         SmaxTopQuarter, (* Smax Top 1/4 root zone HOOGLAND *)
         SmaxBotQuarter, (* Smax Bottom 1/4 root zone HOOGLAND *)
         SmaxTop,   (* Smax Top root zone HOOGLAND *)
         SmaxBot,   (* Smax Bottom root zone HOOGLAND *)
         KcTop : double;
         KcDecline : double; // Reduction Kc (%CCx/day) as result of ageing effects, nitrogen defficiency, etc.
         CCEffectEvapLate : INTEGER; (* % *)
         Day1 : LongInt;   (* Daynummer: first day of croping period starting from sowing/transplanting *)
         DayN : LongInt;   (* Daynummer: last day = harvest day*)
         Length : rep_int_array; (* 1 .. 4 :  = the four growth stages  *)
         RootMin, RootMax : double;   // rooting depth in meter
         RootShape : ShortInt;     // 10 times the root of the root function
         Tbase,                  //Base Temperature (degC)
         Tupper       : double;  //Upper temperature threshold (degC)
         Tcold,                   // Minimum air temperature below which pollination starts to fail (cold stress) (degC)
         Theat        : ShortInt; // Maximum air temperature above which pollination starts to fail (heat stress) (degC)
         GDtranspLow  : double; // Minimum growing degrees required for full crop transpiration (degC - day)
         SizeSeedling : double;  //Canopy cover per seedling (cm2)
         SizePlant    : double;  //Canopy cover of plant on 1st day (cm2) when regrowth
         PlantingDens : LongInt; //number of plants per hectare
         CCo,                    //starting canopy size  (fraction canopy cover)
         CCini        : double;  //starting canopy size for regrowth (fraction canopy cover)
         CGC          : double;  //Canopy growth coefficient (increase of CC in fraction per day)
         GDDCGC       : double;  //Canopy growth coefficient (increase of CC in fraction per growing-degree day)
         CCx          : double;  //expected maximum canopy cover  (fraction canopy cover)
         CDC          : double;  //Canopy Decline Coefficient (decrease of CC in fraction per day)
         GDDCDC       : double;  //Canopy Decline Coefficient (decrease of CC in fraction per growing-degree day)
         CCxAdjusted  : double;  //maximum canopy cover given water stress
         CCxWithered  : double;  //maximum existed CC during season (for correction Evap for withered canopy)
         CCoAdjusted  : double;  //initial canopy size after soil water stress
         DaysToCCini        : integer; //required for regrowth (if CCini > CCo)
         DaysToGermination  : integer;  //given or calculated from GDD
         DaysToFullCanopy   : integer;  //given or calculated from GDD
         DaysToFullCanopySF : integer; // adjusted to soil fertility
         DaysToFlowering    : integer;  //given or calculated from GDD
         LengthFlowering    : integer;  //given or calculated from GDD
         DaysToSenescence   : integer;  //given or calculated from GDD
         DaysToHarvest      : integer;  //given or calculated from GDD
         DaysToMaxRooting   : integer;  //given or calculated from GDD
         DaysToHIo          : integer;  //given or calculated from GDD
         GDDaysToCCini      : integer; //required for regrowth (if CCini > CCo)
         GDDaysToGermination: integer;  //given or calculated from Calendar Days
         GDDaysToFullCanopy : integer;  //given or calculated from Calendar Days
         GDDaysToFullCanopySF : integer;  //adjusted to soil fertility
         GDDaysToFlowering  : INTEGER;  //given or calculated from Calendar Days
         GDDLengthFlowering : Integer;  //given or calculated from Calendar Days
         GDDaysToSenescence : integer;  //given or calculated from Calendar Days
         GDDaysToHarvest    : integer;  //given or calculated from Calendar Days
         GDDaysToMaxRooting : integer;  //given or calculated from Calendar Days
         GDDaysToHIo        : integer;  //given or calculated from Calendar Days
         WP                 : double;  // (normalized) water productivity (gram/m2)
         WPy                : integer; // (normalized) water productivity during yield formation (Percent WP)
         AdaptedToCO2       : ShortInt; // Crop performance under elevated atmospheric CO2 concentration (%)
         HI                 : integer;  // HI harvest index (percentage)
         dHIdt              : double;   // average rate of change in harvest index (% increase per calendar day)
         HIincrease         : ShortInt; // possible increase (%) of HI due to water stress before flowering
         aCoeff             : double; // coefficient describing impact of restricted vegetative growth at flowering on HI
         bCoeff             : double; // coefficient describing impact of stomatal closure at flowering on HI
         DHImax             : ShortInt; // allowable maximum increase (%) of specified HI
         DeterminancyLinked : BOOLEAN; // linkage of determinancy with flowering
         fExcess            : SmallInt; // potential excess of fruits (%) ranging form
         DryMatter          : ShortInt; // dry matter content (%) of fresh yield
         RootMinYear1       : double; // minimum rooting depth in first year in meter (for perennial crops)
         SownYear1          : BOOLEAN; // True = Sown, False = transplanted (for perennial crops)
         YearCCx            : ShortInt; // number of years at which CCx declines to 90 % of its value due to self-thinning - Perennials
         CCxRoot            : double; // shape factor of the decline of CCx over the years due to self-thinning - Perennials
         Assimilates        : rep_Assimilates;
         END;

     rep_TimeCuttings = (NA,IntDay,IntGDD,DryB,DryY,FreshY);
     rep_Cuttings = Record
         Considered : BOOLEAN;
         CCcut      : Integer; // Canopy cover (%) after cutting
         CGCPlus    : Integer; // Increase (percentage) of CGC after cutting
         Day1       : Integer; // first day after time window for generating cuttings (1 = start crop cycle)
         NrDays     : Integer; // number of days of time window for generate cuttings (-9 is whole crop cycle)
         Generate   : Boolean; // ture: generate cuttings; false : schedule for cuttings
         Criterion  : rep_TimeCuttings; // time criterion for generating cuttings
         HarvestEnd : BOOLEAN; // final harvest at crop maturity
         FirstDayNr : LongInt; // first dayNr of list of specified cutting events (-9 = onset growing cycle)
         end;

     rep_Manag = Record
         Mulch           : ShortInt; (* percent soil cover by mulch in growing period *)
         SoilCoverBefore : ShortInt; (* percent soil cover by mulch before growing period *)
         SoilCoverAfter  : ShortInt; (* percent soil cover by mulch after growing period *)
         EffectMulchOffS : ShortInt; (* effect Mulch on evaporation before and after growing period *)
         EffectMulchInS  : ShortInt; (* effect Mulch on evaporation in growing period *)
         FertilityStress : ShortInt;
         BundHeight      : double; // meter;
         RunoffOn        : BOOLEAN;  (* surface runoff *)
         CNcorrection    : INTEGER; // percent increase/decrease of CN
         WeedRC          : ShortInt; (* Relative weed cover in percentage at canopy closure *)
         WeedDeltaRC     : INTEGER; (* Increase/Decrease of Relative weed cover in percentage during mid season*)
         WeedShape       : Double; (* Shape factor for crop canopy suppression*)
         WeedAdj         : ShortInt; (* replacement (%) by weeds of the self-thinned part of the Canopy Cover - only for perennials *)
         Cuttings        : rep_Cuttings; // Multiple cuttings
         end;

     rep_EffectiveRainMethod = (Full,USDA,Percentage);
     rep_MonthInteger = ARRAY[1..12] OF INTEGER;

     rep_EffectiveRain = RECORD    // for 10-day or monthly rainfall data
         Method : rep_EffectiveRainMethod;
         PercentEffRain : ShortInt; // IF Method = Percentage
         ShowersInDecade : ShortInt; // adjustment of surface run-off
         RootNrEvap : ShortInt; // Root for reduction in soil evaporation
         end;

     rep_param = RECORD  // DEFAULT.PAR
         // crop parameters IN CROP.PAR - with Reset option
         EvapDeclineFactor : ShortInt;  // exponential decline with relative soil water [1 = small ... 8 = sharp]
         KcWetBare       : double; //Soil evaporation coefficients from wet bare soil
         PercCCxHIfinal    : ShortInt; // CC threshold below which HI no longer increase (% of 100)
         RootPercentZmin : INTEGER; //starting depth of root sine function in % of Zmin (sowing depth)
         MaxRootZoneExpansion : double; // maximum root zone expansion in cm/day - fixed at 5 cm/day
         KsShapeFactorRoot : Shortint; //shape factro for the effect of water stress on root zone expansion
         TAWGermination    : ShortInt; // Soil water content (% TAW) required at sowing depth for germination
         pAdjFAO         : double; //Adjustment factor for FAO-adjustment of soil water depletion (p) for various ET
         DelayLowOxygen  : INTEGER; //delay [days] for full effect of anaeroby
         ExpFsen           : double; // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
         Beta              : ShortInt; // Percentage decrease of p(senescence) once early canopy senescence is triggered
         ThicknessTopSWC   : ShortInt; // Thickness of top soil for determination of its Soil Water Content (cm)
         // Field parameter IN FIELD.PAR  - with Reset option
         EvapZmax : ShortInt; // cm  maximum soil depth for water extraction by evaporation
         // Runoff parameters IN RUNOFF.PAR  - with Reset option
         RunoffDepth : double; //considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment
         CNcorrection : BOOLEAN; //correction Antecedent Moisture Class (On/Off)
         // Temperature parameters IN TEMPERATURE.PAR  - with Reset option
         Tmin,Tmax   : double; // Default Minimum and maximum air temperature (degC) if no temperature file
         GDDMethod   : ShortInt; // 1 for Method 1, 2 for Method 2, 3 for Method 3
         // General parameters IN GENERAL.PAR
         PercRAW     : INTEGER; //allowable percent RAW depletion for determination Inet
         CompDefThick: double; // Default thickness of soil compartments [m]
         CropDay1    : integer;  // First day after sowing/transplanting (DAP = 1)
         Tbase,Tupper : double; // Default base and upper temperature (degC) assigned to crop
         IrriFwInSeason  : ShortInt; // Percentage of soil surface wetted by irrigation in crop season
         IrriFwOffSeason : ShortInt; // Percentage of soil surface wetted by irrigation off-season
         // Showers parameters (10-day or monthly rainfall) IN SHOWERS.PAR
         ShowersInDecade : rep_MonthInteger; // 10-day or Monthly rainfall --> Runoff estimate
         EffectiveRain : rep_EffectiveRain; // 10-day or Monthly rainfall --> Effective rainfall
         // Salinity
         SaltDiff : ShortInt; // salt diffusion factor (capacity for salt diffusion in micro pores) [%]
         SaltSolub : ShortInt;  // salt solubility [g/liter]
         // Groundwater table
         ConstGwt : BOOLEAN; // groundwater table is constant (or absent) during the simulation period
         // Capillary rise
         RootNrDF : shortint;
         // Initial abstraction for surface runoff
         IniAbstract : shortint;
         END;

     rep_sum = RECORD
         Epot, Tpot, Rain, Irrigation, Infiltrated,
         Runoff, Drain, Eact, Tact, TrW, ECropCycle, CRwater  : double;  (* mm *)
         Biomass, YieldPart, BiomassPot, BiomassUnlim, BiomassTot : double;   (* ton/ha *)
         SaltIn, SaltOut, CRsalt : double; (* ton/ha *)
         End;

     rep_datatype = (Daily,Decadely,Monthly);
     rep_clim = Record
         DataType    : rep_datatype;
         FromD,FromM,FromY : INTEGER; //D = day or decade, Y=1901 is not linked to specific year
         ToD,ToM,ToY : INTEGER;
         FromDayNr, ToDayNr : LongInt; //daynumber
         FromString, ToString : String;
         NrObs       : INTEGER; // number of observations
         End;

     rep_IniComp =  ARRAY[1.. max_No_compartments] of double;
     rep_IniSWC = RECORD
         AtDepths : BOOLEAN;    // at specific depths or for specific layers
         NrLoc    : ShortInt;   // number of depths or layers considered
         Loc      : rep_IniComp;  //depth or layer thickness [m]
         VolProc  : rep_IniComp;  //soil water content (vol%)
         SaltECe  : rep_IniComp; // ECe in dS/m
         AtFC     : BOOLEAN;     // If iniSWC is at FC
         end;

     rep_storage = Record
         Btotal     : double; (* assimilates (ton/ha) stored in root systemn by CropString in Storage-Season *)
         CropString : string; (* full name of crop file which stores Btotal during Storage-Season *)
         Season     : ShortInt;(* season in which Btotal is stored *)
         end;

     rep_sim = Record
         FromDayNr, ToDayNr : LongInt; //daynumber
         IniSWC    : rep_IniSWC;
         ThetaIni,
         ECeIni    : rep_IniComp; // dS/m
         SurfaceStorageIni,
         ECStorageIni       : double;
         CCini,
         Bini,
         Zrini       : double;
         LinkCropToSimPeriod,
         ResetIniSWC : BOOLEAN; // soil water and salts
         InitialStep : INTEGER;
         EvapLimitON : BOOLEAN; // soil evap is before late season stage limited due to sheltering effect of (partly) withered canopy cover
         EvapWCsurf : double; // remaining water (mm) in surface soil layer for stage 1 evaporation [REW .. 0]
         EvapStartStg2 : ShortInt; // % extra to define upper limit of soil water content at start of stage 2 [100 .. 0]
         EvapZ  : double; // actual soil depth (m) for water extraction by evaporation  [EvapZmin/100 .. EvapZmax/100]
         HIfinal : INTEGER; //final Harvest Index might be smaller than HImax due to early canopy decline
         DelayedDays : INTEGER; //delayed days since sowing/planting due to water stress (crop cannot germinate)
         Germinate   : BOOLEAN; // germinate is false when crop cannot germinate due to water stress
         SumEToStress : double; // Sum ETo during stress period to delay canopy senescence
         SumGDD : double; // Sum of Growing Degree-days
         SumGDDfromDay1 : double; // Sum of Growing Degree-days since Crop.Day1
         SCor : single; // correction factor for Crop.SmaxBot if restrictive soil layer inhibit root development
         MultipleRun : BOOLEAN; // Project with a sequence of simulation runs
         NrRuns : INTEGER;
         MultipleRunWithKeepSWC : BOOLEAN; // Project with a sequence of simulation runs and initial SWC is once or more KeepSWC
         MultipleRunConstZrx : double; // Maximum rooting depth for multiple projects with KeepSWC
         IrriECw : double; //quality of irrigation water (dS/m)
         DayAnaero : ShortInt; (* number of days under anaerobic conditions *)
         EffectStress : rep_EffectStress;  // effect of soil fertility and salinity stress on CC, WP and KsSto
         SalinityConsidered : BOOLEAN;

         ProtectedSeedling : BOOLEAN; // IF protected (before CC = 1.25 CC0), seedling triggering of early senescence is switched off
         SWCtopSoilConsidered : BOOLEAN; // Top soil is relative wetter than root zone and determines water stresses

         LengthCuttingInterval : INTEGER; // Default length of cutting interval (days)

         YearSeason : ShortInt; // year number for perennials (1 = 1st year, 2, 3, 4, max = 127)
         RCadj : ShortInt; // adjusted relative cover of weeds with self thinning for perennials
         Storage : rep_storage;

         YearStartCropCycle : INTEGER; // calendar year in which crop cycle starts
         CropDay1Previous : LongInt;  // previous daynumber at the start of teh crop cycle
         End;

     rep_IrriMode = (NoIrri,Manual,Generate,Inet);
     rep_IrriMethod = (MBasin,MBorder,MDrip,MFurrow,MSprinkler);

     rep_GenerateTimeMode = (FixInt,AllDepl,AllRAW,WaterBetweenBunds);
     rep_GenerateDepthMode = (ToFC,FixDepth);
     rep_DayEventInt = Record
         DayNr : Integer;
         Param : Integer;
         end;
     rep_DayEventDbl = Record
         DayNr : Integer;
         Param : Double;
         end;
     rep_SimulationEventsDbl = ARRAY[1..31] OF Rep_DayEventDbl; // for processing 10-day monthly climatic data

     rep_IrriOutSeasonEvents = ARRAY[1..5] OF Rep_DayEventInt;

     rep_RootZoneSalt = Record
         ECe    : double;   // Electrical conductivity of the saturated soil-paste extract (dS/m)
         ECsw   : double;   // Electrical conductivity of the soil water (dS/m)
         ECswFC : double;   // Electrical conductivity of the soil water at Field Capacity(dS/m)
         KsSalt : double;   // stress coefficient for salinity
         end;

     repCriterion = (CumulRain, RainPeriod, RainDecade, RainVsETo);
     repAirTCriterion = (TminPeriod,TmeanPeriod,GDDPeriod,CumulGDD);

     rep_Onset = Record
         GenerateOn : BOOLEAN;  // by rainfall or temperature criterion
         GenerateTempOn : BOOLEAN; // by temperature criterion
         Criterion : repCriterion;
         AirTCriterion : repAirTCriterion;
         StartSearchDayNr, StopSearchDayNr : LongInt; //daynumber
         LengthSearchPeriod : INTEGER; // days
         end;

     rep_EndSeason = Record
         ExtraYears : Integer; // to add to YearStartCropCycle
         GenerateTempOn : BOOLEAN; // by temperature criterion
         AirTCriterion : repAirTCriterion;
         StartSearchDayNr, StopSearchDayNr : LongInt; //daynumber
         LengthSearchPeriod : INTEGER; //days
         end;

     rep_PerennialPeriod = RECORD
        GenerateOnset : BOOLEAN; // onset is generated by air temperature criterion
	OnsetCriterion : repAirTCriterion;
	OnsetFirstDay,OnsetFirstMonth : INTEGER;
	OnsetStartSearchDayNr, OnsetStopSearchDayNr : LongInt; //daynumber
	OnsetLengthSearchPeriod : INTEGER; //days
	OnsetThresholdValue : double; // degC or degree-days
	OnsetPeriodValue : INTEGER; // number of successive days
	OnsetOccurrence : ShortInt; // number of occurrences (1,2 or 3)
        GenerateEnd : BOOLEAN; // end is generate by air temperature criterion
        EndCriterion : repAirTCriterion;
	EndLastDay,EndLastMonth : INTEGER;
        ExtraYears : INTEGER; // number of years to add to the onset year
        EndStartSearchDayNr, EndStopSearchDayNr : LongInt; //daynumber
	EndLengthSearchPeriod : INTEGER; //days
	EndThresholdValue : double; // degC or degree-days
	EndPeriodValue : INTEGER; // number of successive days
	EndOccurrence : ShortInt; // number of occurrences (1,2 or 3)
        GeneratedDayNrOnset,GeneratedDayNrEnd : LongInt;
	end;

      repTypeClimData = (ETData,RainData,TmpData,CO2Data,RainETData);
      rep_TypePlot = (TypeA,TypeZgwt,TypeZr);
      rep_TypeObsSim =(ObsSimCC,ObsSimB,ObsSimSWC);

      rep_OUTindividual = Record
         ClimOUT,
         CropOUT,
         WabalOUT,
         ProfOUT,
         SaltOUT,
         CompWCOUT,
         CompECOUT,
         InetOUT,
         HarvestOUT,
         Requested : BOOLEAN;
         end;

      rep_OUTevaluation = Record
         EvalDataOUT,
         EvalStatOUT,
         Requested : BOOLEAN;
         end;

      rep_OUTfiles = Record
         FileOUTseasonal : BOOLEAN;
         FileOUTindividual : rep_OUTindividual;
         FileOUTevaluation : rep_OUTevaluation;
         end;


VAR PathNameProg,PathNameData,PathNameOutp,PathNameSimul,PathNameObs,PathNameImport : string;
    DataPath,ObsPath : BOOLEAN;
    TemperatureFile,ProjectFile,MultipleProjectFile: string;
    CalendarFileFull,CropFilefull, ClimateFileFull,TemperatureFileFull,CO2FileFull,
    IrriFileFull,SWCiniFileFull,ProjectFileFull,MultipleProjectFileFull,
    FullFileNameProgramParameters : string;
    ProfDescription, ClimateDescription,CalendarDescription,CropDescription,ClimDescription,EToDescription,RainDescription,
    TemperatureDescription,CO2Description,IrriDescription,ManDescription,SWCiniDescription,
    ProjectDescription,MultipleProjectDescription,OffSeasonDescription,GroundWaterDescription: string;
    ClimRecord,
    EToRecord,
    RainRecord,
    TemperatureRecord     : rep_clim;
    Simulation     : rep_sim;
    IrriMode       : rep_IrriMode;
    IrriMethod     : rep_IrriMethod;
    GenerateTimeMode : rep_GenerateTimeMode;
    GenerateDepthMode : rep_GenerateDepthMode;
    IrriFirstDayNr : LongInt;
    SoilLayer      : rep_SoilLayer;
    Compartment    : rep_Comp;
    Soil           : rep_soil;
    NrCompartments : INTEGER;
    TotalWaterContent,
    TotalSaltContent   : rep_Content; //Water Content (mm) and Salt Content (Mg/ha)
    Crop           : rep_Crop;
    Management     : rep_Manag;
    SumWabal       : rep_sum;
    RootingDepth   : double;
    CCiActual,CCiPrev,CCiTopEarlySen : double;

    SenStage       : INTEGER;
    DaySubmerged   : INTEGER;
    ETo, Epot, Tpot, Rain, Irrigation, Infiltrated, CRwater : double;   (* mm/day *)
    Tmin, Tmax : double; (* degC *)
    SurfaceStorage, Runoff, Drain, Eact, Tact, TactWeedInfested : double;        (* mm/day *)
    EvapoEntireSoilSurface : BOOLEAN; // True of soil wetted by RAIN (false = IRRIGATION and fw < 1)
    OutputName     : string;
    PreDay         : BOOLEAN;
    SimulParam     : rep_param;
    Surf0          : double; (* surface water [mm] begin day *)
    NrC,NrD        : INTEGER; (* formats REAL *)
    MinReal, MaxReal : double;
    MinInt, MaxInt : INTEGER;
    IrriBeforeSeason,
    IrriAfterSeason : rep_IrriOutSeasonEvents;
    TypeClimData : repTypeClimData;
    MaxPlotNew : Integer;
    TypePlotNew : rep_TypePlot;
    MaxPlotTr : ShortInt;
    OUTPUTfiles : rep_OUTfiles;
    Onset : rep_Onset;
    EndSeason : rep_EndSeason;
    IniPercTAW : ShortInt; // Default Value for Percentage TAW for Initial Soil Water Content Menu
    // salinity
    ECstorage      : double; (* EC surface storage dS/m *)
    ECdrain        : double; (* EC drain water dS/m *)
    SaltInfiltr    : double; (* salt infiltrated in soil profile Mg/ha *)
    CRsalt         : double; // gram/m2
    RootZoneSalt   : rep_RootZoneSalt;
    ZiAqua         : Integer;  // Depth of Groundwater table below soil surface in centimeter
    ECiAqua        : double; //  EC of the groundwater table in dS/m
    PerennialPeriod : rep_PerennialPeriod;



// Extra for stand alone procedure
    OutputAggregate : ShortInt;
    PathNameList,PathNameParam : string;
    Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily,
    Part1Mult,Part2Eval : BOOLEAN;

    Type
    repTypeProject = (TypePRO,TypePRM,TypeNone);

FUNCTION ActualRootingDepth(DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234 : INTEGER;
                            SumGDD,Zmin,Zmax : double;
                            ShapeFactor : ShortInt;
                            TypeDays : rep_modeCycle) : double;
PROCEDURE CalculateETpot(DAP,L0,L12,L123,LHarvest,DayLastCut : INTEGER;
                         CCi,EToVal,KcVal,KcDeclineVal,CCx,CCxWithered,CCeffectProcent,CO2i,GDDayi,TempGDtranspLow : double;
                         VAR TpotVal, EpotVal : double);

PROCEDURE GlobalZero(VAR SumWabal : rep_sum);
PROCEDURE TimeToMaxCanopySF(CCo,CGC,CCx : double;
                            L0,L12,L123,LToFlor,LFlor : INTEGER;
                            DeterminantCrop : BOOLEAN;
                            VAR L12SF : INTEGER;
                            VAR RedCGC,RedCCx : ShortInt;
                            VAR ClassSF : ShortInt);
PROCEDURE NoManagement;
PROCEDURE LoadManagement(FullName : string);

PROCEDURE NoIrrigation;
PROCEDURE NoManagementOffSeason;
PROCEDURE LoadOffSeason(FullName : string);

PROCEDURE LoadIrriScheduleInfo(FullName : string);
PROCEDURE DetermineNrandThicknessCompartments;
PROCEDURE CalculateAdjustedFC(DepthAquifer : double;
                              VAR CompartAdj   : rep_Comp);
PROCEDURE DesignateSoilLayerToCompartments(NrCompartments,NrSoilLayers : INTEGER;
                                          VAR Compartment : rep_Comp);
PROCEDURE DeclareInitialCondAtFCandNoSalt;
PROCEDURE specify_soil_layer(NrCompartments,NrSoilLayers : INTEGER;
                             VAR SoilLayer : rep_SoilLayer;
                             VAR Compartment : rep_Comp;
                             //InitialWC : rep_InitialWC;
                             VAR TotalWaterContent : rep_Content);

PROCEDURE DetermineParametersCR(SoilClass : ShortInt;
                                KsatMM : double;
                                VAR aParam, bParam : double);
FUNCTION ActiveCells(Comp : CompartmentIndividual) : INTEGER;
PROCEDURE Calculate_Saltmobility(layer : INTEGER;
                                 SaltDiffusion : ShortInt;  // percentage
                                 Macro : ShortInt;
                                 VAR Mobil : rep_salt);
FUNCTION ECeComp (Comp : CompartmentIndividual) : double;
FUNCTION ECswComp (Comp : CompartmentIndividual;
                   atFC : BOOLEAN) : double;
PROCEDURE SaltSolutionDeposit(mm : double; (* mm = l/m2 *)
                      VAR SaltSolution,SaltDeposit : double); (* g/m2 *)
PROCEDURE DetermineSaltContent(ECe : double;
                               VAR Comp : CompartmentIndividual);

PROCEDURE CompleteProfileDescription;
PROCEDURE LoadProfile(FullName : string);

FUNCTION CCiniTotalFromTimeToCCini(TempDaysToCCini,TempGDDaysToCCini,
                                   L0,L12,L12SF,L123,L1234,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234 : INTEGER;
                                   CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,RatDGDD : double;
                                   SFRedCGC,SFRedCCx : ShortInt;
                                   SFCDecline,fWeed : Double;
                                   TheModeCycle : rep_modeCycle) : double;


PROCEDURE CompleteCropDescription;
PROCEDURE LoadCrop (FullName : string);
PROCEDURE CompleteClimateDescription(VAR ClimateRecord : rep_clim);
PROCEDURE LoadClimate(FullName : string;
                      VAR ClimateDescription : string;
                      VAR TempFile,EToFile,RainFile,CO2File: string);
PROCEDURE LoadClim (FullName : string;
                    VAR ClimateDescription : string;
                    VAR ClimateRecord : rep_clim);
PROCEDURE SaveProfile(totalname : string);
PROCEDURE AppendCropFilePerennials(totalname : string;
                                   GenrateTheOnset,GenerateTheEnd : BOOLEAN;
                                   CriterionNrOnset,Day1Onset,Month1Onset,LengthOnset,SuccessiveDaysOnset,OccurrenceOnset : INTEGER;
                                   CriterionNrEnd,DayNEnd,MonthNEnd,ExtraYearEnd,LengthEnd,SuccessiveDaysEnd,OccurrenceEnd : INTEGER;
                                   ThresholdOnset,ThresholdEnd : double);
PROCEDURE SaveCrop(totalname : string);
FUNCTION EndGrowingPeriod(Day1 : longint;
                          VAR DayN : longint) : string;
PROCEDURE DetermineLinkedSimDay1(CropDay1 : LongInt;
                                 VAR SimDay1 :LongInt);
PROCEDURE AdjustCropYearToClimFile(VAR CDay1,CDayN : longint);
PROCEDURE AdjustClimRecordTo(CDayN : longint);
PROCEDURE ResetSWCToFC;
PROCEDURE AdjustSimPeriod;
PROCEDURE AdjustOnsetSearchPeriod;
PROCEDURE SetClimData;
PROCEDURE DetermineRootZoneWC(RootingDepth : double;
                              VAR ZtopSWCconsidered : BOOLEAN);
FUNCTION DayString(DNr : LongInt) : repstring17;
FUNCTION CanopyCoverNoStressSF(DAP,L0,L123,LMaturity,GDDL0,GDDL123,GDDLMaturity : INTEGER;
                               CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
                               TypeDays : rep_modeCycle;
                               SFRedCGC,SFRedCCx : ShortInt) : double;

PROCEDURE ReadSoilSettings;
FUNCTION HarvestIndexDay(DAP  : LongInt;
                         DaysToFlower,HImax : integer;
                         dHIdt,CCi,CCxadjusted : double;
                         PercCCxHIfinal        : ShortInt;
                         TempPlanting : rep_Planting;
                         VAR PercentLagPhase : ShortInt;
                         VAR HIfinal : INTEGER)   : double;
PROCEDURE ReadRainfallSettings;
PROCEDURE ReadCropSettingsParameters;
PROCEDURE ReadFieldSettingsParameters;
PROCEDURE ReadTemperatureSettingsParameters;
FUNCTION AdjustedKsStoToECsw(ECeMin,ECeMax : ShortInt;
                             ResponseECsw : INTEGER;
                             ECei,ECswi,ECswFCi,Wrel,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,KsStoIN : double) : double;

PROCEDURE DetermineRootZoneSaltContent(RootingDepth : double;
                                       VAR ZrECe,ZrECsw,ZrECswFC,ZrKsSalt : double);
FUNCTION CO2ForSimulationPeriod(FromDayNr,ToDayNr : LongInt) : double;

FUNCTION CCiNoWaterStressSF(Dayi,L0,L12SF,L123,L1234,
                            GDDL0,GDDL12SF,GDDL123,GDDL1234  : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
                            SFRedCGC,SFRedCCx : ShortInt;
                            SFCDecline : Double;
                            TheModeCycle : rep_modeCycle) : double;
FUNCTION SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                            L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                            Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,CO2i : double;
                            TheModeCycle : rep_modeCycle) : double;
PROCEDURE TranslateIniLayersToSWProfile(NrLay : ShortInt;
                                        LayThickness,LayVolPr,LayECdS : rep_IniComp;
                                        NrComp : INTEGER;
                                        VAR Comp : rep_Comp);

PROCEDURE TranslateIniPointsToSWProfile(NrLoc : ShortInt;
                                        LocDepth,LocVolPr,LocECdS : rep_IniComp;
                                        NrComp : INTEGER;
                                        VAR Comp : rep_Comp);
PROCEDURE LoadInitialConditions(SWCiniFileFull : string;
                                VAR IniSurfaceStorage : double;
                                VAR IniSWCRead : rep_IniSWC);
PROCEDURE LoadProjectDescription(FullNameProjectFile : string;
                                 VAR DescriptionOfProject : string);
PROCEDURE ComposeOutputFileName(TheProjectFileName : string);
PROCEDURE CheckForKeepSWC(FullNameProjectFile : string;
                          TotalNrOfRuns : INTEGER;
                          VAR RunWithKeepSWC : BOOLEAN;
                          VAR ConstZrxForRun : double);
PROCEDURE AdjustThetaInitial(PrevNrComp : ShortInt;
                             PrevThickComp,PrevVolPrComp,PrevECdSComp : rep_IniComp);
PROCEDURE AdjustSizeCompartments(CropZx : double);
PROCEDURE CheckForWaterTableInProfile(DepthGWTmeter : double;
                                     ProfileComp : rep_comp;
                                     VAR WaterTableInProfile : BOOLEAN);
PROCEDURE LoadGroundWater(FullName : string;
                          AtDayNr : LongInt;
                          VAR Zcm : INTEGER;
                          VAR ECdSm : double);
FUNCTION CCmultiplierWeedAdjusted(ProcentWeedCover : ShortInt;
                                  CCxCrop,FshapeWeed,fCCx : double;
                                  Yeari,MWeedAdj : ShortInt;
                                  VAR RCadj : ShortInt) : double;

PROCEDURE AdjustYearPerennials(TheYearSeason: ShortInt;
                               Sown1stYear : BOOLEAN;
                               TheCycleMode : rep_modeCycle;
                               Zmax,ZminYear1,TheCCo,TheSizeSeedling,
                               TheCGC,TheCCx,TheGDDCGC : double;
                               ThePlantingDens : LongInt;
                               VAR TypeOfPlanting : rep_Planting;
                               VAR Zmin,TheSizePlant,TheCCini : double;
                               VAR TheDaysToCCini,TheGDDaysToCCini : INTEGER);





PROCEDURE NoCropCalendar;
PROCEDURE LoadCropCalendar(FullName : string;
                           VAR GetOnset,GetOnsetTemp : BOOLEAN;
                           VAR DayNrStart : LongInt;
                           YearStart : INTEGER);
PROCEDURE GetFileForProgramParameters(TheFullFileNameProgram : string;
                                      VAR FullFileNameProgramParameters : string);
PROCEDURE LoadProgramParametersProject(FullFileNameProgramParameters : string);

implementation


FUNCTION ActualRootingDepth(DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234 : INTEGER;
                            SumGDD,Zmin,Zmax : double;
                            ShapeFactor : ShortInt;
                            TypeDays : rep_modeCycle) : double;
Var Zini, Zr : double;
    VirtualDay, T0 : INTEGER;



FUNCTION ActualRootingDepthDays(DAP,L0,LZmax,L1234 : INTEGER;
                                Zmin,Zmax : double) : double;
BEGIN // Actual rooting depth at the end of Dayi
VirtualDay := DAP - Simulation.DelayedDays;
IF ((VirtualDay < 1) OR (VirtualDay > L1234))
   THEN ActualRootingDepthDays := 0
   ELSE IF (VirtualDay >= LZmax)
        THEN ActualRootingDepthDays := Zmax
        ELSE IF (Zmin < Zmax)
                THEN BEGIN
                     Zini := ZMin * (SimulParam.RootPercentZmin/100);
                     T0 := ROUND(L0/2);
                     IF (LZmax <= T0)
                        THEN Zr := Zini + (Zmax-Zini)*VirtualDay/LZmax
                        ELSE IF (VirtualDay <= T0)
                                THEN Zr := Zini
                                ELSE BEGIN
                                     Zr := Zini + (Zmax-Zini)
                                        * TimeRootFunction(VirtualDay,ShapeFactor,LZmax,T0);
                                     END;
                     IF (Zr > ZMin) THEN ActualRootingDepthDays := Zr
                                    ELSE ActualRootingDepthDays := ZMin;
                     END
                ELSE ActualRootingDepthDays := ZMax;
END; (* ActualRootingDepthDays *)



FUNCTION ActualRootingDepthGDDays(DAP,L1234,GDDL0,GDDLZmax,GDDL1234 : INTEGER;
                                  SumGDD,Zmin,Zmax : double) : double;
VAR GDDT0 : double;
BEGIN // after sowing the crop has roots even when SumGDD = 0
VirtualDay := DAP - Simulation.DelayedDays;
IF ((VirtualDay < 1) OR (VirtualDay > L1234))
   THEN ActualRootingDepthGDDays := 0
   ELSE IF (SumGDD >= GDDLZmax)
        THEN ActualRootingDepthGDDays := Zmax
        ELSE IF (Zmin < Zmax)
                THEN BEGIN
                     Zini := ZMin * (SimulParam.RootPercentZmin/100);
                     GDDT0 := GDDL0/2;
                     IF (GDDLZmax <= GDDT0)
                        THEN Zr := Zini + (Zmax-Zini)*SumGDD/GDDLZmax
                        ELSE BEGIN
                             IF (SumGDD <= GDDT0)
                                THEN Zr := Zini
                                ELSE BEGIN
                                     Zr := Zini + (Zmax-Zini)
                                        * TimeRootFunction(SumGDD,ShapeFactor,GDDLZmax,GDDT0);
                                     END;
                             END;
                     IF (Zr > ZMin) THEN ActualRootingDepthGDDays := Zr
                                    ELSE ActualRootingDepthGDDays := ZMin;
                     END
                ELSE ActualRootingDepthGDDays := ZMax;
END; (* ActualRootingDepthGDDays *)



BEGIN  (* ActualRootingDepth *)
CASE TypeDays OF
     GDDays  : Zr := ActualRootingDepthGDDays(DAP,L1234,GDDL0,GDDLZmax,GDDL1234,SumGDD,Zmin,Zmax);
     else Zr := ActualRootingDepthDays(DAP,L0,LZmax,L1234,Zmin,Zmax);
     end;
// restrictive soil layer
Simulation.SCor := 1;
IF (ROUND(Soil.RootMax*1000) < ROUND(Zmax*1000))
   THEN ZrAdjustedToRestrictiveLayers(Zr,Soil.NrSoilLayers,SoilLayer,Zr);
// assign
ActualRootingDepth:= Zr;
END; (* ActualRootingDepth *)




PROCEDURE CalculateETpot(DAP,L0,L12,L123,LHarvest,DayLastCut : INTEGER;
                         CCi,EToVal,KcVal,KcDeclineVal,CCx,CCxWithered,CCeffectProcent,CO2i,GDDayi,TempGDtranspLow : double;
                         VAR TpotVal, EpotVal : double);
VAR EpotMin, EpotMax,CCiAdjusted,Multiplier,KsTrCold : double;
    VirtualDay : INTEGER;

BEGIN (* CalculateETpot*)
VirtualDay := DAP - Simulation.DelayedDays;
IF ( ((VirtualDay < L0) AND (Round(100*CCi) = 0)) OR (VirtualDay > LHarvest))   //To handlle Forage crops: Round(100*CCi) = 0
   THEN BEGIN
        TpotVal := 0;
        EpotVal := SimulParam.KcWetBare*EToVal;
        END
   ELSE BEGIN
        (* Correction for micro-advection *)
        CCiAdjusted := 1.72*CCi - 1*(CCi*CCi) + 0.30*(CCi*CCi*CCi);
        IF (CCiAdjusted < 0) THEN CCiAdjusted := 0;
        IF (CCiAdjusted > 1) THEN CCiAdjusted := 1;

        (* Correction for ageing effects - is a function of calendar days *)
        //IF (VirtualDay > (L12+5)) THEN KcVal := KcVal - (VirtualDay-(L12+5))*(KcDeclineVal/100)*CCxWithered;
        IF ((VirtualDay-DayLastCut) > (L12+5))
           THEN KcVal := KcVal - (VirtualDay-DayLastCut-(L12+5))*(KcDeclineVal/100)*CCxWithered;

        (* Correction for elevated atmospheric CO2 concentration *)
        IF (CO2i > 369.41) THEN KcVal := KcVal * (1 - 0.05 * (CO2i-369.41)/(550-369.41));

        (* Correction for Air temperature stress *)
        IF ((CCiAdjusted <= 0.0000001) OR (ROUND(GDDayi) < 0))
           THEN KsTrCold := 1
           ELSE KsTrCold := KsTemperature((0),TempGDtranspLow,GDDayi);

        (* First estimate of Epot and Tpot *)
        TpotVal := CCiAdjusted * KsTrCold * KcVal * EToVal;
        EpotVal := SimulParam.KcWetBare * (1 - CCiAdjusted) * EToVal;

        (* Maximum Epot with withered canopy as a result of (early) senescence*)
        EpotMax := SimulParam.KcWetBare * EToVal * (1 - CCxWithered * CCEffectProcent/100);

        (* Correction Epot for dying crop in late-season stage *)
        IF ((VirtualDay > L123) AND (CCx > 0)) THEN
           BEGIN
           IF (CCi > (CCx/2))
              THEN BEGIN (* not yet full effect *)
                   IF (CCi > CCx)
                      THEN Multiplier := 0  // no effect
                      ELSE Multiplier := (CCx-CCi)/(CCx/2);
                   END
              ELSE Multiplier := 1; // full effect
           EpotVal := EpotVal * (1 - CCx * (CCEffectProcent/100) * Multiplier);
           EpotMin := SimulParam.KcWetBare * (1 - 1.72*CCx + 1*(CCx*CCx) - 0.30*(CCx*CCx*CCx)) * EToVal;
           IF (EpotMin < 0) THEN EpotMin := 0;
           IF (EpotVal < EpotMin) THEN EpotVal := EpotMin;
           IF (EpotVal > EpotMax) THEN EpotVal := EpotMax;
           END;

        (* Correction for canopy senescence before late-season stage *)
        IF Simulation.EvapLimitON THEN IF (EpotVal > EpotMax) THEN EpotVal := EpotMax;

        (* Correction for drop in photosynthetic capacity of a dying green canopy *)
        IF (CCi < CCxWithered) THEN
           BEGIN
           IF (CCxWithered > 0.01) AND (CCi > 0.001)
              THEN TpotVal := TpotVal * Exp(SimulParam.ExpFsen*Ln(CCi/CCxWithered));
           END;
        END;
END; (* CalculateETpot *)



PROCEDURE GlobalZero(VAR SumWabal : rep_sum);
VAR i : INTEGER;
BEGIN
WITH SumWabal DO
  BEGIN
  Epot := 0.0;
  Tpot := 0.0;
  Rain := 0.0;
  Irrigation := 0.0;
  Infiltrated := 0.0;
  Runoff := 0.0;
  Drain := 0.0;
  Eact := 0.0;
  Tact := 0.0;
  TrW := 0.0;
  ECropCycle := 0.0;
  Biomass := 0;
  BiomassPot := 0;
  BiomassUnlim := 0;
  BiomassTot := 0; // crop and weeds (for soil fertility stress)
  YieldPart := 0;
  SaltIn := 0;
  SaltOut := 0;
  CRwater := 0;
  CRsalt := 0;
  END;
TotalWaterContent.BeginDay := 0;
FOR i :=1 to NrCompartments DO
    TotalWaterContent.BeginDay := TotalWaterContent.BeginDay
      + Compartment[i].theta*1000*Compartment[i].Thickness;
END; (* GlobalZero *)




PROCEDURE TimeToMaxCanopySF(CCo,CGC,CCx : double;
                            L0,L12,L123,LToFlor,LFlor : INTEGER;
                            DeterminantCrop : BOOLEAN;
                            VAR L12SF : INTEGER;
                            VAR RedCGC,RedCCx : ShortInt;
                            VAR ClassSF : ShortInt);
VAR CCToReach : double;
    L12SFmax : INTEGER;
BEGIN
IF ((ClassSF = 0) OR ((RedCCx = 0) AND (RedCGC = 0)))
   THEN L12SF := L12
   ELSE BEGIN
        CCToReach := 0.98*(1-RedCCX/100)*CCx;
        L12SF := DaysToReachCCwithGivenCGC(CCToReach,CCo,((1-RedCCX/100)*CCx),(CGC*(1-(RedCGC)/100)),L0);
        // determine L12SFmax
        IF DeterminantCrop
           THEN L12SFmax := LToFlor + ROUND(LFlor/2)
           ELSE L12SFmax := L123;
        // check for L12SFmax
        IF (L12SF > L12SFmax) THEN // full canopy cannot be reached in potential period for vegetative growth
           BEGIN
           //ClassSF := undef_int; // swithc to user defined soil fertility
           //1. increase CGC(soil fertility)
           WHILE ((L12SF > L12SFmax) AND (RedCGC > 0)) DO
              BEGIN
              RedCGC := RedCGC - 1;
              L12SF := DaysToReachCCwithGivenCGC(CCToReach,CCo,((1-RedCCX/100)*CCx),(CGC*(1-(RedCGC)/100)),L0);
              END;
           //2. if not sufficient decrease CCx(soil fertility)
           WHILE ((L12SF > L12SFmax) AND ( ((1-RedCCX/100)*CCx) > 0.10) AND (RedCCx <= 50)) DO
              BEGIN
              RedCCx := RedCCx + 1;
              CCToReach := 0.98*(1-RedCCX/100)*CCx;
              L12SF := DaysToReachCCwithGivenCGC(CCToReach,CCo,((1-RedCCX/100)*CCx),(CGC*(1-(RedCGC)/100)),L0);
              END;
           END;
        END;
END; (* TimeToMaxCanopySF *)



PROCEDURE NoManagement;
BEGIN
ManDescription := 'No specific field management';
WITH Management DO
   BEGIN
   // mulches
   Mulch := 0;
   EffectMulchInS := 50;
   // soil fertility
   FertilityStress := 0;
   CropStressParametersSoilFertility(Crop.StressResponse,FertilityStress,Simulation.EffectStress);
   // soil bunds
   BundHeight := 0;
   Simulation.SurfaceStorageIni := 0.0;
   Simulation.ECStorageIni := 0.0;
   // surface run-off
   RunoffOn := true;
   CNcorrection := 0;
   // weed infestation
   WeedRC := 0;
   WeedDeltaRC := 0;
   WeedShape := - 0.01;
   WeedAdj := 100;
   // multiple cuttings
   Cuttings.Considered := false;
   Cuttings.CCcut := 30;
   Cuttings.CGCPlus := 20;
   Cuttings.Day1 := 1;
   Cuttings.NrDays := undef_int;
   Cuttings.Generate := false;
   Cuttings.Criterion := NA;
   Cuttings.HarvestEnd := false;
   Cuttings.FirstDayNr := undef_int;
   END;
END; (* NoManagement *)


PROCEDURE LoadManagement(FullName : string);
VAR f0 : TextFile;
    i  : ShortInt;
    VersionNr : double;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,ManDescription);
READLN(f0,VersionNr); // AquaCrop Version
WITH Management DO
  BEGIN
  // mulches
  READLN(f0,Mulch);
  READLN(f0,EffectMulchInS);
  // soil fertility
  READLN(f0,FertilityStress); // effect is crop specific
  CropStressParametersSoilFertility(Crop.StressResponse,FertilityStress,Simulation.EffectStress);
  // soil bunds
  READLN(f0,BundHeight);
  Simulation.SurfaceStorageIni := 0.0;
  Simulation.ECStorageIni := 0.0;
  // surface run-off
  READLN(f0,i);
  IF (i = 1)
     THEN RunoffON := false   // prevention of surface runoff
     ELSE RunoffON := true;   // surface runoff is not prevented
  IF (ROUND(VersionNr*10) < 50) // UPDATE required for CN adjustment
     THEN CNcorrection := 0
     ELSE READLN(f0,CNcorrection); // project increase/decrease of CN
  // weed infestation
  IF (ROUND(VersionNr*10) < 50)  // UPDATE required for Version 3.0, 3.1 and 4.0
     THEN BEGIN
          WeedRC := 0; // relative cover of weeds (%)
          WeedDeltaRC := 0;
          WeedShape := -0.01; //shape factor of the CC expansion fucntion in a weed infested field
          END
     ELSE BEGIN
          READLN(f0,WeedRC); //relative cover of weeds (%)
          IF (ROUND(VersionNr*10) < 51)
             THEN WeedDeltaRC := 0
             ELSE READLN(f0,WeedDeltaRC);
          READLN(f0,WeedShape); //shape factor of the CC expansion fucntion in a weed infested field
          END;
  IF (ROUND(VersionNr*10) < 70)  // UPDATE required for versions below 7
     THEN WeedAdj := 100 // replacement (%) by weeds of the self-thinned part of the Canopy Cover - only for perennials
     ELSE READLN(f0,WeedAdj);
  // multiple cuttings
  IF (ROUND(VersionNr*10) >= 70)  // UPDATE required for multiple cuttings
     THEN BEGIN
          READLN(f0,i);  // Consider multiple cuttings: True or False
          IF (i = 0)
             THEN Cuttings.Considered := false
             ELSE Cuttings.Considered := true;
          READLN(f0,Cuttings.CCcut);  // Canopy cover (%) after cutting
          READLN(f0,Cuttings.CGCPlus); // Increase (percentage) of CGC after cutting
          READLN(f0,Cuttings.Day1);  // Considered first day when generating cuttings (1 = start of growth cycle)
          READLN(f0,Cuttings.NrDays);  // Considered number owhen generating cuttings (-9 = total growth cycle)
          READLN(f0,i);  // Generate multiple cuttings: True or False
          IF (i = 1)
             THEN Cuttings.Generate := true
             ELSE Cuttings.Generate := false;
          READLN(f0,i);  // Time criterion for generating cuttings
          CASE i OF
             0  : Cuttings.Criterion := NA; // not applicable
             1  : Cuttings.Criterion := IntDay; // interval in days
             2  : Cuttings.Criterion := IntGDD; // interval in Growing Degree Days
             3  : Cuttings.Criterion := DryB; // produced dry above ground biomass (ton/ha)
             4  : Cuttings.Criterion := DryY; // produced dry yield (ton/ha)
             5  : Cuttings.Criterion := FreshY; // produced fresh yield (ton/ha)
            else  Cuttings.Criterion := NA; // not applicable
            end;
          READLN(f0,i);  // final harvest at crop maturity: True or False (When generating cuttings)
          IF (i = 1)
             THEN Cuttings.HarvestEnd := true
             ELSE Cuttings.HarvestEnd := false;
          READLN(f0,Cuttings.FirstDayNr); // dayNr for Day 1 of list of cuttings (-9 = Day1 is start growing cycle)
          END
     ELSE BEGIN
          Cuttings.Considered := false;
          Cuttings.CCcut := 30;
          Cuttings.CGCPlus := 20;
          Cuttings.Day1 := 1;
          Cuttings.NrDays := undef_int;
          Cuttings.Generate := false;
          Cuttings.Criterion := NA;
          Cuttings.HarvestEnd := false;
          Cuttings.FirstDayNr := undef_int;
          END;
  END;
Close(f0);
END; (* LoadManagement *)






PROCEDURE NoIrrigation;
VAR Nri : INTEGER;
BEGIN
 IrriMode := NoIrri;
 IrriDescription := 'Rainfed cropping';
 IrriMethod := MSprinkler;
 Simulation.IrriECw := 0.0; // dS/m
 GenerateTimeMode := AllRAW;
 GenerateDepthMode := ToFC;
 IrriFirstDayNr := undef_int;
 FOR Nri := 1 TO 5 DO
     BEGIN
     IrriBeforeSeason[Nri].DayNr := 0;
     IrriBeforeSeason[Nri].Param := 0;
     IrriAfterSeason[Nri].DayNr := 0;
     IrriAfterSeason[Nri].Param := 0;
     END;
 SetIrriECw_PreSeason(0.0); //dS/m
 SetIrriECw_PostSeason(0.0); //dS/m
END; (* NoIrrigation *)


PROCEDURE NoManagementOffSeason;
VAR Nri : INTEGER;
BEGIN
OffSeasonDescription := 'No specific off-season conditions';
WITH Management DO
   BEGIN
   // mulches
   SoilCoverBefore := 0;
   SoilCoverAfter := 0;
   EffectMulchOffS := 50;
   // off-season irrigation
   SimulParam.IrriFwOffSeason := 100;
   SetIrriECw_PreSeason(0.0); // dS/m
   FOR Nri := 1 TO 5 DO
       BEGIN
       IrriBeforeSeason[Nri].DayNr := 0;
       IrriBeforeSeason[Nri].Param := 0;
       END;
   SetIrriECw_PostSeason(0.0); // dS/m
   FOR Nri := 1 TO 5 DO
       BEGIN
       IrriAfterSeason[Nri].DayNr := 0;
       IrriAfterSeason[Nri].Param := 0;
       END;
   END;
END; (* NoManagementOffSeason *)



PROCEDURE LoadOffSeason(FullName : string);
VAR f0 : TextFile;
    Nri,NrEvents1,NrEvents2 : INTEGER;
    ParamString : string;
    Par1,Par2 : double;
    VersionNr : double;
    PreSeason_in : double;
    PostSeason_in : double;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,OffSeasonDescription);
READLN(f0,VersionNr); // AquaCrop Version
// mulches
WITH Management DO
  BEGIN
  READLN(f0,SoilCoverBefore);
  READLN(f0,SoilCoverAfter);
  READLN(f0,EffectMulchOffS);
  END;
// irrigation events - initialise
FOR Nri := 1 TO 5 DO
    BEGIN
    IrriBeforeSeason[Nri].DayNr := 0;
    IrriBeforeSeason[Nri].Param := 0;
    IrriAfterSeason[Nri].DayNr := 0;
    IrriAfterSeason[Nri].Param := 0;
    END;
READLN(f0,NrEvents1); //number of irrigation events BEFORE growing period
IF (ROUND(10*VersionNr) < 32) // irrigation water quality BEFORE growing period
   THEN SetIrriECw_PreSeason(0.0)
   ELSE BEGIN
    READLN(f0,PreSeason_in);
    SetIrriECw_PreSeason(PreSeason_in);
    END;
READLN(f0,NrEvents2); //number of irrigation events AFTER growing period
IF (ROUND(10*VersionNr) < 32) // irrigation water quality AFTER growing period
   THEN SetIrriECw_PostSeason(0.0)
   ELSE BEGIN
    READLN(f0,PostSeason_in);
    SetIrriECw_PostSeason(PostSeason_in);
    END;
READLN(f0,SimulParam.IrriFwOffSeason); // percentage of soil surface wetted
// irrigation events - get events before and after season
IF (NrEvents1 > 0) OR (NrEvents2 > 0) THEN FOR Nri := 1 TO 3 DO READLN(f0); // title
IF (NrEvents1 > 0) THEN FOR Nri := 1 TO NrEvents1 DO // events BEFORE growing period
   BEGIN
   READLN(f0,ParamString);
   SplitStringInTwoParams(ParamString,Par1,Par2);
   IrriBeforeSeason[Nri].DayNr := ROUND(Par1);
   IrriBeforeSeason[Nri].Param := ROUND(Par2);
   END;
IF (NrEvents2 > 0) THEN FOR Nri := 1 TO NrEvents2 DO // events AFTER growing period
   BEGIN
   READLN(f0,ParamString);
   SplitStringInTwoParams(ParamString,Par1,Par2);
   IrriAfterSeason[Nri].DayNr := ROUND(Par1);
   IrriAfterSeason[Nri].Param := ROUND(Par2);
   END;
Close(f0);
END; (* LoadOffSeason *)


PROCEDURE LoadIrriScheduleInfo(FullName : string);
VAR f0 : TextFile;
    i : INTEGER;
    VersionNr : double;

BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,IrriDescription);
READLN(f0,VersionNr);  // AquaCrop version

// irrigation method
READLN(f0,i);
CASE i OF
     1 : IrriMethod := MSprinkler;
     2 : IrriMethod := MBasin;
     3 : IrriMethod := MBorder;
     4 : IrriMethod := MFurrow;
     else  IrriMethod := MDrip;
     end;

// fraction of soil surface wetted
READLN(f0,SimulParam.IrriFwInSeason);

// irrigation mode and parameters
READLN(f0,i);
CASE i OF
     0 : IrriMode := NoIrri; // rainfed
     1 : IrriMode := Manual;
     2 : IrriMode := Generate;
     else IrriMode := Inet;
     end;

// 1. Irrigation schedule
IF ((i = 1) AND (ROUND(VersionNr*10) >= 70))
   THEN READLN(f0,IrriFirstDayNr) // line 6
   ELSE IrriFirstDayNr := undef_int; // start of growing period


// 2. Generate
IF (IrriMode = Generate) THEN
   BEGIN
   READLN(f0,i); // time criterion
   Case i OF
        1 : GenerateTimeMode := FixInt;
        2 : GenerateTimeMode := AllDepl;
        3 : GenerateTimeMode := AllRAW;
        4 : GenerateTimeMode := WaterBetweenBunds;
        else GenerateTimeMode := AllRAW;
     end;
   READLN(f0,i); // depth criterion
   Case i OF
        1 : GenerateDepthMode := ToFc;
        else GenerateDepthMode := FixDepth;
     end;
   IrriFirstDayNr := undef_int; // start of growing period
   END;

// 3. Net irrigation requirement
IF (IrriMode = Inet) THEN
   BEGIN
   READLN(f0,SimulParam.PercRAW);
   IrriFirstDayNr := undef_int;  // start of growing period
   END;

Close(f0);
END; (* LoadIrriScheduleInfo *)


PROCEDURE DetermineNrandThicknessCompartments;
VAR TotalDepthL, TotalDepthC, DeltaZ : double;
    i : INTEGER;
BEGIN
TotalDepthL := 0;
FOR i := 1 TO Soil.NrSoilLayers DO TotalDepthL := TotalDepthL + SoilLayer[i].Thickness;
TotalDepthC := 0;
NrCompartments := 0;
REPEAT
  DeltaZ := (TotalDepthL - TotalDepthC);
  NrCompartments := NrCompartments + 1;
  IF (DeltaZ > SimulParam.CompDefThick)
     THEN Compartment[NrCompartments].Thickness := SimulParam.CompDefThick
     ELSE Compartment[NrCompartments].Thickness := DeltaZ;
  TotalDepthC := TotalDepthC + Compartment[NrCompartments].Thickness;
UNTIL ((NrCompartments = max_No_compartments) OR (Abs(TotalDepthC - TotalDepthL) < 0.0001));
END; (* DetermineNrandThicknessCompartments *)


PROCEDURE CalculateAdjustedFC(DepthAquifer : double;
                              VAR CompartAdj   : rep_Comp);
VAR compi,ic : INTEGER;
    Zi,Depth,DeltaV,DeltaFC,Xmax : double;

    FUNCTION NoAdjustment(FCvolPr : Double) : double;
    VAR pF : double;
    BEGIN
    IF (FCvolPr <= 10)
       THEN NoAdjustment := 1
       ELSE BEGIN
            IF (FCvolPr >= 30)
               THEN NoAdjustment := 2
               ELSE BEGIN
                    pF := 2 + 0.3 * (FCvolPr-10)/20;
                    NoAdjustment := (exp(pF*ln(10)))/100;
                    END;
            END;
    END; (* NoAdjustment *)

BEGIN
(*
Depth := 0;
FOR compi := 1 TO NrCompartments DO
    BEGIN
    Depth := Depth + CompartAdj[compi].Thickness;
    Zi := Depth - CompartAdj[compi].Thickness/2;
    IF ((DepthAquifer < 0)
        OR ((DepthAquifer - Zi) >= 2)
        OR (SoilLayer[CompartAdj[compi].Layer].FC >= SoilLayer[CompartAdj[compi].Layer].SAT))
           THEN CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC
           ELSE BEGIN
                IF (Zi >= DepthAquifer)
                   THEN CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].SAT
                   ELSE BEGIN
                        DeltaV := SoilLayer[CompartAdj[compi].Layer].SAT - SoilLayer[CompartAdj[compi].Layer].FC;
                        DeltaFC := (DeltaV/4) * (Zi - (DepthAquifer - 2)) * (Zi - (DepthAquifer - 2));
                        CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC + DeltaFC;
                        END;
                END;
    END;  *)


Depth := 0;
FOR compi := 1 TO NrCompartments DO Depth := Depth + CompartAdj[compi].Thickness;
compi := NrCompartments;
REPEAT
  Zi := Depth - CompartAdj[compi].Thickness/2;
  //Xmax := NoAdjustment(SoilLayer[CompartAdj[compi].Layer].SoilClass);
  Xmax := NoAdjustment(SoilLayer[CompartAdj[compi].Layer].FC);
  IF ((DepthAquifer < 0) OR ((DepthAquifer - Zi) >= Xmax))
      THEN BEGIN
           FOR ic := 1 to compi DO CompartAdj[ic].FCadj := SoilLayer[CompartAdj[ic].Layer].FC;
           compi := 0;
           END
      ELSE BEGIN
           IF (SoilLayer[CompartAdj[compi].Layer].FC >= SoilLayer[CompartAdj[compi].Layer].SAT)
              THEN CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC
              ELSE BEGIN
                   IF (Zi >= DepthAquifer)
                      THEN CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].SAT
                      ELSE BEGIN
                           DeltaV := SoilLayer[CompartAdj[compi].Layer].SAT - SoilLayer[CompartAdj[compi].Layer].FC;
                           DeltaFC := (DeltaV/Sqr(Xmax)) * Sqr(Zi - (DepthAquifer - Xmax));
                           CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC + DeltaFC;
                           END;
                   END;
           Depth := Depth - CompartAdj[compi].Thickness;
           compi := compi - 1;
           END;
UNTIL (compi < 1);
END; (*  CalculateAdjustedFC *)


PROCEDURE DesignateSoilLayerToCompartments(NrCompartments,NrSoilLayers : INTEGER;
                                          VAR Compartment : rep_Comp);
VAR i, layeri, compi : INTEGER;
    depth, depthi : double;
    finished, NextLayer : BOOLEAN;
BEGIN
depth := 0;
depthi := 0;
layeri := 1;
compi := 1;
REPEAT
  depth := depth + SoilLayer[layeri].Thickness;
  REPEAT
    depthi := depthi + Compartment[compi].Thickness/2;
    IF (depthi <= depth)
       THEN BEGIN
            Compartment[compi].Layer := layeri;
            NextLayer := false;
            depthi := depthi + Compartment[compi].Thickness/2;
            compi := compi + 1;
            finished := (compi > NrCompartments);
            END
       ELSE BEGIN
            depthi := depthi - Compartment[compi].Thickness/2;
            NextLayer := true;
            layeri := layeri + 1;
            finished := (layeri > NrSoilLayers);
            END;
  UNTIL finished or NextLayer;
UNTIL finished;
FOR i := compi to NrCompartments DO Compartment[i].Layer := NrSoilLayers;
FOR i := (NrCompartments+1) TO max_No_compartments DO Compartment[i].Thickness := undef_double;
END; (* DesignateSoilLayerToCompartments *)


PROCEDURE DeclareInitialCondAtFCandNoSalt;
VAR layeri,compi,celli : INTEGER;
BEGIN
SetSWCiniFile('(None)');
SWCiniFileFull := GetSWCiniFile(); (* no file *)
SWCiniDescription := 'Soil water profile at Field Capacity';
Simulation.IniSWC.AtDepths := false;
Simulation.IniSWC.NrLoc := Soil.NrSoilLayers;
FOR layeri := 1 TO Soil.NrSoilLayers DO
    BEGIN
    Simulation.IniSWC.Loc[layeri] := SoilLayer[layeri].Thickness;
    Simulation.IniSWC.VolProc[layeri] := SoilLayer[layeri].FC;
    Simulation.IniSWC.SaltECe[layeri] := 0;
    END;
Simulation.IniSWC.AtFC := true;
FOR layeri := (Soil.NrSoilLayers+1) TO max_No_compartments DO
    BEGIN
    Simulation.IniSWC.Loc[layeri] := undef_double;
    Simulation.IniSWC.VolProc[layeri] := undef_double;
    Simulation.IniSWC.SaltECe[layeri] := undef_double;
    END;
FOR compi := 1 TO NrCompartments DO
    For celli := 1 TO SoilLayer[Compartment[compi].Layer].SCP1 DO
        BEGIN // salinity in cells
        Compartment[compi].Salt[celli] := 0.0;
        Compartment[compi].Depo[celli] := 0.0;
        END;
END;(* DeclareInitialCondAtFCandNoSalt *)





PROCEDURE specify_soil_layer(NrCompartments,NrSoilLayers : INTEGER;
                             VAR SoilLayer : rep_SoilLayer;
                             VAR Compartment : rep_Comp;
                             VAR TotalWaterContent : rep_Content);
VAR layeri, compi, celli : INTEGER;
    Total : double;

BEGIN
DesignateSoilLayerToCompartments(NrCompartments,NrSoilLayers,Compartment);

// Set soil layers and compartments at Field Capacity and determine Watercontent (mm)
// No salinity in soil layers and compartmens
// Absence of ground water table (FCadj = FC)
Total := 0;
FOR layeri := 1 TO NrSoilLayers DO SoilLayer[layeri].WaterContent := 0;
FOR compi := 1 TO NrCompartments DO
    BEGIN
    Compartment[compi].Theta := SoilLayer[Compartment[compi].Layer].FC/100;
    Compartment[compi].FCadj := SoilLayer[Compartment[compi].Layer].FC;
    Compartment[compi].DayAnaero := 0;
    For celli := 1 TO SoilLayer[Compartment[compi].Layer].SCP1 DO
        BEGIN // salinity in cells
        Compartment[compi].Salt[celli] := 0.0;
        Compartment[compi].Depo[celli] := 0.0;
        END;
    Simulation.ThetaIni[compi] := Compartment[compi].Theta;
    Simulation.ECeIni[compi] := 0; // initial soil salinity in dS/m
    SoilLayer[Compartment[compi].Layer].WaterContent
     := SoilLayer[Compartment[compi].Layer].WaterContent
        + Simulation.ThetaIni[compi]*100*10*Compartment[compi].Thickness;
    END;
FOR layeri := 1 TO NrSoilLayers DO Total := Total + SoilLayer[layeri].WaterContent;
TotalWaterContent.BeginDay := Total;

// initial soil water content and no salts
DeclareInitialCondAtFCandNoSalt;

// Number of days with RootZone Anaerobic Conditions
Simulation.DayAnaero := 0;

END; (* specify_soil_layer *)

PROCEDURE DetermineParametersCR(SoilClass : ShortInt;
                                KsatMM : double;
                                VAR aParam, bParam : double);
BEGIN
// determine parameters
IF (ROUND(KsatMM*1000) <= 0)
   THEN BEGIN
        aParam := undef_int;
        bParam := undef_int;
        END
   ELSE CASE SoilClass OF
             1 : BEGIN  // sandy soils
                 aParam := -0.3112 - KsatMM/100000;
                 bParam := -1.4936 + 0.2416*LN(KsatMM);
                 END;
             2 : BEGIN // loamy soils
                 aParam := -0.4986 + 9*KsatMM/100000;
                 bParam := -2.1320 + 0.4778*LN(KsatMM);
                 END;
             3 : BEGIN // sandy clayey soils
                 aParam := -0.5677 - 4*KsatMM/100000;
                 bParam := -3.7189 + 0.5922*LN(KsatMM);
                 END;
            else BEGIN // silty clayey soils
                 aParam := -0.6366 + 8*KsatMM/10000;
                 bParam := -1.9165 + 0.7063*LN(KsatMM);
                 END;
            end;
END; (* DetermineParametersCR *)



FUNCTION ActiveCells(Comp : CompartmentIndividual) : INTEGER;
VAR  celi : INTEGER;

BEGIN
IF (Comp.theta <= SoilLayer[Comp.Layer].UL)
   THEN BEGIN
        celi := 0;
        WHILE (Comp.theta > (SoilLayer[Comp.Layer].Dx) * celi) DO celi := celi + 1;
        END
   ELSE celi := SoilLayer[Comp.Layer].SCP1;
ActiveCells := celi;
END; (* ActiveCells *)


PROCEDURE Calculate_Saltmobility(layer : INTEGER;
                                 SaltDiffusion : ShortInt;  // percentage
                                 Macro : ShortInt;
                                 VAR Mobil : rep_salt);
VAR i, CelMax : INTEGER;
    Mix, a, b, xi, yi, UL : double;

BEGIN
Mix := SaltDiffusion/100; // global salt mobility expressed as a fraction
UL := SoilLayer[layer].UL * 100; (* upper limit in VOL% of SC cell *)

//1. convert Macro (vol%) in SaltCelNumber
IF (Macro > UL)
   THEN CelMax := SoilLayer[layer].SCP1
   ELSE CelMax := ROUND((Macro/UL)*SoilLayer[layer].SC);
IF (CelMax <= 0) THEN CelMax := 1;

//2. find a and b
IF (Mix < 0.5)
   THEN BEGIN
        a := Mix * 2;
        b := EXP(10*(0.5-Mix)*LN(10));
        END
   ELSE BEGIN
        a := 2 * (1- Mix);
        b := EXP(10*(Mix-0.5)*LN(10));
        END;

//3. calculate mobility for cells = 1 to Macro
FOR i := 1 to (CelMax-1) DO
    BEGIN
    xi := i/(CelMax-1);
    IF (Mix > 0)
       THEN IF (Mix < 0.5)
               THEN BEGIN
                    yi := EXP(LN(a)+xi*LN(b));
                    Mobil[i] := (yi-a)/(a*b-a);
                    END
               ELSE IF (Mix = 0.5)
                       THEN Mobil[i] := xi
                       ELSE IF (Mix < 1)
                               THEN BEGIN
                                    yi := EXP(LN(a)+(1-xi)*LN(b));
                                    Mobil[i] := 1- (yi-a)/(a*b-a);
                                    END
                               ELSE Mobil[i] := 1
       ELSE Mobil[i] := 0;
    END;

//4. Saltmobility between Macro and SAT
FOR i := CelMax TO SoilLayer[layer].SCP1 DO Mobil[i] := 1;

END; (* Calculate_Saltmobility *)



FUNCTION ECeComp (Comp : CompartmentIndividual) : double;
VAR volSat, TotSalt : double;
    i : INTEGER;
BEGIN
volSAT := (SoilLayer[Comp.Layer].SAT);
TotSalt := 0;
FOR i := 1 TO SoilLayer[Comp.Layer].SCP1 DO TotSalt := TotSalt + Comp.Salt[i] + Comp.Depo[i]; //g/m2
TotSalt := TotSalt/
        (volSAT*10*Comp.Thickness*(1-SoilLayer[Comp.Layer].GravelVol/100)); // g/l
IF (TotSalt > SimulParam.SaltSolub) THEN TotSalt := SimulParam.SaltSolub;
ECeComp := TotSalt/Equiv; //dS/m
END; (* ECeComp *)



FUNCTION ECswComp (Comp : CompartmentIndividual;
                   atFC : BOOLEAN) : double;
VAR TotSalt : double;
    i : INTEGER;

BEGIN
TotSalt := 0;
FOR i := 1 TO SoilLayer[Comp.Layer].SCP1 DO TotSalt := TotSalt + Comp.Salt[i] + Comp.Depo[i]; // g/m2
IF (atFC = true)
   THEN TotSalt := TotSalt/
                (SoilLayer[Comp.Layer].FC*10*Comp.Thickness*(1-SoilLayer[Comp.Layer].GravelVol/100)) // g/l
   ELSE TotSalt := TotSalt/
                (Comp.theta*1000*Comp.Thickness*(1-SoilLayer[Comp.Layer].GravelVol/100)); // g/l
IF (TotSalt > SimulParam.SaltSolub) THEN TotSalt := SimulParam.SaltSolub;
ECswComp := TotSalt/Equiv;
END; (* ECswComp *)




PROCEDURE SaltSolutionDeposit(mm : double; (* mm = l/m2 *)
                      VAR SaltSolution,SaltDeposit : double); (* g/m2 *)
BEGIN
SaltSolution := SaltSolution + SaltDeposit;
IF (SaltSolution > SimulParam.SaltSolub * mm)
    THEN BEGIN
         SaltDeposit := SaltSolution - SimulParam.SaltSolub * mm;
         SaltSolution := SimulParam.SaltSolub * mm;
         END
    ELSE SaltDeposit := 0;
END; (* SaltSolutionDeposit *)




PROCEDURE DetermineSaltContent(ECe : double;
                               VAR Comp : CompartmentIndividual);
VAR TotSalt, SumDF, SAT, UL, Dx, mm, mm1, mmN : double;
    celn, i : INTEGER;

BEGIN
TotSalt := ECe*Equiv*(SoilLayer[Comp.Layer].SAT)*10*Comp.Thickness;
celn := ActiveCells(Comp);
SAT := (SoilLayer[Comp.Layer].SAT)/100;  (* m3/m3 *)
UL := SoilLayer[Comp.Layer].UL; (* m3/m3 *)  (* Upper limit of SC salt cel *)
Dx := SoilLayer[Comp.Layer].Dx;  (* m3/m3 *) (* Size of salts cel (expect last one) *)
mm1 := Dx*1000*Comp.Thickness
       * (1 - SoilLayer[Comp.Layer].GravelVol/100); // g/l (* volume [mm]=[l/m2] of cells *)
mmN := (SAT-UL)*1000*Comp.Thickness
       * (1 - SoilLayer[Comp.Layer].GravelVol/100); // g/l (* volume [mm]=[l/m2] of last cell *)
SumDF := 0;
FOR i := 1 TO SoilLayer[Comp.Layer].SCP1 DO
    BEGIN
    Comp.Salt[i] := 0;
    Comp.Depo[i] := 0;
    END;
FOR i := 1 TO celn DO SumDF := SumDF + SoilLayer[Comp.Layer].SaltMobility[i];
FOR i := 1 TO celn DO
    BEGIN
    Comp.Salt[i] := TotSalt * SoilLayer[Comp.Layer].SaltMobility[i]/SumDF;
    mm := mm1;
    IF (i = SoilLayer[Comp.Layer].SCP1) THEN mm := mmN;
    SaltSolutionDeposit(mm,Comp.Salt[i],Comp.Depo[i]);
    END;
END; (* DetermineSaltContent *)




PROCEDURE CompleteProfileDescription;
VAR i : INTEGER;
BEGIN
FOR i:= (Soil.NrSoilLayers+1) to max_SoilLayers DO set_layer_undef(SoilLayer[i]);
Simulation.ResetIniSWC := true; // soil water content and soil salinity
specify_soil_layer(NrCompartments,Soil.NrSoilLayers,SoilLayer,Compartment,TotalWaterContent);
END; (* CompleteProfileDescription *)


PROCEDURE LoadProfile(FullName : string);
VAR f0 : TextFile;
    i  : INTEGER;
    blank : rep_string3;
    VersionNr : double;

BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,ProfDescription);
READLN(f0,VersionNr);  // AquaCrop version
READLN(f0,Soil.CNvalue);
READLN(f0,Soil.REW);
Simulation.SurfaceStorageIni := 0.0;
Simulation.ECStorageIni := 0.0;
READLN(f0,Soil.NrSoilLayers);
READLN(f0); // depth of restrictive soil layer which is no longer applicable
READLN(f0);
READLN(f0);
// Load characteristics of each soil layer
FOR i := 1 TO Soil.NrSoilLayers DO
    BEGIN
    // Parameters for capillary rise missing in Versions 3.0 and 3.1
    IF (ROUND(VersionNr*10) < 40)
       THEN BEGIN
            READLN(f0,SoilLayer[i].Thickness,SoilLayer[i].SAT,SoilLayer[i].FC,
              SoilLayer[i].WP,SoilLayer[i].InfRate,blank,SoilLayer[i].Description);
            // Default values for Penetrability and Gravel
            SoilLayer[i].Penetrability := 100;
            SoilLayer[i].GravelMass := 0;
            // determine volume gravel
            SoilLayer[i].GravelVol := 0;
            END
       ELSE BEGIN
            IF (ROUND(VersionNr*10) < 60)  // UPDATE required for Version 6.0
               THEN BEGIN
                    READLN(f0,SoilLayer[i].Thickness,SoilLayer[i].SAT,SoilLayer[i].FC,
                      SoilLayer[i].WP,SoilLayer[i].InfRate,SoilLayer[i].CRa,SoilLayer[i].CRb,
                      blank,SoilLayer[i].Description);
                    // Default values for Penetrability and Gravel
                    SoilLayer[i].Penetrability := 100;
                    SoilLayer[i].GravelMass := 0;
                    // determine volume gravel
                    SoilLayer[i].GravelVol := 0;
                    END
               ELSE BEGIN
                    READLN(f0,SoilLayer[i].Thickness,SoilLayer[i].SAT,SoilLayer[i].FC,
                      SoilLayer[i].WP,SoilLayer[i].InfRate,SoilLayer[i].Penetrability,
                      SoilLayer[i].GravelMass,SoilLayer[i].CRa,SoilLayer[i].CRb,
                      blank,SoilLayer[i].Description);
                    // determine volume gravel
                    SoilLayer[i].GravelVol := FromGravelMassToGravelVolume(SoilLayer[i].SAT,SoilLayer[i].GravelMass);
                    END;
            END;
    // determine drainage coefficient
    SoilLayer[i].tau := TauFromKsat(SoilLayer[i].InfRate);
    // determine number of salt cells based on infiltration rate
    IF (SoilLayer[i].InfRate <= 112)
       THEN SoilLayer[i].SCP1 := 11
       ELSE BEGIN
            SoilLayer[i].SCP1 := ROUND(1.6 + 1000/SoilLayer[i].InfRate);
            IF (SoilLayer[i].SCP1 < 2) THEN SoilLayer[i].SCP1 := 2
            END;
    // determine parameters for soil salinity
    SoilLayer[i].SC := SoilLayer[i].SCP1 -1;
    SoilLayer[i].Macro := ROUND(SoilLayer[i].FC);
    SoilLayer[i].UL := ((SoilLayer[i].SAT)/100) * (SoilLayer[i].SC/(SoilLayer[i].SC+2)); (* m3/m3 *)
    SoilLayer[i].Dx := (SoilLayer[i].UL)/SoilLayer[i].SC;  (* m3/m3 *)
    Calculate_SaltMobility(i,SimulParam.SaltDiff,SoilLayer[i].Macro,SoilLayer[i].SaltMobility);
    // determine default parameters for capillary rise if missing
    SoilLayer[i].SoilClass := NumberSoilClass(SoilLayer[i].SAT,SoilLayer[i].FC,SoilLayer[i].WP,SoilLayer[i].InfRate);
    IF (ROUND(VersionNr*10) < 40) THEN
       DetermineParametersCR(SoilLayer[i].SoilClass,SoilLayer[i].InfRate,SoilLayer[i].CRa,SoilLayer[i].CRb);
    END;
DetermineNrandThicknessCompartments;
Close(f0);
Soil.RootMax := RootMaxInSoilProfile(Crop.RootMax,Soil.NrSoilLayers,SoilLayer);
END; // Loadprofile


FUNCTION CCiniTotalFromTimeToCCini(TempDaysToCCini,TempGDDaysToCCini,
                                   L0,L12,L12SF,L123,L1234,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234 : INTEGER;
                                   CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,RatDGDD : double;
                                   SFRedCGC,SFRedCCx : ShortInt;
                                   SFCDecline,fWeed : Double;
                                   TheModeCycle : rep_modeCycle) : double;

VAR DayCC : INTEGER;
    SumGDDforCCini,TempCCini : double;
    Tadj, GDDTadj : INTEGER;
BEGIN
IF (TempDaysToCCini <> 0)
   THEN BEGIN  // regrowth
        SumGDDforCCini := undef_int;
        GDDTadj := undef_int;
        // find adjusted calendar and GDD time
        IF (TempDaysToCCini = undef_int)
             THEN BEGIN // CCx on 1st day
                  Tadj := L12 - L0;
                  IF (TheModeCycle = GDDays) THEN GDDTadj := GDDL12 - GDDL0;
                  END
             ELSE BEGIN // CC on 1st day is < CCx
                  Tadj := TempDaysToCCini;
                  IF (TheModeCycle = GDDays) THEN GDDTadj := TempGDDaysToCCini;
                  END;
        // calculate CCini with adjusted time
        DayCC := L0 + Tadj;
        IF (TheModeCycle = GDDays) THEN SumGDDforCCini := GDDL0 + GDDTadj;
        TempCCini := CCiNoWaterStressSF(DayCC,L0,L12SF,L123,L1234,
                               GDDL0,GDDL12SF,GDDL123,GDDL1234,
                               (CCo*fWeed),(CCx*fWeed),CGC,GDDCGC,
                               (CDC*(fWeed*CCx+2.29)/(CCx+2.29)),
                               (GDDCDC*(fWeed*CCx+2.29)/(CCx+2.29)),SumGDDforCCini,RatDGDD,
                               SFRedCGC,SFRedCCx,SFCDecline,TheModeCycle);
        // correction for fWeed is already in TempCCini (since DayCC > 0);
        END
   ELSE TempCCini := (CCo*fWeed); // sowing or transplanting

CCiniTotalFromTimeToCCini := TempCCini;
END; (* CCiniTotalFromTimeToCCini *)



PROCEDURE CompleteCropDescription;
VAR CGCisGiven : BOOLEAN;
BEGIN
IF ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage))
   THEN BEGIN
        IF (Crop.DaysToHIo > 0)
           THEN BEGIN
                IF (Crop.DaysToHIo > Crop.DaysToHarvest)
                   THEN Crop.dHIdt := Crop.HI/Crop.DaysToHarvest
                   ELSE Crop.dHIdt := Crop.HI/Crop.DaysToHIo;
                IF (Crop.dHIdt > 100) THEN Crop.dHIdt := 100;
                END
           ELSE Crop.dHIdt := 100;
        END
   ELSE BEGIN  //  grain or tuber crops
        IF (Crop.DaysToHIo > 0)
           THEN Crop.dHIdt := Crop.HI/Crop.DaysToHIo
           ELSE Crop.dHIdt := undef_int;
        END;
IF (Crop.ModeCycle = CalendarDays)
   THEN BEGIN
        Crop.DaysToCCini := TimeToCCini(Crop.Planting,Crop.PlantingDens,Crop.SizeSeedling,Crop.SizePlant,Crop.CCx,Crop.CGC);
        Crop.DaysToFullCanopy :=
            DaysToReachCCwithGivenCGC((0.98 * Crop.CCx),Crop.CCo,Crop.CCx,Crop.CGC,Crop.DaysToGermination);
        IF (Management.FertilityStress <> 0)
           THEN TimeToMaxCanopySF(Crop.CCo,Crop.CGC,Crop.CCx,
                  Crop.DaysToGermination,Crop.DaysToFullCanopy,Crop.DaysToSenescence,
                  Crop.DaysToFlowering,Crop.LengthFlowering,Crop.DeterminancyLinked,
                  Crop.DaysToFullCanopySF,Simulation.EffectStress.RedCGC,
                  Simulation.EffectStress.RedCCX,Management.FertilityStress)
           ELSE Crop.DaysToFullCanopySF := Crop.DaysToFullCanopy;
        Crop.GDDaysToCCini := undef_int;
        Crop.GDDaysToGermination := undef_int;
        Crop.GDDaysToFullCanopy := undef_int;
        Crop.GDDaysToFullCanopySF := undef_int;
        Crop.GDDaysToFlowering := undef_int;
        Crop.GDDLengthFlowering := undef_int;
        Crop.GDDaysToSenescence := undef_int;
        Crop.GDDaysToHarvest := undef_int;
        Crop.GDDaysToMaxRooting := undef_int;
        Crop.GDDCGC := undef_int;
        Crop.GDDCDC := undef_int;
        END
   ELSE BEGIN
        Crop.GDDaysToCCini := TimeToCCini(Crop.Planting,Crop.PlantingDens,Crop.SizeSeedling,Crop.SizePlant,Crop.CCx,Crop.GDDCGC);
        Crop.DaysToCCini := TimeToCCini(Crop.Planting,Crop.PlantingDens,Crop.SizeSeedling,Crop.SizePlant,Crop.CCx,Crop.CGC);
        Crop.GDDaysToFullCanopy :=
            DaysToReachCCwithGivenCGC((0.98 * Crop.CCx),Crop.CCo,Crop.CCx,Crop.GDDCGC,Crop.GDDaysToGermination);
        //Crop.GDDaysToFullCanopySF is determined in RUN or ManagementUnit if required
        END;

CGCisGiven := true; // required to adjust Crop.DaysToFullCanopy (does not exist)
DetermineLengthGrowthStages(Crop.CCo,Crop.CCx,Crop.CDC,Crop.DaysToGermination,Crop.DaysToHarvest,CGCisGiven,
                            Crop.DaysToCCini,Crop.Planting,Crop.DaysToSenescence,
                            Crop.Length,Crop.DaysToFullCanopy,Crop.CGC);

Crop.CCoAdjusted := Crop.CCo;
Crop.CCxAdjusted := Crop.CCx;
Crop.CCxWithered := Crop.CCx;
SumWaBal.Biomass := 0;
SumWaBal.BiomassPot := 0;
SumWabal.BiomassUnlim := 0;
SumWaBal.BiomassTot := 0; // crop and weeds (for soil fertility stress)
SumWaBal.YieldPart := 0;
Simulation.EvapLimitON := false;
END; (* CompleteCropDescription *)




PROCEDURE LoadCrop (FullName : string);
VAR f0 : TextFile;
    XX, YY : INTEGER;
    VersionNr : double;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,CropDescription);
WITH Crop DO
  BEGIN
  READLN(f0,VersionNr);  // AquaCrop version
  READLN(f0);  // Protected or Open file

  //subkind
  READLN(f0,XX);
  CASE XX of
       1 : subkind := Vegetative;
       2 : subkind := Grain;
       3 : subkind := Tuber;
       4 : subkind := Forage;
       end;

  // type of planting
  READLN(f0,XX);
  CASE XX of
       1 : Planting := Seed;
       0 : Planting := Transplant;
      -9 : Planting := Regrowth
       else Planting := Seed;
       end;

  //mode
  READLN(f0,XX);
  IF (XX = 0) THEN ModeCycle := GDDays
              ELSE ModeCycle := CalendarDays;

  //adjustment p to ETo
  READLN(f0,YY);
  IF (YY = 0) THEN pMethod := NoCorrection
              ELSE IF (YY = 1) THEN pMethod := FAOCorrection;

  //temperatures controlling crop development
  READLN(f0,Tbase);
  READLN(f0,Tupper);

  // required growing degree days to complete the crop cycle (is identical as to maturity)
  READLN(f0,GDDaysToHarvest);

  // water stress
  READLN(f0,pLeafDefUL);
  READLN(f0,pLeafDefLL);
  READLN(f0,KsShapeFactorLeaf);
  READLN(f0,pdef);
  READLN(f0,KsShapeFactorStomata);
  READLN(f0,pSenescence);
  READLN(f0,KsShapeFactorSenescence);
  READLN(f0,SumEToDelaySenescence);
  READLN(f0,pPollination);
  READLN(f0,AnaeroPoint);

  // soil fertility/salinity stress
  READLN(f0,Crop.StressResponse.Stress);    //Soil fertility stress at calibration (%)
  READLN(f0,Crop.StressResponse.ShapeCGC);  //Shape factor for the response of Canopy Growth Coefficient to soil fertility/salinity stress
  READLN(f0,Crop.StressResponse.ShapeCCX);  //Shape factor for the response of Maximum Canopy Cover to soil fertility/salinity stress
  READLN(f0,Crop.StressResponse.ShapeWP);   //Shape factor for the response of Crop Water Producitity to soil fertility stress
  READLN(f0,Crop.StressResponse.ShapeCDecline);  //Shape factor for the response of Decline of Canopy Cover to soil fertility/salinity stress
  // extra response factor for salinity stress (Version 4.0 and higher)
  // -----  UPDATE response factor
  (*
  IF (ROUND(VersionNr*10) < 40)  // UPDATE required for Version 3.0 and 3.1
     THEN Crop.StressResponse.ShapeKsSto := Crop.StressResponse.ShapeWP
     ELSE READLN(f0,Crop.StressResponse.ShapeKsSto);  //Shape factor for the response of Stomatal Closure to soil salinity stress
       *)
  IF (ROUND(VersionNr*10) >= 40)  // UPDATE required for Version 4.0 and next
     THEN READLN(f0);  //Shape factor for the response of Stomatal Closure to soil salinity stress NO LONGER VALID
  // continue with soil fertility/salinity stress
  IF ((Crop.StressResponse.ShapeCGC > 24.9) AND (Crop.StressResponse.ShapeCCX > 24.9)
      AND (Crop.StressResponse.ShapeWP > 24.9) AND (Crop.StressResponse.ShapeCDecline > 24.9))
    THEN Crop.StressResponse.Calibrated := false
    ELSE Crop.StressResponse.Calibrated := true;

  // temperature stress
  READLN(f0,Tcold); //Minimum air temperature below which pollination starts to fail (cold stress) (degC)
  READLN(f0,Theat); //Maximum air temperature above which pollination starts to fail (heat stress) (degC)
  READLN(f0,GDtranspLow); //Minimum growing degrees required for full biomass production (degC - day)

  // salinity stress (Version 3.2 and higher)
  // -----  UPDATE salinity stress
  IF (ROUND(VersionNr*10) < 32)  // UPDATE required for Version 3.0 and 3.1
     THEN BEGIN
          ECemin := 2; // upper threshold ECe
          ECemax := 15; // lower threhsold ECe
          END
     ELSE BEGIN
          READLN(f0,ECemin); // upper threshold ECe
          READLN(f0,ECemax); // lower threhsold ECe
          READLN(f0); // WAS shape factor of the Ks(salinity) - soil saturation extract (ECe) relationship
          END;
  // -----  UPDATE salinity stress (Version 5.1 and higher)
  IF (ROUND(VersionNr*10) < 51)  // UPDATE required for previous versions
     THEN BEGIN
          CCsaltDistortion := 25; //distortion canopy cover for simulation of effect of salinity stress (%)
          ResponseECsw := 100; //Response of Ks stomata to ECsw: From 0 (none) to +200 (very strong)
          END
     ELSE BEGIN
          READLN(f0,CCsaltDistortion);
          READLN(f0,ResponseECsw);
          END;

  //evapotranspiration
  READLN(f0,KcTop);
  READLN(f0,KcDecline);
  READLN(f0,RootMin);
  READLN(f0,RootMax);
  IF (RootMin > RootMax) THEN RootMin := RootMax; //security for sine function
  READLN(f0,RootShape);
  READLN(f0,SmaxTopQuarter);
  READLN(f0,SmaxBotQuarter);
  DeriveSmaxTopBottom(Crop.SmaxTopQuarter,Crop.SmaxBotQuarter,Crop.SmaxTop,Crop.SmaxBot);
  READLN(f0,CCEffectEvapLate);

  //crop development
  READLN(f0,SizeSeedling);
  IF (ROUND(VersionNr*10) < 50)  // UPDATE required for Version not yet 5.0
     THEN SizePlant := SizeSeedling
     ELSE READLN(f0,SizePlant); // Canopy size of individual plant (re-growth) at 1st day (cm2)');
  READLN(f0,PlantingDens);
  CCo := (PlantingDens/10000) * (SizeSeedling/10000);
  CCini := (PlantingDens/10000) * (SizePlant/10000);
  READLN(f0,CGC);

  READLN(f0,YearCCx); // Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials
  READLN(f0,CCxRoot); // Shape factor of the decline of CCx over the years due to self-thinning - for Perennials
  //READLN(f0,CGCdx);  removed as crop parameter
  //READLN(f0,CGCns);  removed as crop parameter
  READLN(f0);  //READLN(f0,CGCroot);  removed as crop parameter

  READLN(f0,CCx);
  READLN(f0,CDC);
  READLN(f0,DaysToGermination);
  READLN(f0,DaysToMaxRooting);
  READLN(f0,DaysToSenescence);
  READLN(f0,DaysToHarvest);
  READLN(f0,DaysToFlowering);
  READLN(f0,LengthFlowering);
  // -----  UPDATE crop development for Version 3.1
  // leafy vegetable crop has an Harvest Index which builds up starting from sowing
  IF ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage))  THEN
     BEGIN
     DaysToFlowering := 0;
     LengthFlowering := 0;
     END;

  // Crop.DeterminancyLinked
  READLN(f0,XX);
  CASE XX of
       1 : Crop.DeterminancyLinked := true;
       else Crop.DeterminancyLinked := false;
       end;

  // Potential excess of fruits (%) and building up HI
  IF ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage))
     THEN BEGIN
          READLN(f0);  // PercCycle no longer considered
          Crop.fExcess := undef_int;
          END
     ELSE READLN(f0,fExcess);
  READLN(f0,Crop.DaysToHIo);

  // yield response to water
  READLN(f0,WP);
  READLN(f0,WPy);
  // adaptation to elevated CO2 (Version 3.2 and higher)
  // -----  UPDATE Crop performance under elevated atmospheric CO2 concentration (%)
  IF (ROUND(VersionNr*10) < 32)  // UPDATE required for Version 3.0 and 3.1
     THEN AdaptedToCO2 := 50
     ELSE READLN(f0,AdaptedToCO2);
  READLN(f0,HI);
  READLN(f0,HIincrease); // possible increase (%) of HI due to water stress before flowering
  READLN(f0,aCoeff); // coefficient describing impact of restricted vegetative growth at flowering on HI
  READLN(f0,bCoeff); // coefficient describing impact of stomatal closure at flowering on HI
  READLN(f0,DHImax); // allowable maximum increase (%) of specified HI
  // -----  UPDATE yield response to water for Version 3.1
  // leafy vegetable crop has an Harvest Index (default is 85 %)
  IF ((ROUND(VersionNr*10) = 30) AND ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage)))
     THEN IF (ROUND(Crop.HI) = undef_int) THEN HI := 85;

  // growing degree days
  READLN(f0,GDDaysToGermination);
  READLN(f0,GDDaysToMaxRooting);
  READLN(f0,GDDaysToSenescence);
  READLN(f0,GDDaysToHarvest);
  READLN(f0,GDDaysToFlowering);
  READLN(f0,GDDLengthFlowering);
  READLN(f0,GDDCGC);
  READLN(f0,GDDCDC);
  READLN(f0,GDDaysToHIo);
  // -----  UPDATE yield response to water for Version 3.1
  // leafy vegetable crop has an Harvest Index which builds up starting from sowing
  IF ((ModeCycle = GDDays) AND ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage))) THEN
     BEGIN
     GDDaysToFlowering := 0;
     GDDLengthFlowering := 0;
     END;

  // extra version 6.2
  IF (ROUND(VersionNr*10) < 62)  // UPDATE required for Version 6.2
     THEN DryMatter := undef_int // undefined
     ELSE READLN(f0,DryMatter); // dry matter content (%) of fresh yield

  // extra version 7.0
  IF (ROUND(VersionNr*10) < 62)  // UPDATE required for Version 7.0
     THEN BEGIN
          RootMinYear1 := RootMin; // regrowth not yet possible
          SownYear1 := (Planting = Seed);  // type of planting first year
          // transfer of assimilates
          Assimilates.On := false; // Transfer of assimilates between root system and above ground parts is NOT considered
          Assimilates.Period := 0;
          Assimilates.Stored := 0;
          Assimilates.Mobilized := 0;
          END
     ELSE BEGIN
          READLN(f0,RootMinYear1); // Minimum rooting depth in first year in meter (for regrowth)
          READLN(f0,XX);
          CASE XX of
                1 : Crop.SownYear1 := true;  // crop is sown in 1 st year (for perennials)
               else Crop.SownYear1 := false; // crop is transplanted in 1st year (for regrowth)
               end;
          // transfer of assimilates
          READLN(f0,XX);
          CASE XX of
                1 : Assimilates.On := true;  // Transfer of assimilates from above ground parts to root system is considered
               else Assimilates.On := false; // Transfer of assimilates from above ground parts to root system is NOT considered
               end;
          READLN(f0,Assimilates.Period); // Number of days at end of season during which assimilates are stored in root system
          READLN(f0,Assimilates.Stored); // Percentage of assimilates, transferred to root system at last day of season
          READLN(f0,Assimilates.Mobilized); // Percentage of stored assimilates, transferred to above ground parts in next season
          END;

  IF (subkind = Forage) THEN
     BEGIN // data for the determination of the growing period
     // 1. Title
     For XX := 1 to 3 DO READLN(f0);
     // 2. ONSET
     READLN(f0,XX);
     IF (XX = 0)
        THEN PerennialPeriod.GenerateOnset := false // onset is fixed on a specific day
        ELSE BEGIN // onset is generated by an air temperature criterion
             PerennialPeriod.GenerateOnset := true;
             CASE XX OF
               12 : PerennialPeriod.OnsetCriterion := TMeanPeriod; // Criterion: mean air temperature
               13 : PerennialPeriod.OnsetCriterion := GDDPeriod; // Criterion: growing-degree days
               else PerennialPeriod.GenerateOnset := false;
               end;
             END;
     READLN(f0,PerennialPeriod.OnsetFirstDay);
     READLN(f0,PerennialPeriod.OnsetFirstMonth);
     READLN(f0,PerennialPeriod.OnsetLengthSearchPeriod);
     READLN(f0,PerennialPeriod.OnsetThresholdValue); // Mean air temperature or Growing-degree days
     READLN(f0,PerennialPeriod.OnsetPeriodValue); // number of succesive days
     READLN(f0,PerennialPeriod.OnsetOccurrence);  // number of occurrence
     IF (PerennialPeriod.OnsetOccurrence > 3) THEN PerennialPeriod.OnsetOccurrence := 3;
     // 3. END of growing period
     READLN(f0,XX);
     IF (XX = 0)
        THEN PerennialPeriod.GenerateEnd := false  // end is fixed on a specific day
        ELSE BEGIN // end is generated by an air temperature criterion
             PerennialPeriod.GenerateEnd := true;
             CASE XX OF
               62 : PerennialPeriod.EndCriterion := TMeanPeriod; // Criterion: mean air temperature
               63 : PerennialPeriod.EndCriterion := GDDPeriod; // Criterion: growing-degree days
               else PerennialPeriod.GenerateEnd := false;
               end;
             END;
     READLN(f0,PerennialPeriod.EndLastDay);
     READLN(f0,PerennialPeriod.EndLastMonth);
     READLN(f0,PerennialPeriod.ExtraYears);
     READLN(f0,PerennialPeriod.EndLengthSearchPeriod);
     READLN(f0,PerennialPeriod.EndThresholdValue); // Mean air temperature or Growing-degree days
     READLN(f0,PerennialPeriod.EndPeriodValue); // number of succesive days
     READLN(f0,PerennialPeriod.EndOccurrence); // number of occurrence
     IF (PerennialPeriod.EndOccurrence > 3) THEN PerennialPeriod.EndOccurrence := 3;
     END;
  END;
Close(f0);
// maximum rooting depth in given soil profile
Soil.RootMax := RootMaxInSoilProfile(Crop.RootMax,Soil.NrSoilLayers,SoilLayer);

// copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(Crop.DaysToHarvest - Crop.DaysToSenescence);
SetCropFileSet_DaysToHarvest(Crop.DaysToHarvest);
IF (Crop.ModeCycle = GDDays)
   THEN BEGIN
        SetCropFileSet_GDDaysFromSenescenceToEnd(Crop.GDDaysToHarvest - Crop.GDDaysToSenescence);
        SetCropFileSet_GDDaysToHarvest(Crop.GDDaysToHarvest);
        END
   ELSE BEGIN
        SetCropFileSet_GDDaysFromSenescenceToEnd(undef_int);
        SetCropFileSet_GDDaysToHarvest(undef_int);
        END;

END; // LoadCrop


PROCEDURE CompleteClimateDescription(VAR ClimateRecord : rep_clim);
VAR dayStr,yearStr : STRING;
    Deci : INTEGER;
BEGIN
DetermineDayNr(ClimateRecord.FromD,ClimateRecord.FromM,ClimateRecord.FromY,ClimateRecord.FromDayNr);
CASE ClimateRecord.DataType OF
   Daily    : BEGIN
              ClimateRecord.ToDayNr := ClimateRecord.FromDayNr + ClimateRecord.NrObs - 1;
              DetermineDate(ClimateRecord.ToDayNr,ClimateRecord.ToD,ClimateRecord.ToM,ClimateRecord.ToY);
              END;
   Decadely : BEGIN
              Deci := ROUND((ClimateRecord.FromD+9)/10) + ClimateRecord.NrObs - 1;
              ClimateRecord.ToM := ClimateRecord.FromM;
              ClimateRecord.ToY := ClimateRecord.FromY;
              WHILE (Deci > 3) DO
                BEGIN
                Deci := Deci - 3;
                ClimateRecord.ToM := ClimateRecord.ToM + 1;
                IF (ClimateRecord.ToM > 12) THEN BEGIN
                                                 ClimateRecord.ToM := 1;
                                                 ClimateRecord.ToY := ClimateRecord.ToY  + 1;
                                                 END;
                END;
              ClimateRecord.ToD := 10;
              IF (Deci = 2) THEN ClimateRecord.ToD := 20;
              IF (Deci = 3) THEN
                 BEGIN
                 ClimateRecord.ToD := DaysInMonth[ClimateRecord.ToM];
                 IF ((ClimateRecord.ToM = 2) AND LeapYear(ClimateRecord.ToY)) THEN ClimateRecord.ToD := ClimateRecord.ToD + 1;
                 END;
              DetermineDayNr(ClimateRecord.ToD,ClimateRecord.ToM,ClimateRecord.ToY,ClimateRecord.ToDayNr);
              END;
   Monthly  : BEGIN
              ClimateRecord.ToY := ClimateRecord.FromY;
              ClimateRecord.ToM := ClimateRecord.FromM + ClimateRecord.NrObs - 1;
              WHILE (ClimateRecord.ToM > 12) DO
                    BEGIN
                    ClimateRecord.ToY := ClimateRecord.ToY + 1;
                    ClimateRecord.ToM := ClimateRecord.ToM - 12;
                    END;
              ClimateRecord.ToD := DaysInMonth[ClimateRecord.ToM];
              IF ((ClimateRecord.ToM = 2) AND LeapYear(ClimateRecord.ToY)) THEN ClimateRecord.ToD := ClimateRecord.ToD + 1;
              DetermineDayNr(ClimateRecord.ToD,ClimateRecord.ToM,ClimateRecord.ToY,ClimateRecord.ToDayNr);
              END;
   end;
Str(ClimateRecord.FromD:2,dayStr);
IF ClimateRecord.FromY = 1901 THEN yearStr := ''
                              ELSE Str(ClimateRecord.FromY:4,yearStr);
ClimateRecord.FromString := CONCAT(dayStr,' ',NameMonth[ClimateRecord.FromM],' ',yearStr);
Str(ClimateRecord.ToD:2,dayStr);
IF ClimateRecord.FromY = 1901 THEN yearStr := ''
                              ELSE Str(ClimateRecord.ToY:4,yearStr);
ClimateRecord.ToString := CONCAT(dayStr,' ',NameMonth[ClimateRecord.ToM],' ',yearStr);
END; (* CompleteClimateDescription *)


PROCEDURE LoadClimate(FullName : string;
                      VAR ClimateDescription : string;
                      VAR TempFile,EToFile,RainFile,CO2File : string);
VAR f0 : TextFile;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,CLimateDescription);
READLN(f0); // AquaCrop Version
READLN(f0,TempFile);
READLN(f0,EToFile);
READLN(f0,RainFile);
READLN(f0,CO2File);
Close(f0);
END; (* LoadClimate *)




PROCEDURE LoadClim (FullName : string;
                    VAR ClimateDescription : string;
                    VAR ClimateRecord : rep_clim);
VAR f0 : TextFile;
    Ni : INTEGER;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,CLimateDescription);
READLN(f0,Ni);
IF (Ni = 1) THEN ClimateRecord.DataType := Daily
            ELSE IF (Ni = 2) THEN ClimateRecord.DataType := Decadely
                             ELSE ClimateRecord.DataType := Monthly;
READLN(f0,ClimateRecord.FromD);
READLN(f0,ClimateRecord.FromM);
READLN(f0,ClimateRecord.FromY);
READLN(f0);
READLN(f0);
READLN(f0);
ClimateRecord.NrObs := 0;
WHILE NOT Eof(f0) DO
      BEGIN
      ClimateRecord.NrObs := ClimateRecord.NrObs + 1;
      READLN(f0);
      END;
Close(f0);
CompleteClimateDescription(ClimateRecord);
END; // LoadClim



PROCEDURE SaveProfile(totalname : string);
VAR f : TextFile;
    i : INTEGER;
BEGIN
Assign(f,totalname);
Rewrite(f);
WRITELN(f,ProfDescription);
WRITELN(f,'        7.0                 : AquaCrop Version (June 2021)');    // AquaCrop version
WRITELN(f,Soil.CNvalue:9,'                   : CN (Curve Number)');
WRITELN(f,Soil.REW:9,'                   : Readily evaporable water from top layer (mm)');
WRITELN(f,Soil.NrSoilLayers:9,'                   : number of soil horizons');
WRITELN(f,undef_int:9,'                   : variable no longer applicable');
WRITELN(f,'  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description');
WRITELN(f,'  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------');
FOR i := 1 TO Soil.NrSoilLayers DO
    WRITELN(f,SoilLayer[i].Thickness:8:2,SoilLayer[i].SAT:8:1,SoilLayer[i].FC:6:1,
              SoilLayer[i].WP:6:1,SoilLayer[i].InfRate:8:1,SoilLayer[i].Penetrability:11,
              SoilLayer[i].GravelMass:10,SoilLayer[i].CRa:14:6,SoilLayer[i].CRb:10:6,
              '   ',SoilLayer[i].Description:15);
Close(f);

// maximum rooting depth in  soil profile for given crop
Soil.RootMax := RootMaxInSoilProfile(Crop.RootMax,Soil.NrSoilLayers,SoilLayer);
END; (* SaveProfile *)



PROCEDURE AppendCropFilePerennials(totalname : string;
                                   GenrateTheOnset,GenerateTheEnd : BOOLEAN;
                                   CriterionNrOnset,Day1Onset,Month1Onset,LengthOnset,SuccessiveDaysOnset,OccurrenceOnset : INTEGER;
                                   CriterionNrEnd,DayNEnd,MonthNEnd,ExtraYearEnd,LengthEnd,SuccessiveDaysEnd,OccurrenceEnd : INTEGER;
                                   ThresholdOnset,ThresholdEnd : double);
VAR f : TextFile;
    TempString : string;
BEGIN
Assign(f,totalname);
Append(f);
// 1. Title
WRITELN(f); // empty line
WRITELN(f,' Internal crop calendar');
WRITELN(f,' ========================================================');

// 2. ONSET
// criterion number
IF (GenrateTheOnset = false)
   THEN BEGIN
        CriterionNrOnset := 0;
        TempString := '         : The Restart of growth is fixed on a specific date';
        END
   ELSE BEGIN
        CASE CriterionNrOnset OF
          12 : TempString := '         : The Restart of growth is generated by average air temperature';
          13 : TempString := '         : The Restart of growth is generated by Growing-degree days';
          else BEGIN // no valid criterion number
               GenrateTheOnset := false;
               CriterionNrOnset := 0;
               TempString := '         : The Restart of growth is fixed on a specific date';
               END;
          end;
        END;
WRITELN(f,CriterionNrOnset:6,TempString);

// parameters ONSET
IF (GenrateTheOnset = false)
   THEN BEGIN
        WRITELN(f,Day1Onset:6,'         : First Day of growth');
        WRITELN(f,Month1Onset:6,'         : First Month of growth');
        WRITELN(f,LengthOnset:6,'         : Length (days) for the time window (Restart of growth): Not Applicable');
        WRITELN(f,ThresholdOnset:8:1,'       : Threshold for the Restart criterion: Not Applicable');
        WRITELN(f,SuccessiveDaysOnset:6,'         : Number of successive days for the Restart criterion: Not Applicable');
        WRITELN(f,OccurrenceOnset:6,'         : Number of occurrences before the Restart criterion applies: Not Applicable');
        END
   ELSE BEGIN
        WRITELN(f,Day1Onset:6,'         : First Day for the time window (Restart of growth)');
        WRITELN(f,Month1Onset:6,'         : First Month for the time window (Restart of growth)');
        WRITELN(f,LengthOnset:6,'         : Length (days) of the time window (Restart of growth)');
        CASE CriterionNrOnset OF
          12 : TempString := '       : Threshold for the Restart criterion: Average air temperature (degC)';
          13 : TempString := '       : Threshold for the Restart criterion: Growing-degree days';
          end;
        WRITELN(f,ThresholdOnset:8:1,TempString);
        WRITELN(f,SuccessiveDaysOnset:6,'         : Number of successive days for the Restart criterion');
        IF (OccurrenceOnset > 3) THEN OccurrenceOnset := 3;
        WRITELN(f,OccurrenceOnset:6,'         : Number of occurrences before the Restart criterion applies');
        END;

// 3. END of growing period
// criterion number
IF (GenerateTheEnd = false)
   THEN BEGIN
        CriterionNrEnd := 0;
        TempString := '         : The End of growth is fixed on a specific date';
        END
   ELSE BEGIN
        CASE CriterionNrEnd OF
          62 : TempString := '         : The End of growth is generated by average air temperature';
          63 : TempString := '         : The End of growth is generated by Growing-degree days';
          else BEGIN // no valid criterion number
               GenerateTheEnd := false;
               CriterionNrEnd := 0;
               TempString := '         : The End of growth is fixed on a specific date';
               END;
          end;
        END;
WRITELN(f,CriterionNrEnd:6,TempString);

// parameters END of growing period
IF (GenerateTheEnd = false)
   THEN BEGIN
        WRITELN(f,DayNEnd:6,'         : Last Day of growth');
        WRITELN(f,MonthNEnd:6,'         : Last Month of growth');
        WRITELN(f,ExtraYearEnd:6,'         : Number of years to add to the Restart year');
        WRITELN(f,LengthEnd:6,'         : Length (days) for the time window (End of growth): Not Applicable');
        WRITELN(f,ThresholdEnd:8:1,'       : Threshold for the End criterion: Not Applicable');
        WRITELN(f,SuccessiveDaysEnd:6,'         : Number of successive days for the End criterion: Not Applicable');
        WRITELN(f,OccurrenceEnd:6,'         : Number of occurrences before the End criterion applies: Not Applicable');
        END
   ELSE BEGIN
        WRITELN(f,DayNEnd:6,'         : Last Day for the time window (End of growth)');
        WRITELN(f,MonthNEnd:6,'         : Last Month for the time window (End of growth)');
        WRITELN(f,ExtraYearEnd:6,'         : Number of years to add to the Onset year');
        WRITELN(f,LengthEnd:6,'         : Length (days) of the time window (End of growth)');
        CASE CriterionNrEnd OF
          62 : TempString := '       : Threshold for the End criterion: average air temperature (degC)';
          63 : TempString := '       : Threshold for the End criterion: Growing-degree days';
          end;
        WRITELN(f,ThresholdEnd:8:1,TempString);
        WRITELN(f,SuccessiveDaysEnd:6,'         : Number of successive days for the End criterion');
        IF (OccurrenceEnd > 3) THEN OccurrenceEnd := 3;
        WRITELN(f,OccurrenceEnd:6,'         : Number of occurrences before the End criterion applies');
        END;
Close(f);
END; (* AppendCropFilePerennials *)


PROCEDURE SaveCrop(totalname : string);
VAR f : TextFile;
    i,j : INTEGER;
    TempString : string;
BEGIN
Assign(f,totalname);
Rewrite(f);
WRITELN(f,CropDescription);
WITH Crop DO
  BEGIN
  // AquaCrop version
  WRITELN(f,'     7.0       : AquaCrop Version (June 2021)');
  WRITELN(f,'     1         : File not protected');

  //SubKind
  i := 2;
  CASE subkind OF
       Vegetative : BEGIN
                    i := 1;
                    TempString := '         : leafy vegetable crop';
                    END;
       Grain      : BEGIN
                    i := 2;
                    TempString := '         : fruit/grain producing crop';
                    END;
       Tuber      : BEGIN
                    i := 3;
                    TempString := '         : root/tuber crop';
                    END;
       Forage     : BEGIN
                    i := 4;
                    TempString := '         : forage crop';
                    END;
       end;
  WRITELN(f,i:6,TempString);

  //Sown, transplanting or regrowth
  IF (Planting = Seed)
     THEN BEGIN
          i := 1;
          IF (subkind = Forage)
             THEN WRITELN(f,i:6,'         : Crop is sown in 1st year')
             ELSE WRITELN(f,i:6,'         : Crop is sown');
          END
     ELSE BEGIN
          IF (Planting = Transplant)
             THEN BEGIN
                  i := 0;
                  IF (subkind = Forage)
                     THEN WRITELN(f,i:6,'         : Crop is transplanted in 1st year')
                     ELSE WRITELN(f,i:6,'         : Crop is transplanted');
                  END
             ELSE BEGIN
                  i := -9;
                  WRITELN(f,i:6,'         : Crop is regrowth');
                  END;
          END;

  //Mode (description crop cycle)
  i := 1;
  TempString := '         : Determination of crop cycle : by calendar days';
  IF (ModeCycle = GDDays) THEN
     BEGIN
     i := 0;
     TempString := '         : Determination of crop cycle : by growing degree-days';
     END;
  WRITELN(f,i:6,TempString);

  //p correction for ET
  IF (pMethod = NoCorrection)
     THEN BEGIN
          j := 0;
          WRITELN(f,j:6,'         : No adjustment by ETo of soil water depletion factors (p)');
          END
     ELSE BEGIN
          j := 1;
          WRITELN(f,j:6,'         : Soil water depletion factors (p) are adjusted by ETo');
          END;

  // temperatures controlling crop development
  WRITELN(f,Tbase:8:1,'       : Base temperature (degC) below which crop development does not progress');
  WRITELN(f,Tupper:8:1,'       : Upper temperature (degC) above which crop development no longer increases with an increase in temperature');

  // required growing degree days to complete the crop cycle (is identical as to maturity)
  WRITELN(f,GDDaysToHarvest:6,'         : Total length of crop cycle in growing degree-days');

  // water stress
  WRITELN(f,pLeafDefUL:9:2,'      : Soil water depletion factor for canopy expansion (p-exp) - Upper threshold');
  WRITELN(f,pLeafDefLL:9:2,'      : Soil water depletion factor for canopy expansion (p-exp) - Lower threshold');
  WRITELN(f,KsShapeFactorLeaf:8:1,'       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)');
  WRITELN(f,pdef:9:2,'      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold');
  WRITELN(f,KsShapeFactorStomata:8:1,'       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)');
  WRITELN(f,pSenescence:9:2,'      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold');
  WRITELN(f,KsShapeFactorSenescence:8:1,'       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)');
  WRITELN(f,SumEToDelaySenescence:6,'         : Sum(ETo) during dormant period to be exceeded before crop is permanently wilted');
  IF (pPollination = undef_int)
     THEN WRITELN(f,pPollination:9:2,'      : Soil water depletion factor for pollination - Not Applicable')
     ELSE WRITELN(f,pPollination:9:2,'      : Soil water depletion factor for pollination (p - pol) - Upper threshold');
  WRITELN(f,AnaeroPoint:6,'         : Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)');

  // stress response
  WRITELN(f,Crop.StressResponse.Stress:6,'         : Considered soil fertility stress for calibration of stress response (%)');
  IF (Crop.StressResponse.ShapeCGC > 24.9)
     THEN WRITELN(f,Crop.StressResponse.ShapeCGC:9:2,'      : Response of canopy expansion is not considered')
     ELSE WRITELN(f,Crop.StressResponse.ShapeCGC:9:2,'      : Shape factor for the response of canopy expansion to soil fertility stress');
  IF (Crop.StressResponse.ShapeCCX > 24.9)
     THEN WRITELN(f,Crop.StressResponse.ShapeCCX:9:2,'      : Response of maximum canopy cover is not considered')
     ELSE WRITELN(f,Crop.StressResponse.ShapeCCX:9:2,'      : Shape factor for the response of maximum canopy cover to soil fertility stress');
  IF (Crop.StressResponse.ShapeWP > 24.9)
     THEN WRITELN(f,Crop.StressResponse.ShapeWP:9:2,'      : Response of crop Water Productivity is not considered')
     ELSE WRITELN(f,Crop.StressResponse.ShapeWP:9:2,'      : Shape factor for the response of crop Water Productivity to soil fertility stress');
  IF (Crop.StressResponse.ShapeCDecline > 24.9)
     THEN WRITELN(f,Crop.StressResponse.ShapeCDecline:9:2,'      : Response of decline of canopy cover is not considered')
     ELSE WRITELN(f,Crop.StressResponse.ShapeCDecline:9:2,'      : Shape factor for the response of decline of canopy cover to soil fertility stress');
  WRITELN(f,'    -9         : dummy - Parameter no Longer required');

  // temperature stress
  IF (Round(Tcold) = undef_int)
     THEN WRITELN(f,Tcold:6,'         : Cold (air temperature) stress affecting pollination - not considered')
     ELSE WRITELN(f,Tcold:6,'         : Minimum air temperature below which pollination starts to fail (cold stress) (degC)');
  IF (Round(Theat) = undef_int)
     THEN WRITELN(f,Theat:6,'         : Heat (air temperature) stress affecting pollination - not considered')
     ELSE WRITELN(f,Theat:6,'         : Maximum air temperature above which pollination starts to fail (heat stress) (degC)');
  IF (Round(GDtranspLow) = undef_int)
     THEN WRITELN(f,GDtranspLow:8:1,'       : Cold (air temperature) stress on crop transpiration not considered')
     ELSE WRITELN(f,GDtranspLow:8:1,'       : Minimum growing degrees required for full crop transpiration (degC - day)');

 // salinity stress
  WRITELN(f,ECemin:6,'         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)');
  WRITELN(f,ECemax:6,'         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)');
  WRITELN(f,'    -9         : Dummy - no longer applicable'); // shape factor Ks(salt)-ECe
  WRITELN(f,CCsaltDistortion:6,'         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))');
  WRITELN(f,ResponseECsw:6,'         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))');

  //evapotranspiration
  WRITELN(f,KcTop:9:2,'      : Crop coefficient when canopy is complete but prior to senescence (KcTr,x)');
  WRITELN(f,KcDecline:10:3,'     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.');
  WRITELN(f,RootMin:9:2,'      : Minimum effective rooting depth (m)');
  WRITELN(f,RootMax:9:2,'      : Maximum effective rooting depth (m)');
  WRITELN(f,RootShape:6,'         : Shape factor describing root zone expansion');
  WRITELN(f,SmaxTopQuarter:10:3,'     : Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone');
  WRITELN(f,SmaxBotQuarter:10:3,'     : Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone');
  WRITELN(f,CCEffectEvapLate:6,'         : Effect of canopy cover in reducing soil evaporation in late season stage');

  //canopy development
  WRITELN(f,SizeSeedling:9:2,'      : Soil surface covered by an individual seedling at 90 % emergence (cm2)');
  WRITELN(f,SizePlant:9:2,'      : Canopy size of individual plant (re-growth) at 1st day (cm2)');
  WRITELN(f,PlantingDens:9,'      : Number of plants per hectare');
  WRITELN(f,CGC:12:5,'   : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)');
  IF (YearCCx = undef_int)
     THEN WRITELN(f,YearCCx:6,'         : Number of years at which CCx declines to 90 % of its value due to self-thinning - Not Applicable')
     ELSE WRITELN(f,YearCCx:6,'         : Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials');
  IF (Round(CCxRoot) = undef_int)
     THEN WRITELN(f,CCxRoot:9:2,'      : Shape factor of the decline of CCx over the years due to self-thinning - Not Applicable')
     ELSE WRITELN(f,CCxRoot:9:2,'      : Shape factor of the decline of CCx over the years due to self-thinning - for Perennials');
  WRITELN(f,'    -9         : dummy - Parameter no Longer required');

  WRITELN(f,CCx:9:2,'      : Maximum canopy cover (CCx) in fraction soil cover');
  WRITELN(f,CDC:12:5,'   : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)');
  IF (Planting = Seed)
     THEN BEGIN
          WRITELN(f,DaysToGermination:6,'         : Calendar Days: from sowing to emergence');
          WRITELN(f,DaysToMaxRooting:6,'         : Calendar Days: from sowing to maximum rooting depth');
          WRITELN(f,DaysToSenescence:6,'         : Calendar Days: from sowing to start senescence');
          WRITELN(f,DaysToHarvest:6,'         : Calendar Days: from sowing to maturity (length of crop cycle)');
          IF (subkind = Tuber)
             THEN WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from sowing to start of yield formation')
             ELSE WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from sowing to flowering');
          END
     ELSE BEGIN
          IF (Planting = Transplant)
             THEN BEGIN
                  WRITELN(f,DaysToGermination:6,'         : Calendar Days: from transplanting to recovered transplant');
                  WRITELN(f,DaysToMaxRooting:6,'         : Calendar Days: from transplanting to maximum rooting depth');
                  WRITELN(f,DaysToSenescence:6,'         : Calendar Days: from transplanting to start senescence');
                  WRITELN(f,DaysToHarvest:6,'         : Calendar Days: from transplanting to maturity');
                  IF (subkind = Tuber)
                     THEN WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from transplanting to start of yield formation')
                     ELSE WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from transplanting to flowering');
                  END
             ELSE BEGIN  // planting = regrowth
                  WRITELN(f,DaysToGermination:6,'         : Calendar Days: from regrowth to recovering');
                  WRITELN(f,DaysToMaxRooting:6,'         : Calendar Days: from regrowth to maximum rooting depth');
                  WRITELN(f,DaysToSenescence:6,'         : Calendar Days: from regrowth to start senescence');
                  WRITELN(f,DaysToHarvest:6,'         : Calendar Days: from regrowth to maturity');
                  IF (subkind = Tuber)
                     THEN WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from regrowth to start of yield formation')
                     ELSE WRITELN(f,DaysToFlowering:6,'         : Calendar Days: from regrowth to flowering');
                  END;
          END;
  WRITELN(f,LengthFlowering:6,'         : Length of the flowering stage (days)');

  // Crop.DeterminancyLinked
  IF (DeterminancyLinked = true)
     THEN BEGIN
          i := 1;
          TempString := '         : Crop determinancy linked with flowering';
          END
     ELSE BEGIN
          i := 0;
          TempString := '         : Crop determinancy unlinked with flowering';
          END;
  WRITELN(f,i:6,TempString);

  // Potential excess of fruits (%)
  IF ((Crop.subkind = Vegetative) OR (Crop.subkind = Forage))
     THEN WRITELN(f,undef_int:6,'         : parameter NO LONGER required') // Building up of Harvest Index (% of growing cycle)')
     ELSE BEGIN
          WRITE(f,fExcess:6);
          IF (fExcess = undef_int)
             THEN WRITELN(f,'         : Excess of potential fruits - Not Applicable')
             ELSE WRITELN(f,'         : Excess of potential fruits (%)');
          END;

  // Building-up of Harvest Index
  WRITE(f,DaysToHIo:6);
  IF (DaysToHIo = undef_int)
     THEN WRITELN(f,'         : Building up of Harvest Index - Not Applicable')
     ELSE BEGIN
          CASE subkind OF
               Vegetative,
               Forage     : WRITELN(f,'         : Building up of Harvest Index starting at sowing/transplanting (days)');
               Grain      : WRITELN(f,'         : Building up of Harvest Index starting at flowering (days)');
               Tuber      : WRITELN(f,'         : Building up of Harvest Index starting at root/tuber enlargement (days)');
               else WRITELN(f,'         : Building up of Harvest Index during yield formation (days)');
               end;
          END;

  //yield response to water
  WRITELN(f,WP:8:1,'       : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)');
  WRITELN(f,WPy:6,'         : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)');
  WRITELN(f,AdaptedToCO2:6,'         : Crop performance under elevated atmospheric CO2 concentration (%)');
  WRITELN(f,HI:6,'         : Reference Harvest Index (HIo) (%)');
  IF (subkind = Tuber)
     THEN WRITELN(f,HIincrease:6,'         : Possible increase (%) of HI due to water stress before start of yield formation')
     ELSE WRITELN(f,HIincrease:6,'         : Possible increase (%) of HI due to water stress before flowering');
  IF (ROUND(aCoeff) = undef_int)
     THEN WRITELN(f,aCoeff:8:1,'       : No impact on HI of restricted vegetative growth during yield formation ')
     ELSE WRITELN(f,aCoeff:8:1,'       : Coefficient describing positive impact on HI of restricted vegetative growth during yield formation');
  IF (ROUND(bCoeff) = undef_int)
     THEN WRITELN(f,bCoeff:8:1,'       : No effect on HI of stomatal closure during yield formation')
     ELSE WRITELN(f,bCoeff:8:1,'       : Coefficient describing negative impact on HI of stomatal closure during yield formation');
  WRITELN(f,DHImax:6,'         : Allowable maximum increase (%) of specified HI');

  // growing degree days
  IF (Planting = Seed)
     THEN BEGIN
          WRITELN(f,GDDaysToGermination:6,'         : GDDays: from sowing to emergence');
          WRITELN(f,GDDaysToMaxRooting:6,'         : GDDays: from sowing to maximum rooting depth');
          WRITELN(f,GDDaysToSenescence:6,'         : GDDays: from sowing to start senescence');
          WRITELN(f,GDDaysToHarvest:6,'         : GDDays: from sowing to maturity (length of crop cycle)');
          IF (subkind = Tuber)
             THEN WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from sowing to start tuber formation')
             ELSE WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from sowing to flowering');
          END
     ELSE BEGIN
          IF (Planting = Transplant)
             THEN BEGIN
                  WRITELN(f,GDDaysToGermination:6,'         : GDDays: from transplanting to recovered transplant');
                  WRITELN(f,GDDaysToMaxRooting:6,'         : GDDays: from transplanting to maximum rooting depth');
                  WRITELN(f,GDDaysToSenescence:6,'         : GDDays: from transplanting to start senescence');
                  WRITELN(f,GDDaysToHarvest:6,'         : GDDays: from transplanting to maturity');
                  IF (subkind = Tuber)
                     THEN WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from transplanting to start yield formation')
                     ELSE WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from transplanting to flowering');
                  END
             ELSE BEGIN // Planting = regrowth
                  WRITELN(f,GDDaysToGermination:6,'         : GDDays: from regrowth to recovering');
                  WRITELN(f,GDDaysToMaxRooting:6,'         : GDDays: from regrowth to maximum rooting depth');
                  WRITELN(f,GDDaysToSenescence:6,'         : GDDays: from regrowth to start senescence');
                  WRITELN(f,GDDaysToHarvest:6,'         : GDDays: from regrowth to maturity');
                  IF (subkind = Tuber)
                     THEN WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from regrowth to start yield formation')
                     ELSE WRITELN(f,GDDaysToFlowering:6,'         : GDDays: from regrowth to flowering');
                  END;
          END;
  WRITELN(f,GDDLengthFlowering:6,'         : Length of the flowering stage (growing degree days)');
  WRITELN(f,GDDCGC:13:6,'  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)');
  WRITELN(f,GDDCDC:13:6,'  : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)');
  WRITELN(f,GDDaysToHIo:6,'         : GDDays: building-up of Harvest Index during yield formation');

  // added to 6.2
  WRITELN(f,DryMatter:6,'         : dry matter content (%) of fresh yield');

  // added to 7.0 - Perennial crops
  IF (Crop.subkind = Forage)
     THEN WRITELN(f,RootMinYear1:9:2,'      : Minimum effective rooting depth (m) in first year (for perennials)')
     ELSE WRITELN(f,RootMinYear1:9:2,'      : Minimum effective rooting depth (m) in first year - required only in case of regrowth');
  IF (Crop.SownYear1 = true)
     THEN BEGIN
          i := 1;
          IF (Crop.subkind = Forage)
             THEN WRITELN(f,i:6,'         : Crop is sown in 1st year (for perennials)')
             ELSE WRITELN(f,i:6,'         : Crop is sown in 1st year - required only in case of regrowth');
          END
     ELSE BEGIN
          i := 0;
          IF (Crop.subkind = Forage)
             THEN WRITELN(f,i:6,'         : Crop is transplanted in 1st year (for perennials)')
             ELSE WRITELN(f,i:6,'         : Crop is transplanted in 1st year - required only in case of regrowth');
          END;

  // added to 7.0 - Assimilates
  IF (Crop.Assimilates.On = false)
     THEN BEGIN
          i := 0;
          WRITELN(f,i:6,'         : Transfer of assimilates from above ground parts to root system is NOT considered');
          WRITELN(f,i:6,'         : Number of days at end of season during which assimilates are stored in root system');
          WRITELN(f,i:6,'         : Percentage of assimilates transferred to root system at last day of season');
          WRITELN(f,i:6,'         : Percentage of stored assimilates transferred to above ground parts in next season');
          END
     ELSE BEGIN
          i := 1;
          WRITELN(f,i:6,'         : Transfer of assimilates from above ground parts to root system is considered');
          WRITELN(f,Crop.Assimilates.Period:6,'         : Number of days at end of season during which assimilates are stored in root system');
          WRITELN(f,Crop.Assimilates.Stored:6,'         : Percentage of assimilates transferred to root system at last day of season');
          WRITELN(f,Crop.Assimilates.Mobilized:6,'         : Percentage of stored assimilates transferred to above ground parts in next season');
          END;
  END;
Close(f);

// maximum rooting depth in given soil profile
Soil.RootMax := RootMaxInSoilProfile(Crop.RootMax,Soil.NrSoilLayers,SoilLayer);

// copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(Crop.DaysToHarvest - Crop.DaysToSenescence);
SetCropFileSet_DaysToHarvest(Crop.DaysToHarvest);
SetCropFileSet_GDDaysFromSenescenceToEnd(Crop.GDDaysToHarvest - Crop.GDDaysToSenescence);
SetCropFileSet_GDDaysToHarvest(Crop.GDDaysToHarvest);
END; (* SaveCrop *)








FUNCTION EndGrowingPeriod(Day1 : longint;
                          VAR DayN : longint) : string;
VAR dayi,monthi,yeari : integer;
    Strday,StrMonth : string;
BEGIN
// Deze functie bepaald Crop.DayN en de string
DayN := Day1 + Crop.DaysToHarvest - 1;
IF (DayN < Day1) THEN DayN := Day1;
DetermineDate(DayN,dayi,monthi,yeari);
Str(dayi:2,Strday);
StrMonth := NameMonth[monthi];
EndGrowingPeriod := CONCAT(Strday,' ',StrMonth,'  ');
END; (* EndGrowingPeriod *)


PROCEDURE DetermineLinkedSimDay1(CropDay1 : LongInt;
                                 VAR SimDay1 :LongInt);
BEGIN
SimDay1 := CropDay1;
IF (GetClimFile() <> '(None)') THEN
   BEGIN
   (*
   IF SimDay1 < ClimRecord.FromDayNr THEN SimDay1 := ClimRecord.FromDayNr;
   IF SimDay1 > ClimRecord.ToDayNr
      THEN BEGIN
           Simulation.LinkCropToSimPeriod := false;
           SimDay1 := ClimRecord.FromDayNr;
           END; *)
   IF ((SimDay1 < ClimRecord.FromDayNr) OR (SimDay1 > ClimRecord.ToDayNr)) THEN
      BEGIN
      Simulation.LinkCropToSimPeriod := false;
      SimDay1 := ClimRecord.FromDayNr;
      END;
   END;
END; (* DetermineLinkedSimDay1 *)


PROCEDURE AdjustCropYearToClimFile(VAR CDay1,CDayN : longint);
VAR dayi,monthi,yeari : INTEGER;
    temp_str : string;
BEGIN
DetermineDate(CDay1,dayi,monthi,yeari);
IF (GetClimFile() = '(None)')
   THEN yeari := 1901  // yeari = 1901 if undefined year
   ELSE yeari := ClimRecord.FromY; // yeari = 1901 if undefined year
   (*
   ELSE BEGIN
        yeari := Simulation.YearStartCropCycle;
        IF (CDay1 > ClimRecord.ToY) THEN yeari := ClimRecord.FromY;
        END; *)
DetermineDayNr(dayi,monthi,yeari,CDay1);
temp_str := EndGrowingPeriod(CDay1,CDayN);
END; (* AdjustCropYearToClimFile *)


PROCEDURE AdjustClimRecordTo(CDayN : longint);
VAR dayi,monthi,yeari : INTEGER;
BEGIN
DetermineDate(CDayN,dayi,monthi,yeari);
ClimRecord.ToD := 31;
ClimRecord.ToM := 12;
ClimRecord.ToY := yeari;
DetermineDayNr(ClimRecord.ToD,ClimRecord.ToM,ClimRecord.ToY,ClimRecord.ToDayNr);
END; (* AdjustClimRecordTo *)


PROCEDURE ResetSWCToFC;
VAR Loci,layeri,compi,celli : ShortInt;
BEGIN

Simulation.IniSWC.AtDepths := false;
IF (ZiAqua < 0) // no ground water table
   THEN BEGIN
        Simulation.IniSWC.NrLoc := Soil.NrSoilLayers;
        FOR layeri := 1 TO Soil.NrSoilLayers DO
            BEGIN
            Simulation.IniSWC.Loc[layeri] := SoilLayer[layeri].Thickness;
            Simulation.IniSWC.VolProc[layeri] := SoilLayer[layeri].FC;
            Simulation.IniSWC.SaltECe[layeri] := 0;
            END;
        FOR layeri := (Soil.NrSoilLayers+1) TO max_No_compartments DO
            BEGIN
            Simulation.IniSWC.Loc[layeri] := undef_double;
            Simulation.IniSWC.VolProc[layeri] := undef_double;
            Simulation.IniSWC.SaltECe[layeri] := undef_double;
            END;
        END
   ELSE BEGIN
        Simulation.IniSWC.NrLoc := NrCompartments;
        FOR Loci := 1 TO Simulation.IniSWC.NrLoc DO
            BEGIN
            Simulation.IniSWC.Loc[Loci] := Compartment[Loci].Thickness;
            Simulation.IniSWC.VolProc[Loci] := Compartment[Loci].FCadj;
            Simulation.IniSWC.SaltECe[Loci] := 0.0;
        END;
    END;
FOR compi := 1 to NrCompartments DO
    BEGIN
    Compartment[compi].Theta := Compartment[compi].FCadj/100;
    Simulation.ThetaIni[compi] := Compartment[compi].Theta;
    For celli := 1 TO SoilLayer[Compartment[compi].Layer].SCP1 DO
        BEGIN // salinity in cells
        Compartment[compi].Salt[celli] := 0.0;
        Compartment[compi].Depo[celli] := 0.0;
        END;
    END;
END; (* ResetSWCToFC *)



PROCEDURE AdjustSimPeriod;
VAR IniSimFromDayNr : LongInt;
    FullFileName : string;
BEGIN
IniSimFromDayNr := Simulation.FromDayNr;
CASE Simulation.LinkCropToSimPeriod OF
     true : BEGIN
            DetermineLinkedSimDay1(Crop.Day1,Simulation.FromDayNr);
            IF (Crop.Day1 = Simulation.FromDayNr)
               THEN Simulation.ToDayNr := Crop.DayN
               ELSE Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
            IF (GetClimFile() <> '(None)') THEN
               BEGIN
               IF (Simulation.ToDayNr > ClimRecord.ToDayNr) THEN
                   Simulation.ToDayNr := ClimRecord.ToDayNr;
               IF (Simulation.ToDayNr < ClimRecord.FromDayNr) THEN
                      Simulation.ToDayNr := ClimRecord.FromDayNr;
               END;
            END;
    false : BEGIN
            (*
            IF ((GetClimFile() <> '(None)') AND (Simulation.FromDayNr < ClimRecord.FromDayNr)) THEN
               BEGIN
               Simulation.FromDayNr := ClimRecord.FromDayNr;
               Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
               END; *)
            IF (Simulation.FromDayNr > Crop.Day1) THEN Simulation.FromDayNr := Crop.Day1;
            Simulation.ToDayNr := Crop.DayN;
            IF ((GetClimFile() <> '(None)') AND
                ((Simulation.FromDayNr <= ClimRecord.FromDayNr) OR (Simulation.FromDayNr >= ClimRecord.ToDayNr))) THEN
               BEGIN
               Simulation.FromDayNr := ClimRecord.FromDayNr;
               Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
               END;
            END;
    end;

// adjust initial depth and quality of the groundwater when required
IF ((NOT SimulParam.ConstGwt) AND (IniSimFromDayNr <> Simulation.FromDayNr)) THEN
   BEGIN
   IF (GetGroundWaterFile() = '(None)')
       THEN FullFileName := CONCAT(PathNameProg,'GroundWater.AqC')
       ELSE FullFileName := GetGroundWaterFileFull();
   // initialize ZiAqua and ECiAqua
   LoadGroundWater(FullFileName,Simulation.FromDayNr,ZiAqua,ECiAqua);
   CalculateAdjustedFC((ZiAqua/100),Compartment);
   IF Simulation.IniSWC.AtFC THEN ResetSWCToFC;
   END;
END; (* AdjustSimPeriod *)



PROCEDURE AdjustOnsetSearchPeriod;
BEGIN
IF (GetClimFile() = '(None)')
   THEN BEGIN
        Onset.StartSearchDayNr := 1;
        Onset.StopSearchDayNr := Onset.StartSearchDayNr + Onset.LengthSearchPeriod - 1;
        //Onset.StopSearchDayNr := 365;
        END
   ELSE BEGIN
        //Onset.StartSearchDayNr := ClimRecord.FromDayNr;
        //Onset.StopSearchDayNr := ClimRecord.ToDayNr;
        DetermineDayNr((1),(1),Simulation.YearStartCropCycle,Onset.StartSearchDayNr); // 1 January
        IF (Onset.StartSearchDayNr < ClimRecord.FromDayNr) THEN Onset.StartSearchDayNr := ClimRecord.FromDayNr;
        Onset.StopSearchDayNr := Onset.StartSearchDayNr + Onset.LengthSearchPeriod - 1;
        IF (Onset.StopSearchDayNr > ClimRecord.ToDayNr) THEN
           BEGIN
           Onset.StopSearchDayNr := ClimRecord.ToDayNr;
           Onset.LengthSearchPeriod := Onset.StopSearchDayNr - Onset.StartSearchDayNr + 1;
           END;
        END;
END; (* AdjustOnsetSearchPeriod *)


PROCEDURE SetClimData;
VAR SetARecord, SetBRecord : rep_clim;

BEGIN
ClimRecord.NrObs := 999; //(heeft geen belang)
                         // IF 365 (= full undefined year)

//Part A - ETo and Rain files --> ClimFile
IF ((GetEToFile() = '(None)') AND (GetRainFile() = '(None)'))
   THEN BEGIN
        SetClimFile('(None)');
        ClimDescription := 'Specify Climatic data when Running AquaCrop';
        WITH ClimRecord DO
          BEGIN
          DataType := Daily;
          FromString := 'any date';
          ToString := 'any date';
          FromY := 1901;
          END;
        END
   ELSE BEGIN
        SetClimFile('EToRainTempFile');
        ClimDescription := 'Read ETo/RAIN/TEMP data set';
        IF (GetEToFile() = '(None)') THEN
           WITH ClimRecord DO
           BEGIN
           FromY := RainRecord.FromY;
           FromDayNr := RainRecord.FromDayNr;
           ToDayNr := RainRecord.ToDayNr;
           FromString := RainRecord.FromString;
           ToString := RainRecord.ToString;
           IF FullUndefinedRecord(RainRecord.FromY,RainRecord.FromD,RainRecord.FromM,RainRecord.ToD,RainRecord.ToM)
              THEN NrObs := 365;
           END;
        IF (GetRainFile() = '(None)') THEN
           WITH ClimRecord DO
           BEGIN
           FromY := EToRecord.FromY;
           FromDayNr := EToRecord.FromDayNr;
           ToDayNr := EToRecord.ToDayNr;
           FromString := EToRecord.FromString;
           ToString := EToRecord.ToString;
           IF FullUndefinedRecord(EToRecord.FromY,EToRecord.FromD,EToRecord.FromM,EToRecord.ToD,EToRecord.ToM)
              THEN NrObs := 365;
           END;

        IF ((GetEToFile() <> '(None)') AND (GetRainFile() <> '(None)')) THEN
           BEGIN
           SetARecord := EToRecord;
           SetBRecord := RainRecord;
           IF ((EToRecord.FromY = 1901)
               AND FullUndefinedRecord(EToRecord.FromY,EToRecord.FromD,EToRecord.FromM,EToRecord.ToD,EToRecord.ToM))
               AND ((RainRecord.FromY = 1901)
               AND FullUndefinedRecord(RainRecord.FromY,RainRecord.FromD,RainRecord.FromM,RainRecord.ToD,RainRecord.ToM))
               THEN ClimRecord.NrObs := 365;

           IF ((EToRecord.FromY = 1901) AND (RainRecord.FromY <> 1901)) THEN
              BEGIN  // Jaartal van RainRecord ---> SetARecord (= EToRecord)
                     // FromY + adjust FromDayNr and FromString
              SetARecord.FromY := RainRecord.FromY;
              DetermineDayNr(EToRecord.FromD,EToRecord.FromM,SetARecord.FromY,SetARecord.FromDayNr);
              IF (((SetARecord.FromDayNr < RainRecord.FromDayNr)) AND (RainRecord.FromY < RainRecord.ToY)) THEN
                 BEGIN
                 SetARecord.FromY := RainRecord.FromY + 1;
                 DetermineDayNr(EToRecord.FromD,EToRecord.FromM,SetARecord.FromY,SetARecord.FromDayNr);
                 END;
              ClimRecord.FromY := SetARecord.FromY; // nodig voor DayString (werkt met ClimRecord)
              SetARecord.FromString := DayString(SetARecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
              IF (FullUndefinedRecord(EToRecord.FromY,EToRecord.FromD,EToRecord.FromM,EToRecord.ToD,EToRecord.ToM))
                 THEN SetARecord.ToY := RainRecord.ToY
                 ELSE SetARecord.ToY := SetARecord.FromY;
              DetermineDayNr(EToRecord.ToD,EToRecord.ToM,SetARecord.ToY,SetARecord.ToDayNr);
              SetARecord.ToString := DayString(SetARecord.ToDayNr);
              END;

           IF ((EToRecord.FromY <> 1901) AND (RainRecord.FromY = 1901)) THEN
              BEGIN  // Jaartal van EToRecord ---> SetBRecord (= RainRecord)
                     // FromY + adjust FromDayNr and FromString
              SetBRecord.FromY := EToRecord.FromY;
              DetermineDayNr(RainRecord.FromD,RainRecord.FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
              IF (((SetBRecord.FromDayNr < EToRecord.FromDayNr)) AND (EToRecord.FromY < EToRecord.ToY)) THEN
                 BEGIN
                 SetBRecord.FromY := EToRecord.FromY + 1;
                 DetermineDayNr(RainRecord.FromD,RainRecord.FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
                 END;
              ClimRecord.FromY := SetBRecord.FromY; // nodig voor DayString (werkt met ClimRecord)
              SetBRecord.FromString := DayString(SetBRecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
              IF (FullUndefinedRecord(RainRecord.FromY,RainRecord.FromD,RainRecord.FromM,RainRecord.ToD,RainRecord.ToM))
                 THEN SetBRecord.ToY := EToRecord.ToY
                 ELSE SetBRecord.ToY := SetBRecord.FromY;
              DetermineDayNr(RainRecord.ToD,RainRecord.ToM,SetBRecord.ToY,SetBRecord.ToDayNr);
              SetBRecord.ToString := DayString(SetBRecord.ToDayNr);
              END;

           // bepaal characteristieken van ClimRecord
           WITH ClimRecord DO
                BEGIN
                FromY := SetARecord.FromY;
                FromDayNr := SetARecord.FromDayNr;
                FromString := SetARecord.FromString;
                IF (FromDayNr < SetBRecord.FromDayNr) THEN
                        BEGIN
                        FromY := SetBRecord.FromY;
                        FromDayNr := SetBRecord.FromDayNr;
                        FromString := SetBRecord.FromString;
                        END;
                ToDayNr := SetARecord.ToDayNr;
                ToString := SetARecord.ToString;
                IF (ToDayNr > SetBRecord.ToDayNr) THEN
                        BEGIN
                        ToDayNr := SetBRecord.ToDayNr;
                        ToString := SetBRecord.ToString;
                        END;
                IF (ToDayNr < FromDayNr) THEN
                        BEGIN
                        SetClimFile('(None)');
                        ClimDescription := 'ETo data set <--NO OVERLAP--> RAIN data set';
                        NrObs := 0;
                        FromY := 1901;
                        END;
                END;
           END;
        END;


//Part B - ClimFile and Temperature files --> ClimFile
IF (TemperatureFile = '(None)')
   THEN BEGIN
        // no adjustments are required
        END
   ELSE BEGIN
        IF (GetClimFile() = '(None)')
           THEN BEGIN
                SetClimFile('EToRainTempFile');
                ClimDescription := 'Read ETo/RAIN/TEMP data set';
                WITH ClimRecord DO
                     BEGIN
                     FromY := TemperatureRecord.FromY;
                     FromDayNr := TemperatureRecord.FromDayNr;
                     ToDayNr := TemperatureRecord.ToDayNr;
                     FromString := TemperatureRecord.FromString;
                     ToString := TemperatureRecord.ToString;
                     IF ((TemperatureRecord.FromY = 1901) AND FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM))
                        THEN NrObs := 365
                        ELSE NrObs := TemperatureRecord.ToDayNr - TemperatureRecord.FromDayNr + 1;
                     END;
                END
           ELSE BEGIN
                DetermineDate(ClimRecord.FromDayNr,ClimRecord.FromD, ClimRecord.FromM, ClimRecord.FromY);
                DetermineDate(ClimRecord.ToDayNr,ClimRecord.ToD, ClimRecord.ToM, ClimRecord.ToY);
                SetARecord := ClimRecord;
                SetBRecord := TemperatureRecord;

                IF ((ClimRecord.FromY = 1901) AND (TemperatureRecord.FromY = 1901)
                   AND (ClimRecord.NrObs = 365)
                   AND FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM))
                       THEN ClimRecord.NrObs := 365
                       ELSE ClimRecord.NrObs := TemperatureRecord.ToDayNr - TemperatureRecord.FromDayNr + 1;

                IF ((ClimRecord.FromY = 1901) AND (TemperatureRecord.FromY <> 1901)) THEN
                   BEGIN  // Jaartal van TemperatureRecord ---> SetARecord (= ClimRecord)
                     // FromY + adjust FromDayNr and FromString
                   SetARecord.FromY := TemperatureRecord.FromY;
                   DetermineDayNr(ClimRecord.FromD,ClimRecord.FromM,SetARecord.FromY,SetARecord.FromDayNr);
                   IF (((SetARecord.FromDayNr < TemperatureRecord.FromDayNr)) AND (TemperatureRecord.FromY < TemperatureRecord.ToY)) THEN
                      BEGIN
                      SetARecord.FromY := TemperatureRecord.FromY + 1;
                      DetermineDayNr(ClimRecord.FromD,ClimRecord.FromM,SetARecord.FromY,SetARecord.FromDayNr);
                      END;
                   //ClimRecord.FromY := SetARecord.FromY; // nodig voor DayString (werkt met ClimRecord)
                   SetARecord.FromString := DayString(SetARecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
                   IF (FullUndefinedRecord(ClimRecord.FromY,ClimRecord.FromD,ClimRecord.FromM,ClimRecord.ToD,ClimRecord.ToM))
                      THEN SetARecord.ToY := TemperatureRecord.ToY
                      ELSE SetARecord.ToY := SetARecord.FromY;
                   DetermineDayNr(ClimRecord.ToD,ClimRecord.ToM,SetARecord.ToY,SetARecord.ToDayNr);
                   SetARecord.ToString := DayString(SetARecord.ToDayNr);
                   END;

                IF ((ClimRecord.FromY <> 1901) AND (TemperatureRecord.FromY = 1901)) THEN
                   BEGIN  // Jaartal van ClimRecord ---> SetBRecord (= TemperatureRecord)
                     // FromY + adjust FromDayNr and FromString
                   SetBRecord.FromY := ClimRecord.FromY;
                   DetermineDayNr(TemperatureRecord.FromD,TemperatureRecord.FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
                   IF (((SetBRecord.FromDayNr < ClimRecord.FromDayNr)) AND (ClimRecord.FromY < ClimRecord.ToY)) THEN
                      BEGIN
                      SetBRecord.FromY := ClimRecord.FromY + 1;
                      DetermineDayNr(TemperatureRecord.FromD,TemperatureRecord.FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
                      END;
                   //ClimRecord.FromY := SetBRecord.FromY; // nodig voor DayString (werkt met ClimRecord)
                   SetBRecord.FromString := DayString(SetBRecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
                   IF (FullUndefinedRecord(TemperatureRecord.FromY,TemperatureRecord.FromD,TemperatureRecord.FromM,TemperatureRecord.ToD,TemperatureRecord.ToM))
                      THEN SetBRecord.ToY := ClimRecord.ToY
                      ELSE SetBRecord.ToY := SetBRecord.FromY;
                   DetermineDayNr(TemperatureRecord.ToD,TemperatureRecord.ToM,SetBRecord.ToY,SetBRecord.ToDayNr);
                   SetBRecord.ToString := DayString(SetBRecord.ToDayNr);
                   END;

                // bepaal nieuwe characteristieken van ClimRecord
                WITH ClimRecord DO
                   BEGIN
                   FromY := SetARecord.FromY;
                   FromDayNr := SetARecord.FromDayNr;
                   FromString := SetARecord.FromString;
                   IF (FromDayNr < SetBRecord.FromDayNr) THEN
                        BEGIN
                        FromY := SetBRecord.FromY;
                        FromDayNr := SetBRecord.FromDayNr;
                        FromString := SetBRecord.FromString;
                        END;
                   ToDayNr := SetARecord.ToDayNr;
                   ToString := SetARecord.ToString;
                   IF (ToDayNr > SetBRecord.ToDayNr) THEN
                        BEGIN
                        ToDayNr := SetBRecord.ToDayNr;
                        ToString := SetBRecord.ToString;
                        END;
                   IF (ToDayNr < FromDayNr) THEN
                        BEGIN
                        SetClimFile('(None)');
                        ClimDescription := 'Clim data <--NO OVERLAP--> TEMPERATURE data';
                        NrObs := 0;
                        FromY := 1901;
                        END;
                   END;
                END;
        END;
END; (* SetClimData *)


PROCEDURE DetermineRootZoneWC(RootingDepth : double;
                              VAR ZtopSWCconsidered : BOOLEAN);
VAR CumDepth, Factor,frac_value,DrRel,DZtopRel,TopSoilInMeter : double;
    compi : INTEGER;
BEGIN
// calculate SWC in root zone
CumDepth := 0;
compi := 0;
SetRootZoneWC_Actual(0);
SetRootZoneWC_FC(0);
SetRootZoneWC_WP(0);
SetRootZoneWC_SAT(0);
SetRootZoneWC_Leaf(0);
SetRootZoneWC_Thresh(0);
SetRootZoneWC_Sen(0);
REPEAT
  compi := compi + 1;
  CumDepth := CumDepth + Compartment[compi].Thickness;
  IF (CumDepth <= RootingDepth)
     THEN Factor := 1
     ELSE BEGIN
          frac_value := RootingDepth - (CumDepth - Compartment[compi].Thickness);
          IF (frac_value > 0)
             THEN Factor := frac_value/Compartment[compi].Thickness
             ELSE Factor := 0;
          END;
  SetRootZoneWC_Actual(GetRootZoneWC().Actual
     + Factor * 1000 * Compartment[compi].Theta * Compartment[compi].Thickness
              * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  SetRootZoneWC_FC(GetRootZoneWC().FC
     + Factor * 10 * SoilLayer[Compartment[compi].Layer].FC * Compartment[compi].Thickness
              * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  SetRootZoneWC_Leaf(GetRootZoneWC().Leaf
     + Factor * 10 * Compartment[compi].Thickness * (SoilLayer[Compartment[compi].Layer].FC
     - Crop.pLeafAct * (SoilLayer[Compartment[compi].Layer].FC-SoilLayer[Compartment[compi].Layer].WP))
       * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  sETRootZoneWC_Thresh(GetRootZoneWC().Thresh
     + Factor * 10 * Compartment[compi].Thickness * (SoilLayer[Compartment[compi].Layer].FC
     - Crop.pActStom * (SoilLayer[Compartment[compi].Layer].FC-SoilLayer[Compartment[compi].Layer].WP))
       * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  SetRootZoneWC_Sen(GetRootZoneWC().Sen
     + Factor * 10 * Compartment[compi].Thickness * (SoilLayer[Compartment[compi].Layer].FC
     - Crop.pSenAct * (SoilLayer[Compartment[compi].Layer].FC-SoilLayer[Compartment[compi].Layer].WP))
       * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  SetRootZoneWC_WP(GetRootZoneWC().WP
     + Factor * 10 * SoilLayer[Compartment[compi].Layer].WP * Compartment[compi].Thickness
              * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
  SetRootZoneWC_SAT(GetRootZoneWC().SAT
     + Factor * 10 * SoilLayer[Compartment[compi].Layer].SAT * Compartment[compi].Thickness
              * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
UNTIL (CumDepth >= RootingDepth) OR (compi = NrCompartments);

// calculate SWC in top soil (top soil in meter = SimulParam.ThicknessTopSWC/100)
IF ((RootingDepth*100) <= SimulParam.ThicknessTopSWC)
   THEN BEGIN
        SetRootZoneWC_ZtopAct(GetRootZoneWC().Actual);
        SetRootZoneWC_ZtopFC(GetRootZoneWC().FC);
        SetRootZoneWC_ZtopWP(GetRootZoneWC().WP);
        SetRootZoneWC_ZtopThresh(GetRootZoneWC().Thresh);
        END
   ELSE BEGIN
        CumDepth := 0;
        compi := 0;
        SetRootZoneWC_ZtopAct(0);
        SetRootZoneWC_ZtopFC(0);
        SetRootZoneWC_ZtopWP(0);
        SetRootZoneWC_ZtopThresh(0);
        TopSoilInMeter := SimulParam.ThicknessTopSWC/100;
        REPEAT
          compi := compi + 1;
          CumDepth := CumDepth + Compartment[compi].Thickness;
          IF ((CumDepth*100) <= SimulParam.ThicknessTopSWC)
             THEN Factor := 1
             ELSE BEGIN
                  frac_value := TopSoilInMeter - (CumDepth - Compartment[compi].Thickness);
                  IF (frac_value > 0)
                     THEN Factor := frac_value/Compartment[compi].Thickness
                     ELSE Factor := 0;
                  END;
          SetRootZoneWC_ZtopAct(GetRootZoneWC().ZtopAct
            + Factor * 1000 * Compartment[compi].Theta * Compartment[compi].Thickness
                     * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
          SetRootZoneWC_ZtopFC(GetRootZoneWC().ZtopFC
            + Factor * 10 * SoilLayer[Compartment[compi].Layer].FC * Compartment[compi].Thickness
                     * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
          SetRootZoneWC_ZtopWP(GetRootZoneWC().ZtopWP
            + Factor * 10 * SoilLayer[Compartment[compi].Layer].WP * Compartment[compi].Thickness
                     * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
          SetRootZoneWC_ZtopThresh(GetRootZoneWC().ZtopThresh
            + Factor * 10 * Compartment[compi].Thickness * (SoilLayer[Compartment[compi].Layer].FC
            - Crop.pActStom * (SoilLayer[Compartment[compi].Layer].FC-SoilLayer[Compartment[compi].Layer].WP))
              * (1 - SoilLayer[Compartment[compi].Layer].GravelVol/100));
        UNTIL (CumDepth >= TopSoilInMeter) OR (compi = NrCompartments);
        END;

// Relative depletion in rootzone and in top soil
IF ROUND(1000*(GetRootZoneWc().FC - GetRootZoneWc().WP)) > 0
   THEN DrRel := (GetRootZoneWc().FC - GetRootZoneWC().Actual)/(GetRootZoneWc().FC - GetRootZoneWc().WP)
   ELSE DrRel := 0;
IF ROUND(1000*(GetRootZoneWC().ZtopFC - GetRootZoneWc().ZtopWP)) > 0
   THEN DZtopRel := (GetRootZoneWC().ZtopFC - GetRootZoneWc().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWc().ZtopWP)
   ELSE DZtopRel := 0;

// Zone in soil profile considered for determining stress response
IF (DZtopRel < DrRel)
   THEN ZtopSWCconsidered := true  // top soil is relative wetter than root zone
   ELSE ZtopSWCconsidered := false;
END; (* DetermineRootZoneWC *)



FUNCTION DayString(DNr : LongInt) : repstring17;
VAR dayi,monthi,yeari : INTEGER;
    strA, strB : string;
BEGIN
IF (GetClimFile() = '(None)') THEN WHILE (DNr > 365) DO DNr := DNr - 365;
DetermineDate(DNr,dayi,monthi,yeari);
Str(dayi:2,strA);
IF (ClimRecord.FromY = 1901)
   THEN strB := ''
   ELSE Str(yeari:4,strB);
StrB := CONCAT(TRIM(strA),' ',Trim(NameMonth[monthi]),' ',Trim(strB));
WHILE (Length(StrB) < 17) DO StrB := CONCAT(StrB,' ');
DayString := StrB;
END; (* DayString *)


FUNCTION CanopyCoverNoStressSF(DAP,L0,L123,LMaturity,GDDL0,GDDL123,GDDLMaturity : INTEGER;
                               CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
                               TypeDays : rep_modeCycle;
                               SFRedCGC,SFRedCCx : ShortInt) : double;


FUNCTION CanopyCoverNoStressDaysSF(DAP,L0,L123,LMaturity : INTEGER;
                                 CCo,CCx,CGC,CDC : double;
                                 SFRedCGC,SFRedCCx : ShortInt) : double;
VAR CC,CCxAdj,CDCadj : double;
    t : INTEGER;

BEGIN (* CanopyCoverNoStressDaysSF *)
CC := 0.0;
t := DAP - Simulation.DelayedDays;
// CC refers to canopy cover at the end of the day

IF ((t >= 1) AND (t <= LMaturity) AND (CCo > 0)) THEN
   BEGIN
   IF (t <= L0) // before germination or recovering of transplant
      THEN CC := 0
      ELSE BEGIN
           IF (t < L123) // Canopy development and Mid-season stage
              THEN CC := CCatTime((t-L0),CCo,((1-SFRedCGC/100)*CGC),((1-SFRedCCx/100)*CCx))
              ELSE BEGIN // Late-season stage  (t <= LMaturity)
                   IF (CCx < 0.001)
                      THEN CC := 0
                      ELSE BEGIN
                           CCxAdj := CCatTime((L123-L0),CCo,((1-SFRedCGC/100)*CGC),((1-SFRedCCx/100)*CCx));
                           CDCadj := CDC*(CCxAdj+2.29)/(CCx+2.29);
                           IF (CCxAdj < 0.001)
                              THEN CC := 0
                              //ELSE CC := CCxAdj * (1 - 0.05*(exp((t-L123)*CDC*3.33*((CCxAdj+2.29)/(CCx+2.29))/(CCxAdj+2.29))-1));
                              ELSE CC := CCxAdj * (1 - 0.05*(exp((t-L123)*3.33*CDCAdj/(CCxAdj+2.29))-1));
                           END;
                   END;
           END;
   END;
IF (CC > 1) THEN CC := 1;
IF (CC < 0) THEN CC := 0;
CanopyCoverNoStressDaysSF := CC;
END; (* CanopyCoverNoStressDaysSF *)


BEGIN (* CanopyCoverNoStressSF *)
CASE TypeDays OF
     GDDays : CanopyCoverNoStressSF := CanopyCoverNoStressGDDaysSF(GDDL0,GDDL123,GDDLMaturity,
                                                  SumGDD,CCo,CCx,GDDCGC,GDDCDC,SFRedCGC,SFRedCCx);
     else CanopyCoverNoStressSF := CanopyCoverNoStressDaysSF(DAP,L0,L123,LMaturity,
                                               CCo,CCx,CGC,CDC,SFRedCGC,SFRedCCx);
     end;
END; (* CanopyCoverNoStressSF *)


PROCEDURE ReadSoilSettings;
VAR f : textfile;
    FullName : string;
    i : ShortInt;
BEGIN
FullName := CONCAT(PathNameSimul,'Soil.PAR');
Assign(f,FullName);
Reset(f);
READLN(f,SimulParam.RunoffDepth); //considered depth (m) of soil profile for calculation of mean soil water content
READLN(f,i);   // correction CN for Antecedent Moisture Class
IF (i = 1) THEN SimulParam.CNcorrection := true
           ELSE SimulParam.CNcorrection := false;
READLN(f,SimulParam.SaltDiff); // salt diffusion factor (%)
READLN(f,SimulParam.SaltSolub); // salt solubility (g/liter)
READLN(f,SimulParam.RootNrDF); // shape factor capillary rise factor
// new Version 4.1
READLN(f,SimulParam.IniAbstract); // Percentage of S for initial abstraction for surface runoff
SimulParam.IniAbstract := 5; // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
Close(f);
END; (* ReadSoilSettings *)


FUNCTION HarvestIndexDay(DAP  : LongInt;
                         DaysToFlower,HImax : integer;
                         dHIdt,CCi,CCxadjusted : double;
                         PercCCxHIfinal        : ShortInt;
                         TempPlanting : rep_Planting;
                         VAR PercentLagPhase : ShortInt;
                         VAR HIfinal : INTEGER)   : double;

CONST HIo = 1;
VAR HIGC,HIday,HIGClinear : double;
    t,tMax,tSwitch : Integer;

BEGIN
t := DAP - Simulation.DelayedDays - DaysToFlower;
//Simulation.WPyON := false;
PercentLagPhase := 0;
IF (t <= 0)
   THEN HIday := 0
   ELSE BEGIN
        IF ((Crop.Subkind = Vegetative) AND (TempPlanting = Regrowth)) THEN dHIdt := 100;
        IF ((Crop.Subkind = Forage) AND (TempPlanting = Regrowth)) THEN dHIdt := 100;
        IF (dHIdt > 99)
           THEN BEGIN
                HIday := HImax;
                PercentLagPhase := 100;
                END
           ELSE BEGIN
                HIGC := HarvestIndexGrowthCoefficient(HImax,dHIdt);
                GetDaySwitchToLinear(HImax,dHIdt,HIGC,tSwitch,HIGClinear);
                IF (t < tSwitch)
                   THEN BEGIN
                        PercentLagPhase := ROUND(100 * (t/tSwitch));
                        HIday := (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*t));
                        END
                   ELSE BEGIN
                        PercentLagPhase := 100;
                        IF ((Crop.subkind = Tuber) OR (Crop.subkind = Vegetative) OR (Crop.subkind = Forage))
                           THEN BEGIN // continue with logistic equation
                                HIday := (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*t));
                                IF (HIday >= 0.9799*HImax) THEN HIday := HImax;
                                END
                           ELSE BEGIN // switch to linear increase
                                HIday := (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*tSwitch));
                                HIday := Hiday + HIGClinear*(t-tSwitch);
                                END;
                        END;
                IF (HIday > HImax) THEN HIday := HImax;
                IF (HIday <= (HIo + 0.4)) THEN HIday := 0;
                IF ((HImax - HIday) < 0.4) THEN HIday := HImax;
                END;

        // adjust HIfinal if required for inadequate photosynthesis (unsufficient green canopy)
        tMax := ROUND(HImax/dHIdt);
        IF ((HIfinal = HImax) AND (t <= tmax) AND (CCi <= (PercCCxHIfinal/100))
            AND (Crop.subkind <> Vegetative) AND (Crop.subkind <> Forage))
                THEN HIfinal := ROUND(HIday);
        IF (HIday > HIfinal) THEN HIday := HIfinal;
        END;
HarvestIndexDay := HIday;

END; (* HarvestIndexDay *)


PROCEDURE ReadRainfallSettings;
VAR f : textfile;
    FullName : string;
    NrM : ShortInt;
BEGIN
FullName := CONCAT(PathNameSimul,'Rainfall.PAR');
Assign(f,FullName);
Reset(f);
Readln(f); //Settings for processing 10-day or monthly rainfall data
WITH SimulParam DO
   BEGIN
   Readln(f,NrM);
   Case NrM OF
        0 : EffectiveRain.Method := Full;
        1 : EffectiveRain.Method := USDA;
        2 : EffectiveRain.Method := Percentage;
      end;
   Readln(f,EffectiveRain.PercentEffRain); // IF Method is Percentage
   Readln(f,EffectiveRain.ShowersInDecade);  // For estimation of surface run-off
   Readln(f,EffectiveRain.RootNrEvap); // For reduction of soil evaporation
   END;
Close(f);
END; (* ReadRainfallSettings *)


PROCEDURE ReadCropSettingsParameters;
VAR f : textfile;
    FullName : string;
BEGIN
FullName := CONCAT(PathNameSimul,'Crop.PAR');
Assign(f,FullName);
Reset(f);
WITH SimulParam DO
   BEGIN
   Readln(f,EvapDeclineFactor); // evaporation decline factor in stage 2
   Readln(f,KcWetBare); //Kc wet bare soil [-]
   Readln(f,PercCCxHIfinal); // CC threshold below which HI no longer increase(% of 100)
   Readln(f,RootPercentZmin); //Starting depth of root sine function (% of Zmin)
   Readln(f,MaxRootZoneExpansion); // cm/day
   MaxRootZoneExpansion := 5.00; // fixed at 5 cm/day
   Readln(f,KsShapeFactorRoot); // Shape factor for effect water stress on rootzone expansion
   Readln(f,TAWGermination);  // Soil water content (% TAW) required at sowing depth for germination
   Readln(f,pAdjFAO); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
   Readln(f,DelayLowOxygen); //number of days for full effect of deficient aeration
   Readln(f,ExpFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
   Readln(f,Beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
   Readln(f,ThicknessTopSWC); // Thickness top soil (cm) in which soil water depletion has to be determined
   END;
Close(f);
END; (* ReadCropSettingsParameters *)


PROCEDURE ReadFieldSettingsParameters;
VAR f : textfile;
    FullName : string;
BEGIN
FullName := CONCAT(PathNameSimul,'Field.PAR');
Assign(f,FullName);
Reset(f);
WITH SimulParam DO
   BEGIN
   Readln(f,EvapZmax); //maximum water extraction depth by soil evaporation [cm]
   END;
Close(f);
END; (* ReadFieldSettingsParameters *)


PROCEDURE ReadTemperatureSettingsParameters;
VAR f0 : text;
    FullName : string;
BEGIN
FullName := CONCAT(PathNameSimul,'Temperature.PAR');
Assign(f0,FullName);
Reset(f0);
Readln(f0);
WITH SimulParam DO
   BEGIN
   Readln(f0,Tmin);   //Default minimum temperature (degC) if no temperature file is specified
   Readln(f0,Tmax);   //Default maximum temperature (degC) if no temperature file is specified
   Readln(f0,GDDMethod); //Default method for GDD calculations
   IF (GDDMethod > 3) THEN GDDMethod := 3;
   IF (GDDMethod < 1) THEN GDDMethod := 1;
   END;
Close(f0);
END; (* ReadTemperatureSettingsParameters *)

FUNCTION AdjustedKsStoToECsw(ECeMin,ECeMax : ShortInt;
                             ResponseECsw : INTEGER;
                             ECei,ECswi,ECswFCi,Wrel,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,KsStoIN : double) : double;
VAR ECswRel,LocalKsShapeFactorSalt,
    KsSalti,SaltStressi,StoClosure,KsStoOut : double;
BEGIN
IF ((ResponseECsw > 0) AND (Wrel > 0) AND (Simulation.SalinityConsidered = true))
   THEN BEGIN  //adjustment to ECsw considered
        ECswRel := ECswi - (ECswFCi - ECei) + (ResponseECsw-100)*Wrel;
        IF ((ECswRel > ECeMin) AND (ECswRel < ECeMax))
           THEN BEGIN
                // stomatal closure at ECsw relative
                LocalKsShapeFactorSalt := +3; // CONVEX give best ECsw response
                KsSalti := KsSalinity(Simulation.SalinityConsidered,ECeMin,ECeMax,ECswRel,LocalKsShapeFactorSalt);
                SaltStressi := (1-KsSalti)*100;
                StoClosure := Coeffb0Salt + Coeffb1Salt * SaltStressi + Coeffb2Salt * SaltStressi * SaltStressi;
                // adjusted KsSto
                KsStoOut := (1 - StoClosure/100);
                IF (KsStoOut < 0) THEN KsStoOut := 0;
                IF (KsStoOut > KsStoIN) THEN KsStoOut := KsStoIN;
                END
           ELSE BEGIN
                IF (ECswRel >= ECeMax)
                   THEN KsStoOut := 0 // full stress
                   ELSE KsStoOut := KsStoIN; // no extra stress
                END;
        END
   ELSE KsStoOut := KsStoIN;  // no adjustment to ECsw
AdjustedKsStoToECsw := KsStoOut;
END; (* AdjustedKsStoToECsw *)




PROCEDURE DetermineRootZoneSaltContent(RootingDepth : double;
                                       VAR ZrECe,ZrECsw,ZrECswFC,ZrKsSalt : double);
VAR CumDepth, Factor,frac_value : double;
    compi : INTEGER;
BEGIN
CumDepth := 0;
compi := 0;
ZrECe := 0;
ZrECsw := 0;
ZrECswFC := 0;
ZrKsSalt := 1;
IF (RootingDepth >= Crop.RootMin)
   THEN BEGIN
        REPEAT
        compi := compi + 1;
        CumDepth := CumDepth + Compartment[compi].Thickness;
        IF (CumDepth <= RootingDepth)
           THEN Factor := 1
           ELSE BEGIN
                frac_value := RootingDepth - (CumDepth - Compartment[compi].Thickness);
                IF (frac_value > 0)
                   THEN Factor := frac_value/Compartment[compi].Thickness
                   ELSE Factor := 0;
                END;
        Factor := Factor * (Compartment[compi].Thickness)/RootingDepth; // weighting factor
        ZrECe := ZrECe + Factor * ECeComp(Compartment[compi]);
        ZrECsw := ZrECsw + Factor * ECswComp(Compartment[compi],(false)); // not at FC
        ZrECswFC := ZrECswFC + Factor * ECswComp(Compartment[compi],(true)); // at FC
        UNTIL (CumDepth >= RootingDepth) OR (compi = NrCompartments);
        IF (((Crop.ECemin <> undef_int) AND (Crop.ECemax <> undef_int)) AND (Crop.ECemin < Crop.ECemax))
           THEN ZrKsSalt := KsSalinity((true),Crop.ECemin,Crop.ECemax,ZrECe,(0.0))
           ELSE ZrKsSalt := KsSalinity((false),Crop.ECemin,Crop.ECemax,ZrECe,(0.0));
        END
   ELSE BEGIN
        ZrECe := undef_int;
        ZrECsw := undef_int;
        ZrECswFC := undef_int;
        ZrKsSalt := undef_int;
        END;
END;  (* DetermineRootZoneSaltContent *)


FUNCTION CO2ForSimulationPeriod(FromDayNr,ToDayNr : LongInt) : double;
VAR i,Dayi,Monthi,FromYi,ToYi : INTEGER;
    f0 : textfile;
    TempString : string;
    CO2From,CO2To,CO2a,CO2b,YearA,YearB : double;
BEGIN
DetermineDate(FromDayNr,Dayi,Monthi,FromYi);
DetermineDate(ToDayNr,Dayi,Monthi,ToYi);
IF ((FromYi = 1901) OR (ToYi = 1901))
   THEN CO2ForSimulationPeriod := CO2Ref
   ELSE BEGIN
        Assign(f0,CO2FileFull);
        Reset(f0);
        FOR i:= 1 TO 3 DO Readln(f0); // Description and Title
        // from year
        Readln(f0,TempString);
        SplitStringInTwoParams(TempString,YearB,CO2b);
        IF (ROUND(YearB) >= FromYi)
           THEN BEGIN
                CO2From := CO2b;
                YearA := YearB;
                CO2a := CO2b;
                END
           ELSE BEGIN
                REPEAT
                  YearA := YearB;
                  CO2a := Co2b;
                  Readln(f0,TempString);
                  SplitStringInTwoParams(TempString,YearB,CO2b);
                UNTIL ((ROUND(YearB) >= FromYi) OR EoF(f0));
                IF (FromYi > ROUND(YearB))
                   THEN CO2From := CO2b
                   ELSE CO2From := CO2a + (CO2b-CO2a)*(ROUND(FromYi)-ROUND(YearA))/(ROUND(YearB)-ROUND(YearA));
                END;
        // to year
        CO2To := CO2From;
        IF (ToYi > FromYi) AND (ToYi > ROUND(YearA)) THEN
           BEGIN
           IF (ROUND(YearB) >= ToYi)
              THEN CO2To := CO2a + (CO2b-CO2a)*(ROUND(ToYi)-ROUND(YearA))/(ROUND(YearB)-ROUND(YearA))
              ELSE IF (NOT EoF(f0))THEN
                     BEGIN
                     REPEAT
                       YearA := YearB;
                       CO2a := Co2b;
                       Readln(f0,TempString);
                       SplitStringInTwoParams(TempString,YearB,CO2b);
                     UNTIL ((ROUND(YearB) >= ToYi) OR EoF(f0));
                     IF (ToYi > ROUND(YearB))
                        THEN CO2To := CO2b
                        ELSE CO2To := CO2a + (CO2b-CO2a)*(ROUND(ToYi)-ROUND(YearA))/(ROUND(YearB)-ROUND(YearA));
                     END;
           END;
        Close(f0);
        CO2ForSimulationPeriod := (CO2From+CO2To)/2;
        END;
END; (* CO2ForSimulationPeriod *)


FUNCTION CCiNoWaterStressSF(Dayi,L0,L12SF,L123,L1234,
                            GDDL0,GDDL12SF,GDDL123,GDDL1234  : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
                            SFRedCGC,SFRedCCx : ShortInt;
                            SFCDecline : Double;
                            TheModeCycle : rep_modeCycle) : double;

VAR CCi,CCibis,CCxAdj,CDCadj,GDDCDCadj : double;

BEGIN
// Calculate CCi
CCi := CanopyCoverNoStressSF(Dayi,L0,L123,L1234,GDDL0,GDDL123,GDDL1234,
                             CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD,TheModeCycle,
                             SFRedCGC,SFRedCCX);

// Consider CDecline for limited soil fertiltiy
//IF ((Dayi > L12SF) AND (SFCDecline > 0.000001))
IF ((Dayi > L12SF) AND (SFCDecline > 0.000001) AND (L12SF < L123))
   THEN BEGIN
        IF (Dayi < L123)
           THEN BEGIN
                IF (TheModeCycle = CalendarDays)
                   THEN CCi := CCi - (SFCDecline/100) * exp(2*Ln(Dayi-L12SF))/(L123-L12SF)
                   ELSE BEGIN
                        IF ((SumGDD > GDDL12SF) AND (GDDL123 > GDDL12SF)) THEN
                        CCi := CCi - (RatDGDD*SFCDecline/100)
                              * exp(2*Ln(SumGDD-GDDL12SF))/(GDDL123-GDDL12SF);
                        END;
                IF (CCi < 0) THEN CCi := 0;
                END
           ELSE BEGIN
                IF (TheModeCycle = CalendarDays)
                   THEN BEGIN
                        CCi := CCatTime((L123-L0),CCo,(CGC*(1-SFRedCGC/100)),((1-SFRedCCX/100)*CCx));
                        // CCibis is CC in late season when Canopy decline continues
                        CCibis := CCi  - (SFCDecline/100) * (exp(2*Ln(Dayi-L12SF))/(L123-L12SF));
                        IF (CCibis < 0)
                           THEN CCi := 0
                           ELSE CCi := CCi  - ((SFCDecline/100) * (L123-L12SF));
                        IF (CCi < 0.001)
                         THEN CCi := 0
                         ELSE BEGIN
                              CCxAdj := CCi; // is CCx at start of late season, adjusted for canopy decline with soil fertility stress
                              CDCadj := CDC * (CCxAdj + 2.29)/(CCx + 2.29);
                              IF (Dayi < (L123 + LengthCanopyDecline(CCxAdj,CDCadj)))
                                 THEN BEGIN
                                      CCi := CCxAdj * (1 - 0.05*(exp((Dayi-L123)*3.33*CDCadj/(CCxAdj+2.29))-1));
                                      IF (CCibis < CCi) THEN CCi := CCibis; // accept smallest Canopy Cover
                                      END
                                 ELSE CCi := 0;
                              END;
                        END
                   ELSE BEGIN
                        CCi := CCatTime((GDDL123-GDDL0),CCo,(GDDCGC*(1-SFRedCGC/100)),((1-SFRedCCX/100)*CCx));
                        // CCibis is CC in late season when Canopy decline continues
                        IF ((SumGDD > GDDL12SF) AND (GDDL123 > GDDL12SF))
                           THEN CCibis := CCi  -
                               (RatDGDD*SFCDecline/100) * (exp(2*Ln(SumGDD-GDDL12SF))/(GDDL123-GDDL12SF))
                           ELSE CCibis := CCi;
                        IF (CCibis < 0)
                           THEN CCi := 0
                           ELSE CCi := CCi - ((RatDGDD*SFCDecline/100) * (GDDL123-GDDL12SF));
                        IF (CCi < 0.001)
                           THEN CCi := 0
                           ELSE BEGIN
                                CCxAdj := CCi; // is CCx at start of late season, adjusted for canopy decline with soil fertility stress
                                GDDCDCadj := GDDCDC * (CCxAdj + 2.29)/(CCx + 2.29);
                                IF (SumGDD < (GDDL123 + LengthCanopyDecline(CCxAdj,GDDCDCadj)))
                                   THEN BEGIN
                                        CCi := CCxAdj * (1 - 0.05*(exp((SumGDD-GDDL123)*3.33*GDDCDCadj/(CCxAdj+2.29))-1));
                                        IF (CCibis < CCi) THEN CCi := CCibis; // accept smallest Canopy Cover
                                        END
                                   ELSE CCi := 0;
                                END;
                        END;
                IF (CCi < 0) THEN CCi := 0;
                END;
        END;

CCiNoWaterStressSF := CCi;
END; (* CCiNoWaterStressSF *)



FUNCTION SeasonalSumOfKcPot(TheDaysToCCini,TheGDDaysToCCini,
                            L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,
                            Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,CO2i : double;
                            TheModeCycle : rep_modeCycle) : double;
CONST EToStandard = 5;
VAR SumGDD,GDDi,SumKcPot,KsB,SumGDDforPlot,SumGDDfromDay1 : double;
    Tndayi, Txdayi,CCi,CCxWitheredForB,TpotForB,EpotTotForB : double;
    CCinitial,DayFraction, GDDayFraction : double;
    DayCC,Tadj,GDDTadj : INTEGER;
    fTemp : textFile;
    Dayi : INTEGER;
    GrowthON : BOOLEAN;

BEGIN
// 1. Open Temperature file
IF (TemperatureFile <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(PathNameSimul,'TCrop.SIM'));
   Reset(fTemp);
   END;

// 2. Initialise global settings
Simulation.DelayedDays := 0; // required for CalculateETpot
SumKcPot := 0;
SumGDDforPlot := undef_int;
SumGDD := undef_int;
SumGDDfromDay1 := 0;
GrowthON := false;
GDDTadj := undef_int;
DayFraction := undef_int;
GDDayFraction := undef_int;
// 2.bis Initialise 1st day
IF (TheDaysToCCini <> 0)
   THEN BEGIN  // regrowth
        IF (TheDaysToCCini = undef_int)
           THEN BEGIN // CCx on 1st day
                Tadj := L12 - L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := GDDL12 - GDDL0;
                   SumGDD := GDDL12;
                   END;
                CCinitial := CCx;
                END
           ELSE BEGIN // CC on 1st day is < CCx
                Tadj := TheDaysToCCini;
                DayCC := Tadj + L0;
                IF (TheModeCycle = GDDays) THEN
                   BEGIN
                   GDDTadj := TheGDDaysToCCini;
                   SumGDD := GDDL0 + TheGDDaysToCCini;
                   SumGDDforPlot := SumGDD;
                   END;
                CCinitial := CanopyCoverNoStressSF(DayCC,L0,L123,L1234,
                                  GDDL0,GDDL123,GDDL1234,CCo,CCx,CGC,CDC,
                                  GDDCGC,GDDCDC,SumGDDforPlot,TheModeCycle,(0),(0));
                END;
        // Time reduction for days between L12 and L123
        DayFraction := (L123-L12)/(Tadj + L0 + (L123-L12) );
        IF (TheModeCycle = GDDays)
           THEN GDDayFraction := (GDDL123-GDDL12)/(GDDTadj + GDDL0 + (GDDL123-GDDL12));
        END
   ELSE BEGIN  // sowing or transplanting
        Tadj := 0;
        IF (TheModeCycle = GDDays) THEN
           BEGIN
           GDDTadj := 0;
           SumGDD := 0;
           END;
        CCinitial := CCo;
        END;

// 3. Calculate Sum
FOR Dayi := 1 TO L1234 DO
    BEGIN
    // 3.1 calculate growing degrees for the day
    IF (TemperatureFile <> '(None)')
       THEN BEGIN
            READLN(fTemp,Tndayi,Txdayi);
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,SimulParam.GDDMethod);
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,SimulParam.GDDMethod);
    IF (TheModeCycle = GDDays) THEN
       BEGIN
       SumGDD := SumGDD + GDDi;
       SumGDDfromDay1 := SumGDDfromDay1 + GDDi;
       END;

    // 3.2 calculate CCi
    IF (GrowthON = false)
       THEN BEGIN // not yet canopy development
            CCi := 0;
            DayCC := Dayi;
            IF (TheDaysToCCini <> 0)
               THEN BEGIN // regrowth on 1st day
                    CCi := CCinitial;
                    GrowthON := true;
                    END
               ELSE BEGIN // wait for day of germination or recover of transplant
                    IF (TheModeCycle = CalendarDays)
                       THEN BEGIN
                            IF (Dayi = (L0+1)) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END
                       ELSE BEGIN
                            IF (SumGDD > GDDL0) THEN
                               BEGIN
                               CCi := CCinitial;
                               GrowthON := true;
                               END;
                            END;
                    END;
            END
       ELSE BEGIN
            IF (TheDaysToCCini = 0)
               THEN DayCC := Dayi
               ELSE BEGIN
                    DayCC := Dayi + Tadj + L0; // adjusted time scale
                    IF (DayCC > L1234) THEN DayCC := L1234; // special case where L123 > L1234
                    IF (DayCC > L12) THEN
                       BEGIN
                       IF (Dayi <= L123)
                          THEN DayCC := L12 + ROUND(DayFraction * (Dayi+Tadj+L0 - L12)) // slow down
                          ELSE DayCC := Dayi; // switch time scale
                       END;
                    END;
            IF (TheModeCycle = GDDays)
               THEN BEGIN
                    IF (TheGDDaysToCCini = 0)
                       THEN SumGDDforPlot := SumGDDfromDay1
                       ELSE BEGIN
                            SumGDDforPlot := SumGDD;
                            IF (SumGDDforPlot > GDDL1234) THEN SumGDDforPlot := GDDL1234; // special case where L123 > L1234
                            IF (SumGDDforPlot > GDDL12) THEN
                               BEGIN
                               IF (SumGDDfromDay1 <= GDDL123)
                                  THEN SumGDDforPlot := GDDL12 + ROUND(GDDayFraction * (SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12)) // slow down
                                  ELSE SumGDDforPlot := SumGDDfromDay1 // switch time scale
                               END
                            END;
                    END;
            CCi := CanopyCoverNoStressSF(DayCC,L0,L123,L1234,GDDL0,GDDL123,GDDL1234,
                           CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDDforPlot,TheModeCycle,(0),(0));
            END;

    // 3.3 calculate CCxWithered
    CCxWitheredForB := CCi;
    IF (Dayi >= L12) THEN CCxWitheredForB := CCx;

    // 3.4 Calculate Tpot + Adjust for Low temperature (no transpiration)
    IF (CCi > 0.0001)
       THEN CalculateETpot(DayCC,L0,L12,L123,L1234,(0),
            CCi,EToStandard,KcTop,KcDeclAgeing,CCx,CCxWitheredForB,CCeffectProcent,CO2i,GDDi,GDtranspLow,TpotForB,EpotTotForB)
       ELSE TpotForB := 0;

    // 3.5 Sum of Sum Of KcPot
    SumKcPot := SumKcPot + (TpotForB/EToStandard);
    END;

// 5. Close Temperature file
IF (TemperatureFile <> '(None)') THEN Close(fTemp);

// 6. final sum
SeasonalSumOfKcPot := SumKcPot;

END; (* SeasonalSumOfKcPot *)





PROCEDURE TranslateIniLayersToSWProfile(NrLay : ShortInt;
                                        LayThickness,LayVolPr,LayECdS : rep_IniComp;
                                        NrComp : INTEGER;
                                        VAR Comp : rep_Comp);
VAR Compi,Layeri,i : ShortInt;
    SDLay,SDComp,FracC : double;
    GoOn : BOOLEAN;

BEGIN // from specific layers to Compartments
FOR Compi := 1 TO NrComp DO
    BEGIN
    Comp[Compi].Theta := 0;
    Comp[Compi].WFactor := 0;  // used for ECe in this procedure
    END;
Compi := 0;
SDComp := 0;
Layeri := 1;
SDLay := LayThickness[1];
GoOn := true;
WHILE (Compi < NrComp) DO
  BEGIN
  FracC := 0;
  Compi := Compi + 1;
  SDComp := SDComp + Comp[compi].Thickness;
  IF (SDLay >= SDComp)
     THEN BEGIN
          Comp[Compi].Theta := Comp[Compi].Theta + (1-FracC)*LayVolPr[Layeri]/100;
          Comp[Compi].WFactor := Comp[Compi].WFactor + (1-FracC)*LayECdS[Layeri];
          END
     ELSE BEGIN // go to next layer
          WHILE ((SDLay < SDComp) AND GoOn) DO
            BEGIN
            //finish handling previous layer
            FracC := (SDLay - (SDComp-Comp[Compi].Thickness))/(Comp[Compi].Thickness) - FracC;
            Comp[Compi].Theta := Comp[Compi].Theta + FracC*LayVolPr[Layeri]/100;
            Comp[Compi].WFactor := Comp[Compi].WFactor + FracC*LayECdS[Layeri];
            FracC := (SDLay - (SDComp-Comp[Compi].Thickness))/(Comp[Compi].Thickness);
            //add next layer
            IF (Layeri < NrLay)
               THEN BEGIN
                    Layeri := Layeri + 1;
                    SDLay := SDLay + LayThickness[Layeri];
                    END
               ELSE GoOn := false;
            END;
          Comp[Compi].Theta := Comp[Compi].Theta + (1-FracC)*LayVolPr[Layeri]/100;
          Comp[Compi].WFactor := Comp[Compi].WFactor + (1-FracC)*LayECdS[Layeri];
          END;
  END; // next Compartment
IF (NOT GoOn) THEN
   FOR i := (Compi+1) TO NrComp DO
       BEGIN
       Comp[i].Theta := LayVolPr[NrLay]/100;
       Comp[i].WFactor := LayECdS[NrLay];
       END;

// final check of SWC
FOR Compi := 1 TO NrComp DO
    IF (Comp[Compi].Theta > (SoilLayer[Comp[compi].Layer].SAT)/100)
        THEN Comp[Compi].Theta := (SoilLayer[Comp[compi].Layer].SAT)/100;
// salt distribution in cellls
For Compi := 1 TO NrComp DO DetermineSaltContent(Comp[Compi].WFactor,Comp[Compi]);
END; (* TranslateIniLayersToSWProfile *)



PROCEDURE TranslateIniPointsToSWProfile(NrLoc : ShortInt;
                                        LocDepth,LocVolPr,LocECdS : rep_IniComp;
                                        NrComp : INTEGER;
                                        VAR Comp : rep_Comp);
VAR Compi,Loci : ShortInt;
    TotD,Depthi,D1,D2,Th1,Th2,DTopComp,ThTopComp,ThBotComp : double;
    EC1,EC2,ECTopComp,ECBotComp : double;
    AddComp,TheEnd : BOOLEAN;
BEGIN
TotD := 0;
For Compi := 1 TO NrComp DO
    BEGIN
    Comp[Compi].Theta := 0;
    Comp[Compi].WFactor := 0;  // used for salt in (10*VolSat*dZ * EC)
    TotD := TotD + Comp[Compi].Thickness;
    END;
Compi := 0;
Depthi := 0;
AddComp := true;
Th2 := LocVolPr[1];
EC2 := LocECds[1];
D2 := 0;
Loci := 0;
WHILE ((Compi < NrComp) OR ((Compi = NrComp) AND (AddComp = false))) DO
  BEGIN
  // upper and lower boundaries location
  D1 := D2;
  Th1 := Th2;
  EC1 := EC2;
  IF (Loci < NrLoc)
     THEN BEGIN
          Loci := Loci + 1;
          D2 := LocDepth[Loci];
          Th2 := LocVolPr[Loci];
          EC2 := LocECdS[Loci];
          END
     ELSE D2 := TotD;
  // transfer water to compartment (SWC in mm) and salt in (10*VolSat*dZ * EC)
  TheEnd := false;
  DTopComp := D1;  //Depthi is the bottom depth
  ThBotComp := Th1;
  ECBotComp := EC1;
  REPEAT
    ThTopComp := ThBotComp;
    ECTopComp := ECBotComp;
    IF AddComp THEN
       BEGIN
       Compi := Compi + 1;
       Depthi := Depthi + Comp[Compi].Thickness;
       END;
    IF (Depthi < D2)
       THEN BEGIN
            ThBotComp := Th1 + (Th2-Th1)*(Depthi-D1)/(D2-D1);
            Comp[Compi].Theta := Comp[Compi].Theta
                                 + 10*(Depthi-DTopComp)*((ThTopComp+ThBotComp)/2);
            ECBotComp := EC1 + (EC2-EC1)*(Depthi-D1)/(D2-D1);
            Comp[Compi].WFactor := Comp[Compi].WFactor
                     + (10*(Depthi-DTopComp)*SoilLayer[Comp[Compi].Layer].SAT)*((ECTopComp+ECbotComp)/2);
            AddComp := true;
            DTopComp := Depthi;
            IF (Compi = NrComp) THEN TheEnd := true;
            END
       ELSE BEGIN
            ThBotComp := Th2;
            ECBotComp := EC2;
            Comp[Compi].Theta := Comp[Compi].Theta
                                 + 10*(D2-DTopComp)*((ThTopComp+ThBotComp)/2);
            Comp[Compi].WFactor := Comp[Compi].WFactor
                               + (10*(D2-DTopComp)*SoilLayer[Comp[Compi].Layer].SAT)*((ECTopComp+ECbotComp)/2);
            IF (Depthi = D2)
               THEN AddComp := true
               ELSE AddComp := false;
            TheEnd := true;
            END;
  UNTIL TheEnd;
  END;

For Compi := 1 TO NrComp DO // from mm(water) to theta and final check
    BEGIN
    Comp[Compi].Theta := Comp[Compi].Theta/(1000*Comp[Compi].Thickness);
    IF (Comp[Compi].Theta > (SoilLayer[Comp[compi].Layer].SAT)/100)
        THEN Comp[Compi].Theta := (SoilLayer[Comp[compi].Layer].SAT)/100;
    IF (Comp[Compi].Theta < 0) THEN Comp[Compi].Theta := 0;
    END;

For Compi := 1 TO NrComp DO // from (10*VolSat*dZ * EC) to ECe and distribution in cellls
    BEGIN
    Comp[Compi].WFactor := Comp[Compi].WFactor/(10*Comp[Compi].Thickness*SoilLayer[Comp[Compi].Layer].SAT);
    DetermineSaltContent(Comp[Compi].WFactor,Comp[Compi]);
    END;
END; (* TranslateIniPointsToSWProfile *)


PROCEDURE LoadInitialConditions(SWCiniFileFull : string;
                                VAR IniSurfaceStorage : double;
                                VAR IniSWCRead : rep_IniSWC);
VAR f0 : TextFile;
    i : ShortInt;
    StringParam : string;
    VersionNr : double;
BEGIN
Assign(f0,SWCiniFileFull);
Reset(f0);
READLN(f0,SWCiniDescription);
READLN(f0,VersionNr); // AquaCrop Version
IF (ROUND(10*VersionNr) < 41) // initial CC at start of simulation period
   THEN Simulation.CCini := undef_int
   ELSE READLN(f0,Simulation.CCini);
IF (ROUND(10*VersionNr) < 41) // B produced before start of simulation period
   THEN Simulation.Bini := 0.000
   ELSE READLN(f0,Simulation.Bini);
IF (ROUND(10*VersionNr) < 41) // initial rooting depth at start of simulation period
   THEN Simulation.Zrini := undef_int
   ELSE READLN(f0,Simulation.Zrini);
READLN(f0,IniSurfaceStorage);
IF (ROUND(10*VersionNr) < 32) // EC of the ini surface storage
   THEN Simulation.ECStorageIni := 0
   ELSE READLN(f0,Simulation.ECStorageIni);
READLN(f0,i);
IF (i = 1)
   THEN IniSWCRead.AtDepths := true
   ELSE IniSWCRead.AtDepths := false;
READLN(f0,IniSWCRead.NrLoc);
READLN(f0);
READLN(f0);
READLN(f0);
FOR i := 1 TO IniSWCRead.NrLoc DO
    BEGIN
    READLN(f0,StringParam);
    IF (ROUND(10*VersionNr) < 32) // ECe at the locations
       THEN BEGIN
            SplitStringInTwoParams(StringParam,IniSWCRead.Loc[i],IniSWCRead.VolProc[i]);
            IniSWCRead.SaltECe[i] := 0;
            END
       ELSE SplitStringInThreeParams(StringParam,IniSWCRead.Loc[i],IniSWCRead.VolProc[i],IniSWCRead.SaltECe[i]);
    END;
Close(f0);
Simulation.IniSWC.AtFC := false;
END; (* LoadInitialConditions *)


PROCEDURE LoadProjectDescription(FullNameProjectFile : string;
                                 VAR DescriptionOfProject : string);
VAR f0 : TextFile;
BEGIN
Assign(f0,FullNameProjectFile);
Reset(f0);
READLN(f0,DescriptionOfProject);
DescriptionOfProject := Trim(DescriptionOfProject);
Close(f0);
END; (* LoadProjectDescription *)


PROCEDURE ComposeOutputFileName(TheProjectFileName : string);
VAR TempString : string;
    i : ShortInt;
BEGIN
TempString := Trim(TheProjectFileName);
i := Length(TempString);
Delete(TempString,(i-3),(4));
OutputName := TempString;
END; (* ComposeOutputFileName *)

PROCEDURE CheckForKeepSWC(FullNameProjectFile : string;
                          TotalNrOfRuns : INTEGER;
                          VAR RunWithKeepSWC : BOOLEAN;
                          VAR ConstZrxForRun : double);
VAR f0,fx : TextFile;
    i,Runi : INTEGER;
    FileName,PathName,FullFileName : string;
    Zrni,Zrxi,ZrSoili : double;
    VersionNrCrop : double;
    TheNrSoilLayers : ShortInt;
    TheSoilLayer : rep_SoilLayer;
    PreviousProfFilefull : string;

BEGIN
//1. Initial settings
RunWithKeepSWC := false;
ConstZrxForRun := undef_int;

//2. Open project file
Assign(f0,FullNameProjectFile);
Reset(f0);
READLN(f0); // Description
READLN(f0);  // AquaCrop version Nr
FOR i := 1 TO 5 DO READLN(f0); // Type Year, and Simulation and Cropping period of run 1

//3. Look for restrictive soil layer
//restricted to run 1 since with KeepSWC, the soil file has to be common between runs
PreviousProfFilefull := GetProfFilefull(); // keep name soil file (to restore after check)
FOR i := 1 TO 27 DO READLN(f0); // Climate (5x3 = 15),Calendar (3),Crop (3), Irri (3) and Field (3) file
READLN(f0); // info Soil file
READLN(f0,FileName);
READLN(f0,PathName);
PathName := StringReplace(PathName, '"', '', [rfReplaceAll]);
FullFileName := CONCAT(Trim(PathName),Trim(FileName));
LoadProfile(FullFileName);
TheNrSoilLayers := Soil.NrSoilLayers;
TheSoilLayer := SoilLayer;
//ZrRestrict := 1000; // assumed not to be so far a restriction
(*
Assign(fx,FullFileName);
Reset(fx);
FOR i := 1 TO 5 DO READLN(fx);
READLN(fx,ZrRestrict); // depth restrictive soil layer inhibiting root zone expansion
IF (ZrRestrict < 0)
   THEN ZrRestrict := 1000 // undefined, HENCE very large (no restriction)
   ELSE IF (ZrRestrict <= 1.20) THEN ConstZrxForRun := ZrRestrict;
Close(fx); *)

//3.bis  groundwater file
FOR i := 1 TO 3 DO READLN(f0);

//4. Check if runs with KeepSWC exist
Runi := 1;
WHILE (RunWithKeepSWC = false) AND (Runi <= TotalNrOfRuns) DO
   BEGIN
   IF (Runi > 1) THEN FOR i := 1 TO 47 DO READLN(f0);  // 5 + 42 lines with files
   READLN(f0); // info Initial Conditions file
   READLN(f0,FileName);
   READLN(f0); //Pathname
   IF (Trim(FileName) = 'KeepSWC') THEN RunWithKeepSWC := true;
   Runi := Runi + 1;
   END;
IF (RunWithKeepSWC = false) THEN ConstZrxForRun := undef_int; // reset

//5. Look for maximum root zone depth IF RunWithKeepSWC
//IF (RunWithKeepSWC AND (ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict))) THEN
IF (RunWithKeepSWC = true) THEN
   BEGIN
   Reset(f0);
   READLN(f0); // Description
   READLN(f0);  // AquaCrop version Nr
   FOR i := 1 TO 5 DO READLN(f0); // Type Year, and Simulation and Cropping period of run 1
   Runi := 1;
   //WHILE ((ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict)) AND (Runi <= TotalNrOfRuns)) DO
   WHILE (Runi <= TotalNrOfRuns) DO
     BEGIN
     // Simulation and Cropping period
     IF (Runi > 1) THEN FOR i := 1 TO 5 DO READLN(f0);
     // 5 Climate files (15) + Calendar file (3)
     FOR i := 1 TO 18 DO READLN(f0);
     // Crop file
     READLN(f0); // Crop file title
     READLN(f0,FileName);
     READLN(f0,PathName);
     PathName := StringReplace(PathName, '"', '', [rfReplaceAll]);
     FullFileName := CONCAT(Trim(PathName),Trim(FileName));
     Assign(fx,FullFileName);
     Reset(fx);
     READLN(fx); // description
     READLN(fx,VersionNrCrop);
     IF (Round(VersionNrCrop*10) <= 31)
        THEN FOR i := 1 TO 29 DO READLN(fx)  // no Salinity stress (No Reponse Stomata + ECemin + ECemax + ShapeKsSalt)
        ELSE BEGIN
             IF (Round(VersionNrCrop*10) <= 50)
                THEN FOR i := 1 TO 32 DO READLN(fx) // no distortion to salinity and response to ECsw factor
                ELSE FOR i := 1 TO 34 DO READLN(fx);
             END;
     READLN(fx,Zrni); // minimum rooting depth
     READLN(fx,Zrxi); // maximum rooting depth
     ZrSoili := RootMaxInSoilProfile(Zrxi,TheNrSoilLayers,TheSoilLayer);
     IF (ZrSoili > ConstZrxForRun) THEN ConstZrxForRun := ZrSoili;
     Close(fx);
     // Remaining files: Irri (3), Field (3), Soil (3), Gwt (3), Inni (3), Off (3) and FieldData (3) file
     FOR i := 1 TO 21 DO READLN(f0);
     Runi := Runi + 1;
     END;
   END;
Close(f0);

//6. Reload existing soil file
SetProfFilefull(PreviousProfFilefull);
LoadProfile(GetProfFilefull());
END; (* CheckForKeepSWC *)




PROCEDURE AdjustThetaInitial(PrevNrComp : ShortInt;
                             PrevThickComp,PrevVolPrComp,PrevECdSComp : rep_IniComp);
VAR layeri,compi : INTEGER;
    TotDepthC,TotDepthL,Total : Double;

BEGIN
//1. Actual total depth of compartments
TotDepthC := 0;
FOR compi := 1 to NrCompartments DO TotDepthC := TotDepthC + Compartment[compi].Thickness;

//2. Stretch thickness of bottom soil layer if required
TotDepthL := 0;
For layeri := 1 to Soil.NrSoilLayers DO TotDepthL := TotDepthL + SoilLayer[layeri].Thickness;
IF (TotDepthC > TotDepthL) THEN SoilLayer[Soil.NrSoilLayers].Thickness := SoilLayer[Soil.NrSoilLayers].Thickness + (TotDepthC - TotDepthL);

//3. Assign a soil layer to each soil compartment
DesignateSoilLayerToCompartments(NrCompartments,Soil.NrSoilLayers,Compartment);

//4. Adjust initial Soil Water Content of soil compartments
IF Simulation.ResetIniSWC
   THEN BEGIN
        IF Simulation.IniSWC.AtDepths
           THEN TranslateIniPointsToSWProfile(Simulation.IniSWC.NrLoc,Simulation.IniSWC.Loc,Simulation.IniSWC.VolProc,
                                              Simulation.IniSWC.SaltECe,NrCompartments,Compartment)
           ELSE TranslateIniLayersToSWProfile(Simulation.IniSWC.NrLoc,Simulation.IniSWC.Loc,Simulation.IniSWC.VolProc,
                                              Simulation.IniSWC.SaltECe,NrCompartments,Compartment);
        END
   ELSE TranslateIniLayersToSWProfile(PrevNrComp,PrevThickComp,PrevVolPrComp,PrevECdSComp,NrCompartments,Compartment);

//5. Adjust watercontent in soil layers and determine ThetaIni
Total := 0;
FOR layeri := 1 TO Soil.NrSoilLayers DO SoilLayer[layeri].WaterContent := 0;
FOR compi := 1 TO NrCompartments DO
    BEGIN
    Simulation.ThetaIni[compi] := Compartment[compi].Theta;
    SoilLayer[Compartment[compi].Layer].WaterContent := SoilLayer[Compartment[compi].Layer].WaterContent
                                                                + Simulation.ThetaIni[compi]*100*10*Compartment[compi].Thickness;
    END;
FOR layeri := 1 TO Soil.NrSoilLayers DO Total := Total + SoilLayer[layeri].WaterContent;
TotalWaterContent.BeginDay := Total;
END; (* AdjustThetaInitial *)




PROCEDURE AdjustSizeCompartments(CropZx : double);
VAR i ,compi : INTEGER;
    TotDepthC,fAdd : Double;
    PrevNrComp : ShortInt;
    PrevThickComp,PrevVolPrComp,PrevECdSComp : rep_IniComp;
BEGIN

//1. Save intial soil water profile (required when initial soil water profile is NOT reset at start simulation - see 7.)
PrevNrComp := NrCompartments;
FOR compi := 1 To prevnrComp DO
    BEGIN
    PrevThickComp[compi] := Compartment[compi].Thickness;
    PrevVolPrComp[compi] := 100*Compartment[compi].Theta;
    END;

//2. Actual total depth of compartments
TotDepthC := 0;
FOR i := 1 to NrCompartments DO TotDepthC := TotDepthC + Compartment[i].Thickness;

//3. Increase number of compartments (if less than 12)
IF (NrCompartments < 12) THEN
   REPEAT
   NrCompartments := NrCompartments + 1;
   IF ((CropZx - TotDepthC) > SimulParam.CompDefThick)
      THEN Compartment[NrCompartments].Thickness := SimulParam.CompDefThick
      ELSE Compartment[NrCompartments].Thickness := CropZx - TotDepthC;
   TotDepthC := TotDepthC + Compartment[NrCompartments].Thickness;
   UNTIL ((NrCompartments = max_No_compartments) OR ((TotDepthC + 0.00001) >= CropZx));

//4. Adjust size of compartments (if total depth of compartments < rooting depth)
IF ((TotDepthC + 0.00001) < CropZx) THEN
   BEGIN
   NrCompartments := 12;
   fAdd := (CropZx/0.1 - 12)/78;
   FOR i := 1 TO 12 DO
       BEGIN
       Compartment[i].Thickness := 0.1 * (1 + i*fAdd);
       Compartment[i].Thickness := 0.05 * ROUND(Compartment[i].Thickness * 20);
       END;
   TotDepthC := 0;
   FOR i := 1 to NrCompartments DO TotDepthC := TotDepthC + Compartment[i].Thickness;
   IF (TotDepthC < CropZx)
      THEN REPEAT
           Compartment[12].Thickness := Compartment[12].Thickness + 0.05;
           TotDepthC := TotDepthC + 0.05;
           UNTIL (TotDepthC >= CropZx)
      ELSE WHILE ((TotDepthC - 0.04999999) >= CropZx) DO
               BEGIN
               Compartment[12].Thickness := Compartment[12].Thickness - 0.05;
               TotDepthC := TotDepthC - 0.05;
               END;
   END;

//5. Adjust soil water content and theta initial
AdjustThetaInitial(PrevNrComp,PrevThickComp,PrevVolPrComp,PrevECdSComp);
END; (* AdjustSizeCompartments *)


PROCEDURE CheckForWaterTableInProfile(DepthGWTmeter : double;
                                     ProfileComp : rep_comp;
                                     VAR WaterTableInProfile : BOOLEAN);
Var Ztot, Zi : double;
    compi : INTEGER;
BEGIN
WaterTableInProfile := false;
Ztot := 0;
compi := 0;
IF (DepthGWTmeter >= 0) THEN  // groundwater table is present
   REPEAT
   compi := compi + 1;
   Ztot := Ztot + ProfileComp[compi].Thickness;
   Zi := Ztot - ProfileComp[compi].Thickness/2;
   IF (Zi >= DepthGWTmeter) THEN WaterTableInProfile := true;
   UNTIL ((WaterTableInProfile = true) OR (compi >= NrCompartments));
END; (* CheckForWaterTableInProfile *)




PROCEDURE LoadGroundWater(FullName : string;
                          AtDayNr : LongInt;
                          VAR Zcm : INTEGER;
                          VAR ECdSm : double);
VAR f0 : TextFile;
    i,dayi,monthi,yeari,Year1Gwt : INTEGER;
    DayNr1Gwt,DayNr1,DayNr2,DayNrN : LongInt;
    StringREAD : ShortString;
    DayDouble,Z1,EC1,Z2,EC2,ZN,ECN : double;
    TheEnd : BOOLEAN;

    PROCEDURE FindValues(AtDayNr,DayNr1,DayNr2 : LongInt;
                         Z1,EC1,Z2,EC2 : double;
                         VAR Zcm : INTEGER;
                         VAR ECdSm : double);
    BEGIN
    Zcm := ROUND(100 * (Z1 + (Z2-Z1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1)));
    ECdSm := EC1 + (EC2-EC1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1);
    END; (* FindValues *)

BEGIN
// initialize
TheEnd := false;
Year1Gwt := 1901;
DayNr1 := 1;
DayNr2 := 1;
Assign(f0,FullName);
Reset(f0);
READLN(f0,GroundWaterDescription);
READLN(f0); // AquaCrop Version

// mode groundwater table
READLN(f0,i);
CASE i OF
     0 : BEGIN // no groundwater table
         Zcm := undef_int;
         ECdSm := undef_int;
         SimulParam.ConstGwt := true;
         TheEnd := true;
         END;
     1 : BEGIN // constant groundwater table
         SimulParam.ConstGwt := true;
         END;
     else SimulParam.ConstGwt := false;
     end;

// first day of observations (only for variable groundwater table)
IF (NOT SimulParam.ConstGwt) THEN
   BEGIN
   READLN(f0,dayi);
   READLN(f0,monthi);
   READLN(f0,Year1Gwt);
   DetermineDayNr(dayi,monthi,Year1Gwt,DayNr1Gwt);
   END;

// single observation (Constant Gwt) or first observation (Variable Gwt)
IF (i > 0) THEN  // groundwater table is present
   BEGIN
   READLN(f0);
   READLN(f0);
   READLN(f0);
   READLN(f0,StringREAD);
   SplitStringInThreeParams(StringREAD,DayDouble,Z2,EC2);
   IF ((i = 1) OR (Eof(f0)))
      THEN BEGIN // Constant groundwater table or single observation
           Zcm := ROUND(100*Z2);
           ECdSm := EC2;
           TheEnd := true;
           END
      ELSE DayNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
   END;

// other observations
IF (NOT TheEnd) THEN // variable groundwater table with more than 1 observation
   BEGIN
   // adjust AtDayNr
   DetermineDate(AtDayNr,dayi,monthi,yeari);
   IF ((yeari = 1901) AND (Year1Gwt <> 1901)) THEN // Make AtDayNr defined
      DetermineDayNr(dayi,monthi,Year1Gwt,AtDayNr);
   IF ((yeari <> 1901) AND (Year1Gwt = 1901)) THEN // Make AtDayNr undefined
      DetermineDayNr(dayi,monthi,Year1Gwt,AtDayNr);
   // get observation at AtDayNr
   IF (Year1Gwt <> 1901)
      THEN BEGIN // year is defined
           IF (AtDayNr <= DayNr2)
              THEN BEGIN
                   Zcm := ROUND(100*Z2);
                   ECdSm := EC2;
                   END
              ELSE BEGIN
                   WHILE (NOT TheEnd) DO
                      BEGIN
                      DayNr1 := DayNr2;
                      Z1 := Z2;
                      EC1 := EC2;
                      READLN(f0,StringREAD);
                      SplitStringInThreeParams(StringREAD,DayDouble,Z2,EC2);
                      DayNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
                      IF (AtDayNr <= DayNr2) THEN
                         BEGIN
                         FindValues(AtDayNr,DayNr1,DayNr2,Z1,EC1,Z2,EC2,Zcm,ECdSm);
                         TheEnd := true;
                         END;
                      IF ((Eof(f0)) AND (NOT TheEnd)) THEN
                         BEGIN
                         Zcm := ROUND(100*Z2);
                         ECdSm := EC2;
                         TheEnd := true;
                         END;
                      END;
                   END;
           END
      ELSE BEGIN // year is undefined
           IF (AtDayNr <= DayNr2)
              THEN BEGIN
                   DayNr2 := DayNr2 + 365;
                   AtDayNr := AtDayNr + 365;
                   WHILE (NOT Eof(f0)) DO
                      BEGIN
                      READLN(f0,StringREAD);
                      SplitStringInThreeParams(StringREAD,DayDouble,Z1,EC1);
                      DayNr1 := DayNr1Gwt + ROUND(DayDouble) - 1;
                      END;
                   FindValues(AtDayNr,DayNr1,DayNr2,Z1,EC1,Z2,EC2,Zcm,ECdSm);
                   END
              ELSE BEGIN
                   DayNrN := DayNr2 + 365;
                   ZN := Z2;
                   ECN := EC2;
                   WHILE (NOT TheEnd) DO
                      BEGIN
                      DayNr1 := DayNr2;
                      Z1 := Z2;
                      EC1 := EC2;
                      READLN(f0,StringREAD);
                      SplitStringInThreeParams(StringREAD,DayDouble,Z2,EC2);
                      DayNr2 := DayNr1Gwt + ROUND(DayDouble) - 1;
                      IF (AtDayNr <= DayNr2) THEN
                         BEGIN
                         FindValues(AtDayNr,DayNr1,DayNr2,Z1,EC1,Z2,EC2,Zcm,ECdSm);
                         TheEnd := true;
                         END;
                      IF ((Eof(f0)) AND (NOT TheEnd)) THEN
                         BEGIN
                         FindValues(AtDayNr,DayNr2,DayNrN,Z2,EC2,ZN,ECN,Zcm,ECdSm);
                         TheEnd := true;
                         END;
                      END;
                   END;
           END;
   END; // variable groundwater table with more than 1 observation
Close(f0);
END; (* LoadGroundWater *)


FUNCTION CCmultiplierWeedAdjusted(ProcentWeedCover : ShortInt;
                                  CCxCrop,FshapeWeed,fCCx : double;
                                  Yeari,MWeedAdj : ShortInt;
                                  VAR RCadj : ShortInt) : double;
VAR fWeedi,CCxTot100,CCxTot0,CCxTotM,fweedMax,RCadjD,FshapeMinimum : double;

BEGIN
fWeedi := 1;
RCadj := ProcentWeedCover;
IF (ProcentWeedCover > 0) THEN
   BEGIN
   fweedi := CCmultiplierWeed(ProcentWeedCover,CCxCrop,FshapeWeed);
   // FOR perennials when self-thinning
   IF ((Crop.subkind = Forage) AND (Yeari > 1) and (fCCx < 0.995)) THEN
      BEGIN //need for adjustment
      // step 1 - adjusment of shape factor to degree of crop replacement by weeds
      FshapeMinimum := 10 - 20*( (exp(fCCx*3)-1)/(exp(3)-1) + sqr(MWeedAdj/100));
      IF (ROUND(FshapeMinimum*10) = 0) THEN FshapeMinimum := 0.1;
      IF (FshapeWeed < FshapeMinimum) THEN FshapeWeed := FshapeMinimum;

      // step 2 - Estimate of CCxTot
      // A. Total CC (crop and weeds) when self-thinning and 100% weed take over
      fweedi := CCmultiplierWeed(ProcentWeedCover,CCxCrop,FshapeWeed);
      CCxTot100 := fweedi * CCxCrop;
      // B. Total CC (crop and weeds) when self-thinning and 0% weed take over
      IF (fCCx > 0.005)
         THEN fweedi := CCmultiplierWeed(ROUND(fCCx*ProcentWeedCover),(fCCx*CCxCrop),FshapeWeed)
         ELSE fweedi := 1;
      CCxTot0 := fweedi * (fCCx*CCxCrop);
      // C. total CC (crop and weeds) with specified weed take over (MWeedAdj)
      CCxTotM := CCxTot0 + (CCxTot100 - CCxTot0)* MWeedAdj/100;
      IF (CCxTotM < (fCCx*CCxCrop*(1-ProcentWeedCover/100)))
         THEN CCxTotM := fCCx*CCxCrop*(1-ProcentWeedCover/100);
      IF (fCCx > 0.005) THEN
         BEGIN
         fweedi := CCxTotM/(fCCx*CCxCrop);
         fweedMax := 1/(fCCx*CCxCrop);
         IF (ROUND(fweedi*1000) > ROUND(fWeedMax*1000)) THEN fweedi := fweedMax;
         END;

      // step 3 - Estimate of adjusted weed cover
      RCadjD := ProcentWeedCover + (1-fCCx)*CCxCrop*MWeedAdj;
      IF (fCCx > 0.005) THEN
         BEGIN
         IF (RCadjD < (100*(CCxTotM - fCCx*CCxCrop)/CCxTotM))
            THEN RCadjD := 100*(CCxTotM - fCCx*CCxCrop)/CCxTotM;
         IF (RCadjD > (100 * (1- (fCCx*CCxCrop*(1-ProcentWeedCover/100)/CCxTotM))))
            THEN RCadjD := 100*(1- fCCx*CCxCrop*(1-ProcentWeedCover/100)/CCxTotM);
         END;
      RCadj := ROUND(RCadjD);
      IF (RCadj > 100) THEN RCadj := 100;
      END;
   END;
CCmultiplierWeedAdjusted := fWeedi;
END; (* CCmultiplierWeedAdjusted *)


PROCEDURE AdjustYearPerennials(TheYearSeason: ShortInt;
                               Sown1stYear : BOOLEAN;
                               TheCycleMode : rep_modeCycle;
                               Zmax,ZminYear1,TheCCo,TheSizeSeedling,
                               TheCGC,TheCCx,TheGDDCGC : double;
                               ThePlantingDens : INTEGER;
                               VAR TypeOfPlanting : rep_Planting;
                               VAR Zmin,TheSizePlant,TheCCini : double;
                               VAR TheDaysToCCini,TheGDDaysToCCini : INTEGER);
BEGIN
IF (TheYearSeason = 1)
   THEN BEGIN
        IF (Sown1stYear = true) // planting
           THEN TypeOfPlanting := Seed
           ELSE TypeOfPlanting := Transplant;
        Zmin := ZminYear1;  // rooting depth
        END
   ELSE BEGIN
        TypeOfPlanting := Regrowth; // planting
        Zmin := Zmax;  // rooting depth
        // plant size by regrowth
        IF (ROUND(100*TheSizePlant) < ROUND(100*TheSizeSeedling))
           THEN TheSizePlant := 10 * TheSizeSeedling;
        IF (ROUND(100*TheSizePlant) > ROUND((100*TheCCx*10000)/(ThePlantingDens/10000)))
           THEN TheSizePlant := (TheCCx*10000)/(ThePlantingDens/10000); // adjust size plant to maximum possible
        END;
TheCCini := (ThePlantingDens/10000) * (TheSizePlant/10000);
TheDaysToCCini := TimeToCCini(TypeOfPlanting,ThePlantingDens,TheSizeSeedling,TheSizePlant,TheCCx,TheCGC);
IF (TheCycleMode = GDDays)
   THEN TheGDDaysToCCini := TimeToCCini(TypeOfPlanting,ThePlantingDens,TheSizeSeedling,TheSizePlant,TheCCx,TheGDDCGC)
   ELSE TheGDDaysToCCini := undef_int;
END;  (* AdjustYearPerennials *)




PROCEDURE NoCropCalendar;
BEGIN
SetCalendarFile('(None)');
CalendarFileFull := GetCalendarFile();  (* no file *)
CalendarDescription := '';
Onset.GenerateOn := false;
Onset.GenerateTempOn := false;
EndSeason.GenerateTempOn := false;
CalendarDescription := 'No calendar for the Seeding/Planting year';
END; (* NoCropCalendar *)



PROCEDURE LoadCropCalendar(FullName : string;
                           VAR GetOnset,GetOnsetTemp : BOOLEAN;
                           VAR DayNrStart : LongInt;
                           YearStart : INTEGER);

VAR f0 : TextFile;
    Onseti : ShortInt;
    Dayi,Monthi,Yeari,CriterionNr : INTEGER;
    DayNr : LongInt;
BEGIN
GetOnset := false;
GetOnsetTemp := false;
Assign(f0,FullName);
Reset(f0);
READLN(f0,CalendarDescription);
READLN(f0); // AquaCrop Version

// Specification of Onset and End growing season
READLN(f0,Onseti);  // specification of the onset

// Onset growing season
IF (Onseti = 0)
   THEN BEGIN  // onset on a specific day
        READLN(f0); // start search period - not applicable
        READLN(f0); // length search period - not applicable
        READLN(f0,DayNr); // day-number
        DetermineDate(DayNr,Dayi,Monthi,Yeari);
        DetermineDayNr(Dayi,Monthi,YearStart,DayNrStart);
        END
   ELSE BEGIN // onset is generated
        GetOnset := true;
        READLN(f0); // start search period
        READLN(f0); // length search period
        READLN(f0,CriterionNr); // criterion number to decide if based on rainfall or air temperature
        IF (CriterionNr > 10) THEN GetOnsetTemp := true;
        END;
Close(f0);
END; (* LoadCropCalendar *)


PROCEDURE GetFileForProgramParameters(TheFullFileNameProgram : string;
                                      VAR FullFileNameProgramParameters : string);
VAR TheLength : INTEGER;
    TheExtension : STRING;
BEGIN
FullFileNameProgramParameters := '';
TheLength := Length(TheFullFileNameProgram);
TheExtension := Copy(TheFullFileNameProgram,(TheLength-2),3); // PRO or PRM
FullFileNameProgramParameters := Copy(TheFullFileNameProgram,1,(TheLength-3));
IF (TheExtension = 'PRO')
   THEN FullFileNameProgramParameters := CONCAT(FullFileNameProgramParameters,'PP1')
   ELSE FullFileNameProgramParameters := CONCAT(FullFileNameProgramParameters,'PPn');
END; (* GetFileForProgramParameters *)


PROCEDURE LoadProgramParametersProject(FullFileNameProgramParameters : string);
VAR f0 : TextFile;
    i : INTEGER;
BEGIN
IF FileExists(FullFileNameProgramParameters)
   THEN BEGIN // load set of program parameters
        Assign(f0,FullFileNameProgramParameters);
        Reset(f0);
        WITH SimulParam DO
          BEGIN
          // crop
          Readln(f0,EvapDeclineFactor); // evaporation decline factor in stage 2
          Readln(f0,KcWetBare); //Kc wet bare soil [-]
          Readln(f0,PercCCxHIfinal); // CC threshold below which HI no longer increase(% of 100)
          Readln(f0,RootPercentZmin); //Starting depth of root sine function (% of Zmin)
          Readln(f0,MaxRootZoneExpansion); // cm/day
          MaxRootZoneExpansion := 5.00; // fixed at 5 cm/day
          Readln(f0,KsShapeFactorRoot); // Shape factor for effect water stress on rootzone expansion
          Readln(f0,TAWGermination);  // Soil water content (% TAW) required at sowing depth for germination
          Readln(f0,pAdjFAO); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
          Readln(f0,DelayLowOxygen); //number of days for full effect of deficient aeration
          Readln(f0,ExpFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
          Readln(f0,Beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
          Readln(f0,ThicknessTopSWC);  // Thickness top soil (cm) in which soil water depletion has to be determined
          // field
          Readln(f0,EvapZmax); //maximum water extraction depth by soil evaporation [cm]
          // soil
          READLN(f0,SimulParam.RunoffDepth); //considered depth (m) of soil profile for calculation of mean soil water content
          READLN(f0,i);   // correction CN for Antecedent Moisture Class
          IF (i = 1)
             THEN SimulParam.CNcorrection := true
             ELSE SimulParam.CNcorrection := false;
          READLN(f0,SimulParam.SaltDiff); // salt diffusion factor (%)
          READLN(f0,SimulParam.SaltSolub); // salt solubility (g/liter)
          READLN(f0,SimulParam.RootNrDF); // shape factor capillary rise factor
          SimulParam.IniAbstract := 5; // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
          // Temperature
          Readln(f0,Tmin);   //Default minimum temperature (degC) if no temperature file is specified
          Readln(f0,Tmax);   //Default maximum temperature (degC) if no temperature file is specified
          Readln(f0,GDDMethod); //Default method for GDD calculations
          IF (GDDMethod > 3) THEN GDDMethod := 3;
          IF (GDDMethod < 1) THEN GDDMethod := 1;
          // Rainfall
          Readln(f0,i);
          Case i OF
            0 : EffectiveRain.Method := Full;
            1 : EffectiveRain.Method := USDA;
            2 : EffectiveRain.Method := Percentage;
            end;
          Readln(f0,EffectiveRain.PercentEffRain); // IF Method is Percentage
          Readln(f0,EffectiveRain.ShowersInDecade);  // For estimation of surface run-off
          Readln(f0,EffectiveRain.RootNrEvap); // For reduction of soil evaporation
          END;
        // close
        Close(f0);
        END
   ELSE BEGIN // take the default set of program parameters
        ReadSoilSettings;
        ReadRainfallSettings;
        ReadCropSettingsParameters;
        ReadFieldSettingsParameters;
        ReadTemperatureSettingsParameters;
        END;
END; (* LoadProgramParametersProject *)


end.
