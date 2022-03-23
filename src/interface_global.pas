unit interface_global;


interface


const
    Equiv = 0.64;   // conversion factor: 1 dS/m = 0.64 g/l
    max_SoilLayers = 5;
    max_No_compartments = 12;
    undef_double = -9.9;
    undef_int = -9;
    CO2Ref = 369.41;
    EvapZmin = 15; //cm  minimum soil depth for water extraction by evaporation
    ElapsedDays : ARRAY[1..12] of double = (0,31,59.25,90.25,120.25,151.25,181.25,
                                                212.25,243.25,273.25,304.25,334.25);
    DaysInMonth : ARRAY[1..12] of integer = (31,28,31,30,31,30,31,31,30,31,30,31);
    NameMonth : ARRAY[1..12] of string = ('January','February','March','April',
          'May','June','July','August','September','October','November','December');

type
    rep_TypeObsSim =(ObsSimCC,ObsSimB,ObsSimSWC);
    repTypeProject = (TypePRO,TypePRM,TypeNone);
    Pdouble = ^double;

    rep_string25 = string[25]; (* Description SoilLayer *)
    repstring17 = string[17]; (* Date string *)

    rep_salt = ARRAY[1..11] of double; (* saltcontent in g/m2 *)

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

    rep_Comp = ARRAY[1.. max_No_compartments] of CompartmentIndividual;

    rep_subkind = (Vegetative,Grain,Tuber,Forage);
    rep_pMethod = (NoCorrection,FAOCorrection);

    rep_Assimilates = Record
        On          : Boolean;
        Period      : INTEGER; (* Number of days at end of season during which assimilates are stored in root system *)
        Stored      : ShortInt; (* Percentage of assimilates, transferred to root system at last day of season *)
        Mobilized   : ShortInt; (* Percentage of stored assimilates, transferred to above ground parts in next season *)
        end;

    SoilLayerIndividual = Record
        Description  : rep_string25;
        Thickness    : double;   (* meter *)
        SAT          : double;   (* Vol % at Saturation *)
        FC           : double;   (* Vol % at Field Capacity *)
        WP           : double;   (* Vol % at Wilting Point *)
        tau          : double;   (* drainage factor 0 ... 1 *)
        InfRate      : double;   (* Infiltration rate at saturation mm/day *)
        Penetrability : ShortInt; (* root zone expansion rate in percentage*)
        GravelMass    : ShortInt; (* mass percentage of gravel *)
        GravelVol      : double; (* volume percentage of gravel *)
        WaterContent : double;   (* mm *)
        // salinity parameters (cells)
        Macro        : ShortInt; (* Macropores : from Saturation to Macro [vol%] *)
        SaltMobility : rep_salt; (* Mobility of salt in the various salt cellS  *)
        SC           : ShortInt;  (* number of Saltcels between 0 and SC/(SC+2)*SAT vol% *)
        SCP1         : ShortInt;  (* SC + 1   (1 extra Saltcel between SC/(SC+2)*SAT vol% and SAT)
                                  THis last celL is twice as large as the other cels *)
        UL           : double;  (* Upper Limit of SC salt cells = SC/(SC+2) * (SAT/100) in m3/m3 *)
        Dx           : double; (* Size of SC salt cells [m3/m3] = UL/SC *)
        // capilary rise parameters
        SoilClass    : shortInt; // 1 = sandy, 2 = loamy, 3 = sandy clayey, 4 - silty clayey soils
        CRa, CRb     : double; (* coefficients for Capillary Rise *)
        END;

     rep_soil = Record
         REW            : ShortInt; (* Readily evaporable water mm *)
         NrSoilLayers   : ShortInt;
         CNvalue        : ShortInt;
         RootMax        : Single; // maximum rooting depth in soil profile for selected crop
         end;

    rep_SoilLayer = ARRAY[1..max_SoilLayers] of SoilLayerIndividual;
    
    rep_int_array = ARRAY[1..4] OF INTEGER;

    rep_modeCycle = (GDDays, CalendarDays);

    rep_planting = (Seed,Transplant,Regrowth);

    repCriterion = (CumulRain, RainPeriod, RainDecade, RainVsETo);
    repAirTCriterion = (TminPeriod,TmeanPeriod,GDDPeriod,CumulGDD);

    rep_Onset = Record
        GenerateOn : BOOLEAN;  // by rainfall or temperature criterion
        GenerateTempOn : BOOLEAN; // by temperature criterion
        Criterion : repCriterion;
        AirTCriterion : repAirTCriterion;
        StartSearchDayNr : LongInt; // daynumber
        StopSearchDayNr : LongInt; // daynumber
        LengthSearchPeriod : INTEGER; // days
        end;

     rep_EndSeason = Record
         ExtraYears : Integer; // to add to YearStartCropCycle
         GenerateTempOn : BOOLEAN; // by temperature criterion
         AirTCriterion : repAirTCriterion;
         StartSearchDayNr, StopSearchDayNr : integer; //daynumber
         LengthSearchPeriod : INTEGER; //days
         end;

    rep_datatype = (Daily,Decadely, Monthly);

    rep_clim = Record
         DataType    : rep_datatype;
         FromD,FromM,FromY : INTEGER; //D = day or decade, Y=1901 is not linked to specific year
         ToD,ToM,ToY : INTEGER;
         FromDayNr, ToDayNr : LongInt; //daynumber
         FromString, ToString : String;
         NrObs       : INTEGER; // number of observations
         end;

    rep_Content = Record  // total water (mm) or salt (Mg/ha) content
        BeginDay  : double; //at the beginning of the day
        EndDay    : double; //at the end of the day
        ErrorDay  : double; //error on WaterContent or SaltContent over the day
        END;

    rep_EffectStress = Record
         RedCGC          : ShortInt; (* Reduction of CGC (%) *)
         RedCCX          : ShortInt; (* Reduction of CCx (%) *)
         RedWP           : ShortInt; (* Reduction of WP (%) *)
         CDecline        : Double; (* Average decrease of CCx in mid season (%/day) *)
         RedKsSto        : ShortInt; (* Reduction of KsSto (%) *)
         end;

    rep_EffectiveRainMethod = (Full,USDA,Percentage);

    rep_EffectiveRain = Record    // for 10-day or monthly rainfall data
         Method : rep_EffectiveRainMethod;
         PercentEffRain : ShortInt; // IF Method = Percentage
         ShowersInDecade : ShortInt; // adjustment of surface run-off
         RootNrEvap : ShortInt; // Root for reduction in soil evaporation
         end;

    rep_Shapes = Record
         Stress          : ShortInt; (* Percentage soil fertility stress for calibration*)
         ShapeCGC        : Double; (* Shape factor for the response of Canopy Growth Coefficient to soil fertility stress *)
         ShapeCCX        : Double; (* Shape factor for the response of Maximum Canopy Cover to soil fertility stress *)
         ShapeWP         : Double; (* Shape factor for the response of Crop Water Producitity to soil fertility stress *)
         ShapeCDecline   : Double; (* Shape factor for the response of Decline of Canopy Cover to soil fertility stress *)
         Calibrated      : BOOLEAN;
         end;

     rep_RootZoneWC = Record
         Actual : double; // actual soil water content in rootzone [mm]
         FC     : double; //  soil water content [mm] in rootzone at FC
         WP     : double; // soil water content [mm] in rootzone at WP
         SAT    : double; // soil water content [mm] in rootzone at Sat
         Leaf   : double; // soil water content [mm] in rootzone at upper Threshold for leaf expansion
         Thresh : double; // soil water content [mm] in rootzone at Threshold for stomatal closure
         Sen    : double; // soil water content [mm] in rootzone at Threshold for canopy senescence
         ZtopAct : double;  // actual soil water content [mm] in top soil (= top compartment)
         ZtopFC  : double;  // soil water content [mm] at FC in top soil (= top compartment)
         ZtopWP  : double;  // soil water content [mm] at WP in top soil (= top compartment)
         ZtopThresh : double; // soil water content [mm] at Threshold for stomatal closure in top soil
         end;

     rep_IrriECw = Record
         PreSeason  : double;
         PostSeason : double;
         end;

     rep_CropFileSet = Record
         DaysFromSenescenceToEnd : integer;
         DaysToHarvest      : integer;  //given or calculated from GDD
         GDDaysFromSenescenceToEnd : integer;
         GDDaysToHarvest    : integer;  //given or calculated from Calendar Days
         end;

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

     rep_sum = RECORD
         Epot, Tpot, Rain, Irrigation, Infiltrated,
         Runoff, Drain, Eact, Tact, TrW, ECropCycle, CRwater  : double;  (* mm *)
         Biomass, YieldPart, BiomassPot, BiomassUnlim, BiomassTot : double;   (* ton/ha *)
         SaltIn, SaltOut, CRsalt : double; (* ton/ha *)
         End;

     rep_RootZoneSalt = Record
         ECe    : double;   // Electrical conductivity of the saturated soil-paste extract (dS/m)
         ECsw   : double;   // Electrical conductivity of the soil water (dS/m)
         ECswFC : double;   // Electrical conductivity of the soil water at Field Capacity(dS/m)
         KsSalt : double;   // stress coefficient for salinity
         end;

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
     
     rep_GenerateTimeMode = (FixInt,AllDepl,AllRAW,WaterBetweenBunds);
     rep_GenerateDepthMode = (ToFC,FixDepth);

     rep_Crop = Record
         subkind       : rep_subkind;
         ModeCycle     : rep_modeCycle;
         Planting      : rep_Planting; // 1 = sown, 0 = transplanted, -9 = regrowth
         pMethod       : rep_pMethod;
         pdef : double;  // soil water depletion fraction for no stomatal stress as defined (ETo = 5 mm/day)
         pActStom : double; // actual p for no stomatal stress for ETo of the day
         KsShapeFactorLeaf : Double;
         KsShapeFactorStomata : Double;
         KsShapeFactorSenescence : Double;
         pLeafDefUL,pLeafDefLL: double; //soil water depletion fraction for leaf expansion (ETo = 5 mm/day)
         pLeafAct    : double; //actual p for upper limit leaf expansion for ETo of the day
         pSenescence : double; //soil water depletion fraction for canopys senescence (ETo = 5 mm/day)
         pSenAct     : double; //actual p for canopy senescence for ETo of the day
         pPollination : double; //soil water depletion fraction for failure of pollination
         SumEToDelaySenescence : INTEGER;
         AnaeroPoint : INTEGER; (* (SAT - [vol%]) at which deficient aeration *)
         StressResponse : rep_Shapes; // is reponse to soil fertility stress
         ECemin : ShortInt;     // lower threshold for salinity stress (dS/m)
         ECemax : ShortInt;     // upper threshold for salinity stress (dS/m)
         CCsaltDistortion : ShortInt;  // distortion canopy cover for calibration for simulation of effect of salinity stress (%)
         ResponseECsw : INTEGER; // Response of Ks stomata to ECsw for calibration: From 0 (none) to +200 (very strong)
         SmaxTopQuarter : double; (* Smax Top 1/4 root zone HOOGLAND *)
         SmaxBotQuarter : double; (* Smax Bottom 1/4 root zone HOOGLAND *)
         SmaxTop : double;   (* Smax Top root zone HOOGLAND *)
         SmaxBot : double;   (* Smax Bottom root zone HOOGLAND *)
         KcTop : double;
         KcDecline : double; // Reduction Kc (%CCx/day) as result of ageing effects, nitrogen defficiency, etc.
         CCEffectEvapLate : INTEGER; (* % *)
         Day1 : LongInt;   (* Daynummer: first day of croping period starting from sowing/transplanting *)
         DayN : LongInt;   (* Daynummer: last day = harvest day*)
         Length : rep_int_array; (* 1 .. 4 :  = the four growth stages  *)
         RootMin, RootMax : double;   // rooting depth in meter
         RootShape : ShortInt;     // 10 times the root of the root function
         Tbase        : double;           //Base Temperature (degC)
         Tupper       : double;  //Upper temperature threshold (degC)
         Tcold        : ShortInt;            // Minimum air temperature below which pollination starts to fail (cold stress) (degC)
         Theat        : ShortInt; // Maximum air temperature above which pollination starts to fail (heat stress) (degC)
         GDtranspLow  : double; // Minimum growing degrees required for full crop transpiration (degC - day)
         SizeSeedling : double;  //Canopy cover per seedling (cm2)
         SizePlant    : double;  //Canopy cover of plant on 1st day (cm2) when regrowth
         PlantingDens : LongInt; //number of plants per hectare
         CCo          : double;           //starting canopy size  (fraction canopy cover)
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

     rep_IrriMode = (NoIrri,Manual,Generate,Inet);
     rep_IrriMethod = (MBasin,MBorder,MDrip,MFurrow,MSprinkler);

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




     rep_MonthInteger = ARRAY[1..12] OF INTEGER;

     rep_param = RECORD  // DEFAULT.PAR
         // crop parameters IN CROP.PAR - with Reset option
         EvapDeclineFactor : ShortInt;  // exponential decline with relative soil water [1 = small ... 8 = sharp]
         KcWetBare       : double; //Soil evaporation coefficients from wet bare soil
         PercCCxHIfinal    : ShortInt; // CC threshold below which HI no longer increase (% of 100)
         RootPercentZmin : integer; //starting depth of root sine function in % of Zmin (sowing depth)
         MaxRootZoneExpansion : double; // maximum root zone expansion in cm/day - fixed at 5 cm/day
         KsShapeFactorRoot : Shortint; //shape factro for the effect of water stress on root zone expansion
         TAWGermination    : ShortInt; // Soil water content (% TAW) required at sowing depth for germination
         pAdjFAO         : double; //Adjustment factor for FAO-adjustment of soil water depletion (p) for various ET
         DelayLowOxygen  : integer; //delay [days] for full effect of anaeroby
         ExpFsen           : double; // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
         Beta              : ShortInt; // Percentage decrease of p(senescence) once early canopy senescence is triggered
         ThicknessTopSWC   : ShortInt; // Thickness of top soil for determination of its Soil Water Content (cm)
         // Field parameter IN FIELD.PAR  - with Reset option
         EvapZmax : ShortInt; // cm  maximum soil depth for water extraction by evaporation
         // Runoff parameters IN RUNOFF.PAR  - with Reset option
         RunoffDepth : double; //considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment
         CNcorrection : Boolean; //correction Antecedent Moisture Class (On/Off)
         // Temperature parameters IN TEMPERATURE.PAR  - with Reset option
         Tmin,Tmax   : double; // Default Minimum and maximum air temperature (degC) if no temperature file
         GDDMethod   : ShortInt; // 1 for Method 1, 2 for Method 2, 3 for Method 3
         // General parameters IN GENERAL.PAR
         PercRAW     : integer; //allowable percent RAW depletion for determination Inet
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
         ConstGwt : Boolean; // groundwater table is constant (or absent) during the simulation period
         // Capillary rise
         RootNrDF : shortint;
         // Initial abstraction for surface runoff
         IniAbstract : shortint;
         END;

     rep_PerennialPeriod = Record
         GenerateOnset  : boolean;  // onset is generated by air temperature criterion
         OnsetCriterion : repAirTCriterion;  (* another doscstring *)
         OnsetFirstDay,OnsetFirstMonth : integer;
	     OnsetStartSearchDayNr, OnsetStopSearchDayNr : integer; //daynumber
	     OnsetLengthSearchPeriod : integer; //days
	     OnsetThresholdValue : double; // degC or degree-days
	     OnsetPeriodValue : integer; // number of successive days
	     OnsetOccurrence : ShortInt; // number of occurrences (1,2 or 3)
         GenerateEnd : boolean; // end is generate by air temperature criterion
         EndCriterion : repAirTCriterion;
	     EndLastDay,EndLastMonth : integer;
         ExtraYears : integer; // number of years to add to the onset year
         EndStartSearchDayNr, EndStopSearchDayNr : integer; //daynumber
	     EndLengthSearchPeriod : integer; //days
	     EndThresholdValue : double; // degC or degree-days
	     EndPeriodValue : integer; // number of successive days
	     EndOccurrence : ShortInt; // number of occurrences (1,2 or 3)
         GeneratedDayNrOnset,GeneratedDayNrEnd : integer;
         end;



function AquaCropVersion(FullNameXXFile : string) : double;
         external 'aquacrop' name '__ac_global_MOD_aquacropversion';
         

function RootMaxInSoilProfile(
            constref ZmaxCrop : double;
            constref TheNrSoilLayers : shortint;
            constref TheSoilLayer : rep_SoilLayer) : single;
         external 'aquacrop' name '__ac_global_MOD_rootmaxinsoilprofile';


procedure ZrAdjustedToRestrictiveLayers(ZrIN : double;
                                        TheNrSoilLayers : ShortInt;
                                        TheLayer : rep_SoilLayer;
                                        var ZrOUT : double);
         external 'aquacrop' name '__ac_global_MOD_zradjustedtorestrictivelayers';

function TimeRootFunction(
            constref t : double;
            constref ShapeFactor : shortint;
            constref tmax, t0 : double) : double;
         external 'aquacrop' name '__ac_global_MOD_timerootfunction';

procedure DeclareInitialCondAtFCandNoSalt();
         external 'aquacrop' name '__ac_global_MOD_declareinitialcondatfcandnosalt';

procedure set_layer_undef(
            var LayerData : SoilLayerIndividual);
         external 'aquacrop' name '__ac_global_MOD_set_layer_undef';

function GetCrop() : rep_Crop;

function GetCrop_StressResponse() : rep_Shapes;

function GetCrop_StressResponse_Stress() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_stressresponse_stress';

function GetCrop_StressResponse_ShapeCGC() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_stressresponse_shapecgc';

function GetCrop_StressResponse_ShapeCCX() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_stressresponse_shapeccx';

function GetCrop_StressResponse_ShapeWP() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_stressresponse_shapewp';

function GetCrop_StressResponse_ShapeCDecline() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_stressresponse_shapecdecline';

function GetCrop_StressResponse_Calibrated() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getcrop_stressresponse_calibrated_wrap';

procedure SetCrop_StressResponse(constref Crop_StressResponse : rep_Shapes);

procedure SetCrop_StressResponse_Stress(constref Stress : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_stressresponse_stress';

procedure SetCrop_StressResponse_ShapeCGC(constref ShapeCGC : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_stressresponse_shapecgc';

procedure SetCrop_StressResponse_ShapeCCX(constref ShapeCCX : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_stressresponse_shapeccx';

procedure SetCrop_StressResponse_ShapeWP(constref ShapeWP : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_stressresponse_shapewp';

procedure SetCrop_StressResponse_ShapeCDecline(constref ShapeCDecline : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_stressresponse_shapecdecline';

procedure SetCrop_StressResponse_Calibrated(constref Calibrated : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setcrop_stressresponse_calibrated_wrap';

function GetCrop_Length() : rep_int_array;

function GetCrop_Length_i(constref i : integer) : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_length_i';

function GetCrop_subkind() : rep_subkind;

function __GetCrop_subkind() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getcrop_subkind';

function GetCrop_ModeCycle() : rep_modeCycle;

function __GetCrop_ModeCycle() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getcrop_modecycle';

function GetCrop_Planting() : rep_Planting;

function __GetCrop_Planting() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getcrop_planting';

function GetCrop_pMethod() : rep_pMethod;

function __GetCrop_pMethod() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pmethod';

function GetCrop_pdef() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pdef';

function GetCrop_pActStom() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pactstom';

function GetCrop_KsShapeFactorLeaf() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ksshapefactorleaf';

function GetCrop_KsShapeFactorStomata() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ksshapefactorstomata';

function GetCrop_KsShapeFactorSenescence() : Double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ksshapefactorsenescence';

function GetCrop_pLeafDefUL() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pleafdeful';

function GetCrop_pLeafDefLL() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pleafdefll';

function GetCrop_pLeafAct() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_pleafact';

function GetCrop_pSenescence() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_psenescence';

function GetCrop_pSenAct() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_psenact';

function GetCrop_pPollination() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ppollination';

function GetCrop_SumEToDelaySenescence() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_sumetodelaysenescence';

function GetCrop_AnaeroPoint() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_anaeropoint';

function GetCrop_ECemin() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ecemin';

function GetCrop_ECemax() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ecemax';

function GetCrop_CCsaltDistortion() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccsaltdistortion';

function GetCrop_ResponseECsw() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_responseecsw';

function GetCrop_SmaxTopQuarter() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_smaxtopquarter';

function GetCrop_SmaxBotQuarter() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_smaxbotquarter';

function GetCrop_SmaxTop() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_smaxtop';

function GetCrop_SmaxBot() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_smaxbot';

function GetCrop_KcTop() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_kctop';

function GetCrop_KcDecline() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_kcdecline';

function GetCrop_CCEffectEvapLate() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_cceffectevaplate';

function GetCrop_Day1() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_day1';

function GetCrop_DayN() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_dayn';

function GetCrop_RootMin() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_rootmin';

function GetCrop_RootMax() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_rootmax';

function GetCrop_RootShape() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_rootshape';

function GetCrop_Tbase() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_tbase';

function GetCrop_Tupper() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_tupper';

function GetCrop_Tcold() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_tcold';

function GetCrop_Theat() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_theat';

function GetCrop_GDtranspLow() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gdtransplow';

function GetCrop_SizeSeedling() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_sizeseedling';

function GetCrop_SizePlant() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_sizeplant';

function GetCrop_PlantingDens() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_plantingdens';

function GetCrop_CCo() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_cco';

function GetCrop_CCini() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccini';

function GetCrop_CGC() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_cgc';

function GetCrop_GDDCGC() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddcgc';

function GetCrop_CCx() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccx';

function GetCrop_CDC() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_cdc';

function GetCrop_GDDCDC() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddcdc';

function GetCrop_CCxAdjusted() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccxadjusted';

function GetCrop_CCxWithered() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccxwithered';

function GetCrop_CCoAdjusted() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccoadjusted';

function GetCrop_DaysToCCini() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystoccini';

function GetCrop_DaysToGermination() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystogermination';

function GetCrop_DaysToFullCanopy() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystofullcanopy';

function GetCrop_DaysToFullCanopySF() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystofullcanopysf';

function GetCrop_DaysToFlowering() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystoflowering';

function GetCrop_LengthFlowering() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_lengthflowering';

function GetCrop_DaysToSenescence() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystosenescence';

function GetCrop_DaysToHarvest() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystoharvest';

function GetCrop_DaysToMaxRooting() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystomaxrooting';

function GetCrop_DaysToHIo() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_daystohio';

function GetCrop_GDDaysToCCini() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystoccini';

function GetCrop_GDDaysToGermination() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystogermination';

function GetCrop_GDDaysToFullCanopy() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystofullcanopy';

function GetCrop_GDDaysToFullCanopySF() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystofullcanopysf';

function GetCrop_GDDaysToFlowering() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystoflowering';

function GetCrop_GDDLengthFlowering() : Integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddlengthflowering';

function GetCrop_GDDaysToSenescence() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystosenescence';

function GetCrop_GDDaysToHarvest() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystoharvest';

function GetCrop_GDDaysToMaxRooting() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystomaxrooting';

function GetCrop_GDDaysToHIo() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_gddaystohio';

function GetCrop_WP() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_wp';

function GetCrop_WPy() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_wpy';

function GetCrop_AdaptedToCO2() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_adaptedtoco2';

function GetCrop_HI() : integer;
    external 'aquacrop' name '__ac_global_MOD_getcrop_hi';

function GetCrop_dHIdt() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_dhidt';

function GetCrop_HIincrease() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_hiincrease';

function GetCrop_aCoeff() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_acoeff';

function GetCrop_bCoeff() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_bcoeff';

function GetCrop_DHImax() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_dhimax';

function GetCrop_DeterminancyLinked() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getcrop_determinancylinked_wrap';

function GetCrop_fExcess() : SmallInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_fexcess';

function GetCrop_DryMatter() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_drymatter';

function GetCrop_RootMinYear1() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_rootminyear1';

function GetCrop_SownYear1() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getcrop_sownyear1_wrap';

function GetCrop_YearCCx() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_yearccx';

function GetCrop_CCxRoot() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrop_ccxroot';

procedure SetCrop(constref Crop : rep_Crop);

procedure SetCrop_Length(constref Length : rep_int_array);

procedure SetCrop_Length_i(constref i : integer; 
                          constref Length_i : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_length_i';

procedure SetCrop_subkind(constref subkind : rep_subkind);

procedure __SetCrop_subkind(constref subkind : shortint);
    external 'aquacrop' name '__ac_global_MOD_setcrop_subkind';

procedure SetCrop_ModeCycle(constref ModeCycle : rep_modeCycle);

procedure __SetCrop_ModeCycle(constref ModeCycle : shortint);
    external 'aquacrop' name '__ac_global_MOD_setcrop_modecycle';

procedure SetCrop_Planting(constref Planting : rep_Planting);

procedure __SetCrop_Planting(constref Planting : shortint);
    external 'aquacrop' name '__ac_global_MOD_setcrop_planting';

procedure SetCrop_pMethod(constref pMethod : rep_pMethod);

procedure __SetCrop_pMethod(constref pMethod : shortint);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pmethod';

procedure SetCrop_pdef(constref pdef : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pdef';

procedure SetCrop_pActStom(constref pActStom : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pactstom';

procedure SetCrop_KsShapeFactorLeaf(constref KsShapeFactorLeaf : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ksshapefactorleaf';

procedure SetCrop_KsShapeFactorStomata(constref KsShapeFactorStomata : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ksshapefactorstomata';

procedure SetCrop_KsShapeFactorSenescence(constref KsShapeFactorSenescence : Double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ksshapefactorsenescence';

procedure SetCrop_pLeafDefUL(constref pLeafDefUL : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pleafdeful';

procedure SetCrop_pLeafDefLL(constref pLeafDefLL : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pleafdefll';

procedure SetCrop_pLeafAct(constref pLeafAct : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_pleafact';

procedure SetCrop_pSenescence(constref pSenescence : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_psenescence';

procedure SetCrop_pSenAct(constref pSenAct : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_psenact';

procedure SetCrop_pPollination(constref pPollination : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ppollination';

procedure SetCrop_SumEToDelaySenescence(constref SumEToDelaySenescence : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_sumetodelaysenescence';

procedure SetCrop_AnaeroPoint(constref AnaeroPoint : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_anaeropoint';

procedure SetCrop_ECemin(constref ECemin : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ecemin';

procedure SetCrop_ECemax(constref ECemax : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ecemax';

procedure SetCrop_CCsaltDistortion(constref CCsaltDistortion : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccsaltdistortion';

procedure SetCrop_ResponseECsw(constref ResponseECsw : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_responseecsw';

procedure SetCrop_SmaxTopQuarter(constref SmaxTopQuarter : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_smaxtopquarter';

procedure SetCrop_SmaxBotQuarter(constref SmaxBotQuarter : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_smaxbotquarter';

procedure SetCrop_SmaxTop(constref SmaxTop : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_smaxtop';

procedure SetCrop_SmaxBot(constref SmaxBot : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_smaxbot';

procedure SetCrop_KcTop(constref KcTop : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_kctop';

procedure SetCrop_KcDecline(constref KcDecline : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_kcdecline';

procedure SetCrop_CCEffectEvapLate(constref CCEffectEvapLate : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_cceffectevaplate';

procedure SetCrop_Day1(constref Day1 : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_day1';

procedure SetCrop_DayN(constref DayN : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_dayn';

procedure SetCrop_RootMin(constref RootMin : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_rootmin';

procedure SetCrop_RootMax(constref RootMax : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_rootmax';

procedure SetCrop_RootShape(constref RootShape : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_rootshape';

procedure SetCrop_Tbase(constref Tbase : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_tbase';

procedure SetCrop_Tupper(constref Tupper : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_tupper';

procedure SetCrop_Tcold(constref Tcold : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_tcold';

procedure SetCrop_Theat(constref Theat : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_theat';

procedure SetCrop_GDtranspLow(constref GDtranspLow : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gdtransplow';

procedure SetCrop_SizeSeedling(constref SizeSeedling : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_sizeseedling';

procedure SetCrop_SizePlant(constref SizePlant : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_sizeplant';

procedure SetCrop_PlantingDens(constref PlantingDens : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_plantingdens';

procedure SetCrop_CCo(constref CCo : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_cco';

procedure SetCrop_CCini(constref CCini : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccini';

procedure SetCrop_CGC(constref CGC : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_cgc';

procedure SetCrop_GDDCGC(constref GDDCGC : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddcgc';

procedure SetCrop_CCx(constref CCx : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccx';

procedure SetCrop_CDC(constref CDC : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_cdc';

procedure SetCrop_GDDCDC(constref GDDCDC : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddcdc';

procedure SetCrop_CCxAdjusted(constref CCxAdjusted : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccxadjusted';

procedure SetCrop_CCxWithered(constref CCxWithered : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccxwithered';

procedure SetCrop_CCoAdjusted(constref CCoAdjusted : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccoadjusted';

procedure SetCrop_DaysToCCini(constref DaysToCCini : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystoccini';

procedure SetCrop_DaysToGermination(constref DaysToGermination : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystogermination';

procedure SetCrop_DaysToFullCanopy(constref DaysToFullCanopy : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystofullcanopy';

procedure SetCrop_DaysToFullCanopySF(constref DaysToFullCanopySF : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystofullcanopysf';

procedure SetCrop_DaysToFlowering(constref DaysToFlowering : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystoflowering';

procedure SetCrop_LengthFlowering(constref LengthFlowering : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_lengthflowering';

procedure SetCrop_DaysToSenescence(constref DaysToSenescence : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystosenescence';

procedure SetCrop_DaysToHarvest(constref DaysToHarvest : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystoharvest';

procedure SetCrop_DaysToMaxRooting(constref DaysToMaxRooting : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystomaxrooting';

procedure SetCrop_DaysToHIo(constref DaysToHIo : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_daystohio';

procedure SetCrop_GDDaysToCCini(constref GDDaysToCCini : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystoccini';

procedure SetCrop_GDDaysToGermination(constref GDDaysToGermination : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystogermination';

procedure SetCrop_GDDaysToFullCanopy(constref GDDaysToFullCanopy : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystofullcanopy';

procedure SetCrop_GDDaysToFullCanopySF(constref GDDaysToFullCanopySF : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystofullcanopysf';

procedure SetCrop_GDDaysToFlowering(constref GDDaysToFlowering : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystoflowering';

procedure SetCrop_GDDLengthFlowering(constref GDDLengthFlowering : Integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddlengthflowering';

procedure SetCrop_GDDaysToSenescence(constref GDDaysToSenescence : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystosenescence';

procedure SetCrop_GDDaysToHarvest(constref GDDaysToHarvest : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystoharvest';

procedure SetCrop_GDDaysToMaxRooting(constref GDDaysToMaxRooting : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystomaxrooting';

procedure SetCrop_GDDaysToHIo(constref GDDaysToHIo : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_gddaystohio';

procedure SetCrop_WP(constref WP : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_wp';

procedure SetCrop_WPy(constref WPy : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_wpy';

procedure SetCrop_AdaptedToCO2(constref AdaptedToCO2 : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_adaptedtoco2';

procedure SetCrop_HI(constref HI : integer);
    external 'aquacrop' name '__ac_global_MOD_setcrop_hi';

procedure SetCrop_dHIdt(constref dHIdt : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_dhidt';

procedure SetCrop_HIincrease(constref HIincrease : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_hiincrease';

procedure SetCrop_aCoeff(constref aCoeff : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_acoeff';

procedure SetCrop_bCoeff(constref bCoeff : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_bcoeff';

procedure SetCrop_DHImax(constref DHImax : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_dhimax';

procedure SetCrop_DeterminancyLinked(constref DeterminancyLinked : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setcrop_determinancylinked_wrap';

procedure SetCrop_fExcess(constref fExcess : SmallInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_fexcess';

procedure SetCrop_DryMatter(constref DryMatter : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_drymatter';

procedure SetCrop_RootMinYear1(constref RootMinYear1 : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_rootminyear1';

procedure SetCrop_SownYear1(constref SownYear1 : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setcrop_sownyear1_wrap';

procedure SetCrop_YearCCx(constref YearCCx : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_yearccx';

procedure SetCrop_CCxRoot(constref CCxRoot : double);
    external 'aquacrop' name '__ac_global_MOD_setcrop_ccxroot';

function GetCrop_Assimilates() : rep_Assimilates;

function GetCrop_Assimilates_On() : Boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getcrop_assimilates_on_wrap';

function GetCrop_Assimilates_Period() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcrop_assimilates_period';

function GetCrop_Assimilates_Stored() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_assimilates_stored';

function GetCrop_Assimilates_Mobilized() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getcrop_assimilates_mobilized';

procedure SetCrop_Assimilates(constref Crop_Assimilates : rep_Assimilates);

procedure SetCrop_Assimilates_On(constref On : Boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setcrop_assimilates_on_wrap';

procedure SetCrop_Assimilates_Period(constref Period : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcrop_assimilates_period';

procedure SetCrop_Assimilates_Stored(constref Stored : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_assimilates_stored';

procedure SetCrop_Assimilates_Mobilized(constref Mobilized : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setcrop_assimilates_mobilized';

procedure DetermineDayNr(
            constref Dayi,Monthi,Yeari : integer;
            var DayNr : longint);
         external 'aquacrop' name '__ac_global_MOD_determinedaynr';

procedure DetermineDate(
            constref DayNr : longint;
            var Dayi,Monthi,Yeari : integer);
         external 'aquacrop' name '__ac_global_MOD_determinedate';

function TimeToReachZroot(
            constref Zi, Zo, Zx : double;
            constref ShapeRootDeepening : shortint;
            constref Lo, LZxAdj : integer) : double;
         external 'aquacrop' name '__ac_global_MOD_timetoreachzroot';

function MaxCRatDepth(
            constref ParamCRa, ParamCRb, Ksat : double;
            constref Zi, DepthGWT : double) : double;
         external 'aquacrop' name '__ac_global_MOD_maxcratdepth';

function FromGravelMassToGravelVolume(
	    constref PorosityPercent : double;
            constref GravelMassPercent : shortint) : double;
         external 'aquacrop' name '__ac_global_MOD_fromgravelmasstogravelvolume';

function __GetWeedRC(
            constref TheDay : integer;
            constref GDDayi : double;
            constref fCCx : double;
            constref TempWeedRCinput : shortint;
            constref TempWeedAdj : shortint;
            var TempWeedDeltaRC : integer;
            constref L12SF : integer;
            constref TempL123 : integer;
            constref GDDL12SF : integer;
            constref TempGDDL123 : integer;
            constref TheModeCycle : integer) : double;
         external 'aquacrop' name '__ac_global_MOD_getweedrc';
		
function GetWeedRC(
            constref TheDay : integer;
            constref GDDayi : double;
            constref fCCx : double;
            constref TempWeedRCinput : shortint;
            constref TempWeedAdj : shortint;
            var TempWeedDeltaRC : integer;
            constref L12SF : integer;
            constref TempL123 : integer;
            constref GDDL12SF : integer;
            constref TempGDDL123 : integer;
            constref TheModeCycle : rep_modeCycle) : double;

procedure DetermineLengthGrowthStages_wrap(
            constref CCoVal : double;
            constref CCxVal : double;
            constref CDCVal : double;
            constref L0 : integer;
            constref TotalLength : integer;
            constref CGCgiven : boolean;
            constref TheDaysToCCini : integer;
            constref ThePlanting : integer;
            VAR Length123 : integer;
            VAR StLength : rep_int_array;
            VAR Length12 : integer;
            VAR CGCVal : double);
        external 'aquacrop' name '__ac_interface_global_MOD_determinelengthgrowthstages_wrap';

procedure TimeToMaxCanopySF(
            constref CCo,CGC,CCx : double;
            constref L0,L12,L123,LToFlor,LFlor : integer;
            constref DeterminantCrop : boolean;
            VAR L12SF : integer;
            VAR RedCGC,RedCCx : ShortInt;
            VAR ClassSF : ShortInt);
        external 'aquacrop' name '__ac_interface_global_MOD_timetomaxcanopysf_wrap';

procedure CheckForWaterTableInProfile(
            constref DepthGWTmeter : double;
            constref ProfileComp : rep_comp;
            var WaterTableInProfile : boolean);
        external 'aquacrop' name '__ac_global_MOD_checkforwatertableinprofile';
                                                
procedure DetermineLengthGrowthStages(
            constref CCoVal : double;
            constref CCxVal : double;
            constref CDCVal : double;
            constref L0 : integer;
            constref TotalLength : INTEGER;
            constref CGCgiven : BOOLEAN;
            constref TheDaysToCCini : INTEGER;
            constref ThePlanting : rep_planting;
            VAR Length123 : INTEGER;
            VAR StLength : rep_int_array;
            VAR Length12 : integer;
            VAR CGCVal : double);

function __TimeToCCini(
            constref ThePlantingType : integer;
            constref TheCropPlantingDens : integer;
            constref TheSizeSeedling : double;
            constref TheSizePlant : double;
            constref TheCropCCx : double;
            constref TheCropCGC : double) : Integer;
        external 'aquacrop' name '__ac_global_MOD_timetoccini';

function TimeToCCini(
            constref ThePlantingType : rep_planting;
            constref TheCropPlantingDens : integer;
            constref TheSizeSeedling : double;
            constref TheSizePlant : double;
            constref TheCropCCx : double;
            constref TheCropCGC : double) : Integer;

procedure DetermineRootZoneSaltContent(
            constref RootingDepth : double;
            var ZrECe : double;
            var ZrECsw : double;
            var ZrECswFC: double;
            var ZrKsSalt : double);
        external 'aquacrop' name '__ac_global_MOD_determinerootzonesaltcontent';

function MultiplierCCxSelfThinning(
            constref Yeari : integer;
            constref Yearx : integer;
            constref ShapeFactor : double) : double;
         external 'aquacrop' name '__ac_global_MOD_multiplierccxselfthinning';

function DaysToReachCCwithGivenCGC(
           constref CCToReach : double;
           constref CCoVal : double;
           constref CCxVal : double;
           constref CGCVal : double;
           constref L0 : integer) : integer;
        external 'aquacrop' name '__ac_global_MOD_daystoreachccwithgivencgc';

function LengthCanopyDecline(
           constref CCx : double;
           constref CDC : double) : integer;
        external 'aquacrop' name '__ac_global_MOD_lengthcanopydecline';

function CCmultiplierWeed(
            constref ProcentWeedCover : shortint;
            constref CCxCrop : double;
            constref FshapeWeed : double) : double;
         external 'aquacrop' name '__ac_global_MOD_ccmultiplierweed';

function CCmultiplierWeedAdjusted(
            constref ProcentWeedCover: shortint;
            constref CCxCrop : double;
            constref FshapeWeed : double;
            constref fCCx : double;
            constref Yeari : shortint;
            constref MWeedAdj : shortint;
            constref RCadj : shortint) : shortint;
         external 'aquacrop' name '__ac_global_MOD_ccmultiplierweedadjusted';

function HarvestIndexGrowthCoefficient(
        constref HImax,dHIdt : double) : double;
        external 'aquacrop' name '__ac_global_MOD_harvestindexgrowthcoefficient';

function TauFromKsat(constref Ksat : double) : double;
         external 'aquacrop' name '__ac_global_MOD_taufromksat';

function BMRange(constref HIadj : integer) : double;
         external 'aquacrop' name '__ac_global_MOD_bmrange';

function HImultiplier(
            constref RatioBM : double;
            constref RangeBM : double;
            constref HIadj : ShortInt) : double;
         external 'aquacrop' name '__ac_global_MOD_himultiplier';

function AdjustedKsStoToECsw(
            constref ECeMin : ShortInt;
            constref ECeMax : ShortInt;
            constref ResponseECsw : integer;
            constref ECei : double;
            constref ECswi : double;
            constref ECswFCi : double;
            constref Wrel : double;
            constref Coeffb0Salt : double;
            constref Coeffb1Salt : double;
            constref Coeffb2Salt : double;
            constref KsStoIN : double) : double;
         external 'aquacrop' name '__ac_global_MOD_adjustedksstotoecsw';

procedure ComposeOutputFileName(
            constref TheProjectFileName : string);

procedure ComposeOutputFileName_wrap(
            constref TheProjectFileName : string;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_composeoutputfilename_wrap';

procedure GetFileForProgramParameters(
            constref TheFullFileNameProgram : string;
            var FullFileNameProgramParameters: string);

procedure GetFileForProgramParameters_wrap(
            constref TheFullFileNameProgram : PChar;
            constref strlen1 : integer;
            var FullFileNameProgramParameters: PChar;
            constref strlen2 : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_getfileforprogramparameters_wrap';
        
function NumberSoilClass (
            constref SatvolPro : double;
            constref FCvolPro : double;
            constref PWPvolPro : double;
            constref Ksatmm : double) : shortint;
         external 'aquacrop' name '__ac_global_MOD_numbersoilclass';

procedure DeriveSmaxTopBottom(
            constref SxTopQ : double;
            constref SxBotQ : double;
            var SxTop : double;
            var SxBot : double);
         external 'aquacrop' name '__ac_global_MOD_derivesmaxtopbottom';

procedure CropStressParametersSoilFertility(
            constref CropSResp : rep_Shapes;
            constref StressLevel : ShortInt;
            var StressOUT : rep_EffectStress);
         external 'aquacrop' name '__ac_global_MOD_cropstressparameterssoilfertility';

function SoilEvaporationReductionCoefficient(
            constref Wrel : double;
            constref EDecline : double) : double;
         external 'aquacrop' name '__ac_global_MOD_soilevaporationreductioncoefficient';

function KsTemperature(
            constref T0,T1,Tin : double) : double;
         external 'aquacrop' name '__ac_global_MOD_kstemperature';

function KsSalinity(
            constref SalinityResponsConsidered : boolean;
            constref ECeN,ECeX : ShortInt;
            constref ECeVAR,KsShapeSalinity : double) : double;
         external 'aquacrop' name '__ac_global_MOD_kssalinity';

function MultiplierCCoSelfThinning(
            constref Yeari,Yearx : integer;
            constref ShapeFactor : double) : double;
         external 'aquacrop' name '__ac_global_MOD_multiplierccoselfthinning';

function KsAny(
            constref Wrel,pULActual,pLLActual,ShapeFactor : double) : double;
         external 'aquacrop' name '__ac_global_MOD_ksany';

function CCatTime(
            constref Dayi : integer;
            constref CCoIN, CGCIN, CCxIN : double)  : double;
         external 'aquacrop' name '__ac_global_MOD_ccattime';

function DegreesDay(
            constref Tbase,Tupper,TDayMin,TDayMax : double;
            constref GDDSelectedMethod : ShortInt) : double;
         external 'aquacrop' name '__ac_global_MOD_degreesday';

procedure DetermineCNIandIII(
            constref CN2 : ShortInt;
            var CN1,CN3 : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_determinecniandiii';

procedure DetermineCN_default(
            constref Infiltr : double;
            var CN2 : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_determineCN_default';

function CCatGDD(
            constref GDDi, CCoIN, GDDCGCIN, CCxIN : double)  : double;
         external 'aquacrop' name '__ac_global_MOD_ccatgdd';

function CanopyCoverNoStressGDDaysSF(
            constref GDDL0,GDDL123,GDDLMaturity : integer;
            constref SumGDD,CCo,CCx,GDDCGC,GDDCDC : double;
            constref SFRedCGC,SFRedCCx : shortint) : double;
         external 'aquacrop' name '__ac_global_MOD_canopycovernostressgddayssf';

procedure SaltSolutionDeposit(
            constref mm : double; (* mm = l/m2 *)
            VAR SaltSolution,SaltDeposit : double); (* g/m2 *)
         external 'aquacrop' name '__ac_global_MOD_saltsolutiondeposit';

procedure ReadRainfallSettings();
        external 'aquacrop' name '__ac_global_MOD_readrainfallsettings';

procedure ReadSoilSettings();
        external 'aquacrop' name '__ac_global_MOD_readsoilsettings';

function fAdjustedForCO2 (
            constref CO2i, WPi : double;
            constref PercentA : ShortInt) : double;
        external 'aquacrop' name '__ac_global_MOD_fadjustedforco2';

function FullUndefinedRecord(
            constref FromY,FromD,FromM,ToD,ToM : integer) : boolean;
        external 'aquacrop' name '__ac_global_MOD_fullundefinedrecord';

procedure GenerateCO2Description(
            constref CO2FileFull: string;
            var CO2Description : string);

procedure GenerateCO2Description_wrap(
            constref CO2FileFull : PChar;
            constref strlen1 : integer;
            var CO2Description : PChar;
            constref strlen2 : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_generateco2description_wrap';

procedure GetIrriDescription(
            constref IrriFileFull : string;
            var IrriDescription : string);

procedure GetIrriDescription_wrap(
            constref IrriFileFull : PChar;
            constref strlen1 : integer;
            var IrriDescription : PChar;
            constref strlen2 : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_getirridescription_wrap';

procedure GetDaySwitchToLinear(
               constref HImax : integer;
               constref dHIdt,HIGC : double;
               var tSwitch : INTEGER;
               var HIGClinear : double);
        external 'aquacrop' name '__ac_global_MOD_getdayswitchtolinear';

procedure GetNumberSimulationRuns(
            constref TempFileNameFull : string;
            var NrRuns : integer);

procedure GetNumberSimulationRuns_wrap(
            constref TempFileNameFull : PChar;
            constref strlen : integer;
            var NrRuns : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_getnumbersimulationruns_wrap';

function GetCO2File(): string;

function GetCO2File_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getco2file_wrap';

procedure SetCO2File(constref str : string);

procedure SetCO2File_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setco2file_wrap';

function GetCO2FileFull(): string;

function GetCO2FileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getco2filefull_wrap';

procedure SetCO2FileFull(constref str : string);

procedure SetCO2FileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setco2filefull_wrap';

function GetCO2Description(): string;

function GetCO2Description_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getco2description_wrap';

procedure SetCO2Description(constref str : string);

procedure SetCO2Description_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setco2description_wrap';

function GetEToFile(): string;

function GetEToFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getetofile_wrap';

procedure SetEToFile(constref str : string);

procedure SetEToFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setetofile_wrap';

function GetEToFileFull(): string;

function GetEToFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getetofilefull_wrap';

procedure SetEToFileFull(constref str : string);

procedure SetEToFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setetofilefull_wrap';

function GetEToDescription(): string;

function GetEToDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getetodescription_wrap';

procedure SetEToDescription(constref str : string);

procedure SetEToDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setetodescription_wrap';

function GetRainFile(): string;

function GetRainFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getrainfile_wrap';

procedure SetRainFile(constref str : string);

procedure SetRainFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setrainfile_wrap';

function GetRainFileFull(): string;

function GetRainFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getrainfilefull_wrap';

procedure SetRainFileFull(constref str : string);

procedure SetRainFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setrainfilefull_wrap';

function GetRainDescription(): string;

function GetRainDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getraindescription_wrap';

procedure SetRainDescription(constref str : string);

procedure SetRainDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setraindescription_wrap';

function GetIrriFile(): string;

function GetIrriFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getirrifile_wrap';

procedure SetIrriFile(constref str : string);

procedure SetIrriFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setirrifile_wrap';

function GetIrriFileFull(): string;

function GetIrriFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getirrifilefull_wrap';

procedure SetIrriFileFull(constref str : string);

procedure SetIrriFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setirrifilefull_wrap';

function GetClimateFile(): string;

function GetClimateFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getclimatefile_wrap';

procedure SetClimateFile(constref str : string);

procedure SetClimateFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setclimatefile_wrap';

function GetClimateFileFull(): string;

function GetClimateFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getclimatefilefull_wrap';

procedure SetClimateFileFull(constref str : string);

procedure SetClimateFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setclimatefilefull_wrap';

procedure SetIrriDescription(constref str : string);

procedure SetIrriDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setirridescription_wrap';

function GetClimateDescription(): string;

function GetClimateDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getclimatedescription_wrap';

procedure SetClimateDescription(constref str : string);

procedure SetClimateDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setclimatedescription_wrap';

function GetClimFile(): string;

function GetClimFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getclimfile_wrap';

procedure SetClimFile(constref str : string);

procedure SetClimFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setclimfile_wrap';
           
function GetSWCiniFile(): string;

function GetSWCiniFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getswcinifile_wrap';

procedure SetSWCiniFile(constref str : string);

procedure SetSWCiniFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setswcinifile_wrap';


function GetSWCiniFileFull(): string;

function GetSWCiniFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getswcinifilefull_wrap';

procedure SetSWCiniFileFull(constref str : string);

procedure SetSWCiniFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setswcinifilefull_wrap';

function GetSWCiniDescription(): string;

function GetSWCiniDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getswcinidescription_wrap';

procedure SetSWCiniDescription(constref str : string);

procedure SetSWCiniDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setswcinidescription_wrap';

function Geteffectiverain() : rep_EffectiveRain;

function Geteffectiverain_Method() : rep_EffectiveRainMethod;

function __Geteffectiverain_Method() : shortint;
    external 'aquacrop' name '__ac_global_MOD_geteffectiverain_method';

function Geteffectiverain_PercentEffRain() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_geteffectiverain_percenteffrain';

function Geteffectiverain_ShowersInDecade() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_geteffectiverain_showersindecade';

function Geteffectiverain_RootNrEvap() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_geteffectiverain_rootnrevap';

procedure Seteffectiverain(constref effectiverain : rep_EffectiveRain);

procedure Seteffectiverain_Method(constref Method : rep_EffectiveRainMethod);

procedure __Seteffectiverain_Method(constref Method : shortint);
    external 'aquacrop' name '__ac_global_MOD_seteffectiverain_method';

procedure Seteffectiverain_PercentEffRain(constref PercentEffRain : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_seteffectiverain_percenteffrain';

procedure Seteffectiverain_ShowersInDecade(constref ShowersInDecade : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_seteffectiverain_showersindecade';

procedure Seteffectiverain_RootNrEvap(constref RootNrEvap : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_seteffectiverain_rootnrevap';


function GetSimulParam_EvapDeclineFactor() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_evapdeclinefactor';

function GetSimulParam_KcWetBare() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_kcwetbare';

function GetSimulParam_PercCCxHIfinal() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_percccxhifinal';

function GetSimulParam_RootPercentZmin() : integer;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_rootpercentzmin';

function GetSimulParam_MaxRootZoneExpansion() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_maxrootzoneexpansion';

function GetSimulParam_KsShapeFactorRoot() : Shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_ksshapefactorroot';

function GetSimulParam_TAWGermination() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_tawgermination';

function GetSimulParam_pAdjFAO() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_padjfao';

function GetSimulParam_DelayLowOxygen() : integer;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_delaylowoxygen';

function GetSimulParam_ExpFsen() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_expfsen';

function GetSimulParam_Beta() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_beta';

function GetSimulParam_ThicknessTopSWC() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_thicknesstopswc';

function GetSimulParam_EvapZmax() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_evapzmax';

function GetSimulParam_RunoffDepth() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_runoffdepth';

function GetSimulParam_CNcorrection() : Boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulparam_cncorrection_wrap';

function GetSimulParam_Tmin() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_tmin';

function GetSimulParam_Tmax() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_tmax';

function GetSimulParam_GDDMethod() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_gddmethod';

function GetSimulParam_PercRAW() : integer;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_percraw';

function GetSimulParam_CompDefThick() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_compdefthick';

function GetSimulParam_CropDay1() : integer;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_cropday1';

function GetSimulParam_Tbase() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_tbase';

function GetSimulParam_Tupper() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_tupper';

function GetSimulParam_IrriFwInSeason() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_irrifwinseason';

function GetSimulParam_IrriFwOffSeason() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_irrifwoffseason';

function GetSimulParam_SaltDiff() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_saltdiff';

function GetSimulParam_SaltSolub() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_saltsolub';

function GetSimulParam_ConstGwt() : Boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulparam_constgwt_wrap';

function GetSimulParam_RootNrDF() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_rootnrdf';

function GetSimulParam_IniAbstract() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_iniabstract';

function __GetSimulParam_EffectiveRain_method(): integer;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_method';

function GetSimulParam_EffectiveRain_method(): rep_EffectiveRainMethod;

function GetSimulParam_EffectiveRain_PercentEffRain(): shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_percenteffrain';

function GetSimulParam_EffectiveRain_ShowersInDecade(): shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_showersindecade';

function GetSimulParam_EffectiveRain_RootNrEvap(): shortint;
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_rootnrevap';


procedure SetSimulParam_EvapDeclineFactor(constref EvapDeclineFactor : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_evapdeclinefactor';

procedure SetSimulParam_KcWetBare(constref KcWetBare : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_kcwetbare';

procedure SetSimulParam_PercCCxHIfinal(constref PercCCxHIfinal : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_percccxhifinal';

procedure SetSimulParam_RootPercentZmin(constref RootPercentZmin : integer);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_rootpercentzmin';

procedure SetSimulParam_MaxRootZoneExpansion(constref MaxRootZoneExpansion : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_maxrootzoneexpansion';

procedure SetSimulParam_KsShapeFactorRoot(constref KsShapeFactorRoot : Shortint);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_ksshapefactorroot';

procedure SetSimulParam_TAWGermination(constref TAWGermination : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_tawgermination';

procedure SetSimulParam_pAdjFAO(constref pAdjFAO : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_padjfao';

procedure SetSimulParam_DelayLowOxygen(constref DelayLowOxygen : integer);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_delaylowoxygen';

procedure SetSimulParam_ExpFsen(constref ExpFsen : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_expfsen';

procedure SetSimulParam_Beta(constref Beta : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_beta';

procedure SetSimulParam_ThicknessTopSWC(constref ThicknessTopSWC : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_thicknesstopswc';

procedure SetSimulParam_EvapZmax(constref EvapZmax : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_evapzmax';

procedure SetSimulParam_RunoffDepth(constref RunoffDepth : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_runoffdepth';

procedure SetSimulParam_CNcorrection(constref CNcorrection : Boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulparam_cncorrection_wrap';

procedure SetSimulParam_Tmin(constref Tmin : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_tmin';

procedure SetSimulParam_Tmax(constref Tmax : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_tmax';

procedure SetSimulParam_GDDMethod(constref GDDMethod : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_gddmethod';

procedure SetSimulParam_PercRAW(constref PercRAW : integer);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_percraw';

procedure SetSimulParam_CompDefThick(constref CompDefThick : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_compdefthick';

procedure SetSimulParam_CropDay1(constref CropDay1 : integer);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_cropday1';

procedure SetSimulParam_Tbase(constref Tbase : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_tbase';

procedure SetSimulParam_Tupper(constref Tupper : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_tupper';

procedure SetSimulParam_IrriFwInSeason(constref IrriFwInSeason : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_irrifwinseason';

procedure SetSimulParam_IrriFwOffSeason(constref IrriFwOffSeason : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_irrifwoffseason';

procedure SetSimulParam_SaltDiff(constref SaltDiff : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_saltdiff';

procedure SetSimulParam_SaltSolub(constref SaltSolub : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_saltsolub';

procedure SetSimulParam_ConstGwt(constref ConstGwt : Boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulparam_constgwt_wrap';

procedure SetSimulParam_RootNrDF(constref RootNrDF : shortint);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_rootnrdf';

procedure SetSimulParam_IniAbstract(constref IniAbstract : shortint);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_iniabstract';

procedure __SetSimulParam_EffectiveRain_method(constref Method : integer);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_effectiverain_method';

procedure SetSimulParam_EffectiveRain_method(constref Method : rep_EffectiveRainMethod);

procedure SetSimulParam_EffectiveRain_PercentEffRain(constref PercentEffRain : shortint);
    external 'aquacrop' name '__ac_global_MOD_setsimulparam_effectiverain_percenteffrain';
	
procedure SetSimulParam_EffectiveRain_ShowersInDecade(constref ShowersInDecade : shortint);
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_showersindecade';
	
procedure SetSimulParam_EffectiveRain_RootNrEvap(constref RootNrEvap : shortint);
    external 'aquacrop' name '__ac_global_MOD_getsimulparam_effectiverain_rootnrevap';


function GetPathNameProg(): string;

function GetPathNameProg_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getpathnameprog_wrap';

procedure SetPathNameProg(constref str : string);

procedure SetPathNameprog_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setpathnameprog_wrap';

function GetPathNameOutp(): string;

function GetPathNameOutp_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getpathnameoutp_wrap';

procedure SetPathNameOutp(constref str : string);

procedure SetPathNameOutp_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setpathnameoutp_wrap';

function GetPathNameSimul(): string;

function GetPathNameSimul_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getpathnamesimul_wrap';

procedure SetPathNameSimul(constref str : string);

procedure SetPathNameSimul_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setpathnamesimul_wrap'; 

function GetOutputName(): string;

function GetOutputName_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getoutputname_wrap';

procedure SetOutputName(constref str : string);

procedure SetOutputName_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setoutputname_wrap';             

function GetProjectFile(): string;

function GetProjectFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getprojectfile_wrap';

procedure SetProjectFile(constref str : string);

procedure SetProjectFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setprojectfile_wrap';


function GetProjectFileFull(): string;

function GetProjectFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getprojectfilefull_wrap';

procedure SetProjectFileFull(constref str : string);

procedure SetProjectFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setprojectfilefull_wrap';


function GetMultipleProjectFile(): string;

function GetMultipleProjectFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getmultipleprojectfile_wrap';

procedure SetMultipleProjectFile(constref str : string);

procedure SetMultipleProjectFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setmultipleprojectfile_wrap';


function GetMultipleProjectFileFull(): string;

function GetMultipleProjectFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getmultipleprojectfilefull_wrap';

procedure SetMultipleProjectFileFull(constref str : string);

procedure SetMultipleProjectFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setmultipleprojectfilefull_wrap';


function GetFullFileNameProgramParameters(): string;

function GetFullFileNameProgramParameters_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getfullfilenameprogramparameters_wrap';

procedure SetFullFileNameProgramParameters(constref str : string);

procedure SetFullFileNameProgramParameters_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setfullfilenameprogramparameters_wrap';
                

function FileExists(constref full_name : string) : boolean;

function FileExists_wrap(
            constref full_name : string;
            constref strlen : integer) : boolean;
        external 'aquacrop' name '__ac_interface_global_MOD_fileexists_wrap';

function HIadjWStressAtFlowering(
            constref KsVeg,KsSto : double;
            constref a : ShortInt;
            constref b : double) : double;
         external 'aquacrop' name '__ac_global_MOD_hiadjwstressatflowering';

procedure SplitStringInTwoParams(
            constref StringIN : string;
            var Par1,Par2 : double);

procedure SplitStringInTwoParams_wrap(
            constref StringIN : PChar;
            constref strlen : integer;
            var Par1,Par2 : double);
        external 'aquacrop' name '__ac_interface_global_MOD_splitstringintwoparams_wrap';

procedure SplitStringInThreeParams(
            constref StringIN : string;
            var Par1,Par2, Par3 : double);

procedure SplitStringInThreeParams_wrap(
            constref StringIN : PChar;
            constref strlen : integer;
            var Par1,Par2,Par3 : double);
        external 'aquacrop' name '__ac_interface_global_MOD_splitstringinthreeparams_wrap';

function CO2ForSimulationPeriod(
            constref FromDayNr: integer;
            constref ToDayNr : integer) : double;
        external 'aquacrop' name '__ac_global_MOD_co2forsimulationperiod';

function GetRootZoneWC(): rep_RootZoneWC;
        external 'aquacrop' name '__ac_global_MOD_getrootzonewc';

procedure SetRootZoneWC_Actual(constref Actual : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_actual';

procedure SetRootZoneWC_FC(constref FC : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_fc';

procedure SetRootZoneWC_WP(constref WP : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_wp';

procedure SetRootZoneWC_SAT(constref SAT : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_sat';

procedure SetRootZoneWC_Leaf(constref Leaf : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_leaf';

procedure SetRootZoneWC_Thresh(constref Thresh : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_thresh';

procedure SetRootZoneWC_Sen(constref Sen : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_sen';

procedure SetRootZoneWC_ZtopAct(constref ZtopAct : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_ztopact';

procedure SetRootZoneWC_ZtopFC(constref ZtopFC : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_ztopfc';

procedure SetRootZoneWC_ZtopWP(constref ZtopWP : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_ztopwp';

procedure SetRootZoneWC_ZtopThresh(constref ZtopThresh : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonewc_ztopthresh';

function GetCalendarFile(): string;

function GetCalendarFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcalendarfile_wrap';

procedure SetCalendarFile(constref str : string);

procedure SetCalendarFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcalendarfile_wrap';

function GetCalendarFileFull(): string;

function GetCalendarFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcalendarfilefull_wrap';

procedure SetCalendarFileFull(constref str : string);

procedure SetCalendarFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcalendarfilefull_wrap';

function GetCalendarDescription(): string;

function GetCalendarDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcalendardescription_wrap';

procedure SetCalendarDescription(constref str : string);

procedure SetCalendarDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcalendardescription_wrap';

function GetCropFile(): string;

function GetCropFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcropfile_wrap';

procedure SetCropFile(constref str : string);

procedure SetCropFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcropfile_wrap';

function GetCropFileFull(): string;

function GetCropFileFull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcropfilefull_wrap';

procedure SetCropFileFull(constref str : string);

procedure SetCropFileFull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcropfilefull_wrap';

function GetCropDescription(): string;

function GetCropDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getcropdescription_wrap';

procedure SetCropDescription(constref str : string);

procedure SetCropDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setcropdescription_wrap';

function GetProfFile(): string;

function GetProfFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getproffile_wrap';

procedure SetProfFile(constref str : string);

procedure SetProfFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setproffile_wrap';

function GetProfFilefull(): string;

function GetProfFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getproffilefull_wrap';

procedure SetProfFilefull(constref str : string);

procedure SetProfFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setproffilefull_wrap';

function GetProfDescription(): string;

function GetProfDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getprofdescription_wrap';

procedure SetProfDescription(constref str : string);

procedure SetProfDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setprofdescription_wrap';

function GetManFile(): string;

function GetManFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanfile_wrap';

procedure SetManFile(constref str : string);

procedure SetManFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanfile_wrap';

function GetManFilefull(): string;

function GetManFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanfilefull_wrap';

procedure SetManFilefull(constref str : string);

procedure SetManFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanfilefull_wrap';

function GetOffSeasonFile(): string;

function GetOffSeasonFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getoffseasonfile_wrap';

procedure SetOffSeasonFile(constref str : string);

procedure SetOffSeasonFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setoffseasonfile_wrap';

function GetOffSeasonFilefull(): string;

function GetOffSeasonFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getoffseasonfilefull_wrap';

procedure SetOffSeasonFilefull(constref str : string);

procedure SetOffSeasonFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setoffseasonfilefull_wrap';

function GetObservationsFile(): string;

function GetObservationsFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getobservationsfile_wrap';

procedure SetObservationsFile(constref str : string);

procedure SetObservationsFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setobservationsfile_wrap';

function GetObservationsFilefull(): string;

function GetObservationsFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getobservationsfilefull_wrap';

procedure SetObservationsFilefull(constref str : string);

procedure SetObservationsFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setobservationsfilefull_wrap';


function GetObservationsDescription(): string;

function GetObservationsDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getobservationsdescription_wrap';

procedure SetObservationsDescription(constref str : string);

procedure SetObservationsDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setobservationsdescription_wrap';

function GetGroundWaterFile(): string;

function GetGroundWaterFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getgroundwaterfile_wrap';

procedure SetGroundWaterFile(constref str : string);

procedure SetGroundWaterFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setgroundwaterfile_wrap';

function GetGroundWaterFilefull(): string;

function GetGroundWaterFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getgroundwaterfilefull_wrap';

procedure SetGroundWaterFilefull(constref str : string);

procedure SetGroundWaterFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setgroundwaterfilefull_wrap';

function GetTemperatureFile(): string;

function GetTemperatureFile_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturefile_wrap';

procedure SetTemperatureFile(constref str : string);

procedure SetTemperatureFile_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturefile_wrap';

function GetTemperatureFilefull(): string;

function GetTemperatureFilefull_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturefilefull_wrap';

procedure SetTemperatureFilefull(constref str : string);

procedure SetTemperatureFilefull_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturefilefull_wrap';

function GetTemperatureDescription(): string;

function GetTemperatureDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturedescription_wrap';

procedure SetTemperatureDescription(constref str : string);

procedure SetTemperatureDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturedescription_wrap';

function GetClimDescription(): string;

function GetClimDescription_wrap(): PChar; 
        external 'aquacrop' name '__ac_interface_global_MOD_getclimdescription_wrap';

procedure SetClimDescription(constref str : string);

procedure SetClimDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setclimdescription_wrap';

function LeapYear(constref Year : integer) : boolean;
        external 'aquacrop' name '__ac_global_MOD_leapyear';

procedure LoadProjectDescription(
            constref FullNameProjectFile : string;
            var DescriptionOfProject : string);

procedure LoadProjectDescription_wrap(
            constref FullNameProjectFile : PChar;
            constref strlen1 : integer;
            var DescriptionOfProject : PChar;
            constref strlen2 : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_loadprojectdescription_wrap';

procedure CheckFilesInProject(
            constref TempFullFilename : string;
            constref Runi : integer;
            var AllOK : boolean);

procedure CheckFilesInProject_wrap(
            constref TempFullFilename : PChar;
            constref strlen : integer;
            constref Runi : integer;
            var AllOK : boolean);
        external 'aquacrop' name '__ac_interface_global_MOD_checkfilesinproject_wrap';


function GetIrriECw(): rep_IrriECw;
        external 'aquacrop' name '__ac_global_MOD_getirriecw';

procedure SetIrriECw_PreSeason(constref PreSeason : double);
        external 'aquacrop' name '__ac_global_MOD_setirriecw_preseason';

procedure SetIrriECw_PostSeason(constref PostSeason : double);
        external 'aquacrop' name '__ac_global_MOD_setirriecw_postseason';

function GetManagement_Cuttings_Considered(): boolean;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanagement_cuttings_considered_wrap';

function GetManagement_Cuttings_CGCPlus(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_cgcplus';

function GetManagement_Cuttings_CCcut(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_cccut';

function GetManagement_Cuttings_Day1(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_day1';

function GetManagement_Cuttings_NrDays(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_nrdays';

function GetManagement_Cuttings_Generate(): boolean;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanagement_cuttings_generate_wrap';

function __GetManagement_Cuttings_Criterion(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_criterion';

function GetManagement_Cuttings_Criterion(): rep_TimeCuttings;

function GetManagement_Cuttings_HarvestEnd(): boolean;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanagement_cuttings_harvestend_wrap';

function GetManagement_Cuttings_FirstDayNr(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cuttings_firstdaynr';

procedure SetManagement_Cuttings_Considered(constref Considered : boolean);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanagement_cuttings_considered_wrap';

procedure SetManagement_Cuttings_CCcut(constref CCcut : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_cccut';

procedure SetManagement_Cuttings_CGCPlus(constref CGCPlus : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_cgcplus';

procedure SetManagement_Cuttings_Day1(constref Day1 : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_day1';

procedure SetManagement_Cuttings_NrDays(constref NrDays : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_nrdays';

procedure SetManagement_Cuttings_Generate(constref Generate : boolean);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanagement_cuttings_generate_wrap';

procedure __SetManagement_Cuttings_Criterion(constref Criterion : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_criterion';

procedure SetManagement_Cuttings_Criterion(constref Criterion : rep_TimeCuttings);

procedure SetManagement_Cuttings_HarvestEnd(constref HarvestEnd : boolean);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanagement_cuttings_harvestend_wrap';

procedure SetManagement_Cuttings_FirstDayNr(constref FirstDayNr : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cuttings_firstdaynr';

function GetManagement_Mulch(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_mulch';

function GetManagement_SoilCoverBefore(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_soilcoverbefore';

function GetManagement_SoilCoverAfter(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_soilcoverafter';

function GetManagement_EffectMulchOffS(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_effectmulchoffs';

function GetManagement_EffectMulchInS(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_effectmulchins';

function GetManagement_FertilityStress(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_fertilitystress';

function GetManagement_BundHeight(): double;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_bundheight';

function GetManagement_RunoffOn(): boolean;
        external 'aquacrop' name '__ac_interface_global_MOD_getmanagement_runoffon_wrap';

function GetManagement_CNcorrection(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_cncorrection';

function GetManagement_WeedRC(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_weedrc';

function GetManagement_WeedDeltaRC(): integer;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_weeddeltarc';

function GetManagement_WeedShape(): double;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_weedshape';

function GetManagement_WeedAdj(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getmanagement_weedadj';

procedure SetManagement_Mulch(constref Mulch : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_mulch';

procedure SetManagement_SoilCoverBefore(constref Mulch : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_soilcoverbefore';

procedure SetManagement_SoilCoverAfter(constref After : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_soilcoverafter';

procedure SetManagement_EffectMulchOffS(constref EffectMulchOffS : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_effectmulchoffs';

procedure SetManagement_EffectMulchInS(constref EffectMulchInS : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_effectmulchins';

procedure SetManagement_FertilityStress(constref FertilityStress : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_fertilitystress';

procedure SetManagement_BundHeight(constref BundHeight : double);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_bundheight';

procedure SetManagement_RunOffOn(constref RunOffOn : boolean);
        external 'aquacrop' name '__ac_interface_global_MOD_setmanagement_runoffon_wrap';

procedure SetManagement_CNcorrection(constref CNcorrection : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_cncorrection';

procedure SetManagement_WeedRC(constref WeedRC : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_weedrc';

procedure SetManagement_WeedDeltaRC(constref WeedDeltaRC : integer);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_weeddeltarc';

procedure SetManagement_WeedShape(constref WeedShape : double);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_weedshape';

procedure SetManagement_WeedAdj(constref WeedAdj : shortint);
        external 'aquacrop' name '__ac_global_MOD_setmanagement_weedadj';

function GetCropFileSet(): rep_CropFileSet;
        external 'aquacrop' name '__ac_global_MOD_getcropfileset';

procedure SetCropFileSet_DaysFromSenescenceToEnd(constref DaysFromSenescenceToEnd : double);
        external 'aquacrop' name '__ac_global_MOD_setcropfileset_daysfromsenescencetoend';

procedure SetCropFileSet_DaysToHarvest(constref DaysToHarvest : double);
        external 'aquacrop' name '__ac_global_MOD_setcropfileset_daystoharvest';

procedure SetCropFileSet_GDDaysFromSenescenceToEnd(constref GDDaysFromSenescenceToEnd : double);
        external 'aquacrop' name '__ac_global_MOD_setcropfileset_gddaysfromsenescencetoend';

procedure SetCropFileSet_GDDaysToHarvest(constref GDDaysToHarvest : double);
        external 'aquacrop' name '__ac_global_MOD_setcropfileset_gddaystoharvest';

function GetSumWaBal() : rep_sum;

function GetSumWaBal_Epot() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_epot';

function GetSumWaBal_Tpot() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_tpot';

function GetSumWaBal_Rain() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_rain';

function GetSumWaBal_Irrigation() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_irrigation';

function GetSumWaBal_Infiltrated() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_infiltrated';

function GetSumWaBal_Runoff() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_runoff';

function GetSumWaBal_Drain() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_drain';

function GetSumWaBal_Eact() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_eact';

function GetSumWaBal_Tact() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_tact';

function GetSumWaBal_TrW() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_trw';

function GetSumWaBal_ECropCycle() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_ecropcycle';

function GetSumWaBal_CRwater() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_crwater';

function GetSumWaBal_Biomass() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_biomass';

function GetSumWaBal_YieldPart() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_yieldpart';

function GetSumWaBal_BiomassPot() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_biomasspot';

function GetSumWaBal_BiomassUnlim() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_biomassunlim';

function GetSumWaBal_BiomassTot() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_biomasstot';

function GetSumWaBal_SaltIn() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_saltin';

function GetSumWaBal_SaltOut() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_saltout';

function GetSumWaBal_CRsalt() : double;
        external 'aquacrop' name '__ac_global_MOD_getsumwabal_crsalt';

procedure SetSumWaBal(constref SumWaBal : rep_sum);

procedure SetSumWaBal_Epot(constref Epot : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_epot';

procedure SetSumWaBal_Tpot(constref Tpot : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_tpot';

procedure SetSumWaBal_Rain(constref Rain : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_rain';

procedure SetSumWaBal_Irrigation(constref Irrigation : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_irrigation';

procedure SetSumWaBal_Infiltrated(constref Infiltrated : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_infiltrated';

procedure SetSumWaBal_Runoff(constref Runoff : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_runoff';

procedure SetSumWaBal_Drain(constref Drain : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_drain';

procedure SetSumWaBal_Eact(constref Eact : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_eact';

procedure SetSumWaBal_Tact(constref Tact : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_tact';

procedure SetSumWaBal_TrW(constref TrW : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_trw';

procedure SetSumWaBal_ECropCycle(constref ECropCycle : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_ecropcycle';

procedure SetSumWaBal_CRwater(constref CRwater : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_crwater';

procedure SetSumWaBal_Biomass(constref Biomass : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_biomass';

procedure SetSumWaBal_yieldpart(constref yieldpart : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_yieldpart';

procedure SetSumWaBal_BiomassPot(constref BiomassPot : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_biomasspot';

procedure SetSumWaBal_BiomassUnlim(constref BiomassUnlim : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_biomassunlim';

procedure SetSumWaBal_BiomassTot(constref BiomassTot : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_biomasstot';

procedure SetSumWaBal_SaltIn(constref SaltIn : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_saltin';

procedure SetSumWaBal_SaltOut(constref SaltOut : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_saltout';

procedure SetSumWaBal_CRsalt(constref CRsalt : double);
        external 'aquacrop' name '__ac_global_MOD_setsumwabal_crsalt';

function GetSoil(): rep_Soil;
        external 'aquacrop' name '__ac_global_MOD_getsoil';

function GetSoil_REW(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getsoil_rew';

function GetSoil_CNvalue(): shortint;
        external 'aquacrop' name '__ac_global_MOD_getsoil_cnvalue';

procedure SetSoil_REW(constref REW : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_setsoil_rew';

function GetSoil_NrSoilLayers() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoil_nrsoillayers';

procedure SetSoil_NrSoilLayers(constref NrSoilLayers : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_setsoil_nrsoillayers';

procedure SetSoil_CNvalue(constref CNvalue : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_setsoil_cnvalue';

procedure SetSoil_RootMax(constref RootMax : Single);
        external 'aquacrop' name '__ac_global_MOD_setsoil_rootmax';

function GetOnset() : rep_Onset;

function GetOnset_GenerateOn() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getonset_generateon_wrap';

function GetOnset_GenerateTempOn() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getonset_generatetempon_wrap';

function GetOnset_Criterion() : repCriterion;

function __GetOnset_Criterion() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getonset_criterion';

function GetOnset_AirTCriterion() : repAirTCriterion;

function __GetOnset_AirTCriterion() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getonset_airtcriterion';

function GetOnset_StartSearchDayNr() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getonset_startsearchdaynr';

function GetOnset_StopSearchDayNr() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getonset_stopsearchdaynr';

function GetOnset_LengthSearchPeriod() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getonset_lengthsearchperiod';

procedure SetOnset(constref Onset : rep_Onset);

procedure SetOnset_GenerateOn(constref GenerateOn : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setonset_generateon_wrap';

procedure SetOnset_GenerateTempOn(constref GenerateTempOn : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setonset_generatetempon_wrap';

procedure SetOnset_Criterion(constref Criterion : repCriterion);

procedure __SetOnset_Criterion(constref Criterion : shortint);
    external 'aquacrop' name '__ac_global_MOD_setonset_criterion';

procedure SetOnset_AirTCriterion(constref AirTCriterion : repAirTCriterion);

procedure __SetOnset_AirTCriterion(constref AirTCriterion : shortint);
    external 'aquacrop' name '__ac_global_MOD_setonset_airtcriterion';

procedure SetOnset_StartSearchDayNr(constref StartSearchDayNr : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setonset_startsearchdaynr';

procedure SetOnset_StopSearchDayNr(constref StopSearchDayNr : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setonset_stopsearchdaynr';

procedure SetOnset_LengthSearchPeriod(constref LengthSearchPeriod : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setonset_lengthsearchperiod';

function GetEndSeason() : rep_EndSeason;

function GetEndSeason_ExtraYears() : Integer;
    external 'aquacrop' name '__ac_global_MOD_getendseason_extrayears';

function GetEndSeason_GenerateTempOn() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getendseason_generatetempon_wrap';

function GetEndSeason_AirTCriterion() : repAirTCriterion;

function __GetEndSeason_AirTCriterion() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getendseason_airtcriterion';

function GetEndSeason_StartSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getendseason_startsearchdaynr';

function GetEndSeason_StopSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getendseason_stopsearchdaynr';

function GetEndSeason_LengthSearchPeriod() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getendseason_lengthsearchperiod';

procedure SetEndSeason(constref EndSeason : rep_EndSeason);

procedure SetEndSeason_ExtraYears(constref ExtraYears : Integer);
    external 'aquacrop' name '__ac_global_MOD_setendseason_extrayears';

procedure SetEndSeason_GenerateTempOn(constref GenerateTempOn : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setendseason_generatetempon_wrap';

procedure SetEndSeason_AirTCriterion(constref AirTCriterion : repAirTCriterion);

procedure __SetEndSeason_AirTCriterion(constref AirTCriterion : shortint);
    external 'aquacrop' name '__ac_global_MOD_setendseason_airtcriterion';

procedure SetEndSeason_StartSearchDayNr(constref StartSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setendseason_startsearchdaynr';

procedure SetEndSeason_StopSearchDayNr(constref StopSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setendseason_stopsearchdaynr';

procedure SetEndSeason_LengthSearchPeriod(constref LengthSearchPeriod : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setendseason_lengthsearchperiod';


function GetPerennialPeriod() : rep_PerennialPeriod;

function GetPerennialPeriod_GenerateOnset() : boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getperennialperiod_generateonset_wrap';

function GetPerennialPeriod_OnsetCriterion() : repAirTCriterion;

function __GetPerennialPeriod_OnsetCriterion() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetcriterion';

function GetPerennialPeriod_OnsetFirstDay() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetfirstday';

function GetPerennialPeriod_OnsetFirstMonth() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetfirstmonth';

function GetPerennialPeriod_OnsetStartSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetstartsearchdaynr';

function GetPerennialPeriod_OnsetStopSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetstopsearchdaynr';

function GetPerennialPeriod_OnsetLengthSearchPeriod() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetlengthsearchperiod';

function GetPerennialPeriod_OnsetThresholdValue() : double;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetthresholdvalue';

function GetPerennialPeriod_OnsetPeriodValue() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetperiodvalue';

function GetPerennialPeriod_OnsetOccurrence() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_onsetoccurrence';

function GetPerennialPeriod_GenerateEnd() : boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getperennialperiod_generateend_wrap';

function GetPerennialPeriod_EndCriterion() : repAirTCriterion;

function __GetPerennialPeriod_EndCriterion() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endcriterion';

function GetPerennialPeriod_EndLastDay() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endlastday';

function GetPerennialPeriod_EndLastMonth() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endlastmonth';

function GetPerennialPeriod_ExtraYears() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_extrayears';

function GetPerennialPeriod_EndStartSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endstartsearchdaynr';

function GetPerennialPeriod_EndStopSearchDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endstopsearchdaynr';

function GetPerennialPeriod_EndLengthSearchPeriod() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endlengthsearchperiod';

function GetPerennialPeriod_EndThresholdValue() : double;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endthresholdvalue';

function GetPerennialPeriod_EndPeriodValue() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endperiodvalue';

function GetPerennialPeriod_EndOccurrence() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_endoccurrence';

function GetPerennialPeriod_GeneratedDayNrOnset() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_generateddaynronset';

function GetPerennialPeriod_GeneratedDayNrEnd() : integer;
    external 'aquacrop' name '__ac_global_MOD_getperennialperiod_generateddaynrend';

procedure SetPerennialPeriod(constref PerennialPeriod : rep_PerennialPeriod);

procedure SetPerennialPeriod_GenerateOnset(constref GenerateOnset : boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setperennialperiod_generateonset_wrap';

procedure SetPerennialPeriod_OnsetCriterion(constref OnsetCriterion : repAirTCriterion);

procedure __SetPerennialPeriod_OnsetCriterion(constref OnsetCriterion : shortint);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetcriterion';

procedure SetPerennialPeriod_OnsetFirstDay(constref OnsetFirstDay : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetfirstday';

procedure SetPerennialPeriod_OnsetFirstMonth(constref OnsetFirstMonth : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetfirstmonth';

procedure SetPerennialPeriod_OnsetStartSearchDayNr(constref OnsetStartSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetstartsearchdaynr';

procedure SetPerennialPeriod_OnsetStopSearchDayNr(constref OnsetStopSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetstopsearchdaynr';

procedure SetPerennialPeriod_OnsetLengthSearchPeriod(constref OnsetLengthSearchPeriod : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetlengthsearchperiod';

procedure SetPerennialPeriod_OnsetThresholdValue(constref OnsetThresholdValue : double);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetthresholdvalue';

procedure SetPerennialPeriod_OnsetPeriodValue(constref OnsetPeriodValue : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetperiodvalue';

procedure SetPerennialPeriod_OnsetOccurrence(constref OnsetOccurrence : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_onsetoccurrence';

procedure SetPerennialPeriod_GenerateEnd(constref GenerateEnd : boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setperennialperiod_generateend_wrap';

procedure SetPerennialPeriod_EndCriterion(constref EndCriterion : repAirTCriterion);

procedure __SetPerennialPeriod_EndCriterion(constref EndCriterion : shortint);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endcriterion';

procedure SetPerennialPeriod_EndLastDay(constref EndLastDay : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endlastday';

procedure SetPerennialPeriod_EndLastMonth(constref EndLastMonth : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endlastmonth';

procedure SetPerennialPeriod_ExtraYears(constref ExtraYears : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_extrayears';

procedure SetPerennialPeriod_EndStartSearchDayNr(constref EndStartSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endstartsearchdaynr';

procedure SetPerennialPeriod_EndStopSearchDayNr(constref EndStopSearchDayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endstopsearchdaynr';

procedure SetPerennialPeriod_EndLengthSearchPeriod(constref EndLengthSearchPeriod : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endlengthsearchperiod';

procedure SetPerennialPeriod_EndThresholdValue(constref EndThresholdValue : double);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endthresholdvalue';

procedure SetPerennialPeriod_EndPeriodValue(constref EndPeriodValue : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endperiodvalue';

procedure SetPerennialPeriod_EndOccurrence(constref EndOccurrence : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_endoccurrence';

procedure SetPerennialPeriod_GeneratedDayNrOnset(constref GeneratedDayNrOnset : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_generateddaynronset';

procedure SetPerennialPeriod_GeneratedDayNrEnd(constref GeneratedDayNrEnd : integer);
    external 'aquacrop' name '__ac_global_MOD_setperennialperiod_generateddaynrend';


function GetTotalSaltContent(): rep_Content;
        external 'aquacrop' name '__ac_global_MOD_gettotalsaltcontent';

procedure SetTotalSaltContent_BeginDay(constref BeginDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalsaltcontent_beginday';

procedure SetTotalSaltContent_EndDay(constref EndDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalsaltcontent_endday';

procedure SetTotalSaltContent_ErrorDay(constref ErrorDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalsaltcontent_errorday';

function GetTotalWaterContent(): rep_Content;
        external 'aquacrop' name '__ac_global_MOD_gettotalwatercontent';
        
function GetTotalWaterContent_BeginDay(): rep_Content;
        external 'aquacrop' name '__ac_global_MOD_gettotalwatercontent_beginday';

procedure SetTotalWaterContent(constref TotalWaterContent : rep_Content);

procedure SetTotalWaterContent_BeginDay(constref BeginDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalwatercontent_beginday';

procedure SetTotalWaterContent_EndDay(constref EndDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalwatercontent_endday';

procedure SetTotalWaterContent_ErrorDay(constref ErrorDay : double);
        external 'aquacrop' name '__ac_global_MOD_settotalwatercontent_errorday';

function GetRootZoneSalt(): rep_RootZoneSalt;
        external 'aquacrop' name '__ac_global_MOD_getrootzonesalt';

procedure SetRootZoneSalt_ECe(constref ECe : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonesalt_ece';

procedure SetRootZoneSalt_ECsw(constref ECsw : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonesalt_ecsw';

procedure SetRootZoneSalt_ECswFC(constref ECswFC : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonesalt_ecswfc';

procedure SetRootZoneSalt_KsSalt(constref KsSalt : double);
        external 'aquacrop' name '__ac_global_MOD_setrootzonesalt_kssalt';

function __GetGenerateTimeMode(): integer;
        external 'aquacrop' name '__ac_global_MOD_getgeneratetimemode';

function GetGenerateTimeMode(): rep_GenerateTimeMode;

procedure __SetGenerateTimeMode(constref GenerateTimeMode : integer);
        external 'aquacrop' name '__ac_global_MOD_setgeneratetimemode';

procedure SetGenerateTimeMode(constref GenerateTimeMode : rep_GenerateTimeMode);

function __GetGenerateDepthMode(): integer;
        external 'aquacrop' name '__ac_global_MOD_getgeneratedepthmode';

function GetGenerateDepthMode(): rep_GenerateDepthMode;

procedure __SetGenerateDepthMode(constref IrriDepthMode : integer);
        external 'aquacrop' name '__ac_global_MOD_setgeneratedepthmode';

procedure SetGenerateDepthMode(constref GenerateDepthMode : rep_GenerateDepthMode);

function __GetIrriMode(): integer;
        external 'aquacrop' name '__ac_global_MOD_getirrimode';

function GetIrriMode(): rep_IrriMode;

procedure NoIrrigation();
        external 'aquacrop' name '__ac_global_MOD_noirrigation';

procedure LoadIrriScheduleInfo(constref FullName : string);

procedure LoadIrriScheduleInfo_wrap(constref FullName : PChar;
                           constref strlen1 : integer);  
        external 'aquacrop' name '__ac_interface_global_MOD_loadirrischeduleinfo_wrap';

procedure __SetIrriMode(constref IrriMode : integer);
        external 'aquacrop' name '__ac_global_MOD_setirrimode';

procedure SetIrriMode(constref IrriMode : rep_IrriMode);

function __GetIrriMethod(): integer;
        external 'aquacrop' name '__ac_global_MOD_getirrimethod';

function GetIrriMethod(): rep_IrriMethod;

procedure __SetIrriMethod(constref IrriMethod : integer);
        external 'aquacrop' name '__ac_global_MOD_setirrimethod';

procedure SetIrriMethod(constref IrriMethod : rep_IrriMethod);

function GetTemperatureRecord(): rep_clim;

function __GetTemperatureRecord_DataType() : shortint;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_datatype';

function GetTemperatureRecord_DataType() : rep_datatype;

function GetTemperatureRecord_FromD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromd';

function GetTemperatureRecord_FromM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromm';

function GetTemperatureRecord_FromY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromy';

function GetTemperatureRecord_ToD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tod';

function GetTemperatureRecord_ToM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tom';

function GetTemperatureRecord_ToY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_toy';

function GetTemperatureRecord_FromDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromdaynr';

function GetTemperatureRecord_ToDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_todaynr';

function GetTemperatureRecord_NrObs() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_nrobs';

function GetTemperatureRecord_FromString(): string;

function GetTemperatureRecord_FromString_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_fromstring_wrap';

function GetTemperatureRecord_ToString() : string;

function GetTemperatureRecord_ToString_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_tostring_wrap';

procedure SetTemperatureRecord(constref TemperatureRecord : rep_clim);

procedure __SetTemperatureRecord_DataType(constref DataType : shortint);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_datatype';

procedure SetTemperatureRecord_DataType(constref DataType : rep_datatype);

procedure SetTemperatureRecord_FromD(constref FromD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromd';

procedure SetTemperatureRecord_FromM(constref FromM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromm';

procedure SetTemperatureRecord_FromY(constref FromY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromy';

procedure SetTemperatureRecord_ToD(constref ToD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tod';

procedure SetTemperatureRecord_ToM(constref ToM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tom';

procedure SetTemperatureRecord_ToY(constref ToY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_toy';

procedure SetTemperatureRecord_FromDayNr(constref FromDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromdaynr';

procedure SetTemperatureRecord_ToDayNr(constref ToDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_todaynr';

procedure SetTemperatureRecord_NrObs(constref NrObs : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_nrobs';

procedure SetTemperatureRecord_FromString(constref str : string);

procedure SetTemperatureRecord_FromString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_fromstring_wrap';

procedure SetTemperatureRecord_ToString(constref str : string);

procedure SetTemperatureRecord_ToString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_tostring_wrap';

PROCEDURE GlobalZero(
        VAR SumWabal : rep_sum);
     external 'aquacrop' name '__ac_global_MOD_globalzero'; 
     
function GetClimRecord(): rep_clim;

function __GetClimRecord_DataType() : shortint;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_datatype';

function GetClimRecord_DataType() : rep_datatype;

function GetClimRecord_FromD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromd';

function GetClimRecord_FromM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromm';

function GetClimRecord_FromY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromy';

function GetClimRecord_ToD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tod';

function GetClimRecord_ToM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tom';

function GetClimRecord_ToY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_toy';

function GetClimRecord_FromDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromdaynr';

function GetClimRecord_ToDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_todaynr';

function GetClimRecord_NrObs() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_nrobs';

function GetClimRecord_FromString(): string;

function GetClimRecord_FromString_wrap(): PChar; 
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_fromstring_wrap';

function GetClimRecord_ToString() : string;

function GetClimRecord_ToString_wrap(): PChar; 
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_tostring_wrap';

procedure SetClimRecord(constref ClimRecord : rep_clim);

procedure __SetClimRecord_DataType(constref DataType : shortint);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_datatype';

procedure SetClimRecord_DataType(constref DataType : rep_datatype);

procedure SetClimRecord_FromD(constref FromD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromd';

procedure SetClimRecord_FromM(constref FromM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromm';

procedure SetClimRecord_FromY(constref FromY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromy';

procedure SetClimRecord_ToD(constref ToD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tod';

procedure SetClimRecord_ToM(constref ToM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tom';

procedure SetClimRecord_ToY(constref ToY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_toy';

procedure SetClimRecord_FromDayNr(constref FromDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromdaynr';

procedure SetClimRecord_ToDayNr(constref ToDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_todaynr';

procedure SetClimRecord_NrObs(constref NrObs : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_nrobs';

procedure SetClimRecord_FromString(constref str : string);

procedure SetClimRecord_FromString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_fromstring_wrap';

procedure SetClimRecord_ToString(constref str : string);

procedure SetClimRecord_ToString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_tostring_wrap';

function GetRainRecord(): rep_clim;

function __GetRainRecord_DataType() : shortint;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_datatype';

function GetRainRecord_DataType() : rep_datatype;

function GetRainRecord_FromD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromd';

function GetRainRecord_FromM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromm';

function GetRainRecord_FromY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromy';

function GetRainRecord_ToD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tod';

function GetRainRecord_ToM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tom';

function GetRainRecord_ToY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_toy';

function GetRainRecord_FromDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromdaynr';

function GetRainRecord_ToDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_todaynr';

function GetRainRecord_NrObs() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_nrobs';

function GetRainRecord_FromString(): string;

function GetRainRecord_FromString_wrap(): PChar; 
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_fromstring_wrap';

function GetRainRecord_ToString() : string;

function GetRainRecord_ToString_wrap(): PChar; 
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_tostring_wrap';

procedure SetRainRecord(constref RainRecord : rep_clim);

procedure __SetRainRecord_DataType(constref DataType : shortint);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_datatype';

procedure SetRainRecord_DataType(constref DataType : rep_datatype);

procedure SetRainRecord_FromD(constref FromD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromd';

procedure SetRainRecord_FromM(constref FromM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromm';

procedure SetRainRecord_FromY(constref FromY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromy';

procedure SetRainRecord_ToD(constref ToD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tod';

procedure SetRainRecord_ToM(constref ToM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tom';

procedure SetRainRecord_ToY(constref ToY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_toy';

procedure SetRainRecord_FromDayNr(constref FromDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromdaynr';

procedure SetRainRecord_ToDayNr(constref ToDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_todaynr';

procedure SetRainRecord_NrObs(constref NrObs : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_nrobs';

procedure SetRainRecord_FromString(constref str : string);

procedure SetRainRecord_FromString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_fromstring_wrap';

procedure SetRainRecord_ToString(constref str : string);

procedure SetRainRecord_ToString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_tostring_wrap';

function GetEToRecord(): rep_clim;

function __GetEToRecord_DataType() : shortint;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_datatype';

function GetEToRecord_DataType() : rep_datatype;

function GetEToRecord_FromD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromd';

function GetEToRecord_FromM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromm';

function GetEToRecord_FromY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromy';

function GetEToRecord_ToD() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tod';

function GetEToRecord_ToM() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_tom';

function GetEToRecord_ToY() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_toy';

function GetEToRecord_FromDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_fromdaynr';

function GetEToRecord_ToDayNr() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_todaynr';

function GetEToRecord_NrObs() : integer;
         external 'aquacrop' name '__ac_global_MOD_gettemperaturerecord_nrobs';

function GetEToRecord_FromString(): string;

function GetEToRecord_FromString_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_fromstring_wrap';

function GetEToRecord_ToString() : string;

function GetEToRecord_ToString_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_gettemperaturerecord_tostring_wrap';

procedure SetEToRecord(constref EToRecord : rep_clim);

procedure __SetEToRecord_DataType(constref DataType : shortint);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_datatype';

procedure SetEToRecord_DataType(constref DataType : rep_datatype);

procedure SetEToRecord_FromD(constref FromD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromd';

procedure SetEToRecord_FromM(constref FromM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromm';

procedure SetEToRecord_FromY(constref FromY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromy';

procedure SetEToRecord_ToD(constref ToD : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tod';

procedure SetEToRecord_ToM(constref ToM : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_tom';

procedure SetEToRecord_ToY(constref ToY : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_toy';

procedure SetEToRecord_FromDayNr(constref FromDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_fromdaynr';

procedure SetEToRecord_ToDayNr(constref ToDayNr : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_todaynr';

procedure SetEToRecord_NrObs(constref NrObs : integer);
         external 'aquacrop' name '__ac_global_MOD_settemperaturerecord_nrobs';

procedure SetEToRecord_FromString(constref str : string);

procedure SetEToRecord_FromString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_fromstring_wrap';

procedure SetEToRecord_ToString(constref str : string);

procedure SetEToRecord_ToString_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_settemperaturerecord_tostring_wrap';

function GetSimulation() : rep_sim;

function GetSimulation_FromDayNr() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_fromdaynr';

function GetSimulation_ToDayNr() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_todaynr';

function GetSimulation_SurfaceStorageIni() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_surfacestorageini';

function GetSimulation_ECStorageIni() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_ecstorageini';

function GetSimulation_CCini() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_ccini';

function GetSimulation_Bini() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_bini';

function GetSimulation_Zrini() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_zrini';

function GetSimulation_LinkCropToSimPeriod() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_linkcroptosimperiod_wrap';

function GetSimulation_ResetIniSWC() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_resetiniswc_wrap';

function GetSimulation_InitialStep() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_initialstep';

function GetSimulation_EvapLimitON() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_evaplimiton_wrap';

function GetSimulation_EvapWCsurf() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_evapwcsurf';

function GetSimulation_EvapStartStg2() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_evapstartstg2';

function GetSimulation_EvapZ() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_evapz';

function GetSimulation_HIfinal() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_hifinal';

function GetSimulation_DelayedDays() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_delayeddays';

function GetSimulation_Germinate() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_germinate_wrap';

function GetSimulation_SumEToStress() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_sumetostress';

function GetSimulation_SumGDD() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_sumgdd';

function GetSimulation_SumGDDfromDay1() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_sumgddfromday1';

function GetSimulation_SCor() : single;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_scor';

function GetSimulation_MultipleRun() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_multiplerun_wrap';

function GetSimulation_NrRuns() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_nrruns';

function GetSimulation_MultipleRunWithKeepSWC() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_multiplerunwithkeepswc_wrap';

function GetSimulation_MultipleRunConstZrx() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_multiplerunconstzrx';

function GetSimulation_IrriECw() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_irriecw';

function GetSimulation_DayAnaero() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_dayanaero';

function GetSimulation_SalinityConsidered() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_salinityconsidered_wrap';

function GetSimulation_ProtectedSeedling() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_protectedseedling_wrap';

function GetSimulation_SWCtopSoilConsidered() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_swctopsoilconsidered_wrap';

function GetSimulation_LengthCuttingInterval() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_lengthcuttinginterval';

function GetSimulation_YearSeason() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_yearseason';

function GetSimulation_RCadj() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_rcadj';

function GetSimulation_YearStartCropCycle() : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_yearstartcropcycle';

function GetSimulation_CropDay1Previous() : LongInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_cropday1previous';

procedure SetSimulation(constref Simulation : rep_sim);

procedure SetSimulation_FromDayNr(constref FromDayNr : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_fromdaynr';

procedure SetSimulation_ToDayNr(constref ToDayNr : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_todaynr';

procedure SetSimulation_SurfaceStorageIni(constref SurfaceStorageIni : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_surfacestorageini';

procedure SetSimulation_ECStorageIni(constref ECStorageIni : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_ecstorageini';

procedure SetSimulation_CCini(constref CCini : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_ccini';

procedure SetSimulation_Bini(constref Bini : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_bini';

procedure SetSimulation_Zrini(constref Zrini : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_zrini';

procedure SetSimulation_LinkCropToSimPeriod(constref LinkCropToSimPeriod : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_linkcroptosimperiod_wrap';

procedure SetSimulation_ResetIniSWC(constref ResetIniSWC : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_resetiniswc_wrap';

procedure SetSimulation_InitialStep(constref InitialStep : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_initialstep';

procedure SetSimulation_EvapLimitON(constref EvapLimitON : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_evaplimiton_wrap';

procedure SetSimulation_EvapWCsurf(constref EvapWCsurf : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_evapwcsurf';

procedure SetSimulation_EvapStartStg2(constref EvapStartStg2 : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_evapstartstg2';

procedure SetSimulation_EvapZ(constref EvapZ : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_evapz';

procedure SetSimulation_HIfinal(constref HIfinal : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_hifinal';

procedure SetSimulation_DelayedDays(constref DelayedDays : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_delayeddays';

procedure SetSimulation_Germinate(constref Germinate : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_germinate_wrap';

procedure SetSimulation_SumEToStress(constref SumEToStress : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_sumetostress';

procedure SetSimulation_SumGDD(constref SumGDD : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_sumgdd';

procedure SetSimulation_SumGDDfromDay1(constref SumGDDfromDay1 : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_sumgddfromday1';

procedure SetSimulation_SCor(constref SCor : single);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_scor';

procedure SetSimulation_MultipleRun(constref MultipleRun : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_multiplerun_wrap';

procedure SetSimulation_NrRuns(constref NrRuns : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_nrruns';

procedure SetSimulation_MultipleRunWithKeepSWC(constref MultipleRunWithKeepSWC : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_multiplerunwithkeepswc_wrap';

procedure SetSimulation_MultipleRunConstZrx(constref MultipleRunConstZrx : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_multiplerunconstzrx';

procedure SetSimulation_IrriECw(constref IrriECw : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_irriecw';

procedure SetSimulation_DayAnaero(constref DayAnaero : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_dayanaero';

procedure SetSimulation_SalinityConsidered(constref SalinityConsidered : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_salinityconsidered_wrap';

procedure SetSimulation_ProtectedSeedling(constref ProtectedSeedling : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_protectedseedling_wrap';

procedure SetSimulation_SWCtopSoilConsidered(constref SWCtopSoilConsidered : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_swctopsoilconsidered_wrap';

procedure SetSimulation_LengthCuttingInterval(constref LengthCuttingInterval : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_lengthcuttinginterval';

procedure SetSimulation_YearSeason(constref YearSeason : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_yearseason';

procedure SetSimulation_RCadj(constref RCadj : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_rcadj';

procedure SetSimulation_YearStartCropCycle(constref YearStartCropCycle : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_yearstartcropcycle';

procedure SetSimulation_CropDay1Previous(constref CropDay1Previous : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_cropday1previous';

function GetSimulation_IniSWC() : rep_IniSWC;

function GetSimulation_IniSWC_AtDepths() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_iniswc_atdepths_wrap';

function GetSimulation_IniSWC_NrLoc() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_iniswc_nrloc';

function GetSimulation_IniSWC_AtFC() : BOOLEAN;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_iniswc_atfc_wrap';

procedure SetSimulation_IniSWC(constref Simulation_IniSWC : rep_IniSWC);

procedure SetSimulation_IniSWC_AtDepths(constref AtDepths : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_iniswc_atdepths_wrap';

procedure SetSimulation_IniSWC_NrLoc(constref NrLoc : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_iniswc_nrloc';

procedure SetSimulation_IniSWC_AtFC(constref AtFC : BOOLEAN);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_iniswc_atfc_wrap';

function GetSimulation_EffectStress() : rep_EffectStress;

function GetSimulation_EffectStress_RedCGC() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_effectstress_redcgc';

function GetSimulation_EffectStress_RedCCX() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_effectstress_redccx';

function GetSimulation_EffectStress_RedWP() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_effectstress_redwp';

function GetSimulation_EffectStress_CDecline() : Double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_effectstress_cdecline';

function GetSimulation_EffectStress_RedKsSto() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_effectstress_redkssto';

procedure SetSimulation_EffectStress(constref Simulation_EffectStress : rep_EffectStress);

procedure SetSimulation_EffectStress_RedCGC(constref RedCGC : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_effectstress_redcgc';

procedure SetSimulation_EffectStress_RedCCX(constref RedCCX : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_effectstress_redccx';

procedure SetSimulation_EffectStress_RedWP(constref RedWP : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_effectstress_redwp';

procedure SetSimulation_EffectStress_CDecline(constref CDecline : Double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_effectstress_cdecline';

procedure SetSimulation_EffectStress_RedKsSto(constref RedKsSto : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_effectstress_redkssto';

function GetSimulation_ThetaIni() : rep_IniComp;

function GetSimulation_ThetaIni_i(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_thetaini_i';

function GetSimulation_ECeIni() : rep_IniComp;

function GetSimulation_ECeIni_i(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_eceini_i';

procedure SetSimulation_ThetaIni(constref ThetaIni : rep_IniComp);

procedure SetSimulation_ThetaIni_i(constref i : integer;
                                   constref ThetaIni_i : double)
    external 'aquacrop' name '__ac_global_MOD_setsimulation_thetaini_i';

procedure SetSimulation_ECeIni(constref ECeIni : rep_IniComp);

procedure SetSimulation_ECeIni_i(constref i : integer;
                              constref ECeIni_i : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_eceini_i';

function GetSimulation_IniSWC_Loc() : rep_IniComp;

function GetSimulation_IniSWC_Loc_i(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_iniswc_loc_i';

function GetSimulation_IniSWC_VolProc() : rep_IniComp;

function GetSimulation_IniSWC_VolProc_i(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_iniswc_volproc_i';

function GetSimulation_IniSWC_SaltECe() : rep_IniComp;

function GetSimulation_IniSWC_SaltECe_i(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_iniswc_saltece_i';

procedure SetSimulation_IniSWC_Loc(constref Loc : rep_IniComp);

procedure SetSimulation_IniSWC_Loc_i(constref i : integer;
                                     constref Loc_i : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_iniswc_loc_i';

procedure SetSimulation_IniSWC_VolProc(constref VolProc : rep_IniComp);

procedure SetSimulation_IniSWC_VolProc_i(constref i : integer; 
                                         constref VolProc_i : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_iniswc_volproc_i';

procedure SetSimulation_IniSWC_SaltECe(constref SaltECe : rep_IniComp);

procedure SetSimulation_IniSWC_SaltECe_i(constref i : integer; 
                                         constref SaltECe_i : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_iniswc_saltece_i';

function GetSimulation_Storage() : rep_storage;

function GetSimulation_Storage_Btotal() : double;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_storage_btotal';

function GetSimulation_Storage_CropString() : string;

function GetSimulation_Storage_CropString_wrap() : PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_getsimulation_storage_cropstring_wrap';

function GetSimulation_Storage_Season() : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsimulation_storage_season';

procedure SetSimulation_Storage(constref Simulation_Storage : rep_storage);

procedure SetSimulation_Storage_Btotal(constref Btotal : double);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_storage_btotal';

procedure SetSimulation_Storage_CropString(constref CropString : string);

procedure SetSimulation_Storage_CropString_wrap(
    constref p : PChar;
    constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_setsimulation_storage_cropstring_wrap';

procedure SetSimulation_Storage_Season(constref Season : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsimulation_storage_season';

procedure LoadClimate(constref FullName : string;
                      var ClimateDescription : string;
                      var TempFile,EToFile,RainFile,CO2File: string);

procedure LoadClimate_wrap(constref FullName : PChar;
                           constref strlen1 : integer;  
                           var ClimateDescription : PChar;
                           constref strlen2 : integer;
                           var TempFile : PChar;
                           constref strlen3 : integer;
                           var EToFile : PChar;
                           constref strlen4 : integer;
                           var RainFile : PChar;
                           constref strlen5 : integer;
                           var CO2File : PChar;
                           constref strlen6 : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_loadclimate_wrap';

procedure LoadCropCalendar(
            constref FullName : string;
            var GetOnset,GetOnsetTemp : boolean;
            var DayNrStart : integer;
            constref YearStart : integer);

procedure LoadCropCalendar_wrap(constref FullName : PChar;
                                constref strlen : integer;
                                var GetOnset,GetOnsetTemp : boolean;
                                var DayNrStart : integer;
                                constref YearStart : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_loadcropcalendar_wrap';

function GetIrriAfterSeason_i(constref i : integer) : Rep_DayEventInt;

function GetIrriAfterSeason() : rep_IrriOutSeasonEvents;

procedure SetIrriAfterSeason_i(constref i : integer;
                           constref IrriAfterSeason_i : Rep_DayEventInt);

procedure SetIrriAfterSeason(constref IrriAfterSeason : rep_IrriOutSeasonEvents);

function GetIrriAfterSeason_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_global_MOD_getirriafterseason_daynr';

function GetIrriAfterSeason_Param(constref i : integer) : integer;
    external 'aquacrop' name '__ac_global_MOD_getirriafterseason_param';

procedure SetIrriAfterSeason_DayNr(constref i : integer;
                                   constref DayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setirriafterseason_daynr';

procedure SetIrriAfterSeason_Param(constref i : integer;
                                   constref Param : integer);
    external 'aquacrop' name '__ac_global_MOD_setirriafterseason_param';

function GetIrriFirstDayNr() : integer;
    external 'aquacrop' name '__ac_global_MOD_getirrifirstdaynr';

procedure SetIrriFirstDayNr(constref IrriFirstDayNr : LongInt);
    external 'aquacrop' name '__ac_global_MOD_setirrifirstdaynr';

function GetIrriBeforeSeason_i(constref i : integer) : Rep_DayEventInt;

function GetIrriBeforeSeason() : rep_IrriOutSeasonEvents;

procedure SetIrriBeforeSeason_i(constref i : integer;
                           constref IrriBeforeSeason_i : Rep_DayEventInt);

procedure SetIrriBeforeSeason(constref IrriBeforeSeason : rep_IrriOutSeasonEvents);

function GetIrriBeforeSeason_DayNr(constref i : integer) : integer;
    external 'aquacrop' name '__ac_global_MOD_getirribeforeseason_daynr';

function GetIrriBeforeSeason_Param(constref i : integer) : integer;
    external 'aquacrop' name '__ac_global_MOD_getirribeforeseason_param';

procedure SetIrriBeforeSeason_DayNr(constref i : integer;
                                   constref DayNr : integer);
    external 'aquacrop' name '__ac_global_MOD_setirribeforeseason_daynr';

procedure SetIrriBeforeSeason_Param(constref i : integer;
                                   constref Param : integer);
    external 'aquacrop' name '__ac_global_MOD_setirribeforeseason_param';

function GetCompartment_i(constref i : integer) : CompartmentIndividual;

function GetCompartment() : rep_Comp;

procedure SetCompartment_i(constref i : integer;
                           constref Compartment_i : CompartmentIndividual);

procedure SetCompartment(constref Compartment : rep_Comp);

function GetCompartment_Thickness(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_thickness';

function GetCompartment_theta(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_theta';

function GetCompartment_fluxout(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_fluxout';

function GetCompartment_Layer(constref i : integer) : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_layer';

function GetCompartment_Smax(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_smax';

function GetCompartment_FCadj(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_fcadj';

function GetCompartment_DayAnaero(constref i : integer) : INTEGER;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_dayanaero';

function GetCompartment_WFactor(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_wfactor';

function GetCompartment_Salt(constref i1, i2 : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_salt';

function GetCompartment_Depo(constref i1, i2 : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getcompartment_depo';

procedure SetCompartment_Thickness(constref i : integer;
                                   constref Thickness : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_thickness';

procedure SetCompartment_theta(constref i : integer;
                               constref theta : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_theta';

procedure SetCompartment_fluxout(constref i : integer;
                                 constref fluxout : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_fluxout';

procedure SetCompartment_Layer(constref i : integer;
                               constref Layer : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_layer';

procedure SetCompartment_Smax(constref i : integer;
                              constref Smax : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_smax';

procedure SetCompartment_FCadj(constref i : integer;
                               constref FCadj : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_fcadj';

procedure SetCompartment_DayAnaero(constref i : integer;
                                   constref DayAnaero : INTEGER);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_dayanaero';

procedure SetCompartment_WFactor(constref i : integer;
                                 constref WFactor : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_wfactor';

procedure SetCompartment_Salt(constref i1, i2 : integer;
                                constref Salt : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_salt';

procedure SetCompartment_Depo(constref i1, i2 : integer;
                                constref Depo : double);
    external 'aquacrop' name '__ac_global_MOD_setcompartment_depo';

function GetSoilLayer() : rep_SoilLayer;

function GetSoilLayer_i(constref i : integer) : SoilLayerIndividual;

function GetSoilLayer_Description(constref i : integer) : string;

function GetSoilLayer_Description_wrap(constref i : integer) : PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_getsoillayer_description_wrap';

function GetSoilLayer_Thickness(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_thickness';

function GetSoilLayer_SAT(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_sat';

function GetSoilLayer_FC(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_fc';

function GetSoilLayer_WP(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_wp';

function GetSoilLayer_tau(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_tau';

function GetSoilLayer_InfRate(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_infrate';

function GetSoilLayer_Penetrability(constref i : integer) : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_penetrability';

function GetSoilLayer_GravelMass(constref i : integer) : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_gravelmass';

function GetSoilLayer_GravelVol(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_gravelvol';

function GetSoilLayer_WaterContent(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_watercontent';

function GetSoilLayer_Macro(constref i : integer) : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_macro';

function GetSoilLayer_SaltMobility(constref i : integer) : rep_salt;

function GetSoilLayer_SaltMobility_i(constref i1, i2 : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_saltmobility_i';

function GetSoilLayer_SC(constref i : integer) : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_sc';

function GetSoilLayer_SCP1(constref i : integer) : ShortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_scp1';

function GetSoilLayer_UL(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_ul';

function GetSoilLayer_Dx(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_dx';

function GetSoilLayer_SoilClass(constref i : integer) : shortInt;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_soilclass';

function GetSoilLayer_CRa(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_cra';

function GetSoilLayer_CRb(constref i : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_getsoillayer_crb';

procedure SetSoilLayer(constref SoilLayer_in : rep_SoilLayer);

procedure SetSoilLayer_i(constref i : integer;
                         constref SoilLayer_i : SoilLayerIndividual);

procedure SetSoilLayer_Description(constref i : integer;
                         constref Description : string);

procedure SetSoilLayer_Description_wrap(constref i : integer;
                         constref p : PChar);
    external 'aquacrop' name '__ac_interface_global_MOD_setsoillayer_description_wrap';

procedure SetSoilLayer_Thickness(constref i : integer;
                         constref Thickness : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_thickness';

procedure SetSoilLayer_SAT(constref i : integer;
                         constref SAT : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_sat';

procedure SetSoilLayer_FC(constref i : integer;
                         constref FC : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_fc';

procedure SetSoilLayer_WP(constref i : integer;
                         constref WP : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_wp';

procedure SetSoilLayer_tau(constref i : integer;
                         constref tau : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_tau';

procedure SetSoilLayer_InfRate(constref i : integer;
                         constref InfRate : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_infrate';

procedure SetSoilLayer_Penetrability(constref i : integer;
                         constref Penetrability : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_penetrability';

procedure SetSoilLayer_GravelMass(constref i : integer;
                         constref GravelMass : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_gravelmass';

procedure SetSoilLayer_GravelVol(constref i : integer;
                         constref GravelVol : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_gravelvol';

procedure SetSoilLayer_WaterContent(constref i : integer;
                         constref WaterContent : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_watercontent';

procedure SetSoilLayer_Macro(constref i : integer;
                         constref Macro : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_macro';

procedure SetSoilLayer_SaltMobility(constref i : integer;
                                    constref SaltMobility : rep_salt);

procedure SetSoilLayer_SaltMobility_i(constref i1, i2 : integer;
                                    constref SaltMobility_i : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_saltmobility_i';

procedure SetSoilLayer_SC(constref i : integer;
                         constref SC : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_sc';

procedure SetSoilLayer_SCP1(constref i : integer;
                         constref SCP1 : ShortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_scp1';

procedure SetSoilLayer_UL(constref i : integer;
                         constref UL : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_ul';

procedure SetSoilLayer_Dx(constref i : integer;
                         constref Dx : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_dx';

procedure SetSoilLayer_SoilClass(constref i : integer;
                         constref SoilClass : shortInt);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_soilclass';

procedure SetSoilLayer_CRa(constref i : integer;
                         constref CRa : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_cra';

procedure SetSoilLayer_CRb(constref i : integer;
                         constref CRb : double);
    external 'aquacrop' name '__ac_global_MOD_setsoillayer_crb';

function ECeComp(constref Comp : CompartmentIndividual) : double;
    external 'aquacrop' name '__ac_global_MOD_ececomp';

function ECswComp(
            constref Comp : CompartmentIndividual;
            constref atFC : boolean) : double;
    external 'aquacrop' name '__ac_interface_global_MOD_ecswcomp_wrap';

procedure NoManagement;
    external 'aquacrop' name '__ac_global_MOD_nomanagement';

function GetManDescription(): string;

function GetManDescription_wrap(): PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_getmandescription_wrap';

procedure SetManDescription(constref str : string);

procedure SetManDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_setmandescription_wrap';  

procedure LoadManagement(constref FullName : string);

procedure LoadManagement_wrap(constref FullName : PChar;
                         constref strlen : integer);
   external 'aquacrop' name '__ac_interface_global_MOD_loadmanagement_wrap';

function __ActualRootingDepth(
                constref DAP,L0,LZmax,L1234,GDDL0,GDDLZmax : integer;
                constref SumGDD,Zmin,Zmax : double;
                constref ShapeFactor : ShortInt;
                constref int_TypeDays : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_actualrootingdepth';

function ActualRootingDepth(
                constref DAP,L0,LZmax,L1234,GDDL0,GDDLZmax : integer;
                constref SumGDD,Zmin,Zmax : double;
                constref ShapeFactor : ShortInt;
                constref TypeDays : rep_modeCycle) : double;

procedure SaveCrop(constref totalname : string);

procedure SaveCrop_wrap(constref totalname : PChar;
                        constref strlen : integer);
   external 'aquacrop' name '__ac_interface_global_MOD_savecrop_wrap';

procedure SaveProfile(constref totalname : string);

procedure SaveProfile_wrap(constref totalname : PChar;
                           constref strlen : integer);
   external 'aquacrop' name '__ac_interface_global_MOD_saveprofile_wrap';

procedure DetermineParametersCR(constref SoilClass : ShortInt;
                                constref KsatMM : double;
                                var aParam, bParam : double);
    external 'aquacrop' name '__ac_global_MOD_determineparameterscr';

function __CanopyCoverNoStressSF(
                constref DAP,L0,L123,LMaturity,GDDL0,GDDL123,GDDLMaturity : INTEGER;
                constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
                constref int_TypeDays : ShortInt;
                constref SFRedCGC,SFRedCCx : ShortInt) : double;
     external 'aquacrop' name '__ac_global_MOD_canopycovernostresssf';

function CanopyCoverNoStressSF(
                constref DAP,L0,L123,LMaturity,GDDL0,GDDL123,GDDLMaturity : INTEGER;
                constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
                constref TypeDays : rep_modeCycle;
                constref SFRedCGC,SFRedCCx : ShortInt) : double;

function __CCiNoWaterStressSF(
                constref Dayi,L0,L12SF,L123,L1234 : integer;
                constref GDDL0,GDDL12SF,GDDL123,GDDL1234  : integer;
                constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
                constref SFRedCGC,SFRedCCx : ShortInt;
                constref SFCDecline : Double;
                constref TheModeCycle : ShortInt) : double;
     external 'aquacrop' name '__ac_global_MOD_ccinowaterstresssf';

function CCiNoWaterStressSF(Dayi,L0,L12SF,L123,L1234,
                            GDDL0,GDDL12SF,GDDL123,GDDL1234  : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
                            SFRedCGC,SFRedCCx : ShortInt;
                            SFCDecline : Double;
                            TheModeCycle : rep_modeCycle) : double;


procedure DetermineNrandThicknessCompartments();
    external 'aquacrop' name '__ac_global_MOD_determinenrandthicknesscompartments';

function GetNrCompartments() : integer;
    external 'aquacrop' name '__ac_global_MOD_getnrcompartments';

procedure SetNrCompartments(constref NrCompartments_in : integer);
    external 'aquacrop' name '__ac_global_MOD_setnrcompartments';

function GetDrain() : double;
    external 'aquacrop' name '__ac_global_MOD_getdrain';

procedure SetDrain(constref Drain_in : double);
    external 'aquacrop' name '__ac_global_MOD_setdrain';

function GetRain() : double;
    external 'aquacrop' name '__ac_global_MOD_getrain';

procedure SetRain(constref Rain_in : double);
    external 'aquacrop' name '__ac_global_MOD_setrain';

function GetRunoff() : double;
    external 'aquacrop' name '__ac_global_MOD_getrunoff';

procedure SetRunoff(constref Runoff_in : double);
    external 'aquacrop' name '__ac_global_MOD_setrunoff';

function GetSurfaceStorage() : double;
    external 'aquacrop' name '__ac_global_MOD_getsurfacestorage';

procedure SetSurfaceStorage(constref SurfaceStorage_in : double);
    external 'aquacrop' name '__ac_global_MOD_setsurfacestorage';

function GetECstorage() : double;
    external 'aquacrop' name '__ac_global_MOD_getecstorage';

procedure SetECstorage(constref ECstorage_in : double);
    external 'aquacrop' name '__ac_global_MOD_setecstorage';

procedure CalculateAdjustedFC(constref DepthAquifer : double;
                              var CompartAdj   : rep_Comp);
    external 'aquacrop' name '__ac_global_MOD_calculateadjustedfc';

procedure AdjustOnsetSearchPeriod;
    external 'aquacrop' name '__ac_global_MOD_adjustonsetsearchperiod';

function ActiveCells(constref Comp : CompartmentIndividual) : integer;
    external 'aquacrop' name '__ac_global_MOD_activecells';

procedure DetermineSaltContent(constref ECe : double;
                               var Comp : CompartmentIndividual);
    external 'aquacrop' name '__ac_global_MOD_determinesaltcontent';

procedure SetClimData;
    external 'aquacrop' name '__ac_global_MOD_setclimdata';

function DayString(
            constref DNr : LongInt) : repstring17;
    external 'aquacrop' name '__ac_global_MOD_daystring';

procedure AdjustYearPerennials_wrap(
            constref TheYearSeason: ShortInt;
            constref Sown1stYear : BOOLEAN;
            constref int_cyclemode : integer;
            constref Zmax,ZminYear1,TheCCo,TheSizeSeedling, TheCGC,TheCCx,TheGDDCGC : double;
            constref ThePlantingDens : LongInt;
            VAR int_plant : integer;
            VAR Zmin,TheSizePlant,TheCCini : double;
            VAR TheDaysToCCini,TheGDDaysToCCini : INTEGER);
     external 'aquacrop' name '__ac_global_MOD_adjustyearperennials';

procedure AdjustYearPerennials(
            constref TheYearSeason: ShortInt;
            constref Sown1stYear : BOOLEAN;
            constref TheCycleMode : rep_modeCycle;
            constref Zmax,ZminYear1,TheCCo,TheSizeSeedling,TheCGC,TheCCx,TheGDDCGC : double;
            constref ThePlantingDens : LongInt;
            VAR TypeOfPlanting : rep_Planting;
            VAR Zmin,TheSizePlant,TheCCini : double;
            VAR TheDaysToCCini,TheGDDaysToCCini : INTEGER);

procedure NoCropCalendar;
    external 'aquacrop' name '__ac_global_MOD_nocropcalendar';

procedure LoadCrop(constref FullName : string);

procedure LoadCrop_wrap(constref FullName : PChar;
                        constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_loadcrop_wrap';

procedure CalculateETpot(constref DAP,L0,L12,L123,LHarvest,DayLastCut : integer;
                         constref CCi,EToVal,KcVal,KcDeclineVal,CCx : double;
                         constref CCxWithered,CCeffectProcent,CO2i : double;
                         constref GDDayi, TempGDtranspLow : double;
                         var TpotVal, EpotVal : double);
    external 'aquacrop' name '__ac_global_MOD_calculateetpot';

procedure ResetSWCToFC();
    external 'aquacrop' name '__ac_global_MOD_resetswctofc';

function GetZiAqua() : integer;
    external 'aquacrop' name '__ac_global_MOD_getziaqua';

procedure SetZiAqua(constref ZiAqua_in : integer);
    external 'aquacrop' name '__ac_global_MOD_setziaqua';

function SeasonalSumOfKcPot(constref TheDaysToCCini,TheGDDaysToCCini : integer;
                            constref L0,L12,L123,L1234,GDDL0,GDDL12 : integer;
                            constref GDDL123,GDDL1234 : integer;
                            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop : double;
                            constref KcDeclAgeing,CCeffectProcent : double;
                            constref Tbase,Tupper,TDayMin,TDayMax : double;
                            constref GDtranspLow,CO2i : double;
                            constref TheModeCycle : rep_modeCycle) : double;

function __SeasonalSumOfKcPot(constref TheDaysToCCini,TheGDDaysToCCini : integer;
                            constref L0,L12,L123,L1234,GDDL0,GDDL12 : integer;
                            constref GDDL123,GDDL1234 : integer;
                            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop : double;
                            constref KcDeclAgeing,CCeffectProcent : double;
                            constref Tbase,Tupper,TDayMin,TDayMax : double;
                            constref GDtranspLow,CO2i : double;
                            constref TheModeCycle : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_seasonalsumofkcpot';

function HarvestIndexDay(constref DAP  : LongInt;
                         constref DaysToFlower,HImax : integer;
                         constref dHIdt,CCi,CCxadjusted : double;
                         constref PercCCxHIfinal        : ShortInt;
                         constref TempPlanting : rep_Planting;
                         var PercentLagPhase : ShortInt;
                         var HIfinal : INTEGER)   : double;

function __HarvestIndexDay(constref DAP  : LongInt;
                         constref DaysToFlower,HImax : integer;
                         constref dHIdt,CCi,CCxadjusted : double;
                         constref PercCCxHIfinal        : ShortInt;
                         constref int_planting : integer;
                         var PercentLagPhase : ShortInt;
                         var HIfinal : integer)   : double;
    external 'aquacrop' name '__ac_global_MOD_harvestindexday';

procedure NoManagementOffSeason;
    external 'aquacrop' name '__ac_global_MOD_nomanagementoffseason';

function GetOffSeasonDescription(): string;

function GetOffSeasonDescription_wrap(): PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_getoffseasondescription_wrap';

procedure SetOffSeasonDescription(constref str : string);

procedure SetOffSeasonDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_setoffseasondescription_wrap';

procedure LoadOffSeason(constref FullName : string);

procedure LoadOffSeason_wrap(constref FullName : PChar;
                             constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_loadoffseason_wrap';

procedure LoadProgramParametersProject(constref FullFileNameProgramParameters : string);

procedure LoadProgramParametersProject_wrap(constref FullFileNameProgramParameters : PChar;
                                            constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_loadprogramparametersproject_wrap';

procedure ReadCropSettingsParameters();
    external 'aquacrop' name '__ac_global_MOD_readcropsettingparameters';

procedure ReadFieldSettingsParameters();
    external 'aquacrop' name '__ac_global_MOD_readfieldsettingparameters';

procedure ReadTemperatureSettingsParameters();
    external 'aquacrop' name '__ac_global_MOD_readtemperaturesettingparameters';

procedure CompleteCropDescription;
    external 'aquacrop' name '__ac_global_MOD_completecropdescription';

procedure CompleteClimateDescription(var ClimateRecord : rep_clim);

procedure CompleteClimateDescription_wrap(
                            var DataType : integer;
                            var FromD, FromM, FromY : integer;
                            var ToD, ToM, ToY : integer;
                            var FromDayNr, ToDayNr : integer;
                            var FromString : PChar;
                            constref strlen1 : integer;
                            var ToString : PChar;
                            constref strlen2 : integer;
                            var NrObs : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_completeclimatedescription_wrap';



procedure DesignateSoilLayerToCompartments(constref NrCompartments : integer;
                                           constref NrSoilLayers : integer;
                                           var Compartment : rep_Comp);
    external 'aquacrop' name '__ac_global_MOD_designatesoillayertocompartments';


procedure specify_soil_layer(constref NrCompartments,NrSoilLayers : integer;
                             var SoilLayer : rep_SoilLayer;
                             var Compartment : rep_Comp;
                             var TotalWaterContent : rep_Content);
    external 'aquacrop' name '__ac_global_MOD_specify_soil_layer';


procedure Calculate_Saltmobility(constref layer : integer;
                                 constref SaltDiffusion : shortint;  // percentage
                                 constref Macro : shortint;
                                 var Mobil : rep_salt);
    external 'aquacrop' name '__ac_global_MOD_calculate_saltmobility';


procedure CompleteProfileDescription();
    external 'aquacrop' name '__ac_global_MOD_completeprofiledescription';


procedure LoadProfile(FullName : string);

procedure LoadProfile_wrap(constref FullName : PChar;
                           constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_loadprofile_wrap';


procedure DetermineRootZoneWC(
            constref RootingDepth : double;
            VAR ZtopSWCconsidered : boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_determinerootzonewc_wrap';

procedure LoadClim (
            constref FullName : string;
            VAR ClimateDescription : string;
            VAR ClimateRecord : rep_clim);

procedure LoadClim_wrap(
            constref FullName : PChar;
            constref strlen1 : integer;
            var ClimateDescription : PChar;
            constref strlen2 : integer;
            var DataType : integer;
            var FromD, FromM, FromY : integer;
            var ToD, ToM, ToY : integer;
            var FromDayNr, ToDayNr : integer;
            var FromString : PChar;
            constref strlen3 : integer;
            var ToString : PChar;
            constref strlen4 : integer;
            var NrObs : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_loadclim_wrap';

procedure AdjustClimRecordTo(CDayN : longint);
    external 'aquacrop' name '__ac_global_MOD_adjustclimrecordto';

procedure TranslateIniLayersToSWProfile(constref NrLay : ShortInt;
                                        constref LayThickness,LayVolPr,LayECdS : rep_IniComp;
                                        constref NrComp : INTEGER;
                                        VAR Comp : rep_Comp);
    external 'aquacrop' name '__ac_global_MOD_translateinilayerstoswprofile';

procedure TranslateIniPointsToSWProfile(
                                constref NrLoc : ShortInt;
                                constref LocDepth,LocVolPr,LocECdS : rep_IniComp;
                                constref NrComp : integer;
                                VAR Comp : rep_Comp);
    external 'aquacrop' name '__ac_global_MOD_translateinipointstoswprofile';



function CCiniTotalFromTimeToCCini(
                        constref TempDaysToCCini,TempGDDaysToCCini : integer;
                        constref L0,L12,L12SF,L123,L1234,GDDL0,GDDL12 : integer;
                        constref GDDL12SF,GDDL123,GDDL1234 : integer;
                        constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,RatDGDD : double;
                        constref SFRedCGC,SFRedCCx : ShortInt;
                        constref SFCDecline,fWeed : Double;
                        constref TheModeCycle : rep_modeCycle) : double;


function __CCiniTotalFromTimeToCCini(
                        constref TempDaysToCCini,TempGDDaysToCCini : integer;
                        constref L0,L12,L12SF,L123,L1234,GDDL0,GDDL12 : integer;
                        constref GDDL12SF,GDDL123,GDDL1234 : integer;
                        constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,RatDGDD : double;
                        constref SFRedCGC,SFRedCCx : ShortInt;
                        constref SFCDecline,fWeed : Double;
                        constref TheModeCycle : integer) : double;
    external 'aquacrop' name '__ac_global_MOD_ccinitotalfromtimetoccini';


procedure AdjustCropYearToClimFile(VAR CDay1,CDayN : longint);
    external 'aquacrop' name '__ac_global_MOD_adjustcropyeartoclimfile';


function EndGrowingPeriod(constref Day1 : longint;
                          VAR DayN : longint) : string;

function EndGrowingPeriod_wrap(
                          constref Day1 : longint;
                          VAR DayN : longint) : PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_endgrowingperiod_wrap';

procedure LoadInitialConditions(constref SWCiniFileFull : string;
                                VAR IniSurfaceStorage : double);

procedure LoadInitialConditions_wrap(constref SWCiniFileFull : PChar;
                                constref strlen : integer;
                                VAR IniSurfaceStorage : double);
    external 'aquacrop' name '__ac_interface_global_MOD_loadinitialconditions_wrap';

procedure LoadGroundWater(
                    constref FullName : string;
                    constref AtDayNr : LongInt;
                    VAR Zcm : INTEGER;
                    VAR ECdSm : double);

procedure LoadGroundwater_wrap(
                    constref FullName_ptr : PChar;
                    constref strlen : integer;
                    constref AtDayNr : LongInt;
                    VAR Zcm : INTEGER;
                    VAR ECdSm : double);
    external 'aquacrop' name '__ac_interface_global_MOD_loadgroundwater_wrap';

function GetGroundwaterDescription(): string;

function GetGroundwaterDescription_wrap(): PChar;
    external 'aquacrop' name '__ac_interface_global_MOD_getgroundwaterdescription_wrap';

procedure SetGroundwaterDescription(constref str : string);

procedure SetGroundwaterDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
    external 'aquacrop' name '__ac_interface_global_MOD_setgroundwaterdescription_wrap';

procedure AdjustSizeCompartments(constref CropZx : double);
    external 'aquacrop' name '__ac_global_MOD_adjustsizecompartments';


procedure AdjustThetaInitial(
                constref PrevNrComp : ShortInt;
                constref PrevThickComp,PrevVolPrComp,PrevECdSComp : rep_IniComp);
    external 'aquacrop' name '__ac_global_MOD_adjustthetainitial';

function GetDaySubmerged() : integer;
    external 'aquacrop' name '__ac_global_MOD_getdaysubmerged';

procedure SetDaySubmerged(constref DaySubmerged_in : integer);
    external 'aquacrop' name '__ac_global_MOD_setdaysubmerged';

function GetCRsalt() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrsalt';

procedure SetCRsalt(constref CRsalt_in : double);
    external 'aquacrop' name '__ac_global_MOD_setcrsalt';

function GetMaxPlotNew() : integer;
    external 'aquacrop' name '__ac_global_MOD_getmaxplotnew';

procedure SetMaxPlotNew(constref MaxPlotNew_in : integer);
    external 'aquacrop' name '__ac_global_MOD_setmaxplotnew';

function GetMaxPlotTr() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getmaxplottr';

procedure SetMaxPlotTr(constref MaxPlotTr_in : shortint);
    external 'aquacrop' name '__ac_global_MOD_setmaxplottr';

function GetEvapoEntireSoilSurface() : boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getevapoentiresoilsurface_wrap';

procedure SetEvapoEntireSoilSurface(constref EvapoEntireSoilSurface_in : boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setevapoentiresoilsurface_wrap';

function GetPreDay() : boolean;
    external 'aquacrop' name '__ac_interface_global_MOD_getpreday_wrap';

procedure SetPreDay(constref PreDay_in : boolean);
    external 'aquacrop' name '__ac_interface_global_MOD_setpreday_wrap';

function GetIniPercTAW() : shortint;
    external 'aquacrop' name '__ac_global_MOD_getiniperctaw';

procedure SetIniPercTAW(constref IniPercTAW_in : shortint);
    external 'aquacrop' name '__ac_global_MOD_setiniperctaw';

function GetECiAqua() : double;
    external 'aquacrop' name '__ac_global_MOD_geteciaqua';

procedure SetECiAqua(constref ECiAqua_in : double);
    external 'aquacrop' name '__ac_global_MOD_seteciaqua';

function GetEact() : double;
    external 'aquacrop' name '__ac_global_MOD_geteact';

procedure SetEact(constref Eact_in : double);
    external 'aquacrop' name '__ac_global_MOD_seteact';

function GetETo() : double;
    external 'aquacrop' name '__ac_global_MOD_geteto';

procedure SetETo(constref ETo_in : double);
    external 'aquacrop' name '__ac_global_MOD_seteto';

function GetIrrigation() : double;
    external 'aquacrop' name '__ac_global_MOD_getirrigation';

procedure SetIrrigation(constref Irrigation_in : double);
    external 'aquacrop' name '__ac_global_MOD_setirrigation';

function GetInfiltrated() : double;
    external 'aquacrop' name '__ac_global_MOD_getinfiltrated';

procedure SetInfiltrated(constref Infiltrated_in : double);
    external 'aquacrop' name '__ac_global_MOD_setinfiltrated';

function GetCRwater() : double;
    external 'aquacrop' name '__ac_global_MOD_getcrwater';

procedure SetCRwater(constref CRwater_in : double);
    external 'aquacrop' name '__ac_global_MOD_setcrwater';

function GetEpot() : double;
    external 'aquacrop' name '__ac_global_MOD_getepot';

procedure SetEpot(constref Epot_in : double);
    external 'aquacrop' name '__ac_global_MOD_setepot';

function GetTpot() : double;
    external 'aquacrop' name '__ac_global_MOD_gettpot';

procedure SetTpot(constref Tpot_in : double);
    external 'aquacrop' name '__ac_global_MOD_settpot';

function GetTact() : double;
    external 'aquacrop' name '__ac_global_MOD_gettact';

procedure SetTact(constref Tpot_in : double);
    external 'aquacrop' name '__ac_global_MOD_settact';

procedure DetermineLinkedSimDay1(
            constref CropDay1 : LongInt;
            VAR SimDay1 :LongInt);
    external 'aquacrop' name '__ac_global_MOD_determinelinkedsimday1';

procedure AdjustSimPeriod;
    external 'aquacrop' name '__ac_global_MOD_adjustsimperiod';

function GetMultipleProjectDescription(): string;

function GetMultipleProjectDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getmultipleprojectdescription_wrap';

procedure SetMultipleProjectDescription(constref str : string);

procedure SetMultipleProjectDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setmultipleprojectdescription_wrap';

function GetProjectDescription(): string;

function GetProjectDescription_wrap(): PChar;
        external 'aquacrop' name '__ac_interface_global_MOD_getprojectdescription_wrap';

procedure SetProjectDescription(constref str : string);

procedure SetProjectDescription_wrap(
            constref p : PChar;
            constref strlen : integer);
        external 'aquacrop' name '__ac_interface_global_MOD_setprojectdescription_wrap';

function GetRootingDepth() : double;
    external 'aquacrop' name '__ac_global_MOD_getrootingdepth';

procedure SetRootingDepth(constref RootingDepth_in : double);
    external 'aquacrop' name '__ac_global_MOD_setrootingdepth';

function GetECdrain() : double;
    external 'aquacrop' name '__ac_global_MOD_getecdrain';

procedure SetECdrain(constref ECdrain_in : double);
    external 'aquacrop' name '__ac_global_MOD_setecdrain';

function GetSaltInfiltr() : double;
    external 'aquacrop' name '__ac_global_MOD_getsaltinfiltr';

procedure SetSaltInfiltr(constref SaltInfiltr_in : double);
    external 'aquacrop' name '__ac_global_MOD_setsaltinfiltr';


implementation



procedure LoadGroundWater(
                    constref FullName : string;
                    constref AtDayNr : LongInt;
                    VAR Zcm : INTEGER;
                    VAR ECdSm : double);
var
    FullName_ptr : PChar;
    strlen : integer;
begin
    FullName_ptr := PChar(FullName);
    strlen := Length(FullName);
    LoadGroundwater_wrap(Fullname_ptr, strlen, AtDayNr, Zcm, ECdSm);
end;


procedure LoadInitialConditions(constref SWCiniFileFull : string;
                                VAR IniSurfaceStorage : double);
var
    p : PChar;
    strlen : integer;
begin
    p := PChar(SWCiniFileFull);
    strlen := Length(SWCiniFileFull);
    LoadInitialConditions_wrap(p, strlen, IniSurfaceStorage);
end;



function EndGrowingPeriod(constref Day1 : longint;
                          VAR DayN : longint) : string;
var
    p : PChar;
begin
    p := EndGrowingperiod_wrap(Day1, DayN);
    EndGrowingPeriod := AnsiString(p);
end;
    


function CCiniTotalFromTimeToCCini(
                        constref TempDaysToCCini,TempGDDaysToCCini : integer;
                        constref L0,L12,L12SF,L123,L1234,GDDL0,GDDL12 : integer;
                        constref GDDL12SF,GDDL123,GDDL1234 : integer;
                        constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,RatDGDD : double;
                        constref SFRedCGC,SFRedCCx : ShortInt;
                        constref SFCDecline,fWeed : Double;
                        constref TheModeCycle : rep_modeCycle) : double;
var
    int_modeCycle : integer;
begin
    int_modeCycle := ord(TheModeCycle);
    CCiniTotalFromTimeToCCini := __CCiniTotalFromTimeToCCini(TempDaysToCCini, TempGDDaysToCCini,
                                            L0, L12, L12SF, L123, L1234, GDDL0,
                                            GDDL12, GDDL12SF, GDDL123,
                                            GDDL1234, CCo, CCx, CGC, GDDCGC,
                                            CDC, GDDCDC, RatDGDD, SFRedCGC,
                                            SFRedCCx, SFCDecline, fWeed,
                                            int_modecycle)
end;



function SeasonalSumOfKcPot(constref TheDaysToCCini,TheGDDaysToCCini : integer;
                            constref L0,L12,L123,L1234,GDDL0,GDDL12 : integer;
                            constref GDDL123,GDDL1234 : integer;
                            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop : double;
                            constref KcDeclAgeing,CCeffectProcent : double;
                            constref Tbase,Tupper,TDayMin,TDayMax : double;
                            constref GDtranspLow,CO2i : double;
                            constref TheModeCycle : rep_modeCycle) : double;
var
    int_modeCycle : integer;
begin
    int_modeCycle := ord(TheModeCycle);
    SeasonalSumOfKcPot := __SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini,
                                               L0, L12, L123, L1234, GDDL0,
                                               GDDL12, GDDL123, GDDL1234, CCo,
                                               CCx, CGC, GDDCGC, CDC, GDDCDC,
                                               KcTop, KcDeclAgeing, CCeffectProcent,
                                               Tbase, Tupper, TDayMin, TDayMax, 
                                               GDtranspLow, CO2i, int_modeCycle);
end;


function HarvestIndexDay(constref DAP  : LongInt;
                         constref DaysToFlower,HImax : integer;
                         constref dHIdt,CCi,CCxadjusted : double;
                         constref PercCCxHIfinal        : ShortInt;
                         constref TempPlanting : rep_Planting;
                         var PercentLagPhase : ShortInt;
                         var HIfinal : INTEGER)   : double;
var
    int_planting : integer;
begin
    int_planting := ord(TempPlanting);
    HarvestIndexDay := __HarvestIndexDay(DAP, DaysToFlower, HImax, dHIdt,
                                         CCi, CCxadjusted, PercCCxHIfinal, 
                                         int_planting, PercentLagPhase,
                                         HIfinal);
end;


function GetWeedRC(
            constref TheDay : integer;
            constref GDDayi : double;
            constref fCCx : double;
            constref TempWeedRCinput : shortint;
            constref TempWeedAdj : shortint;
            var TempWeedDeltaRC : integer;
            constref L12SF : integer;
            constref TempL123 : integer;
            constref GDDL12SF : integer;
            constref TempGDDL123 : integer;
            constref TheModeCycle : rep_modeCycle) : double;
var
    int_modeCycle: integer;

begin
    int_modeCycle := ord(TheModeCycle);
    GetWeedRC := __GetWeedRC(TheDay, GDDayi, fCCx, TempWeedRCinput, TempWeedAdj,
                             TempWeedDeltaRC, L12SF, TempL123, GDDL12SF,
                             TempGDDL123, int_modeCycle);
end;

function TimeToCCini(
            constref ThePlantingType : rep_planting;
            constref TheCropPlantingDens : integer;
            constref TheSizeSeedling : double;
            constref TheSizePlant : double;
            constref TheCropCCx : double;
            constref TheCropCGC : double) : Integer;
VAR 
    int_planting: integer;


begin
    int_planting := ord(ThePlantingType); 
    TimeToCCini := __TimeToCCini(int_planting, TheCropPlantingDens, TheSizeSeedling,
                                 TheSizePlant, TheCropCCx, TheCropCGC);
end;

procedure DetermineLengthGrowthStages(
            constref CCoVal : double;
            constref CCxVal : double;
            constref CDCVal : double;
            constref L0 : integer;
            constref TotalLength : integer;
            constref CGCgiven : boolean;
            constref TheDaysToCCini : integer;
            constref ThePlanting : rep_planting;
            VAR Length123 : integer;
            VAR StLength : rep_int_array;
            VAR Length12 : integer;
            VAR CGCVal : double);

VAR 
    int_planting : integer;

begin
    int_planting := ord(ThePlanting);
    DetermineLengthGrowthStages_wrap(CCoVal,CCxVal,
                                            CDCVal,L0,
                                            TotalLength,
                                            CGCgiven,
                                            TheDaysToCCini,
                                            int_planting,
                                            Length123,
                                            StLength,
                                            Length12,
                                            CGCVal);
end;

function GetManagement_Cuttings_Criterion() : rep_timecuttings;
var
    int_timecuttings : integer;

begin;
    int_timecuttings := __GetManagement_Cuttings_Criterion();
    GetManagement_Cuttings_Criterion := rep_timecuttings(int_timecuttings);
end;

procedure SetManagement_Cuttings_Criterion(constref Criterion : rep_TimeCuttings); 
var
    int_timecuttings : integer;

begin;
    int_timecuttings := ord(Criterion);
    __SetManagement_Cuttings_Criterion(int_timecuttings);
end;

function GetSimulParam_EffectiveRain_Method() : rep_EffectiveRainMethod;
var
    int_EffectiveRainMethod : integer;

begin;
    int_EffectiveRainMethod := __GetSimulParam_EffectiveRain_Method();
    GetSimulParam_EffectiveRain_Method := rep_EffectiveRainMethod(int_EffectiveRainMethod);
end;

procedure SetSimulParam_EffectiveRain_Method(constref Method: rep_EffectiveRainMethod); 
var
    int_EffectiveRainMethod : integer;

begin;
    int_EffectiveRainMethod := ord(Method);
    __SetSimulParam_EffectiveRain_Method(int_EffectiveRainMethod);
end;


function GetGenerateTimeMode() : rep_GenerateTimeMode;
var
    int_GenerateTimeMode : integer;

begin;
    int_GenerateTimeMode := __GetGenerateTimeMode();
    GetGenerateTimeMode := rep_GenerateTimeMode(int_GenerateTimeMode);
end;

procedure SetGenerateTimeMode(constref GenerateTimeMode : rep_GenerateTimeMode); 
var
    int_GenerateTimeMode : integer;

begin;
    int_GenerateTimeMode := ord(GenerateTimeMode);
    __SetGenerateTimeMode(int_GenerateTimeMode);
end;

function GetGenerateDepthMode() : rep_GenerateDepthMode;
var
    int_GenerateDepthMode : integer;

begin;
    int_GenerateDepthMode := __GetGenerateDepthMode();
    GetGenerateDepthMode := rep_GenerateDepthMode(int_GenerateDepthMode);
end;

procedure SetGenerateDepthMode(constref GenerateDepthMode : rep_GenerateDepthMode); 
var
    int_GenerateDepthMode : integer;

begin;
    int_GenerateDepthMode := ord(GenerateDepthMode);
    __SetGenerateDepthMode(int_GenerateDepthMode);
end;

function Geteffectiverain() : rep_EffectiveRain;
begin;
    Geteffectiverain.Method := Geteffectiverain_Method();
    Geteffectiverain.PercentEffRain := Geteffectiverain_PercentEffRain();
    Geteffectiverain.ShowersInDecade := Geteffectiverain_ShowersInDecade();
    Geteffectiverain.RootNrEvap := Geteffectiverain_RootNrEvap();
end;


function Geteffectiverain_Method() : rep_EffectiveRainMethod;
var
    index : shortint;
begin;
    index := __Geteffectiverain_Method();
    Geteffectiverain_Method := rep_EffectiveRainMethod(index);
end;


procedure Seteffectiverain(constref effectiverain : rep_EffectiveRain);
begin;
    Seteffectiverain_Method(effectiverain.Method);
    Seteffectiverain_PercentEffRain(effectiverain.PercentEffRain);
    Seteffectiverain_ShowersInDecade(effectiverain.ShowersInDecade);
    Seteffectiverain_RootNrEvap(effectiverain.RootNrEvap);
end;


procedure Seteffectiverain_Method(constref Method : rep_EffectiveRainMethod);
var
    index : shortint;
begin;
    index := ord(Method);
    __Seteffectiverain_Method(index);
end;


function GetIrriMode() : rep_IrriMode;
var
    int_IrriMode : integer;

begin;
    int_IrriMode := __GetIrriMode();
    GetIrriMode := rep_IrriMode(int_IrriMode);
end;

procedure SetIrriMode(constref IrriMode : rep_IrriMode); 
var
    int_IrriMode : integer;

begin;
    int_IrriMode := ord(IrriMode);
    __SetIrriMode(int_IrriMode);
end;

function GetIrriMethod() : rep_IrriMethod;
var
    int_IrriMethod : integer;

begin;
    int_IrriMethod := __GetIrriMethod();
    GetIrriMethod := rep_IrriMethod(int_IrriMethod);
end;

procedure SetIrriMethod(constref IrriMethod : rep_IrriMethod); 
var
    int_IrriMethod : integer;

begin;
    int_IrriMethod := ord(IrriMethod);
    __SetIrriMethod(int_IrriMethod);
end;

procedure GetNumberSimulationRuns(
            constref TempFileNameFull : string;
            var NrRuns : integer);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(TempFileNameFull);
    strlen := Length(TempFileNameFull);
    GetNumberSimulationRuns_wrap(p, strlen, NrRuns);
end;

function FileExists(constref full_name : string) : boolean;
var 
    p : PChar;
    strlen : integer;
begin;
    p := PChar(full_name);
    strlen := Length(full_name);
    FileExists := FileExists_wrap(p, strlen);
end;

procedure SplitStringInTwoParams(
            constref StringIN : string;
            var Par1,Par2 : double);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(StringIN);
    strlen := Length(StringIN);
    SplitStringInTwoParams_wrap(p, strlen, Par1, Par2);
end;

procedure SplitStringInThreeParams(
            constref StringIN : string;
            var Par1,Par2,Par3 : double);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(StringIN);
    strlen := Length(StringIN);
    SplitStringInThreeParams_wrap(p, strlen, Par1, Par2,Par3);
end;

function GetCrop() : rep_Crop;
begin;
    GetCrop.subkind := GetCrop_subkind();
    GetCrop.ModeCycle := GetCrop_ModeCycle();
    GetCrop.Planting := GetCrop_Planting();
    GetCrop.pMethod := GetCrop_pMethod();
    GetCrop.pdef := GetCrop_pdef();
    GetCrop.pActStom := GetCrop_pActStom();
    GetCrop.KsShapeFactorLeaf := GetCrop_KsShapeFactorLeaf();
    GetCrop.KsShapeFactorStomata := GetCrop_KsShapeFactorStomata();
    GetCrop.KsShapeFactorSenescence := GetCrop_KsShapeFactorSenescence();
    GetCrop.pLeafDefUL := GetCrop_pLeafDefUL();
    GetCrop.pLeafDefLL := GetCrop_pLeafDefLL();
    GetCrop.pLeafAct := GetCrop_pLeafAct();
    GetCrop.pSenescence := GetCrop_pSenescence();
    GetCrop.pSenAct := GetCrop_pSenAct();
    GetCrop.pPollination := GetCrop_pPollination();
    GetCrop.SumEToDelaySenescence := GetCrop_SumEToDelaySenescence();
    GetCrop.AnaeroPoint := GetCrop_AnaeroPoint();
    GetCrop.StressResponse := GetCrop_StressResponse();
    GetCrop.ECemin := GetCrop_ECemin();
    GetCrop.ECemax := GetCrop_ECemax();
    GetCrop.CCsaltDistortion := GetCrop_CCsaltDistortion();
    GetCrop.ResponseECsw := GetCrop_ResponseECsw();
    GetCrop.SmaxTopQuarter := GetCrop_SmaxTopQuarter();
    GetCrop.SmaxBotQuarter := GetCrop_SmaxBotQuarter();
    GetCrop.SmaxTop := GetCrop_SmaxTop();
    GetCrop.SmaxBot := GetCrop_SmaxBot();
    GetCrop.KcTop := GetCrop_KcTop();
    GetCrop.KcDecline := GetCrop_KcDecline();
    GetCrop.CCEffectEvapLate := GetCrop_CCEffectEvapLate();
    GetCrop.Day1 := GetCrop_Day1();
    GetCrop.DayN := GetCrop_DayN();
    GetCrop.Length := GetCrop_Length();
    GetCrop.RootMin := GetCrop_RootMin();
    GetCrop.RootMax := GetCrop_RootMax();
    GetCrop.RootShape := GetCrop_RootShape();
    GetCrop.Tbase := GetCrop_Tbase();
    GetCrop.Tupper := GetCrop_Tupper();
    GetCrop.Tcold := GetCrop_Tcold();
    GetCrop.Theat := GetCrop_Theat();
    GetCrop.GDtranspLow := GetCrop_GDtranspLow();
    GetCrop.SizeSeedling := GetCrop_SizeSeedling();
    GetCrop.SizePlant := GetCrop_SizePlant();
    GetCrop.PlantingDens := GetCrop_PlantingDens();
    GetCrop.CCo := GetCrop_CCo();
    GetCrop.CCini := GetCrop_CCini();
    GetCrop.CGC := GetCrop_CGC();
    GetCrop.GDDCGC := GetCrop_GDDCGC();
    GetCrop.CCx := GetCrop_CCx();
    GetCrop.CDC := GetCrop_CDC();
    GetCrop.GDDCDC := GetCrop_GDDCDC();
    GetCrop.CCxAdjusted := GetCrop_CCxAdjusted();
    GetCrop.CCxWithered := GetCrop_CCxWithered();
    GetCrop.CCoAdjusted := GetCrop_CCoAdjusted();
    GetCrop.DaysToCCini := GetCrop_DaysToCCini();
    GetCrop.DaysToGermination := GetCrop_DaysToGermination();
    GetCrop.DaysToFullCanopy := GetCrop_DaysToFullCanopy();
    GetCrop.DaysToFullCanopySF := GetCrop_DaysToFullCanopySF();
    GetCrop.DaysToFlowering := GetCrop_DaysToFlowering();
    GetCrop.LengthFlowering := GetCrop_LengthFlowering();
    GetCrop.DaysToSenescence := GetCrop_DaysToSenescence();
    GetCrop.DaysToHarvest := GetCrop_DaysToHarvest();
    GetCrop.DaysToMaxRooting := GetCrop_DaysToMaxRooting();
    GetCrop.DaysToHIo := GetCrop_DaysToHIo();
    GetCrop.GDDaysToCCini := GetCrop_GDDaysToCCini();
    GetCrop.GDDaysToGermination := GetCrop_GDDaysToGermination();
    GetCrop.GDDaysToFullCanopy := GetCrop_GDDaysToFullCanopy();
    GetCrop.GDDaysToFullCanopySF := GetCrop_GDDaysToFullCanopySF();
    GetCrop.GDDaysToFlowering := GetCrop_GDDaysToFlowering();
    GetCrop.GDDLengthFlowering := GetCrop_GDDLengthFlowering();
    GetCrop.GDDaysToSenescence := GetCrop_GDDaysToSenescence();
    GetCrop.GDDaysToHarvest := GetCrop_GDDaysToHarvest();
    GetCrop.GDDaysToMaxRooting := GetCrop_GDDaysToMaxRooting();
    GetCrop.GDDaysToHIo := GetCrop_GDDaysToHIo();
    GetCrop.WP := GetCrop_WP();
    GetCrop.WPy := GetCrop_WPy();
    GetCrop.AdaptedToCO2 := GetCrop_AdaptedToCO2();
    GetCrop.HI := GetCrop_HI();
    GetCrop.dHIdt := GetCrop_dHIdt();
    GetCrop.HIincrease := GetCrop_HIincrease();
    GetCrop.aCoeff := GetCrop_aCoeff();
    GetCrop.bCoeff := GetCrop_bCoeff();
    GetCrop.DHImax := GetCrop_DHImax();
    GetCrop.DeterminancyLinked := GetCrop_DeterminancyLinked();
    GetCrop.fExcess := GetCrop_fExcess();
    GetCrop.DryMatter := GetCrop_DryMatter();
    GetCrop.RootMinYear1 := GetCrop_RootMinYear1();
    GetCrop.SownYear1 := GetCrop_SownYear1();
    GetCrop.YearCCx := GetCrop_YearCCx();
    GetCrop.CCxRoot := GetCrop_CCxRoot();
    GetCrop.Assimilates := GetCrop_Assimilates();
end;

function GetCrop_StressResponse() : rep_Shapes;
begin;
    GetCrop_StressResponse.Stress := GetCrop_StressResponse_Stress();
    GetCrop_StressResponse.ShapeCGC := GetCrop_StressResponse_ShapeCGC();
    GetCrop_StressResponse.ShapeCCX := GetCrop_StressResponse_ShapeCCX();
    GetCrop_StressResponse.ShapeWP := GetCrop_StressResponse_ShapeWP();
    GetCrop_StressResponse.ShapeCDecline := GetCrop_StressResponse_ShapeCDecline();
    GetCrop_StressResponse.Calibrated := GetCrop_StressResponse_Calibrated();
end;


procedure SetCrop_StressResponse(constref Crop_StressResponse : rep_Shapes);
begin;
    SetCrop_StressResponse_Stress(Crop_StressResponse.Stress);
    SetCrop_StressResponse_ShapeCGC(Crop_StressResponse.ShapeCGC);
    SetCrop_StressResponse_ShapeCCX(Crop_StressResponse.ShapeCCX);
    SetCrop_StressResponse_ShapeWP(Crop_StressResponse.ShapeWP);
    SetCrop_StressResponse_ShapeCDecline(Crop_StressResponse.ShapeCDecline);
    SetCrop_StressResponse_Calibrated(Crop_StressResponse.Calibrated);
end;

function GetCrop_Length() : rep_int_array;
var
    i : integer;
begin;
    for i := 1 to 4 do GetCrop_Length[i] := GetCrop_Length_i(i)
end;

function GetCrop_subkind() : rep_subkind;
var
    index : shortint;
begin;
    index := __GetCrop_subkind();
    GetCrop_subkind := rep_subkind(index);
end;


function GetCrop_ModeCycle() : rep_modeCycle;
var
    index : shortint;
begin;
    index := __GetCrop_ModeCycle();
    GetCrop_ModeCycle := rep_modeCycle(index);
end;


function GetCrop_Planting() : rep_Planting;
var
    index : shortint;
begin;
    index := __GetCrop_Planting();
    GetCrop_Planting := rep_Planting(index);
end;


function GetCrop_pMethod() : rep_pMethod;
var
    index : shortint;
begin;
    index := __GetCrop_pMethod();
    GetCrop_pMethod := rep_pMethod(index);
end;

procedure SetCrop_Length(constref Length : rep_int_array);
var
    i : integer;
begin;
    for i := 1 to 4 do SetCrop_Length_i(i, Length[i])
end;

procedure SetCrop(constref Crop : rep_Crop);
begin;
    SetCrop_subkind(Crop.subkind);
    SetCrop_ModeCycle(Crop.ModeCycle);
    SetCrop_Planting(Crop.Planting);
    SetCrop_pMethod(Crop.pMethod);
    SetCrop_pdef(Crop.pdef);
    SetCrop_pActStom(Crop.pActStom);
    SetCrop_KsShapeFactorLeaf(Crop.KsShapeFactorLeaf);
    SetCrop_KsShapeFactorStomata(Crop.KsShapeFactorStomata);
    SetCrop_KsShapeFactorSenescence(Crop.KsShapeFactorSenescence);
    SetCrop_pLeafDefUL(Crop.pLeafDefUL);
    SetCrop_pLeafDefLL(Crop.pLeafDefLL);
    SetCrop_pLeafAct(Crop.pLeafAct);
    SetCrop_pSenescence(Crop.pSenescence);
    SetCrop_pSenAct(Crop.pSenAct);
    SetCrop_pPollination(Crop.pPollination);
    SetCrop_SumEToDelaySenescence(Crop.SumEToDelaySenescence);
    SetCrop_AnaeroPoint(Crop.AnaeroPoint);
    SetCrop_StressResponse(Crop.StressResponse);
    SetCrop_ECemin(Crop.ECemin);
    SetCrop_ECemax(Crop.ECemax);
    SetCrop_CCsaltDistortion(Crop.CCsaltDistortion);
    SetCrop_ResponseECsw(Crop.ResponseECsw);
    SetCrop_SmaxTopQuarter(Crop.SmaxTopQuarter);
    SetCrop_SmaxBotQuarter(Crop.SmaxBotQuarter);
    SetCrop_SmaxTop(Crop.SmaxTop);
    SetCrop_SmaxBot(Crop.SmaxBot);
    SetCrop_KcTop(Crop.KcTop);
    SetCrop_KcDecline(Crop.KcDecline);
    SetCrop_CCEffectEvapLate(Crop.CCEffectEvapLate);
    SetCrop_Day1(Crop.Day1);
    SetCrop_DayN(Crop.DayN);
    SetCrop_Length(Crop.Length);
    SetCrop_RootMin(Crop.RootMin);
    SetCrop_RootMax(Crop.RootMax);
    SetCrop_RootShape(Crop.RootShape);
    SetCrop_Tbase(Crop.Tbase);
    SetCrop_Tupper(Crop.Tupper);
    SetCrop_Tcold(Crop.Tcold);
    SetCrop_Theat(Crop.Theat);
    SetCrop_GDtranspLow(Crop.GDtranspLow);
    SetCrop_SizeSeedling(Crop.SizeSeedling);
    SetCrop_SizePlant(Crop.SizePlant);
    SetCrop_PlantingDens(Crop.PlantingDens);
    SetCrop_CCo(Crop.CCo);
    SetCrop_CCini(Crop.CCini);
    SetCrop_CGC(Crop.CGC);
    SetCrop_GDDCGC(Crop.GDDCGC);
    SetCrop_CCx(Crop.CCx);
    SetCrop_CDC(Crop.CDC);
    SetCrop_GDDCDC(Crop.GDDCDC);
    SetCrop_CCxAdjusted(Crop.CCxAdjusted);
    SetCrop_CCxWithered(Crop.CCxWithered);
    SetCrop_CCoAdjusted(Crop.CCoAdjusted);
    SetCrop_DaysToCCini(Crop.DaysToCCini);
    SetCrop_DaysToGermination(Crop.DaysToGermination);
    SetCrop_DaysToFullCanopy(Crop.DaysToFullCanopy);
    SetCrop_DaysToFullCanopySF(Crop.DaysToFullCanopySF);
    SetCrop_DaysToFlowering(Crop.DaysToFlowering);
    SetCrop_LengthFlowering(Crop.LengthFlowering);
    SetCrop_DaysToSenescence(Crop.DaysToSenescence);
    SetCrop_DaysToHarvest(Crop.DaysToHarvest);
    SetCrop_DaysToMaxRooting(Crop.DaysToMaxRooting);
    SetCrop_DaysToHIo(Crop.DaysToHIo);
    SetCrop_GDDaysToCCini(Crop.GDDaysToCCini);
    SetCrop_GDDaysToGermination(Crop.GDDaysToGermination);
    SetCrop_GDDaysToFullCanopy(Crop.GDDaysToFullCanopy);
    SetCrop_GDDaysToFullCanopySF(Crop.GDDaysToFullCanopySF);
    SetCrop_GDDaysToFlowering(Crop.GDDaysToFlowering);
    SetCrop_GDDLengthFlowering(Crop.GDDLengthFlowering);
    SetCrop_GDDaysToSenescence(Crop.GDDaysToSenescence);
    SetCrop_GDDaysToHarvest(Crop.GDDaysToHarvest);
    SetCrop_GDDaysToMaxRooting(Crop.GDDaysToMaxRooting);
    SetCrop_GDDaysToHIo(Crop.GDDaysToHIo);
    SetCrop_WP(Crop.WP);
    SetCrop_WPy(Crop.WPy);
    SetCrop_AdaptedToCO2(Crop.AdaptedToCO2);
    SetCrop_HI(Crop.HI);
    SetCrop_dHIdt(Crop.dHIdt);
    SetCrop_HIincrease(Crop.HIincrease);
    SetCrop_aCoeff(Crop.aCoeff);
    SetCrop_bCoeff(Crop.bCoeff);
    SetCrop_DHImax(Crop.DHImax);
    SetCrop_DeterminancyLinked(Crop.DeterminancyLinked);
    SetCrop_fExcess(Crop.fExcess);
    SetCrop_DryMatter(Crop.DryMatter);
    SetCrop_RootMinYear1(Crop.RootMinYear1);
    SetCrop_SownYear1(Crop.SownYear1);
    SetCrop_YearCCx(Crop.YearCCx);
    SetCrop_CCxRoot(Crop.CCxRoot);
    SetCrop_Assimilates(Crop.Assimilates);
end;


procedure SetCrop_subkind(constref subkind : rep_subkind);
var
    index : shortint;
begin;
    index := ord(subkind);
    __SetCrop_subkind(index);
end;


procedure SetCrop_ModeCycle(constref ModeCycle : rep_modeCycle);
var
    index : shortint;
begin;
    index := ord(ModeCycle);
    __SetCrop_ModeCycle(index);
end;


procedure SetCrop_Planting(constref Planting : rep_Planting);
var
    index : shortint;
begin;
    index := ord(Planting);
    __SetCrop_Planting(index);
end;


procedure SetCrop_pMethod(constref pMethod : rep_pMethod);
var
    index : shortint;
begin;
    index := ord(pMethod);
    __SetCrop_pMethod(index);
end;

function GetCrop_Assimilates() : rep_Assimilates;
begin;
    GetCrop_Assimilates.On := GetCrop_Assimilates_On();
    GetCrop_Assimilates.Period := GetCrop_Assimilates_Period();
    GetCrop_Assimilates.Stored := GetCrop_Assimilates_Stored();
    GetCrop_Assimilates.Mobilized := GetCrop_Assimilates_Mobilized();
end;


procedure SetCrop_Assimilates(constref Crop_Assimilates : rep_Assimilates);
begin;
    SetCrop_Assimilates_On(Crop_Assimilates.On);
    SetCrop_Assimilates_Period(Crop_Assimilates.Period);
    SetCrop_Assimilates_Stored(Crop_Assimilates.Stored);
    SetCrop_Assimilates_Mobilized(Crop_Assimilates.Mobilized);
end;

function GetOnset() : rep_Onset;
begin;
    GetOnset.GenerateOn := GetOnset_GenerateOn();
    GetOnset.GenerateTempOn := GetOnset_GenerateTempOn();
    GetOnset.Criterion := GetOnset_Criterion();
    GetOnset.AirTCriterion := GetOnset_AirTCriterion();
    GetOnset.StartSearchDayNr := GetOnset_StartSearchDayNr();
    GetOnset.StopSearchDayNr := GetOnset_StopSearchDayNr();
    GetOnset.LengthSearchPeriod := GetOnset_LengthSearchPeriod();
end;


function GetOnset_Criterion() : repCriterion;
var
    index : shortint;
begin;
    index := __GetOnset_Criterion();
    GetOnset_Criterion := repCriterion(index);
end;


function GetOnset_AirTCriterion() : repAirTCriterion;
var
    index : shortint;
begin;
    index := __GetOnset_AirTCriterion();
    GetOnset_AirTCriterion := repAirTCriterion(index);
end;


procedure SetOnset(constref Onset : rep_Onset);
begin;
    SetOnset_GenerateOn(Onset.GenerateOn);
    SetOnset_GenerateTempOn(Onset.GenerateTempOn);
    SetOnset_Criterion(Onset.Criterion);
    SetOnset_AirTCriterion(Onset.AirTCriterion);
    SetOnset_StartSearchDayNr(Onset.StartSearchDayNr);
    SetOnset_StopSearchDayNr(Onset.StopSearchDayNr);
    SetOnset_LengthSearchPeriod(Onset.LengthSearchPeriod);
end;


procedure SetOnset_Criterion(constref Criterion : repCriterion);
var
    index : shortint;
begin;
    index := ord(Criterion);
    __SetOnset_Criterion(index);
end;


procedure SetOnset_AirTCriterion(constref AirTCriterion : repAirTCriterion);
var
    index : shortint;
begin;
    index := ord(AirTCriterion);
    __SetOnset_AirTCriterion(index);
end;

function GetEndSeason() : rep_EndSeason;
begin;
    GetEndSeason.ExtraYears := GetEndSeason_ExtraYears();
    GetEndSeason.GenerateTempOn := GetEndSeason_GenerateTempOn();
    GetEndSeason.AirTCriterion := GetEndSeason_AirTCriterion();
    GetEndSeason.StartSearchDayNr := GetEndSeason_StartSearchDayNr();
    GetEndSeason.StopSearchDayNr := GetEndSeason_StopSearchDayNr();
    GetEndSeason.LengthSearchPeriod := GetEndSeason_LengthSearchPeriod();
end;


function GetEndSeason_AirTCriterion() : repAirTCriterion;
var
    index : shortint;
begin;
    index := __GetEndSeason_AirTCriterion();
    GetEndSeason_AirTCriterion := repAirTCriterion(index);
end;


procedure SetEndSeason(constref EndSeason : rep_EndSeason);
begin;
    SetEndSeason_ExtraYears(EndSeason.ExtraYears);
    SetEndSeason_GenerateTempOn(EndSeason.GenerateTempOn);
    SetEndSeason_AirTCriterion(EndSeason.AirTCriterion);
    SetEndSeason_StartSearchDayNr(EndSeason.StartSearchDayNr);
    SetEndSeason_StopSearchDayNr(EndSeason.StopSearchDayNr);
    SetEndSeason_LengthSearchPeriod(EndSeason.LengthSearchPeriod);
end;


procedure SetEndSeason_AirTCriterion(constref AirTCriterion : repAirTCriterion);
var
    index : shortint;
begin;
    index := ord(AirTCriterion);
    __SetEndSeason_AirTCriterion(index);
end;


function GetPerennialPeriod() : rep_PerennialPeriod;
begin;
    GetPerennialPeriod.GenerateOnset := GetPerennialPeriod_GenerateOnset();
    GetPerennialPeriod.OnsetCriterion := GetPerennialPeriod_OnsetCriterion();
    GetPerennialPeriod.OnsetFirstDay := GetPerennialPeriod_OnsetFirstDay();
    GetPerennialPeriod.OnsetFirstMonth := GetPerennialPeriod_OnsetFirstMonth();
    GetPerennialPeriod.OnsetStartSearchDayNr := GetPerennialPeriod_OnsetStartSearchDayNr();
    GetPerennialPeriod.OnsetStopSearchDayNr := GetPerennialPeriod_OnsetStopSearchDayNr();
    GetPerennialPeriod.OnsetLengthSearchPeriod := GetPerennialPeriod_OnsetLengthSearchPeriod();
    GetPerennialPeriod.OnsetThresholdValue := GetPerennialPeriod_OnsetThresholdValue();
    GetPerennialPeriod.OnsetPeriodValue := GetPerennialPeriod_OnsetPeriodValue();
    GetPerennialPeriod.OnsetOccurrence := GetPerennialPeriod_OnsetOccurrence();
    GetPerennialPeriod.GenerateEnd := GetPerennialPeriod_GenerateEnd();
    GetPerennialPeriod.EndCriterion := GetPerennialPeriod_EndCriterion();
    GetPerennialPeriod.EndLastDay := GetPerennialPeriod_EndLastDay();
    GetPerennialPeriod.EndLastMonth := GetPerennialPeriod_EndLastMonth();
    GetPerennialPeriod.ExtraYears := GetPerennialPeriod_ExtraYears();
    GetPerennialPeriod.EndStartSearchDayNr := GetPerennialPeriod_EndStartSearchDayNr();
    GetPerennialPeriod.EndStopSearchDayNr := GetPerennialPeriod_EndStopSearchDayNr();
    GetPerennialPeriod.EndLengthSearchPeriod := GetPerennialPeriod_EndLengthSearchPeriod();
    GetPerennialPeriod.EndThresholdValue := GetPerennialPeriod_EndThresholdValue();
    GetPerennialPeriod.EndPeriodValue := GetPerennialPeriod_EndPeriodValue();
    GetPerennialPeriod.EndOccurrence := GetPerennialPeriod_EndOccurrence();
    GetPerennialPeriod.GeneratedDayNrOnset := GetPerennialPeriod_GeneratedDayNrOnset();
    GetPerennialPeriod.GeneratedDayNrEnd := GetPerennialPeriod_GeneratedDayNrEnd();
end;


function GetPerennialPeriod_OnsetCriterion() : repAirTCriterion;
var
    index : shortint;
begin;
    index := __GetPerennialPeriod_OnsetCriterion();
    GetPerennialPeriod_OnsetCriterion := repAirTCriterion(index);
end;


function GetPerennialPeriod_EndCriterion() : repAirTCriterion;
var
    index : shortint;
begin;
    index := __GetPerennialPeriod_EndCriterion();
    GetPerennialPeriod_EndCriterion := repAirTCriterion(index);
end;


procedure SetPerennialPeriod(constref PerennialPeriod : rep_PerennialPeriod);
begin;
    SetPerennialPeriod_GenerateOnset(PerennialPeriod.GenerateOnset);
    SetPerennialPeriod_OnsetCriterion(PerennialPeriod.OnsetCriterion);
    SetPerennialPeriod_OnsetFirstDay(PerennialPeriod.OnsetFirstDay);
    SetPerennialPeriod_OnsetFirstMonth(PerennialPeriod.OnsetFirstMonth);
    SetPerennialPeriod_OnsetStartSearchDayNr(PerennialPeriod.OnsetStartSearchDayNr);
    SetPerennialPeriod_OnsetStopSearchDayNr(PerennialPeriod.OnsetStopSearchDayNr);
    SetPerennialPeriod_OnsetLengthSearchPeriod(PerennialPeriod.OnsetLengthSearchPeriod);
    SetPerennialPeriod_OnsetThresholdValue(PerennialPeriod.OnsetThresholdValue);
    SetPerennialPeriod_OnsetPeriodValue(PerennialPeriod.OnsetPeriodValue);
    SetPerennialPeriod_OnsetOccurrence(PerennialPeriod.OnsetOccurrence);
    SetPerennialPeriod_GenerateEnd(PerennialPeriod.GenerateEnd);
    SetPerennialPeriod_EndCriterion(PerennialPeriod.EndCriterion);
    SetPerennialPeriod_EndLastDay(PerennialPeriod.EndLastDay);
    SetPerennialPeriod_EndLastMonth(PerennialPeriod.EndLastMonth);
    SetPerennialPeriod_ExtraYears(PerennialPeriod.ExtraYears);
    SetPerennialPeriod_EndStartSearchDayNr(PerennialPeriod.EndStartSearchDayNr);
    SetPerennialPeriod_EndStopSearchDayNr(PerennialPeriod.EndStopSearchDayNr);
    SetPerennialPeriod_EndLengthSearchPeriod(PerennialPeriod.EndLengthSearchPeriod);
    SetPerennialPeriod_EndThresholdValue(PerennialPeriod.EndThresholdValue);
    SetPerennialPeriod_EndPeriodValue(PerennialPeriod.EndPeriodValue);
    SetPerennialPeriod_EndOccurrence(PerennialPeriod.EndOccurrence);
    SetPerennialPeriod_GeneratedDayNrOnset(PerennialPeriod.GeneratedDayNrOnset);
    SetPerennialPeriod_GeneratedDayNrEnd(PerennialPeriod.GeneratedDayNrEnd);
end;


procedure SetPerennialPeriod_OnsetCriterion(constref OnsetCriterion : repAirTCriterion);
var
    index : shortint;
begin;
    index := ord(OnsetCriterion);
    __SetPerennialPeriod_OnsetCriterion(index);
end;


procedure SetPerennialPeriod_EndCriterion(constref EndCriterion : repAirTCriterion);
var
    index : shortint;
begin;
    index := ord(EndCriterion);
    __SetPerennialPeriod_EndCriterion(index);
end;


procedure SetTotalWaterContent(constref TotalWaterContent : rep_Content);
begin;
    SetTotalWaterContent_BeginDay(TotalWaterContent.BeginDay);
    SetTotalWaterContent_EndDay(TotalWaterContent.EndDay);
    SetTotalWaterContent_ErrorDay(TotalWaterContent.ErrorDay);
end;

procedure GenerateCO2Description(
            constref CO2FileFull : string;
            var CO2Description : string);
var
    p1, p2 : PChar;
    strlen1, strlen2 : integer;

begin;
    p1 := PChar(CO2FileFull);
    p2 := PChar(CO2Description);
    strlen1 := Length(CO2FileFull);
    strlen2 := Length(CO2Description);
    GenerateCO2Description_wrap(p1, strlen1, p2, strlen2);
    SetCO2Description(AnsiString(p2));
end;

function GetCO2File(): string;
var
    p : PChar;

begin;
    p := GetCO2File_wrap();
    GetCO2File := AnsiString(p);
end;


procedure SetCO2File(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCO2File_wrap(p, strlen);
end;

function GetCO2FileFull(): string;
var
    p : PChar;

begin;
    p := GetCO2FileFull_wrap();
    GetCO2FileFull := AnsiString(p);
end;


procedure SetCO2FileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCO2FileFull_wrap(p, strlen);
end;

procedure ComposeOutputFileName(
            constref  TheProjectFileName: string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(TheProjectFileName);
    strlen := Length(TheProjectFileName);
    ComposeOutputFileName_wrap(p, strlen);
end;

function GetCO2Description(): string;
var
    p : PChar;

begin;
    p := GetCO2Description_wrap();
    GetCO2Description := AnsiString(p);
end;


procedure SetCO2Description(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCO2Description_wrap(p, strlen);
end;


function GetEToFile(): string;
var
    p : PChar;

begin;
    p := GetEToFile_wrap();
    GetEToFile := AnsiString(p);
end;


procedure SetEToFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetEToFile_wrap(p, strlen);
end;

function GetEToFileFull(): string;
var
    p : PChar;

begin;
    p := GetEToFileFull_wrap();
    GetEToFileFull := AnsiString(p);
end;


procedure SetEToFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetEToFileFull_wrap(p, strlen);
end;

function GetEToDescription(): string;
var
    p : PChar;

begin;
    p := GetEToDescription_wrap();
    GetEToDescription := AnsiString(p);
end;


procedure SetEToDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetEToDescription_wrap(p, strlen);
end;


function GetProfFile(): string;
var
    p : PChar;

begin;
    p := GetProfFile_wrap();
    GetProfFile := AnsiString(p);
end;

procedure SetProfFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetProfFile_wrap(p, strlen);
end;

function GetProfFilefull(): string;
var
    p : PChar;

begin;
    p := GetProfFilefull_wrap();
    GetProfFilefull := AnsiString(p);
end;

procedure SetProfFilefull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetProfFilefull_wrap(p, strlen);
end;

function GetProfDescription(): string;
var
    p : PChar;

begin;
    p := GetProfDescription_wrap();
    GetProfDescription := AnsiString(p);
end;

procedure SetProfDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetProfDescription_wrap(p, strlen);
end;

function GetManFile(): string;
var
    p : PChar;

begin;
    p := GetManFile_wrap();
    GetManFile := AnsiString(p);
end;

procedure SetManFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetManFile_wrap(p, strlen);
end;

function GetManFilefull(): string;
var
    p : PChar;

begin;
    p := GetManFilefull_wrap();
    GetManFilefull := AnsiString(p);
end;

procedure SetManFilefull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetManFilefull_wrap(p, strlen);
end;

function GetOffSeasonFile(): string;
var
    p : PChar;

begin;
    p := GetOffSeasonFile_wrap();
    GetOffSeasonFile := AnsiString(p);
end;

procedure SetOffSeasonFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetOffSeasonFile_wrap(p, strlen);
end;

function GetOffSeasonFilefull(): string;
var
    p : PChar;

begin;
    p := GetOffSeasonFilefull_wrap();
    GetOffSeasonFilefull := AnsiString(p);
end;

procedure SetOffSeasonFilefull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetOffSeasonFilefull_wrap(p, strlen);
end;

function GetObservationsFile(): string;
var
    p : PChar;

begin;
    p := GetObservationsFile_wrap();
    GetObservationsFile := AnsiString(p);
end;

procedure SetObservationsFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetObservationsFile_wrap(p, strlen);
end;

function GetObservationsFilefull(): string;
var
    p : PChar;

begin;
    p := GetObservationsFilefull_wrap();
    GetObservationsFilefull := AnsiString(p);
end;

procedure SetObservationsFilefull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetObservationsFilefull_wrap(p, strlen);
end;

function GetObservationsDescription(): string;
var
    p : PChar;

begin;
    p := GetObservationsDescription_wrap();
    GetObservationsDescription := AnsiString(p);
end;

procedure SetObservationsDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetObservationsDescription_wrap(p, strlen);
end;


function GetGroundWaterFile(): string;
var
    p : PChar;

begin;
    p := GetGroundWaterFile_wrap();
    GetGroundWaterFile := AnsiString(p);
end;

procedure SetGroundWaterFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetGroundWaterFile_wrap(p, strlen);
end;

function GetGroundWaterFilefull(): string;
var
    p : PChar;

begin;
    p := GetGroundWaterFilefull_wrap();
    GetGroundWaterFilefull := AnsiString(p);
end;

procedure SetGroundWaterFilefull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetGroundWaterFilefull_wrap(p, strlen);
end;

procedure LoadProjectDescription(
            constref FullNameProjectFile : string;
            var DescriptionOfProject: string);
var
    p1, p2 : PChar;
    strlen1, strlen2 : integer;

begin;
    p1 := PChar(FullNameProjectFile);
    p2 := PChar(DescriptionOfProject);
    strlen1 := Length(FullNameProjectFile);
    strlen2 := Length(DescriptionOfProject);
    LoadProjectDescription_wrap(p1, strlen1, p2, strlen2);
    DescriptionOfProject := AnsiString(p2);
end;


procedure CheckFilesInProject(
            constref TempFullFilename : string;
            constref Runi : integer;
            var AllOK : boolean);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(TempFullFilename);
    strlen := Length(TempFullFilename);
    CheckFilesInProject_wrap(p, strlen, Runi, AllOK);
end;

function GetTemperatureRecord_DataType(): rep_datatype;
var
    int_datatype : shortint;
begin;
    int_datatype := __GetTemperatureRecord_DataType();
    GetTemperatureRecord_DataType := rep_DataType(int_datatype);
end;

procedure SetTemperatureRecord_DataType(constref DataType : rep_datatype);
var
   int_datatype : shortint;

begin;
   int_datatype := ord(DataType);
   __SetTemperatureRecord_DataType(int_datatype); 
end;

function GetTemperatureRecord_FromString(): string;
var
    p : PChar;

begin;
    p := GetTemperatureRecord_FromString_wrap();
    GetTemperatureRecord_FromString := AnsiString(p);
end;

function GetTemperatureRecord_ToString(): string;
var
    p : PChar;

begin;
    p := GetTemperatureRecord_ToString_wrap();
    GetTemperatureRecord_toString := AnsiString(p);
end;

procedure SetTemperatureRecord_FromString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetTemperatureRecord_FromString_wrap(p, strlen);
end;

procedure SetTemperatureRecord_ToString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetTemperatureRecord_ToString_wrap(p, strlen);
end;

function GetClimRecord_DataType(): rep_datatype;
var
    int_datatype : shortint;
begin;
    int_datatype := __GetClimRecord_DataType();
    GetClimRecord_DataType := rep_DataType(int_datatype);
end;

procedure SetClimRecord_DataType(constref DataType : rep_datatype);
var
   int_datatype : shortint;

begin;
   int_datatype := ord(DataType);
   __SetClimRecord_DataType(int_datatype);
end;

function GetClimRecord_FromString(): string;
var
    p : PChar;

begin;
    p := GetClimRecord_FromString_wrap();
    GetClimRecord_FromString := AnsiString(p);
end;

function GetClimRecord_ToString(): string;
var
    p : PChar;

begin;
    p := GetClimRecord_ToString_wrap();
    GetClimRecord_toString := AnsiString(p);
end;

procedure SetClimRecord_FromString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimRecord_FromString_wrap(p, strlen);
end;

procedure SetClimRecord_ToString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimRecord_ToString_wrap(p, strlen);
end;

function GetRainRecord_DataType(): rep_datatype;
var
    int_datatype : shortint;
begin;
    int_datatype := __GetRainRecord_DataType();
    GetRainRecord_DataType := rep_DataType(int_datatype);
end;

procedure SetRainRecord_DataType(constref DataType : rep_datatype);
var
   int_datatype : shortint;

begin;
   int_datatype := ord(DataType);
   __SetRainRecord_DataType(int_datatype);
end;

function GetRainRecord_FromString(): string;
var
    p : PChar;

begin;
    p := GetRainRecord_FromString_wrap();
    GetRainRecord_FromString := AnsiString(p);
end;

function GetRainRecord_ToString(): string;
var
    p : PChar;

begin;
    p := GetRainRecord_ToString_wrap();
    GetRainRecord_toString := AnsiString(p);
end;

procedure SetRainRecord_FromString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetRainRecord_FromString_wrap(p, strlen);
end;

procedure SetRainRecord_ToString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetRainRecord_ToString_wrap(p, strlen);
end;

function GetEToRecord_DataType(): rep_datatype;
var
    int_datatype : shortint;
begin;
    int_datatype := __GetEToRecord_DataType();
    GetEToRecord_DataType := rep_DataType(int_datatype);
end;

procedure SetEToRecord_DataType(constref DataType : rep_datatype);
var
   int_datatype : shortint;

begin;
   int_datatype := ord(DataType);
   __SetEToRecord_DataType(int_datatype);
end;

function GetEToRecord_FromString(): string;
var
    p : PChar;

begin;
    p := GetEToRecord_FromString_wrap();
    GetEToRecord_FromString := AnsiString(p);
end;

function GetEToRecord_ToString(): string;
var
    p : PChar;

begin;
    p := GetEToRecord_ToString_wrap();
    GetEToRecord_toString := AnsiString(p);
end;

procedure SetEToRecord_FromString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetEToRecord_FromString_wrap(p, strlen);
end;

procedure SetEToRecord_ToString(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetEToRecord_ToString_wrap(p, strlen);
end;

procedure GetIrriDescription(
            constref IrriFileFull : string;
            var IrriDescription : string);
var
    p1, p2 : PChar;
    strlen1, strlen2 : integer;

begin;
    p1 := PChar(IrriFileFull);
    p2 := PChar(IrriDescription);
    strlen1 := Length(IrriFileFull);
    strlen2 := Length(IrriDescription);
    GetIrriDescription_wrap(p1, strlen1, p2, strlen2);
    IrriDescription := AnsiString(p2);
end;


function GetIrriFile(): string;
var
    p : PChar;

begin;
    p := GetIrriFile_wrap();
    GetIrriFile := AnsiString(p);
end;


procedure SetIrriFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetIrriFile_wrap(p, strlen);
end;


function GetIrriFileFull(): string;
var
    p : PChar;

begin;
    p := GetIrriFileFull_wrap();
    GetIrriFileFull := AnsiString(p);
end;


procedure SetIrriFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetIrriFileFull_wrap(p, strlen);
end;


function GetClimateFile(): string;
var
    p : PChar;

begin;
    p := GetClimateFile_wrap();
    GetClimateFile := AnsiString(p);
end;


procedure SetClimateFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimateFile_wrap(p, strlen);
end;


function GetClimateFileFull(): string;
var
    p : PChar;

begin;
    p := GetClimateFileFull_wrap();
    GetClimateFileFull := AnsiString(p);
end;


procedure SetClimateFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimateFileFull_wrap(p, strlen);
end;


procedure SetIrriDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetIrriDescription_wrap(p, strlen);
end;

function GetClimateDescription(): string;
var
    p : PChar;

begin;
    p := GetClimateDescription_wrap();
    GetClimateDescription := AnsiString(p);
end;


procedure SetClimateDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimateDescription_wrap(p, strlen);
end;


function GetClimFile(): string;
var
    p : PChar;

begin;
    p := GetClimFile_wrap();
    GetClimFile := AnsiString(p);
end;


procedure SetClimFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetClimFile_wrap(p, strlen);
end;


function GetSWCiniFile(): string;
var
    p : PChar;

begin;
    p := GetSWCiniFile_wrap();
    GetSWCiniFile := AnsiString(p);
end;


procedure SetSWCiniFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetSWCiniFile_wrap(p, strlen);
end;


function GetSWCiniFileFull(): string;
var
    p : PChar;

begin;
    p := GetSWCiniFileFull_wrap();
    GetSWCiniFileFull := AnsiString(p);
end;


procedure SetSWCiniFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetSWCiniFileFull_wrap(p, strlen);
end;

function GetSWCiniDescription(): string;
var
    p : PChar;

begin;
    p := GetSWCiniDescription_wrap();
    GetSWCiniDescription := AnsiString(p);
end;


procedure SetSWCiniDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetSWCiniDescription_wrap(p, strlen);
end;


function GetProjectFile(): string;
var
    p : PChar;

begin;
    p := GetProjectFile_wrap();
    GetProjectFile := AnsiString(p);
end;

procedure SetProjectFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetProjectFile_wrap(p, strlen);
end;


function GetProjectFileFull(): string;
var
    p : PChar;

begin;
    p := GetProjectFileFull_wrap();
    GetProjectFileFull := AnsiString(p);
end;

procedure SetProjectFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetProjectFileFull_wrap(p, strlen);
end;


function GetMultipleProjectFile(): string;
var
    p : PChar;

begin;
    p := GetMultipleProjectFile_wrap();
    GetMultipleProjectFile := AnsiString(p);
end;


procedure SetMultipleProjectFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetMultipleProjectFile_wrap(p, strlen);
end;


function GetMultipleProjectFileFull(): string;
var
    p : PChar;

begin;
    p := GetMultipleProjectFileFull_wrap();
    GetMultipleProjectFileFull := AnsiString(p);
end;


procedure SetMultipleProjectFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetMultipleProjectFileFull_wrap(p, strlen);
end;


function GetFullFileNameProgramParameters(): string;
var
    p : PChar;

begin;
    p := GetFullFileNameProgramParameters_wrap();
    GetFullFileNameProgramParameters := AnsiString(p);
end;


procedure SetFullFileNameProgramParameters(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetFullFileNameProgramParameters_wrap(p, strlen);
end;

procedure GetFileForProgramParameters(
            constref  TheFullFileNameProgram: string;
            var FullFileNameProgramParameters: string);
var
    p1, p2 : PChar;
    strlen1, strlen2 : integer;

begin;
    p1 := PChar(TheFullFileNameProgram);
    p2 := PChar(FullFileNameProgramParameters);
    strlen1 := Length(TheFullFileNameProgram);
    strlen2 := Length(FullFileNameProgramParameters);
    GetFileForProgramParameters_wrap(p1, strlen1, p2, strlen2);
    FullFileNameProgramParameters := AnsiString(p2);
end;

function GetPathNameProg(): string;
var
    p : PChar;

begin;
    p := GetPathNameProg_wrap();
    GetPathNameProg := AnsiString(p);
end;


procedure SetPathNameProg(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetPathNameProg_wrap(p, strlen);
end;


function GetPathNameOutp(): string;
var
    p : PChar;

begin;
    p := GetPathNameOutp_wrap();
    GetPathNameOutp := AnsiString(p);
end;

procedure SetPathNameOutp(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetPathNameOutp_wrap(p, strlen);
end;

function GetPathNameSimul(): string;
var
    p : PChar;

begin;
    p := GetPathNameSimul_wrap();
    GetPathNameSimul := AnsiString(p);
end;

function GetOutputName(): string;
var
    p : PChar;

begin;
    p := GetOutputName_wrap();
    GetOutputName := AnsiString(p);
end;

procedure SetOutputName(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetOutputName_wrap(p, strlen);
end;

procedure SetPathNameSimul(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetPathNameSimul_wrap(p, strlen);
    SetMultipleProjectFile_wrap(p, strlen);
end;

function GetRainFile(): string;
var
    p : PChar;

begin;
    p := GetRainFile_wrap();
    GetRainFile := AnsiString(p);
end;

procedure SetRainFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetRainFile_wrap(p, strlen);
end;

function GetRainFileFull(): string;
var
    p : PChar;

begin;
    p := GetRainFileFull_wrap();
    GetRainFileFull := AnsiString(p);
end;

procedure SetRainFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetRainFileFull_wrap(p, strlen);
end;

function GetRainDescription(): string;
var
    p : PChar;

begin;
    p := GetRainDescription_wrap();
    GetRainDescription := AnsiString(p);
end;

procedure SetRainDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetRainDescription_wrap(p, strlen);
end;



function GetCalendarFile(): string;
var
    p : PChar;

begin;
    p := GetCalendarFile_wrap();
    GetCalendarFile := AnsiString(p);
end;

procedure SetCalendarFile(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCalendarFile_wrap(p, strlen);
end;


function GetCalendarFileFull(): string;
var
    p : PChar;

begin;
    p := GetCalendarFileFull_wrap();
    GetCalendarFileFull := AnsiString(p);
end;

procedure SetCalendarFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCalendarFileFull_wrap(p, strlen);
end;


function GetCalendarDescription(): string;
var
    p : PChar;

begin;
    p := GetCalendarDescription_wrap();
    GetCalendarDescription := AnsiString(p);
end;

procedure SetCalendarDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetCalendarDescription_wrap(p, strlen);
end;


function GetCropFile(): string;
var
    p : PChar;

begin;
    p := GetCropFile_wrap();
    GetCropFile := AnsiString(p);
end;


procedure SetCropFile(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetCropFile_wrap(p, strlen);
end;



function GetSumWaBal() : rep_sum;
begin;
    GetSumWaBal.Epot := GetSumWaBal_Epot();
    GetSumWaBal.Tpot := GetSumWaBal_Tpot();
    GetSumWaBal.Rain := GetSumWaBal_Rain();
    GetSumWaBal.Irrigation := GetSumWaBal_Irrigation();
    GetSumWaBal.Infiltrated := GetSumWaBal_Infiltrated();
    GetSumWaBal.Runoff := GetSumWaBal_Runoff();
    GetSumWaBal.Drain := GetSumWaBal_Drain();
    GetSumWaBal.Eact := GetSumWaBal_Eact();
    GetSumWaBal.Tact := GetSumWaBal_Tact();
    GetSumWaBal.TrW := GetSumWaBal_TrW();
    GetSumWaBal.ECropCycle := GetSumWaBal_ECropCycle();
    GetSumWaBal.CRwater := GetSumWaBal_CRwater();
    GetSumWaBal.Biomass := GetSumWaBal_Biomass();
    GetSumWaBal.YieldPart := GetSumWaBal_YieldPart();
    GetSumWaBal.BiomassPot := GetSumWaBal_BiomassPot();
    GetSumWaBal.BiomassUnlim := GetSumWaBal_BiomassUnlim();
    GetSumWaBal.BiomassTot := GetSumWaBal_BiomassTot();
    GetSumWaBal.SaltIn := GetSumWaBal_SaltIn();
    GetSumWaBal.SaltOut := GetSumWaBal_SaltOut();
    GetSumWaBal.CRsalt := GetSumWaBal_CRsalt();
end;

procedure SetSumWaBal(constref SumWaBal : rep_sum);
begin;
    SetSumWaBal_Epot(SumWaBal.Epot);
    SetSumWaBal_Tpot(SumWaBal.Tpot);
    SetSumWaBal_Rain(SumWaBal.Rain);
    SetSumWaBal_Irrigation(SumWaBal.Irrigation);
    SetSumWaBal_Infiltrated(SumWaBal.Infiltrated);
    SetSumWaBal_Runoff(SumWaBal.Runoff);
    SetSumWaBal_Drain(SumWaBal.Drain);
    SetSumWaBal_Eact(SumWaBal.Eact);
    SetSumWaBal_Tact(SumWaBal.Tact);
    SetSumWaBal_TrW(SumWaBal.TrW);
    SetSumWaBal_ECropCycle(SumWaBal.ECropCycle);
    SetSumWaBal_CRwater(SumWaBal.CRwater);
    SetSumWaBal_Biomass(SumWaBal.Biomass);
    SetSumWaBal_YieldPart(SumWaBal.YieldPart);
    SetSumWaBal_BiomassPot(SumWaBal.BiomassPot);
    SetSumWaBal_BiomassUnlim(SumWaBal.BiomassUnlim);
    SetSumWaBal_BiomassTot(SumWaBal.BiomassTot);
    SetSumWaBal_SaltIn(SumWaBal.SaltIn);
    SetSumWaBal_SaltOut(SumWaBal.SaltOut);
    SetSumWaBal_CRsalt(SumWaBal.CRsalt);
end;


function GetCropFileFull(): string;
var
    p : PChar;

begin;
    p := GetCropFileFull_wrap();
    GetCropFileFull := AnsiString(p);
end;


procedure SetCropFileFull(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetCropFileFull_wrap(p, strlen);
end;


function GetCropDescription(): string;
var
    p : PChar;

begin;
    p := GetCropDescription_wrap();
    GetCropDescription := AnsiString(p);
end;


procedure SetCropDescription(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetCropDescription_wrap(p, strlen);
end;


function GetTemperatureFile(): string;
var
     p : PChar;
begin;
     p := GetTemperatureFile_wrap();
     GetTemperatureFile := AnsiString(p);
end;


procedure SetTemperatureFile(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetTemperatureFile_wrap(p, strlen);
end;

function GetTemperatureFilefull(): string;
var
     p : PChar;
begin;
     p := GetTemperatureFilefull_wrap();
     GetTemperatureFilefull := AnsiString(p);
end;


procedure SetTemperatureFilefull(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetTemperatureFilefull_wrap(p, strlen);
end;


function GetTemperatureDescription(): string;
var
     p : PChar;
begin;
     p := GetTemperatureDescription_wrap();
     GetTemperatureDescription := AnsiString(p);
end;


procedure SetTemperatureDescription(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetTemperatureDescription_wrap(p, strlen);
end;


function GetClimDescription(): string;
var
     p : PChar;
begin;
     p := GetClimDescription_wrap();
     GetClimDescription := AnsiString(p);
end;


procedure SetClimDescription(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetClimDescription_wrap(p, strlen);
end;


function GetTemperatureRecord() : rep_clim;
begin
    GetTemperatureRecord.DataType := GetTemperatureRecord_DataType();
    GetTemperatureRecord.FromD    := GetTemperatureRecord_FromD();
    GetTemperatureRecord.FromM    := GetTemperatureRecord_FromM();
    GetTemperatureRecord.FromY    := GetTemperatureRecord_FromY();
    GetTemperatureRecord.ToD      := GetTemperatureRecord_ToD();
    GetTemperatureRecord.ToM      := GetTemperatureRecord_ToM();
    GetTemperatureRecord.ToY      := GetTemperatureRecord_ToY();
    GetTemperatureRecord.ToDayNr  := GetTemperatureRecord_ToDayNr();
    GetTemperatureRecord.FromDayNr:= GetTemperatureRecord_FromDayNr();
    GetTemperatureRecord.NrObs    := GetTemperatureRecord_NrObs();
    GetTemperatureRecord.FromString := GetTemperatureRecord_FromString();
    GetTemperatureRecord.ToString   := GetTemperatureRecord_ToString();
end;

procedure SetTemperatureRecord(constref TemperatureRecord : rep_clim);
begin
    SetTemperatureRecord_DataType(TemperatureRecord.DataType);
    SetTemperatureRecord_FromD(TemperatureRecord.FromD);
    SetTemperatureRecord_FromM(TemperatureRecord.FromM);
    SetTemperatureRecord_FromY(TemperatureRecord.FromY);
    SetTemperatureRecord_ToD(TemperatureRecord.ToD);
    SetTemperatureRecord_ToM(TemperatureRecord.ToM);
    SetTemperatureRecord_ToY(TemperatureRecord.ToY);
    SetTemperatureRecord_ToDayNr(TemperatureRecord.ToDayNr);
    SetTemperatureRecord_FromDayNr(TemperatureRecord.FromDayNr);
    SetTemperatureRecord_NrObs(TemperatureRecord.NrObs);
    SetTemperatureRecord_FromString(TemperatureRecord.FromString);
    SetTemperatureRecord_ToString(TemperatureRecord.ToString);
end;

function GetClimRecord() : rep_clim;
begin
    GetClimRecord.DataType := GetClimRecord_DataType();
    GetClimRecord.FromD    := GetClimRecord_FromD();
    GetClimRecord.FromM    := GetClimRecord_FromM();
    GetClimRecord.FromY    := GetClimRecord_FromY();
    GetClimRecord.ToD      := GetClimRecord_ToD();
    GetClimRecord.ToM      := GetClimRecord_ToM();
    GetClimRecord.ToY      := GetClimRecord_ToY();
    GetClimRecord.ToDayNr  := GetClimRecord_ToDayNr();
    GetClimRecord.FromDayNr:= GetClimRecord_FromDayNr();
    GetClimRecord.NrObs    := GetClimRecord_NrObs();
    GetClimRecord.FromString := GetClimRecord_FromString();
    GetClimRecord.ToString   := GetClimRecord_ToString();
end;

procedure SetClimRecord(constref ClimRecord : rep_clim);
begin
    SetClimRecord_DataType(ClimRecord.DataType);
    SetClimRecord_FromD(ClimRecord.FromD);
    SetClimRecord_FromM(ClimRecord.FromM);
    SetClimRecord_FromY(ClimRecord.FromY);
    SetClimRecord_ToD(ClimRecord.ToD);
    SetClimRecord_ToM(ClimRecord.ToM);
    SetClimRecord_ToY(ClimRecord.ToY);
    SetClimRecord_ToDayNr(ClimRecord.ToDayNr);
    SetClimRecord_FromDayNr(ClimRecord.FromDayNr);
    SetClimRecord_NrObs(ClimRecord.NrObs);
    SetClimRecord_FromString(ClimRecord.FromString);
    SetClimRecord_ToString(ClimRecord.ToString);
end;

function GetRainRecord() : rep_clim;
begin
    GetRainRecord.DataType := GetRainRecord_DataType();
    GetRainRecord.FromD    := GetRainRecord_FromD();
    GetRainRecord.FromM    := GetRainRecord_FromM();
    GetRainRecord.FromY    := GetRainRecord_FromY();
    GetRainRecord.ToD      := GetRainRecord_ToD();
    GetRainRecord.ToM      := GetRainRecord_ToM();
    GetRainRecord.ToY      := GetRainRecord_ToY();
    GetRainRecord.ToDayNr  := GetRainRecord_ToDayNr();
    GetRainRecord.FromDayNr:= GetRainRecord_FromDayNr();
    GetRainRecord.NrObs    := GetRainRecord_NrObs();
    GetRainRecord.FromString := GetRainRecord_FromString();
    GetRainRecord.ToString   := GetRainRecord_ToString();
end;

procedure SetRainRecord(constref RainRecord : rep_clim);
begin
    SetRainRecord_DataType(RainRecord.DataType);
    SetRainRecord_FromD(RainRecord.FromD);
    SetRainRecord_FromM(RainRecord.FromM);
    SetRainRecord_FromY(RainRecord.FromY);
    SetRainRecord_ToD(RainRecord.ToD);
    SetRainRecord_ToM(RainRecord.ToM);
    SetRainRecord_ToY(RainRecord.ToY);
    SetRainRecord_ToDayNr(RainRecord.ToDayNr);
    SetRainRecord_FromDayNr(RainRecord.FromDayNr);
    SetRainRecord_NrObs(RainRecord.NrObs);
    SetRainRecord_FromString(RainRecord.FromString);
    SetRainRecord_ToString(RainRecord.ToString);
end;

function GetEToRecord() : rep_clim;
begin
    GetEToRecord.DataType := GetEToRecord_DataType();
    GetEToRecord.FromD    := GetEToRecord_FromD();
    GetEToRecord.FromM    := GetEToRecord_FromM();
    GetEToRecord.FromY    := GetEToRecord_FromY();
    GetEToRecord.ToD      := GetEToRecord_ToD();
    GetEToRecord.ToM      := GetEToRecord_ToM();
    GetEToRecord.ToY      := GetEToRecord_ToY();
    GetEToRecord.ToDayNr  := GetEToRecord_ToDayNr();
    GetEToRecord.FromDayNr:= GetEToRecord_FromDayNr();
    GetEToRecord.NrObs    := GetEToRecord_NrObs();
    GetEToRecord.FromString := GetEToRecord_FromString();
    GetEToRecord.ToString   := GetEToRecord_ToString();
end;

procedure SetEToRecord(constref EToRecord : rep_clim);
begin
    SetEToRecord_DataType(EToRecord.DataType);
    SetEToRecord_FromD(EToRecord.FromD);
    SetEToRecord_FromM(EToRecord.FromM);
    SetEToRecord_FromY(EToRecord.FromY);
    SetEToRecord_ToD(EToRecord.ToD);
    SetEToRecord_ToM(EToRecord.ToM);
    SetEToRecord_ToY(EToRecord.ToY);
    SetEToRecord_ToDayNr(EToRecord.ToDayNr);
    SetEToRecord_FromDayNr(EToRecord.FromDayNr);
    SetEToRecord_NrObs(EToRecord.NrObs);
    SetEToRecord_FromString(EToRecord.FromString);
    SetEToRecord_ToString(EToRecord.ToString);
end;

function GetSimulation_ThetaIni() : rep_IniComp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetSimulation_ThetaIni[i] := GetSimulation_ThetaIni_i(i)
end;

function GetSimulation_ECeIni() : rep_IniComp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetSimulation_ECeIni[i] := GetSimulation_ECeIni_i(i)
end;

function GetSimulation() : rep_sim;
begin;
    GetSimulation.FromDayNr := GetSimulation_FromDayNr();
    GetSimulation.ToDayNr := GetSimulation_ToDayNr();
    GetSimulation.ThetaIni := GetSimulation_ThetaIni();
    GetSimulation.ECeIni := GetSimulation_ECeIni();
    GetSimulation.IniSWC := GetSimulation_IniSWC();
    GetSimulation.SurfaceStorageIni := GetSimulation_SurfaceStorageIni();
    GetSimulation.ECStorageIni := GetSimulation_ECStorageIni();
    GetSimulation.CCini := GetSimulation_CCini();
    GetSimulation.Bini := GetSimulation_Bini();
    GetSimulation.Zrini := GetSimulation_Zrini();
    GetSimulation.LinkCropToSimPeriod := GetSimulation_LinkCropToSimPeriod();
    GetSimulation.ResetIniSWC := GetSimulation_ResetIniSWC();
    GetSimulation.InitialStep := GetSimulation_InitialStep();
    GetSimulation.EvapLimitON := GetSimulation_EvapLimitON();
    GetSimulation.EvapWCsurf := GetSimulation_EvapWCsurf();
    GetSimulation.EvapStartStg2 := GetSimulation_EvapStartStg2();
    GetSimulation.EvapZ := GetSimulation_EvapZ();
    GetSimulation.HIfinal := GetSimulation_HIfinal();
    GetSimulation.DelayedDays := GetSimulation_DelayedDays();
    GetSimulation.Germinate := GetSimulation_Germinate();
    GetSimulation.SumEToStress := GetSimulation_SumEToStress();
    GetSimulation.SumGDD := GetSimulation_SumGDD();
    GetSimulation.SumGDDfromDay1 := GetSimulation_SumGDDfromDay1();
    GetSimulation.SCor := GetSimulation_SCor();
    GetSimulation.MultipleRun := GetSimulation_MultipleRun();
    GetSimulation.NrRuns := GetSimulation_NrRuns();
    GetSimulation.MultipleRunWithKeepSWC := GetSimulation_MultipleRunWithKeepSWC();
    GetSimulation.MultipleRunConstZrx := GetSimulation_MultipleRunConstZrx();
    GetSimulation.IrriECw := GetSimulation_IrriECw();
    GetSimulation.DayAnaero := GetSimulation_DayAnaero();
    GetSimulation.EffectStress := GetSimulation_EffectStress();
    GetSimulation.SalinityConsidered := GetSimulation_SalinityConsidered();
    GetSimulation.ProtectedSeedling := GetSimulation_ProtectedSeedling();
    GetSimulation.SWCtopSoilConsidered := GetSimulation_SWCtopSoilConsidered();
    GetSimulation.LengthCuttingInterval := GetSimulation_LengthCuttingInterval();
    GetSimulation.YearSeason := GetSimulation_YearSeason();
    GetSimulation.RCadj := GetSimulation_RCadj();
    GetSimulation.Storage := GetSimulation_Storage();
    GetSimulation.YearStartCropCycle := GetSimulation_YearStartCropCycle();
    GetSimulation.CropDay1Previous := GetSimulation_CropDay1Previous();
end;

procedure SetSimulation_ThetaIni(constref ThetaIni : rep_IniComp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetSimulation_ThetaIni_i(i, ThetaIni[i])
end;

procedure SetSimulation_ECeIni(constref ECeIni : rep_IniComp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetSimulation_ECeIni_i(i, ECeIni[i])
end;

procedure SetSimulation(constref Simulation : rep_sim);
begin;
    SetSimulation_FromDayNr(Simulation.FromDayNr);
    SetSimulation_ToDayNr(Simulation.ToDayNr);
    SetSimulation_ThetaIni(Simulation.ThetaIni);
    SetSimulation_ECeIni(Simulation.ECeIni);
    SetSimulation_IniSWC(Simulation.IniSWC);
    SetSimulation_SurfaceStorageIni(Simulation.SurfaceStorageIni);
    SetSimulation_ECStorageIni(Simulation.ECStorageIni);
    SetSimulation_CCini(Simulation.CCini);
    SetSimulation_Bini(Simulation.Bini);
    SetSimulation_Zrini(Simulation.Zrini);
    SetSimulation_LinkCropToSimPeriod(Simulation.LinkCropToSimPeriod);
    SetSimulation_ResetIniSWC(Simulation.ResetIniSWC);
    SetSimulation_InitialStep(Simulation.InitialStep);
    SetSimulation_EvapLimitON(Simulation.EvapLimitON);
    SetSimulation_EvapWCsurf(Simulation.EvapWCsurf);
    SetSimulation_EvapStartStg2(Simulation.EvapStartStg2);
    SetSimulation_EvapZ(Simulation.EvapZ);
    SetSimulation_HIfinal(Simulation.HIfinal);
    SetSimulation_DelayedDays(Simulation.DelayedDays);
    SetSimulation_Germinate(Simulation.Germinate);
    SetSimulation_SumEToStress(Simulation.SumEToStress);
    SetSimulation_SumGDD(Simulation.SumGDD);
    SetSimulation_SumGDDfromDay1(Simulation.SumGDDfromDay1);
    SetSimulation_SCor(Simulation.SCor);
    SetSimulation_MultipleRun(Simulation.MultipleRun);
    SetSimulation_NrRuns(Simulation.NrRuns);
    SetSimulation_MultipleRunWithKeepSWC(Simulation.MultipleRunWithKeepSWC);
    SetSimulation_MultipleRunConstZrx(Simulation.MultipleRunConstZrx);
    SetSimulation_IrriECw(Simulation.IrriECw);
    SetSimulation_DayAnaero(Simulation.DayAnaero);
    SetSimulation_EffectStress(Simulation.EffectStress);
    SetSimulation_SalinityConsidered(Simulation.SalinityConsidered);
    SetSimulation_ProtectedSeedling(Simulation.ProtectedSeedling);
    SetSimulation_SWCtopSoilConsidered(Simulation.SWCtopSoilConsidered);
    SetSimulation_LengthCuttingInterval(Simulation.LengthCuttingInterval);
    SetSimulation_YearSeason(Simulation.YearSeason);
    SetSimulation_RCadj(Simulation.RCadj);
    SetSimulation_Storage(Simulation.Storage);
    SetSimulation_YearStartCropCycle(Simulation.YearStartCropCycle);
    SetSimulation_CropDay1Previous(Simulation.CropDay1Previous);
end;

function GetSimulation_IniSWC_Loc() : rep_IniComp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetSimulation_IniSWC_Loc[i] := GetSimulation_IniSWC_Loc_i(i)
end;

function GetSimulation_IniSWC_VolProc() : rep_IniComp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetSimulation_IniSWC_VolProc[i] := GetSimulation_IniSWC_VolProc_i(i)
end;

function GetSimulation_IniSWC_SaltECe() : rep_IniComp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetSimulation_IniSWC_SaltECe[i] := GetSimulation_IniSWC_SaltECe_i(i)
end;

function GetSimulation_IniSWC() : rep_IniSWC;
begin;
    GetSimulation_IniSWC.AtDepths := GetSimulation_IniSWC_AtDepths();
    GetSimulation_IniSWC.NrLoc := GetSimulation_IniSWC_NrLoc();
    GetSimulation_IniSWC.Loc := GetSimulation_IniSWC_Loc();
    GetSimulation_IniSWC.VolProc := GetSimulation_IniSWC_VolProc();
    GetSimulation_IniSWC.SaltECe := GetSimulation_IniSWC_SaltECe();
    GetSimulation_IniSWC.AtFC := GetSimulation_IniSWC_AtFC();
end;

procedure SetSimulation_IniSWC_Loc(constref Loc : rep_IniComp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetSimulation_IniSWC_Loc_i(i, Loc[i]);
end;

procedure SetSimulation_IniSWC_VolProc(constref VolProc : rep_IniComp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetSimulation_IniSWC_VolProc_i(i, VolProc[i])
end;

procedure SetSimulation_IniSWC_SaltECe(constref SaltECe : rep_IniComp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetSimulation_IniSWC_SaltECe_i(i, SaltECe[i])
end;

procedure SetSimulation_IniSWC(constref Simulation_IniSWC : rep_IniSWC);
begin;
    SetSimulation_IniSWC_AtDepths(Simulation_IniSWC.AtDepths);
    SetSimulation_IniSWC_Loc(Simulation_IniSWC.Loc);
    SetSimulation_IniSWC_VolProc(Simulation_IniSWC.VolProc);
    SetSimulation_IniSWC_SaltECe(Simulation_IniSWC.SaltECe);
    SetSimulation_IniSWC_NrLoc(Simulation_IniSWC.NrLoc);
    SetSimulation_IniSWC_AtFC(Simulation_IniSWC.AtFC);
end;


function GetSimulation_EffectStress() : rep_EffectStress;
begin;
    GetSimulation_EffectStress.RedCGC := GetSimulation_EffectStress_RedCGC();
    GetSimulation_EffectStress.RedCCX := GetSimulation_EffectStress_RedCCX();
    GetSimulation_EffectStress.RedWP := GetSimulation_EffectStress_RedWP();
    GetSimulation_EffectStress.CDecline := GetSimulation_EffectStress_CDecline();
    GetSimulation_EffectStress.RedKsSto := GetSimulation_EffectStress_RedKsSto();
end;


procedure SetSimulation_EffectStress(constref Simulation_EffectStress : rep_EffectStress);
begin;
    SetSimulation_EffectStress_RedCGC(Simulation_EffectStress.RedCGC);
    SetSimulation_EffectStress_RedCCX(Simulation_EffectStress.RedCCX);
    SetSimulation_EffectStress_RedWP(Simulation_EffectStress.RedWP);
    SetSimulation_EffectStress_CDecline(Simulation_EffectStress.CDecline);
    SetSimulation_EffectStress_RedKsSto(Simulation_EffectStress.RedKsSto);
end;

function GetSimulation_Storage() : rep_storage;
begin;
    GetSimulation_Storage.Btotal := GetSimulation_Storage_Btotal();
    GetSimulation_Storage.CropString := GetSimulation_Storage_CropString();
    GetSimulation_Storage.Season := GetSimulation_Storage_Season();
end;


function GetSimulation_Storage_CropString() : string;
var
    p : PChar;
begin;
    p := GetSimulation_Storage_CropString_wrap();
    GetSimulation_Storage_CropString := AnsiString(p);
end;


procedure SetSimulation_Storage(constref Simulation_Storage : rep_storage);
begin;
    SetSimulation_Storage_Btotal(Simulation_Storage.Btotal);
    SetSimulation_Storage_CropString(Simulation_Storage.CropString);
    SetSimulation_Storage_Season(Simulation_Storage.Season);
end;


procedure SetSimulation_Storage_CropString(constref CropString : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(CropString);
    strlen := Length(CropString);
    SetSimulation_Storage_CropString_wrap(p, strlen);
end;

procedure LoadIrriScheduleInfo(
            constref FullName : string);
var
    p1 : PChar;
    strlen1 : integer;

begin;
    p1 := PChar(FullName);
    strlen1 := Length(FullName);
    LoadIrriScheduleInfo_wrap(p1, strlen1);
end;

procedure LoadClimate(
            constref FullName : string;
            var ClimateDescription, TempFile, EToFile, RainFile, CO2File : string);
var
    p1, p2, p3, p4, p5, p6 : PChar;
    strlen1, strlen2, strlen3, strlen4, strlen5, strlen6 : integer;

begin;
    p1 := PChar(FullName);
    p2 := PChar(ClimateDescription);
    p3 := PChar(TempFile);
    p4 := PChar(EToFile);
    p5 := PChar(RainFile);
    p6 := PChar(CO2File);
    strlen1 := Length(FullName);
    strlen2 := Length(ClimateDescription);
    strlen3 := Length(TempFile);
    strlen4 := Length(EToFile);
    strlen5 := Length(RainFile);
    strlen6 := Length(CO2File);
    LoadClimate_wrap(p1, strlen1, p2, strlen2, p3, strlen3, p4, strlen4, p5, strlen5, p6, strlen6);
end;

procedure LoadCropCalendar(
            constref FullName : string;
            var GetOnset,GetOnsetTemp : boolean;
            var DayNrStart : integer;
            constref YearStart : integer);

var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(FullName);
    strlen := Length(FullName);
    LoadCropCalendar_wrap(p,strlen,GetOnset,GetOnsetTemp,DayNrStart,YearStart);
end;

function GetIrriAfterSeason() : rep_IrriOutSeasonEvents;
var
    i : integer;
begin;
    for i := 1 to 5 do GetIrriAfterSeason[i] := GetIrriAfterSeason_i(i)
end;


procedure SetIrriAfterSeason(constref IrriAfterSeason : rep_IrriOutSeasonEvents);
var
    i : integer;
begin;
    for i := 1 to 5 do SetIrriAfterSeason_i(i, IrriAfterSeason[i])
end;

function GetIrriAfterSeason_i(constref i : integer) : Rep_DayEventInt;
begin;
    GetIrriAfterSeason_i.DayNr := GetIrriAfterSeason_DayNr(i);
    GetIrriAfterSeason_i.Param := GetIrriAfterSeason_Param(i);
end;

procedure SetIrriAfterSeason_i(constref i : integer;
                          constref IrriAfterSeason_i : Rep_DayEventInt);
begin;
    SetIrriAfterSeason_DayNr(i, IrriAfterSeason_i.DayNr);
    SetIrriAfterSeason_Param(i, IrriAfterSeason_i.Param);
end;

function GetIrriBeforeSeason() : rep_IrriOutSeasonEvents;
var
    i : integer;
begin;
    for i := 1 to 5 do GetIrriBeforeSeason[i] := GetIrriBeforeSeason_i(i)
end;


procedure SetIrriBeforeSeason(constref IrriBeforeSeason : rep_IrriOutSeasonEvents);
var
    i : integer;
begin;
    for i := 1 to 5 do SetIrriBeforeSeason_i(i, IrriBeforeSeason[i])
end;

function GetIrriBeforeSeason_i(constref i : integer) : Rep_DayEventInt;
begin;
    GetIrriBeforeSeason_i.DayNr := GetIrriBeforeSeason_DayNr(i);
    GetIrriBeforeSeason_i.Param := GetIrriBeforeSeason_Param(i);
end;

procedure SetIrriBeforeSeason_i(constref i : integer;
                          constref IrriBeforeSeason_i : Rep_DayEventInt);
begin;
    SetIrriBeforeSeason_DayNr(i, IrriBeforeSeason_i.DayNr);
    SetIrriBeforeSeason_Param(i, IrriBeforeSeason_i.Param);
end;


function GetCompartment() : rep_Comp;
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do GetCompartment[i] := GetCompartment_i(i)
end;


procedure SetCompartment(constref Compartment : rep_Comp);
var
    i : integer;
begin;
    for i := 1 to max_No_compartments do SetCompartment_i(i, Compartment[i])
end;

function GetCompartment_i(constref i : integer) : CompartmentIndividual;
var
    i2 : integer;
begin;
    GetCompartment_i.Thickness := GetCompartment_Thickness(i);
    GetCompartment_i.Theta := GetCompartment_Theta(i);
    GetCompartment_i.fluxout := GetCompartment_fluxout(i);
    GetCompartment_i.Layer := GetCompartment_Layer(i);
    GetCompartment_i.Smax := GetCompartment_Smax(i);
    GetCompartment_i.FCadj := GetCompartment_FCadj(i);
    GetCompartment_i.DayAnaero := GetCompartment_DayAnaero(i);
    GetCompartment_i.WFactor := GetCompartment_WFactor(i);
    for i2 := 1 to 11 do GetCompartment_i.Salt[i2] := GetCompartment_Salt(i, i2);
    for i2 := 1 to 11 do GetCompartment_i.Depo[i2] := GetCompartment_Depo(i, i2);
end;

procedure SetCompartment_i(constref i : integer;
                          constref Compartment_i : CompartmentIndividual);
var
    i2 : integer;
begin;
    SetCompartment_Thickness(i, Compartment_i.Thickness);
    SetCompartment_Theta(i, Compartment_i.Theta);
    SetCompartment_fluxout(i, Compartment_i.fluxout);
    SetCompartment_Layer(i, Compartment_i.Layer);
    SetCompartment_Smax(i, Compartment_i.Smax);
    SetCompartment_FCadj(i, Compartment_i.FCadj);
    SetCompartment_DayAnaero(i, Compartment_i.DayAnaero);
    SetCompartment_WFactor(i, Compartment_i.WFactor);
    for i2 := 1 to 11 do SetCompartment_Salt(i, i2, Compartment_i.Salt[i2]);
    for i2 := 1 to 11 do SetCompartment_Depo(i, i2, Compartment_i.Depo[i2]);
end;

function GetSoilLayer() : rep_SoilLayer;
var
    i : integer;
begin;
    for i := 1 to max_SoilLayers do GetSoilLayer[i] := GetSoilLayer_i(i)
end;

function GetSoilLayer_i(constref i : integer) : SoilLayerIndividual;
begin;
    GetSoilLayer_i.Description := GetSoilLayer_Description(i);
    GetSoilLayer_i.Thickness := GetSoilLayer_Thickness(i);
    GetSoilLayer_i.SAT := GetSoilLayer_SAT(i);
    GetSoilLayer_i.FC := GetSoilLayer_FC(i);
    GetSoilLayer_i.WP := GetSoilLayer_WP(i);
    GetSoilLayer_i.tau := GetSoilLayer_tau(i);
    GetSoilLayer_i.InfRate := GetSoilLayer_InfRate(i);
    GetSoilLayer_i.Penetrability := GetSoilLayer_Penetrability(i);
    GetSoilLayer_i.GravelMass := GetSoilLayer_GravelMass(i);
    GetSoilLayer_i.GravelVol := GetSoilLayer_GravelVol(i);
    GetSoilLayer_i.WaterContent := GetSoilLayer_WaterContent(i);
    GetSoilLayer_i.Macro := GetSoilLayer_Macro(i);
    GetSoilLayer_i.SaltMobility := GetSoilLayer_SaltMobility(i);
    GetSoilLayer_i.SC := GetSoilLayer_SC(i);
    GetSoilLayer_i.SCP1 := GetSoilLayer_SCP1(i);
    GetSoilLayer_i.UL := GetSoilLayer_UL(i);
    GetSoilLayer_i.Dx := GetSoilLayer_Dx(i);
    GetSoilLayer_i.SoilClass := GetSoilLayer_SoilClass(i);
    GetSoilLayer_i.CRa := GetSoilLayer_CRa(i);
    GetSoilLayer_i.CRb := GetSoilLayer_CRb(i);
end;


function GetSoilLayer_Description(constref i : integer) : string;
var
    p : PChar;
begin;
    p := GetSoilLayer_Description_wrap(i);
    GetSoilLayer_Description := AnsiString(p);
end;

function GetSoilLayer_SaltMobility(constref i : integer) : rep_salt;
var
    i2 : integer;
begin;
    for i2 := 1 to 11 do GetSoilLayer_SaltMobility[i2] := GetSoilLayer_SaltMobility_i(i, i2)
end;

procedure SetSoilLayer(constref SoilLayer_in : rep_SoilLayer);
var
    i : integer;
begin;
    for i := 1 to max_SoilLayers do SetSoilLayer_i(i, SoilLayer_in[i])
end;

procedure SetSoilLayer_i(constref i : integer;
                        constref SoilLayer_i : SoilLayerIndividual);
begin;
    SetSoilLayer_Description(i, SoilLayer_i.Description);
    SetSoilLayer_Thickness(i, SoilLayer_i.Thickness);
    SetSoilLayer_SAT(i, SoilLayer_i.SAT);
    SetSoilLayer_FC(i, SoilLayer_i.FC);
    SetSoilLayer_WP(i, SoilLayer_i.WP);
    SetSoilLayer_tau(i, SoilLayer_i.tau);
    SetSoilLayer_InfRate(i, SoilLayer_i.InfRate);
    SetSoilLayer_Penetrability(i, SoilLayer_i.Penetrability);
    SetSoilLayer_GravelMass(i, SoilLayer_i.GravelMass);
    SetSoilLayer_GravelVol(i, SoilLayer_i.GravelVol);
    SetSoilLayer_WaterContent(i, SoilLayer_i.WaterContent);
    SetSoilLayer_Macro(i, SoilLayer_i.Macro);
    SetSoilLayer_SaltMobility(i, SoilLayer_i.SaltMobility);
    SetSoilLayer_SC(i, SoilLayer_i.SC);
    SetSoilLayer_SCP1(i, SoilLayer_i.SCP1);
    SetSoilLayer_UL(i, SoilLayer_i.UL);
    SetSoilLayer_Dx(i, SoilLayer_i.Dx);
    SetSoilLayer_SoilClass(i, SoilLayer_i.SoilClass);
    SetSoilLayer_CRa(i, SoilLayer_i.CRa);
    SetSoilLayer_CRb(i, SoilLayer_i.CRb);
end;


procedure SetSoilLayer_Description(constref i : integer;
                                   constref Description : string);
var
    p : PChar;
begin;
    p := PChar(Description);
    SetSoilLayer_Description_wrap(i, p);
end;

procedure SetSoilLayer_SaltMobility(constref i : integer;
                                    constref SaltMobility : rep_salt);
var
    i2 : integer;
begin;
    for i2 := 1 to 11 do SetSoilLayer_SaltMobility_i(i, i2, SaltMobility[i2])
end;

function GetManDescription(): string;
var
    p : PChar;

begin;
    p := GetManDescription_wrap();
    GetManDescription := AnsiString(p);
end;

procedure SetManDescription(constref str : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(str);
    strlen := Length(str);
    SetManDescription_wrap(p, strlen);
end;

procedure LoadManagement(constref FullName : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(FullName);
    strlen := Length(FullName);
    LoadManagement_wrap(p,strlen);
end;


function ActualRootingDepth(
               constref DAP,L0,LZmax,L1234,GDDL0,GDDLZmax : integer;
               constref SumGDD,Zmin,Zmax : double;
               constref ShapeFactor : ShortInt;
               constref TypeDays : rep_modeCycle) : double;
var
    int_TypeDays: integer;
begin
    int_TypeDays := ord(TypeDays);
    ActualRootingDepth := __ActualRootingDepth(DAP, L0, LZmax, L1234, GDDL0,
                                           GDDLZmax, SumGDD, Zmin, Zmax,
                                           ShapeFactor, int_TypeDays);
end;

procedure SaveCrop(constref totalname : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(totalname);
    strlen := Length(totalname);
    SaveCrop_wrap(p,strlen);
end;

procedure SaveProfile(constref totalname : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(totalname);
    strlen := Length(totalname);
    SaveProfile_wrap(p,strlen);
end;


function CanopyCoverNoStressSF(
               constref DAP,L0,L123,LMaturity,GDDL0,GDDL123,GDDLMaturity : INTEGER;
               constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
               constref TypeDays : rep_modeCycle;
               constref SFRedCGC,SFRedCCx : ShortInt) : double;
var
    int_TypeDays: integer;
begin
    int_TypeDays := ord(TypeDays);
    CanopyCoverNoStressSF := __CanopyCoverNoStressSF(DAP,L0,L123,LMaturity,
                                     GDDL0,GDDL123,GDDLMaturity, 
                                     CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD,
                                     int_TypeDays, SFRedCGC,SFRedCCx);
end;

function CCiNoWaterStressSF(Dayi,L0,L12SF,L123,L1234,
                            GDDL0,GDDL12SF,GDDL123,GDDL1234  : INTEGER;
                            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
                            SFRedCGC,SFRedCCx : ShortInt;
                            SFCDecline : Double;
                            TheModeCycle : rep_modeCycle) : double;
var
    int_TheModeCycle: integer;
begin
    int_TheModeCycle := ord(TheModeCycle);
    CCiNoWaterStressSF :=  __CCiNoWaterStressSF(Dayi,L0,L12SF,L123,L1234,GDDL0,
        GDDL12SF,GDDL123,GDDL1234, CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD,
        SFRedCGC,SFRedCCx, SFCDecline, int_TheModeCycle);
end;



procedure AdjustYearPerennials(
            constref TheYearSeason: ShortInt;
            constref Sown1stYear : BOOLEAN;
            constref TheCycleMode : rep_modeCycle;
            constref Zmax,ZminYear1,TheCCo,TheSizeSeedling, TheCGC,TheCCx,TheGDDCGC : double;
            constref ThePlantingDens : LongInt;
            VAR TypeOfPlanting : rep_planting;
            VAR Zmin,TheSizePlant,TheCCini : double;
            VAR TheDaysToCCini,TheGDDaysToCCini : INTEGER);
var
  int_cyclemode : integer;
  int_plant : integer;
begin;
  int_cyclemode := ord(TheCycleMode);
  int_plant := ord(TypeOfPlanting);
  AdjustYearPerennials_wrap(TheYearSeason, Sown1stYear,
                            int_cyclemode,Zmax,ZminYear1,TheCCo,TheSizeSeedling, 
                            TheCGC,TheCCx,TheGDDCGC, ThePlantingDens,
                            int_plant, Zmin,TheSizePlant,TheCCini,
                            TheDaysToCCini,TheGDDaysToCCini);
  TypeOfPlanting := rep_planting(int_plant);
end;


procedure LoadProfile(FullName : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(FullName);
    strlen := Length(FullName);
    LoadProfile_wrap(p,strlen);
end;


procedure LoadCrop(constref FullName : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(FullName);
    strlen := Length(FullName);
    LoadCrop_wrap(p,strlen);
end;


function GetOffSeasonDescription(): string;
var
    p : PChar;

begin;
    p := GetOffSeasonDescription_wrap();
    GetOffSeasonDescription := AnsiString(p);
end;



procedure SetOffSeasonDescription(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetOffSeasonDescription_wrap(p, strlen);
end;


procedure LoadOffSeason(constref FullName : string);
var
    p : PChar;
    strlen : integer;

begin;
    p := PChar(FullName);
    strlen := Length(FullName);
    LoadOffSeason_wrap(p,strlen);
end;


procedure LoadProgramParametersProject(constref FullFileNameProgramParameters : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(FullFileNameProgramParameters);
    strlen := Length(FullFileNameProgramParameters);
    LoadProgramParametersProject_wrap(p,strlen);
end;



procedure LoadClim(constref FullName : string;
                   var ClimateDescription : string;
                   var ClimateRecord : rep_clim);
var
    int_datatype : integer;
    FullName_ptr, ClimateDescription_ptr : PChar;
    FromString_ptr, ToString_ptr : PChar;
    strlen1, strlen2, strlen3, strlen4 : integer;
begin
    int_datatype := ord(ClimateRecord.DataType);
    FullName_ptr := PChar(FullName);
    ClimateDescription_ptr := PChar(ClimateDescription);
    strlen1 := Length(FullName);
    strlen2 := Length(ClimateDescription);
    FromString_ptr := PChar(ClimateRecord.FromString);
    ToString_ptr := PChar(ClimateRecord.ToString);
    strlen3 := Length(ClimateRecord.FromString);
    strlen4 := Length(ClimateRecord.ToString);

    LoadClim_wrap(  FullName_ptr, strlen1, 
                    ClimateDescription_ptr, strlen2, 
                    int_datatype,
                    ClimateRecord.FromD,
                    ClimateRecord.FromM,
                    ClimateRecord.FromY,
                    ClimateRecord.ToD,
                    ClimateRecord.ToM,
                    ClimateRecord.ToY,
                    ClimateRecord.FromDayNr,
                    ClimateRecord.ToDayNr,
                    FromString_ptr, strlen3,
                    ToString_ptr, strlen4,
                    ClimateRecord.NrObs);
    ClimateDescription := AnsiString(ClimateDescription_ptr);
    ClimateRecord.DataType := rep_datatype(int_datatype);
    ClimateRecord.FromString := AnsiString(FromString_ptr);
    ClimateRecord.ToString := AnsiString(ToString_ptr);
end;



function GetGroundwaterDescription(): string;
var
    p : PChar;

begin;
    p := GetGroundwaterDescription_wrap();
    GetGroundwaterDescription := AnsiString(p);
end;



procedure SetGroundwaterDescription(constref str : string);
var
    p : PChar;
    strlen : integer;
begin;
    p := PChar(str);
    strlen := Length(str);
    SetGroundwaterDescription_wrap(p, strlen);
end;



procedure CompleteClimateDescription(var ClimateRecord : rep_clim);
var
    int_datatype : integer;
    FromString_ptr, ToString_ptr : PChar;
    strlen1, strlen2 : integer;
begin
    int_datatype := ord(ClimateRecord.DataType);
    FromString_ptr := PChar(ClimateRecord.FromString);
    ToString_ptr := PChar(ClimateRecord.ToString);
    strlen1 := Length(ClimateRecord.FromString);
    strlen2 := Length(ClimateRecord.ToString);

    CompleteClimateDescription_wrap(int_datatype,
                                    ClimateRecord.FromD,
                                    ClimateRecord.FromM,
                                    ClimateRecord.FromY,
                                    ClimateRecord.ToD,
                                    ClimateRecord.ToM,
                                    ClimateRecord.ToY,
                                    ClimateRecord.FromDayNr,
                                    ClimateRecord.ToDayNr,
                                    FromString_ptr, strlen1,
                                    ToString_ptr, strlen2,
                                    ClimateRecord.NrObs);

    ClimateRecord.DataType := rep_datatype(int_datatype);
    ClimateRecord.FromString := AnsiString(FromString_ptr);
    ClimateRecord.ToString := AnsiString(ToString_ptr);
end;


function GetMultipleProjectDescription(): string;
var
     p : PChar;
begin;
     p := GetMultipleProjectDescription_wrap();
     GetMultipleProjectDescription := AnsiString(p);
end;


procedure SetMultipleProjectDescription(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetMultipleProjectDescription_wrap(p, strlen);
end;


function GetProjectDescription(): string;
var
     p : PChar;
begin;
     p := GetProjectDescription_wrap();
     GetProjectDescription := AnsiString(p);
end;


procedure SetProjectDescription(constref str : string);
var
     p : PChar;
     strlen : integer;
begin;
     p := PChar(str);
     strlen := Length(str);
     SetProjectDescription_wrap(p, strlen);
end;


initialization


end.
