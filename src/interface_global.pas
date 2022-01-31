unit interface_global;


interface


const
    max_SoilLayers = 5;
    undef_double = -9.9;
    undef_int = -9;
    CO2Ref = 369.41;
    ElapsedDays : ARRAY[1..12] of double = (0,31,59.25,90.25,120.25,151.25,181.25,
                                                212.25,243.25,273.25,304.25,334.25);
    DaysInMonth : ARRAY[1..12] of integer = (31,28,31,30,31,30,31,31,30,31,30,31);

type
    rep_string25 = string[25]; (* Description SoilLayer *)

    rep_salt = ARRAY[1..11] of double; (* saltcontent in g/m2 *)

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

     rep_soil = Record
         REW            : ShortInt; (* Readily evaporable water mm *)
         NrSoilLayers   : ShortInt;
         CNvalue        : ShortInt;
         RootMax        : Single; // maximum rooting depth in soil profile for selected crop
         end;

     rep_Assimilates = Record
         On          : Boolean;
         Period      : INTEGER; (* Number of days at end of season during which assimilates are stored in root system *)
         Stored      : ShortInt; (* Percentage of assimilates, transferred to root system at last day of season *)
         Mobilized   : ShortInt; (* Percentage of stored assimilates, transferred to above ground parts in next season *)
         end;

    rep_SoilLayer = ARRAY[1..max_SoilLayers] of SoilLayerIndividual;
    
    rep_int_array = ARRAY[1..4] OF INTEGER;

    rep_modeCycle = (GDDays, CalendarDays);

    rep_planting = (Seed,Transplant,Regrowth);

    rep_subkind = (Vegetative,Grain,Tuber,Forage);
    rep_pMethod = (NoCorrection,FAOCorrection);

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

     rep_DayEventDbl = Record
         DayNr : Integer;
         Param : Double;
         end;
     rep_SimulationEventsDbl = ARRAY[1..31] OF Rep_DayEventDbl; // for processing 10-day monthly climatic data

     rep_GenerateTimeMode = (FixInt,AllDepl,AllRAW,WaterBetweenBunds);
     rep_GenerateDepthMode = (ToFC,FixDepth);

     rep_IrriMode = (NoIrri,Manual,Generate,Inet);
     rep_IrriMethod = (MBasin,MBorder,MDrip,MFurrow,MSprinkler);


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

function GetCrop_Length() :: rep_int_array;

function GetCrop_Length_i(constref i : integer) : double;

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

procedure SetCrop_Length(constref Length :: rep_int_array);

function SetCrop_Length_i(constref i : integer; 
                          constref Length_i : double);

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

procedure SetSoil_REW(constref REW : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_setsoil_rew';

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


implementation


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
    GetSetCrop_subkind(GetCrop_subkind();
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

initialization


end.
