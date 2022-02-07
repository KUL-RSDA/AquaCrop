unit interface_global;


interface


const
    max_SoilLayers = 5;
    max_No_compartments = 12;
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

     rep_GenerateTimeMode = (FixInt,AllDepl,AllRAW,WaterBetweenBunds);
     rep_GenerateDepthMode = (ToFC,FixDepth);

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



initialization


end.
