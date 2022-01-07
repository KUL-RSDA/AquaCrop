unit interface_global;


interface


const
    max_SoilLayers = 5;
    max_No_compartments = 12;
    undef_double = -9.9;
    undef_int = -9;
    CO2Ref = 369.41;

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

    rep_SoilLayer = ARRAY[1..max_SoilLayers] of SoilLayerIndividual;

    rep_modeCycle = (GDDays, CalendarDays);

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

function fAdjustedForCO2 (
            constref CO2i, WPi : double;
            constref PercentA : ShortInt) : double;
        external 'aquacrop' name '__ac_global_MOD_fadjustedforco2';

function FullUndefinedRecord(
            constref FromY,FromD,FromM,ToD,ToM : integer) : boolean;
        external 'aquacrop' name '__ac_global_MOD_fullundefinedrecord';

function __CCiNoWaterStressSF(
            constref Dayi,L0,L12SF,L123,L1234 : integer;
            constref GDDL0,GDDL12SF,GDDL123,GDDL1234  : integer;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
            constref SFRedCGC,SFRedCCx : ShortInt;
            constref SFCDecline : double;
            constref TheModeCycle : integer) : double;
        external 'aquacrop' name '__ac_global_MOD_ccinowaterstresssf';

function CCiNoWaterStressSF(
            constref Dayi,L0,L12SF,L123,L1234 : integer;
            constref GDDL0,GDDL12SF,GDDL123,GDDL1234  : integer;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
            constref SFRedCGC,SFRedCCx : ShortInt;
            constref SFCDecline : double;
            constref TheModeCycle : rep_modeCycle) : double;

function CanopyCoverNoStressSF(
            constref DAP,L0,L123,LMaturity : integer;
            constref GDDL0,GDDL123,GDDLMaturity : integer;
            constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
            constref TypeDays : integer;
            constref SFRedCGC,SFRedCCx : ShortInt) : double;
        external 'aquacrop' name '__ac_global_MOD_canopycovernostresssf';

function CanopyCoverNoStressSF(
            constref DAP,L0,L123,LMaturity : integer;
            constref GDDL0,GDDL123,GDDLMaturity : integer;
            constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
            constref TypeDays : rep_modeCycle;
            constref SFRedCGC,SFRedCCx : ShortInt) : double;


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

function CCiNoWaterStressSF(
            constref Dayi,L0,L12SF,L123,L1234 : integer;
            constref GDDL0,GDDL12SF,GDDL123,GDDL1234  : integer;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,SumGDD,RatDGDD : double;
            constref SFRedCGC,SFRedCCx : ShortInt;
            constref SFCDecline : double;
            constref TheModeCycle : rep_modeCycle) : double;
var
    int_modeCycle: integer;

begin
    int_modeCycle := ord(TheModeCycle)
    CCiNoWaterStressSF := __CCiNoWaterStressSF(Dayi, L0, L12SF, L123, L1234,
        GDDL0, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC,
        GDDCDC, SumGDD, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline,
        int_TheModeCycle);
end;


function CanopyCoverNoStressSF(
            constref DAP,L0,L123,LMaturity : integer;
            constref GDDL0,GDDL123,GDDLMaturity : integer;
            constref CCo,CCx,CGC,CDC,GDDCGC,GDDCDC,SumGDD : double;
            constref TypeDays : rep_modeCycle;
            constref SFRedCGC,SFRedCCx : ShortInt) : double;

var
    int_modeCycle: integer;

begin
    int_modeCycle := ord(TheModeCycle)
    CanopyCoverNoStressSF := __CCiNoWaterStressSF(DAP, L0, L123, LMaturity, &
        GDDL0, GDDL123, GDDLMaturity, CCo, CCx, CGC, CDC, GDDCGC, SumGDD, &
        int_TheModeCycle, SFRedCGC, SFRedCCx);
end;


initialization


end.
