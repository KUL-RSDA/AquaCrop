unit interface_global;


interface


const
    max_SoilLayers = 5;
    undef_double = -9.9;
    undef_int = -9;
    max_No_compartments = 12;

type
    rep_string25 = string[25]; (* Description SoilLayer *)

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


    rep_Comp = ARRAY[1..max_No_compartments] OF CompartmentIndividual; 

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

function HarvestIndexGrowthCoefficient(
        constref HImax,dHIdt : double) : double;
        external 'aquacrop' name '__ac_global_MOD_harvestindexgrowthcoefficient';

function TauFromKsat(constref Ksat : double) : double;
         external 'aquacrop' name '__ac_global_MOD_taufromksat';

procedure CheckForWaterTableInProfile(
	    constref DepthGWTmeter : double;
            constref ProfileComp : rep_Comp;
            var WaterTableInProfile : BOOLEAN)
	    external 'aquacrop'name '__ac_global_MOD_checkforwatertableinprofile';

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

function SoilEvaporationReductionCoefficient(
            constref Wrel : double;
            constref EDecline : double) : double;
         external 'aquacrop' name '__ac_global_MOD_soilevaporationreductioncoefficient';

function CCatTime(
            constref Dayi : integer;
            constref CCoIN, CGCIN, CCxIN : double)  : double;
         external 'aquacrop' name '__ac_global_MOD_ccattime';

procedure DetermineCNIandIII(
            constref CN2 : ShortInt;
            var CN1,CN3 : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_determinecniandiii';

procedure DetermineCN_default(
            constref Infiltr : double;
            var CN2 : ShortInt);
        external 'aquacrop' name '__ac_global_MOD_determineCN_default';

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



initialization


end.
