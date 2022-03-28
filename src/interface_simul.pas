unit interface_simul;

interface

uses Global, interface_global, TempProcessing, interface_tempprocessing;

type
    rep_control = (begin_day,end_day);
    rep_WhichTheta = (AtSAT,AtFC,AtWP,AtAct);

function GetCDCadjustedNoStressNew(
            constref CCx, CDC, CCxAdjusted): double;
     external 'aquacrop' name '__ac_simul_MOD_getcdcadjustednostressnew';

procedure AdjustpLeafToETo(
            constref EToMean : double;
            VAR pLeafULAct : double ;
            VAR pLeafLLAct : double);
     external 'aquacrop' name '__ac_simul_MOD_adjustpleaftoeto';


procedure DeterminePotentialBiomass(
            constref VirtualTimeCC : integer;
            constref cumGDDadjCC : double;
            constref CO2i : double;
            constref GDDayi : double;
            VAR CCxWitheredTpotNoS : double;
            VAR BiomassUnlim : double);
     external 'aquacrop' name '__ac_simul_MOD_determinepotentialbiomass';

procedure DetermineBiomassAndYield(
            constref dayi : integer;
            constref ETo : double;
            constref TminOnDay : double;
            constref TmaxOnDay : double;
            constref CO2i,GDDayi : double;
            constref Tact : double;
            constref SumKcTop : double;
            constref CGCref : double;
            constref GDDCGCref : double;
            constref Coeffb0 : double;
            constref Coeffb1 : double;
            constref Coeffb2 : double;
            constref FracBiomassPotSF : double;
            constref Coeffb0Salt : double;
            constref Coeffb1Salt : double;
            constref Coeffb2Salt : double;
            constref AverageSaltStress : double;
            constref SumGDDadjCC : double;
            constref CCtot : double;
            constref FracAssim : double;
            constref VirtualTimeCC :  integer;
            constref SumInterval :  integer;
            VAR Biomass : double;
            VAR BiomassPot : double;
            VAR BiomassUnlim,BiomassTot : double;
            VAR YieldPart : double;
            VAR WPi : double;
            VAR HItimesBEF : double;
            VAR ScorAT1 : double;
            VAR ScorAT2 : double;
            VAR HItimesAT1 : double;
            VAR HItimesAT2 : double;
            VAR HItimesAT : double;
            VAR alfa : double;
            VAR alfaMax : double;
            VAR SumKcTopStress : double;
            VAR SumKci : double;
            VAR CCxWitheredTpot : double;
            VAR CCxWitheredTpotNoS : double;
            VAR WeedRCi : double;
            VAR CCw : double;
            VAR Trws : double;
            VAR StressSFadjNEW : ShortInt;
            VAR PreviousStressLevel : ShortInt;
            VAR StoreAssimilates : boolean;
            VAR MobilizeAssimilates : boolean; 
            VAR AssimToMobilize : double;
            VAR AssimMobilized : double;
            VAR Bin : double;
            VAR Bout : double;
            VAR TESTVAL : double);
     external 'aquacrop' name '__ac_interface_simul_MOD_determinebiomassandyield_wrap';

procedure AdjustpStomatalToETo(
            constref MeanETo : double;
            VAR pStomatULAct : double);
     external 'aquacrop' name '__ac_simul_MOD_adjustpstomataltoeto';


procedure AdjustpSenescenceToETo(
           constref EToMean : double;
           constref TimeSenescence : double;
           constref WithBeta : BOOLEAN;
           VAR pSenAct : double);
    external 'aquacrop' name '__ac_interface_simul_MOD_adjustpsenescencetoeto_wrap';

procedure CheckGermination();
    external 'aquacrop' name '__ac_simul_MOD_checkgermination'; 

procedure calculate_transpiration(
            constref Tpot : double;
            constref Coeffb0Salt : double;
            constref Coeffb1Salt : double; 
            constref Coeffb2Salt : double);
    external 'aquacrop' name '__ac_simul_MOD_calculate_transpiration';

procedure surface_transpiration(
            constref Coeffb0Salt : double;
            constref Coeffb1Salt: double; 
            constref Coeffb2Salt: double);
    external 'aquacrop' name '__ac_simul_MOD_surface_transpiration';


//-----------------------------------------------------------------------------
// BUDGET_module
//-----------------------------------------------------------------------------


function calculate_delta_theta(
             constref theta, thetaAdjFC : double;
             constref NrLayer : integer): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_delta_theta';

function calculate_theta(
             constref delta_theta, thetaAdjFC : double;
             constref NrLayer : integer): double;
     external 'aquacrop' name '__ac_simul_MOD_calculate_theta';

procedure calculate_drainage();
     external 'aquacrop' name '__ac_simul_MOD_calculate_drainage';

procedure calculate_runoff(constref MaxDepth : double );
     external 'aquacrop' name '__ac_simul_MOD_calculate_runoff';

procedure Calculate_irrigation(var SubDrain : double;
                               var TargetTimeVal, TargetDepthVal : integer);
    external 'aquacrop' name '__ac_simul_MOD_calculate_irrigation'; 

procedure CalculateEffectiveRainfall(var SubDrain : double);
    external 'aquacrop' name '__ac_simul_MOD_calculateeffectiverainfall';

procedure calculate_CapillaryRise(VAR CRwater,CRsalt : double);
    external 'aquacrop' name '__ac_simul_MOD_calculate_capillaryrise';

procedure CheckWaterSaltBalance_wrap(
            constref dayi : integer;
            constref InfiltratedRain : double;
            constref control : integer; 
            constref InfiltratedIrrigation : double;
            constref InfiltratedStorage : double;
            var Surf0, ECInfilt, ECdrain, HorizontalWaterFlow,HorizontalSaltFlow,SubDrain: double);
    external 'aquacrop' name '__ac_simul_MOD_checkwatersaltbalance';

procedure CheckWaterSaltBalance(
            constref dayi : integer;
            constref InfiltratedRain : double;
            constref control : rep_control;
            constref InfiltratedIrrigation : double;
            constref InfiltratedStorage : double;
            var Surf0, ECInfilt, ECdrain, HorizontalWaterFlow,HorizontalSaltFlow,SubDrain: double);

procedure calculate_saltcontent(
                constref InfiltratedRain, InfiltratedIrrigation : double;
                constref InfiltratedStorage, SubDrain : double;
                constref dayi : integer);
    external 'aquacrop' name '__ac_simul_MOD_calculate_saltcontent';

procedure calculate_infiltration(
                VAR InfiltratedRain,InfiltratedIrrigation : double;
                VAR InfiltratedStorage, SubDrain : double);
    external 'aquacrop' name '__ac_simul_MOD_calculate_infiltration';

procedure calculate_Extra_runoff(VAR InfiltratedRain, InfiltratedIrrigation: double;
                                 VAR InfiltratedStorage, SubDrain : double);
    external 'aquacrop' name '__ac_simul_MOD_calculate_extra_runoff';

procedure calculate_surfacestorage(VAR InfiltratedRain,InfiltratedIrrigation: double;
                                   VAR InfiltratedStorage,ECinfilt : double;
                                   constref SubDrain : double;
                                   constref dayi : integer);
    external 'aquacrop' name '__ac_simul_MOD_calculate_surfacestorage';

procedure DetermineCCiGDD(
            constref CCxTotal, CCoTotal : double;
            var StressLeaf : double;
            constref FracAssim : double;
            constref MobilizationON, StorageON : boolean;
            constref SumGDDAdjCC : double;
            constref VirtualTimeCC : integer;
            var StressSenescence : double;
            var TimeSenescence : double;
            var NoMoreCrop : boolean;
            constref CDCTotal : double;
            var CGCAdjustmentAfterCutting : boolean;
            constref GDDayFraction, GDDayi, GDDCDCTotal : double;
            constref GDDTadj : integer);
    external 'aquacrop' name '__ac_interface_simul_MOD_determineccigdd_wrap';

procedure EffectSoilFertilitySalinityStress(
                        VAR StressSFadjNew : Shortint;
                        constref Coeffb0Salt, Coeffb1Salt, Coeffb2Salt : double;
                        constref NrDayGrow : integer;
                        constref StressTotSaltPrev : double;
                        constref VirtualTimeCC : integer);
    external 'aquacrop' name '__ac_simul_MOD_effectsoilfertilitysalinitystress';

procedure CalculateEvaporationSurfaceWater();
    external 'aquacrop' name '__ac_simul_MOD_calculateevaporationsurfacewater';

function WCEvapLayer(constref Zlayer : double;
                     constref AtTheta : rep_WhichTheta) : double;

function __WCEvapLayer(constref Zlayer : double;
                       constref AtTheta : integer) : double;
    external 'aquacrop' name '__ac_simul_MOD_wcevaplayer';

procedure PrepareStage2();
    external 'aquacrop' name '__ac_simul_MOD_preparestage2';

procedure PrepareStage1();
    external 'aquacrop' name '__ac_simul_MOD_preparestage1';

procedure AdjustEpotMulchWettedSurface(
                        constref dayi: integer;
                        constref EpotTot: double;
                        VAR Epot : double;
                        VAR EvapWCsurface : double);
    external 'aquacrop' name '__ac_simul_MOD_adjustepotmulchwettedsurface';

procedure ConcentrateSalts;
    external 'aquacrop' name '__ac_simul_MOD_concentratesalts';

procedure ExtractWaterFromEvapLayer(constref EvapToLose : double;
                                    constref Zact : double;
                                    constref Stg1 : boolean);
    external 'aquacrop' name '__ac_interface_simul_MOD_extractwaterfromevaplayer_wrap';

procedure CalculateSoilEvaporationStage1;
    external 'aquacrop' name '__ac_simul_MOD_calculatesoilevaporationstage1';

procedure CalculateSoilEvaporationStage2;
    external 'aquacrop' name '__ac_simul_MOD_calculatesoilevaporationstage2';

procedure DetermineCCi(
                constref CCxTotal, CCoTotal : double;
                var StressLeaf : double;
                constref FracAssim : double;
                constref MobilizationON, StorageON : boolean;
                constref Tadj, VirtualTimeCC : integer;
                var StressSenescence, TimeSenescence : double;
                var NoMoreCrop : boolean;
                constref CDCTotal : double;
                var CGCAdjustmentAfterCutting : boolean;
                constref DayFraction, GDDCDCTotal : double;
                var TESTVAL : double);
    external 'aquacrop' name '__ac_interface_simul_MOD_determinecci_wrap';

procedure FeedbackCC;
    external 'aquacrop' name '__ac_simul_MOD_feedbackcc';

procedure HorizontalInflowGWTable(constref DepthGWTmeter : double;
                                  constref HorizontalSaltFlow : double;
                                  constref HorizontalWaterFlow : double);
    external 'aquacrop' name '__ac_simul_MOD_horizontalinflowgwtable';


//-----------------------------------------------------------------------------
// end BUDGET_module
//-----------------------------------------------------------------------------


implementation

function WCEvapLayer(constref Zlayer : double;
                     constref AtTheta : rep_WhichTheta) : double;
var
    int_attheta : integer;
begin
    int_attheta := ord(AtTheta);
    WCEvapLayer := __WCEvapLayer(Zlayer, int_attheta);
end;


procedure CheckWaterSaltBalance(
            constref dayi : integer;
            constref InfiltratedRain : double;
            constref control : rep_control;
            constref InfiltratedIrrigation : double;
            constref InfiltratedStorage : double;
            var Surf0, ECInfilt, ECdrain, HorizontalWaterFlow,HorizontalSaltFlow,SubDrain: double);
var
  int_control : integer;
begin;
  int_control := ord(control);
  CheckWaterSaltBalance_wrap(
            dayi, InfiltratedRain, int_control,
            InfiltratedIrrigation, InfiltratedStorage,
            Surf0, ECInfilt, ECdrain, HorizontalWaterFlow,HorizontalSaltFlow,SubDrain);
end;


initialization


end.

