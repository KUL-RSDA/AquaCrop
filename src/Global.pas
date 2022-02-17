unit Global;

interface

uses SysUtils, interface_global;


Const 
      NameMonth : ARRAY[1..12] of string = ('January','February','March','April',
          'May','June','July','August','September','October','November','December');

      EvapZmin = 15; //cm  minimum soil depth for water extraction by evaporation

TYPE
     repstring17 = string[17]; (* Date string *)
     rep_string3  = string[3];  (* Read/Write ProfFile *)

TYPE
     rep_DayEventInt = Record
         DayNr : Integer;
         Param : Integer;
         end;

     rep_IrriOutSeasonEvents = ARRAY[1..5] OF Rep_DayEventInt;

     rep_TypeObsSim =(ObsSimCC,ObsSimB,ObsSimSWC);


VAR DataPath,ObsPath : BOOLEAN;
    SWCiniFileFull,ProjectFileFull,MultipleProjectFileFull : string;
    ClimDescription,IrriDescription,
    ProjectDescription,MultipleProjectDescription,OffSeasonDescription,GroundWaterDescription: string;

    IrriFirstDayNr : LongInt;
    RootingDepth   : double;
    CCiActual,CCiPrev,CCiTopEarlySen : double;

    SenStage       : INTEGER;
    DaySubmerged   : INTEGER;
    ETo, Epot, Tpot, Rain, Irrigation, Infiltrated, CRwater : double;   (* mm/day *)
    Tmin, Tmax : double; (* degC *)
    SurfaceStorage, Runoff, Drain, Eact, Tact, TactWeedInfested : double;        (* mm/day *)
    EvapoEntireSoilSurface : BOOLEAN; // True of soil wetted by RAIN (false = IRRIGATION and fw < 1)
    PreDay         : BOOLEAN;
    Surf0          : double; (* surface water [mm] begin day *)
    NrC,NrD        : INTEGER; (* formats REAL *)
    MinReal, MaxReal : double;
    MinInt, MaxInt : INTEGER;
    IrriBeforeSeason,
    IrriAfterSeason : rep_IrriOutSeasonEvents;
    MaxPlotNew : Integer;
    MaxPlotTr : ShortInt;
    IniPercTAW : ShortInt; // Default Value for Percentage TAW for Initial Soil Water Content Menu
    // salinity
    ECstorage      : double; (* EC surface storage dS/m *)
    ECdrain        : double; (* EC drain water dS/m *)
    SaltInfiltr    : double; (* salt infiltrated in soil profile Mg/ha *)
    CRsalt         : double; // gram/m2
    ZiAqua         : Integer;  // Depth of Groundwater table below soil surface in centimeter
    ECiAqua        : double; //  EC of the groundwater table in dS/m




// Extra for stand alone procedure
    OutputAggregate : ShortInt;
    PathNameList,PathNameParam : string;
    Out1Wabal,Out2Crop,Out3Prof,Out4Salt,Out5CompWC,Out6CompEC,Out7Clim,OutDaily,
    Part1Mult,Part2Eval : BOOLEAN;

    Type
    repTypeProject = (TypePRO,TypePRM,TypeNone);


PROCEDURE CalculateETpot(DAP,L0,L12,L123,LHarvest,DayLastCut : INTEGER;
                         CCi,EToVal,KcVal,KcDeclineVal,CCx,CCxWithered,CCeffectProcent,CO2i,GDDayi,TempGDtranspLow : double;
                         VAR TpotVal, EpotVal : double);

PROCEDURE NoIrrigation;
PROCEDURE NoManagementOffSeason;
PROCEDURE LoadOffSeason(FullName : string);

PROCEDURE LoadIrriScheduleInfo(FullName : string);
PROCEDURE CalculateAdjustedFC(DepthAquifer : double;
                              VAR CompartAdj   : rep_Comp);
PROCEDURE DesignateSoilLayerToCompartments(NrCompartments,NrSoilLayers : INTEGER;
                                          VAR Compartment : rep_Comp);

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
PROCEDURE SetClimData;
PROCEDURE DetermineRootZoneWC(RootingDepth : double;
                              VAR ZtopSWCconsidered : BOOLEAN);
FUNCTION DayString(DNr : LongInt) : repstring17;


FUNCTION HarvestIndexDay(DAP  : LongInt;
                         DaysToFlower,HImax : integer;
                         dHIdt,CCi,CCxadjusted : double;
                         PercCCxHIfinal        : ShortInt;
                         TempPlanting : rep_Planting;
                         VAR PercentLagPhase : ShortInt;
                         VAR HIfinal : INTEGER)   : double;

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
                                VAR IniSurfaceStorage : double);

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

PROCEDURE GetFileForProgramParameters(TheFullFileNameProgram : string;
                                      VAR FullFileNameProgramParameters : string);
PROCEDURE LoadProgramParametersProject(FullFileNameProgramParameters : string);


implementation


PROCEDURE CalculateETpot(DAP,L0,L12,L123,LHarvest,DayLastCut : INTEGER;
                         CCi,EToVal,KcVal,KcDeclineVal,CCx,CCxWithered,CCeffectProcent,CO2i,GDDayi,TempGDtranspLow : double;
                         VAR TpotVal, EpotVal : double);
VAR EpotMin, EpotMax,CCiAdjusted,Multiplier,KsTrCold : double;
    VirtualDay : INTEGER;

BEGIN (* CalculateETpot*)
VirtualDay := DAP - GetSimulation_DelayedDays();
IF ( ((VirtualDay < L0) AND (Round(100*CCi) = 0)) OR (VirtualDay > LHarvest))   //To handlle Forage crops: Round(100*CCi) = 0
   THEN BEGIN
        TpotVal := 0;
        EpotVal := GetSimulParam_KcWetBare()*EToVal;
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
        EpotVal := GetSimulParam_KcWetBare() * (1 - CCiAdjusted) * EToVal;

        (* Maximum Epot with withered canopy as a result of (early) senescence*)
        EpotMax := GetSimulParam_KcWetBare() * EToVal * (1 - CCxWithered * CCEffectProcent/100);

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
           EpotMin := GetSimulParam_KcWetBare() * (1 - 1.72*CCx + 1*(CCx*CCx) - 0.30*(CCx*CCx*CCx)) * EToVal;
           IF (EpotMin < 0) THEN EpotMin := 0;
           IF (EpotVal < EpotMin) THEN EpotVal := EpotMin;
           IF (EpotVal > EpotMax) THEN EpotVal := EpotMax;
           END;

        (* Correction for canopy senescence before late-season stage *)
        IF GetSimulation_EvapLimitON() THEN IF (EpotVal > EpotMax) THEN EpotVal := EpotMax;

        (* Correction for drop in photosynthetic capacity of a dying green canopy *)
        IF (CCi < CCxWithered) THEN
           BEGIN
           IF (CCxWithered > 0.01) AND (CCi > 0.001)
              THEN TpotVal := TpotVal * Exp(GetSimulParam_ExpFsen()*Ln(CCi/CCxWithered));
           END;
        END;
END; (* CalculateETpot *)


PROCEDURE NoIrrigation;
VAR Nri : INTEGER;
BEGIN
 SetIrriMode(NoIrri);
 IrriDescription := 'Rainfed cropping';
 SetIrriMethod(MSprinkler);
 SetSimulation_IrriECw(0.0); // dS/m
 SetGenerateTimeMode(AllRAW);
 SetGenerateDepthMode(ToFC);
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
// mulches
SetManagement_SoilCoverBefore(0);
SetManagement_SoilCoverAfter(0);
SetManagement_EffectMulchOffS(50);
// off-season irrigation
SetSimulParam_IrriFwOffSeason(100);
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
END; (* NoManagementOffSeason *)



PROCEDURE LoadOffSeason(FullName : string);
VAR f0 : TextFile;
    Nri,NrEvents1,NrEvents2 : INTEGER;
    ParamString : string;
    Par1,Par2 : double;
    VersionNr : double;
    PreSeason_in : double;
    PostSeason_in : double;
    TempShortInt, simul_irri_of: shortint;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,OffSeasonDescription);
READLN(f0,VersionNr); // AquaCrop Version
// mulches
READLN(f0,TempShortInt);
SetManagement_SoilCoverBefore(TempShortInt);
READLN(f0,TempShortInt);
SetManagement_SoilCoverAfter(TempShortInt);
READLN(f0,TempShortInt);
SetManagement_EffectMulchOffS(TempShortInt);

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
READLN(f0,simul_irri_of); // percentage of soil surface wetted
SetSimulParam_IrriFwOffSeason(simul_irri_of);
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
    simul_irri_in,simul_percraw : shortint; 

BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,IrriDescription);
READLN(f0,VersionNr);  // AquaCrop version

// irrigation method
READLN(f0,i);
CASE i OF
     1 : SetIrriMethod(MSprinkler);
     2 : SetIrriMethod(MBasin);
     3 : SetIrriMethod(MBorder);
     4 : SetIrriMethod(MFurrow);
     else  SetIrriMethod(MDrip);
     end;

// fraction of soil surface wetted
READLN(f0,simul_irri_in);
SetSimulParam_IrriFwInSeason(simul_irri_in);

// irrigation mode and parameters
READLN(f0,i);
CASE i OF
     0 : SetIrriMode(NoIrri); // rainfed
     1 : SetIrriMode(Manual);
     2 : SetIrriMode(Generate);
     else SetIrriMode(Inet);
     end;

// 1. Irrigation schedule
IF ((i = 1) AND (ROUND(VersionNr*10) >= 70))
   THEN READLN(f0,IrriFirstDayNr) // line 6
   ELSE IrriFirstDayNr := undef_int; // start of growing period


// 2. Generate
IF (GetIrriMode() = Generate) THEN
   BEGIN
   READLN(f0,i); // time criterion
   Case i OF
        1 : SetGenerateTimeMode(FixInt);
        2 : SetGenerateTimeMode(AllDepl);
        3 : SetGenerateTimeMode(AllRAW);
        4 : SetGenerateTimeMode(WaterBetweenBunds);
        else SetGenerateTimeMode(AllRAW);
     end;
   READLN(f0,i); // depth criterion
   Case i OF
        1 : SetGenerateDepthMode(ToFc);
        else SetGenerateDepthMode(FixDepth);
     end;
   IrriFirstDayNr := undef_int; // start of growing period
   END;

// 3. Net irrigation requirement
IF (GetIrriMode() = Inet) THEN
   BEGIN
   READLN(f0,simul_percraw);
   SetSimulParam_PercRAW(simul_percraw);
   IrriFirstDayNr := undef_int;  // start of growing period
   END;

Close(f0);
END; (* LoadIrriScheduleInfo *)


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
FOR compi := 1 TO GetNrCompartments() DO Depth := Depth + CompartAdj[compi].Thickness;
compi := GetNrCompartments();
REPEAT
  Zi := Depth - CompartAdj[compi].Thickness/2;
  //Xmax := NoAdjustment(SoilLayer[CompartAdj[compi].Layer].SoilClass);
  Xmax := NoAdjustment(GetSoilLayer_i(CompartAdj[compi].Layer).FC);
  IF ((DepthAquifer < 0) OR ((DepthAquifer - Zi) >= Xmax))
      THEN BEGIN
           FOR ic := 1 to compi DO CompartAdj[ic].FCadj := GetSoilLayer_i(CompartAdj[ic].Layer).FC;
           compi := 0;
           END
      ELSE BEGIN
           IF (GetSoilLayer_i(CompartAdj[compi].Layer).FC >= GetSoilLayer_i(CompartAdj[compi].Layer).SAT)
              THEN CompartAdj[compi].FCadj := GetSoilLayer_i(CompartAdj[compi].Layer).FC
              ELSE BEGIN
                   IF (Zi >= DepthAquifer)
                      THEN CompartAdj[compi].FCadj := GetSoilLayer_i(CompartAdj[compi].Layer).SAT
                      ELSE BEGIN
                           DeltaV := GetSoilLayer_i(CompartAdj[compi].Layer).SAT - GetSoilLayer_i(CompartAdj[compi].Layer).FC;
                           DeltaFC := (DeltaV/Sqr(Xmax)) * Sqr(Zi - (DepthAquifer - Xmax));
                           CompartAdj[compi].FCadj := GetSoilLayer_i(CompartAdj[compi].Layer).FC + DeltaFC;
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
  depth := depth + GetSoilLayer_i(layeri).Thickness;
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
FOR layeri := 1 TO NrSoilLayers DO SetSoilLayer_WaterContent(layeri, 0);
FOR compi := 1 TO NrCompartments DO
    BEGIN
    Compartment[compi].Theta := GetSoilLayer_i(Compartment[compi].Layer).FC/100;
    Compartment[compi].FCadj := GetSoilLayer_i(Compartment[compi].Layer).FC;
    Compartment[compi].DayAnaero := 0;
    For celli := 1 TO GetSoilLayer_i(Compartment[compi].Layer).SCP1 DO
        BEGIN // salinity in cells
        Compartment[compi].Salt[celli] := 0.0;
        Compartment[compi].Depo[celli] := 0.0;
        END;
    SetSimulation_ThetaIni_i(compi,Compartment[compi].Theta);
    SetSimulation_ECeIni_i(compi,0); // initial soil salinity in dS/m
    SetSoilLayer_WaterContent(Compartment[compi].Layer, GetSoilLayer_i(Compartment[compi].Layer).WaterContent
        + GetSimulation_ThetaIni_i(compi)*100*10*Compartment[compi].Thickness);
    END;
FOR layeri := 1 TO NrSoilLayers DO Total := Total + GetSoilLayer_i(layeri).WaterContent;
SetTotalWaterContent_BeginDay(Total);

// initial soil water content and no salts
DeclareInitialCondAtFCandNoSalt;

// Number of days with RootZone Anaerobic Conditions
SetSimulation_DayAnaero(0);

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
IF (Comp.theta <= GetSoilLayer_i(Comp.Layer).UL)
   THEN BEGIN
        celi := 0;
        WHILE (Comp.theta > (GetSoilLayer_i(Comp.Layer).Dx) * celi) DO celi := celi + 1;
        END
   ELSE celi := GetSoilLayer_i(Comp.Layer).SCP1;
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
UL := GetSoilLayer_i(layer).UL * 100; (* upper limit in VOL% of SC cell *)

//1. convert Macro (vol%) in SaltCelNumber
IF (Macro > UL)
   THEN CelMax := GetSoilLayer_i(layer).SCP1
   ELSE CelMax := ROUND((Macro/UL)*GetSoilLayer_i(layer).SC);
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
FOR i := CelMax TO GetSoilLayer_i(layer).SCP1 DO Mobil[i] := 1;

END; (* Calculate_Saltmobility *)




PROCEDURE SaltSolutionDeposit(mm : double; (* mm = l/m2 *)
                      VAR SaltSolution,SaltDeposit : double); (* g/m2 *)
BEGIN
SaltSolution := SaltSolution + SaltDeposit;
IF (SaltSolution > GetSimulParam_SaltSolub() * mm)
    THEN BEGIN
         SaltDeposit := SaltSolution - GetSimulParam_SaltSolub() * mm;
         SaltSolution := GetSimulParam_SaltSolub() * mm;
         END
    ELSE SaltDeposit := 0;
END; (* SaltSolutionDeposit *)




PROCEDURE DetermineSaltContent(ECe : double;
                               VAR Comp : CompartmentIndividual);
VAR TotSalt, SumDF, SAT, UL, Dx, mm, mm1, mmN : double;
    celn, i : INTEGER;

BEGIN
TotSalt := ECe*Equiv*(GetSoilLayer_i(Comp.Layer).SAT)*10*Comp.Thickness;
celn := ActiveCells(Comp);
SAT := (GetSoilLayer_i(Comp.Layer).SAT)/100;  (* m3/m3 *)
UL := GetSoilLayer_i(Comp.Layer).UL; (* m3/m3 *)  (* Upper limit of SC salt cel *)
Dx := GetSoilLayer_i(Comp.Layer).Dx;  (* m3/m3 *) (* Size of salts cel (expect last one) *)
mm1 := Dx*1000*Comp.Thickness
       * (1 - GetSoilLayer_i(Comp.Layer).GravelVol/100); // g/l (* volume [mm]=[l/m2] of cells *)
mmN := (SAT-UL)*1000*Comp.Thickness
       * (1 - GetSoilLayer_i(Comp.Layer).GravelVol/100); // g/l (* volume [mm]=[l/m2] of last cell *)
SumDF := 0;
FOR i := 1 TO GetSoilLayer_i(Comp.Layer).SCP1 DO
    BEGIN
    Comp.Salt[i] := 0;
    Comp.Depo[i] := 0;
    END;
FOR i := 1 TO celn DO SumDF := SumDF + GetSoilLayer_SaltMobility_i(Comp.Layer, i);
FOR i := 1 TO celn DO
    BEGIN
    Comp.Salt[i] := TotSalt * GetSoilLayer_SaltMobility_i(Comp.Layer, i)/SumDF;
    mm := mm1;
    IF (i = GetSoilLayer_i(Comp.Layer).SCP1) THEN mm := mmN;
    SaltSolutionDeposit(mm,Comp.Salt[i],Comp.Depo[i]);
    END;
END; (* DetermineSaltContent *)




PROCEDURE CompleteProfileDescription;
VAR i : INTEGER;
TotalWaterContent_temp : rep_Content;
Compartment_temp : rep_Comp;
soillayer_i_temp : SoilLayerIndividual;
soillayer_temp : rep_SoilLayer;
BEGIN
FOR i:= (GetSoil().NrSoilLayers+1) to max_SoilLayers DO 
    BEGIN
        soillayer_i_temp := GetSoilLayer_i(i);
        set_layer_undef(soillayer_i_temp);
        SetSoilLayer_i(i, soillayer_i_temp);
    END;
SetSimulation_ResetIniSWC(true); // soil water content and soil salinity
TotalWaterContent_temp := GetTotalWaterContent();
Compartment_temp := GetCompartment();
soillayer_temp := GetSoilLayer();
specify_soil_layer(GetNrCompartments(),GetSoil().NrSoilLayers,soillayer_temp,Compartment_temp,TotalWaterContent_temp);
SetSoilLayer(soillayer_temp);
SetTotalWaterContent(TotalWaterContent_temp);
SetCompartment(Compartment_temp);
END; (* CompleteProfileDescription *)


PROCEDURE LoadProfile(FullName : string);
VAR f0 : TextFile;
    i  : INTEGER;
    blank : rep_string3;
    VersionNr : double;
    TempShortInt : shortint;
    ProfDescriptionLocal : string;
    thickness_temp, SAT_temp, FC_temp, WP_temp, infrate_temp : double;
    cra_temp, crb_temp, dx_temp : double;
    description_temp : string;
    penetrability_temp, gravelm_temp : shortint;
    saltmob_temp : rep_salt;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,ProfDescriptionLocal);
SetProfDescription(ProfDescriptionLocal);
READLN(f0,VersionNr);  // AquaCrop version
READLN(f0,TempShortInt);
SetSoil_CNvalue(TempShortInt);
READLN(f0,TempShortInt);
SetSoil_REW(TempShortInt);
SetSimulation_SurfaceStorageIni(0.0);
SetSimulation_ECStorageIni(0.0);
READLN(f0,TempShortInt);
SetSoil_NrSoilLayers(TempShortInt);
READLN(f0); // depth of restrictive soil layer which is no longer applicable
READLN(f0);
READLN(f0);
// Load characteristics of each soil layer
FOR i := 1 TO GetSoil().NrSoilLayers DO
    BEGIN
    // Parameters for capillary rise missing in Versions 3.0 and 3.1
    IF (ROUND(VersionNr*10) < 40)
       THEN BEGIN
            READLN(f0,thickness_temp,SAT_temp,FC_temp,
              WP_temp,infrate_temp,blank,description_temp);
            SetSoilLayer_Thickness(i, thickness_temp);
            SetSoilLayer_SAT(i, SAT_temp);
            SetSoilLayer_FC(i, FC_temp);
            SetSoilLayer_WP(i, WP_temp); 
            SetSoilLayer_InfRate(i, infrate_temp);
            SetSoilLayer_Description(i, description_temp);
            // Default values for Penetrability and Gravel
            SetSoilLayer_Penetrability(i, 100);
            SetSoilLayer_GravelMass(i, 0);
            // determine volume gravel
            SetSoilLayer_GravelVol(i, 0);
            END
       ELSE BEGIN
            IF (ROUND(VersionNr*10) < 60)  // UPDATE required for Version 6.0
               THEN BEGIN
                    READLN(f0,thickness_temp,SAT_temp,FC_temp, WP_temp,infrate_temp,
                           cra_temp, crb_temp,blank, description_temp);
                    SetSoilLayer_Thickness(i, thickness_temp);
                    SetSoilLayer_SAT(i, SAT_temp);
                    SetSoilLayer_FC(i, FC_temp);
                    SetSoilLayer_WP(i, WP_temp); 
                    SetSoilLayer_InfRate(i, infrate_temp);
                    SetSoilLayer_CRa(i, cra_temp);
                    SetSoilLayer_CRb(i, crb_temp);
                    SetSoilLayer_Description(i, description_temp);
                    // Default values for Penetrability and Gravel
                    SetSoilLayer_Penetrability(i, 100);
                    SetSoilLayer_GravelMass(i, 0);
                    // determine volume gravel
                    SetSoilLayer_GravelVol(i, 0);
                    END
               ELSE BEGIN
                    READLN(f0,thickness_temp,SAT_temp,FC_temp, WP_temp,infrate_temp,
                           penetrability_temp, gravelm_temp, cra_temp, crb_temp,description_temp);
                    SetSoilLayer_Thickness(i, thickness_temp);
                    SetSoilLayer_SAT(i, SAT_temp);
                    SetSoilLayer_FC(i, FC_temp);
                    SetSoilLayer_WP(i, WP_temp); 
                    SetSoilLayer_InfRate(i, infrate_temp);
                    SetSoilLayer_Penetrability(i, penetrability_temp);
                    SetSoilLayer_GravelMass(i, gravelm_temp);
                    SetSoilLayer_CRa(i, cra_temp);
                    SetSoilLayer_CRb(i, crb_temp);
                    SetSoilLayer_Description(i, description_temp);
                    // determine volume gravel
                    SetSoilLayer_GravelVol(i, FromGravelMassToGravelVolume(GetSoilLayer_i(i).SAT,GetSoilLayer_i(i).GravelMass));
                    END;
            END;
    // determine drainage coefficient
    SetSoilLayer_tau(i, TauFromKsat(GetSoilLayer_i(i).InfRate));
    // determine number of salt cells based on infiltration rate
    IF (GetSoilLayer_i(i).InfRate <= 112)
       THEN SetSoilLayer_SCP1(i, 11)
       ELSE BEGIN
            SetSoilLayer_SCP1(i, ROUND(1.6 + 1000/GetSoilLayer_i(i).InfRate));
            IF (GetSoilLayer_i(i).SCP1 < 2) THEN SetSoilLayer_SCP1(i, 2)
            END;
    // determine parameters for soil salinity
    SetSoilLayer_SC(i, GetSoilLayer_i(i).SCP1 -1);
    SetSoilLayer_Macro(i, ROUND(GetSoilLayer_i(i).FC));
    SetSoilLayer_UL(i, ((GetSoilLayer_SAT(i))/100) * (GetSoilLayer_SC(i)/(GetSoilLayer_SC(i)+2))); (* m3/m3 *)
    dx_temp := (GetSoilLayer_UL(i))/GetSoilLayer_SC(i);
    SetSoilLayer_Dx(i, dx_temp);  (* m3/m3 *)
    saltmob_temp := GetSoilLayer_i(i).SaltMobility;
    Calculate_SaltMobility(i,GetSimulParam_SaltDiff(),GetSoilLayer_i(i).Macro,saltmob_temp);
    SetSoilLayer_SaltMobility(i, saltmob_temp);
    // determine default parameters for capillary rise if missing
    SetSoilLayer_SoilClass(i, NumberSoilClass(GetSoilLayer_i(i).SAT,GetSoilLayer_i(i).FC,GetSoilLayer_i(i).WP,GetSoilLayer_i(i).InfRate));
    IF (ROUND(VersionNr*10) < 40) THEN
       BEGIN
       cra_temp := GetSoilLayer_i(i).CRa;
       crb_temp := GetSoilLayer_i(i).CRb;
       DetermineParametersCR(GetSoilLayer_i(i).SoilClass,GetSoilLayer_i(i).InfRate,cra_temp,crb_temp);
       SetSoilLayer_CRa(i, cra_temp);
       SetSoilLayer_CRb(i, crb_temp);
       END;
    END;
DetermineNrandThicknessCompartments;
Close(f0);
SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetSoil().NrSoilLayers,GetSoilLayer()));
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
    FertStress : shortint;
    RedCGC_temp, RedCCX_temp :  ShortInt;
    Crop_DaysToSenescence_temp : integer;
    Crop_Length_temp : rep_int_array ;
    Crop_DaysToFullCanopy_temp : integer;
    Crop_CGC_temp : double;
    Crop_DaysToFullCanopySF_temp : integer;

BEGIN
IF ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))
   THEN BEGIN
        IF (GetCrop().DaysToHIo > 0)
           THEN BEGIN
                IF (GetCrop().DaysToHIo > GetCrop().DaysToHarvest)
                   THEN SetCrop_dHIdt(GetCrop().HI/GetCrop().DaysToHarvest)
                   ELSE SetCrop_dHIdt(GetCrop().HI/GetCrop().DaysToHIo);
                IF (GetCrop().dHIdt > 100) THEN SetCrop_dHIdt(100);
                END
           ELSE SetCrop_dHIdt(100);
        END
   ELSE BEGIN  //  grain or tuber crops
        IF (GetCrop().DaysToHIo > 0)
           THEN SetCrop_dHIdt(GetCrop().HI/GetCrop().DaysToHIo)
           ELSE SetCrop_dHIdt(undef_int);
        END;
IF (GetCrop_ModeCycle() = CalendarDays)
   THEN BEGIN
        SetCrop_DaysToCCini(TimeToCCini(GetCrop().Planting,GetCrop().PlantingDens,GetCrop().SizeSeedling,GetCrop().SizePlant,GetCrop().CCx,GetCrop().CGC));
        SetCrop_DaysToFullCanopy(DaysToReachCCwithGivenCGC((0.98 * GetCrop().CCx),GetCrop().CCo,GetCrop().CCx,GetCrop().CGC,GetCrop().DaysToGermination));
        IF (GetManagement_FertilityStress() <> 0)
           THEN BEGIN
             FertStress := GetManagement_FertilityStress();
             Crop_DaysToFullCanopySF_temp := GetCrop().DaysToFullCanopySF;
             RedCGC_temp := GetSimulation_EffectStress_RedCGC();
             RedCCX_temp := GetSimulation_EffectStress_RedCCX();
             TimeToMaxCanopySF(GetCrop().CCo,GetCrop().CGC,GetCrop().CCx,
                  GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,
                  GetCrop().DaysToFlowering,GetCrop().LengthFlowering,GetCrop().DeterminancyLinked,
                  Crop_DaysToFullCanopySF_temp,RedCGC_temp,
                  RedCCX_temp,FertStress);
             SetManagement_FertilityStress(FertStress);
             SetSimulation_EffectStress_RedCGC(RedCGC_temp);
             SetSimulation_EffectStress_RedCCX(RedCCX_temp);
             SetCrop_DaysToFullCanopySF(Crop_DaysToFullCanopySF_temp);
            END
           ELSE SetCrop_DaysToFullCanopySF(GetCrop().DaysToFullCanopy);
        SetCrop_GDDaysToCCini(undef_int);
        SetCrop_GDDaysToGermination(undef_int);
        SetCrop_GDDaysToFullCanopy(undef_int);
        SetCrop_GDDaysToFullCanopySF(undef_int);
        SetCrop_GDDaysToFlowering(undef_int);
        SetCrop_GDDLengthFlowering(undef_int);
        SetCrop_GDDaysToSenescence(undef_int);
        SetCrop_GDDaysToHarvest(undef_int);
        SetCrop_GDDaysToMaxRooting(undef_int);
        SetCrop_GDDCGC(undef_int);
        SetCrop_GDDCDC(undef_int);
        END
   ELSE BEGIN
        SetCrop_GDDaysToCCini(TimeToCCini(GetCrop().Planting,GetCrop().PlantingDens,GetCrop().SizeSeedling,GetCrop().SizePlant,GetCrop().CCx,GetCrop().GDDCGC));
        SetCrop_DaysToCCini(TimeToCCini(GetCrop().Planting,GetCrop().PlantingDens,GetCrop().SizeSeedling,GetCrop().SizePlant,GetCrop().CCx,GetCrop().CGC));
        SetCrop_GDDaysToFullCanopy(DaysToReachCCwithGivenCGC((0.98 * GetCrop().CCx),GetCrop().CCo,GetCrop().CCx,GetCrop().GDDCGC,GetCrop().GDDaysToGermination));
        //Crop.GDDaysToFullCanopySF is determined in RUN or ManagementUnit if required
        END;

CGCisGiven := true; // required to adjust Crop.DaysToFullCanopy (does not exist)
Crop_DaysToSenescence_temp := GetCrop().DaysToSenescence;
Crop_Length_temp := GetCrop().Length;
Crop_DaysToFullCanopy_temp := GetCrop().DaysToFullCanopy;
Crop_CGC_temp := GetCrop().CGC;
DetermineLengthGrowthStages(GetCrop().CCo,GetCrop().CCx,GetCrop().CDC,GetCrop().DaysToGermination,GetCrop().DaysToHarvest,CGCisGiven,
                            GetCrop().DaysToCCini,GetCrop().Planting,Crop_DaysToSenescence_temp,
                            Crop_Length_temp,Crop_DaysToFullCanopy_temp,Crop_CGC_temp);
SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp);
SetCrop_Length(Crop_Length_temp);
SetCrop_DaysToFullCanopy(Crop_DaysToFullCanopy_temp);
SetCrop_CGC(Crop_CGC_temp);

SetCrop_CCoAdjusted(GetCrop().CCo);
SetCrop_CCxAdjusted(GetCrop().CCx);
SetCrop_CCxWithered(GetCrop().CCx);
SetSumWaBal_Biomass(0);
SetSumWaBal_BiomassPot(0);
SetSumWaBal_BiomassUnlim(0);
SetSumWaBal_BiomassTot(0); // crop and weeds (for soil fertility stress)
SetSumWaBal_YieldPart(0);
SetSimulation_EvapLimitON(false);
END; (* CompleteCropDescription *)




PROCEDURE LoadCrop (FullName : string);
VAR f0 : TextFile;
    XX, YY : INTEGER;
    VersionNr : double;
    TempShortInt,perenperiod_onsetOcc_temp,perenperiod_endOcc_temp : shortint;
    TempInt,perenperiod_onsetFD_temp,perenperiod_onsetFM_temp,perenperiod_onsetLSP_temp,
                                perenperiod_onsetPV_temp,perenperiod_endLD_temp,
                                perenperiod_endLM_temp,perenperiod_extrayears_temp,
                                perenperiod_endLSP_temp,perenperiod_endPV_temp : integer;
    TempDouble : double;
    TempBoolean : boolean;
    Crop_SmaxBot_temp,perenperiod_onsetTV_temp,perenperiod_endTV_temp : double;
    Crop_SmaxTop_temp : double;
    CropDescriptionLocal : string;
BEGIN
Assign(f0,FullName);
Reset(f0);
READLN(f0,CropDescriptionLocal);
SetCropDescription(CropDescriptionLocal);
READLN(f0,VersionNr);  // AquaCrop version
READLN(f0);  // Protected or Open file

//subkind
READLN(f0,XX);
CASE XX of
     1 : SetCrop_subkind(Vegetative);
     2 : SetCrop_subkind(Grain);
     3 : SetCrop_subkind(Tuber);
     4 : SetCrop_subkind(Forage);
     end;

// type of planting
READLN(f0,XX);
CASE XX of
     1 : SetCrop_Planting(Seed);
     0 : SetCrop_Planting(Transplant);
    -9 : SetCrop_Planting(Regrowth)
     else SetCrop_Planting(Seed);
     end;

//mode
READLN(f0,XX);
IF (XX = 0) THEN SetCrop_ModeCycle(GDDays)
            ELSE SetCrop_ModeCycle(CalendarDays);

//adjustment p to ETo
READLN(f0,YY);
IF (YY = 0) THEN SetCrop_pMethod(NoCorrection)
            ELSE IF (YY = 1) THEN SetCrop_pMethod(FAOCorrection);

//temperatures controlling crop development
READLN(f0,TempDouble);
SetCrop_Tbase(TempDouble);
READLN(f0,TempDouble);
SetCrop_Tupper(TempDouble);

// required growing degree days to complete the crop cycle (is identical as to maturity)
READLN(f0,TempInt);
SetCrop_GDDaysToHarvest(TempInt);

// water stress
READLN(f0,TempDouble);
SetCrop_pLeafDefUL(TempDouble);
READLN(f0,TempDouble);
SetCrop_pLeafDefLL(TempDouble);
READLN(f0,TempDouble);
SetCrop_KsShapeFactorLeaf(TempDouble);
READLN(f0,TempDouble);
SetCrop_pdef(TempDouble);
READLN(f0,TempDouble);
SetCrop_KsShapeFactorStomata(TempDouble);
READLN(f0,TempDouble);
SetCrop_pSenescence(TempDouble);
READLN(f0,TempDouble);
SetCrop_KsShapeFactorSenescence(TempDouble);
READLN(f0,TempInt);
SetCrop_SumEToDelaySenescence(TempInt);
READLN(f0,TempDouble);
SetCrop_pPollination(TempDouble);
READLN(f0,TempInt);
SetCrop_AnaeroPoint(TempInt);

// soil fertility/salinity stress
READLN(f0,TempShortInt);    //Soil fertility stress at calibration (%)
SetCrop_StressResponse_Stress(TempShortInt);
READLN(f0,TempDouble);  //Shape factor for the response of Canopy Growth Coefficient to soil fertility/salinity stress
SetCrop_StressResponse_ShapeCGC(TempDouble);
READLN(f0,TempDouble);  //Shape factor for the response of Maximum Canopy Cover to soil fertility/salinity stress
SetCrop_StressResponse_ShapeCCX(TempDouble);
READLN(f0,TempDouble);   //Shape factor for the response of Crop Water Producitity to soil fertility stress
SetCrop_StressResponse_ShapeWP(TempDouble);
READLN(f0,TempDouble);  //Shape factor for the response of Decline of Canopy Cover to soil fertility/salinity stress
SetCrop_StressResponse_ShapeCDecline(TempDouble);
// extra response factor for salinity stress (Version 4.0 and higher)
// -----  UPDATE response factor
(*
IF (ROUND(VersionNr*10) < 40)  // UPDATE required for Version 3.0 and 3.1
   THEN SetCrop_StressResponse_ShapeKsSto(GetCrop_StressResponse().ShapeWP)
   ELSE READLN(f0,SetCrop_StressResponse_ShapeKsSto();  //Shape factor for the response of Stomatal Closure to soil salinity stress
     *)
IF (ROUND(VersionNr*10) >= 40)  // UPDATE required for Version 4.0 and next
   THEN READLN(f0);  //Shape factor for the response of Stomatal Closure to soil salinity stress NO LONGER VALID
// continue with soil fertility/salinity stress
IF ((GetCrop_StressResponse().ShapeCGC > 24.9) AND (GetCrop_StressResponse().ShapeCCX > 24.9)
    AND (GetCrop_StressResponse().ShapeWP > 24.9) AND (GetCrop_StressResponse().ShapeCDecline > 24.9))
  THEN SetCrop_StressResponse_Calibrated(false)
  ELSE SetCrop_StressResponse_Calibrated(true);

// temperature stress
READLN(f0,TempShortInt); //Minimum air temperature below which pollination starts to fail (cold stress) (degC)
SetCrop_Tcold(TempShortInt);
READLN(f0,TempShortInt); //Maximum air temperature above which pollination starts to fail (heat stress) (degC)
SetCrop_Theat(TempShortInt);
READLN(f0,TempDouble); //Minimum growing degrees required for full biomass production (degC - day)
SetCrop_GDtranspLow(TempDouble);

// salinity stress (Version 3.2 and higher)
// -----  UPDATE salinity stress
IF (ROUND(VersionNr*10) < 32)  // UPDATE required for Version 3.0 and 3.1
   THEN BEGIN
        SetCrop_ECemin(2); // upper threshold ECe
        SetCrop_ECemax(15); // lower threhsold ECe
        END
   ELSE BEGIN
        READLN(f0,TempShortInt); // upper threshold ECe
        SetCrop_ECemin(TempShortInt); // upper threshold ECe
        READLN(f0,TempShortInt); // lower threhsold ECe
        SetCrop_ECemax(TempShortInt); // upper threshold ECe
        READLN(f0); // WAS shape factor of the Ks(salinity) - soil saturation extract (ECe) relationship
        END;
// -----  UPDATE salinity stress (Version 5.1 and higher)
IF (ROUND(VersionNr*10) < 51)  // UPDATE required for previous versions
   THEN BEGIN
        SetCrop_CCsaltDistortion(25); //distortion canopy cover for simulation of effect of salinity stress (%)
        SetCrop_ResponseECsw(100); //Response of Ks stomata to ECsw: From 0 (none) to +200 (very strong)
        END
   ELSE BEGIN
        READLN(f0,TempShortInt);
        SetCrop_CCsaltDistortion(TempShortInt);
        READLN(f0,TempInt);
        SetCrop_ResponseECsw(TempInt);
        END;

//evapotranspiration
READLN(f0,TempDouble);
SetCrop_KcTop(TempDouble);
READLN(f0,TempDouble);
SetCrop_KcDecline(TempDouble);
READLN(f0,TempDouble);
SetCrop_RootMin(TempDouble);
READLN(f0,TempDouble);
SetCrop_RootMax(TempDouble);
IF (GetCrop().RootMin > GetCrop().RootMax) THEN SetCrop_RootMin(GetCrop().RootMax); //security for sine function
READLN(f0,TempShortInt);
SetCrop_RootShape(TempShortInt);
READLN(f0,TempDouble);
SetCrop_SmaxTopQuarter(TempDouble);
READLN(f0,TempDouble);
SetCrop_SmaxBotQuarter(TempDouble);
Crop_SmaxTop_temp := GetCrop().SmaxTop;
Crop_SmaxBot_temp := GetCrop().SmaxBot;
DeriveSmaxTopBottom(GetCrop().SmaxTopQuarter,GetCrop().SmaxBotQuarter,Crop_SmaxTop_temp,Crop_SmaxBot_temp);
SetCrop_SmaxTop(Crop_SmaxTop_temp);
SetCrop_SmaxBot(Crop_SmaxBot_temp);
READLN(f0,TempInt);
SetCrop_CCEffectEvapLate(TempInt);

//crop development
READLN(f0,TempDouble);
SetCrop_SizeSeedling(TempDouble);
IF (ROUND(VersionNr*10) < 50)  // UPDATE required for Version not yet 5.0
   THEN SetCrop_SizePlant(GetCrop().SizeSeedling)
   ELSE BEGIN 
       READLN(f0,TempDouble); // Canopy size of individual plant (re-growth) at 1st day (cm2)
       SetCrop_SizePlant(TempDouble);
       END;
READLN(f0,TempInt);
SetCrop_PlantingDens(TempInt);
SetCrop_CCo((GetCrop().PlantingDens/10000) * (GetCrop().SizeSeedling/10000));
SetCrop_CCini((GetCrop().PlantingDens/10000) * (GetCrop().SizePlant/10000));
READLN(f0,TempDouble);
SetCrop_CGC(TempDouble);

READLN(f0,TempShortInt); // Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials
SetCrop_YearCCx(TempShortInt);
READLN(f0,TempDouble); // Shape factor of the decline of CCx over the years due to self-thinning - for Perennials
SetCrop_CCxRoot(TempDouble);
//READLN(f0,CGCdx);  removed as crop parameter
//READLN(f0,CGCns);  removed as crop parameter
READLN(f0);  //READLN(f0,CGCroot);  removed as crop parameter

READLN(f0,TempDouble);
SetCrop_CCx(TempDouble);
READLN(f0,TempDouble);
SetCrop_CDC(TempDouble);
READLN(f0,TempInt);
SetCrop_DaysToGermination(TempInt);
READLN(f0,TempInt);
SetCrop_DaysToMaxRooting(TempInt);
READLN(f0,TempInt);
SetCrop_DaysToSenescence(TempInt);
READLN(f0,TempInt);
SetCrop_DaysToHarvest(TempInt);
READLN(f0,TempInt);
SetCrop_DaysToFlowering(TempInt);
READLN(f0,TempInt);
SetCrop_LengthFlowering(TempInt);
// -----  UPDATE crop development for Version 3.1
// leafy vegetable crop has an Harvest Index which builds up starting from sowing
IF ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))  THEN
   BEGIN
   SetCrop_DaysToFlowering(0);
   SetCrop_LengthFlowering(0);
   END;

// Crop.DeterminancyLinked
READLN(f0,XX);
CASE XX of
     1 : SetCrop_DeterminancyLinked(true);
     else SetCrop_DeterminancyLinked(false);
     end;

// Potential excess of fruits (%) and building up HI
IF ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))
   THEN BEGIN
        READLN(f0);  // PercCycle no longer considered
        SetCrop_fExcess(undef_int);
        END
   ELSE BEGIN
        READLN(f0,TempInt);
        SetCrop_fExcess(TempInt);
        END;
READLN(f0,TempInt);
SetCrop_DaysToHIo(TempInt);

// yield response to water
READLN(f0,TempDouble);
SetCrop_WP(TempDouble);
READLN(f0,TempInt);
SetCrop_WPy(TempInt);
// adaptation to elevated CO2 (Version 3.2 and higher)
// -----  UPDATE Crop performance under elevated atmospheric CO2 concentration (%)
IF (ROUND(VersionNr*10) < 32)  // UPDATE required for Version 3.0 and 3.1
   THEN SetCrop_AdaptedToCO2(50)
   ELSE BEGIN
        READLN(f0,TempShortInt);
        SetCrop_AdaptedToCO2(TempShortInt);
        END;
READLN(f0,TempInt);
SetCrop_HI(TempInt);
READLN(f0,TempShortInt);
SetCrop_HIincrease(TempShortInt); // possible increase (%) of HI due to water stress before flowering
READLN(f0,TempDouble);
SetCrop_aCoeff(TempDouble); // coefficient describing impact of restricted vegetative growth at flowering on HI
READLN(f0,TempDouble);
SetCrop_bCoeff(TempDouble); // coefficient describing impact of stomatal closure at flowering on HI
READLN(f0,TempShortInt);
SetCrop_DHImax(TempShortInt); // allowable maximum increase (%) of specified HI
// -----  UPDATE yield response to water for Version 3.1
// leafy vegetable crop has an Harvest Index (default is 85 %)
IF ((ROUND(VersionNr*10) = 30) AND ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage)))
   THEN IF (ROUND(GetCrop().HI) = undef_int) THEN SetCrop_HI(85);

// growing degree days
READLN(f0,TempInt);
SetCrop_GDDaysToGermination(TempInt);
READLN(f0,TempInt);
SetCrop_GDDaysToMaxRooting(TempInt);
READLN(f0,TempInt);
SetCrop_GDDaysToSenescence(TempInt);
READLN(f0,TempInt);
SetCrop_GDDaysToHarvest(TempInt);
READLN(f0,TempInt);
SetCrop_GDDaysToFlowering(TempInt);
READLN(f0,TempInt);
SetCrop_GDDLengthFlowering(TempInt);
READLN(f0,TempDouble);
SetCrop_GDDCGC(TempDouble);
READLN(f0,TempDouble);
SetCrop_GDDCDC(TempDouble);
READLN(f0,TempInt);
SetCrop_GDDaysToHIo(TempInt);

// -----  UPDATE yield response to water for Version 3.1
// leafy vegetable crop has an Harvest Index which builds up starting from sowing
IF ((GetCrop_ModeCycle() = GDDays) AND ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))) THEN
   BEGIN
   SetCrop_GDDaysToFlowering(0);
   SetCrop_GDDLengthFlowering(0);
   END;

// extra version 6.2
IF (ROUND(VersionNr*10) < 62)  // UPDATE required for Version 6.2
   THEN SetCrop_DryMatter(undef_int) // undefined
   ELSE BEGIN
        READLN(f0,TempShortInt);
        SetCrop_DryMatter(TempShortInt); // dry matter content (%) of fresh yield
        END;

// extra version 7.0
IF (ROUND(VersionNr*10) < 62)  // UPDATE required for Version 7.0
   THEN BEGIN
        SetCrop_RootMinYear1(GetCrop().RootMin); // regrowth not yet possible
        TempBoolean := GetCrop().Planting = Seed;
        SetCrop_SownYear1(TempBoolean);  // type of planting first year
        // transfer of assimilates
        SetCrop_Assimilates_On(false); // Transfer of assimilates between root system and above ground parts is NOT considered
        SetCrop_Assimilates_Period(0);
        SetCrop_Assimilates_Stored(0);
        SetCrop_Assimilates_Mobilized(0);
        END
   ELSE BEGIN
        READLN(f0,TempDouble); 
        SetCrop_RootMinYear1(TempDouble); // Minimum rooting depth in first year in meter (for regrowth)
        READLN(f0,XX);
        CASE XX of
              1 : SetCrop_SownYear1(true);  // crop is sown in 1 st year (for perennials)
             else SetCrop_SownYear1(false); // crop is transplanted in 1st year (for regrowth)
             end;
        // transfer of assimilates
        READLN(f0,XX);
        CASE XX of
              1 : SetCrop_Assimilates_On(true);  // Transfer of assimilates from above ground parts to root system is considered
             else SetCrop_Assimilates_On(false); // Transfer of assimilates from above ground parts to root system is NOT considered
             end;
        READLN(f0,TempInt);
        SetCrop_Assimilates_Period(TempInt); // Number of days at end of season during which assimilates are stored in root system
        READLN(f0,TempShortInt);
        SetCrop_Assimilates_Stored(TempShortInt); // Percentage of assimilates, transferred to root system at last day of season
        READLN(f0,TempShortInt);
        SetCrop_Assimilates_Mobilized(TempShortInt); // Percentage of stored assimilates, transferred to above ground parts in next season
        END;

IF (GetCrop_subkind() = Forage) THEN
   BEGIN // data for the determination of the growing period
   // 1. Title
   For XX := 1 to 3 DO READLN(f0);
   // 2. ONSET
   READLN(f0,XX);
   IF (XX = 0)
      THEN SetPerennialPeriod_GenerateOnset(false) // onset is fixed on a specific day
      ELSE BEGIN // onset is generated by an air temperature criterion
           SetPerennialPeriod_GenerateOnset(true);
           CASE XX OF
             12 : SetPerennialPeriod_OnsetCriterion(TMeanPeriod); // Criterion: mean air temperature
             13 : SetPerennialPeriod_OnsetCriterion(GDDPeriod); // Criterion: growing-degree days
             else SetPerennialPeriod_GenerateOnset(false);
             end;
           END;
   READLN(f0,perenperiod_onsetFD_temp);
   SetPerennialPeriod_OnsetFirstDay(perenperiod_onsetFD_temp);
   READLN(f0,perenperiod_onsetFM_temp);
   SetPerennialPeriod_OnsetFirstMonth(perenperiod_onsetFM_temp); 
   READLN(f0,perenperiod_onsetLSP_temp);
   SetPerennialPeriod_OnsetLengthSearchPeriod(perenperiod_onsetLSP_temp);
   READLN(f0,perenperiod_onsetTV_temp); // Mean air temperature or Growing-degree days
   SetPerennialPeriod_OnsetThresholdValue(perenperiod_onsetTV_temp);
   READLN(f0,perenperiod_onsetPV_temp); // number of succesive days
   SetPerennialPeriod_OnsetPeriodValue(perenperiod_onsetPV_temp);
   READLN(f0,perenperiod_onsetOcc_temp);  // number of occurrence
   SetPerennialPeriod_OnsetOccurrence(perenperiod_onsetOcc_temp);
   IF (GetPerennialPeriod_OnsetOccurrence() > 3) THEN SetPerennialPeriod_OnsetOccurrence(3);
   // 3. END of growing period
   READLN(f0,XX);
   IF (XX = 0)
      THEN SetPerennialPeriod_GenerateEnd(false)  // end is fixed on a specific day
      ELSE BEGIN // end is generated by an air temperature criterion
           SetPerennialPeriod_GenerateEnd(true);
           CASE XX OF
             62 : SetPerennialPeriod_EndCriterion(TMeanPeriod); // Criterion: mean air temperature
             63 : SetPerennialPeriod_EndCriterion(GDDPeriod); // Criterion: growing-degree days
             else SetPerennialPeriod_GenerateEnd(false);
             end;
           END;
   READLN(f0,perenperiod_endLD_temp);
   SetPerennialPeriod_EndLastDay(perenperiod_endLD_temp);
   READLN(f0,perenperiod_endLM_temp);
   SetPerennialPeriod_EndLastMonth(perenperiod_endLM_temp);
   READLN(f0,perenperiod_extrayears_temp);
   SetPerennialPeriod_ExtraYears(perenperiod_extrayears_temp);
   READLN(f0,perenperiod_endLSP_temp);
   SetPerennialPeriod_EndLengthSearchPeriod(perenperiod_endLSP_temp);
   READLN(f0,perenperiod_endTV_temp); // Mean air temperature or Growing-degree days
   SetPerennialPeriod_EndThresholdValue(perenperiod_endTV_temp);
   READLN(f0,perenperiod_endPV_temp); // number of succesive days
   SetPerennialPeriod_EndPeriodValue(perenperiod_endPV_temp); 
   READLN(f0,perenperiod_endOcc_temp); // number of occurrence
   SetPerennialPeriod_EndOccurrence(perenperiod_endOcc_temp);
   IF (GetPerennialPeriod_EndOccurrence() > 3) THEN SetPerennialPeriod_EndOccurrence(3);
   END;
Close(f0);
// maximum rooting depth in given soil profile
SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetSoil().NrSoilLayers,GetSoilLayer()));

// copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(GetCrop().DaysToHarvest - GetCrop().DaysToSenescence);
SetCropFileSet_DaysToHarvest(GetCrop().DaysToHarvest);
IF (GetCrop_ModeCycle() = GDDays)
   THEN BEGIN
        SetCropFileSet_GDDaysFromSenescenceToEnd(GetCrop().GDDaysToHarvest - GetCrop().GDDaysToSenescence);
        SetCropFileSet_GDDaysToHarvest(GetCrop().GDDaysToHarvest);
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
WRITELN(f,GetProfDescription());
WRITELN(f,'        7.0                 : AquaCrop Version (June 2021)');    // AquaCrop version
WRITELN(f,GetSoil().CNvalue:9,'                   : CN (Curve Number)');
WRITELN(f,GetSoil().REW:9,'                   : Readily evaporable water from top layer (mm)');
WRITELN(f,GetSoil().NrSoilLayers:9,'                   : number of soil horizons');
WRITELN(f,undef_int:9,'                   : variable no longer applicable');
WRITELN(f,'  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description');
WRITELN(f,'  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------');
FOR i := 1 TO GetSoil().NrSoilLayers DO
    WRITELN(f,GetSoilLayer_i(i).Thickness:8:2,GetSoilLayer_i(i).SAT:8:1,GetSoilLayer_i(i).FC:6:1,
              GetSoilLayer_i(i).WP:6:1,GetSoilLayer_i(i).InfRate:8:1,GetSoilLayer_i(i).Penetrability:11,
              GetSoilLayer_i(i).GravelMass:10,GetSoilLayer_i(i).CRa:14:6,GetSoilLayer_i(i).CRb:10:6,
              '   ',GetSoilLayer_i(i).Description:15);
Close(f);

// maximum rooting depth in  soil profile for given crop
SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetSoil().NrSoilLayers,GetSoilLayer()));
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
WRITELN(f,GetCropDescription());
// AquaCrop version
WRITELN(f,'     7.0       : AquaCrop Version (June 2021)');
WRITELN(f,'     1         : File not protected');

//SubKind
i := 2;
CASE GetCrop_subkind() OF
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
IF (GetCrop().Planting = Seed)
   THEN BEGIN
        i := 1;
        IF (GetCrop_subkind() = Forage)
           THEN WRITELN(f,i:6,'         : Crop is sown in 1st year')
           ELSE WRITELN(f,i:6,'         : Crop is sown');
        END
   ELSE BEGIN
        IF (GetCrop().Planting = Transplant)
           THEN BEGIN
                i := 0;
                IF (GetCrop_subkind() = Forage)
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
IF (GetCrop_ModeCycle() = GDDays) THEN
   BEGIN
   i := 0;
   TempString := '         : Determination of crop cycle : by growing degree-days';
   END;
WRITELN(f,i:6,TempString);

//p correction for ET
IF (GetCrop().pMethod = NoCorrection)
   THEN BEGIN
        j := 0;
        WRITELN(f,j:6,'         : No adjustment by ETo of soil water depletion factors (p)');
        END
   ELSE BEGIN
        j := 1;
        WRITELN(f,j:6,'         : Soil water depletion factors (p) are adjusted by ETo');
        END;

// temperatures controlling crop development
WRITELN(f,GetCrop().Tbase:8:1,'       : Base temperature (degC) below which crop development does not progress');
WRITELN(f,GetCrop().Tupper:8:1,'       : Upper temperature (degC) above which crop development no longer increases with an increase in temperature');

// required growing degree days to complete the crop cycle (is identical as to maturity)
WRITELN(f,GetCrop().GDDaysToHarvest:6,'         : Total length of crop cycle in growing degree-days');

// water stress
WRITELN(f,GetCrop().pLeafDefUL:9:2,'      : Soil water depletion factor for canopy expansion (p-exp) - Upper threshold');
WRITELN(f,GetCrop().pLeafDefLL:9:2,'      : Soil water depletion factor for canopy expansion (p-exp) - Lower threshold');
WRITELN(f,GetCrop().KsShapeFactorLeaf:8:1,'       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)');
WRITELN(f,GetCrop().pdef:9:2,'      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold');
WRITELN(f,GetCrop().KsShapeFactorStomata:8:1,'       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)');
WRITELN(f,GetCrop().pSenescence:9:2,'      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold');
WRITELN(f,GetCrop().KsShapeFactorSenescence:8:1,'       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)');
WRITELN(f,GetCrop().SumEToDelaySenescence:6,'         : Sum(ETo) during dormant period to be exceeded before crop is permanently wilted');
IF (GetCrop().pPollination = undef_int)
   THEN WRITELN(f,GetCrop().pPollination:9:2,'      : Soil water depletion factor for pollination - Not Applicable')
   ELSE WRITELN(f,GetCrop().pPollination:9:2,'      : Soil water depletion factor for pollination (p - pol) - Upper threshold');
WRITELN(f,GetCrop().AnaeroPoint:6,'         : Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)');

// stress response
WRITELN(f,GetCrop_StressResponse().Stress:6,'         : Considered soil fertility stress for calibration of stress response (%)');
IF (GetCrop_StressResponse().ShapeCGC > 24.9)
   THEN WRITELN(f,GetCrop_StressResponse().ShapeCGC:9:2,'      : Response of canopy expansion is not considered')
   ELSE WRITELN(f,GetCrop_StressResponse().ShapeCGC:9:2,'      : Shape factor for the response of canopy expansion to soil fertility stress');
IF (GetCrop_StressResponse().ShapeCCX > 24.9)
   THEN WRITELN(f,GetCrop_StressResponse().ShapeCCX:9:2,'      : Response of maximum canopy cover is not considered')
   ELSE WRITELN(f,GetCrop_StressResponse().ShapeCCX:9:2,'      : Shape factor for the response of maximum canopy cover to soil fertility stress');
IF (GetCrop_StressResponse().ShapeWP > 24.9)
   THEN WRITELN(f,GetCrop_StressResponse().ShapeWP:9:2,'      : Response of crop Water Productivity is not considered')
   ELSE WRITELN(f,GetCrop_StressResponse().ShapeWP:9:2,'      : Shape factor for the response of crop Water Productivity to soil fertility stress');
IF (GetCrop_StressResponse().ShapeCDecline > 24.9)
   THEN WRITELN(f,GetCrop_StressResponse().ShapeCDecline:9:2,'      : Response of decline of canopy cover is not considered')
   ELSE WRITELN(f,GetCrop_StressResponse().ShapeCDecline:9:2,'      : Shape factor for the response of decline of canopy cover to soil fertility stress');
WRITELN(f,'    -9         : dummy - Parameter no Longer required');

// temperature stress
IF (Round(GetCrop().Tcold) = undef_int)
   THEN WRITELN(f,GetCrop().Tcold:6,'         : Cold (air temperature) stress affecting pollination - not considered')
   ELSE WRITELN(f,GetCrop().Tcold:6,'         : Minimum air temperature below which pollination starts to fail (cold stress) (degC)');
IF (Round(GetCrop().Theat) = undef_int)
   THEN WRITELN(f,GetCrop().Theat:6,'         : Heat (air temperature) stress affecting pollination - not considered')
   ELSE WRITELN(f,GetCrop().Theat:6,'         : Maximum air temperature above which pollination starts to fail (heat stress) (degC)');
IF (Round(GetCrop().GDtranspLow) = undef_int)
   THEN WRITELN(f,GetCrop().GDtranspLow:8:1,'       : Cold (air temperature) stress on crop transpiration not considered')
   ELSE WRITELN(f,GetCrop().GDtranspLow:8:1,'       : Minimum growing degrees required for full crop transpiration (degC - day)');

// salinity stress
WRITELN(f,GetCrop().ECemin:6,'         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)');
WRITELN(f,GetCrop().ECemax:6,'         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)');
WRITELN(f,'    -9         : Dummy - no longer applicable'); // shape factor Ks(salt)-ECe
WRITELN(f,GetCrop().CCsaltDistortion:6,'         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))');
WRITELN(f,GetCrop().ResponseECsw:6,'         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))');

//evapotranspiration
WRITELN(f,GetCrop().KcTop:9:2,'      : Crop coefficient when canopy is complete but prior to senescence (KcTr,x)');
WRITELN(f,GetCrop().KcDecline:10:3,'     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.');
WRITELN(f,GetCrop().RootMin:9:2,'      : Minimum effective rooting depth (m)');
WRITELN(f,GetCrop().RootMax:9:2,'      : Maximum effective rooting depth (m)');
WRITELN(f,GetCrop().RootShape:6,'         : Shape factor describing root zone expansion');
WRITELN(f,GetCrop().SmaxTopQuarter:10:3,'     : Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone');
WRITELN(f,GetCrop().SmaxBotQuarter:10:3,'     : Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone');
WRITELN(f,GetCrop().CCEffectEvapLate:6,'         : Effect of canopy cover in reducing soil evaporation in late season stage');

//canopy development
WRITELN(f,GetCrop().SizeSeedling:9:2,'      : Soil surface covered by an individual seedling at 90 % emergence (cm2)');
WRITELN(f,GetCrop().SizePlant:9:2,'      : Canopy size of individual plant (re-growth) at 1st day (cm2)');
WRITELN(f,GetCrop().PlantingDens:9,'      : Number of plants per hectare');
WRITELN(f,GetCrop().CGC:12:5,'   : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)');
IF (GetCrop().YearCCx = undef_int)
   THEN WRITELN(f,GetCrop().YearCCx:6,'         : Number of years at which CCx declines to 90 % of its value due to self-thinning - Not Applicable')
   ELSE WRITELN(f,GetCrop().YearCCx:6,'         : Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials');
IF (Round(GetCrop().CCxRoot) = undef_int)
   THEN WRITELN(f,GetCrop().CCxRoot:9:2,'      : Shape factor of the decline of CCx over the years due to self-thinning - Not Applicable')
   ELSE WRITELN(f,GetCrop().CCxRoot:9:2,'      : Shape factor of the decline of CCx over the years due to self-thinning - for Perennials');
WRITELN(f,'    -9         : dummy - Parameter no Longer required');

WRITELN(f,GetCrop().CCx:9:2,'      : Maximum canopy cover (CCx) in fraction soil cover');
WRITELN(f,GetCrop().CDC:12:5,'   : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)');
IF (GetCrop().Planting = Seed)
   THEN BEGIN
        WRITELN(f,GetCrop().DaysToGermination:6,'         : Calendar Days: from sowing to emergence');
        WRITELN(f,GetCrop().DaysToMaxRooting:6,'         : Calendar Days: from sowing to maximum rooting depth');
        WRITELN(f,GetCrop().DaysToSenescence:6,'         : Calendar Days: from sowing to start senescence');
        WRITELN(f,GetCrop().DaysToHarvest:6,'         : Calendar Days: from sowing to maturity (length of crop cycle)');
        IF (GetCrop_subkind() = Tuber)
           THEN WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from sowing to start of yield formation')
           ELSE WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from sowing to flowering');
        END
   ELSE BEGIN
        IF (GetCrop().Planting = Transplant)
           THEN BEGIN
                WRITELN(f,GetCrop().DaysToGermination:6,'         : Calendar Days: from transplanting to recovered transplant');
                WRITELN(f,GetCrop().DaysToMaxRooting:6,'         : Calendar Days: from transplanting to maximum rooting depth');
                WRITELN(f,GetCrop().DaysToSenescence:6,'         : Calendar Days: from transplanting to start senescence');
                WRITELN(f,GetCrop().DaysToHarvest:6,'         : Calendar Days: from transplanting to maturity');
                IF (GetCrop_subkind() = Tuber)
                   THEN WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from transplanting to start of yield formation')
                   ELSE WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from transplanting to flowering');
                END
           ELSE BEGIN  // planting = regrowth
                WRITELN(f,GetCrop().DaysToGermination:6,'         : Calendar Days: from regrowth to recovering');
                WRITELN(f,GetCrop().DaysToMaxRooting:6,'         : Calendar Days: from regrowth to maximum rooting depth');
                WRITELN(f,GetCrop().DaysToSenescence:6,'         : Calendar Days: from regrowth to start senescence');
                WRITELN(f,GetCrop().DaysToHarvest:6,'         : Calendar Days: from regrowth to maturity');
                IF (GetCrop_subkind() = Tuber)
                   THEN WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from regrowth to start of yield formation')
                   ELSE WRITELN(f,GetCrop().DaysToFlowering:6,'         : Calendar Days: from regrowth to flowering');
                END;
        END;
WRITELN(f,GetCrop().LengthFlowering:6,'         : Length of the flowering stage (days)');

// Crop.DeterminancyLinked
IF (GetCrop().DeterminancyLinked = true)
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
IF ((GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))
   THEN WRITELN(f,undef_int:6,'         : parameter NO LONGER required') // Building up of Harvest Index (% of growing cycle)')
   ELSE BEGIN
        WRITE(f,GetCrop().fExcess:6);
        IF (GetCrop().fExcess = undef_int)
           THEN WRITELN(f,'         : Excess of potential fruits - Not Applicable')
           ELSE WRITELN(f,'         : Excess of potential fruits (%)');
        END;

// Building-up of Harvest Index
WRITE(f,GetCrop().DaysToHIo:6);
IF (GetCrop().DaysToHIo = undef_int)
   THEN WRITELN(f,'         : Building up of Harvest Index - Not Applicable')
   ELSE BEGIN
        CASE GetCrop_subkind() OF
             Vegetative,
             Forage     : WRITELN(f,'         : Building up of Harvest Index starting at sowing/transplanting (days)');
             Grain      : WRITELN(f,'         : Building up of Harvest Index starting at flowering (days)');
             Tuber      : WRITELN(f,'         : Building up of Harvest Index starting at root/tuber enlargement (days)');
             else WRITELN(f,'         : Building up of Harvest Index during yield formation (days)');
             end;
        END;

//yield response to water
WRITELN(f,GetCrop().WP:8:1,'       : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)');
WRITELN(f,GetCrop().WPy:6,'         : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)');
WRITELN(f,GetCrop().AdaptedToCO2:6,'         : Crop performance under elevated atmospheric CO2 concentration (%)');
WRITELN(f,GetCrop().HI:6,'         : Reference Harvest Index (HIo) (%)');
IF (GetCrop_subkind() = Tuber)
   THEN WRITELN(f,GetCrop().HIincrease:6,'         : Possible increase (%) of HI due to water stress before start of yield formation')
   ELSE WRITELN(f,GetCrop().HIincrease:6,'         : Possible increase (%) of HI due to water stress before flowering');
IF (ROUND(GetCrop().aCoeff) = undef_int)
   THEN WRITELN(f,GetCrop().aCoeff:8:1,'       : No impact on HI of restricted vegetative growth during yield formation ')
   ELSE WRITELN(f,GetCrop().aCoeff:8:1,'       : Coefficient describing positive impact on HI of restricted vegetative growth during yield formation');
IF (ROUND(GetCrop().bCoeff) = undef_int)
   THEN WRITELN(f,GetCrop().bCoeff:8:1,'       : No effect on HI of stomatal closure during yield formation')
   ELSE WRITELN(f,GetCrop().bCoeff:8:1,'       : Coefficient describing negative impact on HI of stomatal closure during yield formation');
WRITELN(f,GetCrop().DHImax:6,'         : Allowable maximum increase (%) of specified HI');

// growing degree days
IF (GetCrop().Planting = Seed)
   THEN BEGIN
        WRITELN(f,GetCrop().GDDaysToGermination:6,'         : GDDays: from sowing to emergence');
        WRITELN(f,GetCrop().GDDaysToMaxRooting:6,'         : GDDays: from sowing to maximum rooting depth');
        WRITELN(f,GetCrop().GDDaysToSenescence:6,'         : GDDays: from sowing to start senescence');
        WRITELN(f,GetCrop().GDDaysToHarvest:6,'         : GDDays: from sowing to maturity (length of crop cycle)');
        IF (GetCrop_subkind() = Tuber)
           THEN WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from sowing to start tuber formation')
           ELSE WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from sowing to flowering');
        END
   ELSE BEGIN
        IF (GetCrop().Planting = Transplant)
           THEN BEGIN
                WRITELN(f,GetCrop().GDDaysToGermination:6,'         : GDDays: from transplanting to recovered transplant');
                WRITELN(f,GetCrop().GDDaysToMaxRooting:6,'         : GDDays: from transplanting to maximum rooting depth');
                WRITELN(f,GetCrop().GDDaysToSenescence:6,'         : GDDays: from transplanting to start senescence');
                WRITELN(f,GetCrop().GDDaysToHarvest:6,'         : GDDays: from transplanting to maturity');
                IF (GetCrop_subkind() = Tuber)
                   THEN WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from transplanting to start yield formation')
                   ELSE WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from transplanting to flowering');
                END
           ELSE BEGIN // Crop.Planting = regrowth
                WRITELN(f,GetCrop().GDDaysToGermination:6,'         : GDDays: from regrowth to recovering');
                WRITELN(f,GetCrop().GDDaysToMaxRooting:6,'         : GDDays: from regrowth to maximum rooting depth');
                WRITELN(f,GetCrop().GDDaysToSenescence:6,'         : GDDays: from regrowth to start senescence');
                WRITELN(f,GetCrop().GDDaysToHarvest:6,'         : GDDays: from regrowth to maturity');
                IF (GetCrop_subkind() = Tuber)
                   THEN WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from regrowth to start yield formation')
                   ELSE WRITELN(f,GetCrop().GDDaysToFlowering:6,'         : GDDays: from regrowth to flowering');
                END;
        END;
WRITELN(f,GetCrop().GDDLengthFlowering:6,'         : Length of the flowering stage (growing degree days)');
WRITELN(f,GetCrop().GDDCGC:13:6,'  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)');
WRITELN(f,GetCrop().GDDCDC:13:6,'  : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)');
WRITELN(f,GetCrop().GDDaysToHIo:6,'         : GDDays: building-up of Harvest Index during yield formation');

// added to 6.2
WRITELN(f,GetCrop().DryMatter:6,'         : dry matter content (%) of fresh yield');

// added to 7.0 - Perennial crops
IF (GetCrop_subkind() = Forage)
   THEN WRITELN(f,GetCrop().RootMinYear1:9:2,'      : Minimum effective rooting depth (m) in first year (for perennials)')
   ELSE WRITELN(f,GetCrop().RootMinYear1:9:2,'      : Minimum effective rooting depth (m) in first year - required only in case of regrowth');
IF (GetCrop().SownYear1 = true)
   THEN BEGIN
        i := 1;
        IF (GetCrop_subkind() = Forage)
           THEN WRITELN(f,i:6,'         : Crop is sown in 1st year (for perennials)')
           ELSE WRITELN(f,i:6,'         : Crop is sown in 1st year - required only in case of regrowth');
        END
   ELSE BEGIN
        i := 0;
        IF (GetCrop_subkind() = Forage)
           THEN WRITELN(f,i:6,'         : Crop is transplanted in 1st year (for perennials)')
           ELSE WRITELN(f,i:6,'         : Crop is transplanted in 1st year - required only in case of regrowth');
        END;

// added to 7.0 - Assimilates
IF (GetCrop_Assimilates().On = false)
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
        WRITELN(f,GetCrop_Assimilates().Period:6,'         : Number of days at end of season during which assimilates are stored in root system');
        WRITELN(f,GetCrop_Assimilates().Stored:6,'         : Percentage of assimilates transferred to root system at last day of season');
        WRITELN(f,GetCrop_Assimilates().Mobilized:6,'         : Percentage of stored assimilates transferred to above ground parts in next season');
        END;
Close(f);

// maximum rooting depth in given soil profile
SetSoil_RootMax(RootMaxInSoilProfile(GetCrop().RootMax,GetSoil().NrSoilLayers,GetSoilLayer()));

// copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(GetCrop().DaysToHarvest - GetCrop().DaysToSenescence);
SetCropFileSet_DaysToHarvest(GetCrop().DaysToHarvest);
SetCropFileSet_GDDaysFromSenescenceToEnd(GetCrop().GDDaysToHarvest - GetCrop().GDDaysToSenescence);
SetCropFileSet_GDDaysToHarvest(GetCrop().GDDaysToHarvest);
END; (* SaveCrop *)








FUNCTION EndGrowingPeriod(Day1 : longint;
                          VAR DayN : longint) : string;
VAR dayi,monthi,yeari : integer;
    Strday,StrMonth : string;
BEGIN
// This function determines Crop.DayN and the string
DayN := Day1 + GetCrop().DaysToHarvest - 1;
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
   IF SimDay1 < GetClimRecord_FromDayNr() THEN SimDay1 := GetClimRecord_FromDayNr();
   IF SimDay1 > GetClimRecord_ToDayNr()
      THEN BEGIN
           Simulation.LinkCropToSimPeriod := false;
           SimDay1 := GetClimRecord_FromDayNr();
           END; *)
   IF ((SimDay1 < GetClimRecord_FromDayNr()) OR (SimDay1 > GetClimRecord_ToDayNr())) THEN
      BEGIN
      SetSimulation_LinkCropToSimPeriod(false);
      SimDay1 := GetClimRecord_FromDayNr();
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
   ELSE yeari := GetClimRecord_FromY(); // yeari = 1901 if undefined year
   (*
   ELSE BEGIN
        yeari := Simulation.YearStartCropCycle;
        IF (CDay1 > GetClimRecord_ToY()) THEN yeari := GetClimRecord_FromY();
        END; *)
DetermineDayNr(dayi,monthi,yeari,CDay1);
temp_str := EndGrowingPeriod(CDay1,CDayN);
END; (* AdjustCropYearToClimFile *)


PROCEDURE AdjustClimRecordTo(CDayN : longint);
VAR dayi,monthi,yeari : INTEGER;
    ToDayNr_tmp : INTEGER;
BEGIN
DetermineDate(CDayN,dayi,monthi,yeari);
SetClimRecord_ToD(31);
SetClimRecord_ToM(12);
SetClimRecord_ToY(yeari);
DetermineDayNr(GetClimRecord_ToD(),GetClimRecord_ToM(),GetClimRecord_ToY(),ToDayNr_tmp);
SetClimRecord_ToDayNr(ToDayNr_tmp)
END; (* AdjustClimRecordTo *)


PROCEDURE ResetSWCToFC;
VAR Loci,layeri,compi,celli : ShortInt;
BEGIN

SetSimulation_IniSWC_AtDepths(false);
IF (ZiAqua < 0) // no ground water table
   THEN BEGIN
        SetSimulation_IniSWC_NrLoc(GetSoil().NrSoilLayers);
        FOR layeri := 1 TO GetSoil().NrSoilLayers DO
            BEGIN
            SetSimulation_IniSWC_Loc_i(layeri,GetSoilLayer_i(layeri).Thickness);
            SetSimulation_IniSWC_VolProc_i(layeri,GetSoilLayer_i(layeri).FC);
            SetSimulation_IniSWC_SaltECe_i(layeri,0);
            END;
        FOR layeri := (GetSoil().NrSoilLayers+1) TO max_No_compartments DO
            BEGIN
            SetSimulation_IniSWC_Loc_i(layeri,undef_double);
            SetSimulation_IniSWC_VolProc_i(layeri,undef_double);
            SetSimulation_IniSWC_SaltECe_i(layeri,undef_double);
            END;
        END
   ELSE BEGIN
        SetSimulation_IniSWC_NrLoc(GetNrCompartments());
        FOR Loci := 1 TO GetSimulation_IniSWC_NrLoc() DO
            BEGIN
            SetSimulation_IniSWC_Loc_i(Loci,GetCompartment_Thickness(Loci));
            SetSimulation_IniSWC_VolProc_i(Loci,GetCompartment_FCadj(Loci));
            SetSimulation_IniSWC_SaltECe_i(Loci,0.0);
        END;
    END;
FOR compi := 1 to GetNrCompartments() DO
    BEGIN
    SetCompartment_Theta(compi, GetCompartment_FCadj(compi)/100);
    SetSimulation_ThetaIni_i(compi,GetCompartment_Theta(compi));
    For celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
        BEGIN // salinity in cells
        SetCompartment_Salt(compi, celli, 0.0);
        SetCompartment_Depo(compi, celli, 0.0);
        END;
    END;
END; (* ResetSWCToFC *)



PROCEDURE AdjustSimPeriod;
VAR IniSimFromDayNr : LongInt;
    FullFileName : string;
    FromDayNr_temp : integer;
    Compartment_temp : rep_Comp;

BEGIN
IniSimFromDayNr := GetSimulation_FromDayNr();
CASE GetSimulation_LinkCropToSimPeriod() OF
     true : BEGIN
            FromDayNr_temp := GetSimulation_FromDayNr();
            DetermineLinkedSimDay1(GetCrop().Day1,FromDayNr_temp);
            SetSimulation_FromDayNr(FromDayNr_temp);
            IF (GetCrop().Day1 = GetSimulation_FromDayNr())
               THEN SetSimulation_ToDayNr(GetCrop().DayN)
               ELSE SetSimulation_ToDayNr(GetSimulation_FromDayNr() + 30); // 30 days
            IF (GetClimFile() <> '(None)') THEN
               BEGIN
               IF (GetSimulation_ToDayNr() > GetClimRecord_ToDayNr()) THEN
                   SetSimulation_ToDayNr(GetClimRecord_ToDayNr());
               IF (GetSimulation_ToDayNr() < GetClimRecord_FromDayNr()) THEN
                      SetSimulation_ToDayNr(GetClimRecord_FromDayNr());
               END;
            END;
    false : BEGIN
            (*
            IF ((GetClimFile() <> '(None)') AND (Simulation.FromDayNr < GetClimRecord_FromDayNr())) THEN
               BEGIN
               Simulation.FromDayNr := GetClimRecord_FromDayNr();
               Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
               END; *)
            IF (GetSimulation_FromDayNr() > GetCrop().Day1) THEN SetSimulation_FromDayNr(GetCrop().Day1);
            SetSimulation_ToDayNr(GetCrop().DayN);
            IF ((GetClimFile() <> '(None)') AND
                ((GetSimulation_FromDayNr() <= GetClimRecord_FromDayNr()) OR (GetSimulation_FromDayNr() >= GetClimRecord_ToDayNr()))) THEN
               BEGIN
               SetSimulation_FromDayNr(GetClimRecord_FromDayNr());
               SetSimulation_ToDayNr(GetSimulation_FromDayNr() + 30); // 30 days
               END;
            END;
    end;

// adjust initial depth and quality of the groundwater when required
IF ((NOT GetSimulParam_ConstGwt()) AND (IniSimFromDayNr <> GetSimulation_FromDayNr())) THEN
   BEGIN
   IF (GetGroundWaterFile() = '(None)')
       THEN FullFileName := CONCAT(GetPathNameProg(),'GroundWater.AqC')
       ELSE FullFileName := GetGroundWaterFileFull();
   // initialize ZiAqua and ECiAqua
   LoadGroundWater(FullFileName,GetSimulation_FromDayNr(),ZiAqua,ECiAqua);
   Compartment_temp := GetCompartment();
   CalculateAdjustedFC((ZiAqua/100),Compartment_temp);
   SetCompartment(Compartment_temp);
   IF GetSimulation_IniSWC_AtFC() THEN ResetSWCToFC;
   END;
END; (* AdjustSimPeriod *)


PROCEDURE SetClimData;
VAR SetARecord, SetBRecord : rep_clim;
    tmptoD, tmpToM, tmpToY : integer;
    tmpFromD, tmpFromM, tmpFromY : integer;
BEGIN
SetClimRecord_NrObs(999); //(heeft geen belang)
                         // IF 365 (= full undefined year)

//Part A - ETo and Rain files --> ClimFile
IF ((GetEToFile() = '(None)') AND (GetRainFile() = '(None)'))
   THEN BEGIN
        SetClimFile('(None)');
        ClimDescription := 'Specify Climatic data when Running AquaCrop';
        SetClimRecord_DataType(Daily);
        SetClimRecord_FromString('any date');
        SetClimRecord_ToString('any date');
        SetClimRecord_FromY(1901);
        END
   ELSE BEGIN
        SetClimFile('EToRainTempFile');
        ClimDescription := 'Read ETo/RAIN/TEMP data set';
        IF (GetEToFile() = '(None)') THEN
           BEGIN
           SetClimRecord_FromY(GetRainRecord_FromY());
           SetClimRecord_FromDayNr(GetRainRecord_FromDayNr());
           SetClimRecord_ToDayNr(GetRainRecord_ToDayNr());
           SetClimRecord_FromString(GetRainRecord_FromString());
           SetClimRecord_ToString(GetRainRecord_ToString());
           IF FullUndefinedRecord(GetRainRecord_FromY(),GetRainRecord_FromD(),GetRainRecord_FromM(),GetRainRecord_ToD(),GetRainRecord_ToM())
              THEN SetClimRecord_NrObs(365);
           END;
        IF (GetRainFile() = '(None)') THEN
           BEGIN
           SetClimRecord_FromY(GetEToRecord_FromY());
           SetClimRecord_FromDayNr(GetEToRecord_FromDayNr());
           SetClimRecord_ToDayNr(GetEToRecord_ToDayNr());
           SetClimRecord_FromString(GetEToRecord_FromString());
           SetClimRecord_ToString(GetEToRecord_ToString());
           IF FullUndefinedRecord(GetEToRecord_FromY(),GetEToRecord_FromD(),GetEToRecord_FromM(),GetEToRecord_ToD(),GetEToRecord_ToM())
              THEN SetClimRecord_NrObs(365);
           END;

        IF ((GetEToFile() <> '(None)') AND (GetRainFile() <> '(None)')) THEN
           BEGIN
           SetARecord := GetEToRecord();
           SetBRecord := GetRainRecord();
           IF ((GetEToRecord_FromY() = 1901)
               AND FullUndefinedRecord(GetEToRecord_FromY(),GetEToRecord_FromD(),GetEToRecord_FromM(),GetEToRecord_ToD(),GetEToRecord_ToM()))
               AND ((GetRainRecord_FromY() = 1901)
               AND FullUndefinedRecord(GetRainRecord_FromY(),GetRainRecord_FromD(),GetRainRecord_FromM(),GetRainRecord_ToD(),GetRainRecord_ToM()))
               THEN SetClimRecord_NrObs(365);

           IF ((GetEToRecord_FromY() = 1901) AND (GetRainRecord_FromY() <> 1901)) THEN
              BEGIN  // Jaartal van RainRecord ---> SetARecord (= EToRecord)
                     // FromY + adjust FromDayNr and FromString
              SetARecord.FromY := GetRainRecord_FromY();
              DetermineDayNr(GetEToRecord_FromD(),GetEToRecord_FromM(),SetARecord.FromY,SetARecord.FromDayNr);
              IF (((SetARecord.FromDayNr < GetRainRecord_FromDayNr())) AND (GetRainRecord_FromY() < GetRainRecord_ToY())) THEN
                 BEGIN
                 SetARecord.FromY := GetRainRecord_FromY() + 1;
                 DetermineDayNr(GetEToRecord_FromD(),GetEToRecord_FromM(),SetARecord.FromY,SetARecord.FromDayNr);
                 END;
              SetClimRecord_FromY(SetARecord.FromY); // nodig voor DayString (werkt met ClimRecord)
              SetARecord.FromString := DayString(SetARecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
              IF (FullUndefinedRecord(GetEToRecord_FromY(),GetEToRecord_FromD(),GetEToRecord_FromM(),GetEToRecord_ToD(),GetEToRecord_ToM()))
                 THEN SetARecord.ToY := GetRainRecord_ToY()
                 ELSE SetARecord.ToY := SetARecord.FromY;
              DetermineDayNr(GetEToRecord_ToD(),GetEToRecord_ToM(),SetARecord.ToY,SetARecord.ToDayNr);
              SetARecord.ToString := DayString(SetARecord.ToDayNr);
              END;

           IF ((GetEToRecord_FromY() <> 1901) AND (GetRainRecord_FromY() = 1901)) THEN
              BEGIN  // Jaartal van EToRecord ---> SetBRecord (= RainRecord)
                     // FromY + adjust FromDayNr and FromString
              SetBRecord.FromY := GetEToRecord_FromY();
              DetermineDayNr(GetRainRecord_FromD(),GetRainRecord_FromM(),SetBRecord.FromY,SetBRecord.FromDayNr);
              IF (((SetBRecord.FromDayNr < GetEToRecord_FromDayNr())) AND (GetEToRecord_FromY() < GetEToRecord_ToY())) THEN
                 BEGIN
                 SetBRecord.FromY := GetEToRecord_FromY() + 1;
                 DetermineDayNr(GetRainRecord_FromD(),GetRainRecord_FromM(),SetBRecord.FromY,SetBRecord.FromDayNr);
                 END;
              SetClimRecord_FromY(SetBRecord.FromY); // nodig voor DayString (werkt met ClimRecord)
              SetBRecord.FromString := DayString(SetBRecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
              IF (FullUndefinedRecord(GetRainRecord_FromY(),GetRainRecord_FromD(),GetRainRecord_FromM(),GetRainRecord_ToD(),GetRainRecord_ToM()))
                 THEN SetBRecord.ToY := GetEToRecord_ToY()
                 ELSE SetBRecord.ToY := SetBRecord.FromY;
              DetermineDayNr(GetRainRecord_ToD(),GetRainRecord_ToM(),SetBRecord.ToY,SetBRecord.ToDayNr);
              SetBRecord.ToString := DayString(SetBRecord.ToDayNr);
              END;

           // bepaal characteristieken van ClimRecord
           WITH GetClimRecord() DO
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
IF (GetTemperatureFile() = '(None)')
   THEN BEGIN
        // no adjustments are required
        END
   ELSE BEGIN
        IF (GetClimFile() = '(None)')
           THEN BEGIN
                SetClimFile('EToRainTempFile');
                ClimDescription := 'Read ETo/RAIN/TEMP data set';
                SetClimRecord_FromY(GetTemperatureRecord().FromY);
                SetClimRecord_FromDayNr(GetTemperatureRecord().FromDayNr);
                SetClimRecord_ToDayNr(GetTemperatureRecord().ToDayNr);
                SetClimRecord_FromString(GetTemperatureRecord().FromString);
                SetClimRecord_ToString(GetTemperatureRecord().ToString);
                IF ((GetTemperatureRecord().FromY = 1901) AND FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM))
                   THEN SetClimRecord_NrObs(365)
                   ELSE SetClimRecord_NrObs(GetTemperatureRecord().ToDayNr - GetTemperatureRecord().FromDayNr + 1);
                END
           ELSE BEGIN
                DetermineDate(GetClimRecord_FromDayNr(),tmpFromD, tmpFromM, tmpFromY);
                SetClimRecord_FromD(tmpFromD);
                SetClimRecord_FromM(tmpFromM);
                SetClimRecord_FromY(tmpFromY);
                DetermineDate(GetClimRecord_ToDayNr(), tmpToD, tmpToM, tmpToY);
                SetClimRecord_ToD(tmpToD);
                SetClimRecord_ToM(tmpToM);
                SetClimRecord_ToY(tmpToY);
                SetARecord := GetClimRecord();
                SetBRecord := GetTemperatureRecord();

                IF ((GetClimRecord_FromY() = 1901) AND (GetTemperatureRecord().FromY = 1901)
                   AND (GetClimRecord_NrObs() = 365)
                   AND FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM))
                       THEN SetClimRecord_NrObs(365)
                       ELSE SetClimRecord_NrObs(GetTemperatureRecord_ToDayNr() - GetTemperatureRecord().FromDayNr + 1);

                IF ((GetClimRecord_FromY() = 1901) AND (GetTemperatureRecord().FromY <> 1901)) THEN
                   BEGIN  // Jaartal van TemperatureRecord ---> SetARecord (= ClimRecord)
                     // FromY + adjust FromDayNr and FromString
                   SetARecord.FromY := GetTemperatureRecord().FromY;
                   DetermineDayNr(GetClimRecord_FromD(),GetClimRecord_FromM(),SetARecord.FromY,SetARecord.FromDayNr);
                   IF (((SetARecord.FromDayNr < GetTemperatureRecord().FromDayNr)) AND (GetTemperatureRecord().FromY < GetTemperatureRecord().ToY)) THEN
                      BEGIN
                      SetARecord.FromY := GetTemperatureRecord().FromY + 1;
                      DetermineDayNr(GetClimRecord_FromD(),GetClimRecord_FromM(),SetARecord.FromY,SetARecord.FromDayNr);
                      END;
                   //SetClimRecord_FromY(SetARecord.FromY); // nodig voor DayString (werkt met ClimRecord)
                   SetARecord.FromString := DayString(SetARecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
                   IF (FullUndefinedRecord(GetClimRecord_FromY(),GetClimRecord_FromD(),GetClimRecord_FromM(),GetClimRecord_ToD(),GetClimRecord_ToM()))
                      THEN SetARecord.ToY := GetTemperatureRecord().ToY
                      ELSE SetARecord.ToY := SetARecord.FromY;
                   DetermineDayNr(GetClimRecord_ToD(),GetClimRecord_ToM(),SetARecord.ToY,SetARecord.ToDayNr);
                   SetARecord.ToString := DayString(SetARecord.ToDayNr);
                   END;

                IF ((GetClimRecord_FromY() <> 1901) AND (GetTemperatureRecord().FromY = 1901)) THEN
                   BEGIN  // Jaartal van ClimRecord ---> SetBRecord (= GetTemperatureRecord())
                     // FromY + adjust FromDayNr and FromString
                   SetBRecord.FromY := GetClimRecord_FromY();
                   DetermineDayNr(GetTemperatureRecord().FromD,GetTemperatureRecord().FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
                   IF (((SetBRecord.FromDayNr < GetClimRecord_FromDayNr())) AND (GetClimRecord_FromY() < GetClimRecord_ToY())) THEN
                      BEGIN
                      SetBRecord.FromY := GetClimRecord_FromY() + 1;
                      DetermineDayNr(GetTemperatureRecord().FromD,GetTemperatureRecord().FromM,SetBRecord.FromY,SetBRecord.FromDayNr);
                      END;
                   //SetClimRecord_FromY(SetBRecord.FromY); // nodig voor DayString (werkt met ClimRecord)
                   SetBRecord.FromString := DayString(SetBRecord.FromDayNr);
                     // ToY + adjust ToDayNr and ToString
                   IF (FullUndefinedRecord(GetTemperatureRecord().FromY,GetTemperatureRecord().FromD,GetTemperatureRecord().FromM,GetTemperatureRecord().ToD,GetTemperatureRecord().ToM))
                      THEN SetBRecord.ToY := GetClimRecord_ToY()
                      ELSE SetBRecord.ToY := SetBRecord.FromY;
                   DetermineDayNr(GetTemperatureRecord().ToD,GetTemperatureRecord().ToM,SetBRecord.ToY,SetBRecord.ToDayNr);
                   SetBRecord.ToString := DayString(SetBRecord.ToDayNr);
                   END;

                // bepaal nieuwe characteristieken van ClimRecord
                WITH GetClimRecord() DO
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
  CumDepth := CumDepth + GetCompartment_Thickness(compi);
  IF (CumDepth <= RootingDepth)
     THEN Factor := 1
     ELSE BEGIN
          frac_value := RootingDepth - (CumDepth - GetCompartment_Thickness(compi));
          IF (frac_value > 0)
             THEN Factor := frac_value/GetCompartment_Thickness(compi)
             ELSE Factor := 0;
          END;
  SetRootZoneWC_Actual(GetRootZoneWC().Actual
     + Factor * 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
              * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_FC(GetRootZoneWC().FC
     + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).FC * GetCompartment_Thickness(compi)
              * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_Leaf(GetRootZoneWC().Leaf
     + Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC
     - GetCrop().pLeafAct * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC-GetSoilLayer_i(GetCompartment_Layer(compi)).WP))
       * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_Thresh(GetRootZoneWC().Thresh
     + Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC
     - GetCrop().pActStom * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC-GetSoilLayer_i(GetCompartment_Layer(compi)).WP))
       * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_Sen(GetRootZoneWC().Sen
     + Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC
     - GetCrop().pSenAct * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC-GetSoilLayer_i(GetCompartment_Layer(compi)).WP))
       * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_WP(GetRootZoneWC().WP
     + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).WP * GetCompartment_Thickness(compi)
              * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
  SetRootZoneWC_SAT(GetRootZoneWC().SAT
     + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).SAT * GetCompartment_Thickness(compi)
              * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
UNTIL (CumDepth >= RootingDepth) OR (compi = GetNrCompartments());

// calculate SWC in top soil (top soil in meter = SimulParam.ThicknessTopSWC/100)
IF ((RootingDepth*100) <= GetSimulParam_ThicknessTopSWC())
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
        TopSoilInMeter := GetSimulParam_ThicknessTopSWC()/100;
        REPEAT
          compi := compi + 1;
          CumDepth := CumDepth + GetCompartment_Thickness(compi);
          IF ((CumDepth*100) <= GetSimulParam_ThicknessTopSWC())
             THEN Factor := 1
             ELSE BEGIN
                  frac_value := TopSoilInMeter - (CumDepth - GetCompartment_Thickness(compi));
                  IF (frac_value > 0)
                     THEN Factor := frac_value/GetCompartment_Thickness(compi)
                     ELSE Factor := 0;
                  END;
          SetRootZoneWC_ZtopAct(GetRootZoneWC().ZtopAct
            + Factor * 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
          SetRootZoneWC_ZtopFC(GetRootZoneWC().ZtopFC
            + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).FC * GetCompartment_Thickness(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
          SetRootZoneWC_ZtopWP(GetRootZoneWC().ZtopWP
            + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).WP * GetCompartment_Thickness(compi)
                     * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
          SetRootZoneWC_ZtopThresh(GetRootZoneWC().ZtopThresh
            + Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC
            - GetCrop().pActStom * (GetSoilLayer_i(GetCompartment_Layer(compi)).FC-GetSoilLayer_i(GetCompartment_Layer(compi)).WP))
              * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100));
        UNTIL (CumDepth >= TopSoilInMeter) OR (compi = GetNrCompartments());
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
IF (GetClimRecord_FromY() = 1901)
   THEN strB := ''
   ELSE Str(yeari:4,strB);
StrB := CONCAT(TRIM(strA),' ',Trim(NameMonth[monthi]),' ',Trim(strB));
WHILE (Length(StrB) < 17) DO StrB := CONCAT(StrB,' ');
DayString := StrB;
END; (* DayString *)


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
t := DAP - GetSimulation_DelayedDays() - DaysToFlower;
//Simulation.WPyON := false;
PercentLagPhase := 0;
IF (t <= 0)
   THEN HIday := 0
   ELSE BEGIN
        IF ((GetCrop().Subkind = Vegetative) AND (TempPlanting = Regrowth)) THEN dHIdt := 100;
        IF ((GetCrop().Subkind = Forage) AND (TempPlanting = Regrowth)) THEN dHIdt := 100;
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
                        IF ((GetCrop_subkind() = Tuber) OR (GetCrop_subkind() = Vegetative) OR (GetCrop_subkind() = Forage))
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
            AND (GetCrop_subkind() <> Vegetative) AND (GetCrop_subkind() <> Forage))
                THEN HIfinal := ROUND(HIday);
        IF (HIday > HIfinal) THEN HIday := HIfinal;
        END;
HarvestIndexDay := HIday;

END; (* HarvestIndexDay *)



PROCEDURE ReadCropSettingsParameters;
VAR f : textfile;
    FullName : string;
    simul_ed,simul_pCCHIf,simul_SFR,simul_TAWg,simul_beta,simul_Tswc : Shortint;
    simul_kcWB,simul_RZEma,simul_pfao,simul_expFsen : double;
    simul_RpZmi,simul_lowox : integer;
BEGIN
FullName := CONCAT(GetPathNameSimul(),'Crop.PAR');
Assign(f,FullName);
Reset(f);
Readln(f,simul_ed); // evaporation decline factor in stage 2
SetSimulParam_EvapDeclineFactor(simul_ed);
Readln(f,simul_kcWB); //Kc wet bare soil [-]
SetSimulParam_KcWetBare(simul_kcWB);
Readln(f,simul_pCCHIf); // CC threshold below which HI no longer increase(% of 100)
SetSimulParam_PercCCxHIfinal(simul_pCCHIf);
Readln(f,simul_RpZmi); //Starting depth of root sine function (% of Zmin)
SetSimulParam_RootPercentZmin(simul_RpZmi);
Readln(f,simul_RZEma); // cm/day
SetSimulParam_MaxRootZoneExpansion(simul_RZEma);
SetSimulParam_MaxRootZoneExpansion(5.00); // fixed at 5 cm/day
Readln(f,simul_SFR); // Shape factor for effect water stress on rootzone expansion
SetSimulParam_KsShapeFactorRoot(simul_SFR);
Readln(f,simul_TAWg);  // Soil water content (% TAW) required at sowing depth for germination
SetSimulParam_TAWGermination(simul_TAWg);
Readln(f,simul_pfao); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
SetSimulParam_pAdjFAO(simul_pfao);
Readln(f,simul_lowox); //number of days for full effect of deficient aeration
SetSimulParam_DelayLowOxygen(simul_lowox);
Readln(f,simul_expFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
SetSimulParam_ExpFsen(simul_expFsen);
Readln(f,simul_beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
SetSimulParam_Beta(simul_beta);
Readln(f,simul_Tswc); // Thickness top soil (cm) in which soil water depletion has to be determined
SetSimulParam_ThicknessTopSWC(simul_Tswc);
Close(f);
END; (* ReadCropSettingsParameters *)


PROCEDURE ReadFieldSettingsParameters;
VAR f : textfile;
    FullName : string;
    simul_evmax : Shortint;
BEGIN
FullName := CONCAT(GetPathNameSimul(),'Field.PAR');
Assign(f,FullName);
Reset(f);
Readln(f,simul_evmax); //maximum water extraction depth by soil evaporation [cm]
SetSimulParam_EvapZmax(simul_evmax);
Close(f);
END; (* ReadFieldSettingsParameters *)


PROCEDURE ReadTemperatureSettingsParameters;
VAR f0 : text;
    FullName : string;
    simul_GDD : Shortint;
    simul_Tmi, simul_Tma : double; 
BEGIN
FullName := CONCAT(GetPathNameSimul(),'Temperature.PAR');
Assign(f0,FullName);
Reset(f0);
Readln(f0);
Readln(f0,simul_Tmi);   //Default minimum temperature (degC) if no temperature file is specified
SetSimulParam_Tmin(simul_Tmi);
Readln(f0,simul_Tma);   //Default maximum temperature (degC) if no temperature file is specified
SetSimulParam_Tmax(simul_Tma);
Readln(f0,simul_GDD); //Default method for GDD calculations
SetSimulParam_GDDMethod(simul_GDD);
IF (GetSimulParam_GDDMethod() > 3) THEN SetSimulParam_GDDMethod(3);
IF (GetSimulParam_GDDMethod() < 1) THEN SetSimulParam_GDDMethod(1);
Close(f0);
END; (* ReadTemperatureSettingsParameters *)

FUNCTION AdjustedKsStoToECsw(ECeMin,ECeMax : ShortInt;
                             ResponseECsw : INTEGER;
                             ECei,ECswi,ECswFCi,Wrel,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,KsStoIN : double) : double;
VAR ECswRel,LocalKsShapeFactorSalt,
    KsSalti,SaltStressi,StoClosure,KsStoOut : double;
BEGIN
IF ((ResponseECsw > 0) AND (Wrel > 0) AND (GetSimulation_SalinityConsidered() = true))
   THEN BEGIN  //adjustment to ECsw considered
        ECswRel := ECswi - (ECswFCi - ECei) + (ResponseECsw-100)*Wrel;
        IF ((ECswRel > ECeMin) AND (ECswRel < ECeMax))
           THEN BEGIN
                // stomatal closure at ECsw relative
                LocalKsShapeFactorSalt := +3; // CONVEX give best ECsw response
                KsSalti := KsSalinity(GetSimulation_SalinityConsidered(),ECeMin,ECeMax,ECswRel,LocalKsShapeFactorSalt);
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
IF (RootingDepth >= GetCrop().RootMin)
   THEN BEGIN
        REPEAT
        compi := compi + 1;
        CumDepth := CumDepth + GetCompartment_Thickness(compi);
        IF (CumDepth <= RootingDepth)
           THEN Factor := 1
           ELSE BEGIN
                frac_value := RootingDepth - (CumDepth - GetCompartment_Thickness(compi));
                IF (frac_value > 0)
                   THEN Factor := frac_value/GetCompartment_Thickness(compi)
                   ELSE Factor := 0;
                END;
        Factor := Factor * (GetCompartment_Thickness(compi))/RootingDepth; // weighting factor
        ZrECe := ZrECe + Factor * ECeComp(GetCompartment_i(compi));
        ZrECsw := ZrECsw + Factor * ECswComp(GetCompartment_i(compi),(false)); // not at FC
        ZrECswFC := ZrECswFC + Factor * ECswComp(GetCompartment_i(compi),(true)); // at FC
        UNTIL (CumDepth >= RootingDepth) OR (compi = GetNrCompartments());
        IF (((GetCrop().ECemin <> undef_int) AND (GetCrop().ECemax <> undef_int)) AND (GetCrop().ECemin < GetCrop().ECemax))
           THEN ZrKsSalt := KsSalinity((true),GetCrop().ECemin,GetCrop().ECemax,ZrECe,(0.0))
           ELSE ZrKsSalt := KsSalinity((false),GetCrop().ECemin,GetCrop().ECemax,ZrECe,(0.0));
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
        Assign(f0,GetCO2FileFull());
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
VAR SumGDD,GDDi,SumKcPot,SumGDDforPlot,SumGDDfromDay1 : double;
    Tndayi, Txdayi,CCi,CCxWitheredForB,TpotForB,EpotTotForB : double;
    CCinitial,DayFraction, GDDayFraction : double;
    DayCC,Tadj,GDDTadj : INTEGER;
    fTemp : textFile;
    Dayi : INTEGER;
    GrowthON : BOOLEAN;

BEGIN
// 1. Open Temperature file
IF (GetTemperatureFile() <> '(None)') THEN
   BEGIN
   Assign(fTemp,CONCAT(GetPathNameSimul(),'TCrop.SIM'));
   Reset(fTemp);
   END;

// 2. Initialise global settings
SetSimulation_DelayedDays(0); // required for CalculateETpot
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
    IF (GetTemperatureFile() <> '(None)')
       THEN BEGIN
            READLN(fTemp,Tndayi,Txdayi);
            GDDi := DegreesDay(Tbase,Tupper,Tndayi,Txdayi,GetSimulParam_GDDMethod());
            END
       ELSE GDDi := DegreesDay(Tbase,Tupper,TDayMin,TDayMax,GetSimulParam_GDDMethod());
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
IF (GetTemperatureFile() <> '(None)') THEN Close(fTemp);

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
    IF (Comp[Compi].Theta > (GetSoilLayer_i(Comp[compi].Layer).SAT)/100)
        THEN Comp[Compi].Theta := (GetSoilLayer_i(Comp[compi].Layer).SAT)/100;
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
                     + (10*(Depthi-DTopComp)*GetSoilLayer_i(Comp[Compi].Layer).SAT)*((ECTopComp+ECbotComp)/2);
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
                               + (10*(D2-DTopComp)*GetSoilLayer_i(Comp[Compi].Layer).SAT)*((ECTopComp+ECbotComp)/2);
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
    IF (Comp[Compi].Theta > (GetSoilLayer_i(Comp[compi].Layer).SAT)/100)
        THEN Comp[Compi].Theta := (GetSoilLayer_i(Comp[compi].Layer).SAT)/100;
    IF (Comp[Compi].Theta < 0) THEN Comp[Compi].Theta := 0;
    END;

For Compi := 1 TO NrComp DO // from (10*VolSat*dZ * EC) to ECe and distribution in cellls
    BEGIN
    Comp[Compi].WFactor := Comp[Compi].WFactor/(10*Comp[Compi].Thickness*GetSoilLayer_i(Comp[Compi].Layer).SAT);
    DetermineSaltContent(Comp[Compi].WFactor,Comp[Compi]);
    END;
END; (* TranslateIniPointsToSWProfile *)


PROCEDURE LoadInitialConditions(SWCiniFileFull : string;
                                VAR IniSurfaceStorage : double);
VAR f0 : TextFile;
    i : ShortInt;
    StringParam,swcinidescr_temp : string;
    VersionNr : double;
    CCini_temp, Bini_temp, Zrini_temp, ECStorageIni_temp : double;
    NrLoc_temp : shortint;
    Loc_i_temp, VolProc_i_temp, SaltECe_i_temp : double;
BEGIN
// IniSWCRead attribute of the function was removed to fix a 
// bug occurring when the function was called in TempProcessing.pas
// Keep in mind that this could affect the graphical interface
Assign(f0,SWCiniFileFull);
Reset(f0);
READLN(f0,swcinidescr_temp);
setSWCiniDescription(swcinidescr_temp);
READLN(f0,VersionNr); // AquaCrop Version
IF (ROUND(10*VersionNr) < 41) // initial CC at start of simulation period
   THEN SetSimulation_CCini(undef_int)
   ELSE BEGIN
        READLN(f0,CCini_temp);
        SetSimulation_CCini(CCini_temp);
        end;
IF (ROUND(10*VersionNr) < 41) // B produced before start of simulation period
   THEN SetSimulation_Bini(0.000)
   ELSE BEGIN
        READLN(f0,Bini_temp);
        SetSimulation_Bini(Bini_temp);
        end;
IF (ROUND(10*VersionNr) < 41) // initial rooting depth at start of simulation period
   THEN SetSimulation_Zrini(undef_int)
   ELSE BEGIN
        READLN(f0,Zrini_temp);
        SetSimulation_Zrini(Zrini_temp);
        END;
READLN(f0,IniSurfaceStorage);
IF (ROUND(10*VersionNr) < 32) // EC of the ini surface storage
   THEN SetSimulation_ECStorageIni(0)
   ELSE BEGIN 
        READLN(f0,ECStorageIni_temp);
        SetSimulation_ECStorageIni(ECStorageIni_temp);
        END;
READLN(f0,i);
IF (i = 1)
   THEN SetSimulation_IniSWC_AtDepths(true)
   ELSE SetSimulation_IniSWC_AtDepths(false);
READLN(f0,NrLoc_temp);
SetSimulation_IniSWC_NrLoc(NrLoc_temp);
READLN(f0);
READLN(f0);
READLN(f0);
FOR i := 1 TO GetSimulation_IniSWC_NrLoc() DO
    BEGIN
    READLN(f0,StringParam);
    Loc_i_temp := GetSimulation_IniSWC_Loc_i(i);
    VolProc_i_temp := GetSimulation_IniSWC_VolProc_i(i);
    IF (ROUND(10*VersionNr) < 32) // ECe at the locations
       THEN BEGIN
            SplitStringInTwoParams(StringParam,Loc_i_temp,VolProc_i_temp);
            SetSimulation_IniSWC_SaltECe_i(i, 0);
            END
       ELSE BEGIN
            SaltECe_i_temp := GetSimulation_IniSWC_SaltECe_i(i);
            SplitStringInThreeParams(StringParam,Loc_i_temp,VolProc_i_temp,SaltECe_i_temp);
            SetSimulation_IniSWC_SaltECe_i(i, SaltECe_i_temp);
            END;
    SetSimulation_IniSWC_Loc_i(i, Loc_i_temp);
    SetSimulation_IniSWC_VolProc_i(i, VolProc_i_temp);
    END;
Close(f0);
SetSimulation_IniSWC_AtFC(false);
END; (* LoadInitialConditions *)


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
TheNrSoilLayers := GetSoil().NrSoilLayers;
TheSoilLayer := GetSoilLayer();
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
    Compartment_temp : rep_Comp;

BEGIN
//1. Actual total depth of compartments
TotDepthC := 0;
FOR compi := 1 to GetNrCompartments() DO TotDepthC := TotDepthC + GetCompartment_Thickness(compi);

//2. Stretch thickness of bottom soil layer if required
TotDepthL := 0;
For layeri := 1 to GetSoil().NrSoilLayers DO TotDepthL := TotDepthL + GetSoilLayer_i(layeri).Thickness;
IF (TotDepthC > TotDepthL) THEN SetSoilLayer_Thickness(GetSoil().NrSoilLayers, GetSoilLayer_i(GetSoil().NrSoilLayers).Thickness + (TotDepthC - TotDepthL));

//3. Assign a soil layer to each soil compartment
Compartment_temp := GetCompartment();
DesignateSoilLayerToCompartments(GetNrCompartments(),GetSoil().NrSoilLayers,Compartment_temp);
SetCompartment(Compartment_temp);

//4. Adjust initial Soil Water Content of soil compartments
IF GetSimulation_ResetIniSWC()
   THEN BEGIN
        IF GetSimulation_IniSWC_AtDepths()
           THEN BEGIN
                Compartment_temp := GetCompartment();
                TranslateIniPointsToSWProfile(GetSimulation_IniSWC_NrLoc(),GetSimulation_IniSWC_Loc(),GetSimulation_IniSWC_VolProc(),
                                              GetSimulation_IniSWC_SaltECe(),GetNrCompartments(),Compartment_temp);
                SetCompartment(Compartment_temp);
                END
           ELSE BEGIN
                Compartment_temp := GetCompartment();
                TranslateIniLayersToSWProfile(GetSimulation_IniSWC_NrLoc(),GetSimulation_IniSWC_Loc(),GetSimulation_IniSWC_VolProc(),
                                              GetSimulation_IniSWC_SaltECe(),GetNrCompartments(),Compartment_temp);
                SetCompartment(Compartment_temp);
                END;
        END
   ELSE BEGIN
        Compartment_temp := GetCompartment();
        TranslateIniLayersToSWProfile(PrevNrComp,PrevThickComp,PrevVolPrComp,PrevECdSComp,GetNrCompartments(),Compartment_temp);
        SetCompartment(Compartment_temp);
        END;

//5. Adjust watercontent in soil layers and determine ThetaIni
Total := 0;
FOR layeri := 1 TO GetSoil().NrSoilLayers DO SetSoilLayer_WaterContent(layeri, 0);
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    SetSimulation_ThetaIni_i(compi,GetCompartment_Theta(compi));
    SetSoilLayer_WaterContent(GetCompartment_Layer(compi), GetSoilLayer_i(GetCompartment_Layer(compi)).WaterContent
                                                                + GetSimulation_ThetaIni_i(compi)*100*10*GetCompartment_Thickness(compi));
    END;
FOR layeri := 1 TO GetSoil().NrSoilLayers DO Total := Total + GetSoilLayer_i(layeri).WaterContent;
SetTotalWaterContent_BeginDay(Total);
END; (* AdjustThetaInitial *)




PROCEDURE AdjustSizeCompartments(CropZx : double);
VAR i ,compi : INTEGER;
    TotDepthC,fAdd : Double;
    PrevNrComp : ShortInt;
    PrevThickComp,PrevVolPrComp,PrevECdSComp : rep_IniComp;
BEGIN

//1. Save intial soil water profile (required when initial soil water profile is NOT reset at start simulation - see 7.)
PrevNrComp := GetNrCompartments();
FOR compi := 1 To prevnrComp DO
    BEGIN
    PrevThickComp[compi] := GetCompartment_Thickness(compi);
    PrevVolPrComp[compi] := 100*GetCompartment_Theta(compi);
    END;

//2. Actual total depth of compartments
TotDepthC := 0;
FOR i := 1 to GetNrCompartments() DO TotDepthC := TotDepthC + GetCompartment_Thickness(compi);

//3. Increase number of compartments (if less than 12)
IF (GetNrCompartments() < 12) THEN
   REPEAT
   SetNrCompartments(GetNrCompartments() + 1);
   IF ((CropZx - TotDepthC) > GetSimulParam_CompDefThick())
      THEN SetCompartment_Thickness(GetNrCompartments(), GetSimulParam_CompDefThick())
      ELSE SetCompartment_Thickness(GetNrCompartments(), CropZx - TotDepthC);
   TotDepthC := TotDepthC + GetCompartment_Thickness(GetNrCompartments());
   UNTIL ((GetNrCompartments() = max_No_compartments) OR ((TotDepthC + 0.00001) >= CropZx));

//4. Adjust size of compartments (if total depth of compartments < rooting depth)
IF ((TotDepthC + 0.00001) < CropZx) THEN
   BEGIN
   SetNrCompartments(12);
   fAdd := (CropZx/0.1 - 12)/78;
   FOR i := 1 TO 12 DO
       BEGIN
       SetCompartment_Thickness(i, 0.1 * (1 + i*fAdd));
       SetCompartment_Thickness(i, 0.05 * ROUND(GetCompartment_Thickness(i) * 20));
       END;
   TotDepthC := 0;
   FOR i := 1 to GetNrCompartments() DO TotDepthC := TotDepthC + GetCompartment_Thickness(i);
   IF (TotDepthC < CropZx)
      THEN REPEAT
           SetCompartment_Thickness(12, GetCompartment_Thickness(12) + 0.05);
           TotDepthC := TotDepthC + 0.05;
           UNTIL (TotDepthC >= CropZx)
      ELSE WHILE ((TotDepthC - 0.04999999) >= CropZx) DO
               BEGIN
               SetCompartment_Thickness(12, GetCompartment_Thickness(12) - 0.05);
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
   UNTIL ((WaterTableInProfile = true) OR (compi >= GetNrCompartments()));
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
         SetSimulParam_ConstGwt(true);
         TheEnd := true;
         END;
     1 : BEGIN // constant groundwater table
         SetSimulParam_ConstGwt(true);
         END;
     else SetSimulParam_ConstGwt(false);
     end;

// first day of observations (only for variable groundwater table)
IF (NOT GetSimulParam_ConstGwt()) THEN
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
SetCalendarFileFull(GetCalendarFile());  (* no file *)
SetCalendarDescription('');
SetOnset_GenerateOn(false);
SetOnset_GenerateTempOn(false);
SetEndSeason_GenerateTempOn(false);
SetCalendarDescription('No calendar for the Seeding/Planting year');
END; (* NoCropCalendar *)




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
    i,simul_RpZmi,simul_lowox : INTEGER;
    simul_ed,effrainperc,effrainshow,effrainrootE,simul_saltdiff,simul_saltsolub,simul_root,simul_pCCHIf,simul_SFR,simul_TAWg,simul_beta,simul_Tswc,simul_GDD,simul_EZma : ShortInt;
    simul_rod,simul_kcWB,simul_RZEma,simul_pfao,simul_expFsen,simul_Tmi,simul_Tma : double ;
BEGIN
IF FileExists(FullFileNameProgramParameters)
   THEN BEGIN // load set of program parameters
        Assign(f0,FullFileNameProgramParameters);
        Reset(f0);
        // crop
        Readln(f0,simul_ed); // evaporation decline factor in stage 2
        SetSimulParam_EvapDeclineFactor(simul_ed);
        Readln(f0,simul_kcWB); //Kc wet bare soil [-]
        SetSimulParam_KcWetBare(simul_kcWB);
        Readln(f0,simul_pCCHIf); // CC threshold below which HI no longer increase(% of 100)
        SetSimulParam_PercCCxHIfinal(simul_pCCHIf);
        Readln(f0,simul_RpZmi); //Starting depth of root sine function (% of Zmin)
        SetSimulParam_RootPercentZmin(simul_RpZmi);
        Readln(f0,simul_RZEma); // cm/day
        SetSimulParam_MaxRootZoneExpansion(simul_RZEma);
        SetSimulParam_MaxRootZoneExpansion(5.00); // fixed at 5 cm/day
        Readln(f0,simul_SFR); // Shape factor for effect water stress on rootzone expansion
        SetSimulParam_KsShapeFactorRoot(simul_SFR);
        Readln(f0,simul_TAWg);  // Soil water content (% TAW) required at sowing depth for germination
        SetSimulParam_TAWGermination(simul_TAWg);
        Readln(f0,simul_pfao); //Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
        SetSimulParam_pAdjFAO(simul_pfao);
        Readln(f0,simul_lowox); //number of days for full effect of deficient aeration
        SetSimulParam_DelayLowOxygen(simul_lowox);
        Readln(f0,simul_expFsen); // exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
        SetSimulParam_ExpFsen(simul_expFsen);
        Readln(f0,simul_beta); // Decrease (percentage) of p(senescence) once early canopy senescence is triggered
        SetSimulParam_Beta(simul_beta);
        Readln(f0,simul_Tswc);  // Thickness top soil (cm) in which soil water depletion has to be determined
        SetSimulParam_ThicknessTopSWC(simul_Tswc);
        // field
        Readln(f0,simul_EZma); //maximum water extraction depth by soil evaporation [cm]
        SetSimulParam_EvapZmax(simul_EZma);
        // soil
        READLN(f0,simul_rod); //considered depth (m) of soil profile for calculation of mean soil water content
        SetSimulParam_RunoffDepth(simul_rod);
        READLN(f0,i);   // correction CN for Antecedent Moisture Class
        IF (i = 1)
           THEN SetSimulParam_CNcorrection(true)
           ELSE SetSimulParam_CNcorrection(false);
        READLN(f0,simul_saltdiff); // salt diffusion factor (%)
        READLN(f0,simul_saltsolub); // salt solubility (g/liter)
        READLN(f0,simul_root); // shape factor capillary rise factor
        SetSimulParam_SaltDiff(simul_saltdiff);
        SetSimulParam_SaltSolub(simul_saltsolub);
        SetSimulParam_RootNrDF(simul_root);
        SetSimulParam_IniAbstract(5); // fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
        // Temperature
        Readln(f0,simul_Tmi);   //Default minimum temperature (degC) if no temperature file is specified
        SetSimulParam_Tmin(simul_Tmi);
        Readln(f0,simul_Tma);   //Default maximum temperature (degC) if no temperature file is specified
        SetSimulParam_Tmax(simul_Tma);
        Readln(f0,simul_GDD); //Default method for GDD calculations
        SetSimulParam_GDDMethod(simul_GDD);
        IF (GetSimulParam_GDDMethod() > 3) THEN SetSimulParam_GDDMethod(3);
        IF (GetSimulParam_GDDMethod()< 1) THEN SetSimulParam_GDDMethod(3);
        // Rainfall
        Readln(f0,i);
        Case i OF
          0 : SetSimulParam_EffectiveRain_Method(Full);
          1 : SetSimulParam_EffectiveRain_Method(USDA);
          2 : SetSimulParam_EffectiveRain_Method(Percentage);
          end;
        Readln(f0,effrainperc); // IF Method is Percentage
        SetSimulParam_EffectiveRain_PercentEffRain(effrainperc);
        Readln(f0,effrainshow);  // For estimation of surface run-off
        SetSimulParam_EffectiveRain_ShowersInDecade(effrainshow);
        Readln(f0,effrainrootE); // For reduction of soil evaporation
        SetSimulParam_EffectiveRain_RootNrEvap(effrainrootE);
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
