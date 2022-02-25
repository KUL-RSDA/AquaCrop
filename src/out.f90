UNIT GLOBAL;

INTERFACE

USES SYSUTILS, INTERFACE_GLOBAL;


ConstEvapZmin = 15 !! cm  minimum soil depth for water extraction by evaporation

rep_string3  = string[3] !! Read/Write ProfFile


rep_TypeObsSim =(ObsSimCC,ObsSimB,ObsSimSWC)


logical :: DataPath, ObsPath
character(len=STRING_LENGTH) :: SWCiniFileFull, ProjectFileFull, MultipleProjectFileFull
character(len=STRING_LENGTH) :: ProjectDescription, MultipleProjectDescription, OffSeasonDescription, GroundWaterDescription

real(dp) :: RootingDepth
real(dp) :: CCiActual, CCiPrev, CCiTopEarlySen

integer(int32) :: SenStage
integer(int32) :: DaySubmerged
real(dp) :: ETo, Epot, Tpot, Rain, Irrigation, Infiltrated, CRwater
    !! mm/day
real(dp) :: Tmin, Tmax
    !! degC
real(dp) :: SurfaceStorage, Runoff, Drain, Eact, Tact, TactWeedInfested
    !! mm/day
logical :: EvapoEntireSoilSurface
    !! True of soil wetted by RAIN (false = IRRIGATION and fw < 1)
logical :: PreDay
real(dp) :: Surf0
    !! surface water [mm] begin day
integer(int32) :: NrC, NrD
    !! formats REAL
real(dp) :: MinReal, MaxReal
integer(int32) :: MinInt, MaxInt
integer(int32) :: MaxPlotNew
integer(int8) :: MaxPlotTr
integer(int8) :: IniPercTAW
    !! Default Value for Percentage TAW for Initial Soil Water Content Menu
real(dp) :: ECstorage
    !! salinity
    !! EC surface storage dS/m
real(dp) :: ECdrain
    !! EC drain water dS/m
real(dp) :: SaltInfiltr
    !! salt infiltrated in soil profile Mg/ha
real(dp) :: CRsalt
    !! gram/m2
real(dp) :: ECiAqua
    !! EC of the groundwater table in dS/m




integer(int8) :: OutputAggregate
    !! Extra for stand alone procedure
character(len=STRING_LENGTH) :: PathNameList, PathNameParam
logical :: Out1Wabal, Out2Crop, Out3Prof, Out4Salt, Out5CompWC, Out6CompEC, Out7Clim, OutDaily, Part1Mult, Part2Eval

repTypeProject = (TypePRO,TypePRM,TypeNone)

subroutine NoManagementOffSeason()

end subroutine NoManagementOffSeason

subroutine LoadOffSeason(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadOffSeason


subroutine CalculateAdjustedFC(DepthAquifer, CompartAdj)
    real(dp), intent(in) :: DepthAquifer
    type(rep_Comp), intent(inout) :: CompartAdj
end subroutine CalculateAdjustedFC

subroutine DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_Comp), intent(inout) :: Compartment
end subroutine DesignateSoilLayerToCompartments


subroutine specify_soil_layer(NrCompartments, NrSoilLayers, SoilLayer, Compartment, TotalWaterContent)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_SoilLayer), intent(inout) :: SoilLayer
    type(rep_Comp), intent(inout) :: Compartment
    type(rep_Content), intent(inout) :: TotalWaterContent
end subroutine specify_soil_layer

integer(int32) function ActiveCells(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ActiveCells

subroutine Calculate_Saltmobility(layer, SaltDiffusion, Macro, Mobil)
    integer(int32), intent(in) :: layer
    integer(int8), intent(in) :: SaltDiffusion
    integer(int8), intent(in) :: Macro
    type(rep_salt), intent(inout) :: Mobil
end subroutine Calculate_Saltmobility



subroutine DetermineSaltContent(ECe, Comp)
    real(dp), intent(in) :: ECe
    type(CompartmentIndividual), intent(inout) :: Comp
end subroutine DetermineSaltContent


subroutine CompleteProfileDescription()

end subroutine CompleteProfileDescription

subroutine LoadProfile(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadProfile


real(dp) function CCiniTotalFromTimeToCCini(TempDaysToCCini, TempGDDaysToCCini, L0, L12, L12SF, L123, L1234, GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, fWeed, TheModeCycle)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    real(dp), intent(in) :: fWeed
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiniTotalFromTimeToCCini



subroutine CompleteCropDescription()

end subroutine CompleteCropDescription

subroutine CompleteClimateDescription(ClimateRecord)
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine CompleteClimateDescription

subroutine LoadClim(FullName, ClimateDescription, ClimateRecord)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine LoadClim

subroutine AppendCropFilePerennials(totalname, GenrateTheOnset, GenerateTheEnd, CriterionNrOnset, Day1Onset, Month1Onset, LengthOnset, SuccessiveDaysOnset, OccurrenceOnset, CriterionNrEnd, DayNEnd, MonthNEnd, ExtraYearEnd, LengthEnd, SuccessiveDaysEnd, OccurrenceEnd, ThresholdOnset, ThresholdEnd)
    character(len=STRING_LENGTH), intent(in) :: totalname
    logical, intent(in) :: GenrateTheOnset
    logical, intent(in) :: GenerateTheEnd
    integer(int32), intent(in) :: CriterionNrOnset
    integer(int32), intent(in) :: Day1Onset
    integer(int32), intent(in) :: Month1Onset
    integer(int32), intent(in) :: LengthOnset
    integer(int32), intent(in) :: SuccessiveDaysOnset
    integer(int32), intent(in) :: OccurrenceOnset
    integer(int32), intent(in) :: CriterionNrEnd
    integer(int32), intent(in) :: DayNEnd
    integer(int32), intent(in) :: MonthNEnd
    integer(int32), intent(in) :: ExtraYearEnd
    integer(int32), intent(in) :: LengthEnd
    integer(int32), intent(in) :: SuccessiveDaysEnd
    integer(int32), intent(in) :: OccurrenceEnd
    real(dp), intent(in) :: ThresholdOnset
    real(dp), intent(in) :: ThresholdEnd
end subroutine AppendCropFilePerennials

character(len=STRING_LENGTH) function EndGrowingPeriod(Day1, DayN)
    integer(int32), intent(in) :: Day1
    integer(int32), intent(inout) :: DayN
end function EndGrowingPeriod

subroutine DetermineLinkedSimDay1(CropDay1, SimDay1)
    integer(int32), intent(in) :: CropDay1
    integer(int32), intent(inout) :: SimDay1
end subroutine DetermineLinkedSimDay1

subroutine AdjustCropYearToClimFile(CDay1, CDayN)
    integer(int32), intent(inout) :: CDay1
    integer(int32), intent(inout) :: CDayN
end subroutine AdjustCropYearToClimFile

subroutine AdjustClimRecordTo(CDayN)
    integer(int32), intent(in) :: CDayN
end subroutine AdjustClimRecordTo

subroutine AdjustSimPeriod()

end subroutine AdjustSimPeriod

subroutine DetermineRootZoneWC(RootingDepth, ZtopSWCconsidered)
    real(dp), intent(in) :: RootingDepth
    logical, intent(inout) :: ZtopSWCconsidered
end subroutine DetermineRootZoneWC


subroutine ReadCropSettingsParameters()

end subroutine ReadCropSettingsParameters

subroutine ReadFieldSettingsParameters()

end subroutine ReadFieldSettingsParameters

subroutine ReadTemperatureSettingsParameters()

end subroutine ReadTemperatureSettingsParameters


real(dp) function SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini, L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, CO2i, TheModeCycle)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: CO2i
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function SeasonalSumOfKcPot

subroutine TranslateIniLayersToSWProfile(NrLay, LayThickness, LayVolPr, LayECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLay
    type(rep_IniComp), intent(in) :: LayThickness
    type(rep_IniComp), intent(in) :: LayVolPr
    type(rep_IniComp), intent(in) :: LayECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniLayersToSWProfile


subroutine TranslateIniPointsToSWProfile(NrLoc, LocDepth, LocVolPr, LocECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLoc
    type(rep_IniComp), intent(in) :: LocDepth
    type(rep_IniComp), intent(in) :: LocVolPr
    type(rep_IniComp), intent(in) :: LocECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniPointsToSWProfile

subroutine LoadInitialConditions(SWCiniFileFull, IniSurfaceStorage)
    character(len=STRING_LENGTH), intent(in) :: SWCiniFileFull
    real(dp), intent(inout) :: IniSurfaceStorage
end subroutine LoadInitialConditions


subroutine CheckForKeepSWC(FullNameProjectFile, TotalNrOfRuns, RunWithKeepSWC, ConstZrxForRun)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    integer(int32), intent(in) :: TotalNrOfRuns
    logical, intent(inout) :: RunWithKeepSWC
    real(dp), intent(inout) :: ConstZrxForRun
end subroutine CheckForKeepSWC

subroutine AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
    integer(int8), intent(in) :: PrevNrComp
    type(rep_IniComp), intent(in) :: PrevThickComp
    type(rep_IniComp), intent(in) :: PrevVolPrComp
    type(rep_IniComp), intent(in) :: PrevECdSComp
end subroutine AdjustThetaInitial

subroutine AdjustSizeCompartments(CropZx)
    real(dp), intent(in) :: CropZx
end subroutine AdjustSizeCompartments

subroutine CheckForWaterTableInProfile(DepthGWTmeter, ProfileComp, WaterTableInProfile)
    real(dp), intent(in) :: DepthGWTmeter
    type(rep_comp), intent(in) :: ProfileComp
    logical, intent(inout) :: WaterTableInProfile
end subroutine CheckForWaterTableInProfile

subroutine LoadGroundWater(FullName, AtDayNr, Zcm, ECdSm)
    character(len=STRING_LENGTH), intent(in) :: FullName
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine LoadGroundWater


subroutine GetFileForProgramParameters(TheFullFileNameProgram, FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: TheFullFileNameProgram
    character(len=STRING_LENGTH), intent(inout) :: FullFileNameProgramParameters
end subroutine GetFileForProgramParameters

subroutine LoadProgramParametersProject(FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: FullFileNameProgramParameters
end subroutine LoadProgramParametersProject



IMPLEMENTATION

subroutine NoManagementOffSeason()

end subroutine NoManagementOffSeason

integer(int32) :: Nri
OffSeasonDescription = 'No specific off-season conditions'
! mulches
SetManagement_SoilCoverBefore(0)
SetManagement_SoilCoverAfter(0)
SetManagement_EffectMulchOffS(50)
! off-season irrigation
SetSimulParam_IrriFwOffSeason(100)
SetIrriECw_PreSeason(0.0) ! dS/m
do Nri = 1, 5 
    SetIrriBeforeSeason_DayNr(Nri, 0)
    SetIrriBeforeSeason_Param(Nri, 0)
end do
SetIrriECw_PostSeason(0.0) ! dS/m
do Nri = 1, 5 
    SetIrriAfterSeason_DayNr(Nri, 0)
    SetIrriAfterSeason_Param(Nri, 0)
end do
! NoManagementOffSeason 



subroutine LoadOffSeason(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadOffSeason

type(TextFile) :: f0
integer(int32) :: Nri, NrEvents1, NrEvents2
character(len=STRING_LENGTH) :: ParamString
real(dp) :: Par1, Par2
real(dp) :: VersionNr
real(dp) :: PreSeason_in
real(dp) :: PostSeason_in
integer(int8) :: TempShortInt, simul_irri_of
Assign(f0, FullName)
Reset(f0)
READLN(f0, OffSeasonDescription)
READLN(f0, VersionNr) ! AquaCrop Version
! mulches
READLN(f0, TempShortInt)
SetManagement_SoilCoverBefore(TempShortInt)
READLN(f0, TempShortInt)
SetManagement_SoilCoverAfter(TempShortInt)
READLN(f0, TempShortInt)
SetManagement_EffectMulchOffS(TempShortInt)

! irrigation events - initialise
do Nri = 1, 5 
    SetIrriBeforeSeason_DayNr(Nri, 0)
    SetIrriBeforeSeason_Param(Nri, 0)
    SetIrriAfterSeason_DayNr(Nri, 0)
    SetIrriAfterSeason_Param(Nri, 0)
end do
READLN(f0, NrEvents1) ! number of irrigation events BEFORE growing period
if (nint(10*VersionNr) < 32) then ! irrigation water quality BEFORE growing period
    SetIrriECw_PreSeason(0.0)
else
    READLN(f0, PreSeason_in)
    SetIrriECw_PreSeason(PreSeason_in)
end if
READLN(f0, NrEvents2) ! number of irrigation events AFTER growing period
if (nint(10*VersionNr) < 32) then ! irrigation water quality AFTER growing period
    SetIrriECw_PostSeason(0.0)
else
    READLN(f0, PostSeason_in)
    SetIrriECw_PostSeason(PostSeason_in)
end if
READLN(f0, simul_irri_of) ! percentage of soil surface wetted
SetSimulParam_IrriFwOffSeason(simul_irri_of)
! irrigation events - get events before and after season
if (NrEvents1 > 0) .or. (NrEvents2 > 0) then
    do Nri = 1, 3 
        READLN(f0) ! title
    end do
    if (NrEvents1 > 0) then
        do Nri = 1, NrEvents1 
            ! events BEFORE growing period
            READLN(f0, ParamString)
            SplitStringInTwoParams(ParamString, Par1, Par2)
            SetIrriBeforeSeason_DayNr(Nri, nint(Par1))
            SetIrriBeforeSeason_Param(Nri, nint(Par2))
        end do
        if (NrEvents2 > 0) then
            do Nri = 1, NrEvents2 
                ! events AFTER growing period
                READLN(f0, ParamString)
                SplitStringInTwoParams(ParamString, Par1, Par2)
                SetIrriAfterSeason_DayNr(Nri, nint(Par1))
                SetIrriAfterSeason_Param(Nri, nint(Par2))
            end do
            Close(f0)
        end do
        ! LoadOffSeason 
    end do


subroutine CalculateAdjustedFC(DepthAquifer, CompartAdj)
    real(dp), intent(in) :: DepthAquifer
    type(rep_Comp), intent(inout) :: CompartAdj
end subroutine CalculateAdjustedFC

integer(int32) :: compi, ic
real(dp) :: Zi, Depth, DeltaV, DeltaFC, Xmax

real(dp) function NoAdjustment(FCvolPr)
    real(dp), intent(in) :: FCvolPr
end function NoAdjustment

real(dp) :: pF
if (FCvolPr <= 10) then
    NoAdjustment = 1
else
    if (FCvolPr >= 30) then
        NoAdjustment = 2
    else
        pF = 2 + 0.3 * (FCvolPr-10)/20
        NoAdjustment = (exp(pF*log(10)))/100
    end if
end if
! NoAdjustment 

!
! Depth := 0;
! FOR compi := 1 TO NrCompartments DO
!     Depth := Depth + CompartAdj[compi].Thickness;
!     Zi := Depth - CompartAdj[compi].Thickness/2;
!     IF ((DepthAquifer < 0)
!         OR ((DepthAquifer - Zi) >= 2)
!         OR (SoilLayer[CompartAdj[compi].Layer].FC >= SoilLayer[CompartAdj[compi].Layer].SAT)) THEN
!         CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC
!     ELSE
!         IF (Zi >= DepthAquifer) THEN
!             CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].SAT
!         ELSE
!             DeltaV := SoilLayer[CompartAdj[compi].Layer].SAT - SoilLayer[CompartAdj[compi].Layer].FC;
!             DeltaFC := (DeltaV/4) * (Zi - (DepthAquifer - 2)) * (Zi - (DepthAquifer - 2));
!             CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC + DeltaFC;
!         end if
!     end if
!     ;  *)
    
    
    Depth = 0
end if
do compi = 1, GetNrCompartments() 
    Depth = Depth + CompartAdj(compi)%Thickness
end do
compi = GetNrCompartments()
REPEAT
Zi = Depth - CompartAdj(compi)%Thickness/2
! Xmax := NoAdjustment(SoilLayer[CompartAdj[compi].Layer].SoilClass);
Xmax = NoAdjustment(GetSoilLayer_i(CompartAdj(compi)%Layer)%FC)
if ((DepthAquifer < 0) .or. ((DepthAquifer - Zi) >= Xmax)) then
    do ic = 1, compi 
        CompartAdj(ic)%FCadj = GetSoilLayer_i(CompartAdj(ic)%Layer)%FC
    end do
    compi = 0
else
    if (GetSoilLayer_i(CompartAdj(compi)%Layer)%FC >= GetSoilLayer_i(CompartAdj(compi)%Layer)%SAT) then
        CompartAdj(compi)%FCadj = GetSoilLayer_i(CompartAdj(compi)%Layer)%FC
    else
        if (Zi >= DepthAquifer) then
            CompartAdj(compi)%FCadj = GetSoilLayer_i(CompartAdj(compi)%Layer)%SAT
        else
            DeltaV = GetSoilLayer_i(CompartAdj(compi)%Layer)%SAT - GetSoilLayer_i(CompartAdj(compi)%Layer)%FC
            DeltaFC = (DeltaV/Sqr(Xmax)) * Sqr(Zi - (DepthAquifer - Xmax))
            CompartAdj(compi)%FCadj = GetSoilLayer_i(CompartAdj(compi)%Layer)%FC + DeltaFC
        end if
    end if
    Depth = Depth - CompartAdj(compi)%Thickness
    compi = compi - 1
end if
UNTIL (compi < 1)
!  CalculateAdjustedFC 


subroutine DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_Comp), intent(inout) :: Compartment
end subroutine DesignateSoilLayerToCompartments

integer(int32) :: i, layeri, compi
real(dp) :: depth, depthi
logical :: finished, NextLayer
depth = 0
depthi = 0
layeri = 1
compi = 1
REPEAT
depth = depth + GetSoilLayer_i(layeri)%Thickness
REPEAT
depthi = depthi + Compartment(compi)%Thickness/2
if (depthi <= depth) then
    Compartment(compi)%Layer = layeri
    NextLayer = .false.
    depthi = depthi + Compartment(compi)%Thickness/2
    compi = compi + 1
    finished = (compi > NrCompartments)
else
    depthi = depthi - Compartment(compi)%Thickness/2
    NextLayer = .true.
    layeri = layeri + 1
    finished = (layeri > NrSoilLayers)
end if
UNTIL finished or NextLayer
UNTIL finished
do i = compi, NrCompartments 
    Compartment(i)%Layer = NrSoilLayers
end do
do i = (NrCompartments+1), max_No_compartments 
    Compartment(i)%Thickness = undef_double
end do
! DesignateSoilLayerToCompartments 


subroutine specify_soil_layer(NrCompartments, NrSoilLayers, SoilLayer, Compartment, TotalWaterContent)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_SoilLayer), intent(inout) :: SoilLayer
    type(rep_Comp), intent(inout) :: Compartment
    type(rep_Content), intent(inout) :: TotalWaterContent
end subroutine specify_soil_layer

integer(int32) :: layeri, compi, celli
real(dp) :: Total

DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)

! Set soil layers and compartments at Field Capacity and determine Watercontent (mm)
! No salinity in soil layers and compartmens
! Absence of ground water table (FCadj = FC)
Total = 0
do layeri = 1, NrSoilLayers 
    SetSoilLayer_WaterContent(layeri, 0)
end do
do compi = 1, NrCompartments 
    Compartment(compi)%Theta = GetSoilLayer_i(Compartment(compi)%Layer)%FC/100
    Compartment(compi)%FCadj = GetSoilLayer_i(Compartment(compi)%Layer)%FC
    Compartment(compi)%DayAnaero = 0
    do celli = 1, GetSoilLayer_i(Compartment(compi)%Layer)%SCP1 
        ! salinity in cells
        Compartment(compi)%Salt(celli) = 0.0
        Compartment(compi)%Depo(celli) = 0.0
    end do
    SetSimulation_ThetaIni_i(compi, Compartment(compi)%Theta)
    SetSimulation_ECeIni_i(compi, 0) ! initial soil salinity in dS/m
    SetSoilLayer_WaterContent(Compartment(compi)%Layer, GetSoilLayer_i(Compartment(compi)%Layer)%WaterContent
    + GetSimulation_ThetaIni_i(compi)*100*10*Compartment(compi)%Thickness)
end do
do layeri = 1, NrSoilLayers 
    Total = Total + GetSoilLayer_i(layeri)%WaterContent
end do
SetTotalWaterContent_BeginDay(Total)

! initial soil water content and no salts
DeclareInitialCondAtFCandNoSalt

! Number of days with RootZone Anaerobic Conditions
SetSimulation_DayAnaero(0)

! specify_soil_layer 


integer(int32) function ActiveCells(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ActiveCells

integer(int32) ::  celi

if (Comp%theta <= GetSoilLayer_i(Comp%Layer)%UL) then
    celi = 0
    do while (Comp%theta > (GetSoilLayer_i(Comp%Layer)%Dx) * celi) 
        celi = celi + 1
    end do
else
    celi = GetSoilLayer_i(Comp%Layer)%SCP1
end do
ActiveCells = celi
! ActiveCells 


subroutine Calculate_Saltmobility(layer, SaltDiffusion, Macro, Mobil)
    integer(int32), intent(in) :: layer
    integer(int8), intent(in) :: SaltDiffusion
    integer(int8), intent(in) :: Macro
    type(rep_salt), intent(inout) :: Mobil
end subroutine Calculate_Saltmobility

integer(int32) :: i, CelMax
real(dp) :: Mix, a, b, xi, yi, UL

Mix = SaltDiffusion/100 ! global salt mobility expressed as a fraction
UL = GetSoilLayer_i(layer)%UL * 100 ! upper limit in VOL% of SC cell 

! 1. convert Macro (vol%) in SaltCelNumber
if (Macro > UL) then
    CelMax = GetSoilLayer_i(layer)%SCP1
else
    CelMax = nint((Macro/UL)*GetSoilLayer_i(layer)%SC)
end if
if (CelMax <= 0) then
    CelMax = 1
end if

! 2. find a and b
if (Mix < 0.5) then
    a = Mix * 2
    b = EXP(10*(0.5-Mix)*LN(10))
else
    a = 2 * (1- Mix)
    b = EXP(10*(Mix-0.5)*LN(10))
end if

! 3. calculate mobility for cells = 1 to Macro
do i = 1, (CelMax-1) 
    xi = i/(CelMax-1)
    if (Mix > 0) then
        if (Mix < 0.5) then
            yi = EXP(LN(a)+xi*LN(b))
            Mobil(i) = (yi-a)/(a*b-a)
        elseif (Mix = 0.5) then
            Mobil(i) = xi
        elseif (Mix < 1) then
            yi = EXP(LN(a)+(1-xi)*LN(b))
            Mobil(i) = 1- (yi-a)/(a*b-a)
        else
            Mobil(i) = 1
        else
            Mobil(i) = 0
        end if
    end if
    
    ! 4. Saltmobility between Macro and SAT
    do i = CelMax, GetSoilLayer_i(layer)%SCP1 
        Mobil(i) = 1
    end do
    
    ! Calculate_Saltmobility 
end do



subroutine DetermineSaltContent(ECe, Comp)
    real(dp), intent(in) :: ECe
    type(CompartmentIndividual), intent(inout) :: Comp
end subroutine DetermineSaltContent

real(dp) :: TotSalt, SumDF, SAT, UL, Dx, mm, mm1, mmN
integer(int32) :: celn, i

TotSalt = ECe*Equiv*(GetSoilLayer_i(Comp%Layer)%SAT)*10*Comp%Thickness
celn = ActiveCells(Comp)
SAT = (GetSoilLayer_i(Comp%Layer)%SAT)/100  ! m3/m3 
UL = GetSoilLayer_i(Comp%Layer)%UL ! m3/m3   ! Upper limit of SC salt cel 
Dx = GetSoilLayer_i(Comp%Layer)%Dx  ! m3/m3  ! Size of salts cel (expect last one) 
mm1 = Dx*1000*Comp%Thickness
* (1 - GetSoilLayer_i(Comp%Layer)%GravelVol/100) ! g/l ! volume [mm]=[l/m2] of cells 
mmN = (SAT-UL)*1000*Comp%Thickness
* (1 - GetSoilLayer_i(Comp%Layer)%GravelVol/100) ! g/l ! volume [mm]=[l/m2] of last cell 
SumDF = 0
do i = 1, GetSoilLayer_i(Comp%Layer)%SCP1 
    Comp%Salt(i) = 0
    Comp%Depo(i) = 0
end do
do i = 1, celn 
    SumDF = SumDF + GetSoilLayer_SaltMobility_i(Comp%Layer, i)
end do
do i = 1, celn 
    Comp%Salt(i) = TotSalt * GetSoilLayer_SaltMobility_i(Comp%Layer, i)/SumDF
    mm = mm1
    if (i = GetSoilLayer_i(Comp%Layer)%SCP1) then
        mm = mmN
    end if
    SaltSolutionDeposit(mm, Comp%Salt(i), Comp%Depo(i))
end if
! DetermineSaltContent 




subroutine CompleteProfileDescription()

end subroutine CompleteProfileDescription

integer(int32) :: i
type(rep_Content) :: TotalWaterContent_temp
type(rep_Comp) :: Compartment_temp
type(SoilLayerIndividual) :: soillayer_i_temp
type(rep_SoilLayer) :: soillayer_temp
do i= (GetSoil()%NrSoilLayers+1), max_SoilLayers 
    soillayer_i_temp = GetSoilLayer_i(i)
    set_layer_undef(soillayer_i_temp)
    SetSoilLayer_i(i, soillayer_i_temp)
end do
SetSimulation_ResetIniSWC(true) ! soil water content and soil salinity
TotalWaterContent_temp = GetTotalWaterContent()
Compartment_temp = GetCompartment()
soillayer_temp = GetSoilLayer()
specify_soil_layer(GetNrCompartments(), GetSoil()%NrSoilLayers, soillayer_temp, Compartment_temp, TotalWaterContent_temp)
SetSoilLayer(soillayer_temp)
SetTotalWaterContent(TotalWaterContent_temp)
SetCompartment(Compartment_temp)
! CompleteProfileDescription 


subroutine LoadProfile(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadProfile

type(TextFile) :: f0
integer(int32) :: i
type(rep_string3) :: blank
real(dp) :: VersionNr
integer(int8) :: TempShortInt
character(len=STRING_LENGTH) :: ProfDescriptionLocal
real(dp) :: thickness_temp, SAT_temp, FC_temp, WP_temp, infrate_temp
real(dp) :: cra_temp, crb_temp, dx_temp
character(len=STRING_LENGTH) :: description_temp
integer(int8) :: penetrability_temp, gravelm_temp
type(rep_salt) :: saltmob_temp
Assign(f0, FullName)
Reset(f0)
READLN(f0, ProfDescriptionLocal)
SetProfDescription(ProfDescriptionLocal)
READLN(f0, VersionNr)  ! AquaCrop version
READLN(f0, TempShortInt)
SetSoil_CNvalue(TempShortInt)
READLN(f0, TempShortInt)
SetSoil_REW(TempShortInt)
SetSimulation_SurfaceStorageIni(0.0)
SetSimulation_ECStorageIni(0.0)
READLN(f0, TempShortInt)
SetSoil_NrSoilLayers(TempShortInt)
READLN(f0) ! depth of restrictive soil layer which is no longer applicable
READLN(f0)
READLN(f0)
! Load characteristics of each soil layer
do i = 1, GetSoil()%NrSoilLayers 
    ! Parameters for capillary rise missing in Versions 3.0 and 3.1
    if (nint(VersionNr*10) < 40) then
        READLN(f0, thickness_temp, SAT_temp, FC_temp,
        WP_temp, infrate_temp, blank, description_temp)
        SetSoilLayer_Thickness(i, thickness_temp)
        SetSoilLayer_SAT(i, SAT_temp)
        SetSoilLayer_FC(i, FC_temp)
        SetSoilLayer_WP(i, WP_temp)
        SetSoilLayer_InfRate(i, infrate_temp)
        SetSoilLayer_Description(i, description_temp)
        ! Default values for Penetrability and Gravel
        SetSoilLayer_Penetrability(i, 100)
        SetSoilLayer_GravelMass(i, 0)
        ! determine volume gravel
        SetSoilLayer_GravelVol(i, 0)
    else
        if (nint(VersionNr*10) < 60) then ! UPDATE required for Version 6.0
            READLN(f0, thickness_temp, SAT_temp, FC_temp, WP_temp, infrate_temp,
            cra_temp, crb_temp, blank, description_temp)
            SetSoilLayer_Thickness(i, thickness_temp)
            SetSoilLayer_SAT(i, SAT_temp)
            SetSoilLayer_FC(i, FC_temp)
            SetSoilLayer_WP(i, WP_temp)
            SetSoilLayer_InfRate(i, infrate_temp)
            SetSoilLayer_CRa(i, cra_temp)
            SetSoilLayer_CRb(i, crb_temp)
            SetSoilLayer_Description(i, description_temp)
            ! Default values for Penetrability and Gravel
            SetSoilLayer_Penetrability(i, 100)
            SetSoilLayer_GravelMass(i, 0)
            ! determine volume gravel
            SetSoilLayer_GravelVol(i, 0)
        else
            READLN(f0, thickness_temp, SAT_temp, FC_temp, WP_temp, infrate_temp,
            penetrability_temp, gravelm_temp, cra_temp, crb_temp, description_temp)
            SetSoilLayer_Thickness(i, thickness_temp)
            SetSoilLayer_SAT(i, SAT_temp)
            SetSoilLayer_FC(i, FC_temp)
            SetSoilLayer_WP(i, WP_temp)
            SetSoilLayer_InfRate(i, infrate_temp)
            SetSoilLayer_Penetrability(i, penetrability_temp)
            SetSoilLayer_GravelMass(i, gravelm_temp)
            SetSoilLayer_CRa(i, cra_temp)
            SetSoilLayer_CRb(i, crb_temp)
            SetSoilLayer_Description(i, description_temp)
            ! determine volume gravel
            SetSoilLayer_GravelVol(i, FromGravelMassToGravelVolume(GetSoilLayer_i(i)%SAT, GetSoilLayer_i(i)%GravelMass))
        end if
    end if
    ! determine drainage coefficient
    SetSoilLayer_tau(i, TauFromKsat(GetSoilLayer_i(i)%InfRate))
    ! determine number of salt cells based on infiltration rate
    if (GetSoilLayer_i(i)%InfRate <= 112) then
        SetSoilLayer_SCP1(i, 11)
    else
        SetSoilLayer_SCP1(i, nint(1.6 + 1000/GetSoilLayer_i(i)%InfRate))
        if (GetSoilLayer_i(i)%SCP1 < 2) then
            SetSoilLayer_SCP1(i, 2)
        end if
        ! determine parameters for soil salinity
        SetSoilLayer_SC(i, GetSoilLayer_i(i)%SCP1 -1)
        SetSoilLayer_Macro(i, nint(GetSoilLayer_i(i)%FC))
        SetSoilLayer_UL(i, ((GetSoilLayer_SAT(i))/100) * (GetSoilLayer_SC(i)/(GetSoilLayer_SC(i)+2))) ! m3/m3 
        dx_temp = (GetSoilLayer_UL(i))/GetSoilLayer_SC(i)
        SetSoilLayer_Dx(i, dx_temp)  ! m3/m3 
        saltmob_temp = GetSoilLayer_i(i)%SaltMobility
        Calculate_SaltMobility(i, GetSimulParam_SaltDiff(), GetSoilLayer_i(i)%Macro, saltmob_temp)
        SetSoilLayer_SaltMobility(i, saltmob_temp)
        ! determine default parameters for capillary rise if missing
        SetSoilLayer_SoilClass(i, NumberSoilClass(GetSoilLayer_i(i)%SAT, GetSoilLayer_i(i)%FC, GetSoilLayer_i(i)%WP, GetSoilLayer_i(i)%InfRate))
        if (nint(VersionNr*10) < 40) then
            cra_temp = GetSoilLayer_i(i)%CRa
            crb_temp = GetSoilLayer_i(i)%CRb
            DetermineParametersCR(GetSoilLayer_i(i)%SoilClass, GetSoilLayer_i(i)%InfRate, cra_temp, crb_temp)
            SetSoilLayer_CRa(i, cra_temp)
            SetSoilLayer_CRb(i, crb_temp)
        end if
    end if
    DetermineNrandThicknessCompartments
    Close(f0)
    SetSoil_RootMax(RootMaxInSoilProfile(GetCrop()%RootMax, GetSoil()%NrSoilLayers, GetSoilLayer()))
    ! Loadprofile
end if


real(dp) function CCiniTotalFromTimeToCCini(TempDaysToCCini, TempGDDaysToCCini, L0, L12, L12SF, L123, L1234, GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, fWeed, TheModeCycle)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    real(dp), intent(in) :: fWeed
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiniTotalFromTimeToCCini


integer(int32) :: DayCC
real(dp) :: SumGDDforCCini, TempCCini
integer(int32) :: Tadj, GDDTadj
if (TempDaysToCCini /= 0) then
    ! regrowth
    SumGDDforCCini = undef_int
    GDDTadj = undef_int
    ! find adjusted calendar and GDD time
    if (TempDaysToCCini = undef_int) then
        ! CCx on 1st day
        Tadj = L12 - L0
        if (TheModeCycle = GDDays) then
            GDDTadj = GDDL12 - GDDL0
        end if
    else
        ! CC on 1st day is < CCx
        Tadj = TempDaysToCCini
        if (TheModeCycle = GDDays) then
            GDDTadj = TempGDDaysToCCini
        end if
    end if
    ! calculate CCini with adjusted time
    DayCC = L0 + Tadj
    if (TheModeCycle = GDDays) then
        SumGDDforCCini = GDDL0 + GDDTadj
    end if
    TempCCini = CCiNoWaterStressSF(DayCC, L0, L12SF, L123, L1234,
    GDDL0, GDDL12SF, GDDL123, GDDL1234,
    (CCo*fWeed), (CCx*fWeed), CGC, GDDCGC,
    (CDC*(fWeed*CCx+2.29)/(CCx+2.29)),
    (GDDCDC*(fWeed*CCx+2.29)/(CCx+2.29)), SumGDDforCCini, RatDGDD,
    SFRedCGC, SFRedCCx, SFCDecline, TheModeCycle)
    ! correction for fWeed is already in TempCCini (since DayCC > 0);
else
    TempCCini = (CCo*fWeed) ! sowing or transplanting
end if

CCiniTotalFromTimeToCCini = TempCCini
! CCiniTotalFromTimeToCCini 



subroutine CompleteCropDescription()

end subroutine CompleteCropDescription

logical :: CGCisGiven
integer(int8) :: FertStress
integer(int8) :: RedCGC_temp, RedCCX_temp
integer(int32) :: Crop_DaysToSenescence_temp
type(rep_int_array) :: Crop_Length_temp
integer(int32) :: Crop_DaysToFullCanopy_temp
real(dp) :: Crop_CGC_temp
integer(int32) :: Crop_DaysToFullCanopySF_temp

if ((GetCrop_subkind() = Vegetative) .or. (GetCrop_subkind() = Forage)) then
    if (GetCrop()%DaysToHIo > 0) then
        if (GetCrop()%DaysToHIo > GetCrop()%DaysToHarvest) then
            SetCrop_dHIdt(GetCrop()%HI/GetCrop()%DaysToHarvest)
        else
            SetCrop_dHIdt(GetCrop()%HI/GetCrop()%DaysToHIo)
        end if
        if (GetCrop()%dHIdt > 100) then
            SetCrop_dHIdt(100)
        end if
    else
        SetCrop_dHIdt(100)
    end if
else
    !  grain or tuber crops
    if (GetCrop()%DaysToHIo > 0) then
        SetCrop_dHIdt(GetCrop()%HI/GetCrop()%DaysToHIo)
    else
        SetCrop_dHIdt(undef_int)
    end if
end if
if (GetCrop_ModeCycle() = CalendarDays) then
    SetCrop_DaysToCCini(TimeToCCini(GetCrop()%Planting, GetCrop()%PlantingDens, GetCrop()%SizeSeedling, GetCrop()%SizePlant, GetCrop()%CCx, GetCrop()%CGC))
    SetCrop_DaysToFullCanopy(DaysToReachCCwithGivenCGC((0.98 * GetCrop()%CCx), GetCrop()%CCo, GetCrop()%CCx, GetCrop()%CGC, GetCrop()%DaysToGermination))
    if (GetManagement_FertilityStress() /= 0) then
        FertStress = GetManagement_FertilityStress()
        Crop_DaysToFullCanopySF_temp = GetCrop()%DaysToFullCanopySF
        RedCGC_temp = GetSimulation_EffectStress_RedCGC()
        RedCCX_temp = GetSimulation_EffectStress_RedCCX()
        TimeToMaxCanopySF(GetCrop()%CCo, GetCrop()%CGC, GetCrop()%CCx,
        GetCrop()%DaysToGermination, GetCrop()%DaysToFullCanopy, GetCrop()%DaysToSenescence,
        GetCrop()%DaysToFlowering, GetCrop()%LengthFlowering, GetCrop()%DeterminancyLinked,
        Crop_DaysToFullCanopySF_temp, RedCGC_temp,
        RedCCX_temp, FertStress)
        SetManagement_FertilityStress(FertStress)
        SetSimulation_EffectStress_RedCGC(RedCGC_temp)
        SetSimulation_EffectStress_RedCCX(RedCCX_temp)
        SetCrop_DaysToFullCanopySF(Crop_DaysToFullCanopySF_temp)
    else
        SetCrop_DaysToFullCanopySF(GetCrop()%DaysToFullCanopy)
    end if
    SetCrop_GDDaysToCCini(undef_int)
    SetCrop_GDDaysToGermination(undef_int)
    SetCrop_GDDaysToFullCanopy(undef_int)
    SetCrop_GDDaysToFullCanopySF(undef_int)
    SetCrop_GDDaysToFlowering(undef_int)
    SetCrop_GDDLengthFlowering(undef_int)
    SetCrop_GDDaysToSenescence(undef_int)
    SetCrop_GDDaysToHarvest(undef_int)
    SetCrop_GDDaysToMaxRooting(undef_int)
    SetCrop_GDDCGC(undef_int)
    SetCrop_GDDCDC(undef_int)
else
    SetCrop_GDDaysToCCini(TimeToCCini(GetCrop()%Planting, GetCrop()%PlantingDens, GetCrop()%SizeSeedling, GetCrop()%SizePlant, GetCrop()%CCx, GetCrop()%GDDCGC))
    SetCrop_DaysToCCini(TimeToCCini(GetCrop()%Planting, GetCrop()%PlantingDens, GetCrop()%SizeSeedling, GetCrop()%SizePlant, GetCrop()%CCx, GetCrop()%CGC))
    SetCrop_GDDaysToFullCanopy(DaysToReachCCwithGivenCGC((0.98 * GetCrop()%CCx), GetCrop()%CCo, GetCrop()%CCx, GetCrop()%GDDCGC, GetCrop()%GDDaysToGermination))
    ! Crop.GDDaysToFullCanopySF is determined in RUN or ManagementUnit if required
end if

CGCisGiven = .true. ! required to adjust Crop.DaysToFullCanopy (does not exist)
Crop_DaysToSenescence_temp = GetCrop()%DaysToSenescence
Crop_Length_temp = GetCrop()%Length
Crop_DaysToFullCanopy_temp = GetCrop()%DaysToFullCanopy
Crop_CGC_temp = GetCrop()%CGC
DetermineLengthGrowthStages(GetCrop()%CCo, GetCrop()%CCx, GetCrop()%CDC, GetCrop()%DaysToGermination, GetCrop()%DaysToHarvest, CGCisGiven,
GetCrop()%DaysToCCini, GetCrop()%Planting, Crop_DaysToSenescence_temp,
Crop_Length_temp, Crop_DaysToFullCanopy_temp, Crop_CGC_temp)
SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp)
SetCrop_Length(Crop_Length_temp)
SetCrop_DaysToFullCanopy(Crop_DaysToFullCanopy_temp)
SetCrop_CGC(Crop_CGC_temp)

SetCrop_CCoAdjusted(GetCrop()%CCo)
SetCrop_CCxAdjusted(GetCrop()%CCx)
SetCrop_CCxWithered(GetCrop()%CCx)
SetSumWaBal_Biomass(0)
SetSumWaBal_BiomassPot(0)
SetSumWaBal_BiomassUnlim(0)
SetSumWaBal_BiomassTot(0) ! crop and weeds (for soil fertility stress)
SetSumWaBal_YieldPart(0)
SetSimulation_EvapLimitON(false)
! CompleteCropDescription 



subroutine CompleteClimateDescription(ClimateRecord)
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine CompleteClimateDescription

character(len=STRING_LENGTH) :: dayStr, yearStr
integer(int32) :: Deci
DetermineDayNr(ClimateRecord%FromD, ClimateRecord%FromM, ClimateRecord%FromY, ClimateRecord%FromDayNr)
CASE ClimateRecord%DataType OF
    Daily    :
    ClimateRecord%ToDayNr = ClimateRecord%FromDayNr + ClimateRecord%NrObs - 1
    DetermineDate(ClimateRecord%ToDayNr, ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY)
end case
Decadely :
Deci = nint((ClimateRecord%FromD+9)/10) + ClimateRecord%NrObs - 1
ClimateRecord%ToM = ClimateRecord%FromM
ClimateRecord%ToY = ClimateRecord%FromY
do while (Deci > 3) 
    Deci = Deci - 3
    ClimateRecord%ToM = ClimateRecord%ToM + 1
    if (ClimateRecord%ToM > 12) then
        ClimateRecord%ToM = 1
        ClimateRecord%ToY = ClimateRecord%ToY  + 1
    end if
end if
ClimateRecord%ToD = 10
if (Deci = 2) then
    ClimateRecord%ToD = 20
end if
if (Deci = 3) then
    ClimateRecord%ToD = DaysInMonth(ClimateRecord%ToM)
    if ((ClimateRecord%ToM = 2) .and. LeapYear(ClimateRecord%ToY)) then
        ClimateRecord%ToD = ClimateRecord%ToD + 1
    end if
end if
DetermineDayNr(ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY, ClimateRecord%ToDayNr)
Monthly  :
ClimateRecord%ToY = ClimateRecord%FromY
ClimateRecord%ToM = ClimateRecord%FromM + ClimateRecord%NrObs - 1
do while (ClimateRecord%ToM > 12) 
    ClimateRecord%ToY = ClimateRecord%ToY + 1
    ClimateRecord%ToM = ClimateRecord%ToM - 12
end do
ClimateRecord%ToD = DaysInMonth(ClimateRecord%ToM)
if ((ClimateRecord%ToM = 2) .and. LeapYear(ClimateRecord%ToY)) then
    ClimateRecord%ToD = ClimateRecord%ToD + 1
end if
DetermineDayNr(ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY, ClimateRecord%ToDayNr)
Str(ClimateRecord%FromD:2, dayStr)
if (ClimateRecord%FromY = 1901 then
    yearStr = ''
else
    Str(ClimateRecord%FromY:4, yearStr)
end if
ClimateRecord%FromString = CONCAT(dayStr, ' ', NameMonth(ClimateRecord%FromM), ' ', yearStr)
Str(ClimateRecord%ToD:2, dayStr)
if (ClimateRecord%FromY = 1901 then
    yearStr = ''
else
    Str(ClimateRecord%ToY:4, yearStr)
end if
ClimateRecord%ToString = CONCAT(dayStr, ' ', NameMonth(ClimateRecord%ToM), ' ', yearStr)
! CompleteClimateDescription 


subroutine LoadClim(FullName, ClimateDescription, ClimateRecord)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine LoadClim

type(TextFile) :: f0
integer(int32) :: Ni
Assign(f0, FullName)
Reset(f0)
READLN(f0, CLimateDescription)
READLN(f0, Ni)
if (Ni = 1) then
    ClimateRecord%DataType = Daily
elseif (Ni = 2) then
    ClimateRecord%DataType = Decadely
else
    ClimateRecord%DataType = Monthly
end if
READLN(f0, ClimateRecord%FromD)
READLN(f0, ClimateRecord%FromM)
READLN(f0, ClimateRecord%FromY)
READLN(f0)
READLN(f0)
READLN(f0)
ClimateRecord%NrObs = 0
do while (NOT Eof(f0) 
    ClimateRecord%NrObs = ClimateRecord%NrObs + 1
    READLN(f0)
end do
Close(f0)
CompleteClimateDescription(ClimateRecord)
! LoadClim



subroutine AppendCropFilePerennials(totalname, GenrateTheOnset, GenerateTheEnd, CriterionNrOnset, Day1Onset, Month1Onset, LengthOnset, SuccessiveDaysOnset, OccurrenceOnset, CriterionNrEnd, DayNEnd, MonthNEnd, ExtraYearEnd, LengthEnd, SuccessiveDaysEnd, OccurrenceEnd, ThresholdOnset, ThresholdEnd)
    character(len=STRING_LENGTH), intent(in) :: totalname
    logical, intent(in) :: GenrateTheOnset
    logical, intent(in) :: GenerateTheEnd
    integer(int32), intent(in) :: CriterionNrOnset
    integer(int32), intent(in) :: Day1Onset
    integer(int32), intent(in) :: Month1Onset
    integer(int32), intent(in) :: LengthOnset
    integer(int32), intent(in) :: SuccessiveDaysOnset
    integer(int32), intent(in) :: OccurrenceOnset
    integer(int32), intent(in) :: CriterionNrEnd
    integer(int32), intent(in) :: DayNEnd
    integer(int32), intent(in) :: MonthNEnd
    integer(int32), intent(in) :: ExtraYearEnd
    integer(int32), intent(in) :: LengthEnd
    integer(int32), intent(in) :: SuccessiveDaysEnd
    integer(int32), intent(in) :: OccurrenceEnd
    real(dp), intent(in) :: ThresholdOnset
    real(dp), intent(in) :: ThresholdEnd
end subroutine AppendCropFilePerennials

type(TextFile) :: f
character(len=STRING_LENGTH) :: TempString
Assign(f, totalname)
Append(f)
! 1. Title
WRITELN(f) ! empty line
WRITELN(f, ' Internal crop calendar')
WRITELN(f, ' ========================================================')

! 2. ONSET
! criterion number
if (GenrateTheOnset == .false.) then
    CriterionNrOnset = 0
    TempString = '         : The Restart of growth is fixed on a specific date'
else
    CASE CriterionNrOnset OF
        12 : TempString = '         : The Restart of growth is generated by average air temperature'
    end case
    13 : TempString = '         : The Restart of growth is generated by Growing-degree days'
else
    ! no valid criterion number
    GenrateTheOnset = .false.
    CriterionNrOnset = 0
    TempString = '         : The Restart of growth is fixed on a specific date'
end case
WRITELN(f, CriterionNrOnset:6, TempString)

! parameters ONSET
if (GenrateTheOnset == .false.) then
    WRITELN(f, Day1Onset:6, '         : First Day of growth')
    WRITELN(f, Month1Onset:6, '         : First Month of growth')
    WRITELN(f, LengthOnset:6, '         : Length (days) for the time window (Restart of growth): Not Applicable')
    WRITELN(f, ThresholdOnset:8:1, '       : Threshold for the Restart criterion: Not Applicable')
    WRITELN(f, SuccessiveDaysOnset:6, '         : Number of successive days for the Restart criterion: Not Applicable')
    WRITELN(f, OccurrenceOnset:6, '         : Number of occurrences before the Restart criterion applies: Not Applicable')
else
    WRITELN(f, Day1Onset:6, '         : First Day for the time window (Restart of growth)')
    WRITELN(f, Month1Onset:6, '         : First Month for the time window (Restart of growth)')
    WRITELN(f, LengthOnset:6, '         : Length (days) of the time window (Restart of growth)')
    CASE CriterionNrOnset OF
        12 : TempString = '       : Threshold for the Restart criterion: Average air temperature (degC)'
    end case
    13 : TempString = '       : Threshold for the Restart criterion: Growing-degree days'
end case
WRITELN(f, ThresholdOnset:8:1, TempString)
WRITELN(f, SuccessiveDaysOnset:6, '         : Number of successive days for the Restart criterion')
if (OccurrenceOnset > 3) then
    OccurrenceOnset = 3
end if
WRITELN(f, OccurrenceOnset:6, '         : Number of occurrences before the Restart criterion applies')

! 3. END of growing period
! criterion number
if (GenerateTheEnd == .false.) then
    CriterionNrEnd = 0
    TempString = '         : The End of growth is fixed on a specific date'
else
    CASE CriterionNrEnd OF
        62 : TempString = '         : The End of growth is generated by average air temperature'
    end case
    63 : TempString = '         : The End of growth is generated by Growing-degree days'
else
    ! no valid criterion number
    GenerateTheEnd = .false.
    CriterionNrEnd = 0
    TempString = '         : The End of growth is fixed on a specific date'
end case
WRITELN(f, CriterionNrEnd:6, TempString)

! parameters END of growing period
if (GenerateTheEnd == .false.) then
    WRITELN(f, DayNEnd:6, '         : Last Day of growth')
    WRITELN(f, MonthNEnd:6, '         : Last Month of growth')
    WRITELN(f, ExtraYearEnd:6, '         : Number of years to add to the Restart year')
    WRITELN(f, LengthEnd:6, '         : Length (days) for the time window (End of growth): Not Applicable')
    WRITELN(f, ThresholdEnd:8:1, '       : Threshold for the End criterion: Not Applicable')
    WRITELN(f, SuccessiveDaysEnd:6, '         : Number of successive days for the End criterion: Not Applicable')
    WRITELN(f, OccurrenceEnd:6, '         : Number of occurrences before the End criterion applies: Not Applicable')
else
    WRITELN(f, DayNEnd:6, '         : Last Day for the time window (End of growth)')
    WRITELN(f, MonthNEnd:6, '         : Last Month for the time window (End of growth)')
    WRITELN(f, ExtraYearEnd:6, '         : Number of years to add to the Onset year')
    WRITELN(f, LengthEnd:6, '         : Length (days) of the time window (End of growth)')
    CASE CriterionNrEnd OF
        62 : TempString = '       : Threshold for the End criterion: average air temperature (degC)'
    end case
    63 : TempString = '       : Threshold for the End criterion: Growing-degree days'
end case
WRITELN(f, ThresholdEnd:8:1, TempString)
WRITELN(f, SuccessiveDaysEnd:6, '         : Number of successive days for the End criterion')
if (OccurrenceEnd > 3) then
    OccurrenceEnd = 3
end if
WRITELN(f, OccurrenceEnd:6, '         : Number of occurrences before the End criterion applies')
Close(f)
! AppendCropFilePerennials 


character(len=STRING_LENGTH) function EndGrowingPeriod(Day1, DayN)
    integer(int32), intent(in) :: Day1
    integer(int32), intent(inout) :: DayN
end function EndGrowingPeriod

integer(int32) :: dayi, monthi, yeari
character(len=STRING_LENGTH) :: Strday, StrMonth
! This function determines Crop.DayN and the string
DayN = Day1 + GetCrop()%DaysToHarvest - 1
if (DayN < Day1) then
    DayN = Day1
end if
DetermineDate(DayN, dayi, monthi, yeari)
Str(dayi:2, Strday)
StrMonth = NameMonth(monthi)
EndGrowingPeriod = CONCAT(Strday, ' ', StrMonth, '  ')
! EndGrowingPeriod 


subroutine DetermineLinkedSimDay1(CropDay1, SimDay1)
    integer(int32), intent(in) :: CropDay1
    integer(int32), intent(inout) :: SimDay1
end subroutine DetermineLinkedSimDay1

SimDay1 = CropDay1
if (GetClimFile() /= '(None)') then
    !
!     IF SimDay1 < GetClimRecord_FromDayNr() THEN
!         SimDay1 := GetClimRecord_FromDayNr();
!     end if
!     IF SimDay1 > GetClimRecord_ToDayNr() THEN
!         Simulation.LinkCropToSimPeriod := false;
!         SimDay1 := GetClimRecord_FromDayNr();
!         ; *)
        if ((SimDay1 < GetClimRecord_FromDayNr()) .or. (SimDay1 > GetClimRecord_ToDayNr())) then
            SetSimulation_LinkCropToSimPeriod(false)
            SimDay1 = GetClimRecord_FromDayNr()
        end if
    end if
    ! DetermineLinkedSimDay1 
end if


subroutine AdjustCropYearToClimFile(CDay1, CDayN)
    integer(int32), intent(inout) :: CDay1
    integer(int32), intent(inout) :: CDayN
end subroutine AdjustCropYearToClimFile

integer(int32) :: dayi, monthi, yeari
character(len=STRING_LENGTH) :: temp_str
DetermineDate(CDay1, dayi, monthi, yeari)
if (GetClimFile() = '(None)') then
    yeari = 1901  ! yeari = 1901 if undefined year
else
    yeari = GetClimRecord_FromY() ! yeari = 1901 if undefined year
end if
!
! ELSE
! yeari := Simulation.YearStartCropCycle;
! IF (CDay1 > GetClimRecord_ToY()) THEN
!     yeari := GetClimRecord_FromY();
! end if
! ; *)
DetermineDayNr(dayi, monthi, yeari, CDay1)
temp_str = EndGrowingPeriod(CDay1, CDayN)
! AdjustCropYearToClimFile 


subroutine AdjustClimRecordTo(CDayN)
    integer(int32), intent(in) :: CDayN
end subroutine AdjustClimRecordTo

integer(int32) :: dayi, monthi, yeari
integer(int32) :: ToDayNr_tmp
DetermineDate(CDayN, dayi, monthi, yeari)
SetClimRecord_ToD(31)
SetClimRecord_ToM(12)
SetClimRecord_ToY(yeari)
DetermineDayNr(GetClimRecord_ToD(), GetClimRecord_ToM(), GetClimRecord_ToY(), ToDayNr_tmp)
SetClimRecord_ToDayNr(ToDayNr_tmp)
! AdjustClimRecordTo 


subroutine AdjustSimPeriod()

end subroutine AdjustSimPeriod

integer(int32) :: IniSimFromDayNr
character(len=STRING_LENGTH) :: FullFileName
integer(int32) :: FromDayNr_temp, ZiAqua_temp
type(rep_Comp) :: Compartment_temp

IniSimFromDayNr = GetSimulation_FromDayNr()
CASE GetSimulation_LinkCropToSimPeriod() OF
    true :
    FromDayNr_temp = GetSimulation_FromDayNr()
    DetermineLinkedSimDay1(GetCrop()%Day1, FromDayNr_temp)
    SetSimulation_FromDayNr(FromDayNr_temp)
    if (GetCrop()%Day1 = GetSimulation_FromDayNr()) then
        SetSimulation_ToDayNr(GetCrop()%DayN)
    else
        SetSimulation_ToDayNr(GetSimulation_FromDayNr() + 30) ! 30 days
    end if
    if (GetClimFile() /= '(None)') then
        if (GetSimulation_ToDayNr() > GetClimRecord_ToDayNr()) then
            SetSimulation_ToDayNr(GetClimRecord_ToDayNr())
        end if
        if (GetSimulation_ToDayNr() < GetClimRecord_FromDayNr()) then
            SetSimulation_ToDayNr(GetClimRecord_FromDayNr())
        end if
    end if
end if
false :
!
! IF ((GetClimFile() <> '(None)') AND (Simulation.FromDayNr < GetClimRecord_FromDayNr())) THEN
!     Simulation.FromDayNr := GetClimRecord_FromDayNr();
!     Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
!     ; *)
    if (GetSimulation_FromDayNr() > GetCrop()%Day1) then
        SetSimulation_FromDayNr(GetCrop()%Day1)
    end if
    SetSimulation_ToDayNr(GetCrop()%DayN)
end if
if ((GetClimFile() /= '(None)') AN
    ((GetSimulation_FromDayNr() <= GetClimRecord_FromDayNr()) .or. (GetSimulation_FromDayNr() >= GetClimRecord_ToDayNr()))) then
    SetSimulation_FromDayNr(GetClimRecord_FromDayNr())
    SetSimulation_ToDayNr(GetSimulation_FromDayNr() + 30) ! 30 days
end if

! adjust initial depth and quality of the groundwater when required
if ((NOT GetSimulParam_ConstGwt()) .and. (IniSimFromDayNr /= GetSimulation_FromDayNr())) then
    if (GetGroundWaterFile() = '(None)') then
        FullFileName = CONCAT(GetPathNameProg(), 'GroundWater%AqC')
    else
        FullFileName = GetGroundWaterFileFull()
    end if
    ! initialize ZiAqua and ECiAqua
    ZiAqua_temp = GetZiAqua()
    LoadGroundWater(FullFileName, GetSimulation_FromDayNr(), ZiAqua_temp, ECiAqua)
    SetZiAqua(ZiAqua_temp)
    Compartment_temp = GetCompartment()
    CalculateAdjustedFC((GetZiAqua()/100), Compartment_temp)
    SetCompartment(Compartment_temp)
    if (GetSimulation_IniSWC_AtFC() then
        ResetSWCToFC
    end if
end if
! AdjustSimPeriod 


subroutine DetermineRootZoneWC(RootingDepth, ZtopSWCconsidered)
    real(dp), intent(in) :: RootingDepth
    logical, intent(inout) :: ZtopSWCconsidered
end subroutine DetermineRootZoneWC

real(dp) :: CumDepth, Factor, frac_value, DrRel, DZtopRel, TopSoilInMeter
integer(int32) :: compi
! calculate SWC in root zone
CumDepth = 0
compi = 0
SetRootZoneWC_Actual(0)
SetRootZoneWC_FC(0)
SetRootZoneWC_WP(0)
SetRootZoneWC_SAT(0)
SetRootZoneWC_Leaf(0)
SetRootZoneWC_Thresh(0)
SetRootZoneWC_Sen(0)
REPEAT
compi = compi + 1
CumDepth = CumDepth + GetCompartment_Thickness(compi)
if (CumDepth <= RootingDepth) then
    Factor = 1
else
    frac_value = RootingDepth - (CumDepth - GetCompartment_Thickness(compi))
    if (frac_value > 0) then
        Factor = frac_value/GetCompartment_Thickness(compi)
    else
        Factor = 0
    end if
end if
SetRootZoneWC_Actual(GetRootZoneWC()%Actual
+ Factor * 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_FC(GetRootZoneWC()%FC
+ Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi))%FC * GetCompartment_Thickness(compi)
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_Leaf(GetRootZoneWC()%Leaf
+ Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC
- GetCrop()%pLeafAct * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC-GetSoilLayer_i(GetCompartment_Layer(compi))%WP))
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_Thresh(GetRootZoneWC()%Thresh
+ Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC
- GetCrop()%pActStom * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC-GetSoilLayer_i(GetCompartment_Layer(compi))%WP))
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_Sen(GetRootZoneWC()%Sen
+ Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC
- GetCrop()%pSenAct * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC-GetSoilLayer_i(GetCompartment_Layer(compi))%WP))
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_WP(GetRootZoneWC()%WP
+ Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi))%WP * GetCompartment_Thickness(compi)
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
SetRootZoneWC_SAT(GetRootZoneWC()%SAT
+ Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi))%SAT * GetCompartment_Thickness(compi)
* (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
UNTIL (CumDepth >= RootingDepth) .or. (compi = GetNrCompartments())

! calculate SWC in top soil (top soil in meter = SimulParam.ThicknessTopSWC/100)
if ((RootingDepth*100) <= GetSimulParam_ThicknessTopSWC()) then
    SetRootZoneWC_ZtopAct(GetRootZoneWC()%Actual)
    SetRootZoneWC_ZtopFC(GetRootZoneWC()%FC)
    SetRootZoneWC_ZtopWP(GetRootZoneWC()%WP)
    SetRootZoneWC_ZtopThresh(GetRootZoneWC()%Thresh)
else
    CumDepth = 0
    compi = 0
    SetRootZoneWC_ZtopAct(0)
    SetRootZoneWC_ZtopFC(0)
    SetRootZoneWC_ZtopWP(0)
    SetRootZoneWC_ZtopThresh(0)
    TopSoilInMeter = GetSimulParam_ThicknessTopSWC()/100
    REPEAT
    compi = compi + 1
    CumDepth = CumDepth + GetCompartment_Thickness(compi)
    if ((CumDepth*100) <= GetSimulParam_ThicknessTopSWC()) then
        Factor = 1
    else
        frac_value = TopSoilInMeter - (CumDepth - GetCompartment_Thickness(compi))
        if (frac_value > 0) then
            Factor = frac_value/GetCompartment_Thickness(compi)
        else
            Factor = 0
        end if
    end if
    SetRootZoneWC_ZtopAct(GetRootZoneWC()%ZtopAct
    + Factor * 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
    SetRootZoneWC_ZtopFC(GetRootZoneWC()%ZtopFC
    + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi))%FC * GetCompartment_Thickness(compi)
    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
    SetRootZoneWC_ZtopWP(GetRootZoneWC()%ZtopWP
    + Factor * 10 * GetSoilLayer_i(GetCompartment_Layer(compi))%WP * GetCompartment_Thickness(compi)
    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
    SetRootZoneWC_ZtopThresh(GetRootZoneWC()%ZtopThresh
    + Factor * 10 * GetCompartment_Thickness(compi) * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC
    - GetCrop()%pActStom * (GetSoilLayer_i(GetCompartment_Layer(compi))%FC-GetSoilLayer_i(GetCompartment_Layer(compi))%WP))
    * (1 - GetSoilLayer_i(GetCompartment_Layer(compi))%GravelVol/100))
    UNTIL (CumDepth >= TopSoilInMeter) .or. (compi = GetNrCompartments())
end if

! Relative depletion in rootzone and in top soil
if (nint(1000*(GetRootZoneWc()%FC - GetRootZoneWc()%WP)) > 0 then
    DrRel = (GetRootZoneWc()%FC - GetRootZoneWC()%Actual)/(GetRootZoneWc()%FC - GetRootZoneWc()%WP)
else
    DrRel = 0
end if
if (nint(1000*(GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopWP)) > 0 then
    DZtopRel = (GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopAct)/(GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopWP)
else
    DZtopRel = 0
end if

! Zone in soil profile considered for determining stress response
if (DZtopRel < DrRel) then
    ZtopSWCconsidered = .true.  ! top soil is relative wetter than root zone
else
    ZtopSWCconsidered = .false.
end if
! DetermineRootZoneWC 



subroutine ReadCropSettingsParameters()

end subroutine ReadCropSettingsParameters

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
integer(int8) :: simul_ed, simul_pCCHIf, simul_SFR, simul_TAWg, simul_beta, simul_Tswc
real(dp) :: simul_kcWB, simul_RZEma, simul_pfao, simul_expFsen
integer(int32) :: simul_RpZmi, simul_lowox
FullName = CONCAT(GetPathNameSimul(), 'Crop%PAR')
Assign(f, FullName)
Reset(f)
READLN(f, simul_ed) ! evaporation decline factor in stage 2
SetSimulParam_EvapDeclineFactor(simul_ed)
READLN(f, simul_kcWB) ! Kc wet bare soil [-]
SetSimulParam_KcWetBare(simul_kcWB)
READLN(f, simul_pCCHIf) ! CC threshold below which HI no longer increase(% of 100)
SetSimulParam_PercCCxHIfinal(simul_pCCHIf)
READLN(f, simul_RpZmi) ! Starting depth of root sine function (% of Zmin)
SetSimulParam_RootPercentZmin(simul_RpZmi)
READLN(f, simul_RZEma) ! cm/day
SetSimulParam_MaxRootZoneExpansion(simul_RZEma)
SetSimulParam_MaxRootZoneExpansion(5.00) ! fixed at 5 cm/day
READLN(f, simul_SFR) ! Shape factor for effect water stress on rootzone expansion
SetSimulParam_KsShapeFactorRoot(simul_SFR)
READLN(f, simul_TAWg)  ! Soil water content (% TAW) required at sowing depth for germination
SetSimulParam_TAWGermination(simul_TAWg)
READLN(f, simul_pfao) ! Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
SetSimulParam_pAdjFAO(simul_pfao)
READLN(f, simul_lowox) ! number of days for full effect of deficient aeration
SetSimulParam_DelayLowOxygen(simul_lowox)
READLN(f, simul_expFsen) ! exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
SetSimulParam_ExpFsen(simul_expFsen)
READLN(f, simul_beta) ! Decrease (percentage) of p(senescence) once early canopy senescence is triggered
SetSimulParam_Beta(simul_beta)
READLN(f, simul_Tswc) ! Thickness top soil (cm) in which soil water depletion has to be determined
SetSimulParam_ThicknessTopSWC(simul_Tswc)
Close(f)
! ReadCropSettingsParameters 


subroutine ReadFieldSettingsParameters()

end subroutine ReadFieldSettingsParameters

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
integer(int8) :: simul_evmax
FullName = CONCAT(GetPathNameSimul(), 'Field%PAR')
Assign(f, FullName)
Reset(f)
READLN(f, simul_evmax) ! maximum water extraction depth by soil evaporation [cm]
SetSimulParam_EvapZmax(simul_evmax)
Close(f)
! ReadFieldSettingsParameters 


subroutine ReadTemperatureSettingsParameters()

end subroutine ReadTemperatureSettingsParameters

type(text) :: f0
character(len=STRING_LENGTH) :: FullName
integer(int8) :: simul_GDD
real(dp) :: simul_Tmi, simul_Tma
FullName = CONCAT(GetPathNameSimul(), 'Temperature%PAR')
Assign(f0, FullName)
Reset(f0)
READLN(f0)
READLN(f0, simul_Tmi)   ! Default minimum temperature (degC) if no temperature file is specified
SetSimulParam_Tmin(simul_Tmi)
READLN(f0, simul_Tma)   ! Default maximum temperature (degC) if no temperature file is specified
SetSimulParam_Tmax(simul_Tma)
READLN(f0, simul_GDD) ! Default method for GDD calculations
SetSimulParam_GDDMethod(simul_GDD)
if (GetSimulParam_GDDMethod() > 3) then
    SetSimulParam_GDDMethod(3)
end if
if (GetSimulParam_GDDMethod() < 1) then
    SetSimulParam_GDDMethod(1)
end if
Close(f0)
! ReadTemperatureSettingsParameters 




real(dp) function SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini, L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, CO2i, TheModeCycle)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: CO2i
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function SeasonalSumOfKcPot

EToStandard = 5
real(dp) :: SumGDD, GDDi, SumKcPot, SumGDDforPlot, SumGDDfromDay1
real(dp) :: Tndayi, Txdayi, CCi, CCxWitheredForB, TpotForB, EpotTotForB
real(dp) :: CCinitial, DayFraction, GDDayFraction
integer(int32) :: DayCC, Tadj, GDDTadj
type(textFile) :: fTemp
integer(int32) :: Dayi
logical :: GrowthON

! 1. Open Temperature file
if (GetTemperatureFile() /= '(None)') then
    Assign(fTemp, CONCAT(GetPathNameSimul(), 'TCrop%SIM'))
    Reset(fTemp)
end if

! 2. Initialise global settings
SetSimulation_DelayedDays(0) ! required for CalculateETpot
SumKcPot = 0
SumGDDforPlot = undef_int
SumGDD = undef_int
SumGDDfromDay1 = 0
GrowthON = .false.
GDDTadj = undef_int
DayFraction = undef_int
GDDayFraction = undef_int
! 2.bis Initialise 1st day
if (TheDaysToCCini /= 0) then
    ! regrowth
    if (TheDaysToCCini = undef_int) then
        ! CCx on 1st day
        Tadj = L12 - L0
        if (TheModeCycle = GDDays) then
            GDDTadj = GDDL12 - GDDL0
            SumGDD = GDDL12
        end if
        CCinitial = CCx
    else
        ! CC on 1st day is < CCx
        Tadj = TheDaysToCCini
        DayCC = Tadj + L0
        if (TheModeCycle = GDDays) then
            GDDTadj = TheGDDaysToCCini
            SumGDD = GDDL0 + TheGDDaysToCCini
            SumGDDforPlot = SumG
        end if
        CCinitial = CanopyCoverNoStressSF(DayCC, L0, L123, L1234,
        GDDL0, GDDL123, GDDL1234, CCo, CCx, CGC, CDC,
        GDDCGC, GDDCDC, SumGDDforPlot, TheModeCycle, (0), (0))
    end if
    ! Time reduction for days between L12 and L123
    DayFraction = (L123-L12)/(Tadj + L0 + (L123-L12) )
    if (TheModeCycle = GDDays) then
        GDDayFraction = (GDDL123-GDDL12)/(GDDTadj + GDDL0 + (GDDL123-GDDL12))
    end if
else
    ! sowing or transplanting
    Tadj = 0
    if (TheModeCycle = GDDays) then
        GDDTadj = 0
        SumGDD = 0
    end if
    CCinitial = CCo
end if

! 3. Calculate Sum
do Dayi = 1, L1234 
    ! 3.1 calculate growing degrees for the day
    if (GetTemperatureFile() /= '(None)') then
        READLN(fTemp, Tndayi, Txdayi)
        GDDi = DegreesDay(Tbase, Tupper, Tndayi, Txdayi, GetSimulParam_GDDMethod())
    else
        GDDi = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, GetSimulParam_GDDMethod())
    end if
    if (TheModeCycle = GDDays) then
        SumGDD = SumGDD + GDDi
        SumGDDfromDay1 = SumGDDfromDay1 + GDDi
    end if
    
    ! 3.2 calculate CCi
    if (GrowthON == .false.) then
        ! not yet canopy development
        CCi = 0
        DayCC = Dayi
        if (TheDaysToCCini /= 0) then
            ! regrowth on 1st day
            CCi = CCinitial
            GrowthON = .true.
        else
            ! wait for day of germination or recover of transplant
            if (TheModeCycle = CalendarDays) then
                if (Dayi = (L0+1)) then
                    CCi = CCinitial
                    GrowthON = .true.
                end if
            else
                if (SumGDD > GDDL0) then
                    CCi = CCinitial
                    GrowthON = .true.
                end if
            end if
        end if
    else
        if (TheDaysToCCini = 0) then
            DayCC = Dayi
        else
            DayCC = Dayi + Tadj + L0 ! adjusted time scale
            if (DayCC > L1234) then
                DayCC = L1234 ! special case where L123 > L1234
            end if
            if (DayCC > L12) then
                if (Dayi <= L123) then
                    DayCC = L12 + nint(DayFraction * (Dayi+Tadj+L0 - L12)) ! slow down
                else
                    DayCC = Dayi ! switch time scale
                end if
            end if
        end if
        if (TheModeCycle = GDDays) then
            if (TheGDDaysToCCini = 0) then
                SumGDDforPlot = SumGDDfromDay1
            else
                SumGDDforPlot = SumG
                if (SumGDDforPlot > GDDL1234) then
                    SumGDDforPlot = GDDL1234 ! special case where L123 > L1234
                end if
                if (SumGDDforPlot > GDDL12) then
                    if (SumGDDfromDay1 <= GDDL123) then
                        SumGDDforPlot = GDDL12 + nint(GDDayFraction * (SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12)) ! slow down
                    else
                        SumGDDforPlot = SumGDDfromDay1 ! switch time scale
                    end if
                end if
                CCi = CanopyCoverNoStressSF(DayCC, L0, L123, L1234, GDDL0, GDDL123, GDDL1234,
                CCo, CCx, CGC, CDC, GDDCGC, GDDCDC, SumGDDforPlot, TheModeCycle, (0), (0))
            end if
            
            ! 3.3 calculate CCxWithered
            CCxWitheredForB = CCi
            if (Dayi >= L12) then
                CCxWitheredForB = CCx
            end if
            
            ! 3.4 Calculate Tpot + Adjust for Low temperature (no transpiration)
            if (CCi > 0.0001) then
                CalculateETpot(DayCC, L0, L12, L123, L1234, (0),
                CCi, EToStandard, KcTop, KcDeclAgeing, CCx, CCxWitheredForB, CCeffectProcent, CO2i, GDDi, GDtranspLow, TpotForB, EpotTotForB)
            else
                TpotForB = 0
            end if
            
            ! 3.5 Sum of Sum Of KcPot
            SumKcPot = SumKcPot + (TpotForB/EToStandard)
        end if
        
        ! 5. Close Temperature file
        if (GetTemperatureFile() /= '(None)') then
            Close(fTemp)
        end if
        
        ! 6. final sum
        SeasonalSumOfKcPot = SumKcPot
        
        ! SeasonalSumOfKcPot 
    end if





subroutine TranslateIniLayersToSWProfile(NrLay, LayThickness, LayVolPr, LayECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLay
    type(rep_IniComp), intent(in) :: LayThickness
    type(rep_IniComp), intent(in) :: LayVolPr
    type(rep_IniComp), intent(in) :: LayECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniLayersToSWProfile

integer(int8) :: Compi, Layeri, i
real(dp) :: SDLay, SDComp, FracC
logical :: GoOn

! from specific layers to Compartments
do Compi = 1, NrComp 
    Comp(Compi)%Theta = 0
    Comp(Compi)%WFactor = 0  ! used for ECe in this procedure
end do
Compi = 0
SDComp = 0
Layeri = 1
SDLay = LayThickness(1)
GoOn = .true.
do while (Compi < NrComp) 
    FracC = 0
    Compi = Compi + 1
    SDComp = SDComp + Comp(compi)%Thickness
    if (SDLay >= SDComp) then
        Comp(Compi)%Theta = Comp(Compi)%Theta + (1-FracC)*LayVolPr(Layeri)/100
        Comp(Compi)%WFactor = Comp(Compi)%WFactor + (1-FracC)*LayECdS(Layeri)
    else
        ! go to next layer
        do while ((SDLay < SDComp) .and. GoOn) 
            ! finish handling previous layer
            FracC = (SDLay - (SDComp-Comp(Compi)%Thickness))/(Comp(Compi)%Thickness) - FracC
            Comp(Compi)%Theta = Comp(Compi)%Theta + FracC*LayVolPr(Layeri)/100
            Comp(Compi)%WFactor = Comp(Compi)%WFactor + FracC*LayECdS(Layeri)
            FracC = (SDLay - (SDComp-Comp(Compi)%Thickness))/(Comp(Compi)%Thickness)
            ! add next layer
            if (Layeri < NrLay) then
                Layeri = Layeri + 1
                SDLay = SDLay + LayThickness(Layeri)
            else
                GoOn = .false.
            end if
        end if
        Comp(Compi)%Theta = Comp(Compi)%Theta + (1-FracC)*LayVolPr(Layeri)/100
        Comp(Compi)%WFactor = Comp(Compi)%WFactor + (1-FracC)*LayECdS(Layeri)
    end if
    ! next Compartment
end if
if (NOT GoOn) then
    do i = (Compi+1), NrComp 
        Comp(i)%Theta = LayVolPr(NrLay)/100
        Comp(i)%WFactor = LayECdS(NrLay)
    end do
    
    ! final check of SWC
    do Compi = 1, NrComp 
        if (Comp(Compi)%Theta > (GetSoilLayer_i(Comp(compi)%Layer)%SAT)/100) then
            Comp(Compi)%Theta = (GetSoilLayer_i(Comp(compi)%Layer)%SAT)/100
        end if
        ! salt distribution in cellls
        do Compi = 1, NrComp 
            DetermineSaltContent(Comp(Compi)%WFactor, Comp(Compi))
        end do
        ! TranslateIniLayersToSWProfile 
    end do



subroutine TranslateIniPointsToSWProfile(NrLoc, LocDepth, LocVolPr, LocECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLoc
    type(rep_IniComp), intent(in) :: LocDepth
    type(rep_IniComp), intent(in) :: LocVolPr
    type(rep_IniComp), intent(in) :: LocECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniPointsToSWProfile

integer(int8) :: Compi, Loci
real(dp) :: TotD, Depthi, D1, D2, Th1, Th2, DTopComp, ThTopComp, ThBotComp
real(dp) :: EC1, EC2, ECTopComp, ECBotComp
logical :: AddComp, TheEnd
TotD = 0
do Compi = 1, NrComp 
    Comp(Compi)%Theta = 0
    Comp(Compi)%WFactor = 0  ! used for salt in (10*VolSat*dZ * EC)
    TotD = TotD + Comp(Compi)%Thickness
end do
Compi = 0
Depthi = 0
AddComp = .true.
Th2 = LocVolPr(1)
EC2 = LocECds(1)
D2 = 0
Loci = 0
do while ((Compi < NrComp) .or. ((Compi = NrComp) .and. (AddComp == .false.))) 
    ! upper and lower boundaries location
    D1 = D2
    Th1 = Th2
    EC1 = EC2
    if (Loci < NrLoc) then
        Loci = Loci + 1
        D2 = LocDepth(Loci)
        Th2 = LocVolPr(Loci)
        EC2 = LocECdS(Loci)
    else
        D2 = Tot
    end if
    ! transfer water to compartment (SWC in mm) and salt in (10*VolSat*dZ * EC)
    TheEnd = .false.
    DTopComp = D1  ! Depthi is the bottom depth
    ThBotComp = Th1
    ECBotComp = EC1
    REPEAT
    ThTopComp = ThBotComp
    ECTopComp = ECBotComp
    if (AddComp then
        Compi = Compi + 1
        Depthi = Depthi + Comp(Compi)%Thickness
    end if
    if (Depthi < D2) then
        ThBotComp = Th1 + (Th2-Th1)*(Depthi-D1)/(D2-D1)
        Comp(Compi)%Theta = Comp(Compi)%Theta
        + 10*(Depthi-DTopComp)*((ThTopComp+ThBotComp)/2)
        ECBotComp = EC1 + (EC2-EC1)*(Depthi-D1)/(D2-D1)
        Comp(Compi)%WFactor = Comp(Compi)%WFactor
        + (10*(Depthi-DTopComp)*GetSoilLayer_i(Comp(Compi)%Layer)%SAT)*((ECTopComp+ECbotComp)/2)
        AddComp = .true.
        DTopComp = Depthi
        if (Compi = NrComp) then
            TheEnd = .true.
        end if
    else
        ThBotComp = Th2
        ECBotComp = EC2
        Comp(Compi)%Theta = Comp(Compi)%Theta
        + 10*(D2-DTopComp)*((ThTopComp+ThBotComp)/2)
        Comp(Compi)%WFactor = Comp(Compi)%WFactor
        + (10*(D2-DTopComp)*GetSoilLayer_i(Comp(Compi)%Layer)%SAT)*((ECTopComp+ECbotComp)/2)
        if (Depthi = D2) then
            AddComp = .true.
        else
            AddComp = .false.
        end if
        TheEnd = .true.
    end if
    UNTIL TheEnd
end if

do Compi = 1, NrComp 
    ! from mm(water) to theta and final check
    Comp(Compi)%Theta = Comp(Compi)%Theta/(1000*Comp(Compi)%Thickness)
    if (Comp(Compi)%Theta > (GetSoilLayer_i(Comp(compi)%Layer)%SAT)/100) then
        Comp(Compi)%Theta = (GetSoilLayer_i(Comp(compi)%Layer)%SAT)/100
    end if
    if (Comp(Compi)%Theta < 0) then
        Comp(Compi)%Theta = 0
    end if
end if

do Compi = 1, NrComp 
    ! from (10*VolSat*dZ * EC) to ECe and distribution in cellls
    Comp(Compi)%WFactor = Comp(Compi)%WFactor/(10*Comp(Compi)%Thickness*GetSoilLayer_i(Comp(Compi)%Layer)%SAT)
    DetermineSaltContent(Comp(Compi)%WFactor, Comp(Compi))
end do
! TranslateIniPointsToSWProfile 


subroutine LoadInitialConditions(SWCiniFileFull, IniSurfaceStorage)
    character(len=STRING_LENGTH), intent(in) :: SWCiniFileFull
    real(dp), intent(inout) :: IniSurfaceStorage
end subroutine LoadInitialConditions

type(TextFile) :: f0
integer(int8) :: i
character(len=STRING_LENGTH) :: StringParam, swcinidescr_temp
real(dp) :: VersionNr
real(dp) :: CCini_temp, Bini_temp, Zrini_temp, ECStorageIni_temp
integer(int8) :: NrLoc_temp
real(dp) :: Loc_i_temp, VolProc_i_temp, SaltECe_i_temp
! IniSWCRead attribute of the function was removed to fix a
! bug occurring when the function was called in TempProcessing.pas
! Keep in mind that this could affect the graphical interface
Assign(f0, SWCiniFileFull)
Reset(f0)
READLN(f0, swcinidescr_temp)
setSWCiniDescription(swcinidescr_temp)
READLN(f0, VersionNr) ! AquaCrop Version
if (nint(10*VersionNr) < 41) then ! initial CC at start of simulation period
    SetSimulation_CCini(undef_int)
else
    READLN(f0, CCini_temp)
    SetSimulation_CCini(CCini_temp)
end if
if (nint(10*VersionNr) < 41) then ! B produced before start of simulation period
    SetSimulation_Bini(0.000)
else
    READLN(f0, Bini_temp)
    SetSimulation_Bini(Bini_temp)
end if
if (nint(10*VersionNr) < 41) then ! initial rooting depth at start of simulation period
    SetSimulation_Zrini(undef_int)
else
    READLN(f0, Zrini_temp)
    SetSimulation_Zrini(Zrini_temp)
end if
READLN(f0, IniSurfaceStorage)
if (nint(10*VersionNr) < 32) then ! EC of the ini surface storage
    SetSimulation_ECStorageIni(0)
else
    READLN(f0, ECStorageIni_temp)
    SetSimulation_ECStorageIni(ECStorageIni_temp)
end if
READLN(f0, i)
if (i = 1) then
    SetSimulation_IniSWC_AtDepths(true)
else
    SetSimulation_IniSWC_AtDepths(false)
end if
READLN(f0, NrLoc_temp)
SetSimulation_IniSWC_NrLoc(NrLoc_temp)
READLN(f0)
READLN(f0)
READLN(f0)
do i = 1, GetSimulation_IniSWC_NrLoc() 
    READLN(f0, StringParam)
    Loc_i_temp = GetSimulation_IniSWC_Loc_i(i)
    VolProc_i_temp = GetSimulation_IniSWC_VolProc_i(i)
    if (nint(10*VersionNr) < 32) then ! ECe at the locations
        SplitStringInTwoParams(StringParam, Loc_i_temp, VolProc_i_temp)
        SetSimulation_IniSWC_SaltECe_i(i, 0)
    else
        SaltECe_i_temp = GetSimulation_IniSWC_SaltECe_i(i)
        SplitStringInThreeParams(StringParam, Loc_i_temp, VolProc_i_temp, SaltECe_i_temp)
        SetSimulation_IniSWC_SaltECe_i(i, SaltECe_i_temp)
    end if
    SetSimulation_IniSWC_Loc_i(i, Loc_i_temp)
    SetSimulation_IniSWC_VolProc_i(i, VolProc_i_temp)
end if
Close(f0)
SetSimulation_IniSWC_AtFC(false)
! LoadInitialConditions 


subroutine CheckForKeepSWC(FullNameProjectFile, TotalNrOfRuns, RunWithKeepSWC, ConstZrxForRun)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    integer(int32), intent(in) :: TotalNrOfRuns
    logical, intent(inout) :: RunWithKeepSWC
    real(dp), intent(inout) :: ConstZrxForRun
end subroutine CheckForKeepSWC

type(TextFile) :: f0, fx
integer(int32) :: i, Runi
character(len=STRING_LENGTH) :: FileName, PathName, FullFileName
real(dp) :: Zrni, Zrxi, ZrSoili
real(dp) :: VersionNrCrop
integer(int8) :: TheNrSoilLayers
type(rep_SoilLayer) :: TheSoilLayer
character(len=STRING_LENGTH) :: PreviousProfFilefull

! 1. Initial settings
RunWithKeepSWC = .false.
ConstZrxForRun = undef_int

! 2. Open project file
Assign(f0, FullNameProjectFile)
Reset(f0)
READLN(f0) ! Description
READLN(f0)  ! AquaCrop version Nr
do i = 1, 5 
    READLN(f0) ! Type Year, and Simulation and Cropping period of run 1
end do

! 3. Look for restrictive soil layer
! restricted to run 1 since with KeepSWC, the soil file has to be common between runs
PreviousProfFilefull = GetProfFilefull() ! keep name soil file (to restore after check)
do i = 1, 27 
    READLN(f0) ! Climate (5x3 = 15),Calendar (3),Crop (3), Irri (3) and Field (3) file
end do
READLN(f0) ! info Soil file
READLN(f0, FileName)
READLN(f0, PathName)
PathName = StringReplace(PathName, '"', '', (rfReplaceAll))
FullFileName = CONCAT(Trim(PathName), Trim(FileName))
LoadProfile(FullFileName)
TheNrSoilLayers = GetSoil()%NrSoilLayers
TheSoilLayer = GetSoilLayer()
! ZrRestrict := 1000; ! assumed not to be so far a restriction
!
! Assign(fx,FullFileName);
! Reset(fx);
! FOR i := 1 TO 5 DO
!     READLN(fx);
! end do
! READLN(fx,ZrRestrict); // depth restrictive soil layer inhibiting root zone expansion
! IF (ZrRestrict < 0) THEN
!     ZrRestrict := 1000 // undefined, HENCE very large (no restriction)
! ELSEIF (ZrRestrict <= 1.20) THEN
!     ConstZrxForRun := ZrRestrict;
! end if
! Close(fx); *)

! 3.bis  groundwater file
do i = 1, 3 
    READLN(f0)
end do

! 4. Check if runs with KeepSWC exist
Runi = 1
do while (RunWithKeepSWC == .false.) .and. (Runi <= TotalNrOfRuns) 
    if (Runi > 1) then
        do i = 1, 47 
            READLN(f0)  ! 5 + 42 lines with files
        end do
        READLN(f0) ! info Initial Conditions file
    end do
    READLN(f0, FileName)
    READLN(f0) ! Pathname
    if (Trim(FileName) = 'KeepSWC') then
        RunWithKeepSWC = .true.
    end if
    Runi = Runi + 1
end if
if (RunWithKeepSWC == .false.) then
    ConstZrxForRun = undef_int ! reset
end if

! 5. Look for maximum root zone depth IF RunWithKeepSWC
! IF (RunWithKeepSWC AND (ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict))) THEN
if (RunWithKeepSWC == .true.) then
    Reset(f0)
    READLN(f0) ! Description
    READLN(f0)  ! AquaCrop version Nr
    do i = 1, 5 
        READLN(f0) ! Type Year, and Simulation and Cropping period of run 1
    end do
    Runi = 1
    ! WHILE ((ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict)) AND (Runi <= TotalNrOfRuns)) DO
    do while (Runi <= TotalNrOfRuns) 
        ! Simulation and Cropping period
        if (Runi > 1) then
            do i = 1, 5 
                READLN(f0)
            end do
            ! 5 Climate files (15) + Calendar file (3)
            do i = 1, 18 
                READLN(f0)
            end do
            ! Crop file
            READLN(f0) ! Crop file title
        end do
        READLN(f0, FileName)
        READLN(f0, PathName)
        PathName = StringReplace(PathName, '"', '', (rfReplaceAll))
        FullFileName = CONCAT(Trim(PathName), Trim(FileName))
        Assign(fx, FullFileName)
        Reset(fx)
        READLN(fx) ! description
        READLN(fx, VersionNrCrop)
        if (Round(VersionNrCrop*10) <= 31) then
            do i = 1, 29 
                READLN(fx)  ! no Salinity stress (No Reponse Stomata + ECemin + ECemax + ShapeKsSalt)
            else
                if (Round(VersionNrCrop*10) <= 50) then
                    do i = 1, 32 
                        READLN(fx) ! no distortion to salinity and response to ECsw factor
                    else
                        do i = 1, 34 
                            READLN(fx)
                        end do
                    end do
                    READLN(fx, Zrni) ! minimum rooting depth
                end do
                READLN(fx, Zrxi) ! maximum rooting depth
                ZrSoili = RootMaxInSoilProfile(Zrxi, TheNrSoilLayers, TheSoilLayer)
                if (ZrSoili > ConstZrxForRun) then
                    ConstZrxForRun = ZrSoili
                end if
                Close(fx)
                ! Remaining files: Irri (3), Field (3), Soil (3), Gwt (3), Inni (3), Off (3) and FieldData (3) file
                do i = 1, 21 
                    READLN(f0)
                end do
                Runi = Runi + 1
            end do
        end do
        Close(f0)
        
        ! 6. Reload existing soil file
        SetProfFilefull(PreviousProfFilefull)
        LoadProfile(GetProfFilefull())
        ! CheckForKeepSWC 
    end do




subroutine AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
    integer(int8), intent(in) :: PrevNrComp
    type(rep_IniComp), intent(in) :: PrevThickComp
    type(rep_IniComp), intent(in) :: PrevVolPrComp
    type(rep_IniComp), intent(in) :: PrevECdSComp
end subroutine AdjustThetaInitial

integer(int32) :: layeri, compi
real(dp) :: TotDepthC, TotDepthL, Total
type(rep_Comp) :: Compartment_temp

! 1. Actual total depth of compartments
TotDepthC = 0
do compi = 1, GetNrCompartments() 
    TotDepthC = TotDepthC + GetCompartment_Thickness(compi)
end do

! 2. Stretch thickness of bottom soil layer if required
TotDepthL = 0
do layeri = 1 to GetSoil()%NrSoilLayers 
    TotDepthL = TotDepthL + GetSoilLayer_i(layeri)%Thickness
end do
if (TotDepthC > TotDepthL) then
    SetSoilLayer_Thickness(GetSoil()%NrSoilLayers, GetSoilLayer_i(GetSoil()%NrSoilLayers)%Thickness + (TotDepthC - TotDepthL))
end if

! 3. Assign a soil layer to each soil compartment
Compartment_temp = GetCompartment()
DesignateSoilLayerToCompartments(GetNrCompartments(), GetSoil()%NrSoilLayers, Compartment_temp)
SetCompartment(Compartment_temp)

! 4. Adjust initial Soil Water Content of soil compartments
if (GetSimulation_ResetIniSWC() then
    if (GetSimulation_IniSWC_AtDepths() then
        Compartment_temp = GetCompartment()
        TranslateIniPointsToSWProfile(GetSimulation_IniSWC_NrLoc(), GetSimulation_IniSWC_Loc(), GetSimulation_IniSWC_VolProc(),
        GetSimulation_IniSWC_SaltECe(), GetNrCompartments(), Compartment_temp)
        SetCompartment(Compartment_temp)
    else
        Compartment_temp = GetCompartment()
        TranslateIniLayersToSWProfile(GetSimulation_IniSWC_NrLoc(), GetSimulation_IniSWC_Loc(), GetSimulation_IniSWC_VolProc(),
        GetSimulation_IniSWC_SaltECe(), GetNrCompartments(), Compartment_temp)
        SetCompartment(Compartment_temp)
    end if
else
    Compartment_temp = GetCompartment()
    TranslateIniLayersToSWProfile(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp, GetNrCompartments(), Compartment_temp)
    SetCompartment(Compartment_temp)
end if

! 5. Adjust watercontent in soil layers and determine ThetaIni
Total = 0
do layeri = 1, GetSoil()%NrSoilLayers 
    SetSoilLayer_WaterContent(layeri, 0)
end do
do compi = 1, GetNrCompartments() 
    SetSimulation_ThetaIni_i(compi, GetCompartment_Theta(compi))
    SetSoilLayer_WaterContent(GetCompartment_Layer(compi), GetSoilLayer_i(GetCompartment_Layer(compi))%WaterContent
    + GetSimulation_ThetaIni_i(compi)*100*10*GetCompartment_Thickness(compi))
end do
do layeri = 1, GetSoil()%NrSoilLayers 
    Total = Total + GetSoilLayer_i(layeri)%WaterContent
end do
SetTotalWaterContent_BeginDay(Total)
! AdjustThetaInitial 




subroutine AdjustSizeCompartments(CropZx)
    real(dp), intent(in) :: CropZx
end subroutine AdjustSizeCompartments

integer(int32) :: i , compi
real(dp) :: TotDepthC, fAdd
integer(int8) :: PrevNrComp
type(rep_IniComp) :: PrevThickComp, PrevVolPrComp, PrevECdSComp

! 1. Save intial soil water profile (required when initial soil water profile is NOT reset at start simulation - see 7.)
PrevNrComp = GetNrCompartments()
do compi = 1, prevnrComp 
    PrevThickComp(compi) = GetCompartment_Thickness(compi)
    PrevVolPrComp(compi) = 100*GetCompartment_Theta(compi)
end do

! 2. Actual total depth of compartments
TotDepthC = 0
do i = 1, GetNrCompartments() 
    TotDepthC = TotDepthC + GetCompartment_Thickness(compi)
end do

! 3. Increase number of compartments (if less than 12)
if (GetNrCompartments() < 12) then
    REPEAT
    SetNrCompartments(GetNrCompartments() + 1)
end if
if ((CropZx - TotDepthC) > GetSimulParam_CompDefThick()) then
    SetCompartment_Thickness(GetNrCompartments(), GetSimulParam_CompDefThick())
else
    SetCompartment_Thickness(GetNrCompartments(), CropZx - TotDepthC)
end if
TotDepthC = TotDepthC + GetCompartment_Thickness(GetNrCompartments())
UNTIL ((GetNrCompartments() = max_No_compartments) .or. ((TotDepthC + 0.00001) >= CropZx))

! 4. Adjust size of compartments (if total depth of compartments < rooting depth)
if ((TotDepthC + 0.00001) < CropZx) then
    SetNrCompartments(12)
    fAdd = (CropZx/0.1 - 12)/78
    do i = 1, 12 
        SetCompartment_Thickness(i, 0.1 * (1 + i*fAdd))
        SetCompartment_Thickness(i, 0.05 * nint(GetCompartment_Thickness(i) * 20))
    end do
    TotDepthC = 0
    do i = 1, GetNrCompartments() 
        TotDepthC = TotDepthC + GetCompartment_Thickness(i)
    end do
    if (TotDepthC < CropZx) then
        REPEAT
        SetCompartment_Thickness(12, GetCompartment_Thickness(12) + 0.05)
    end if
    TotDepthC = TotDepthC + 0.05
    UNTIL (TotDepthC >= CropZx)
else
    do while ((TotDepthC - 0.04999999) >= CropZx) 
        SetCompartment_Thickness(12, GetCompartment_Thickness(12) - 0.05)
        TotDepthC = TotDepthC - 0.05
    end do
end do

! 5. Adjust soil water content and theta initial
AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
! AdjustSizeCompartments 


subroutine CheckForWaterTableInProfile(DepthGWTmeter, ProfileComp, WaterTableInProfile)
    real(dp), intent(in) :: DepthGWTmeter
    type(rep_comp), intent(in) :: ProfileComp
    logical, intent(inout) :: WaterTableInProfile
end subroutine CheckForWaterTableInProfile

real(dp) :: Ztot, Zi
integer(int32) :: compi
WaterTableInProfile = .false.
Ztot = 0
compi = 0
if (DepthGWTmeter >= 0) then
    ! groundwater table is present
    REPEAT
    compi = compi + 1
end if
Ztot = Ztot + ProfileComp(compi)%Thickness
Zi = Ztot - ProfileComp(compi)%Thickness/2
if (Zi >= DepthGWTmeter) then
    WaterTableInProfile = .true.
end if
UNTIL ((WaterTableInProfile == .true.) .or. (compi >= GetNrCompartments()))
! CheckForWaterTableInProfile 




subroutine LoadGroundWater(FullName, AtDayNr, Zcm, ECdSm)
    character(len=STRING_LENGTH), intent(in) :: FullName
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine LoadGroundWater

type(TextFile) :: f0
integer(int32) :: i, dayi, monthi, yeari, Year1Gwt
integer(int32) :: DayNr1Gwt, DayNr1, DayNr2, DayNrN
type(ShortString) :: StringREAD
real(dp) :: DayDouble, Z1, EC1, Z2, EC2, ZN, ECN
logical :: TheEnd

subroutine FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(in) :: DayNr1
    integer(int32), intent(in) :: DayNr2
    real(dp), intent(in) :: Z1
    real(dp), intent(in) :: EC1
    real(dp), intent(in) :: Z2
    real(dp), intent(in) :: EC2
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine FindValues

Zcm = nint(100 * (Z1 + (Z2-Z1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1)))
ECdSm = EC1 + (EC2-EC1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1)
! FindValues 

! initialize
TheEnd = .false.
Year1Gwt = 1901
DayNr1 = 1
DayNr2 = 1
Assign(f0, FullName)
Reset(f0)
READLN(f0, GroundWaterDescription)
READLN(f0) ! AquaCrop Version

! mode groundwater table
READLN(f0, i)
CASE i OF
    0 :
    ! no groundwater table
    Zcm = undef_int
    ECdSm = undef_int
    SetSimulParam_ConstGwt(true)
    TheEnd = .true.
end case
1 :
! constant groundwater table
SetSimulParam_ConstGwt(true)
else
SetSimulParam_ConstGwt(false)

! first day of observations (only for variable groundwater table)
if (NOT GetSimulParam_ConstGwt()) then
    READLN(f0, dayi)
    READLN(f0, monthi)
    READLN(f0, Year1Gwt)
    DetermineDayNr(dayi, monthi, Year1Gwt, DayNr1Gwt)
end if

! single observation (Constant Gwt) or first observation (Variable Gwt)
if (i > 0) then
    ! groundwater table is present
    READLN(f0)
    READLN(f0)
    READLN(f0)
    READLN(f0, StringREAD)
    SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
    if ((i = 1) .or. (Eof(f0))) then
        ! Constant groundwater table or single observation
        Zcm = nint(100*Z2)
        ECdSm = EC2
        TheEnd = .true.
    else
        DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
    end if
end if

! other observations
if (NOT TheEnd) then
    ! variable groundwater table with more than 1 observation
    ! adjust AtDayNr
    DetermineDate(AtDayNr, dayi, monthi, yeari)
    if ((yeari = 1901) .and. (Year1Gwt /= 1901)) then
        ! Make AtDayNr defined
        DetermineDayNr(dayi, monthi, Year1Gwt, AtDayNr)
    end if
    if ((yeari /= 1901) .and. (Year1Gwt = 1901)) then
        ! Make AtDayNr undefined
        DetermineDayNr(dayi, monthi, Year1Gwt, AtDayNr)
    end if
    ! get observation at AtDayNr
    if (Year1Gwt /= 1901) then
        ! year is defined
        if (AtDayNr <= DayNr2) then
            Zcm = nint(100*Z2)
            ECdSm = EC2
        else
            do while (NOT TheEnd) 
                DayNr1 = DayNr2
                Z1 = Z2
                EC1 = EC2
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
                DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
                if (AtDayNr <= DayNr2) then
                    FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
                    TheEnd = .true.
                end if
                if ((Eof(f0)) .and. (NOT TheEnd)) then
                    Zcm = nint(100*Z2)
                    ECdSm = EC2
                    TheEnd = .true.
                end if
            end if
        end if
    else
        ! year is undefined
        if (AtDayNr <= DayNr2) then
            DayNr2 = DayNr2 + 365
            AtDayNr = AtDayNr + 365
            do while (NOT Eof(f0)) 
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z1, EC1)
                DayNr1 = DayNr1Gwt + nint(DayDouble) - 1
            end do
            FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
        else
            DayNrN = DayNr2 + 365
            ZN = Z2
            ECN = EC2
            do while (NOT TheEnd) 
                DayNr1 = DayNr2
                Z1 = Z2
                EC1 = EC2
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
                DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
                if (AtDayNr <= DayNr2) then
                    FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
                    TheEnd = .true.
                end if
                if ((Eof(f0)) .and. (NOT TheEnd)) then
                    FindValues(AtDayNr, DayNr2, DayNrN, Z2, EC2, ZN, ECN, Zcm, ECdSm)
                    TheEnd = .true.
                end if
            end if
        end if
    end if
    ! variable groundwater table with more than 1 observation
end if
Close(f0)
! LoadGroundWater 


subroutine GetFileForProgramParameters(TheFullFileNameProgram, FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: TheFullFileNameProgram
    character(len=STRING_LENGTH), intent(inout) :: FullFileNameProgramParameters
end subroutine GetFileForProgramParameters

integer(int32) :: TheLength
character(len=STRING_LENGTH) :: TheExtension
FullFileNameProgramParameters = ''
TheLength = Length(TheFullFileNameProgram)
TheExtension = Copy(TheFullFileNameProgram, (TheLength-2), 3) ! PRO or PRM
FullFileNameProgramParameters = Copy(TheFullFileNameProgram, 1, (TheLength-3))
if (TheExtension = 'PRO') then
    FullFileNameProgramParameters = CONCAT(FullFileNameProgramParameters, 'PP1')
else
    FullFileNameProgramParameters = CONCAT(FullFileNameProgramParameters, 'PPn')
end if
! GetFileForProgramParameters 


subroutine LoadProgramParametersProject(FullFileNameProgramParameters)
    character(len=*), intent(in) :: FullFileNameProgramParameters

    integer :: fhandle
    integer(int32) :: i, simul_RpZmi, simul_lowox
    integer(int8) :: simul_ed, effrainperc, effrainshow, effrainrootE, &
                     simul_saltdiff, simul_saltsolub, simul_root, simul_pCCHIf, &
                     simul_SFR, simul_TAWg, simul_beta, simul_Tswc, simul_GDD, &
                     simul_EZma
    real(dp) :: simul_rod, simul_kcWB, simul_RZEma, simul_pfao, simul_expFsen, &
                simul_Tmi, simul_Tma
    logical :: file_exists

    inquire(file=trim(FullFileNameProgramParameters), exist=file_exists)
    if (file_exists) then
        ! load set of program parameters
        open(newunit=fhandle, file=trim(FullFileNameProgramParameters), &
             status='old', action='read)
        ! crop
        read(fhandle, *) simul_ed ! evaporation decline factor in stage 2
        call SetSimulParam_EvapDeclineFactor(simul_ed)
        read(fhandle, *) simul_kcWB ! Kc wet bare soil [-]
        call SetSimulParam_KcWetBare(simul_kcWB)
        read(fhandle, *) simul_pCCHIf ! CC threshold below which HI no longer
                                      ! increase(% of 100)
        call SetSimulParam_PercCCxHIfinal(simul_pCCHIf)
        read(fhandle, *) simul_RpZmi ! Starting depth of root sine function 
                                     ! (% of Zmin)
        call SetSimulParam_RootPercentZmin(simul_RpZmi)
        read(fhandle, *) simul_RZEma ! cm/day
        call SetSimulParam_MaxRootZoneExpansion(simul_RZEma)
        call SetSimulParam_MaxRootZoneExpansion(5.00_dp) ! fixed at 5 cm/day
        read(fhandle, *) simul_SFR ! Shape factor for effect water stress 
                                   ! on rootzone expansion
        call SetSimulParam_KsShapeFactorRoot(simul_SFR)
        read(fhandle, *) simul_TAWg  ! Soil water content (% TAW) required 
                                     ! at sowing depth for germination
        call SetSimulParam_TAWGermination(simul_TAWg)
        read(fhandle, *) simul_pfao ! Adjustment factor for FAO-adjustment 
                                    ! soil water depletion (p) for various ET
        call SetSimulParam_pAdjFAO(simul_pfao)
        read(fhandle, *) simul_lowox ! number of days for full effect of 
                                     ! deficient aeration
        call SetSimulParam_DelayLowOxygen(simul_lowox)
        read(fhandle, *) simul_expFsen ! exponent of senescence factor 
                                       ! adjusting drop in photosynthetic 
                                       ! activity of dying crop
        call SetSimulParam_ExpFsen(simul_expFsen)
        read(fhandle, *) simul_beta ! Decrease (percentage) of p(senescence) 
                                    ! once early canopy senescence is triggered
        call SetSimulParam_Beta(simul_beta)
        read(fhandle, *) simul_Tswc  ! Thickness top soil (cm) in which soil 
                                     ! water depletion has to be determined
        call SetSimulParam_ThicknessTopSWC(simul_Tswc)
        ! field
        read(fhandle, *) simul_EZma ! maximum water extraction depth by soil 
                                    ! evaporation [cm]
        call SetSimulParam_EvapZmax(simul_EZma)
        ! soil
        read(fhandle, *) simul_rod ! considered depth (m) of soil profile for 
                                   ! calculation of mean soil water content
        call SetSimulParam_RunoffDepth(simul_rod)
        read(fhandle, *) i   ! correction CN for Antecedent Moisture Class
        if (i == 1) then
            call SetSimulParam_CNcorrection(.true.)
        else
            call SetSimulParam_CNcorrection(.false.)
        end if
        read(fhandle, *) simul_saltdiff ! salt diffusion factor (%)
        read(fhandle, *) simul_saltsolub ! salt solubility (g/liter)
        read(fhandle, *) simul_root ! shape factor capillary rise factor
        call SetSimulParam_SaltDiff(simul_saltdiff)
        call SetSimulParam_SaltSolub(simul_saltsolub)
        call SetSimulParam_RootNrDF(simul_root)
        call SetSimulParam_IniAbstract(5_int8) ! fixed in Version 5.0 cannot be &
                                     ! changed since linked with equations for 
                                     ! CN AMCII and CN converions
        ! Temperature
        read(fhandle, *) simul_Tmi   ! Default minimum temperature (degC) if no &
                                     ! temperature file is specified
        call SetSimulParam_Tmin(simul_Tmi)
        read(fhandle, *) simul_Tma   ! Default maximum temperature (degC) if no &
                                     ! temperature file is specified
        call SetSimulParam_Tmax(simul_Tma)
        read(fhandle, *) simul_GDD ! Default method for GDD calculations
        call SetSimulParam_GDDMethod(simul_GDD)
        if (GetSimulParam_GDDMethod() > 3_int8) then
            SetSimulParam_GDDMethod(3_int8)
        end if
        if (GetSimulParam_GDDMethod()< 1_int8) then
            SetSimulParam_GDDMethod(3_int8)
        end if
        ! Rainfall
        read(fhandle, *) i
        select case (i)
            case (0)
                call SetSimulParam_EffectiveRain_Method(EffectiveRainMethod_Full)
            case (1)
                call SetSimulParam_EffectiveRain_Method(EffectiveRainMethod_USDA)
            case (2)
                call SetSimulParam_EffectiveRain_Method(EffectiveRainMethod_Percentage)
        end select
        read(fhandle, *) effrainperc ! IF Method is Percentage
        call SetSimulParam_EffectiveRain_PercentEffRain(effrainperc)
        read(fhandle, *) effrainshow  ! For estimation of surface run-off
        call SetSimulParam_EffectiveRain_ShowersInDecade(effrainshow)
        read(fhandle, *) effrainrootE ! For reduction of soil evaporation
        call SetSimulParam_EffectiveRain_RootNrEvap(effrainrootE)
        ! close
        Close(fhandle)
    else
        ! take the default set of program parameters
        call ReadSoilSettings
        call ReadRainfallSettings
        call ReadCropSettingsParameters
        call ReadFieldSettingsParameters
        call ReadTemperatureSettingsParameters
    end if
end subroutine LoadProgramParametersProject


end%
