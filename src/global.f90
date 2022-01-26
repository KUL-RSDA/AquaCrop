module ac_global

use ac_kinds, only: dp, &
                    int8, &
                    int32, &
                    intEnum, &
                    sp
use iso_fortran_env, only: iostat_end

implicit none


integer(int32), parameter :: max_SoilLayers = 5
real(dp), parameter :: undef_double = -9.9_dp
    !! value for 'undefined' real(dp) variables
integer(int32), parameter :: undef_int = -9
    !! value for 'undefined' int32 variables
real(dp), parameter :: PI = 3.1415926535_dp
real(dp), parameter :: CO2Ref = 369.41_dp; 
    !! reference CO2 in ppm by volume for year 2000 for Mauna Loa
    !! (Hawaii,USA)
real(dp), parameter :: eps =10E-08
real(dp), dimension(12), parameter :: ElapsedDays = [0._dp, 31._dp, 59.25_dp, &
                                                    90.25_dp, 120.25_dp, 151.25_dp, &
                                                    181.25_dp, 212.25_dp, 243.25_dp, &
                                                    273.25_dp, 304.25_dp, 334.25_dp]
integer(int32), dimension (12), parameter :: DaysInMonth = [31,28,31,30,31,30, &
                                                            31,31,30,31,30,31]

integer(intEnum), parameter :: modeCycle_GDDDays = 0
    !! index of GDDDays in modeCycle enumerated type
integer(intEnum), parameter :: modeCycle_CalendarDays = 1
    !! index of CalendarDays in modeCycle enumerated type

integer(intEnum), parameter :: plant_seed = 0
    !! index of seed in planting enumerated type
integer(intEnum), parameter :: plant_transplant = 1
    !! index of transplant in planting enumerated type
integer(intEnum), parameter :: plant_regrowth= 2
    !! index of regrowth in planting enumerated type

integer(intEnum), parameter :: TimeCuttings_NA = 0
    !! index of NA in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_IntDay = 1
    !! index of IntDay in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_IntGDD = 2
    !! index of IntGDD in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_DryB = 3
    !! index of DryB in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_DryY = 4
    !! index of DryY in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_FreshY= 5
    !! index of FreshY in TimeCuttings enumerated type

integer(intEnum), parameter :: GenerateTimeMode_FixInt = 0
    !! index of FixInt in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_AllDepl = 1
    !! index of AllDepl in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_AllRAW = 2
    !! index of AllRAW in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_WaterBetweenBuns = 3
    !! index of WaterBetweenBuns in GenerateTimeMode enumerated type

integer(intEnum), parameter :: GenerateDepthMode_ToFC = 0
    !! index of ToFC in GenerateDepthMode enumerated type
integer(intEnum), parameter :: GenerateDepthMode_FixDepth = 1
    !! index of FixDepth in GenerateDepthMode enumerated type

integer(intEnum), parameter :: IrriMode_NoIrri = 0
    !! index of NoIrri in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Manual = 1
    !! index of Manual in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Generate = 2
    !! index of Generate in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Inet = 3
    !! index of inet in IrriMode enumerated type

integer(intEnum), parameter :: IrriMethod_MBasin = 0
    !! index of MBasin in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MBorder = 1
    !! index of MBorder in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MDrip = 2
    !! index of MDrip in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MFurrow = 3
    !! index of MFurrow in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MSprinkler = 4
    !! index of MSprinkler in IrriMode enumerated type

integer(intEnum), parameter :: datatype_daily = 0
    !! index of daily in datatype enumerated type
integer(intEnum), parameter :: datatype_decadely = 1
    !! index of decadely in datatype enumerated type
integer(intEnum), parameter :: datatype_monthly= 2
    !! index of monthly in datatype enumerated type

type SoilLayerIndividual
    character(len=25) :: Description
        !! Undocumented
    real(dp) :: Thickness
        !! meter
    real(dp) :: SAT
        !! Vol % at Saturation
    real(dp) :: FC
        !! Vol % at Field Capacity
    real(dp) :: WP
        !! Vol % at Wilting Point
    real(dp) :: tau
        !! drainage factor 0 ... 1
    real(dp) :: InfRate
        !! Infiltration rate at saturation mm/day
    integer(int8) :: Penetrability
        !! root zone expansion rate in percentage
    integer(int8) :: GravelMass
        !! mass percentage of gravel
    real(dp) :: GravelVol
        !! volume percentage of gravel
    real(dp) :: WaterContent
        !! mm
    ! salinity parameters (cells)
    integer(int8) :: Macro
        !! Macropores : from Saturation to Macro [vol%]
    real(dp), dimension(11) :: SaltMobility
        !! Mobility of salt in the various salt cellS
    integer(int8) :: SC
        !! number of Saltcels between 0 and SC/(SC+2)*SAT vol%
    integer(int8) :: SCP1
        !! SC + 1   (1 extra Saltcel between SC/(SC+2)*SAT vol% and SAT)
        !! THis last celL is twice as large as the other cels *)
    real(dp) :: UL
        !! Upper Limit of SC salt cells = SC/(SC+2) * (SAT/100) in m3/m3
    real(dp) :: Dx
        !! Size of SC salt cells [m3/m3] = UL/SC
    ! capilary rise parameters
    integer(int8) :: SoilClass
        !! 1 = sandy, 2 = loamy, 3 = sandy clayey, 4 - silty clayey soils
    real(dp) :: CRa, CRb
        !! coefficients for Capillary Rise
end type SoilLayerIndividual

type rep_Shapes
    integer(int8) :: Stress
        !! Percentage soil fertility stress for calibration
    real(dp) :: ShapeCGC
        !! Shape factor for the response of Canopy Growth Coefficient to soil
        !! fertility stress
    real(dp) :: ShapeCCX
        !! Shape factor for the response of Maximum Canopy Cover to soil
        !! fertility stress
    real(dp) :: ShapeWP
        !! Shape factor for the response of Crop Water Producitity to soil
        !! fertility stress
    real(dp) :: ShapeCDecline
        !! Shape factor for the response of Decline of Canopy Cover to soil
        !! fertility stress
    logical :: Calibrated
        !! Undocumented
end type rep_Shapes

type rep_soil
    integer(int8) :: REW 
        !! (* Readily evaporable water mm *)
    integer(int8) :: NrSoilLayers
    integer(int8) :: CNvalue
    real(sp) :: RootMax 
        !! maximum rooting depth in soil profile for selected crop
end type rep_soil

type rep_Content
    real(dp) :: BeginDay
        !! at the beginning of the day
    real(dp) :: EndDay
        !! at the end of the day
    real(dp) :: ErrorDay
        !! error on WaterContent or SaltContent over the day
end type rep_Content

type rep_EffectStress
    integer(int8) :: RedCGC
        !! Reduction of CGC (%)
    integer(int8) :: RedCCX
        !! Reduction of CCx (%)
    integer(int8) :: RedWP
        !! Reduction of WP (%)
    real(dp) :: CDecline
        !! Average decrease of CCx in mid season (%/day)
    integer(int8) :: RedKsSto
        !! Reduction of KsSto (%)
end type rep_EffectStress

type rep_RootZoneWC 
    real(dp) :: Actual
        !! actual soil water content in rootzone [mm]
    real(dp) :: FC
        !! soil water content [mm] in rootzone at FC
    real(dp) :: WP
        !! soil water content [mm] in rootzone at WP
    real(dp) :: SAT
        !! soil water content [mm] in rootzone at Sat
    real(dp) :: Leaf
        !! soil water content [mm] in rootzone at upper Threshold for leaf
        !! expansion
    real(dp) :: Thresh
        !! soil water content [mm] in rootzone at Threshold for stomatal
        !! closure
    real(dp) :: Sen
        !! soil water content [mm] in rootzone at Threshold for canopy
        !! senescence
    real(dp) :: ZtopAct
        !! actual soil water content [mm] in top soil (= top compartment)
    real(dp) :: ZtopFC
        !! soil water content [mm] at FC in top soil (= top compartment)
    real(dp) :: ZtopWP
        !! soil water content [mm] at WP in top soil (= top compartment)
    real(dp) :: ZtopThresh
        !! soil water content [mm] at Threshold for stomatal closure in top
        !! soil
end type rep_RootZoneWC 

type rep_IrriECw 
    real(dp) :: PreSeason
        !! Undocumented
    real(dp) :: PostSeason
        !! Undocumented
end type rep_IrriECw 

type rep_clim
    integer(intEnum) :: DataType
        !! Undocumented
    integer(int32) :: FromD, FromM, FromY
        !! D = day or decade, Y=1901 is not linked to specific year
    integer(int32) :: ToD, ToM, ToY
        !! Undocumented
    integer(int32) :: FromDayNr, ToDayNr
        !! daynumber
    character(len=:), allocatable :: FromString, ToString
        !! Undocumented, note GDL: randomly chose 500
    integer(int32) :: NrObs
        !! number of observations
end type rep_clim

type rep_CropFileSet 
    integer(int32) :: DaysFromSenescenceToEnd
        !! Undocumented
    integer(int32) :: DaysToHarvest
        !! given or calculated from GDD
    integer(int32) :: GDDaysFromSenescenceToEnd
        !! Undocumented
    integer(int32) :: GDDaysToHarvest
        !! given or calculated from Calendar Days
end type rep_CropFileSet

type rep_Cuttings 
    logical :: Considered
        !! Undocumented
    integer(int32) :: CCcut
        !! Canopy cover (%) after cutting
    integer(int32) :: CGCPlus
        !! Increase (percentage) of CGC after cutting
    integer(int32) :: Day1
        !! first day after time window for generating cuttings (1 = start crop cycle)
    integer(int32) :: NrDays
        !! number of days of time window for generate cuttings (-9 is whole crop cycle)
    logical :: Generate
        !! ture: generate cuttings; false : schedule for cuttings
    integer(intEnum) :: Criterion
        !! time criterion for generating cuttings
    logical :: HarvestEnd
        !! final harvest at crop maturity
    integer(int32) :: FirstDayNr
        !! first dayNr of list of specified cutting events (-9 = onset growing cycle)
end type rep_Cuttings 


type rep_Manag 
    integer(int8) :: Mulch
        !! percent soil cover by mulch in growing period
    integer(int8) :: SoilCoverBefore
        !! percent soil cover by mulch before growing period
    integer(int8) :: SoilCoverAfter
        !! percent soil cover by mulch after growing period
    integer(int8) :: EffectMulchOffS
        !! effect Mulch on evaporation before and after growing period
    integer(int8) :: EffectMulchInS
        !! effect Mulch on evaporation in growing period
    integer(int8) :: FertilityStress
        !! Undocumented
    real(dp) :: BundHeight
        !! meter;
    logical :: RunoffOn
        !! surface runoff
    integer(int32) :: CNcorrection
        !! percent increase/decrease of CN
    integer(int8) :: WeedRC
        !! Relative weed cover in percentage at canopy closure
    integer(int32) :: WeedDeltaRC
        !! Increase/Decrease of Relative weed cover in percentage during mid season
    real(dp) :: WeedShape
        !! Shape factor for crop canopy suppression
    integer(int8) :: WeedAdj
        !! replacement (%) by weeds of the self-thinned part of the Canopy Cover - only for perennials
    type(rep_Cuttings) :: Cuttings
        !! Multiple cuttings
end type rep_Manag


type rep_sum 
    real(dp) :: Epot, Tpot, Rain, Irrigation, Infiltrated
        !! Undocumented
    real(dp) :: Runoff, Drain, Eact, Tact, TrW, ECropCycle, CRwater
        !! mm
    real(dp) :: Biomass, YieldPart, BiomassPot, BiomassUnlim, BiomassTot
        !! ton/ha
    real(dp) :: SaltIn, SaltOut, CRsalt
        !! ton/ha
end type rep_sum 


type rep_RootZoneSalt 
    real(dp) :: ECe
        !! Electrical conductivity of the saturated soil-paste extract (dS/m)
    real(dp) :: ECsw
        !! Electrical conductivity of the soil water (dS/m)
    real(dp) :: ECswFC
        !! Electrical conductivity of the soil water at Field Capacity(dS/m)
    real(dp) :: KsSalt
        !! stress coefficient for salinity
end type rep_RootZoneSalt

type(rep_clim)  :: TemperatureRecord

character(len=:), allocatable :: RainFile
character(len=:), allocatable :: RainFileFull
character(len=:), allocatable :: RainDescription
character(len=:), allocatable :: EToFile
character(len=:), allocatable :: EToFileFull
character(len=:), allocatable :: EToDescription
character(len=:), allocatable :: CalendarFile
character(len=:), allocatable :: CalendarFileFull
character(len=:), allocatable :: CO2File
character(len=:), allocatable :: CO2FileFull
character(len=:), allocatable :: CO2Description
character(len=:), allocatable :: IrriFile
character(len=:), allocatable :: IrriFileFull
character(len=:), allocatable :: CropFile
character(len=:), allocatable :: CropFileFull
character(len=:), allocatable :: PathNameProg
character(len=:), allocatable :: PathNameOutp
character(len=:), allocatable :: PathNameSimul
character(len=:), allocatable :: ProfFile
character(len=:), allocatable :: ProfFilefull
character(len=:), allocatable :: ManFile
character(len=:), allocatable :: ManFilefull
character(len=:), allocatable :: ObservationsFile
character(len=:), allocatable :: ObservationsFilefull
character(len=:), allocatable :: ObservationsDescription
character(len=:), allocatable :: OffSeasonFile
character(len=:), allocatable :: OffSeasonFilefull
character(len=:), allocatable :: GroundWaterFile
character(len=:), allocatable :: GroundWaterFilefull
character(len=:), allocatable :: ClimateFile
character(len=:), allocatable :: ClimateFileFull
character(len=:), allocatable :: ClimFile
character(len=:), allocatable :: SWCiniFile
character(len=:), allocatable :: SWCiniFileFull
character(len=:), allocatable :: ProjectFile
character(len=:), allocatable :: ProjectFileFull
character(len=:), allocatable :: MultipleProjectFile
character(len=:), allocatable :: MultipleProjectFileFull

type(rep_IrriECw) :: IrriECw
type(rep_Manag) :: Management
type(rep_Cuttings) :: Cuttings
type(rep_Content) :: TotalSaltContent
type(rep_Content) :: TotalWaterContent
type(rep_soil) :: Soil
type(rep_RootZoneWC) :: RootZoneWC
type(rep_CropFileSet) :: CropFileSet
type(rep_sum) :: SumWaBal
type(rep_RootZoneSalt) :: RootZoneSalt

integer(intEnum) :: GenerateTimeMode
integer(intEnum) :: GenerateDepthMode
integer(intEnum) :: IrriMode
integer(intEnum) :: IrriMethod



contains


function trunc(x) result(y)
    !! Returns the integer part of x, which is always smaller than (or equal to) x
    !! in absolute value.
    real(dp), intent(in) :: x
    integer(int32) :: y
    
    if (x > 0) then
        y = floor(x, kind=int32)
    else
        y = ceiling(x, kind=int32)
    end if
end function trunc


real(dp) function AquaCropVersion(FullNameXXFile)
    character(len=*), intent(in) :: FullNameXXFile

    integer :: fhandle
    real(dp) :: VersionNr

    open(newunit=fhandle, file=trim(FullNameXXFile), status='old', &
         action='read')

    read(fhandle, *)  ! Description
    read(fhandle, *) VersionNr  ! AquaCrop version

    close(fhandle)

    AquaCropVersion = VersionNr
end function AquaCropVersion


real(sp) function RootMaxInSoilProfile(ZmaxCrop, TheNrSoilLayers, TheSoilLayer)
    real(dp), intent(in) :: ZmaxCrop
    integer(int8), intent(in) :: TheNrSoilLayers
    type(SoilLayerIndividual), dimension(max_SoilLayers), intent(in) :: &
                                                                TheSoilLayer

    real(dp) :: Zmax
    real(dp) :: Zsoil
    integer :: layi

    Zmax = ZmaxCrop
    Zsoil = 0._dp

    layi = 0
    do while ((layi < TheNrSoilLayers) .and. (Zmax > 0))
        layi = layi + 1

        if ((TheSoilLayer(layi)%Penetrability < 100) .and. &
            (nint(Zsoil*1000) < nint(ZmaxCrop*1000))) then
            Zmax = undef_int
        end if

        Zsoil = Zsoil + TheSoilLayer(layi)%Thickness
    end do

    if (Zmax < 0) then
        call ZrAdjustedToRestrictiveLayers(ZmaxCrop, TheNrSoilLayers, &
                                           TheSoilLayer, Zmax)
    end if

    RootMaxInSoilProfile = real(Zmax, kind=sp)
end function RootMaxInSoilProfile


subroutine ZrAdjustedToRestrictiveLayers(ZrIN, TheNrSoilLayers, TheLayer, ZrOUT)
    real(dp), intent(in) :: ZrIN
    integer(int8), intent(in) :: TheNrSoilLayers
    type(SoilLayerIndividual), dimension(max_SoilLayers), intent(in) :: TheLayer
    real(dp), intent(inout) :: ZrOUT

    integer :: Layi
    real(dp) :: Zsoil, ZrAdj, ZrRemain, DeltaZ, ZrTest
    logical :: TheEnd

    ZrOUT = ZrIn

    ! initialize (layer 1)
    layi = 1
    Zsoil = TheLayer(layi)%Thickness
    ZrAdj = 0
    ZrRemain = ZrIN
    DeltaZ = Zsoil
    TheEnd = .false.

    ! check succesive layers
    do while (.not. TheEnd)
        ZrTest = ZrAdj + ZrRemain * (TheLayer(layi)%Penetrability/100._dp)

        if ((layi == TheNrSoilLayers) &
            .or. (TheLayer(layi)%Penetrability == 0) &
            .or. (nint(ZrTest*10000) <= nint(Zsoil*10000))) then
            ! no root expansion in layer
            TheEnd = .true.
            ZrOUT = ZrTest
        else
            ZrAdj = Zsoil
            ZrRemain = ZrRemain - DeltaZ/(TheLayer(layi)%Penetrability/100._dp)
            layi = layi + 1
            Zsoil = Zsoil + TheLayer(layi)%Thickness
            DeltaZ = TheLayer(layi)%Thickness
        end if
    end do
end subroutine ZrAdjustedToRestrictiveLayers


subroutine set_layer_undef(LayerData)
    type(SoilLayerIndividual), intent(inout) :: LayerData

    integer(int32) :: i

    LayerData%Description = ''
    LayerData%Thickness = undef_double
    LayerData%SAT = undef_double
    LayerData%FC = undef_double
    LayerData%WP = undef_double
    LayerData%tau = undef_double
    LayerData%InfRate = undef_double
    LayerData%Penetrability = undef_int
    LayerData%GravelMass = undef_int
    LayerData%GravelVol = undef_int
    LayerData%Macro = undef_int
    LayerData%UL = undef_double
    LayerData%Dx = undef_double
    do i = 1, 11
        LayerData%SaltMobility(i) = undef_double  ! maximum 11 salt cells
    end do
    LayerData%SoilClass = undef_int
    LayerData%CRa = undef_int
    LayerData%CRb = undef_int
    LayerData%WaterContent = undef_double
end subroutine set_layer_undef


subroutine CropStressParametersSoilFertility(CropSResp, &
                    StressLevel, StressOUT)
    type(rep_Shapes), intent(in) :: CropSResp
    integer(int8), intent(in)    :: StressLevel
    type(rep_EffectStress), intent(inout) :: StressOUT

    real(dp) :: Ksi, pULActual, pLLActual

    pLLActual = 1._dp

    ! decline canopy growth coefficient (CGC)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCGC)
    StressOUT%RedCGC = nint((1._dp-Ksi)*100._dp, int8)
    ! decline maximum canopy cover (CCx)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCCX)
    StressOUT%RedCCX = nint((1._dp-Ksi)*100._dp, int8)
    ! decline crop water productivity (WP)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeWP)
    StressOUT%RedWP = nint((1._dp-Ksi)*100._dp, int8)
    ! decline Canopy Cover (CDecline)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCDecline)
    StressOUT%CDecline = 1._dp - Ksi
    ! inducing stomatal closure (KsSto) not applicable
    Ksi = 1._dp
    StressOUT%RedKsSto = nint((1._dp-Ksi)*100._dp, int8)
end subroutine CropStressParametersSoilFertility


real(dp) function TimeRootFunction(t, ShapeFactor, tmax, t0)
    real(dp), intent(in) :: t
    integer(int8), intent(in) :: ShapeFactor
    real(dp), intent(in) :: tmax
    real(dp), intent(in) :: t0

    TimeRootFunction = exp((10._dp / ShapeFactor) * log((t-t0) / (tmax-t0)))
end function TimeRootFunction


real(dp) function TimeToReachZroot(Zi, Zo, Zx, ShapeRootDeepening, Lo, LZxAdj)
    real(dp), intent(in) :: Zi
    real(dp), intent(in) :: Zo
    real(dp), intent(in) :: Zx
    integer(int8), intent(in) :: ShapeRootDeepening
    integer(int32), intent(in) :: Lo
    integer(int32), intent(in) :: LZxAdj

    real(dp) :: ti, T1

    ti = real(undef_int, kind=dp)

    if (nint(Zi*100) >= nint(Zx*100)) then
        ti = real(LZxAdj, kind=dp)
    else
        if (((Zo+0.0001_dp) < Zx) .and. (LZxAdj > Lo/2._dp) .and. (LZxAdj > 0) &
            .and. (ShapeRootDeepening > 0)) then
            T1 = exp((ShapeRootDeepening/10._dp) * log((Zi-Zo) / (Zx-Zo)))
            ti = T1 * (LZxAdj - Lo/2._dp) + Lo/2._dp
        end if
    end if

    TimeToReachZroot = ti
end function TimeToReachZroot


real(dp) function FromGravelMassToGravelVolume(PorosityPercent,&
                                               GravelMassPercent)
    real(dp), intent(in)      :: PorosityPercent
    integer(int8), intent(in) :: GravelMassPercent

    real(dp), parameter ::  MineralBD = 2.65 !! Mg/m3
    real(dp) :: MatrixBD, SoilBD

    if (GravelMassPercent > 0) then
        MatrixBD = MineralBD * (1._dp - PorosityPercent/100._dp)
        SoilBD = 100._dp/(GravelMassPercent/MineralBD + &
                          (100._dp-GravelMassPercent)/MatrixBD)
        FromGravelMassToGravelVolume = GravelMassPercent * (SoilBD/MineralBD)
   else
       FromGravelMassToGravelVolume = 0.0_dp
   end if
end function FromGravelMassToGravelVolume


real(dp) function GetWeedRC(TheDay, GDDayi, fCCx, TempWeedRCinput, TempWeedAdj,&
                            TempWeedDeltaRC, L12SF, TempL123, GDDL12SF, &
                            TempGDDL123, TheModeCycle)
    integer(int32), intent(in) :: TheDay
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: fCCx
    integer(int8), intent(in) :: TempWeedRCinput
    integer(int8), intent(in) :: TempWeedAdj
    integer(int32), intent(inout) :: TempWeedDeltaRC
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: TempL123
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: TempGDDL123
    integer(intEnum), intent(in) :: TheModeCycle

    real(dp) :: WeedRCDayCalc

    WeedRCDayCalc = TempWeedRCinput

    if ((TempWeedRCinput > 0) .and. (TempWeedDeltaRC /= 0)) then
        ! daily RC when increase/decline of RC in season (i.e. TempWeedDeltaRC <> 0)
        ! adjust the slope of increase/decline of RC in case of self-thinning (i.e. fCCx < 1)
        if ((TempWeedDeltaRC /= 0) .and. (fCCx < 0.999_dp)) then
            ! only when self-thinning and there is increase/decline of RC
            if (fCCx < 0.005_dp) then
                TempWeedDeltaRC = 0
            else
                TempWeedDeltaRC = nint(TempWeedDeltaRC * exp( &
                                       log(fCCx) * (1+TempWeedAdj/100._dp)), &
                                       kind=int32)
            end if
        end if

        ! calculate WeedRCDay by considering (adjusted) decline/increase of RC
        if (TheModeCycle == modeCycle_CalendarDays) then
            if (TheDay > L12SF) then
                if (TheDay >= TempL123) then
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        TempWeedDeltaRC/100._dp)
                else
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        (TempWeedDeltaRC/100._dp) &
                                         * (TheDay-L12SF) / (TempL123-L12SF))
                end if
            end if
        else
            if (GDDayi > GDDL12SF) then
                if (GDDayi > TempGDDL123) then
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        TempWeedDeltaRC/100._dp)
                else
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        (TempWeedDeltaRC/100._dp) &
                                         * (GDDayi-GDDL12SF) &
                                         / (TempGDDL123-GDDL12SF))
                end if
            end if
        end if

        ! fine-tuning for over- or undershooting in case of self-thinning
        if (fCCx < 0.999_dp) then
            ! only for self-thinning
            if ((fCCx < 1) .and. (fCCx > 0) .and. (WeedRCDayCalc > 98)) then
                WeedRCDayCalc = 98._dp
            end if
            if (WeedRCDayCalc < 0) then
                WeedRCDayCalc = 0._dp
            end if
            if (fCCx <= 0) then
                WeedRCDayCalc = 100._dp
            end if
        end if
    end if

    GetWeedRC = WeedRCDayCalc
end function GetWeedRC

subroutine DetermineLengthGrowthStages(CCoVal, CCxVal, CDCVal, L0, TotalLength, &
                                        CGCgiven, TheDaysToCCini, ThePlanting, &
                                         Length123, StLength, Length12, CGCVal)
    real(dp), intent(in) :: CCoVal
    real(dp), intent(in) :: CCxVal
    real(dp), intent(in) :: CDCVal
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: TotalLength
    logical, intent(in) :: CGCgiven
    integer(int32), intent(in) :: TheDaysToCCini
    integer(intEnum), intent(in) :: ThePlanting
    integer(int32), intent(inout) :: Length123
    integer(int32), dimension(4), intent(inout) :: StLength
    integer(int32), intent(inout) :: Length12
    real(dp), intent(inout) :: CGCVal

    real(dp) :: CCxVal_scaled
    real(dp) :: CCToReach
    integer(int32) :: L12Adj

    if (Length123 < Length12) then
        Length123 = Length12
    end if

    ! 1. Initial and 2. Crop Development stage
    ! CGC is given and Length12 is already adjusted to it
    ! OR Length12 is given and CGC has to be determined
    if ((CCoVal >= CCxVal) .or. (Length12 <= L0)) then
        Length12 = 0
        StLength(1) = 0
        StLength(2) = 0
        CGCVal = Undef_int
    else
        if (.not. CGCgiven) then ! Length12 is given and CGC has to be determined
            CGCVal = real((log((0.25_dp*CCxVal/CCoVal)/(1._dp-0.98_dp))/(Length12-L0)), kind=dp)
            ! Check if CGC < maximum value (0.40) and adjust Length12 if required
            if (CGCVal > 0.40_dp) then
                CGCVal = 0.40_dp
                CCxVal_scaled = 0.98_dp*CCxVal
                Length12 = DaysToReachCCwithGivenCGC(CCxVal_scaled , CCoVal, &
                                                             CCxVal, CGCVal, L0)
                if (Length123 < Length12) then
                    Length123 = Length12
                end if
            end if
        end if
        ! find StLength[1]
        CCToReach = 0.10_dp
        StLength(1) = DaysToReachCCwithGivenCGC(CCToReach, CCoVal, CCxVal, &
                                                                    CGCVal, L0)
        ! find StLength[2]
        StLength(2) = Length12 - StLength(1)
    end if
    L12Adj = Length12

    ! adjust Initial and Crop Development stage, in case crop starts as regrowth
    if (ThePlanting == plant_regrowth) then
        if (TheDaystoCCini == undef_int) then
            ! maximum canopy cover is already reached at start season
            L12Adj = 0
            StLength(1) = 0
            StLength(2) = 0
        else
            if (TheDaystoCCini == 0) then
                ! start at germination
                L12Adj = Length12 - L0
                StLength(1) = StLength(1) - L0
            else
                ! start after germination
                L12Adj = Length12 - (L0 + TheDaysToCCini)
                StLength(1) = StLength(1) - (L0 + TheDaysToCCini)
            end if
            if (StLength(1) < 0) then
                StLength(1) = 0
            end if
            StLength(2) = L12Adj - StLength(1)
        end if
    end if

    ! 3. Mid season stage
    StLength(3) = Length123 - L12Adj

    ! 4. Late season stage
    StLength(4) = LengthCanopyDecline(CCxVal, CDCVal)

    ! final adjustment
    if (StLength(1) > TotalLength) then
        StLength(1) = TotalLength
        StLength(2) = 0
        StLength(3) = 0
        StLength(4) = 0
    else
        if ((StLength(1)+StLength(2)) > TotalLength) then
            StLength(2) = TotalLength - StLength(1)
            StLength(3) = 0
            StLength(4) = 0
        else
            if ((StLength(1)+StLength(2)+StLength(3)) > TotalLength) then
                StLength(3) = TotalLength - StLength(1) - StLength(2)
                StLength(4) = 0
            elseif ((StLength(1)+StLength(2)+StLength(3)+StLength(4)) > &
                                                              TotalLength) then
                StLength(4) = TotalLength - StLength(1) - StLength(2) - StLength(3)
            end if
        end if
    end if

end subroutine DetermineLengthGrowthStages


integer(int32) function TimeToCCini(ThePlantingType, TheCropPlantingDens, &
                          TheSizeSeedling, TheSizePlant, TheCropCCx, TheCropCGC)
    integer(intEnum), intent(in) :: ThePlantingType
    integer(int32), intent(in) :: TheCropPlantingDens
    real(dp), intent(in) :: TheSizeSeedling
    real(dp), intent(in) :: TheSizePlant
    real(dp), intent(in) :: TheCropCCx
    real(dp), intent(in) :: TheCropCGC


    integer(int32) :: ElapsedTime
    real(dp) :: TheCropCCo
    real(dp) :: TheCropCCini

    if ((ThePlantingType == plant_seed) .or. (ThePlantingType == plant_transplant) &
                                   .or. (TheSizeSeedling >= TheSizePlant)) then
        ElapsedTime = 0
    else
        TheCropCCo = (TheCropPlantingDens/10000._dp) * (TheSizeSeedling/10000._dp)
        TheCropCCini = (TheCropPlantingDens/10000._dp) * (TheSizePlant/10000._dp)
        if (TheCropCCini >= (0.98_dp*TheCropCCx)) then
            ElapsedTime = undef_int
        else
            if (TheCropCCini <= TheCropCCx/2) then
                ElapsedTime = nint(((log(TheCropCCini/TheCropCCo))/TheCropCGC), kind=int32)
            else
                ElapsedTime = (-1)* nint(((log(((TheCropCCx-TheCropCCini)* &
                     TheCropCCo)/(0.25_dp*TheCropCCx*TheCropCCx)))/TheCropCGC), kind=int32)
            end if
        end if
    end if
    TimeToCCini = ElapsedTime
end function TimeToCCini

real(dp) function MultiplierCCxSelfThinning(Yeari, Yearx, ShapeFactor)
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(in) :: Yearx
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: fCCx, Year0
    
    fCCx = 1
    if ((Yeari >= 2) .and. (Yearx >= 2) .and. (nint(100._dp*ShapeFactor, &
                                                    int32) /= 0)) then
        Year0 = 1._dp + (Yearx-1._dp) * exp(ShapeFactor*log(10._dp))
        if (Yeari >= Year0) then
            fCCx = 0
        else
            fCCx = 0.9_dp + 0.1_dp * (1._dp - exp((1._dp/ShapeFactor) &
                        *log((Yeari-1._dp)/(Yearx-1._dp))))
        end if
        if (fCCx < 0) then
            fCCx = 0
        end if
    end if
    MultiplierCCxSelfThinning = fCCx     
end function MultiplierCCxSelfThinning

integer(int32) function DaysToReachCCwithGivenCGC(CCToReach, CCoVal, &
                                                        CCxVal, CGCVal, L0)
    real(dp), intent(inout) :: CCToReach
    real(dp), intent(in) :: CCoVal
    real(dp), intent(in) :: CCxVal
    real(dp), intent(in) :: CGCVal
    integer(int32), intent(in) :: L0

    real(dp) :: L
    if ((CCoVal > CCToReach) .or. (CCoVal >= CCxVal)) then
        L = 0
    else
        if (CCToReach > (0.98_dp*CCxVal)) then
            CCToReach = 0.98_dp*CCxVal
        end if
        if (CCToReach <= CCxVal/2._dp) then
            L = log(CCToReach/CCoVal)/CGCVal
        else
            L = log((0.25_dp*CCxVal*CCxVal/CCoVal)/(CCxVal-CCToReach))/CGCVal
        end if

    end if
    DaysToReachCCwithGivenCGC = L0 + nint(L, int32)
end function DaysToReachCCwithGivenCGC

integer(int32) function LengthCanopyDecline(CCx, CDC)
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CDC

    integer(int32) :: ND

    ND = 0
    if (CCx > 0) then
        if (CDC <= epsilon(1._dp)) then
            ND = undef_int
        else
            ND = nint((((CCx+2.29_dp)/(CDC*3.33_dp))*log(1._dp + 1._dp/0.05_dp &
                     ) + 0.50_dp), int32)  ! + 0.50 to guarantee that CC is zero
        end if

    end if
    LengthCanopyDecline = ND
end function LengthCanopyDecline


real(dp) function HarvestIndexGrowthCoefficient(HImax, dHIdt)
    real(dp), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt

    real(dp) :: HIo, HIvar, HIGC, t

    HIo = 1

    if (HImax > HIo) then
        t = HImax/dHIdt
        HIGC = 0.001_dp
        HIGC = HIGC + 0.001_dp
        HIvar = (HIo*HImax)/(HIo+(HImax-HIo)*exp(-HIGC*t))
        do while (HIvar <= (0.98_dp*HImax))
            HIGC = HIGC + 0.001_dp
            HIvar = (HIo*HImax)/(HIo+(HImax-HIo)*exp(-HIGC*t))
        end do

        if (HIvar >= HImax) then
            HIGC = HIGC - 0.001_dp
        end if
    else
        HIGC = undef_int
    end if
    HarvestIndexGrowthCoefficient = HIGC

end function HarvestIndexGrowthCoefficient


real(dp) function TauFromKsat(Ksat)
    real(dp), intent(in) :: Ksat

    integer(int32) :: TauTemp

    if (abs(Ksat) < epsilon(1._dp)) then
        TauFromKsat = 0
    else
        TauTemp = nint(100.0_dp*0.0866_dp*exp(0.35_dp*log(Ksat)), kind=int32)
        if (TauTemp < 0) then
            TauTemp = 0
        end if
        if (TauTemp > 100) then
            TauTemp = 100
        end if
        TauFromKsat = TauTemp/100.0_dp
    end if
end function TauFromKsat


integer(int8) function NumberSoilClass(SatvolPro, FCvolPro, PWPvolPro, Ksatmm)
    real(dp), intent(in) :: SatvolPro
    real(dp), intent(in) :: FCvolPro
    real(dp), intent(in) :: PWPvolPro
    real(dp), intent(in) :: Ksatmm

    if (SATvolPro <= 55.0_dp) then
        if (PWPvolPro >= 20.0_dp) then
            if ((SATvolPro >= 49.0_dp) .and. (FCvolPro >= 40.0_dp)) then
                NumberSoilClass = 4  ! silty clayey soils
            else
                NumberSoilClass = 3  ! sandy clayey soils
            end if
        else
            if (FCvolPro < 23.0_dp) then
                NumberSoilClass = 1 ! sandy soils
            else
                if ((PWPvolPro > 16.0_dp) .and. (Ksatmm < 100.0_dp)) then
                    NumberSoilClass = 3 ! sandy clayey soils
                else
                    if ((PWPvolPro < 6.0_dp) .and. (FCvolPro < 28.0_dp) &
                        .and. (Ksatmm >750.0_dp)) then
                        NumberSoilClass = 1 ! sandy soils
                    else
                        NumberSoilClass = 2  ! loamy soils
                    end if
                end if
            end if
        end if
    else
        NumberSoilClass = 4 ! silty clayey soils
    end if
end function NumberSoilClass


subroutine DeriveSmaxTopBottom(SxTopQ, SxBotQ, SxTop, SxBot)
    real(dp), intent(in) :: SxTopQ
    real(dp), intent(in) :: SxBotQ
    real(dp), intent(inout) :: SxTop
    real(dp), intent(inout) :: SxBot

    real(dp) :: x, V1, V2, V11, V22

    V1 = SxTopQ
    V2 = SxBotQ
    if (abs(V1 - V2) < 1e-12_dp) then
        SxTop = V1
        SxBot = V2
    else
        if (SxTopQ < SxBotQ) then
            V1 = SxBotQ
            V2 = SxTopQ
        end if
        x = 3.0_dp * V2/(V1-V2)
        if (x < 0.5_dp) then
            V11 = (4.0_dp/3.5_dp) * V1
            V22 = 0.0_dp
        else
            V11 = (x + 3.5_dp) * V1/(x+3.0_dp)
            V22 = (x - 0.5_dp) * V2/x
        end if
        if (SxTopQ > SxBotQ) then
            SxTop = V11
            SxBot = V22
        else
            SxTop = V22
            SxBot = V11
        end if
    end if
end subroutine DeriveSmaxTopBottom


real(dp) function KsTemperature(T0, T1, Tin)
    real(dp), intent(in) :: T0
    real(dp), intent(in) :: T1
    real(dp), intent(in) :: Tin

    real(dp) :: M
    integer(int8) :: a

    M = 1._dp ! no correction applied (TO and/or T1 is undefined, or T0=T1)
    if (((nint(T0, kind=int32) /= undef_int) .and. &
         (nint(T1, kind=int32) /= undef_int)) .and. abs(T0-T1)> eps) then
        if (T0 < T1) then
            a =  1  ! cold stress
        else
            a = -1 ! heat stress
        end if
        if ((a*Tin > a*T0) .and. (a*Tin < a*T1)) then 
           ! within range for correction
            M = GetKs(T0, T1, Tin)
            if (M < 0) then
                M = 0._dp
            end if
            if (M > 1) then
                M = 1._dp
            end if
        else
            if (a*Tin <= a*T0) then
                M = 0._dp
            end if
            if (a*Tin >= a*T1) then
                M = 1._dp
            end if
        end if
    end if
    KsTemperature = M

    contains
    real(dp) function GetKs(T0, T1, Tin)
        real(dp), intent(in) :: T0
        real(dp), intent(in) :: T1
        real(dp), intent(in) :: Tin

        real(dp), parameter  :: Mo = 0.02_dp
        real(dp), parameter  :: Mx = 1.0_dp

        real(dp) :: MRate, Ksi, Trel

        Trel = (Tin-T0)/(T1-T0)
        ! derive rate of increase (MRate)
        MRate = (-1._dp)*(log((Mo*Mx-0.98_dp*Mo)/(0.98_dp*(Mx-Mo))))
        ! get Ks from logistic equation
        Ksi = (Mo*Mx)/(Mo+(Mx-Mo)*exp(-MRate*Trel))
        ! adjust for Mo
        Ksi = Ksi - Mo * (1._dp - Trel)
        GetKs = Ksi
     end function GetKs
end function KsTemperature


real(dp) function KsSalinity(SalinityResponsConsidered, &
                ECeN, ECeX, ECeVAR, KsShapeSalinity)
    logical, intent(in) :: SalinityResponsConsidered
    integer(int8), intent(in) :: ECeN
    integer(int8), intent(in) :: ECeX
    real(dp), intent(in) :: ECeVAR
    real(dp), intent(in) :: KsShapeSalinity

    real(dp) :: M, tmp_var

    M = 1._dp ! no correction applied
    if (SalinityResponsConsidered) then
        if ((ECeVAR > ECeN) .and. (ECeVar < ECeX)) then
            ! within range for correction
            if ((nint(KsShapeSalinity*10._dp) /= 0) .and. &
                (nint(KsShapeSalinity*10) /= 990)) then
                tmp_var = real(ECeN, kind=dp)
                M = KsAny(ECeVar, tmp_var, real(ECeX, kind=dp), KsShapeSalinity) 
                ! convex or concave
            else
                if (nint(KsShapeSalinity*10._dp) == 0) then
                    M = 1._dp - (ECeVAR-ECeN)/(ECeX-ECeN) 
                    ! linear (KsShapeSalinity = 0)
                else
                    M = KsTemperature(real(ECeX, kind=dp), real(ECeN, kind=dp), ECeVAR) 
                    ! logistic equation (KsShapeSalinity = 99)
                end if
            end if
        else
            if (ECeVAR <= ECeN) then
                M = 1._dp  ! no salinity stress
            end if
            if (ECeVar >= ECeX) then
                M = 0._dp  ! full salinity stress
            end if
        end if
    end if
    if (M > 1) then
        M = 1._dp
    end if
    if (M < 0) then
        M = 0._dp
    end if
    KsSalinity = M
end function KsSalinity


real(dp) function SoilEvaporationReductionCoefficient(Wrel, Edecline)
    real(dp), intent(in) :: Wrel
    real(dp), intent(in) :: Edecline

    if (Wrel <= 0.00001_dp) then
        SoilEvaporationReductionCoefficient = 0.0_dp
    else
        if (Wrel >= 0.99999_dp) then
            SoilEvaporationReductionCoefficient = 1.0_dp
        else
            SoilEvaporationReductionCoefficient =&
                (exp(Edecline*Wrel) - 1.0_dp)/(exp(Edecline) - 1.0_dp)
        end if
    end if
end function SoilEvaporationReductionCoefficient


real(dp) function MaxCRatDepth(ParamCRa, ParamCRb, Ksat, Zi, DepthGWT)
    real(dp), intent(in) :: ParamCRa
    real(dp), intent(in) :: ParamCRb
    real(dp), intent(in) :: Ksat
    real(dp), intent(in) :: Zi
    real(dp), intent(in) :: DepthGWT

    real(dp) :: CRmax

    CRmax = 0._dp
    if ((Ksat > 0._dp) .and. (DepthGWT > 0._dp) .and. ((DepthGWT-Zi) < 4._dp)) then
        if (Zi >= DepthGWT) then
            CRmax = 99._dp
        else
            CRmax = exp((log(DepthGWT - Zi) - ParamCRb)/ParamCRa)
            if (CRmax > 99._dp) then
                CRmax = 99._dp
            end if
        end if
    end if
    MaxCRatDepth = CRmax
end function MaxCRatDepth


real(dp) function CCmultiplierWeed(ProcentWeedCover, CCxCrop, FshapeWeed)
    integer(int8), intent(in) :: ProcentWeedCover
    real(dp), intent(in) :: CCxCrop
    real(dp), intent(in) :: FshapeWeed

    real(dp) :: fWeed

    if ((ProcentWeedCover > 0) .and. (CCxCrop < 0.9999_dp) .and. (CCxCrop > 0.001_dp)) then
        if (ProcentWeedCover == 100) then
            fWeed = 1._dp/CCxCrop
        else
            fWeed = 1._dp - (1._dp - 1._dp/CCxCrop) * &
              (exp(FshapeWeed*ProcentWeedCover/100._dp) - 1._dp)/(exp(FshapeWeed) - 1._dp)
            if (fWeed > (1._dp/CCxCrop)) then
                fWeed = 1._dp/CCxCrop
            end if
        end if
    else
        fWeed = 1._dp
    end if
    CCmultiplierWeed = fWeed
end function CCmultiplierWeed


real(dp) function BMRange(HIadj)
    integer(int32), intent(in) :: HIadj

    real(dp) :: BMR

    if (HIadj <= 0) then
        BMR = 0.0_dp
    else
        BMR = (log(real(HIadj, kind=dp))/0.0562_dp)/100.0_dp
    end if
    if (BMR > 1.0_dp) then
        BMR = 1.0_dp
    end if
    BMRange = BMR
end function BMRange


real(dp) function HImultiplier(RatioBM, RangeBM, HIadj)
    real(dp), intent(in) :: RatioBM
    real(dp), intent(in) :: RangeBM
    integer(int8), intent(in) :: HIadj

    real(dp) :: Rini, Rmax, Rend

    Rini = 1.0_dp - RangeBM
    REnd = 1.0_dp
    RMax = Rini + (2.0_dp/3.0_dp) * (REnd-Rini)
    if (RatioBM <= RIni) then
        HImultiplier = 1.0_dp
    elseif (RatioBM <= RMax) then
        HImultiplier = 1.0_dp&
            + (1.0_dp + sin(PI*(1.5_dp-(RatioBM-RIni)/(RMax-RIni))))&
            *(HIadj/200.0_dp)
    elseif (RatioBM <= REnd) then
        HImultiplier = 1.0_dp&
            + (1.0_dp + sin(PI*(0.5_dp+(RatioBM-RMax)/(REnd-RMax))))&
            *(HIadj/200.0_dp)
    else
        HImultiplier = 1.0_dp
    end if
end function HImultiplier


real(dp) function CCatTime(Dayi, CCoIN, CGCIN, CCxIN)
    integer(int32), intent(in) :: Dayi
    real(dp), intent(in) :: CCoIN
    real(dp), intent(in) :: CGCIN
    real(dp), intent(in) :: CCxIN

    real(dp) :: CCi

    CCi = CCoIN * exp(CGCIN * Dayi)
    if (CCi > CCxIN/2._dp) then
        CCi = CCxIN - 0.25_dp * (CCxIN/CCoIN) * CCxIN * exp(-CGCIN*Dayi)
    end if
    CCatTime = CCi
end function CCatTime


subroutine DetermineDayNr(Dayi, Monthi, Yeari, DayNr)
    integer(int32), intent(in) :: Dayi
    integer(int32), intent(in) :: Monthi
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(inout) :: DayNr

    DayNr = trunc((Yeari - 1901)*365.25_dp + ElapsedDays(Monthi) + Dayi + 0.05_dp)
    
end subroutine DetermineDayNr


subroutine DetermineDate(DayNr, Dayi, Monthi, Yeari)
    integer(int32), intent(in) :: DayNr
    integer(int32), intent(inout) :: Dayi
    integer(int32), intent(inout) :: Monthi
    integer(int32), intent(inout) :: Yeari

    real(dp) :: SumDayMonth

    Yeari = trunc((DayNr-0.05_dp)/365.25_dp)
    SumDayMonth = (DayNr - Yeari*365.25_dp)
    Yeari = 1901 + Yeari
    Monthi = 1

    do while (Monthi < 12)
        if (SumDayMonth <= ElapsedDays(Monthi+1)) exit
        Monthi = Monthi + 1
    end do
    Dayi = nint(SumDayMonth - ElapsedDays(Monthi) + 0.25_dp + 0.06_dp, kind=int32)
end subroutine DetermineDate


real(dp) function DegreesDay(Tbase, Tupper, TDayMin, TDayMax, GDDSelectedMethod)
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    integer(int8), intent(in) :: GDDSelectedMethod

    real(dp) :: TstarMax, TstarMin
    real(dp) :: Tavg, DgrD

    select case (GDDSelectedMethod)
    case (1)
        ! Method 1. - No adjustemnt of Tmax, Tmin before calculation of Taverage
        Tavg = (TDayMax+TDayMin)/2._dp
        if (Tavg > Tupper) then
            Tavg = Tupper
        end if
        if (Tavg < Tbase) then
            Tavg = Tbase
        end if

    case (2)
        ! Method 2. -  Adjustment for Tbase before calculation of Taverage
        TstarMax = TDayMax
        if (TDayMax < Tbase) then
            TstarMax = Tbase
        end if
        if (TDayMax > Tupper) then
            TstarMax = Tupper
        end if
        TstarMin = TDayMin
        if (TDayMin < Tbase) then
            TstarMin = Tbase
        end if
        if (TDayMin > Tupper) then
            TstarMin = Tupper
        end if
        Tavg = (TstarMax+TstarMin)/2._dp

    case default
       ! Method 3.
        TstarMax = TDayMax
        if (TDayMax < Tbase) then
             TstarMax = Tbase
        end if
        if (TDayMax > Tupper) then
            TstarMax = Tupper
        end if
        TstarMin = TDayMin
        if (TDayMin > Tupper) then
            TstarMin = Tupper
        end if
        Tavg = (TstarMax+TstarMin)/2._dp
        if (Tavg < Tbase) then
            Tavg = Tbase
        end if
    end select
    DgrD =  Tavg - Tbase
    DegreesDay =  DgrD
end function DegreesDay



subroutine DetermineCNIandIII(CN2, CN1, CN3)
    integer(int8), intent(in) :: CN2
    integer(int8), intent(inout) :: CN1
    integer(int8), intent(inout) :: CN3

    CN1 = nint(1.4_dp*(exp(-14*log(10._dp))) + 0.507_dp*CN2 &
                - 0.00374_dp*CN2*CN2 + 0.0000867_dp*CN2*CN2*CN2, kind=int8)
    CN3 = nint(5.6_dp*(exp(-14*log(10._dp))) + 2.33_dp*CN2 &
               - 0.0209_dp*CN2*CN2 + 0.000076_dp*CN2*CN2*CN2, kind=int8)

    if (CN1 <= 0) then
        CN1 = 1
    elseif (CN1 > 100) then
        CN1 = 100
    end if
    if (CN3 <= 0) then
        CN3 = 1
    elseif (CN3 > 100) then
        CN3 = 100
    end if
    if (CN3 < CN2) then
        CN3 = CN2
    end if
end subroutine DetermineCNIandIII


subroutine DetermineCN_default(Infiltr, CN2)
    real(dp), intent(in) :: Infiltr
    integer(int8), intent(inout) :: CN2

    if (Infiltr > 864) then
        CN2 = 46
    elseif (Infiltr >= 347) then
        CN2 = 61
    elseif (Infiltr >= 36) then
        CN2 = 72
    else
        CN2 = 77
    end if
end subroutine DetermineCN_default


real(dp) function MultiplierCCoSelfThinning(Yeari, Yearx, ShapeFactor)
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(in) :: Yearx
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: fCCo, Year0

    fCCo = 1._dp
    if ((Yeari >= 1) .and. (Yearx >= 2) .and. (nint(100*ShapeFactor) /= 0)) then
        Year0 = 1._dp + (Yearx-1) * exp(ShapeFactor*log(10._dp))
        if ((Yeari >= Year0) .or. (Year0 <= 1)) then
            fCCo = 0._dp
        else
            fCCo = 1._dp - (Yeari-1)/(Year0-1._dp)
        end if
        if (fCCo < 0._dp) then
            fCCo = 0._dp
        end if
    end if
    MultiplierCCoSelfThinning = fCCo
end function MultiplierCCoSelfThinning


real(dp) function KsAny(Wrel, pULActual, pLLActual, ShapeFactor)
    real(dp), intent(in) :: Wrel
    real(dp), intent(inout) :: pULActual
    real(dp), intent(in) :: pLLActual
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: pRelativeLLUL, KsVal
    ! Wrel : WC in rootzone (negative .... 0=FC ..... 1=WP .... > 1)
    !        FC .. UpperLimit ... LowerLimit .. WP
    ! p relative (negative .... O=UpperLimit ...... 1=LowerLimit .....>1)

    if ((pLLActual - pULActual) < 0.0001_dp) then
        pULActual = pLLActual - 0.0001_dp
    end if

    pRelativeLLUL = (Wrel - pULActual)/(pLLActual - pULActual)

    if (pRelativeLLUL <= 0._dp) then
        KsVal = 1._dp
    elseif (pRelativeLLUL >= 1._dp) then
        KsVal = 0._dp
    else
        if (nint(10*ShapeFactor) == 0) then ! straight line
            KsVal = 1._dp - &
                    (exp(pRelativeLLUL*0.01_dp)-1._dp)/(exp(0.01_dp)-1._dp)
        else
            KsVal = 1._dp - &
                    (exp(pRelativeLLUL*ShapeFactor)-1._dp)/(exp(ShapeFactor)-1._dp)
        end if
        if (KsVal > 1._dp) then
            KsVal = 1._dp
        end if
        if (KsVal < 0._dp) then
            KsVal = 0._dp
        end if
    end if
    KsAny = KsVal
end function KsAny

real(dp) function CCatGDD(GDDi, CCoIN, GDDCGCIN, CCxIN)
    real(dp), intent(in) :: GDDi
    real(dp), intent(in) :: CCoIN
    real(dp), intent(in) :: GDDCGCIN
    real(dp), intent(in) :: CCxIN

    real(dp) :: CCi

    CCi = CCoIN * exp(GDDCGCIN * GDDi)
    if (CCi > CCxIN/2._dp) then
        CCi = CCxIN - 0.25_dp * (CCxIN/CCoIN) * CCxIN * exp(-GDDCGCIN*GDDi)
    end if
    CCatGDD = CCi
end function CCatGDD


real(dp) function CanopyCoverNoStressGDDaysSF(GDDL0, GDDL123, GDDLMaturity, SumGDD, &
        CCo, CCx, GDDCGC, GDDCDC, SFRedCGC, SFRedCCx)
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDLMaturity
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: GDDCDC
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx

    real(dp) :: CC, CCxAdj, GDDCDCadj

    ! SumGDD refers to the end of the day and Delayed days are not considered
    CC = 0._dp
    if ((SumGDD > 0._dp) .and. (nint(SumGDD, kind=int32) <= GDDLMaturity) .and. (CCo > 0._dp)) then
        if (SumGDD <= GDDL0) then ! before germination or recovering of transplant
            CC = 0._dp
        else
            if (SumGDD < GDDL123) then ! Canopy development and Mid-season stage
                CC = CCatGDD(real((SumGDD-GDDL0),dp), CCo, ((1._dp-SFRedCGC/100._dp)*GDDCGC), & 
                  ((1._dp-SFRedCCx/100._dp)*CCx))
            else
                ! Late-season stage  (SumGDD <= GDDLMaturity)
                if (CCx < 0.001_dp) then
                    CC = 0._dp
                else
                    CCxAdj = CCatGDD(real(GDDL123-GDDL0,dp), CCo, ((1_dp-SFRedCGC/100._dp)*GDDCGC), &
                      ((1._dp-SFRedCCx/100._dp)*CCx))
                    GDDCDCadj = GDDCDC*(CCxadj+2.29_dp)/(CCx+2.29_dp)
                    if (CCxAdj < 0.001_dp) then
                        CC = 0._dp
                    else
                        CC = CCxAdj * (1._dp - 0.05_dp*(exp((SumGDD-GDDL123)*3.33_dp*GDDCDCadj/&
                          (CCxAdj+2.29_dp))-1._dp))
                    end if
                end if
            end if
        end if
    end if
    if (CC > 1._dp) then
        CC = 1._dp
    end if
    if (CC < 0._dp) then
        CC = 0._dp
    end if
    CanopyCoverNoStressGDDaysSF = CC
end function CanopyCoverNoStressGDDaysSF


real(dp) function HIadjWStressAtFlowering(KsVeg, KsSto, a, b)
    real(dp), intent(in) :: KsVeg
    real(dp), intent(in) :: KsSto
    integer(int8), intent(in) :: a
    real(dp), intent(in) :: b

    if (a == undef_int) then
        if (nint(b, kind=int32) == undef_int) then
            HIadjWStressAtFlowering = 1._dp
        elseif (KsSto > 0.001_dp) then
            HIadjWStressAtFlowering = (exp(0.10_dp*log(KsSto))) * (1._dp-(1._dp-KsSto)/b)
        else
            HIadjWStressAtFlowering = 0.0_dp
        end if
    else
        if (nint(b, kind=int32) == undef_int) then
            HIadjWStressAtFlowering = (1._dp + (1._dp-KsVeg)/a)
        elseif (KsSto > 0.001_dp) then
            HIadjWStressAtFlowering = (1._dp + (1._dp-KsVeg)/a) * (exp(0.10_dp*log(KsSto))) &
              * (1._dp-(1._dp-KsSto)/b)
        else
            HIadjWStressAtFlowering = 0._dp
        end if
    end if
end function HIadjWStressAtFlowering


real(dp) function fAdjustedForCO2(CO2i, WPi, PercentA)
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: WPi
    integer(int8), intent(in) :: PercentA

    real(dp) :: fW, fCO2Old, fType, fSink, fShape, CO2rel, fCO2adj, fCO2

    ! 1. Correction for crop type: fType
    if (WPi >= 40._dp) then
        fType = 0._dp ! no correction for C4 crops
    else
        if (WPi <= 20._dp) then
            fType = 1._dp ! full correction for C3 crops
        else
            fType = (40._dp-WPi)/(40._dp-20._dp)
        end if
    end if

    ! 2. crop sink strength coefficient: fSink
    fSink = PercentA/100._dp
    if (fSink < 0._dp) then
        fSink = 0._dp ! based on FACE expirements
    end if
    if (fSink > 1._dp) then
        fSink = 1._dp ! theoretical adjustment
    end if

    ! 3. Correction coefficient for CO2: fCO2Old
    fCO2Old = undef_int
    if (CO2i <= 550._dp) then
        ! 3.1 weighing factor for CO2
        if (CO2i <= CO2Ref) then
            fw = 0._dp
        else
            if (CO2i >= 550._dp) then
                fW = 1._dp
            else
                fw = 1._dp - (550._dp - CO2i)/(550._dp - CO2Ref)
            end if
        end if
        
        ! 3.2 adjustment for CO2
        fCO2Old = (CO2i/CO2Ref)/(1._dp+(CO2i-CO2Ref)*((1._dp-fW)*0.000138_dp&
            + fW*(0.000138_dp*fSink + 0.001165_dp*(1._dp-fSink))))
    end if

    ! 4. Adjusted correction coefficient for CO2: fCO2adj
    fCO2adj = undef_int
    if (CO2i > CO2Ref) then
        ! 4.1 Shape factor
        fShape = -4.61824_dp - 3.43831_dp*fSink - 5.32587_dp*fSink*fSink
        
        ! 4.2 adjustment for CO2
        if (CO2i >= 2000._dp) then
            fCO2adj = 1.58_dp  ! maximum is reached
        else
            CO2rel = (CO2i-CO2Ref)/(2000._dp-CO2Ref)
            fCO2adj = 1._dp + 0.58_dp * ((exp(CO2rel*fShape) - 1._dp)/&
                (exp(fShape) - 1._dp))
        end if
    end if

    ! 5. Selected adjusted coefficient for CO2: fCO2
    if (CO2i <= CO2Ref) then
        fCO2 = fCO2Old
    else
        fCO2 = fCO2adj
        if ((CO2i <= 550._dp) .and. (fCO2Old < fCO2adj)) then
            fCO2 = fCO2Old
        end if
    end if

    ! 6. final adjustment
    fAdjustedForCO2 = 1._dp + fType*(fCO2-1._dp)
end function fAdjustedForCO2


logical function FullUndefinedRecord(FromY, FromD, FromM, ToD, ToM)
    integer(int32), intent(in) :: FromY
    integer(int32), intent(in) :: FromD
    integer(int32), intent(in) :: FromM
    integer(int32), intent(in) :: ToD
    integer(int32), intent(in) :: ToM

    FullUndefinedRecord = ((FromY == 1901) .and. (FromD == 1)&
        .and. (FromM == 1) .and. (ToD == 31) .and. (ToM == 12))
end function FullUndefinedRecord


subroutine GenerateCO2Description(CO2FileFull, CO2Description)
    character(len=*), intent(in) :: CO2FileFull
    character(len=*), intent(inout) :: CO2Description

    integer :: fhandle

    open(newunit=fhandle, file=trim(CO2FileFull), status='old', &
         action='read')
    read(fhandle, *) CO2Description
    close(fhandle)

    if (trim(GetCO2File()) == 'MaunaLoa.CO2') then
        ! since this is an AquaCrop file, the Description is determined by AquaCrop
        CO2Description = 'Default atmospheric CO2 concentration from 1902 to 2099'
    end if
end subroutine GenerateCO2Description


subroutine GetIrriDescription(IrriFileFull, IrriDescription)
    character(len=*), intent(in) :: IrriFileFull
    character(len=*), intent(inout) :: IrriDescription

    integer :: fhandle

    open(newunit=fhandle, file=trim(IrriFileFull), status='old', &
         action='read')
    read(fhandle, *) IrriDescription
    close(fhandle)
end subroutine GetIrriDescription


subroutine GetDaySwitchToLinear(HImax, dHIdt, HIGC, tSwitch, HIGClinear)
    integer(int32), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt
    real(dp), intent(in) :: HIGC
    integer(int32), intent(inout) :: tSwitch
    real(dp), intent(inout) :: HIGClinear

    real(dp) :: HIi, HiM1, HIfinal
    integer(int32) :: tmax, ti
    integer(int32), parameter :: HIo = 1

    tmax = nint(HImax/dHIdt, kind=int32)
    ti = 0
    HiM1 = HIo
    if (tmax > 0) then
        loop: do
            ti = ti + 1
            HIi = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*ti))
            HIfinal = HIi + (tmax - ti)*(HIi-HIM1)
            HIM1 = HIi
            if ((HIfinal > HImax) .or. (ti >= tmax)) exit loop
        end do loop 
        tSwitch = ti - 1
    else
        tSwitch = 0
    end if
    if (tSwitch > 0) then
        HIi = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*tSwitch))
    else
        HIi = 0
    end if
    HIGClinear = (HImax-HIi)/(tmax-tSwitch)
end subroutine GetDaySwitchToLinear

subroutine GetNumberSimulationRuns(TempFileNameFull, NrRuns)
    character(len=*), intent(in) :: TempFileNameFull
    integer(int32), intent(inout) :: NrRuns

    integer :: fhandle
    integer(int32) :: NrFileLines, rc, i

    NrRuns = 1

    open(newunit=fhandle, file=trim(TempFileNameFull), status='old', &
         action='read', iostat=rc)
    read(fhandle, *, iostat=rc)  ! Description
    read(fhandle, *, iostat=rc)  ! AquaCrop version Nr

    do i = 1, 5 
        read(fhandle, *, iostat=rc) ! Type year and Simulation and Cropping period Run 1
    end do

    NrFileLines = 42 ! Clim(15),Calendar(3),Crop(3),Irri(3),Field(3),Soil(3),Gwt(3),Inni(3),Off(3),FieldData(3)
    do i = 1, NrFileLines 
        read(fhandle, *, iostat=rc) ! Files Run 1
    end do

    read_loop: do
        i = 0
        do while (i < (NrFileLines+5))
            read(fhandle, *, iostat=rc)
            if (rc == iostat_end) exit read_loop
            i = i + 1
        end do

        if (i == (NrFileLines+5)) then
            NrRuns = NrRuns + 1
        end if
    end do read_loop
    close(fhandle)
end subroutine GetNumberSimulationRuns

logical function FileExists(full_name)
    character(len=*), intent(in) :: full_name

    inquire(file=trim(full_name), exist=FileExists)
end function FileExists 


subroutine SplitStringInTwoParams(StringIN, Par1, Par2)
    character(len=*), intent(in) :: StringIN
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2

    integer(int32) :: LengthS, i, Parami
    character :: CharA
    character(len=255) :: StringNumber

    LengthS = len(StringIN)
    i = 0
    Parami = 0
    ! divide the line in parameters
    do while ((i < LengthS) .and. (Parami < 2)) 
        i = i + 1
        CharA = StringIN(i:i)
        if (ichar(CharA) > 32) then
            ! next Parameter
            Parami = Parami + 1
            StringNumber = ''
            do while ((ichar(CharA) > 32) .and. (i <= LengthS))
                StringNumber = trim(StringNumber) // CharA
                i = i + 1
                if (i <= LengthS) then
                    CharA = StringIN(i:i)
                end if
            end do
            if (Parami == 1) then
                read(StringNumber, *) Par1
            end if
            if (Parami == 2) then
                read(StringNumber, *) Par2
            end if
            ! next Parameter
        end if
        ! end of line
    end do
end subroutine SplitStringInTwoParams


subroutine SplitStringInThreeParams(StringIN, Par1, Par2, Par3)
    character(len=*), intent(in) :: StringIN
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2
    real(dp), intent(inout) :: Par3

    integer(int32) :: LengthS, i, Parami
    character :: CharA
    character(len=255) :: StringNumber

    LengthS = len(StringIN)
    i = 0
    Parami = 0
    ! divide the line in parameters
    do while ((i < LengthS) .and. (Parami < 3)) 
        i = i + 1
        CharA = StringIN(i:i)
        if (ichar(CharA) > 32) then
            ! next Parameter
            Parami = Parami + 1
            StringNumber = ''
            do while ((ichar(CharA) > 32) .and. (i <= LengthS))
                StringNumber = trim(StringNumber) // CharA
                i = i + 1
                if (i <= LengthS) then
                    CharA = StringIN(i:i)
                end if
            end do
            if (Parami == 1) then
                read(StringNumber, *) Par1
            end if
            if (Parami == 2) then
                read(StringNumber, *) Par2
            end if
            if (Parami == 3) then
                read(StringNumber, *) Par3
            end if
            ! next Parameter
        end if
        ! end of line
    end do
end subroutine SplitStringInThreeParams

!! Global variables section !!

function GetIrriFile() result(str)
    !! Getter for the "IrriFile" global variable.
    character(len=len(IrriFile)) :: str

    str = IrriFile
end function GetIrriFile


subroutine SetIrriFile(str)
    !! Setter for the "IrriFile" global variable.
    character(len=*), intent(in) :: str

    IrriFile = str
end subroutine SetIrriFile


function GetIrriFileFull() result(str)
    !! Getter for the "IrriFileFull" global variable.
    character(len=len(IrriFileFull)) :: str

    str = IrriFileFull
end function GetIrriFileFull


subroutine SetIrriFileFull(str)
    !! Setter for the "IrriFileFull" global variable.
    character(len=*), intent(in) :: str

    IrriFileFull = str
end subroutine SetIrriFileFull


function GetClimateFile() result(str)
    !! Getter for the "ClimateFile" global variable.
    character(len=len(ClimateFile)) :: str
    
    str = ClimateFile
end function GetClimateFile

subroutine SetClimateFile(str)
    !! Setter for the "ClimateFile" global variable.
    character(len=*), intent(in) :: str
    
    ClimateFile = str
end subroutine SetClimateFile

function GetClimateFileFull() result(str)
    !! Getter for the "ClimateFileFull" global variable.
    character(len=len(ClimateFileFull)) :: str
    
    str = ClimateFileFull
end function GetClimateFileFull

subroutine SetClimateFileFull(str)
    !! Setter for the "ClimateFileFull" global variable.
    character(len=*), intent(in) :: str
    
    ClimateFileFull = str
end subroutine SetClimateFileFull

function GetClimFile() result(str)
    !! Getter for the "ClimFile" global variable.
    character(len=len(ClimFile)) :: str
    
    str = ClimFile
end function GetClimFile

subroutine SetClimFile(str)
    !! Setter for the "ClimFile" global variable.
    character(len=*), intent(in) :: str
    
    ClimFile = str
end subroutine SetClimFile

function GetSWCiniFile() result(str)
    !! Getter for the "SWCiniFile" global variable.
    character(len=len(SWCiniFile)) :: str
    
    str = SWCiniFile
end function GetSWCiniFile

subroutine SetSWCiniFile(str)
    !! Setter for the "SWCiniFile" global variable.
    character(len=*), intent(in) :: str
    
    SWCiniFile = str
end subroutine SetSWCiniFile


function GetSWCiniFileFull() result(str)
    !! Getter for the "SWCiniFileFull" global variable.
    character(len=len(SWCiniFileFull)) :: str
    
    str = SWCiniFileFull
end function GetSWCiniFileFull

subroutine SetSWCiniFileFull(str)
    !! Setter for the "SWCiniFileFull" global variable.
    character(len=*), intent(in) :: str
    
    SWCiniFileFull = str
end subroutine SetSWCiniFileFull


function GetPathNameProg() result(str)
    !! Getter for the "PathNameProg" global variable.
    character(len=len(PathNameProg)) :: str
    
    str = PathNameProg
end function GetPathNameProg

subroutine SetPathNameProg(str)
    !! Setter for the "PathNameProg" global variable.
    character(len=*), intent(in) :: str
    
    PathNameProg = str
end subroutine SetPathNameProg

function GetPathNameOutp() result(str)
    !! Getter for the "PathNameOutp" global variable.
    character(len=len(PathNameOutp)) :: str
    
    str = PathNameOutp
end function GetPathNameOutp

subroutine SetPathNameOutp(str)
    !! Setter for the "PathNameOutp" global variable.
    character(len=*), intent(in) :: str
    
    PathNameOutp = str
end subroutine SetPathNameOutp

function GetPathNameSimul() result(str)
    !! Getter for the "PathNameSimul" global variable.
    character(len=len(PathNameSimul)) :: str
    
    str = PathNameSimul
end function GetPathNameSimul

subroutine SetPathNameSimul(str)
    !! Setter for the "PathNameSimul" global variable.
    character(len=*), intent(in) :: str
    
    PathNameSimul = str
end subroutine SetPathNameSimul


function GetProjectFile() result(str)
    !! Getter for the "ProjectFile" global variable.
    character(len=len(ProjectFile)) :: str
    
    str = ProjectFile
end function GetProjectFile

subroutine SetProjectFile(str)
    !! Setter for the "ProjectFile" global variable.
    character(len=*), intent(in) :: str
    
    ProjectFile = str
end subroutine SetProjectFile

function GetProjectFileFull() result(str)
    !! Getter for the "ProjectFileFull" global variable.
    character(len=len(ProjectFileFull)) :: str
    
    str = ProjectFileFull
end function GetProjectFileFull

subroutine SetProjectFileFull(str)
    !! Setter for the "ProjectFileFull" global variable.
    character(len=*), intent(in) :: str
    
    ProjectFileFull = str
end subroutine SetProjectFileFull

function GetMultipleProjectFile() result(str)
    !! Getter for the "MultipleProjectFile" global variable.
    character(len=len(MultipleProjectFile)) :: str
    
    str = MultipleProjectFile
end function GetMultipleProjectFile

subroutine SetMultipleProjectFile(str)
    !! Setter for the "MultipleProjectFile" global variable.
    character(len=*), intent(in) :: str
    
    MultipleProjectFile = str
end subroutine SetMultipleProjectFile

function GetMultipleProjectFileFull() result(str)
    !! Getter for the "MultipleProjectFileFull" global variable.
    character(len=len(MultipleProjectFileFull)) :: str
    
    str = MultipleProjectFileFull
end function GetMultipleProjectFileFull

subroutine SetMultipleProjectFileFull(str)
    !! Setter for the "MultipleProjectFileFull" global variable.
    character(len=*), intent(in) :: str
    
    MultipleProjectFileFull = str
end subroutine SetMultipleProjectFileFull


logical function LeapYear(Year)
    integer(int32), intent(in) :: Year

    LeapYear = .false.
    if (frac(Year/4._dp) <= 0.01_dp) then
        LeapYear = .true.
    end if

    contains

    real(dp) function frac(val)
        real(dp), intent(in) :: val

        frac = val - floor(val)
    end function frac 
end function LeapYear


subroutine CheckFilesInProject(TempFullFilename, Runi, AllOK)
    character(len=*), intent(in) :: TempFullFilename
    integer(int32), intent(in) :: Runi
    logical, intent(inout) :: AllOK

    integer :: fhandle
    character(len=:), allocatable :: TempFileName, TempPathName, TempFullName
    character(len=1024) :: buffer
    integer(int32) :: i, TotalFiles
  
    AllOK = .true.
    open(newunit=fhandle, file=trim(TempFullFilename), status='old', &
        action='read')
    read(fhandle, *) ! Description
    read(fhandle, *)  ! AquaCrop version Nr
    
    ! Prepare
    if (Runi > 1) then
        do i = 1, 5 
            read(fhandle, *) ! Type year and Simulation and Cropping period of run 1
        end do
        do i = 1, 42 
            read(fhandle, *) ! files previous runs
        end do
    end if

    ! Type Year and Simulation and Cropping period of the run
    do i = 1, 5 
        read(fhandle, *)
    end do

    ! Check the 14 files
    i = 1
    TotalFiles = 14
    do while (AllOK .and. (i <= TotalFiles))
        read(fhandle, *) ! Info
        read(fhandle, *) buffer  ! FileName
        TempFileName = trim(buffer)
        if (trim(TempFileName) == '(None)') then
            read(fhandle, *)
        else
            if ((i == (TotalFiles-2)) .and. (trim(TempFileName) == 'KeepSWC')) then ! file initial conditions
                read(fhandle, *) ! Keep initial SWC
            else
                read(fhandle, *) buffer ! PathName
                TempPathName = trim(buffer)
                TempFullName = trim(TempPathName) // trim(TempFileName)
                if (FileExists(trim(TempFullName)) .eqv. .false.) then
                    AllOK = .false.
                end if
            end if
        end if
        i = i + 1
    end do
    close(fhandle)
end subroutine CheckFilesInProject

!! Global variables section !!

function GetCO2File() result(str)
    !! Getter for the "CO2File" global variable.
    character(len=len(CO2File)) :: str

    str = CO2File
end function GetCO2File

subroutine SetCO2File(str)
    !! Setter for the "CO2File" global variable.
    character(len=*), intent(in) :: str

    CO2File = str
end subroutine SetCO2File

function GetCO2FileFull() result(str)
    !! Getter for the "CO2FileFull" global variable.
    character(len=len(CO2FileFull)) :: str

    str = CO2FileFull
end function GetCO2FileFull

subroutine SetCO2FileFull(str)
    !! Setter for the "CO2FileFull" global variable.
    character(len=*), intent(in) :: str

    CO2FileFull = str
end subroutine SetCO2FileFull

function GetCO2Description() result(str)
    !! Getter for the "CO2Description" global variable.
    character(len=len(CO2Description)) :: str

    str = CO2Description
end function GetCO2Description

subroutine SetCO2Description(str)
    !! Setter for the "CO2Description" global variable.
    character(len=*), intent(in) :: str

    CO2Description = str
end subroutine SetCO2Description

type(rep_RootZoneWC) function GetRootZoneWC()
    !! Getter for the "RootZoneWC" global variable.

    GetRootZoneWC = RootZoneWC
end function GetRootZoneWC

subroutine SetRootZoneWC_Actual(Actual)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Actual

    RootZoneWC%Actual = Actual
end subroutine SetRootZoneWC_Actual

subroutine SetRootZoneWC_FC(FC)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: FC

    RootZoneWC%FC = FC
end subroutine SetRootZoneWC_FC

subroutine SetRootZoneWC_WP(WP)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: WP

    RootZoneWC%WP = WP
end subroutine SetRootZoneWC_WP

subroutine SetRootZoneWC_SAT(SAT)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: SAT

    RootZoneWC%SAT = SAT
end subroutine SetRootZoneWC_SAT

subroutine SetRootZoneWC_Leaf(Leaf)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Leaf

    RootZoneWC%Leaf = Leaf
end subroutine SetRootZoneWC_Leaf

subroutine SetRootZoneWC_Thresh(Thresh)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Thresh

    RootZoneWC%Thresh = Thresh
end subroutine SetRootZoneWC_Thresh

subroutine SetRootZoneWC_Sen(Sen)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Sen

    RootZoneWC%Sen = Sen
end subroutine SetRootZoneWC_Sen

subroutine SetRootZoneWC_ZtopAct(ZtopAct)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopAct

    RootZoneWC%ZtopAct = ZtopAct
end subroutine SetRootZoneWC_ZtopAct

subroutine SetRootZoneWC_ZtopFC(ZtopFC)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopFC

    RootZoneWC%ZtopFC = ZtopFC
end subroutine SetRootZoneWC_ZtopFC

subroutine SetRootZoneWC_ZtopWP(ZtopWP)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopWP

    RootZoneWC%ZtopWP = ZtopWP
end subroutine SetRootZoneWC_ZtopWP

subroutine SetRootZoneWC_ZtopThresh(ZtopThresh)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopThresh

    RootZoneWC%ZtopThresh = ZtopThresh
end subroutine SetRootZoneWC_ZtopThresh

function GetCalendarFile() result(str)
    !! Getter for the "CalendarFile" global variable.
    character(len=len(CalendarFile)) :: str

    str = CalendarFile
end function GetCalendarFile

subroutine SetCalendarFile(str)
    !! Setter for the "CalendarFile" global variable.
    character(len=*), intent(in) :: str

    CalendarFile = str
end subroutine SetCalendarFile

function GetCalendarFileFull() result(str)
    !! Getter for the "CalendarFileFull" global variable.
    character(len=len(CalendarFileFull)) :: str

    str = CalendarFileFull
end function GetCalendarFileFull

subroutine SetCalendarFileFull(str)
    !! Setter for the "CalendarFileFull" global variable.
    character(len=*), intent(in) :: str

    CalendarFileFull = str
end subroutine SetCalendarFileFull

function GetCropFile() result(str)
    !! Getter for the "CropFile" global variable.
    character(len=len(CropFile)) :: str

    str = CropFile
end function GetCropFile

subroutine SetCropFile(str)
    !! Setter for the "CropFile" global variable.
    character(len=*), intent(in) :: str

    CropFile = str
end subroutine SetCropFile

function GetCropFileFull() result(str)
    !! Getter for the "CropFile" global variable.
    character(len=len(CropFileFull)) :: str

    str = CropFileFull
end function GetCropFileFull

subroutine SetCropFileFull(str)
    !! Setter for the "CropFile" global variable.
    character(len=*), intent(in) :: str

    CropFileFull = str
end subroutine SetCropFileFull

type(rep_IrriECw) function GetIrriECw()
    !! Getter for the "IrriECw" global variable.

    GetIrriECw = IrriECw
end function GetIrriECw

subroutine SetIrriECw_PreSeason(PreSeason)
    !! Setter for the "soil" global variable.
    real(dp), intent(in) :: PreSeason

    IrriECw%PreSeason = PreSeason
end subroutine SetIrriECw_PreSeason

subroutine SetIrriECw_PostSeason(PostSeason)
    !! Setter for the "soil" global variable.
    real(dp), intent(in) :: PostSeason

    IrriECw%PostSeason = PostSeason
end subroutine SetIrriECw_PostSeason

function GetProfFile() result(str)
    !! Getter for the "ProfFile" global variable.
    character(len=len(ProfFile)) :: str

    str = ProfFile
end function GetProfFile

subroutine SetProfFile(str)
    !! Setter for the "ProfFile" global variable.
    character(len=*), intent(in) :: str

    ProfFile = str
end subroutine SetProfFile

function GetProfFilefull() result(str)
    !! Getter for the "ProfFilefull" global variable.
    character(len=len(ProfFilefull)) :: str

    str = ProfFilefull
end function GetProfFilefull

subroutine SetProfFilefull(str)
    !! Setter for the "ProfFilefull" global variable.
    character(len=*), intent(in) :: str

    ProfFilefull = str
end subroutine SetProfFilefull

function GetManFile() result(str)
    !! Getter for the "ManFile" global variable.
    character(len=len(ManFile)) :: str

    str = ManFile
end function GetManFile

subroutine SetManFile(str)
    !! Setter for the "ManFile" global variable.
    character(len=*), intent(in) :: str

    ManFile = str
end subroutine SetManFile

function GetManFilefull() result(str)
    !! Getter for the "ManFilefull" global variable.
    character(len=len(ManFilefull)) :: str

    str = ManFilefull
end function GetManFilefull

subroutine SetManFilefull(str)
    !! Setter for the "ManFilefull" global variable.
    character(len=*), intent(in) :: str

    ManFilefull = str
end subroutine SetManFilefull

function GetOffSeasonFile() result(str)
    !! Getter for the "OffSeasonFile" global variable.
    character(len=len(OffSeasonFile)) :: str

    str = OffSeasonFile
end function GetOffSeasonFile

subroutine SetOffSeasonFile(str)
    !! Setter for the "OffSeasonFile" global variable.
    character(len=*), intent(in) :: str

    OffSeasonFile = str
end subroutine SetOffSeasonFile

function GetOffSeasonFilefull() result(str)
    !! Getter for the "OffSeasonFilefull" global variable.
    character(len=len(OffSeasonFilefull)) :: str

    str = OffSeasonFilefull
end function GetOffSeasonFilefull

subroutine SetOffSeasonFilefull(str)
    !! Setter for the "OffSeasonFilefull" global variable.
    character(len=*), intent(in) :: str

    OffSeasonFilefull = str
end subroutine SetOffSeasonFilefull

function GetObservationsFile() result(str)
    !! Getter for the "ObservationsFile" global variable.
    character(len=len(ObservationsFile)) :: str

    str = ObservationsFile
end function GetObservationsFile

subroutine SetObservationsFile(str)
    !! Setter for the "ObservationsFile" global variable.
    character(len=*), intent(in) :: str

    ObservationsFile = str
end subroutine SetObservationsFile

function GetObservationsFilefull() result(str)
    !! Getter for the "ObservationsFilefull" global variable.
    character(len=len(ObservationsFilefull)) :: str

    str = ObservationsFilefull
end function GetObservationsFilefull

subroutine SetObservationsFilefull(str)
    !! Setter for the "ObservationsFilefull" global variable.
    character(len=*), intent(in) :: str

    ObservationsFilefull = str
end subroutine SetObservationsFilefull

function GetObservationsDescription() result(str)
    !! Getter for the "ObservationsDescription" global variable.
    character(len=len(ObservationsDescription)) :: str

    str = ObservationsDescription
end function GetObservationsDescription

subroutine SetObservationsDescription(str)
    !! Setter for the "ObservationsDescription" global variable.
    character(len=*), intent(in) :: str

    ObservationsDescription = str
end subroutine SetObservationsDescription

function GetGroundWaterFile() result(str)
    !! Getter for the "GroundWaterFile" global variable.
    character(len=len(GroundWaterFile)) :: str

    str = GroundWaterFile
end function GetGroundWaterFile

subroutine SetGroundWaterFile(str)
    !! Setter for the "GroundWaterFile" global variable.
    character(len=*), intent(in) :: str

    GroundWaterFile = str
end subroutine SetGroundWaterFile

function GetGroundWaterFilefull() result(str)
    !! Getter for the "GroundWaterFilefull" global variable.
    character(len=len(GroundWaterFilefull)) :: str

    str = GroundWaterFilefull
end function GetGroundWaterFilefull

subroutine SetGroundWaterFilefull(str)
    !! Setter for the "GroundWaterFilefull" global variable.
    character(len=*), intent(in) :: str

    GroundWaterFilefull = str
end subroutine SetGroundWaterFilefull

type(rep_CropFileSet) function GetCropFileSet()
    !! Getter for the "CropFileSet" global variable.

    GetCropFileSet = CropFileSet
end function GetCropFileSet

subroutine SetCropFileSet_DaysFromSenescenceToEnd(DaysFromSenescenceToEnd)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: DaysFromSenescenceToEnd

    CropFileSet%DaysFromSenescenceToEnd = DaysFromSenescenceToEnd
end subroutine SetCropFileSet_DaysFromSenescenceToEnd

subroutine SetCropFileSet_DaysToHarvest(DaysToHarvest)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: DaysToHarvest

    CropFileSet%DaysToHarvest = DaysToHarvest
end subroutine SetCropFileSet_DaysToHarvest

subroutine SetCropFileSet_GDDaysFromSenescenceToEnd(GDDaysFromSenescenceToEnd)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: GDDaysFromSenescenceToEnd

    CropFileSet%GDDaysFromSenescenceToEnd = GDDaysFromSenescenceToEnd
end subroutine SetCropFileSet_GDDaysFromSenescenceToEnd

subroutine SetCropFileSet_GDDaysToHarvest(GDDaysToHarvest)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: GDDaysToHarvest

    CropFileSet%GDDaysToHarvest = GDDaysToHarvest
end subroutine SetCropFileSet_GDDaysToHarvest


function GetEToFile() result(str)
    !! Getter for the "EToFile" global variable.
    character(len=len(EToFile)) :: str

    str = EToFile
end function GetEToFile


subroutine SetEToFile(str)
    !! Setter for the "EToFile" global variable.
    character(len=*), intent(in) :: str

    EToFile = str
end subroutine SetEToFile


function GetEToFileFull() result(str)
    !! Getter for the "EToFileFull" global variable.
    character(len=len(EToFileFull)) :: str

    str = EToFileFull
end function GetEToFileFull


subroutine SetEToFileFull(str)
    !! Setter for the "EToFileFull" global variable.
    character(len=*), intent(in) :: str

    EToFileFull = str
end subroutine SetEToFileFull


function GetEToDescription() result(str)
    !! Getter for the "EToDescription" global variable.
    character(len=len(EToDescription)) :: str

    str = EToDescription
end function GetEToDescription


subroutine SetEToDescription(str)
    !! Setter for the "EToDescription" global variable.
    character(len=*), intent(in) :: str

    EToDescription = str
end subroutine SetEToDescription


function GetRainFile() result(str)
    !! Getter for the "RainFile" global variable.
    character(len=len(RainFile)) :: str

    str = RainFile
end function GetRainFile


subroutine SetRainFile(str)
    !! Setter for the "RainFile" global variable.
    character(len=*), intent(in) :: str

    RainFile = str
end subroutine SetRainFile


function GetRainFileFull() result(str)
    !! Getter for the "RainFileFull" global variable.
    character(len=len(RainFileFull)) :: str

    str = RainFileFull
end function GetRainFileFull


subroutine SetRainFileFull(str)
    !! Setter for the "RainFileFull" global variable.
    character(len=*), intent(in) :: str

    RainFileFull = str
end subroutine SetRainFileFull


function GetRainDescription() result(str)
    !! Getter for the "RainDescription" global variable.
    character(len=len(RainDescription)) :: str

    str = RainDescription
end function GetRainDescription


subroutine SetRainDescription(str)
    !! Setter for the "RainDescription" global variable.
    character(len=*), intent(in) :: str

    RainDescription = str
end subroutine SetRainDescription


integer(int8) function GetManagement_Mulch()
    !! Getter for the "Management" global variable.

    GetManagement_Mulch = Management%Mulch
end function GetManagement_Mulch

integer(int8) function GetManagement_SoilCoverBefore()
    !! Getter for the "Management" global variable.

    GetManagement_SoilCoverBefore = Management%SoilCoverBefore
end function GetManagement_SoilCoverBefore

integer(int8) function GetManagement_SoilCoverAfter()
    !! Getter for the "Management" global variable.

    GetManagement_SoilCoverAfter = Management%SoilCoverAfter
end function GetManagement_SoilCoverAfter

integer(int8) function GetManagement_EffectMulchOffS()
    !! Getter for the "Management" global variable.

    GetManagement_EffectMulchOffS = Management%EffectMulchOffS
end function GetManagement_EffectMulchOffS

integer(int8) function GetManagement_EffectMulchInS()
    !! Getter for the "Management" global variable.

    GetManagement_EffectMulchInS = Management%EffectMulchInS
end function GetManagement_EffectMulchInS

integer(int8) function GetManagement_FertilityStress()
    !! Getter for the "Management" global variable.

    GetManagement_FertilityStress = Management%FertilityStress
end function GetManagement_FertilityStress

real(dp) function GetManagement_BundHeight()
    !! Getter for the "Management" global variable.

    GetManagement_BundHeight = Management%BundHeight
end function GetManagement_BundHeight

logical function GetManagement_RunoffOn()
    !! Getter for the "Management" global variable.

    GetManagement_RunoffOn = Management%RunoffOn
end function GetManagement_RunoffOn

integer(int32) function GetManagement_CNcorrection()
    !! Getter for the "Management" global variable.

    GetManagement_CNcorrection = Management%CNcorrection
end function GetManagement_CNcorrection

integer(int8) function GetManagement_WeedRC()
    !! Getter for the "Management" global variable.

    GetManagement_WeedRC = Management%WeedRC
end function GetManagement_WeedRC

integer(int32) function GetManagement_WeedDeltaRC()
    !! Getter for the "Management" global variable.

    GetManagement_WeedDeltaRC = Management%WeedDeltaRC
end function GetManagement_WeedDeltaRC

real(dp) function GetManagement_WeedShape()
    !! Getter for the "Management" global variable.

    GetManagement_WeedShape = Management%WeedShape
end function GetManagement_WeedShape

integer(int8) function GetManagement_WeedAdj()
    !! Getter for the "Management" global variable.

    GetManagement_WeedAdj = Management%WeedAdj
end function GetManagement_WeedAdj

type(rep_Cuttings) function GetManagement_Cuttings()
    !! Setter for the "Management" global variable.

    GetManagement_Cuttings = Management%Cuttings
end function GetManagement_Cuttings

subroutine SetManagement_Mulch(Mulch)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: Mulch

    Management%Mulch = Mulch
end subroutine SetManagement_Mulch

subroutine SetManagement_SoilCoverBefore(SoilCoverBefore)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: SoilCoverBefore

    Management%SoilCoverBefore = SoilCoverBefore
end subroutine SetManagement_SoilCoverBefore

subroutine SetManagement_SoilCoverAfter(SoilCoverAfter)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: SoilCoverAfter

    Management%SoilCoverAfter = SoilCoverAfter
end subroutine SetManagement_SoilCoverAfter

subroutine SetManagement_EffectMulchOffS(EffectMulchOffS)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: EffectMulchOffS

    Management%EffectMulchOffS = EffectMulchOffS
end subroutine SetManagement_EffectMulchOffS

subroutine SetManagement_EffectMulchInS(EffectMulchInS)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: EffectMulchInS

    Management%EffectMulchInS = EffectMulchInS
end subroutine SetManagement_EffectMulchInS

subroutine SetManagement_FertilityStress(FertilityStress)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: FertilityStress

    Management%FertilityStress = FertilityStress
end subroutine SetManagement_FertilityStress

subroutine SetManagement_BundHeight(BundHeight)
    !! Setter for the "Management" global variable.
    real(dp), intent(in) :: BundHeight

    Management%BundHeight = BundHeight
end subroutine SetManagement_BundHeight

subroutine SetManagement_RunoffOn(RunoffOn)
    !! Setter for the "Management" global variable.
    logical, intent(in) :: RunoffOn

    Management%RunoffOn = RunoffOn
end subroutine SetManagement_RunoffOn

subroutine SetManagement_CNcorrection(CNcorrection)
    !! Setter for the "Management" global variable.
    integer(int32), intent(in) :: CNcorrection

    Management%CNcorrection = CNcorrection
end subroutine SetManagement_CNcorrection

subroutine SetManagement_WeedRC(WeedRC)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: WeedRC

    Management%WeedRC = WeedRC
end subroutine SetManagement_WeedRC

subroutine SetManagement_WeedDeltaRC(WeedDeltaRC)
    !! Setter for the "Management" global variable.
    integer(int32), intent(in) :: WeedDeltaRC

    Management%WeedDeltaRC = WeedDeltaRC
end subroutine SetManagement_WeedDeltaRC

subroutine SetManagement_WeedShape(WeedShape)
    !! Setter for the "Management" global variable.
    real(dp), intent(in) :: WeedShape

    Management%WeedShape = WeedShape
end subroutine SetManagement_WeedShape

subroutine SetManagement_WeedAdj(WeedAdj)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: WeedAdj

    Management%WeedAdj = WeedAdj
end subroutine SetManagement_WeedAdj

subroutine SetManagement_Cuttings(Cuttings)
    !! Setter for the "Management" global variable.
    type(rep_Cuttings), intent(in) :: Cuttings

    Management%Cuttings = Cuttings
end subroutine SetManagement_Cuttings

logical function GetManagement_Cuttings_Considered()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Considered = Cuttings%Considered
end function GetManagement_Cuttings_Considered

integer(int32) function GetManagement_Cuttings_CCcut()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_CCcut = Cuttings%CCcut
end function GetManagement_Cuttings_CCcut

integer(int32) function GetManagement_Cuttings_CGCPlus()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_CGCPlus = Cuttings%CGCPlus
end function GetManagement_Cuttings_CGCPlus

integer(int32) function GetManagement_Cuttings_Day1()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Day1 = Cuttings%Day1
end function GetManagement_Cuttings_Day1

integer(int32) function GetManagement_Cuttings_NrDays()
    !! Setter for the "Cuttings" global variable.

    GetManagement_Cuttings_NrDays = Cuttings%NrDays
end function GetManagement_Cuttings_NrDays

logical function GetManagement_Cuttings_Generate()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Generate = Cuttings%Generate
end function GetManagement_Cuttings_Generate

integer(intEnum) function GetManagement_Cuttings_Criterion()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Criterion = Cuttings%Criterion
end function GetManagement_Cuttings_Criterion

logical function GetManagement_Cuttings_HarvestEnd()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_HarvestEnd = Cuttings%HarvestEnd
end function GetManagement_Cuttings_HarvestEnd

integer(int32) function GetManagement_Cuttings_FirstDayNr()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_FirstDayNr = Cuttings%FirstDayNr
end function GetManagement_Cuttings_FirstDayNr

subroutine SetManagement_Cuttings_Considered(Considered)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: Considered

    Cuttings%Considered = Considered
end subroutine SetManagement_Cuttings_Considered

subroutine SetManagement_Cuttings_CCcut(CCcut)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: CCcut

    Cuttings%CCcut = CCcut
end subroutine SetManagement_Cuttings_CCcut

subroutine SetManagement_Cuttings_CGCPlus(CGCPlus)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: CGCPlus

    Cuttings%CGCPlus = CGCPlus
end subroutine SetManagement_Cuttings_CGCPlus

subroutine SetManagement_Cuttings_Day1(Day1)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: Day1

    Cuttings%Day1 = Day1
end subroutine SetManagement_Cuttings_Day1

subroutine SetManagement_Cuttings_NrDays(NrDays)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: NrDays

    Cuttings%NrDays = NrDays
end subroutine SetManagement_Cuttings_NrDays

subroutine SetManagement_Cuttings_Generate(Generate)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: Generate

    Cuttings%Generate = Generate
end subroutine SetManagement_Cuttings_Generate

subroutine SetManagement_Cuttings_Criterion(Criterion)
    !! Setter for the "Cuttings" global variable.
    integer(intEnum), intent(in) :: Criterion

    Cuttings%Criterion = Criterion
end subroutine SetManagement_Cuttings_Criterion

subroutine SetManagement_Cuttings_HarvestEnd(HarvestEnd)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: HarvestEnd

    Cuttings%HarvestEnd = HarvestEnd
end subroutine SetManagement_Cuttings_HarvestEnd

subroutine SetManagement_Cuttings_FirstDayNr(FirstDayNr)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: FirstDayNr

    Cuttings%FirstDayNr = FirstDayNr
end subroutine SetManagement_Cuttings_FirstDayNr

real(dp) function GetSumWaBal_Epot()
    !! Getter for the "SumWaBal" global variable.

     GetSumWaBal_Epot = SumWaBal%Epot
end function GetSumWaBal_Epot

real(dp) function GetSumWaBal_Tpot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Tpot = SumWaBal%Tpot
end function GetSumWaBal_Tpot

real(dp) function GetSumWaBal_Rain()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Rain = SumWaBal%Rain
end function GetSumWaBal_Rain

real(dp) function GetSumWaBal_Irrigation()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Irrigation = SumWaBal%Irrigation
end function GetSumWaBal_Irrigation

real(dp) function GetSumWaBal_Infiltrated()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Infiltrated = SumWaBal%Infiltrated
end function GetSumWaBal_Infiltrated

real(dp) function GetSumWaBal_Runoff()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Runoff = SumWaBal%Runoff
end function GetSumWaBal_Runoff

real(dp) function GetSumWaBal_Drain()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Drain = SumWaBal%Drain
end function GetSumWaBal_Drain

real(dp) function GetSumWaBal_Eact()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Eact = SumWaBal%Eact
end function GetSumWaBal_Eact

real(dp) function GetSumWaBal_Tact()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Tact = SumWaBal%Tact
end function GetSumWaBal_Tact

real(dp) function GetSumWaBal_TrW()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_TrW = SumWaBal%TrW
end function GetSumWaBal_TrW

real(dp) function GetSumWaBal_ECropCycle()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_ECropCycle = SumWaBal%ECropCycle
end function GetSumWaBal_ECropCycle

real(dp) function GetSumWaBal_CRwater()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_CRwater = SumWaBal%CRwater
end function GetSumWaBal_CRwater

real(dp) function GetSumWaBal_Biomass()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Biomass = SumWaBal%Biomass
end function GetSumWaBal_Biomass

real(dp) function GetSumWaBal_YieldPart()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_YieldPart = SumWaBal%YieldPart
end function GetSumWaBal_YieldPart

real(dp) function GetSumWaBal_BiomassPot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassPot = SumWaBal%BiomassPot
end function GetSumWaBal_BiomassPot

real(dp) function GetSumWaBal_BiomassUnlim()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassUnlim = SumWaBal%BiomassUnlim
end function GetSumWaBal_BiomassUnlim

real(dp) function GetSumWaBal_BiomassTot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassTot = SumWaBal%BiomassTot
end function GetSumWaBal_BiomassTot

real(dp) function GetSumWaBal_SaltIn()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_SaltIn = SumWaBal%SaltIn
end function GetSumWaBal_SaltIn

real(dp) function GetSumWaBal_SaltOut()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_SaltOut = SumWaBal%SaltOut
end function GetSumWaBal_SaltOut

real(dp) function GetSumWaBal_CRSalt()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_CRSalt = SumWaBal%CRSalt
end function GetSumWaBal_CRSalt

subroutine SetSumWaBal_Epot(Epot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Epot

    SumWaBal%Epot = Epot
end subroutine SetSumWaBal_Epot

subroutine SetSumWaBal_Tpot(Tpot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Tpot

    SumWaBal%Tpot = Tpot
end subroutine SetSumWaBal_Tpot

subroutine SetSumWaBal_Rain(Rain)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Rain

    SumWaBal%Rain = Rain
end subroutine SetSumWaBal_Rain

subroutine SetSumWaBal_Irrigation(Irrigation)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Irrigation

    SumWaBal%Irrigation = Irrigation
end subroutine SetSumWaBal_Irrigation

subroutine SetSumWaBal_Infiltrated(Infiltrated)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Infiltrated

    SumWaBal%Infiltrated = Infiltrated
end subroutine SetSumWaBal_Infiltrated

subroutine SetSumWaBal_Runoff(Runoff)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Runoff

    SumWaBal%Runoff = Runoff
end subroutine SetSumWaBal_Runoff

subroutine SetSumWaBal_Drain(Drain)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Drain

    SumWaBal%Drain = Drain
end subroutine SetSumWaBal_Drain

subroutine SetSumWaBal_Eact(Eact)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Eact

    SumWaBal%Eact = Eact
end subroutine SetSumWaBal_Eact

subroutine SetSumWaBal_Tact(Tact)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Tact

    SumWaBal%Tact = Tact
end subroutine SetSumWaBal_Tact

subroutine SetSumWaBal_TrW(TrW)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: TrW

    SumWaBal%TrW = TrW
end subroutine SetSumWaBal_TrW

subroutine SetSumWaBal_ECropCycle(ECropCycle)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: ECropCycle

    SumWaBal%ECropCycle = ECropCycle
end subroutine SetSumWaBal_ECropCycle

subroutine SetSumWaBal_CRwater(CRwater)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: CRwater

    SumWaBal%CRwater = CRwater
end subroutine SetSumWaBal_CRwater

subroutine SetSumWaBal_Biomass(Biomass)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Biomass

    SumWaBal%Biomass = Biomass
end subroutine SetSumWaBal_Biomass

subroutine SetSumWaBal_YieldPart(YieldPart)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: YieldPart

    SumWaBal%YieldPart = YieldPart
end subroutine SetSumWaBal_YieldPart

subroutine SetSumWaBal_BiomassPot(BiomassPot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassPot

    SumWaBal%BiomassPot = BiomassPot
end subroutine SetSumWaBal_BiomassPot

subroutine SetSumWaBal_BiomassUnlim(BiomassUnlim)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassUnlim

    SumWaBal%BiomassUnlim = BiomassUnlim
end subroutine SetSumWaBal_BiomassUnlim

subroutine SetSumWaBal_BiomassTot(BiomassTot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassTot

    SumWaBal%BiomassTot = BiomassTot
end subroutine SetSumWaBal_BiomassTot

subroutine SetSumWaBal_SaltIn(SaltIn)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: SaltIn

    SumWaBal%SaltIn = SaltIn
end subroutine SetSumWaBal_SaltIn

subroutine SetSumWaBal_SaltOut(SaltOut)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: SaltOut

    SumWaBal%SaltOut = SaltOut
end subroutine SetSumWaBal_SaltOut

subroutine SetSumWaBal_CRSalt(CRSalt)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: CRSalt

    SumWaBal%CRSalt = CRSalt
end subroutine SetSumWaBal_CRSalt

type(rep_soil) function GetSoil()
    !! Getter for the "Soil" global variable.

    GetSoil = Soil
end function GetSoil

subroutine SetSoil_REW(REW)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: REW

    Soil%REW = REW
end subroutine SetSoil_REW

subroutine SetSoil_NrSoilLayers(NrSoilLayers)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: NrSoilLayers

    Soil%NrSoilLayers = NrSoilLayers
end subroutine SetSoil_NrSoilLayers

subroutine SetSoil_CNvalue(CNvalue)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: CNvalue

    Soil%CNvalue = CNvalue
end subroutine SetSoil_CNvalue

subroutine SetSoil_RootMax(RootMax)
    !! Setter for the "Soil" global variable.
    real(sp), intent(in) :: RootMax

    Soil%RootMax = RootMax
end subroutine SetSoil_RootMax

type(rep_Content) function GetTotalSaltContent()
    !! Getter for the "TotalSaltContent" global variable.

    GetTotalSaltContent = TotalSaltContent
end function GetTotalSaltContent

subroutine SetTotalSaltContent_BeginDay(BeginDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: BeginDay

    TotalSaltContent%BeginDay = BeginDay
end subroutine SetTotalSaltContent_BeginDay

subroutine SetTotalSaltContent_EndDay(EndDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: EndDay

    TotalSaltContent%EndDay = EndDay
end subroutine SetTotalSaltContent_EndDay

subroutine SetTotalSaltContent_ErrorDay(ErrorDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: ErrorDay

    TotalSaltContent%ErrorDay = ErrorDay
end subroutine SetTotalSaltContent_ErrorDay

type(rep_Content) function GetTotalWaterContent()
    !! Getter for the "TotalWaterContent" global variable.

    GetTotalWaterContent = TotalWaterContent
end function GetTotalWaterContent

subroutine SetTotalWaterContent_BeginDay(BeginDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: BeginDay

    TotalWaterContent%BeginDay = BeginDay
end subroutine SetTotalWaterContent_BeginDay

subroutine SetTotalWaterContent_EndDay(EndDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: EndDay

    TotalWaterContent%EndDay = EndDay
end subroutine SetTotalWaterContent_EndDay

subroutine SetTotalWaterContent_ErrorDay(ErrorDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: ErrorDay

    TotalWaterContent%ErrorDay = ErrorDay
end subroutine SetTotalWaterContent_ErrorDay


type(rep_RootZoneSalt) function GetRootZoneSalt()
    !! Getter for the "RootZoneSalt" global variable.

    GetRootZoneSalt = RootZoneSalt
end function GetRootZoneSalt

subroutine SetRootZoneSalt_ECe(ECe)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECe

    RootZoneSalt%ECe = ECe
end subroutine SetRootZoneSalt_ECe

subroutine SetRootZoneSalt_ECsw(ECsw)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECsw

    RootZoneSalt%ECsw = ECsw
end subroutine SetRootZoneSalt_ECsw

subroutine SetRootZoneSalt_ECswFC(ECswFC)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECswFC

    RootZoneSalt%ECswFC = ECswFC
end subroutine SetRootZoneSalt_ECswFC

subroutine SetRootZoneSalt_KsSalt(KsSalt)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: KsSalt

    RootZoneSalt%KsSalt = KsSalt
end subroutine SetRootZoneSalt_KsSalt

integer(intEnum) function GetGenerateTimeMode()
    !! Getter for the "GenerateTimeMode" global variable.

    GetGenerateTimeMode = GenerateTimeMode
end function GetGenerateTimeMode

integer(intEnum) function GetGenerateDepthMode()
    !! Getter for the "GenerateDepthMode" global variable.

    GetGenerateDepthMode = GenerateDepthMode
end function GetGenerateDepthMode

subroutine SetGenerateTimeMode(int_in)
    !! Setter for the "GenerateTimeMode" global variable.
    integer(intEnum), intent(in) :: int_in

    GenerateTimeMode = int_in
end subroutine SetGenerateTimeMode

subroutine SetGenerateDepthMode(int_in)
    !! Setter for the "GenerateDepthMode" global variable.
    integer(intEnum), intent(in) :: int_in

    GenerateDepthMode = int_in
end subroutine SetGenerateDepthMode

integer(intEnum) function GetIrriMode()
    !! Getter for the "IrriMode" global variable.

    GetIrriMode = IrriMode
end function GetIrriMode

integer(intEnum) function GetIrriMethod()
    !! Getter for the "IrriMethod" global variable.

    GetIrriMethod = IrriMethod
end function GetIrriMethod

subroutine SetIrriMode(int_in)
    !! Setter for the "IrriMode" global variable.
    integer(intEnum), intent(in) :: int_in

    IrriMode = int_in
end subroutine SetIrriMode

subroutine SetIrriMethod(int_in)
    !! Setter for the "IrriMethod" global variable.
    integer(intEnum), intent(in) :: int_in

    IrriMethod = int_in
end subroutine SetIrriMethod


type(rep_clim) function GetTemperatureRecord()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord = TemperatureRecord
end function GetTemperatureRecord

integer(intEnum) function GetTemperatureRecord_DataType()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_DataType = TemperatureRecord%DataType
end function GetTemperatureRecord_DataType

integer(int32) function GetTemperatureRecord_FromD()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromD = TemperatureRecord%FromD
end function GetTemperatureRecord_FromD

integer(int32) function GetTemperatureRecord_FromM()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromM = TemperatureRecord%FromM
end function GetTemperatureRecord_FromM

integer(int32) function GetTemperatureRecord_FromY()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromY = TemperatureRecord%FromY
end function GetTemperatureRecord_FromY

integer(int32) function GetTemperatureRecord_ToD()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToD = TemperatureRecord%ToD
end function GetTemperatureRecord_ToD

integer(int32) function GetTemperatureRecord_ToM()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToM = TemperatureRecord%ToM
end function GetTemperatureRecord_ToM

integer(int32) function GetTemperatureRecord_ToY()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToY = TemperatureRecord%ToY
end function GetTemperatureRecord_ToY

integer(int32) function GetTemperatureRecord_FromDayNr()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromDayNr = TemperatureRecord%FromDayNr
end function GetTemperatureRecord_FromDayNr

integer(int32) function GetTemperatureRecord_ToDayNr()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToDayNr = TemperatureRecord%ToDayNr
end function GetTemperatureRecord_ToDayNr

function GetTemperatureRecord_FromString() result(str)
    !! Getter for the "TemperatureRecord" global variable.
    character(len=len(TemperatureRecord%FromString)) :: str

    str = TemperatureRecord%FromString
end function GetTemperatureRecord_FromString

function GetTemperatureRecord_ToString() result(str)
    !! Getter for the "TemperatureRecord" global variable.
    character(len=len(TemperatureRecord%ToString)) :: str

    str = TemperatureRecord%ToString
end function GetTemperatureRecord_ToString

integer(int32) function GetTemperatureRecord_NrObs()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_NrObs = TemperatureRecord%NrObs
end function GetTemperatureRecord_NrObs

subroutine SetTemperatureRecord_DataType(DataType)
    !! Setter for the "TemperatureRecord" global variable.
    integer(intEnum), intent(in) :: DataType

    TemperatureRecord%DataType = DataType
end subroutine SetTemperatureRecord_DataType

subroutine SetTemperatureRecord_FromD(FromD)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromD

    TemperatureRecord%FromD = FromD
end subroutine SetTemperatureRecord_FromD

subroutine SetTemperatureRecord_FromM(FromM)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromM

    TemperatureRecord%FromM = FromM
end subroutine SetTemperatureRecord_FromM

subroutine SetTemperatureRecord_FromY(FromY)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromY

    TemperatureRecord%FromY = FromY
end subroutine SetTemperatureRecord_FromY

subroutine SetTemperatureRecord_ToD(ToD)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToD

    TemperatureRecord%ToD = ToD
end subroutine SetTemperatureRecord_ToD

subroutine SetTemperatureRecord_ToM(ToM)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToM

    TemperatureRecord%ToM = ToM
end subroutine SetTemperatureRecord_ToM

subroutine SetTemperatureRecord_TOY(ToY)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToY

    TemperatureRecord%ToY = ToY
end subroutine SetTemperatureRecord_ToY

subroutine SetTemperatureRecord_ToDayNr(ToDayNr)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToDayNr

    TemperatureRecord%ToDayNr = ToDayNr
end subroutine SetTemperatureRecord_ToDayNr

subroutine SetTemperatureRecord_FromDayNr(FromDayNr)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromDayNr

    TemperatureRecord%FromDayNr = FromDayNr
end subroutine SetTemperatureRecord_FromDayNr

subroutine SetTemperatureRecord_NrObs(NrObs)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: NrObs

    TemperatureRecord%NrObs = NrObs
end subroutine SetTemperatureRecord_NrObs

subroutine SetTemperatureRecord_ToString(ToString)
    !! Setter for the "TemperatureRecord" global variable.
    character(len=*), intent(in) :: ToString

    TemperatureRecord%ToString = ToString
end subroutine SetTemperatureRecord_ToString

subroutine SetTemperatureRecord_FromString(FromString)
    !! Setter for the "TemperatureRecord" global variable.
    character(len=*), intent(in) :: FromString

    TemperatureRecord%FromString = FromString
end subroutine SetTemperatureRecord_FromString

end module ac_global
