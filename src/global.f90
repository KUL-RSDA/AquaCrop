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


character(len=:), allocatable :: CalendarFile
character(len=:), allocatable :: CO2File
character(len=:), allocatable :: IrriFile
character(len=:), allocatable :: CropFile
character(len=:), allocatable :: ProfFile
type(rep_IrriECw) :: IrriECw
type(rep_RootZoneWC) :: RootZoneWC
type(rep_CropFileSet) :: CropFileSet


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


subroutine GetCO2Description(CO2FileFull, CO2Description)
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
end subroutine GetCO2Description


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


end module ac_global
