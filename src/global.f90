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


contains


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


end module ac_global
