module ac_global

use ac_kinds, only: dp, &
                    int8, &
                    int16, &
                    intEnum, &
                    sp
implicit none


integer(int16), parameter :: max_SoilLayers = 5
real(dp), parameter :: undef_double = -9.9_dp
    !! value for 'undefined' real(dp) variables
integer(int16), parameter :: undef_int = -9
    !! value for 'undefined' int16 variables
real(dp), parameter :: PI = 3.1415926535_dp
real(dp), parameter :: CO2Ref = 369.41_dp; 
    !! reference CO2 in ppm by volume for year 2000 for Mauna Loa
    !! (Hawaii,USA)

integer(intEnum), parameter :: modeCycle_GDDDays = 0
    !! index of GDDDays in modeCycle enumerated type
integer(intEnum), parameter :: modeCycle_CalendarDays = 1
    !! index of CalendarDays in modeCycle enumerated type

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

    integer(int16) :: i

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
    integer(int16), intent(in) :: Lo
    integer(int16), intent(in) :: LZxAdj

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
    integer(int16), intent(in) :: TheDay
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: fCCx
    integer(int8), intent(in) :: TempWeedRCinput
    integer(int8), intent(in) :: TempWeedAdj
    integer(int16), intent(inout) :: TempWeedDeltaRC
    integer(int16), intent(in) :: L12SF
    integer(int16), intent(in) :: TempL123
    integer(int16), intent(in) :: GDDL12SF
    integer(int16), intent(in) :: TempGDDL123
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
                                       kind=int16)
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

    integer(int16) :: TauTemp

    if (abs(Ksat) < epsilon(1._dp)) then
        TauFromKsat = 0
    else
        TauTemp = nint(100.0_dp*0.0866_dp*exp(0.35_dp*log(Ksat)), kind=int16)
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


real(dp) function BMRange(HIadj)
    integer(int16), intent(in) :: HIadj

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
    integer(int16), intent(in) :: Dayi
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
    integer(int16), intent(in) :: FromY
    integer(int16), intent(in) :: FromD
    integer(int16), intent(in) :: FromM
    integer(int16), intent(in) :: ToD
    integer(int16), intent(in) :: ToM

    FullUndefinedRecord = ((FromY == 1901) .and. (FromD == 1)&
        .and. (FromM == 1) .and. (ToD == 31) .and. (ToM == 12))
end function FullUndefinedRecord


end module ac_global
