module ac_global

use ac_kinds, only: dp, &
                    int8, &
                    int16
implicit none


real(dp), parameter :: undef_double = -9.9_dp
    !! value for 'undefined' real(dp) variables
integer(int16), parameter :: undef_int = -9
    !! value for 'undefined' int16 variables


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

end module ac_global
