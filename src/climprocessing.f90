module ac_climprocessing

use ac_kinds, only: dp, &
                    int8, &
                    int32, &
                    sp
implicit none


contains

subroutine GetParameters(C1, C2, C3, UL, LL, Mid)
    real(dp), intent(in) :: C1
    real(dp), intent(in) :: C2
    real(dp), intent(in) :: C3
    real(dp), intent(inout) :: UL
    real(dp), intent(inout) :: LL
    real(dp), intent(inout) :: Mid

UL = (C1+C2)/2.0_dp
LL = (C2+C3)/2.0_dp
Mid = 2.0_dp*C2 - (UL+LL)/2.0_dp
! --previous decade-->/UL/....... Mid ......../LL/<--next decade--
end subroutine GetParameters



end module ac_climprocessing
