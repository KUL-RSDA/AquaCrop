module ac_climprocessing

use ac_kinds, only:  dp, &
                    int8, &
                    int32, &
                    sp
implicit none


contains


subroutine GetInterpolationParameters(C1, C2, C3, X1, X2, X3, aOver3, bOver2, c)
    real(dp), intent(in) :: C1
    real(dp), intent(in) :: C2
    real(dp), intent(in) :: C3
    integer(int32), intent(in) :: X1
    integer(int32), intent(in) :: X2
    integer(int32), intent(in) :: X3
    real(dp), intent(inout) :: aOver3
    real(dp), intent(inout) :: bOver2
    real(dp), intent(inout) :: c

    ! n1=n2=n3=30 --> better parabola
    aOver3 = (C1-2*C2+C3)/(6*30*30*30)
    bOver2 = (-6*C1+9*C2-3*C3)/(6*30*30)
    c = (11*C1-7*C2+2*C3)/(6*30)

end subroutine GetInterpolationParameters


end module ac_climprocessing
