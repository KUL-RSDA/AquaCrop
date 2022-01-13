module ac_climprocessing

use ac_kinds, only: dp, &
                    int8, &
                    int32, &
                    sp
implicit none


contains


subroutine AdjustDecadeMONTHandYEAR(DecFile, Mfile, Yfile)
    integer(int32), intent(inout) :: DecFile
    integer(int32), intent(inout) :: Mfile
    integer(int32), intent(inout) :: Yfile

    DecFile = 1
    Mfile = Mfile + 1
    if (Mfile > 12) then
        Mfile = 1
        YFile = Yfile + 1
    end if
end subroutine AdjustDecadeMONTHandYEAR


subroutine AdjustMONTHandYEAR(Mfile, Yfile)
    integer(int32), intent(inout) :: Mfile
    integer(int32), intent(inout) :: Yfile

    Mfile = Mfile - 12
    YFile = Yfile + 1
end subroutine AdjustMONTHandYEAR


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
