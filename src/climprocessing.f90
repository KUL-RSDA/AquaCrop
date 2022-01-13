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


end module ac_climprocessing
