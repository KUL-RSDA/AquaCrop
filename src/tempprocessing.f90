module ac_tempprocessing

use ac_kinds, only: int32
implicit none


contains


subroutine AdjustMONTHandYEAR(MFile, Yfile)
    integer(int32), intent(inout) :: MFile
    integer(int32), intent(inout) :: Yfile

    Mfile = Mfile - 12
    YFile = Yfile + 1
end subroutine AdjustMONTHandYEAR

end module ac_tempprocessing
