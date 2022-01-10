module ac_interface_global

use, intrinsic :: iso_c_binding, only: c_f_pointer, &
                                       c_ptr
use ac_global, only: GetNumberSimulationRuns, &
                     SplitStringInTwoParams, &
                     SplitStringInThreeParams, &
                     FileExists
use ac_kinds, only: dp, &
                    int32
implicit none


contains


function pointer2string(c_pointer, strlen) result(string)
    !! Returns a Fortran string from a C-pointer plus the string length.
    type(c_ptr), intent(in) :: c_pointer
        !! C-style pointer
    integer(int32), intent(in) :: strlen
        !! Length of the string
    character(len=strlen) :: string

    character, pointer, dimension(:) :: f_pointer
    integer :: i

    call c_f_pointer(c_pointer, f_pointer, [strlen])

    do i = 1, strlen
        string(i:i) = f_pointer(i)
    end do
end function pointer2string


subroutine GetNumberSimulationRuns_wrap(TempFileNameFull, strlen, NrRuns)
    !! Wrapper for [[ac_global:GetNumberSimulationRuns]] for foreign languages.
    type(c_ptr), intent(in) :: TempFileNameFull
    integer(int32), intent(in) :: strlen
    integer(int32), intent(out) :: NrRuns

    character(len=strlen) :: string

    string = pointer2string(TempFileNameFull, strlen)
    call GetNumberSimulationRuns(string, NrRuns)
end subroutine GetNumberSimulationRuns_wrap


logical function FileExists_wrap(full_name, strlen)
    !! Wrapper for [[ac_global:FileExists]] for foreign languages.
    type(c_ptr), intent(in) :: full_name
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(full_name, strlen)
    FileExists_wrap = FileExists(string)
end function FileExists_wrap


subroutine SplitStringInTwoParams_wrap(StringIN, strlen, Par1, Par2)
    !! Wrapper for [[ac_global:SplitStringInTwoParams]] for foreign languages.
    type(c_ptr), intent(in) :: StringIN
    integer(int32), intent(in) :: strlen
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2

    character(len=strlen) :: string

    string = pointer2string(StringIN, strlen)
    call SplitStringInTwoParams(string, Par1, Par2)
end subroutine SplitStringInTwoParams_wrap

subroutine SplitStringInThreeParams_wrap(StringIN, strlen, Par1, Par2, Par3)
    !! Wrapper for [[ac_global:SplitStringInTwoParams]] for foreign languages.
    type(c_ptr), intent(in) :: StringIN
    integer(int32), intent(in) :: strlen
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2
    real(dp), intent(inout) :: Par3

    character(len=strlen) :: string

    string = pointer2string(StringIN, strlen)
    call SplitStringInThreeParams(string, Par1, Par2, Par3)
end subroutine SplitStringInThreeParams_wrap

end module ac_interface_global
