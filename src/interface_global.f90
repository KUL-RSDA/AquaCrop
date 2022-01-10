module ac_interface_global

use, intrinsic :: iso_c_binding, only: c_f_pointer, &
                                       c_ptr
use ac_global, only: GetNumberSimulationRuns
use ac_kinds, only: int32
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


subroutine GetCO2Description_wrap(CO2FileFull, strlen, CO2Description)
    !! Wrapper for [[ac_global:GetCO2Description]] for foreign languages.
    type(c_ptr), intent(in) :: COI2FileFull
    integer(int32), intent(in) :: strlen
    character(len=*), intent(inout) :: CO2Description

    character(len=strlen) :: string

    string = pointer2string(CO2FileFull, strlen)
    call GetNumberSimulationRuns(string, CO2Description)
end subroutine GetCO2Description_wrap

end module ac_interface_global
