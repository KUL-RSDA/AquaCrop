module ac_interface_global

use, intrinsic :: iso_c_binding, only: c_f_pointer, &
                                       c_loc, &
                                       c_null_char, &
                                       c_ptr
use ac_global, only: GetNumberSimulationRuns, &
                     SplitStringInTwoParams, &
                     SplitStringInThreeParams, &
                     FileExists, &
                     GetCO2Description, &
                     GetCO2File, &
                     SetCO2File, &
                     GetProfFile, &
                     SetProfFile
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


function string2pointer(string) result(c_pointer)
    !! Returns a C-pointer from a Fortran string.
    character(len=*), intent(in) :: string
    type(c_ptr) :: c_pointer

    character(len=:), allocatable, target, save :: f_string

    f_string = string // c_null_char
    c_pointer = c_loc(f_string)
end function string2pointer


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


subroutine GetCO2Description_wrap(CO2FileFull, strlen1, CO2Description, &
            strlen2)
    !! Wrapper for [[ac_global:GetCO2Description]] for foreign languages.
    type(c_ptr), intent(in) :: CO2FileFull
    integer(int32), intent(in) :: strlen1
    type(c_ptr), intent(inout) :: CO2Description
    integer(int32), intent(in) :: strlen2

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2

    string1 = pointer2string(CO2FileFull, strlen1)
    string2 = pointer2string(CO2Description, strlen2)
    call GetCO2Description(string1, string2)
end subroutine GetCO2Description_wrap


function GetCO2File_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCO2File]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCO2File())
end function GetCO2File_wrap


subroutine SetCO2File_wrap(CO2File, strlen)
    !! Wrapper for [[ac_global:SetCO2File]] for foreign languages.
    type(c_ptr), intent(in) :: CO2File
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CO2File, strlen)
    call SetCO2File(string)
end subroutine SetCO2File_wrap


function GetProfFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetProfFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetProfFile())
end function GetProfFile_wrap


subroutine SetProfFile_wrap(ProfFile, strlen)
    !! Wrapper for [[ac_global:SetProfFile]] for foreign languages.
    type(c_ptr), intent(in) :: ProfFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ProfFile, strlen)
    call SetProfFile(string)
end subroutine SetProfFile_wrap



end module ac_interface_global
