module ac_interface_global

use, intrinsic :: iso_c_binding, only: c_f_pointer, &
                                       c_loc, &
                                       c_null_char, &
                                       c_ptr
use ac_global, only: CheckFilesInProject, &
                     DetermineLengthGrowthStages, &
                     FileExists, &
                     GetCalendarFile, &
                     GetCO2Description, &
                     GetCO2File, &
                     GetCropFile, &
                     GetIrriDescription, &
                     GetIrriFile, &
                     GetClimateFile, &
                     GetClimFile, &
                     GetSWCiniFile, &
                     GetNumberSimulationRuns, &
                     GetProfFile, &
                     GetProfFilefull, &
                     GetManFile, &
                     GetManFilefull, &
                     GetOffSeasonFile, &
                     GetOffSeasonFilefull, &
                     GetGroundWaterFile, &
                     GetGroundWaterFilefull, &
                     SetCO2File, &
                     GetEToFile, &
                     SetEToFile, &
                     GetEToFileFull, &
                     SetEToFileFull, &
                     GetRainFile, &
                     setRainFile, &
                     GetRainFileFull, &
                     setRainFileFull, &
                     SetCalendarFile, &
                     SetCropFile, &
                     SetIrriFile, &
                     SetClimateFile, &
                     SetClimFile, &
                     setSWCinifile, &
                     SetProfFile, &
                     SetProfFilefull, &
                     SetManFile, &
                     SetManFilefull, &
                     SetOffSeasonFile, &
                     SetOffSeasonFilefull, &
                     SetGroundWaterFile, &
                     SetGroundWaterFilefull, &
                     SplitStringInTwoParams, &
                     SplitStringInThreeParams
use ac_kinds, only: dp, &
                    int32, &
                    intEnum
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

subroutine CheckFilesInProject_wrap(TempFullFilename, strlen, Runi, AllOK)
    !! Wrapper for [[ac_global:CheckFilesInProject]] for foreign languages.
    type(c_ptr), intent(in) :: TempFullFilename
    integer(int32), intent(in) :: strlen
    integer(int32), intent(in) :: Runi
    logical, intent(inout) :: AllOK

    character(len=strlen) :: string

    string = pointer2string(TempFullFilename, strlen)
    call CheckFilesInProject(string, Runi, AllOK)
end subroutine CheckFilesInProject_wrap

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


subroutine DetermineLengthGrowthStages_wrap(CCoVal, CCxVal, CDCVal, L0, &
                        TotalLength, CGCgiven, TheDaysToCCini, ThePlanting, &
                        Length123, StLength, Length12, CGCVal)
    real(dp), intent(in) :: CCoVal
    real(dp), intent(in) :: CCxVal
    real(dp), intent(in) :: CDCVal
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: TotalLength
    logical(1), intent(in) :: CGCgiven
    integer(int32), intent(in) :: TheDaysToCCini
    integer(intEnum), intent(in) :: ThePlanting
    integer(int32), intent(inout) :: Length123
    integer(int32), dimension(4), intent(inout) :: StLength
    integer(int32), intent(inout) :: Length12
    real(dp), intent(inout) :: CGCVal

    logical :: CGCgiven_f

    CGCgiven_f = CGCgiven
    call DetermineLengthGrowthStages(CCoVal, CCxVal, CDCVal, L0, TotalLength, &
                                     CGCgiven_f, TheDaysToCCini, ThePlanting, &
                                     Length123, StLength, Length12, CGCVal)
end subroutine DetermineLengthGrowthStages_wrap


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



function GetEToFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetEToFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetEToFile())
end function GetEToFile_wrap


subroutine SetEToFile_wrap(EToFile, strlen)
    !! Wrapper for [[ac_global:SetEToFile]] for foreign languages.
    type(c_ptr), intent(in) :: EToFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
    
    string = pointer2string(EToFile, strlen)
    call SetEToFile(string)
end subroutine SetEToFile_wrap

function GetEToFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetEToFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetEToFileFull())
end function GetEToFileFull_wrap


subroutine SetEToFileFull_wrap(EToFileFull, strlen)
    !! Wrapper for [[ac_global:SetEToFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: EToFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
    
    string = pointer2string(EToFileFull, strlen)
    call SetEToFileFull(string)
end subroutine SetEToFileFull_wrap

subroutine GetIrriDescription_wrap(IrriFileFull, strlen1, IrriDescription, &
            strlen2)
    !! Wrapper for [[ac_global:GetIrriDescription]] for foreign languages.
    type(c_ptr), intent(in) :: IrriFileFull
    integer(int32), intent(in) :: strlen1
    type(c_ptr), intent(inout) :: IrriDescription
    integer(int32), intent(in) :: strlen2

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2

    string1 = pointer2string(IrriFileFull, strlen1)
    string2 = pointer2string(IrriDescription, strlen2)
    call GetIrriDescription(string1, string2)
end subroutine GetIrriDescription_wrap


function GetIrriFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetIrriFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetIrriFile())
end function GetIrriFile_wrap


subroutine SetIrriFile_wrap(IrriFile, strlen)
    !! Wrapper for [[ac_global:SetIrriFile]] for foreign languages.
    type(c_ptr), intent(in) :: IrriFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(IrriFile, strlen)
    call SetIrriFile(string)
end subroutine SetIrriFile_wrap


function GetClimateFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetClimateFile]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetClimateFile())
end function GetClimateFile_wrap


subroutine SetClimateFile_wrap(ClimateFile, strlen)
    !! Wrapper for [[ac_global:SetClimateFile]] for foreign languages.
    type(c_ptr), intent(in) :: ClimateFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ClimateFile, strlen)
    call SetClimateFile(string)
end subroutine SetClimateFile_wrap


function GetClimFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetClimFile]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetClimFile())
end function GetClimFile_wrap


subroutine SetClimFile_wrap(ClimFile, strlen)
    !! Wrapper for [[ac_global:SetClimFile]] for foreign languages.
    type(c_ptr), intent(in) :: ClimFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ClimFile, strlen)
    call SetClimFile(string)
end subroutine SetClimFile_wrap


function GetSWCiniFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetSWCiniFile]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetSWCiniFile())
end function GetSWCiniFile_wrap


subroutine SetSWCiniFile_wrap(SWCiniFile, strlen)
    !! Wrapper for [[ac_global:SetSWCiniFile]] for foreign languages.
    type(c_ptr), intent(in) :: SWCiniFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(SWCiniFile, strlen)
    call SetSWCiniFile(string)
end subroutine SetSWCiniFile_wrap



function GetRainFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetRainFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetRainFile())
end function GetRainFile_wrap


subroutine SetRainFile_wrap(RainFile, strlen)
    !! Wrapper for [[ac_global:SetRainFile]] for foreign languages.
    type(c_ptr), intent(in) :: RainFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
 
    string = pointer2string(RainFile, strlen)
    call SetRainFile(string)
end subroutine SetRainFile_wrap

function GetRainFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetRainFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetRainFileFull())
end function GetRainFileFull_wrap


subroutine SetRainFileFull_wrap(RainFileFull, strlen)
    !! Wrapper for [[ac_global:SetRainFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: RainFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
 
    string = pointer2string(RainFileFull, strlen)
    call SetRainFileFull(string)
end subroutine SetRainFileFull_wrap

    
function GetCalendarFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCalendarFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCalendarFile())
end function GetCalendarFile_wrap

subroutine SetCalendarFile_wrap(CalendarFile, strlen)
    !! Wrapper for [[ac_global:SetCO2File]] for foreign languages.
    type(c_ptr), intent(in) :: CalendarFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CalendarFile, strlen)
    call SetCalendarFile(string)
end subroutine SetCalendarFile_wrap

function GetCropFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCropFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCropFile())
end function GetCropFile_wrap

subroutine SetCropFile_wrap(CropFile, strlen)
    !! Wrapper for [[ac_global:SetCropFile]] for foreign languages.
    type(c_ptr), intent(in) :: CropFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CropFile, strlen)
    call SetCropFile(string)
end subroutine SetCropFile_wrap

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


function GetProfFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetProfFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetProfFilefull())
end function GetProfFilefull_wrap

subroutine SetProfFilefull_wrap(ProfFilefull, strlen)
    !! Wrapper for [[ac_global:SetProfFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: ProfFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ProfFilefull, strlen)
    call SetProfFilefull(string)
end subroutine SetProfFilefull_wrap

function GetManFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetManFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetManFile())
end function GetManFile_wrap

subroutine SetManFile_wrap(ManFile, strlen)
    !! Wrapper for [[ac_global:SetManFile]] for foreign languages.
    type(c_ptr), intent(in) :: ManFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ManFile, strlen)
    call SetManFile(string)
end subroutine SetManFile_wrap


function GetManFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetManFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetManFilefull())
end function GetManFilefull_wrap

subroutine SetManFilefull_wrap(ManFilefull, strlen)
    !! Wrapper for [[ac_global:SetManFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: ManFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ManFilefull, strlen)
    call SetManFilefull(string)
end subroutine SetManFilefull_wrap

function GetOffSeasonFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetOffSeasonFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetOffSeasonFile())
end function GetOffSeasonFile_wrap

subroutine SetOffSeasonFile_wrap(OffSeasonFile, strlen)
    !! Wrapper for [[ac_global:SetvFile]] for foreign languages.
    type(c_ptr), intent(in) :: OffSeasonFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(OffSeasonFile, strlen)
    call SetOffSeasonFile(string)
end subroutine SetOffSeasonFile_wrap


function GetOffSeasonFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetOffSeasonFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetOffSeasonFilefull())
end function GetOffSeasonFilefull_wrap

subroutine SetOffSeasonFilefull_wrap(OffSeasonFilefull, strlen)
    !! Wrapper for [[ac_global:SetOffSeasonFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: OffSeasonFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(OffSeasonFilefull, strlen)
    call SetOffSeasonFilefull(string)
end subroutine SetOffSeasonFilefull_wrap


function GetGroundWaterFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetGroundWaterFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetGroundWaterFile())
end function GetGroundWaterFile_wrap

subroutine SetGroundWaterFile_wrap(GroundWaterFile, strlen)
    !! Wrapper for [[ac_global:SetvFile]] for foreign languages.
    type(c_ptr), intent(in) :: GroundWaterFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(GroundWaterFile, strlen)
    call SetGroundWaterFile(string)
end subroutine SetGroundWaterFile_wrap


function GetGroundWaterFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetGroundWaterFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetGroundWaterFilefull())
end function GetGroundWaterFilefull_wrap

subroutine SetGroundWaterFilefull_wrap(GroundWaterFilefull, strlen)
    !! Wrapper for [[ac_global:SetGroundWaterFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: GroundWaterFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(GroundWaterFilefull, strlen)
    call SetGroundWaterFilefull(string)
end subroutine SetGroundWaterFilefull_wrap


end module ac_interface_global
