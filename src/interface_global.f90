module ac_interface_global

use, intrinsic :: iso_c_binding, only: c_f_pointer, &
                                       c_loc, &
                                       c_null_char, &
                                       c_ptr
use ac_global, only: CheckFilesInProject, &
                     DetermineLengthGrowthStages, &
                     TimeToMaxCanopySF, &
                     FileExists, &
                     GetCalendarFile, &
                     GetCalendarFileFull, &
                     GetCalendarDescription, &
                     GenerateCO2Description, &
                     GetCO2File, &
                     GetCO2FileFull, &
                     GetCO2Description, &
                     GetCropFile, &
                     GetCropFileFull, &
                     GetCropDescription, &
                     GetCrop_DeterminancyLinked, &
                     GetCrop_SownYear1, &
                     GetCrop_StressResponse_Calibrated, &
                     GetCrop_Assimilates_On, &
                     GetIrriDescription, &
                     GetIrriFile, &
                     GetIrriFileFull, &
                     GetClimateFile, &
                     GetClimateFileFull, &
                     GetClimateDescription, &
                     GetClimFile, &
                     GetSWCiniFile, &
                     GetSWCiniFileFull, &
                     GetProjectFile, &
                     GetProjectFileFull, &
                     GetMultipleProjectFile, &
                     GetMultipleProjectFileFull, &
                     GetNumberSimulationRuns, &
                     GetPathNameProg, &
                     GetPathNameOutp, &
                     GetPathNameSimul, &
                     GetProfFile, &
                     GetProfFilefull, &
                     GetProfDescription, &
                     GetManagement_Cuttings_Considered, &
                     GetManagement_Cuttings_Generate, &
                     GetManagement_Cuttings_HarvestEnd, &
                     GetManagement_RunoffOn, &
                     GetManFile, &
                     GetManFilefull, &
                     GetObservationsFile, &
                     GetObservationsFilefull, &
                     GetObservationsDescription, &
                     GetOffSeasonFile, &
                     GetOffSeasonFilefull, &
                     GetGroundWaterFile, &
                     GetGroundWaterFilefull, &
                     GetEToFile, &
                     GetEToFileFull, &
                     GetEToDescription, &
                     GetOnset_GenerateOn, &
                     GetOnset_GenerateTempOn, &
                     GetRainFile, &
                     setRainFile, &
                     GetRainFileFull, &
                     GetRainDescription, &
                     LoadClimate, &
                     LoadProjectDescription, &
                     setRainFileFull, &
                     setRainDescription, &
                     SetCalendarFile, &
                     SetCalendarFileFull, &
                     SetCalendarDescription, &
                     SetCO2File, &
                     SetCO2FileFull, &
                     SetCO2Description, &
                     SetCropFile, &
                     SetCropFileFull, &
                     SetCropDescription, &
                     SetCrop_DeterminancyLinked, &
                     SetCrop_SownYear1, &
                     SetCrop_StressResponse_Calibrated, &
                     SetCrop_Assimilates_On, &
                     SetIrriFile, &
                     SetIrriFileFull, &
                     SetClimateFile, &
                     SetClimateFileFull, &
                     SetClimateDescription, &
                     SetClimFile, &
                     SetEToFile, &
                     SetEToFileFull, &
                     SetEToDescription, &
                     setSWCiniFile, &
                     setSWCiniFileFull, &
                     SetPathNameProg, &
                     SetPathNameOutp, &
                     SetPathNameSimul, &
                     SetProjectFile, &
                     SetProjectFileFull, &
                     SetMultipleProjectFile, &
                     SetMultipleProjectFileFull, &
                     SetProfFile, &
                     SetProfFilefull, &
                     SetProfDescription, &
                     SetManFile, &
                     SetManFilefull, &
                     SetManagement_Cuttings_Considered, &
                     SetManagement_Cuttings_Generate, &
                     SetManagement_Cuttings_HarvestEnd, &
                     SetManagement_RunoffOn, &
                     SetOnset_GenerateOn, &
                     SetOnset_GenerateTempOn, &
                     SetObservationsFile, &
                     SetObservationsFilefull, &
                     SetObservationsDescription, &
                     SetOffSeasonFile, &
                     SetOffSeasonFilefull, &
                     SetGroundWaterFile, &
                     SetGroundWaterFilefull, &
                     GetTemperatureFile, &
                     GetTemperatureFilefull, &
                     GetTemperatureDescription, &
                     SetTemperatureFile, &
                     SetTemperatureFilefull, &
                     SetTemperatureDescription, &
                     SplitStringInTwoParams, &
                     SplitStringInThreeParams, &
                     SetTemperatureRecord_FromString, &
                     GetTemperatureRecord_FromString, &
                     SetTemperatureRecord_ToString, &
                     GetTemperatureRecord_ToString
use ac_kinds, only: dp, &
                    int32, &
                    intEnum, &
                    int8
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

function GetCrop_Assimilates_On_wrap() result(On)
    !! Wrapper for [[ac_global:GetCrop_Assimilates_On]] for foreign languages.
    logical(1) :: On

    On = GetCrop_Assimilates_On()
end function GetCrop_Assimilates_On_wrap

subroutine SetCrop_Assimilates_On_wrap(On)
    !! Wrapper for [[ac_global:SetCrop_Assimilates_On]] for foreign languages.
    logical(1), intent(in) :: On

    logical :: bool

    bool = On
    call SetCrop_Assimilates_On(bool)
end subroutine SetCrop_Assimilates_On_wrap

function GetCrop_DeterminancyLinked_wrap() result(DeterminancyLinked)
    !! Wrapper for [[ac_global:GetCrop_DeterminancyLinked]] for foreign languages.
    logical(1) :: DeterminancyLinked

    DeterminancyLinked = GetCrop_DeterminancyLinked()
end function GetCrop_DeterminancyLinked_wrap

function GetCrop_SownYear1_wrap() result(SownYear1)
    !! Wrapper for [[ac_global:GetCrop_SownYear1]] for foreign languages.
    logical(1) :: SownYear1

    SownYear1 = GetCrop_SownYear1()
end function GetCrop_SownYear1_wrap

subroutine SetCrop_DeterminancyLinked_wrap(DeterminancyLinked)
    !! Wrapper for [[ac_global:SetCrop_DeterminancyLinked]] for foreign languages.
    logical(1), intent(in) :: DeterminancyLinked

    logical :: bool

    bool = DeterminancyLinked
    call SetCrop_DeterminancyLinked(bool)
end subroutine SetCrop_DeterminancyLinked_wrap

subroutine SetCrop_SownYear1_wrap(SownYear1)
    !! Wrapper for [[ac_global:SetCrop_SownYear1]] for foreign languages.
    logical(1), intent(in) :: SownYear1

    logical :: bool

    bool = SownYear1
    call SetCrop_SownYear1(bool)
end subroutine SetCrop_SownYear1_wrap

function GetCrop_StressResponse_Calibrated_wrap() result(Calibrated)
    !! Wrapper for [[ac_global:GetCrop_StressResponse_Calibrated]] for foreign languages.
    logical(1) :: Calibrated

    Calibrated = GetCrop_StressResponse_Calibrated()
end function GetCrop_StressResponse_Calibrated_wrap

subroutine SetCrop_StressResponse_Calibrated_wrap(Calibrated)
    !! Wrapper for [[ac_global:SetCrop_StressResponse_Calibrated]] for foreign languages.
    logical(1), intent(in) :: Calibrated

    logical :: bool

    bool = Calibrated
    call SetCrop_StressResponse_Calibrated(bool)
end subroutine SetCrop_StressResponse_Calibrated_wrap

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

subroutine LoadProjectDescription_wrap(FullNameProjectFile, strlen1, &
                                                DescriptionOfProject, strlen2)
    !! Wrapper for [[ac_global:LoadProjectDescription]] for foreign languages.
    type(c_ptr), intent(in) :: FullNameProjectFile
    integer(int32), intent(in) :: strlen1
    type(c_ptr), intent(inout) :: DescriptionOfProject
    integer(int32), intent(in) :: strlen2

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2

    string1 = pointer2string(FullNameProjectFile, strlen1)
    string2 = pointer2string(DescriptionOfProject, strlen2)
    call LoadProjectDescription(string1, string2)
end subroutine LoadProjectDescription_wrap

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

subroutine GenerateCO2Description_wrap(CO2FileFull, strlen1, CO2Description, &
            strlen2)
    !! Wrapper for [[ac_global:GenerateCO2Description]] for foreign languages.
    type(c_ptr), intent(in) :: CO2FileFull
    integer(int32), intent(in) :: strlen1
    type(c_ptr), intent(inout) :: CO2Description
    integer(int32), intent(in) :: strlen2

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2

    string1 = pointer2string(CO2FileFull, strlen1)
    string2 = pointer2string(CO2Description, strlen2)
    call GenerateCO2Description(string1, string2)
end subroutine GenerateCO2Description_wrap

subroutine TimeToMaxCanopySF_wrap(CCo, CGC, CCx, L0, L12, L123, LToFlor, &
                                  LFlor, DeterminantCrop, L12SF, RedCGC, &
                                  RedCCx, ClassSF)
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CCx
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LToFlor
    integer(int32), intent(in) :: LFlor
    logical(1), intent(in) :: DeterminantCrop
    integer(int32), intent(inout) :: L12SF
    integer(int8), intent(inout) :: RedCGC
    integer(int8), intent(inout) :: RedCCx
    integer(int8), intent(inout) :: ClassSF
    
    logical :: DeterminantCrop_f
    
    DeterminantCrop_f = DeterminantCrop
    call TimeToMaxCanopySF(CCo, CGC, CCx, L0, L12, L123, LToFlor, &
                                  LFlor, DeterminantCrop_f, L12SF, RedCGC, &
                                  RedCCx, ClassSF)
end subroutine TimeToMaxCanopySF_wrap

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

subroutine LoadClimate_wrap(FullName, strlen1, ClimateDescription, strlen2, & 
                            TempFile, strlen3, EToFile, strlen4, &
                            RainFile, strlen5, CO2File, strlen6)
    !! Wrapper for [[ac_global:LoadClimate]] for foreign languages.
    type(c_ptr), intent(in) :: FullName
    integer(int32), intent(in) :: strlen1
    type(c_ptr), intent(inout) :: ClimateDescription
    integer(int32), intent(in) :: strlen2
    type(c_ptr), intent(inout) :: TempFile
    integer(int32), intent(in) :: strlen3
    type(c_ptr), intent(inout) :: EToFile
    integer(int32), intent(in) :: strlen4
    type(c_ptr), intent(inout) :: RainFile
    integer(int32), intent(in) :: strlen5
    type(c_ptr), intent(inout) :: CO2File
    integer(int32), intent(in) :: strlen6

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2
    character(len=strlen1) :: string3
    character(len=strlen2) :: string4
    character(len=strlen1) :: string5
    character(len=strlen2) :: string6

    string1 = pointer2string(FullName, strlen1)
    string2 = pointer2string(ClimateDescription, strlen2)
    string3 = pointer2string(TempFile, strlen3)
    string4 = pointer2string(EToFile, strlen4)
    string5 = pointer2string(RainFile, strlen5)
    string6 = pointer2string(CO2File, strlen6)
    call LoadClimate(string1, string2, string3, string4, string5, string6)
end subroutine LoadClimate_wrap


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

function GetCO2FileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCO2FileFull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCO2FileFull())
end function GetCO2FileFull_wrap


subroutine SetCO2FileFull_wrap(CO2FileFull, strlen)
    !! Wrapper for [[ac_global:SetCO2FileFull]] for foreign languages.
    type(c_ptr), intent(in) :: CO2Filefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CO2FileFull, strlen)
    call SetCO2Filefull(string)
end subroutine SetCO2FileFull_wrap

function GetCO2Description_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCO2Description]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCO2Description())
end function GetCO2Description_wrap


subroutine SetCO2Description_wrap(CO2Description, strlen)
    !! Wrapper for [[ac_global:SetCO2Description]] for foreign languages.
    type(c_ptr), intent(in) :: CO2Description
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CO2Description, strlen)
    call SetCO2Description(string)
end subroutine SetCO2Description_wrap


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

function GetEToDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetEToDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetEToDescription())
end function GetEToDescription_wrap


subroutine SetEToDescription_wrap(EToDescription, strlen)
    !! Wrapper for [[ac_global:SetEToDescription]] for foreign languages.
    type(c_ptr), intent(in) :: EToDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
    
    string = pointer2string(EToDescription, strlen)
    call SetEToDescription(string)
end subroutine SetEToDescription_wrap


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


function GetIrriFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetIrriFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetIrriFileFull())
end function GetIrriFileFull_wrap


subroutine SetIrriFileFull_wrap(IrriFileFull, strlen)
    !! Wrapper for [[ac_global:SetIrriFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: IrriFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(IrriFileFull, strlen)
    call SetIrriFileFull(string)
end subroutine SetIrriFileFull_wrap


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


function GetClimateFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetClimateFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetClimateFileFull())
end function GetClimateFileFull_wrap


subroutine SetClimateFileFull_wrap(ClimateFileFull, strlen)
    !! Wrapper for [[ac_global:SetClimateFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: ClimateFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ClimateFileFull, strlen)
    call SetClimateFileFull(string)
end subroutine SetClimateFileFull_wrap

function GetClimateDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetClimateDescription]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetClimateDescription())
end function GetClimateDescription_wrap


subroutine SetClimateDescription_wrap(ClimateDescription, strlen)
    !! Wrapper for [[ac_global:SetClimateDescription]] for foreign languages.
    type(c_ptr), intent(in) :: ClimateDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ClimateDescription, strlen)
    call SetClimateDescription(string)
end subroutine SetClimateDescription_wrap

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


function GetSWCiniFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetSWCiniFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetSWCiniFileFull())
end function GetSWCiniFileFull_wrap


subroutine SetSWCiniFileFull_wrap(SWCiniFileFull, strlen)
    !! Wrapper for [[ac_global:SetSWCiniFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: SWCiniFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(SWCiniFileFull, strlen)
    call SetSWCiniFileFull(string)
end subroutine SetSWCiniFileFull_wrap

function GetPathNameProg_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetPathNameProg]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetPathNameProg())
end function GetPathNameProg_wrap


subroutine SetPathNameProg_wrap(PathNameProg, strlen)
    !! Wrapper for [[ac_global:SetPathNameProg]] for foreign languages.
    type(c_ptr), intent(in) :: PathNameProg
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(PathNameProg, strlen)
    call SetPathNameProg(string)
end subroutine SetPathNameProg_wrap

function GetPathNameOutp_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetPathNameOutp]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetPathNameOutp())
end function GetPathNameOutp_wrap


subroutine SetPathNameOutp_wrap(PathNameOutp, strlen)
    !! Wrapper for [[ac_global:SetPathNameOutp]] for foreign languages.
    type(c_ptr), intent(in) :: PathNameOutp
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(PathNameOutp, strlen)
    call SetPathNameOutp(string)
end subroutine SetPathNameOutp_wrap

function GetPathNameSimul_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetPathNameSimul]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetPathNameSimul())
end function GetPathNameSimul_wrap


subroutine SetPathNameSimul_wrap(PathNameSimul, strlen)
    !! Wrapper for [[ac_global:SetPathNameSimul]] for foreign languages.
    type(c_ptr), intent(in) :: PathNameSimul
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(PathNameSimul, strlen)
    call SetPathNameSimul(string)
end subroutine SetPathNameSimul_wrap


function GetProjectFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetProjectFile]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetProjectFile())
end function GetProjectFile_wrap


subroutine SetProjectFile_wrap(ProjectFile, strlen)
    !! Wrapper for [[ac_global:SetProjectFile]] for foreign languages.
    type(c_ptr), intent(in) :: ProjectFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ProjectFile, strlen)
    call SetProjectFile(string)
end subroutine SetProjectFile_wrap


function GetProjectFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetProjectFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetProjectFileFull())
end function GetProjectFileFull_wrap


subroutine SetProjectFileFull_wrap(ProjectFileFull, strlen)
    !! Wrapper for [[ac_global:SetProjectFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: ProjectFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ProjectFileFull, strlen)
    call SetProjectFileFull(string)
end subroutine SetProjectFileFull_wrap


function GetMultipleProjectFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetMultipleProjectFile]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetMultipleProjectFile())
end function GetMultipleProjectFile_wrap


subroutine SetMultipleProjectFile_wrap(MultipleProjectFile, strlen)
    !! Wrapper for [[ac_global:SetMultipleProjectFile]] for foreign languages.
    type(c_ptr), intent(in) :: MultipleProjectFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(MultipleProjectFile, strlen)
    call SetMultipleProjectFile(string)
end subroutine SetMultipleProjectFile_wrap

function GetOnset_GenerateOn_wrap() result(GenerateOn)
    !! Wrapper for [[ac_global:GetOnset_GenerateOn]] for foreign languages.
    logical(1) :: GenerateOn

    GenerateOn = GetOnset_GenerateOn()
end function GetOnset_GenerateOn_wrap

function GetOnset_GenerateTempOn_wrap() result(GenerateTempOn)
    !! Wrapper for [[ac_global:GetOnset_GenerateTempOn]] for foreign languages.
    logical(1) :: GenerateTempOn

    GenerateTempOn = GetOnset_GenerateTempOn()
end function GetOnset_GenerateTempOn_wrap

subroutine SetOnset_GenerateOn_wrap(GenerateOn)
    !! Wrapper for [[ac_global:SetOnset_GenerateOn]] for foreign languages.
    logical(1), intent(in) :: GenerateOn

    logical :: bool

    bool = GenerateOn
    call SetOnset_GenerateOn(bool)
end subroutine SetOnset_GenerateOn_wrap

subroutine SetOnset_GenerateTempOn_wrap(GenerateTempOn)
    !! Wrapper for [[ac_global:SetOnset_GenerateTempOn]] for foreign languages.
    logical(1), intent(in) :: GenerateTempOn

    logical :: bool

    bool = GenerateTempOn
    call SetOnset_GenerateTempOn(bool)
end subroutine SetOnset_GenerateTempOn_wrap

function GetMultipleProjectFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetMultipleProjectFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer
    
    c_pointer = string2pointer(GetMultipleProjectFileFull())
end function GetMultipleProjectFileFull_wrap


subroutine SetMultipleProjectFileFull_wrap(MultipleProjectFileFull, strlen)
    !! Wrapper for [[ac_global:SetMultipleProjectFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: MultipleProjectFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(MultipleProjectFileFull, strlen)
    call SetMultipleProjectFileFull(string)
end subroutine SetMultipleProjectFileFull_wrap


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


function GetRainDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetRainDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetRainDescription())
end function GetRainDescription_wrap


subroutine SetRainDescription_wrap(RainDescription, strlen)
    !! Wrapper for [[ac_global:SetRainDescription]] for foreign languages.
    type(c_ptr), intent(in) :: RainDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string
 
    string = pointer2string(RainDescription, strlen)
    call SetRainDescription(string)
end subroutine SetRainDescription_wrap

    
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

function GetCalendarFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCalendarFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCalendarFileFull())
end function GetCalendarFileFull_wrap

subroutine SetCalendarFileFull_wrap(CalendarFileFull, strlen)
    !! Wrapper for [[ac_global:SetCO2File]] for foreign languages.
    type(c_ptr), intent(in) :: CalendarFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CalendarFileFull, strlen)
    call SetCalendarFileFull(string)
end subroutine SetCalendarFileFull_wrap

function GetCalendarDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCalendarDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCalendarDescription())
end function GetCalendarDescription_wrap

subroutine SetCalendarDescription_wrap(CalendarDescription, strlen)
    !! Wrapper for [[ac_global:SetCO2File]] for foreign languages.
    type(c_ptr), intent(in) :: CalendarDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CalendarDescription, strlen)
    call SetCalendarDescription(string)
end subroutine SetCalendarDescription_wrap

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

function GetCropFileFull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCropFileFull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCropFileFull())
end function GetCropFileFull_wrap

subroutine SetCropFileFull_wrap(CropFileFull, strlen)
    !! Wrapper for [[ac_global:SetCropFileFull]] for foreign languages.
    type(c_ptr), intent(in) :: CropFileFull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CropFileFull, strlen)
    call SetCropFileFull(string)
end subroutine SetCropFileFull_wrap

function GetCropDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetCropDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetCropDescription())
end function GetCropDescription_wrap

subroutine SetCropDescription_wrap(CropDescription, strlen)
    !! Wrapper for [[ac_global:SetCropDescription]] for foreign languages.
    type(c_ptr), intent(in) :: CropDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(CropDescription, strlen)
    call SetCropDescription(string)
end subroutine SetCropDescription_wrap

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

function GetProfDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetProfDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetProfDescription())
end function GetProfDescription_wrap

subroutine SetProfDescription_wrap(ProfDescription, strlen)
    !! Wrapper for [[ac_global:SetProfDescription]] for foreign languages.
    type(c_ptr), intent(in) :: ProfDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ProfDescription, strlen)
    call SetProfDescription(string)
end subroutine SetProfDescription_wrap

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

function GetManagement_Cuttings_Considered_wrap() result(Considered_f)

    logical(1) :: Considered_f

    Considered_f = GetManagement_Cuttings_Considered()
end function GetManagement_Cuttings_Considered_wrap


function GetManagement_Cuttings_Generate_wrap() result(Generate_f)

    logical(1) :: Generate_f

    Generate_f = GetManagement_Cuttings_Generate()
end function GetManagement_Cuttings_Generate_wrap


function GetManagement_Cuttings_HarvestEnd_wrap() result(HarvestEnd_f)

    logical(1) :: HarvestEnd_f

    HarvestEnd_f = GetManagement_Cuttings_HarvestEnd()
end function GetManagement_Cuttings_HarvestEnd_wrap


subroutine SetManagement_Cuttings_Considered_wrap(Considered)
    logical(1), intent(in) :: Considered

    logical :: Considered_f

    Considered_f = Considered
    call SetManagement_Cuttings_Considered(Considered_f)    
end subroutine SetManagement_Cuttings_Considered_wrap


subroutine SetManagement_Cuttings_Generate_wrap(Generate)
    logical(1), intent(in) :: Generate

    logical :: Generate_f

    Generate_f = Generate
    call SetManagement_Cuttings_Generate(Generate_f)    
end subroutine SetManagement_Cuttings_Generate_wrap


subroutine SetManagement_Cuttings_HarvestEnd_wrap(HarvestEnd)
    logical(1), intent(in) :: HarvestEnd

    logical :: HarvestEnd_f

    HarvestEnd_f = HarvestEnd
    call SetManagement_Cuttings_HarvestEnd(HarvestEnd_f)    
end subroutine SetManagement_Cuttings_HarvestEnd_wrap


function GetManagement_RunoffOn_wrap() result(RunoffOn_f)

    logical(1) :: RunoffOn_f

    RunoffOn_f = GetManagement_RunoffOn()
end function GetManagement_RunoffOn_wrap


subroutine SetManagement_RunoffOn_wrap(RunoffOn)
    logical(1), intent(in) :: RunoffOn

    logical :: RunoffOn_f

    RunoffOn_f = RunoffOn
    call SetManagement_RunoffOn(RunoffOn_f)    
end subroutine SetManagement_RunoffOn_wrap

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

function GetObservationsFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetObservationsFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetObservationsFile())
end function GetObservationsFile_wrap

subroutine SetObservationsFile_wrap(ObservationsFile, strlen)
    !! Wrapper for [[ac_global:SetvFile]] for foreign languages.
    type(c_ptr), intent(in) :: ObservationsFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ObservationsFile, strlen)
    call SetObservationsFile(string)
end subroutine SetObservationsFile_wrap


function GetObservationsFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetObservationsFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetObservationsFilefull())
end function GetObservationsFilefull_wrap

subroutine SetObservationsFilefull_wrap(ObservationsFilefull, strlen)
    !! Wrapper for [[ac_global:SetObservationsFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: ObservationsFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ObservationsFilefull, strlen)
    call SetObservationsFilefull(string)
end subroutine SetObservationsFilefull_wrap

function GetObservationsDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetObservationsDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetObservationsDescription())
end function GetObservationsDescription_wrap

subroutine SetObservationsDescription_wrap(ObservationsDescription, strlen)
    !! Wrapper for [[ac_global:SetObservationsDescription]] for foreign languages.
    type(c_ptr), intent(in) :: ObservationsDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(ObservationsDescription, strlen)
    call SetObservationsDescription(string)
end subroutine SetObservationsDescription_wrap


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

function GetTemperatureFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetTemperatureFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTemperatureFile())
end function GetTemperatureFile_wrap


subroutine SetTemperatureFile_wrap(TemperatureFile, strlen)
    !! Wrapper for [[ac_global:TemperatureFile]] for foreign languages.
    type(c_ptr), intent(in) :: TemperatureFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TemperatureFile, strlen)
    call SetTemperatureFile(string)
end subroutine SetTemperatureFile_wrap

function GetTemperatureFilefull_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetTemperatureFilefull]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTemperatureFilefull())
end function GetTemperatureFilefull_wrap


subroutine SetTemperatureFilefull_wrap(TemperatureFilefull, strlen)
    !! Wrapper for [[ac_global:TemperatureFilefull]] for foreign languages.
    type(c_ptr), intent(in) :: TemperatureFilefull
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TemperatureFilefull, strlen)
    call SetTemperatureFilefull(string)
end subroutine SetTemperatureFilefull_wrap


function GetTemperatureDescription_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetTemperatureDescription]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTemperatureDescription())
end function GetTemperatureDescription_wrap


subroutine SetTemperatureDescription_wrap(TemperatureDescription, strlen)
    !! Wrapper for [[ac_global:TemperatureDescription]] for foreign languages.
    type(c_ptr), intent(in) :: TemperatureDescription
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TemperatureDescription, strlen)
    call SetTemperatureDescription(string)
end subroutine SetTemperatureDescription_wrap


subroutine SetTemperatureRecord_ToString_wrap(&
                     TemperatureRecord_ToString, strlen)
    !! Wrapper for [[ac_global:TemperatureRecord_ToString]] for foreign
    !languages.
    type(c_ptr), intent(in) :: TemperatureRecord_ToString
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TemperatureRecord_ToString, strlen)
    call SetTemperatureRecord_ToString(string)
end subroutine SetTemperatureRecord_ToString_wrap

function GetTemperatureRecord_ToString_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetTemperatureRecord_ToString]] for foreign
    !languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTemperatureRecord_ToString())
end function GetTemperatureRecord_ToString_wrap

subroutine SetTemperatureRecord_FromString_wrap(&
                     TemperatureRecord_FromString, strlen)
    !! Wrapper for [[ac_global:TemperatureRecord_FromString]] for foreign
    !languages.
    type(c_ptr), intent(in) :: TemperatureRecord_FromString
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TemperatureRecord_FromString, strlen)
    call SetTemperatureRecord_FromString(string)
end subroutine SetTemperatureRecord_FromString_wrap

function GetTemperatureRecord_FromString_wrap() result(c_pointer)
    !! Wrapper for [[ac_global:GetTemperatureRecord_FromString]] for foreign
    !languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTemperatureRecord_FromString())
end function GetTemperatureRecord_FromString_wrap


end module ac_interface_global
