module aquacrop_wrap

use ac_kinds, only: int8, &
                    int32, &
                    intEnum
use ac_interface_global, only: pointer2string, &
                               string2pointer
use, intrinsic :: iso_c_binding, only: c_ptr
implicit none


interface
    ! Interface to the procedures exported in the external AquaCrop library

    subroutine InitializeTheProgram()
    end subroutine InitializeTheProgram

    subroutine FinalizeTheProgram()
    end subroutine FinalizeTheProgram

    subroutine WriteProjectsInfo_wrap(line)
        import c_ptr
        type(c_ptr), intent(in) :: line
    end subroutine WriteProjectsInfo_wrap

    subroutine GetListProjectsFile_wrap(ListProjectsFile, length)
        import c_ptr, int32
        type(c_ptr), intent(out) :: ListProjectsFile
        integer(int32), intent(out) :: length
    end subroutine GetListProjectsFile_wrap

    function GetNumberOfProjects() result(nprojects)
        import int32
        integer(int32) :: nprojects
    end function GetNumberOfProjects

    subroutine GetProjectFileName_wrap(iproject, TheProjectFile, length)
        import c_ptr, int32
        integer(int32), intent(in) :: iproject
        type(c_ptr), intent(out) :: TheProjectFile
        integer(int32), intent(out) :: length
    end subroutine GetProjectFileName_wrap

    subroutine GetProjectType_wrap(TheProjectFile, TheProjectType)
        import c_ptr, intEnum
        type(c_ptr), intent(in) :: TheProjectFile
        integer(intEnum), intent(out) :: TheProjectType
    end subroutine GetProjectType_wrap

    subroutine InitializeProject_wrap(iproject, TheProjectFile, TheProjectType)
        import c_ptr, int32, IntEnum
        integer(int32), intent(in) :: iproject
        type(c_ptr), intent(in) :: TheProjectFile
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine InitializeProject_wrap

    function GetDayNri() result(DayNri)
        integer :: DayNri
    end function GetDayNri

    subroutine InitializeSimulation_wrap(TheProjectFile, TheProjectType)
        import c_ptr, intEnum
        type(c_ptr), intent(in) :: TheProjectFile
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine InitializeSimulation_wrap

    subroutine InitializeRun_wrap(NrRun, TheProjectType)
        import int8, intEnum
        integer(int8), intent(in) :: NrRun
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine InitializeRun_wrap

    subroutine AdvanceOneTimeStep()
    end subroutine AdvanceOneTimeStep

    subroutine FinalizeRun1_wrap(NrRun, TheProjectFile, TheProjectType)
        import c_ptr, int8, intEnum
        integer(int8), intent(in) :: NrRun
        type(c_ptr), intent(in) :: TheProjectFile
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine FinalizeRun1_wrap

    subroutine FinalizeRun2_wrap(NrRun, TheProjectType)
        import int8, intEnum
        integer(int8), intent(in) :: NrRun
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine FinalizeRun2_wrap

    subroutine FinalizeSimulation()
    end subroutine FinalizeSimulation

    function GetSimulation_NrRuns() result(NrRuns)
        integer :: NrRuns
    end function GetSimulation_NrRuns

    function GetSimulation_ToDayNr() result(ToDayNr)
        integer :: ToDayNr
    end function GetSimulation_ToDayNr
end interface


contains


function GetListProjectsFile() result(str)
    character(len=:), allocatable :: str

    type(c_ptr) :: c_pointer
    integer(int32) :: length

    call GetListProjectsFile_wrap(c_pointer, length)
    str = pointer2string(c_pointer, length)
end function GetListProjectsFile


subroutine WriteProjectsInfo(line)
    character(len=*), intent(in) :: line

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(line)
    call WriteProjectsInfo_wrap(c_pointer)
end subroutine WriteProjectsInfo


function GetProjectFileName(iproject) result(str)
    integer, intent(in) :: iproject
    character(len=:), allocatable :: str

    type(c_ptr) :: c_pointer
    integer(int32) :: length

    call GetProjectFileName_wrap(iproject, c_pointer, length)
    str = pointer2string(c_pointer, length)
end function GetProjectFileName


subroutine GetProjectType(TheProjectFile, TheProjectType)
    character(len=*), intent(in) :: TheProjectFile
    integer(intEnum), intent(out) :: TheProjectType

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(TheProjectFile)
    call GetProjectType_wrap(c_pointer, TheProjectType)
end subroutine GetProjectType


subroutine InitializeProject(iproject, TheProjectFile, TheProjectType)
    integer, intent(in) :: iproject
    character(len=*), intent(in) :: TheProjectFile
    integer(intEnum), intent(in) :: TheProjectType

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(TheProjectFile)
    call InitializeProject_wrap(iproject, c_pointer, TheProjectType)
end subroutine InitializeProject


subroutine InitializeSimulation(TheProjectFile, TheProjectType)
    character(len=*), intent(in) :: TheProjectFile
    integer(intEnum), intent(in) :: TheProjectType

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(TheProjectFile)
    call InitializeSimulation_wrap(c_pointer, TheProjectType)
end subroutine InitializeSimulation


subroutine InitializeRun(NrRun, TheProjectType)
    integer, intent(in) :: NrRun
    integer(intEnum), intent(in) :: TheProjectType

    call InitializeRun_wrap(int(NrRun, kind=int8), TheProjectType)
end subroutine InitializeRun


subroutine FinalizeRun1(NrRun, TheProjectFile, TheProjectType)
    integer, intent(in) :: NrRun
    character(len=*), intent(in) :: TheProjectFile
    integer(intEnum), intent(in) :: TheProjectType

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(TheProjectFile)
    call FinalizeRun1_wrap(int(NrRun, kind=int8), c_pointer, TheProjectType)
end subroutine FinalizeRun1


subroutine FinalizeRun2(NrRun, TheProjectType)
    integer, intent(in) :: NrRun
    integer(intEnum), intent(in) :: TheProjectType

    call FinalizeRun2_wrap(int(NrRun, kind=int8), TheProjectType)
end subroutine FinalizeRun2

end module aquacrop_wrap
