program pluginbarev70

use ac_global, only: int32, &
                     intEnum
use ac_interface_global, only: pointer2string, &
                               string2pointer
use, intrinsic :: iso_c_binding, only: c_ptr
implicit none


interface
    subroutine StartTheProgram()
    end subroutine StartTheProgram

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

    subroutine RunSimulation_wrap(TheProjectFile, TheProjectType)
        import c_ptr, intEnum
        type(c_ptr), intent(in) :: TheProjectFile
        integer(intEnum), intent(in) :: TheProjectType
    end subroutine RunSimulation_wrap
end interface


integer :: iproject, nprojects
character(len=:), allocatable :: ListProjectsFile, TheProjectFile
logical :: ListProjectFileExist
integer(intEnum) :: TheProjectType

!to run AquaCrop in "standalone" mode:
!call StartTheProgram()

call InitializeTheProgram()

ListProjectsFile = GetListProjectsFile()
inquire(file=trim(ListProjectsFile), exist=ListProjectFileExist)
nprojects = GetNumberOfProjects()

if (nprojects > 0) then
    call WriteProjectsInfo('')
    call WriteProjectsInfo('Projects handled:')
end if

do iproject = 1, nprojects
    TheProjectFile = GetProjectFileName(iproject)
    call GetProjectType(TheProjectFile, TheProjectType)
    call InitializeProject(iproject, TheProjectFile, TheProjectType)
    call RunSimulation(TheProjectFile, TheProjectType)
end do

if (nprojects == 0) then
    call WriteProjectsInfo('')
    call WriteProjectsInfo('Projects loaded: None')

    if (ListProjectFileExist) then
        call WriteProjectsInfo('File ''ListProjects.txt'' does not contain ANY project file')
    else
        call WriteProjectsInfo('Missing File ''ListProjects.txt'' in LIST directory')
    end if
end if

call FinalizeTheProgram()


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


subroutine RunSimulation(TheProjectFile, TheProjectType)
    character(len=*), intent(in) :: TheProjectFile
    integer(intEnum), intent(in) :: TheProjectType

    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(TheProjectFile)
    call RunSimulation_wrap(c_pointer, TheProjectType)
end subroutine RunSimulation

end program pluginbarev70
