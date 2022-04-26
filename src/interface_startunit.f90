module ac_interface_startunit

use, intrinsic :: iso_c_binding, only: c_ptr

use ac_interface_global, only: pointer2string, &
                               string2pointer

use ac_kinds, only: int32, &
                    intEnum

use ac_startunit, only: fProjects_open, &
                        fProjects_write, &
                        GetListProjectsFile, &
                        GetProjectFilename, &
                        InitializeProject, &
                        WriteProjectsInfo

implicit none


contains

subroutine fProjects_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fProjects_open(filename, mode)
end subroutine fProjects_open_wrap


subroutine fProjects_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fProjects_write(line, advance_f)
end subroutine fProjects_write_wrap


function GetListProjectsFile_wrap() result(ptr)
    type(c_ptr) :: ptr

    ptr = string2pointer(GetListProjectsFile())
end function GetListProjectsFile_wrap


function GetProjectFilename_wrap(iproject) result(ptr)
    integer(int32), intent(in) :: iproject
    type(c_ptr) :: ptr

    ptr = string2pointer(GetProjectFilename(iproject))
end function GetProjectFilename_wrap


subroutine InitializeProject_wrap(iproject, p, strlen, TheProjectType)
    integer(int32), intent(in) :: iproject
    type(c_ptr), intent(in) :: p
    integer(int32), intent(in) :: strlen
    integer(intEnum), intent(in) :: TheProjectType

    character(len=strlen) :: TheProjectFile

    TheProjectFile = pointer2string(p, strlen)
    call InitializeProject(iproject, TheProjectFile, TheprojectType)
end subroutine InitializeProject_wrap

subroutine WriteProjectsInfo_wrap(p, strlen)
    type(c_ptr), intent(in) :: p
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: line

    line = pointer2string(p, strlen)
    call WriteProjectsInfo(line)
end subroutine WriteProjectsInfo_wrap


end module ac_interface_startunit
