module ac_interface_run

use, intrinsic :: iso_c_binding, only: c_char, &
                                       c_ptr
use ac_interface_global, only: pointer2string, &
                               string2pointer
use ac_kinds, only: int32
use ac_run, only:   fDaily_open, &
                    fDaily_write, &
                    fRun_open, &
                    fRun_write, &
                    fIrri_eof, &
                    fIrri_open, &
                    fIrri_read, &
                    GetCutInfoRecord1_NoMoreInfo, &
                    GetCutInfoRecord2_NoMoreInfo, &
                    GetIrriInfoRecord1_NoMoreInfo, &
                    GetIrriInfoRecord2_NoMoreInfo, &
                    GetTransfer_Mobilize, &
                    GetTransfer_Store, &
                    SetCutInfoRecord1_NoMoreInfo, &
                    SetCutInfoRecord2_NoMoreInfo, &
                    SetIrriInfoRecord1_NoMoreInfo, &
                    SetIrriInfoRecord2_NoMoreInfo, &
                    SetTransfer_Mobilize, &
                    SetTransfer_Store


implicit none


contains

subroutine fDaily_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fDaily_open(filename, mode)
end subroutine fDaily_open_wrap


subroutine fDaily_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fDaily_write(line, advance_f)
end subroutine fDaily_write_wrap

subroutine fRun_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fRun_open(filename, mode)
end subroutine fRun_open_wrap


subroutine fRun_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fRun_write(line, advance_f)
end subroutine fRun_write_wrap


subroutine fIrri_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fIrri_open(filename, mode)
end subroutine fIrri_open_wrap


function fIrri_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fIrri_read()
    line_ptr = string2pointer(line)
end function fIrri_read_wrap


function fIrri_eof_wrap() result(eof)
    logical(1) :: eof

    eof = fIrri_eof()
end function fIrri_eof_wrap


function GetCutInfoRecord1_NoMoreInfo_wrap() result(NoMoreInfo_f)

    logical(1) :: NoMoreInfo_f

    NoMoreInfo_f = GetCutInfoRecord1_NoMoreInfo()
end function GetCutInfoRecord1_NoMoreInfo_wrap

function GetCutInfoRecord2_NoMoreInfo_wrap() result(NoMoreInfo_f)

    logical(1) :: NoMoreInfo_f

    NoMoreInfo_f = GetCutInfoRecord2_NoMoreInfo()
end function GetCutInfoRecord2_NoMoreInfo_wrap

function GetIrriInfoRecord1_NoMoreInfo_wrap() result(NoMoreInfo_f)

    logical(1) :: NoMoreInfo_f

    NoMoreInfo_f = GetIrriInfoRecord1_NoMoreInfo()
end function GetIrriInfoRecord1_NoMoreInfo_wrap

function GetIrriInfoRecord2_NoMoreInfo_wrap() result(NoMoreInfo_f)

    logical(1) :: NoMoreInfo_f

    NoMoreInfo_f = GetIrriInfoRecord2_NoMoreInfo()
end function GetIrriInfoRecord2_NoMoreInfo_wrap

function GetTransfer_Mobilize_wrap() result(Mobilize_f)

    logical(1) :: Mobilize_f

    Mobilize_f = GetTransfer_Mobilize()
end function GetTransfer_Mobilize_wrap

function GetTransfer_Store_wrap() result(Store_f)

    logical(1) :: Store_f

    Store_f = GetTransfer_Store()
end function GetTransfer_Store_wrap

subroutine SetCutInfoRecord1_NoMoreInfo_wrap(NoMoreInfo)
    logical(1), intent(in) :: NoMoreInfo

    logical :: NoMoreInfo_f

    NoMoreInfo_f = NoMoreInfo
    call SetCutInfoRecord1_NoMoreInfo(NoMoreInfo_f)    
end subroutine SetCutInfoRecord1_NoMoreInfo_wrap

subroutine SetCutInfoRecord2_NoMoreInfo_wrap(NoMoreInfo)
    logical(1), intent(in) :: NoMoreInfo

    logical :: NoMoreInfo_f

    NoMoreInfo_f = NoMoreInfo
    call SetCutInfoRecord2_NoMoreInfo(NoMoreInfo_f)    
end subroutine SetCutInfoRecord2_NoMoreInfo_wrap

subroutine SetIrriInfoRecord1_NoMoreInfo_wrap(NoMoreInfo)
    logical(1), intent(in) :: NoMoreInfo

    logical :: NoMoreInfo_f

    NoMoreInfo_f = NoMoreInfo
    call SetIrriInfoRecord1_NoMoreInfo(NoMoreInfo_f)    
end subroutine SetIrriInfoRecord1_NoMoreInfo_wrap

subroutine SetIrriInfoRecord2_NoMoreInfo_wrap(NoMoreInfo)
    logical(1), intent(in) :: NoMoreInfo

    logical :: NoMoreInfo_f

    NoMoreInfo_f = NoMoreInfo
    call SetIrriInfoRecord2_NoMoreInfo(NoMoreInfo_f)    
end subroutine SetIrriInfoRecord2_NoMoreInfo_wrap

subroutine SetTransfer_Mobilize_wrap(Mobilize)
    logical(1), intent(in) :: Mobilize

    logical :: Mobilize_f

    Mobilize_f = Mobilize
    call SetTransfer_Mobilize(Mobilize_f)    
end subroutine SetTransfer_Mobilize_wrap

subroutine SetTransfer_Store_wrap(Store)
    logical(1), intent(in) :: Store

    logical :: Store_f

    Store_f = Store
    call SetTransfer_Store(Store_f)    
end subroutine SetTransfer_Store_wrap


end module ac_interface_run
