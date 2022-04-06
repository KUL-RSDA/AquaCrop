module ac_interface_run

use, intrinsic :: iso_c_binding, only: c_char, &
                                       c_ptr
use ac_interface_global, only: pointer2string, &
                               string2pointer
use ac_kinds, only: int32
use ac_run, only:   fRun_open, &
                    fRun_write, &
                    fIrri_eof, &
                    fIrri_open, &
                    fIrri_read, &
                    fEToSIM_open, &
                    fEToSIM_read, &
                    fRainSIM_open, &
                    fRainSIM_read, &
                    fTempSIM_open, &
                    fTempSIM_read, &
                    fCuts_eof, &
                    fCuts_open, &
                    fCuts_read, &
                    fObs_eof, &
                    fObs_open, &
                    fObs_read, &
                    GetCutInfoRecord1_NoMoreInfo, &
                    GetCutInfoRecord2_NoMoreInfo, &
                    GetGlobalIrriECw, &
                    GetIrriInfoRecord1_NoMoreInfo, &
                    GetIrriInfoRecord2_NoMoreInfo, &
                    GetTransfer_Mobilize, &
                    GetTransfer_Store, &
                    SetCutInfoRecord1_NoMoreInfo, &
                    SetCutInfoRecord2_NoMoreInfo, &
                    SetGlobalIrriECw, &
                    SetIrriInfoRecord1_NoMoreInfo, &
                    SetIrriInfoRecord2_NoMoreInfo, &
                    SetTransfer_Mobilize, &
                    SetTransfer_Store


implicit none


contains


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


subroutine fEToSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fEToSIM_open(filename, mode)
end subroutine fEToSIM_open_wrap


subroutine fTempSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fTempSIM_open(filename, mode)
end subroutine fTempSIM_open_wrap



function fEToSIM_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fEToSIM_read()
    line_ptr = string2pointer(line)
end function fEToSIM_read_wrap


function fTempSIM_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fTempSIM_read()
    line_ptr = string2pointer(line)
end function fTempSIM_read_wrap


subroutine fRainSIM_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fRainSIM_open(filename, mode)
end subroutine fRainSIM_open_wrap


subroutine fCuts_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fCuts_open(filename, mode)
end subroutine fCuts_open_wrap


function fRainSIM_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fRainSIM_read()
    line_ptr = string2pointer(line)
end function fRainSIM_read_wrap


function fCuts_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fCuts_read()
    line_ptr = string2pointer(line)
end function fCuts_read_wrap


function fCuts_eof_wrap() result(eof)
    logical(1) :: eof

    eof = fCuts_eof()
end function fCuts_eof_wrap

subroutine fObs_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fObs_open(filename, mode)
end subroutine fObs_open_wrap


function fObs_read_wrap() result(line_ptr)
    type(c_ptr) :: line_ptr

    character(len=:), allocatable :: line

    line = fObs_read()
    line_ptr = string2pointer(line)
end function fObs_read_wrap


function fObs_eof_wrap() result(eof)
    logical(1) :: eof

    eof = fObs_eof()
end function fObs_eof_wrap


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


function GetGlobalIrriECw_wrap() result(GlobalIrriECw_f)

    logical(1) :: GlobalIrriECw_f

    GlobalIrriECw_f = GetGlobalIrriECw()
end function GetGlobalIrriECw_wrap


subroutine SetGlobalIrriECw_wrap(GlobalIrriECw_in)
    logical(1), intent(in) :: GlobalIrriECw_in

    logical :: GlobalIrriECw_f

    GlobalIrriECw_f = GlobalIrriECw_in
    call SetGlobalIrriECw(GlobalIrriECw_f)    
end subroutine SetGlobalIrriECw_wrap


end module ac_interface_run
