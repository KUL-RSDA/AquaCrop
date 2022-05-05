module ac_interface_run

use, intrinsic :: iso_c_binding, only: c_char, &
                                       c_ptr

use ac_interface_global, only: pointer2string, &
                               string2pointer
                               
use ac_kinds, only: int8, &
                    int32, &
                    dp, &
                    intenum

use ac_run, only:   CheckForPrint, &
                    fDaily_open, &
                    fDaily_write, &
                    fRun_open, &
                    fRun_write, &
                    FinalizeRun1, &
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
                    fEval_open, &
                    fEval_write, &
                    GetCutInfoRecord1_NoMoreInfo, &
                    GetCutInfoRecord2_NoMoreInfo, &
                    GetCutInfoRecord1_FromDay, &
                    GetfEval_filename, &
                    fHarvest_open, &
                    fHarvest_write, &
                    GetfHarvest_filename, &
                    GetGlobalIrriECw, &
                    GetIrriInfoRecord1_NoMoreInfo, &
                    GetIrriInfoRecord2_NoMoreInfo, &
                    GetTheProjectFile, &
                    GetNoYear, &
                    GetTransfer_Mobilize, &
                    GetTransfer_Store, &
                    InitializeSimulation, &
                    SetCutInfoRecord1_NoMoreInfo, &
                    SetCutInfoRecord2_NoMoreInfo, &
                    SetCutInfoRecord1_FromDay, &
                    SetfEval_filename, &
                    SetfHarvest_filename, &
                    SetGlobalIrriECw, &
                    SetIrriInfoRecord1_NoMoreInfo, &
                    SetIrriInfoRecord2_NoMoreInfo, &
                    SetTheProjectFile, &
                    SetNoYear, &
                    SetTransfer_Mobilize, &
                    SetTransfer_Store, &
                    WriteTheResults, &
                    GetWaterTableInProfile, &
                    SetWaterTableInProfile, &
                    GetStartMode, &
                    SetStartMode, &
                    GetNoMoreCrop, &
                    SetNoMoreCrop, &
                    GetCGCadjustmentAfterCutting, &
                    SetCGCadjustmentAfterCutting, &
                    WriteSimPeriod, &
                    WriteIntermediatePeriod, &
                    InitializeTransferAssimilates, &
                    RunSimulation

implicit none


contains

subroutine CheckForPrint_wrap(TheProjectFile, strlen)
    !! Wrapper for [[ac_run:CheckForPrint]] for foreign languages.
    type(c_ptr), intent(in) :: TheProjectFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TheProjectFile, strlen)
    call CheckForPrint(string)
end subroutine CheckForPrint_wrap

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


function GetfHarvest_filename_wrap() result(c_pointer)
    !! Wrapper for [[ac_run:GetfHarvest_filename_wrap]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetfHarvest_filename())
end function GetfHarvest_filename_wrap


subroutine SetfHarvest_filename_wrap(fHarvest_filename, strlen)
    !! Wrapper for [[ac_run:SetfHarvest_filename_wrap]] for foreign languages.
    type(c_ptr), intent(in) :: fHarvest_filename
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(fHarvest_filename, strlen)
    call SetfHarvest_filename(string)
end subroutine SetfHarvest_filename_wrap


subroutine fHarvest_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fHarvest_open(filename, mode)
end subroutine fHarvest_open_wrap


subroutine fHarvest_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fHarvest_write(line, advance_f)
end subroutine fHarvest_write_wrap


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


function GetfEval_filename_wrap() result(filename_ptr)
    type(c_ptr) :: filename_ptr

    character(len=:), allocatable :: filename

    filename = GetfEval_filename()
    filename_ptr = string2pointer(filename)
end function GetfEval_filename_wrap
    

subroutine SetfEval_filename_wrap(filename_ptr, strlen)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: strlen

    character(len=:), allocatable :: filename

    filename = pointer2string(filename_ptr, strlen)
    call SetfEval_filename(filename)
end subroutine SetfEval_filename_wrap


subroutine fEval_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fEval_open(filename, mode)
end subroutine fEval_open_wrap

subroutine InitializeSimulation_wrap(TheProjectFileStr,strlen, TheProjectType)
    type(c_ptr), intent(in) :: TheProjectFileStr
    integer(int32), intent(in) :: strlen
    integer(intenum), intent(in) :: TheProjectType

    character(len=strlen) :: string

    string = pointer2string(TheProjectFileStr, strlen)
    call InitializeSimulation(string, TheProjectType)
end subroutine InitializeSimulation_wrap


subroutine fEval_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fEval_write(line, advance_f)
end subroutine fEval_write_wrap


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




subroutine WriteTheResults_wrap(ANumber, Day1, Month1, Year1, DayN, MonthN, &
                           YearN, RPer, EToPer, GDDPer, IrriPer, InfiltPer, &
                           ROPer, DrainPer, CRwPer, EPer, ExPer, TrPer, TrWPer, &
                           TrxPer, SalInPer, SalOutPer, &
                           SalCRPer, BiomassPer, BUnlimPer, BmobPer, BstoPer, &
                           TheProjectFile_ptr, strlen)
    integer(int8), intent(in) :: ANumber
    integer(int32), intent(in) :: Day1
    integer(int32), intent(in) :: Month1
    integer(int32), intent(in) :: Year1
    integer(int32), intent(in) :: DayN
    integer(int32), intent(in) :: MonthN
    integer(int32), intent(in) :: YearN
    real(dp), intent(in) :: RPer
    real(dp), intent(in) :: EToPer
    real(dp), intent(in) :: GDDPer
    real(dp), intent(in) :: IrriPer
    real(dp), intent(in) :: InfiltPer
    real(dp), intent(in) :: ROPer
    real(dp), intent(in) :: DrainPer
    real(dp), intent(in) :: CRwPer
    real(dp), intent(in) :: EPer
    real(dp), intent(in) :: ExPer
    real(dp), intent(in) :: TrPer
    real(dp), intent(in) :: TrWPer
    real(dp), intent(in) :: TrxPer
    real(dp), intent(in) :: SalInPer
    real(dp), intent(in) :: SalOutPer
    real(dp), intent(in) :: SalCRPer
    real(dp), intent(in) :: BiomassPer
    real(dp), intent(in) :: BUnlimPer
    real(dp), intent(in) :: BmobPer
    real(dp), intent(in) :: BstoPer
    type(c_ptr), intent(in) :: TheProjectFile_ptr
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: TheProjectFile

    TheProjectFile = pointer2string(TheProjectFile_ptr, strlen)
    call WriteTheResults (ANumber, Day1, Month1, Year1, DayN, MonthN, &
                           YearN, RPer, EToPer, GDDPer, IrriPer, InfiltPer, &
                           ROPer, DrainPer, CRwPer, EPer, ExPer, TrPer, TrWPer, &
                           TrxPer, SalInPer, SalOutPer, &
                           SalCRPer, BiomassPer, BUnlimPer, BmobPer, BstoPer, &
                           TheProjectFile)
end subroutine WriteTheResults_wrap



function GetStartMode_wrap() result(StartMode_f)

    logical(1) :: StartMode_f

    StartMode_f = GetStartMode()
end function GetStartMode_wrap


subroutine SetStartMode_wrap(StartMode_in)
    logical(1), intent(in) :: StartMode_in

    logical :: StartMode_f

    StartMode_f = StartMode_in
    call SetStartMode(StartMode_f)
end subroutine SetStartMode_wrap


function GetWaterTableInProfile_wrap() result(WaterTableInProfile_f)

    logical(1) :: WaterTableInProfile_f

    WaterTableInProfile_f = GetWaterTableInProfile()
end function GetWaterTableInProfile_wrap


subroutine SetWaterTableInProfile_wrap(WaterTableInProfile_in)
    logical(1), intent(in) :: WaterTableInProfile_in

    logical :: WaterTableInProfile_f

    WaterTableInProfile_f = WaterTableInProfile_in
    call SetWaterTableInProfile(WaterTableInProfile_f)
end subroutine SetWaterTableInProfile_wrap

function GetNoMoreCrop_wrap() result(NoMoreCrop_f)

    logical(1) :: NoMoreCrop_f

    NoMoreCrop_f = GetNoMoreCrop()
end function GetNoMoreCrop_wrap


subroutine SetNoMoreCrop_wrap(NoMoreCrop_in)
    logical(1), intent(in) :: NoMoreCrop_in

    logical :: NoMoreCrop_f

    NoMoreCrop_f = NoMoreCrop_in
    call SetNoMoreCrop(NoMoreCrop_f)
end subroutine SetNoMoreCrop_wrap

function GetCGCadjustmentAfterCutting_wrap() result(CGCadjustmentAfterCutting_f)

    logical(1) :: CGCadjustmentAfterCutting_f

    CGCadjustmentAfterCutting_f = GetCGCadjustmentAfterCutting()
end function GetCGCadjustmentAfterCutting_wrap


subroutine SetCGCadjustmentAfterCutting_wrap(CGCadjustmentAfterCutting_in)
    logical(1), intent(in) :: CGCadjustmentAfterCutting_in

    logical :: CGCadjustmentAfterCutting_f

    CGCadjustmentAfterCutting_f = CGCadjustmentAfterCutting_in
    call SetCGCadjustmentAfterCutting(CGCadjustmentAfterCutting_f)
end subroutine SetCGCadjustmentAfterCutting_wrap

function GetTheProjectFile_wrap() result(c_pointer)
    !! Wrapper for [[ac_run:GetTheProjectFile]] for foreign languages.
    type(c_ptr) :: c_pointer

    c_pointer = string2pointer(GetTheProjectFile())
end function GetTheProjectFile_wrap


subroutine SetTheProjectFile_wrap(TheProjectFile, strlen)
    !! Wrapper for [[ac_global:SetTheProjectFile]] for foreign languages.
    type(c_ptr), intent(in) :: TheProjectFile
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: string

    string = pointer2string(TheProjectFile, strlen)
    call SetTheProjectFile(string)
end subroutine SetTheProjectFile_wrap



function GetNoYear_wrap() result(NoYear_f)

    logical(1) :: NoYear_f

    NoYear_f = GetNoYear()
end function GetNoYear_wrap


subroutine SetNoYear_wrap(NoYear_in)
    logical(1), intent(in) :: NoYear_in

    logical :: NoYear_f

    NoYear_f = NoYear_in
    call SetNoYear(NoYear_f)    
end subroutine SetNoYear_wrap


subroutine WriteSimPeriod_wrap(NrRun, TheProjectFile_ptr, strlen)
    integer(int8), intent(in) :: NrRun
    type(c_ptr), intent(in) :: TheProjectFile_ptr
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: TheProjectFile

    TheProjectFile = pointer2string(TheProjectFile_ptr, strlen)
    call WriteSimPeriod(NrRun, TheProjectFile)
end subroutine WriteSimPeriod_wrap



subroutine WriteIntermediatePeriod_wrap(TheProjectFile_ptr, strlen)
    type(c_ptr), intent(in) :: TheProjectFile_ptr
    integer(int32), intent(in) :: strlen

    character(len=strlen) :: TheProjectFile

    TheProjectFile = pointer2string(TheProjectFile_ptr, strlen)
    call WriteIntermediatePeriod(TheProjectFile)
end subroutine WriteIntermediatePeriod_wrap


subroutine InitializeTransferAssimilates_wrap(Bin, Bout, AssimToMobilize, &
                                         AssimMobilized, FracAssim, &
                                         StorageON, MobilizationON)
    real(dp), intent(inout) :: Bin
    real(dp), intent(inout) :: Bout
    real(dp), intent(inout) :: AssimToMobilize
    real(dp), intent(inout) :: AssimMobilized
    real(dp), intent(inout) :: FracAssim
    logical(1), intent(inout) :: StorageON
    logical(1), intent(inout) :: MobilizationON

    logical :: StorageON_f, MobilizationON_f

    StorageON_f = StorageON
    MobilizationON_f = MobilizationON
    call InitializeTransferAssimilates(Bin, Bout, AssimToMobilize, &
                                         AssimMobilized, FracAssim, &
                                         StorageON_f, MobilizationON_f)
    StorageON = StorageON_f
    MobilizationON = MobilizationON_f
end subroutine InitializeTransferAssimilates_wrap

subroutine FinalizeRun1_wrap(NrRun,TheProjectFile,strlen, TheProjectType)
    integer(int8), intent(in) :: NrRun
    type(c_ptr), intent(in) :: TheProjectFile
    integer(int32), intent(in) :: strlen
    integer(intenum), intent(in) :: TheProjectType

    character(len=strlen) :: string

    string = pointer2string(TheProjectFile, strlen)
    call FinalizeRun1(NrRun,string, TheProjectType)
end subroutine FinalizeRun1_wrap


subroutine RunSimulation_wrap(p, strlen, TheProjectType)
    type(c_ptr), intent(in) :: p
    integer(int32), intent(in) :: strlen
    integer(intEnum), intent(in) :: TheProjectType

    character(len=strlen) :: string

    string = pointer2string(p, strlen)
    call RunSimulation(string, TheProjectType)
end subroutine RunSimulation_wrap



end module ac_interface_run
