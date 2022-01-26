module ac_interface_run

use ac_run, only:   GetCutInfoRecord1_NoMoreInfo, &
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
