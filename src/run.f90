module ac_run

use ac_kinds, only: dp, &
                    int32
                    
implicit none

type rep_GwTable 
    integer(int32) :: DNr1, DNr2
        !! Undocumented
    integer(int32) :: Z1, Z2
        !! cm
    real(dp) :: EC1, EC2
        !! dS/m
end type rep_GwTable 


type rep_plotPar 
    real(dp) :: PotVal, ActVal
        !! Undocumented
end type rep_plotPar 


type repIrriInfoRecord 
    logical :: NoMoreInfo
        !! Undocumented
    integer(int32) :: FromDay
        !! Undocumented
    integer(int32) :: ToDay
        !! Undocumented
    integer(int32) :: TimeInfo
        !! Undocumented
    integer(int32) :: DepthInfo
        !! Undocumented
end type repIrriInfoRecord 


type rep_StressTot 
    real(dp) :: Salt
        !! Undocumented
    real(dp) :: Temp
        !! Undocumented
    real(dp) :: Exp
        !! Undocumented
    real(dp) :: Sto
        !! Undocumented
    real(dp) :: Weed
        !! Undocumented
    integer(int32) :: NrD
        !! Undocumented
end type rep_StressTot 


type repCutInfoRecord 
    logical :: NoMoreInfo
        !! Undocumented
    integer(int32) :: FromDay
        !! Undocumented
    integer(int32) :: ToDay
        !! Undocumented
    integer(int32) :: IntervalInfo
        !! Undocumented
    real(dp) :: IntervalGDD
        !! Undocumented
    real(dp) :: MassInfo
        !! Undocumented
end type repCutInfoRecord 


type rep_Transfer 
    logical :: Store
        !! transfer of assimilates from above ground parts to root system is active
    logical :: Mobilize
        !! transfer of assimialtes from root system to above ground parts is active
    real(dp) :: ToMobilize
        !! Total mass of assimilates (ton/ha) to mobilize at start of the season
    real(dp) :: Bmobilized
        !! Cumulative sum of assimilates (ton/ha) mobilized form root system
end type rep_Transfer 

type(rep_GwTable) :: GwTable
type(rep_plotPar) :: PlotVarCrop
type(repIrriInfoRecord) :: IrriInfoRecord1, IrriInfoRecord2
type(rep_StressTot) :: StressTot
type(repCutInfoRecord) :: CutInfoRecord1, CutInfoRecord2
type(rep_Transfer) :: Transfer

contains



!! Section for Getters and Setters for global variables

! GwTable

type(rep_GwTable) GetGwTable()
    !! Getter for the "GwTable" global variable.
    
    GetGwTable = GwTable
end function GetGwTable

subroutine SetGwTable_DNr1(DNr1)
    !! Setter for the "GwTable" global variable. 
    integer(int32), intent(in) :: DNr1

    GwTable%DNr1 = DNr1
end subroutine SetGwTable_DNr1

subroutine SetGwTable_DNr2(DNr2)
    !! Setter for the "GwTable" global variable. 
    integer(int32), intent(in) :: DNr2

    GwTable%DNr2 = DNr2
end subroutine SetGwTable_DNr2

subroutine SetGwTable_Z1(Z1)
    !! Setter for the "GwTable" global variable. 
    integer(int32), intent(in) :: Z1

    GwTable%Z1 = Z1
end subroutine SetGwTable_Z1

subroutine SetGwTable_Z2(Z2)
    !! Setter for the "GwTable" global variable. 
    integer(int32), intent(in) :: Z2

    GwTable%Z2 = Z2
end subroutine SetGwTable_Z2

subroutine SetGwTable_EC1(EC1)
    !! Setter for the "GwTable" global variable. 
    real(dp), intent(in) :: EC1

    GwTable%EC1 = EC1
end subroutine SetGwTable_EC1

subroutine SetGwTable_EC2(EC2)
    !! Setter for the "GwTable" global variable. 
    real(dp), intent(in) :: EC2

    GwTable%EC2 = EC2
end subroutine SetGwTable_EC2

! PlotVarCrop

type(rep_plotPar) GetPlotVarCrop()
    !! Getter for the "PlotVarCrop" global variable.
    
    GetPlotVarCrop = PlotVarCrop
end function GetPlotVarCrop

subroutine SetPlotVarCrop_PotVal(PotVal)
    !! Setter for the "PlotVarCrop" global variable. 
    real(dp), intent(in) :: PotVal

    PlotVarCrop%PotVal = PotVal
end subroutine SetPlotVarCrop_PotVal

subroutine SetPlotVarCrop_ActVal(ActVal)
    !! Setter for the "PlotVarCrop" global variable. 
    real(dp), intent(in) :: ActVal

    PlotVarCrop%ActVal = ActVal
end subroutine SetPlotVarCrop_ActVal

! IrriInfoRecord1

logical GetIrriInfoRecord1_NoMoreInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_NoMoreInfo = IrriInfoRecord1%NoMoreInfo
end function GetIrriInfoRecord1_NoMoreInfo

integer(int32) GetIrriInfoRecord1_FromDay()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_FromDay = IrriInfoRecord1%FromDay
end function GetIrriInfoRecord1_FromDay

integer(int32) GetIrriInfoRecord1_ToDay()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_ToDay = IrriInfoRecord1%ToDay
end function GetIrriInfoRecord1_ToDay

integer(int32) GetIrriInfoRecord1_TimeInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_TimeInfo = IrriInfoRecord1%TimeInfo
end function GetIrriInfoRecord1_TimeInfo

integer(int32) GetIrriInfoRecord1_DepthInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_DepthInfo = IrriInfoRecord1%DepthInfo
end function GetIrriInfoRecord1_DepthInfo

SetIrriInfoRecord1_NoMoreInfo(NoMoreInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    logical, intent(in) :: NoMoreInfo
    
    IrriInfoRecord1%NoMoreInfo = NoMoreInfo
end function SetIrriInfoRecord1_NoMoreInfo

SetIrriInfoRecord1_FromDay(FromDay)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: FromDay
    
    IrriInfoRecord1%FromDay = GetIrriInfoRecord1_FromDay
end function SetIrriInfoRecord1_FromDay

SetIrriInfoRecord1_ToDay(ToDay)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: ToDay
    
    IrriInfoRecord1%ToDay = GetIrriInfoRecord1_ToDay
end function SetIrriInfoRecord1_ToDay

SetIrriInfoRecord1_TimeInfo(TimeInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: TimeInfo
    
    IrriInfoRecord1%TimeInfo = GetIrriInfoRecord1_TimeInfo
end function SetIrriInfoRecord1_TimeInfo

SetIrriInfoRecord1_DepthInfo(DepthInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: DepthInfo
    
    IrriInfoRecord1%DepthInfo = GetIrriInfoRecord1_DepthInfo
end function SetIrriInfoRecord1_DepthInfo

! IrriInfoRecord2

logical GetIrriInfoRecord2_NoMoreInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_NoMoreInfo = IrriInfoRecord2%NoMoreInfo
end function GetIrriInfoRecord2_NoMoreInfo

integer(int32) GetIrriInfoRecord2_FromDay()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_FromDay = IrriInfoRecord2%FromDay
end function GetIrriInfoRecord2_FromDay

integer(int32) GetIrriInfoRecord2_ToDay()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_ToDay = IrriInfoRecord2%ToDay
end function GetIrriInfoRecord2_ToDay

integer(int32) GetIrriInfoRecord2_TimeInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_TimeInfo = IrriInfoRecord2%TimeInfo
end function GetIrriInfoRecord2_TimeInfo

integer(int32) GetIrriInfoRecord2_DepthInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_DepthInfo = IrriInfoRecord2%DepthInfo
end function GetIrriInfoRecord2_DepthInfo

SetIrriInfoRecord2_NoMoreInfo(NoMoreInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    logical, intent(in) :: NoMoreInfo
    
    IrriInfoRecord2%NoMoreInfo = NoMoreInfo
end function SetIrriInfoRecord2_NoMoreInfo

SetIrriInfoRecord2_FromDay(FromDay)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: FromDay
    
    IrriInfoRecord2%FromDay = GetIrriInfoRecord2_FromDay
end function SetIrriInfoRecord2_FromDay

SetIrriInfoRecord2_ToDay(ToDay)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: ToDay
    
    IrriInfoRecord2%ToDay = GetIrriInfoRecord2_ToDay
end function SetIrriInfoRecord2_ToDay

SetIrriInfoRecord2_TimeInfo(TimeInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: TimeInfo
    
    IrriInfoRecord2%TimeInfo = GetIrriInfoRecord2_TimeInfo
end function SetIrriInfoRecord2_TimeInfo

SetIrriInfoRecord2_DepthInfo(DepthInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: DepthInfo
    
    IrriInfoRecord2%DepthInfo = GetIrriInfoRecord2_DepthInfo
end function SetIrriInfoRecord2_DepthInfo

! StressTot

type(rep_StressTot) GetStressTot()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot = StressTot
end function GetStressTot

subroutine SetStressTot_Salt(Salt)
    !! Setter for the "StressTot" global variable. 
    real(dp), intent(in) :: Salt

    StressTot%Salt = Salt
end subroutine SetStressTot_Salt

subroutine SetStressTot_Temp(Temp)
    !! Setter for the "StressTot" global variable. 
    real(dp), intent(in) :: Temp

    StressTot%Temp = Temp
end subroutine SetStressTot_Temp

subroutine SetStressTot_Exp(Exp)
    !! Setter for the "StressTot" global variable. 
    real(dp), intent(in) :: Exp

    StressTot%Exp = Exp
end subroutine SetStressTot_Exp

subroutine SetStressTot_Sto(Sto)
    !! Setter for the "StressTot" global variable. 
    real(dp), intent(in) :: Sto

    StressTot%Sto = Sto
end subroutine SetStressTot_Sto

subroutine SetStressTot_Weed(Weed)
    !! Setter for the "StressTot" global variable. 
    real(dp), intent(in) :: Weed

    StressTot%Weed = Weed
end subroutine SetStressTot_Weed

subroutine SetStressTot_NrD(NrD)
    !! Setter for the "StressTot" global variable. 
    integer(int32), intent(in) :: NrD

    StressTot%NrD = NrD
end subroutine SetStressTot_NrD

! CutInfoRecord1

logical GetCutInfoRecord1_NoMoreInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_NoMoreInfo = CutInfoRecord1%NoMoreInfo
end function GetCutInfoRecord1_NoMoreInfo

integer(int32) GetCutInfoRecord1_FromDay()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_FromDay = CutInfoRecord1%FromDay
end function GetCutInfoRecord1_FromDay

integer(int32) GetCutInfoRecord1_ToDay()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_ToDay = CutInfoRecord1%ToDay
end function GetCutInfoRecord1_ToDay

integer(int32) GetCutInfoRecord1_IntervalInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_IntervalInfo = CutInfoRecord1%IntervalInfo
end function GetCutInfoRecord1_IntervalInfo

real(dp) GetCutInfoRecord1_IntervalGDD()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_IntervalGDD = CutInfoRecord1%IntervalGDD
end function GetCutInfoRecord1_IntervalGDD

real(dp) GetCutInfoRecord1_MassInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_MassInfo = CutInfoRecord1%MassInfo
end function GetCutInfoRecord1_MassInfo

subroutine SetCutInfoRecord1_NoMoreInfo(NoMoreInfo)
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_NoMoreInfo = CutInfoRecord1%NoMoreInfo
end function GetCutInfoRecord1_NoMoreInfo

! CutInfoRecord2

logical GetCutInfoRecord2_NoMoreInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_NoMoreInfo = CutInfoRecord2%NoMoreInfo
end function GetCutInfoRecord2_NoMoreInfo

integer(int32) GetCutInfoRecord2_FromDay()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_FromDay = CutInfoRecord2%FromDay
end function GetCutInfoRecord2_FromDay

integer(int32) GetCutInfoRecord2_ToDay()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_ToDay = CutInfoRecord2%ToDay
end function GetCutInfoRecord2_ToDay

integer(int32) GetCutInfoRecord2_IntervalInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_IntervalInfo = CutInfoRecord2%IntervalInfo
end function GetCutInfoRecord2_IntervalInfo

real(dp) GetCutInfoRecord2_IntervalGDD()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_IntervalGDD = CutInfoRecord2%IntervalGDD
end function GetCutInfoRecord2_IntervalGDD

real(dp) GetCutInfoRecord2_MassInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_MassInfo = CutInfoRecord2%MassInfo
end function GetCutInfoRecord2_MassInfo

! Transfer

logical GetTransfer_Store()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Store = Transfer%Store
end function GetTransfer_Store

logical GetTransfer_Mobilize()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Mobilize = Transfer%Mobilize
end function GetTransfer_Mobilize

real(dp) GetTransfer_ToMobilize()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_ToMobilize = Transfer%ToMobilize
end function GetTransfer_ToMobilize

real(dp) GetTransfer_Bmobilized()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Bmobilized = Transfer%Bmobilized
end function GetTransfer_Bmobilized

end module ac_run
