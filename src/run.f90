module ac_run

use iso_fortran_env, only: iostat_end
use ac_kinds, only: dp, &
                    int32
use ac_global, only:    CompartmentIndividual, &
                        datatype_daily, &
                        datatype_decadely, &
                        datatype_monthly, &
                        DegreesDay, &
                        DetermineDate, &
                        DetermineDayNr, &
                        DetermineSaltContent, &
                        FileExists, &
                        GetCompartment_i, &
                        GetCompartment_Layer, &
                        GetCompartment_Thickness, &
                        GetCrop_Day1, &
                        GetCrop_Tbase, &
                        GetCrop_Tupper, &
                        GetGroundWaterFile, &
                        GetGroundWaterFileFull, &
                        GetNrCompartments, &
                        GetPathNameProg, &
                        GetSimulation_FromDayNr, &
                        GetSimulation_SumGDD, &
                        GetSimulation_ToDayNr, &
                        GetSimulParam_GDDMethod, &
                        GetSoilLayer_SAT, &
                        GetTemperatureFilefull, &
                        GetTemperatureRecord_DataType, &
                        GetTemperatureRecord_FromDayNr, &
                        GetZiAqua, &
                        GetECiAqua, &
                        rep_DayEventDbl, &
                        rep_sum, &
                        roundc, &
                        SetCompartment_i, &
                        SetCompartment_Theta, &
                        SetSimulation_SumGDD, &
                        SplitStringInThreeParams, &
                        SplitStringInTwoParams


use ac_tempprocessing, only:    GetDecadeTemperatureDataSet

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
type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet

contains



!! Section for Getters and Setters for global variables

! GwTable

integer(int32) function GetGwTable_DNr1()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_DNr1 = GwTable%DNr1
end function GetGwTable_DNr1

integer(int32) function GetGwTable_DNr2()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_DNr2 = GwTable%DNr2
end function GetGwTable_DNr2

integer(int32) function GetGwTable_Z1()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_Z1 = GwTable%Z1
end function GetGwTable_Z1

integer(int32) function GetGwTable_Z2()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_Z2 = GwTable%Z2
end function GetGwTable_Z2

real(dp) function GetGwTable_EC1()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_EC1 = GwTable%EC1
end function GetGwTable_EC1

real(dp) function GetGwTable_EC2()
    !! Getter for the "GetGwTable" global variable.
    
    GetGwTable_EC2 = GwTable%EC2
end function GetGwTable_EC2

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

type(rep_plotPar) function GetPlotVarCrop()
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

logical function GetIrriInfoRecord1_NoMoreInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_NoMoreInfo = IrriInfoRecord1%NoMoreInfo
end function GetIrriInfoRecord1_NoMoreInfo

integer(int32) function GetIrriInfoRecord1_FromDay()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_FromDay = IrriInfoRecord1%FromDay
end function GetIrriInfoRecord1_FromDay

integer(int32) function GetIrriInfoRecord1_ToDay()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_ToDay = IrriInfoRecord1%ToDay
end function GetIrriInfoRecord1_ToDay

integer(int32) function GetIrriInfoRecord1_TimeInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_TimeInfo = IrriInfoRecord1%TimeInfo
end function GetIrriInfoRecord1_TimeInfo

integer(int32) function GetIrriInfoRecord1_DepthInfo()
    !! Getter for the "IrriInfoRecord1" global variable.
    
    GetIrriInfoRecord1_DepthInfo = IrriInfoRecord1%DepthInfo
end function GetIrriInfoRecord1_DepthInfo

subroutine SetIrriInfoRecord1_NoMoreInfo(NoMoreInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    logical, intent(in) :: NoMoreInfo
    
    IrriInfoRecord1%NoMoreInfo = NoMoreInfo
end subroutine SetIrriInfoRecord1_NoMoreInfo

subroutine SetIrriInfoRecord1_FromDay(FromDay)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: FromDay
    
    IrriInfoRecord1%FromDay = FromDay
end subroutine SetIrriInfoRecord1_FromDay

subroutine SetIrriInfoRecord1_ToDay(ToDay)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: ToDay
    
    IrriInfoRecord1%ToDay = ToDay
end subroutine SetIrriInfoRecord1_ToDay

subroutine SetIrriInfoRecord1_TimeInfo(TimeInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: TimeInfo
    
    IrriInfoRecord1%TimeInfo = TimeInfo
end subroutine SetIrriInfoRecord1_TimeInfo

subroutine SetIrriInfoRecord1_DepthInfo(DepthInfo)
    !! Setter for the "IrriInfoRecord1" global variable.
    integer(int32), intent(in) :: DepthInfo
    
    IrriInfoRecord1%DepthInfo = DepthInfo
end subroutine SetIrriInfoRecord1_DepthInfo

! IrriInfoRecord2

logical function GetIrriInfoRecord2_NoMoreInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_NoMoreInfo = IrriInfoRecord2%NoMoreInfo
end function GetIrriInfoRecord2_NoMoreInfo

integer(int32) function GetIrriInfoRecord2_FromDay()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_FromDay = IrriInfoRecord2%FromDay
end function GetIrriInfoRecord2_FromDay

integer(int32) function GetIrriInfoRecord2_ToDay()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_ToDay = IrriInfoRecord2%ToDay
end function GetIrriInfoRecord2_ToDay

integer(int32) function GetIrriInfoRecord2_TimeInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_TimeInfo = IrriInfoRecord2%TimeInfo
end function GetIrriInfoRecord2_TimeInfo

integer(int32) function GetIrriInfoRecord2_DepthInfo()
    !! Getter for the "IrriInfoRecord2" global variable.
    
    GetIrriInfoRecord2_DepthInfo = IrriInfoRecord2%DepthInfo
end function GetIrriInfoRecord2_DepthInfo

subroutine SetIrriInfoRecord2_NoMoreInfo(NoMoreInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    logical, intent(in) :: NoMoreInfo
    
    IrriInfoRecord2%NoMoreInfo = NoMoreInfo
end subroutine SetIrriInfoRecord2_NoMoreInfo

subroutine SetIrriInfoRecord2_FromDay(FromDay)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: FromDay
    
    IrriInfoRecord2%FromDay = FromDay
end subroutine SetIrriInfoRecord2_FromDay

subroutine SetIrriInfoRecord2_ToDay(ToDay)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: ToDay
    
    IrriInfoRecord2%ToDay = ToDay
end subroutine SetIrriInfoRecord2_ToDay

subroutine SetIrriInfoRecord2_TimeInfo(TimeInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: TimeInfo
    
    IrriInfoRecord2%TimeInfo = TimeInfo
end subroutine SetIrriInfoRecord2_TimeInfo

subroutine SetIrriInfoRecord2_DepthInfo(DepthInfo)
    !! Setter for the "IrriInfoRecord2" global variable.
    integer(int32), intent(in) :: DepthInfo
    
    IrriInfoRecord2%DepthInfo = DepthInfo
end subroutine SetIrriInfoRecord2_DepthInfo

! StressTot

real(dp) function GetStressTot_Salt()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_Salt = StressTot%Salt
end function GetStressTot_Salt

real(dp) function GetStressTot_Temp()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_Temp = StressTot%Temp
end function GetStressTot_Temp

real(dp) function GetStressTot_Exp()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_Exp = StressTot%Exp
end function GetStressTot_Exp

real(dp) function GetStressTot_Sto()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_Sto = StressTot%Sto
end function GetStressTot_Sto

real(dp) function GetStressTot_Weed()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_Weed = StressTot%Weed
end function GetStressTot_Weed

integer(int32) function GetStressTot_NrD()
    !! Getter for the "StressTot" global variable.
    
    GetStressTot_NrD = StressTot%NrD
end function GetStressTot_NrD

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

logical function GetCutInfoRecord1_NoMoreInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_NoMoreInfo = CutInfoRecord1%NoMoreInfo
end function GetCutInfoRecord1_NoMoreInfo

integer(int32) function GetCutInfoRecord1_FromDay()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_FromDay = CutInfoRecord1%FromDay
end function GetCutInfoRecord1_FromDay

integer(int32) function GetCutInfoRecord1_ToDay()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_ToDay = CutInfoRecord1%ToDay
end function GetCutInfoRecord1_ToDay

integer(int32) function GetCutInfoRecord1_IntervalInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_IntervalInfo = CutInfoRecord1%IntervalInfo
end function GetCutInfoRecord1_IntervalInfo

real(dp) function GetCutInfoRecord1_IntervalGDD()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_IntervalGDD = CutInfoRecord1%IntervalGDD
end function GetCutInfoRecord1_IntervalGDD

real(dp) function GetCutInfoRecord1_MassInfo()
    !! Getter for the "CutInfoRecord1" global variable.
    
    GetCutInfoRecord1_MassInfo = CutInfoRecord1%MassInfo
end function GetCutInfoRecord1_MassInfo

subroutine SetCutInfoRecord1_NoMoreInfo(NoMoreInfo)
    !! Setter for the "CutInfoRecord1" global variable.
    logical, intent(in) :: NoMoreInfo
    
    CutInfoRecord1%NoMoreInfo = NoMoreInfo
end subroutine SetCutInfoRecord1_NoMoreInfo


subroutine SetCutInfoRecord1_FromDay(FromDay)
    !! Setter for the "CutInfoRecord1" global variable.
    integer(int32), intent(in) :: FromDay
    
    CutInfoRecord1%FromDay = FromDay
end subroutine SetCutInfoRecord1_FromDay

subroutine SetCutInfoRecord1_ToDay(ToDay)
    !! Setter for the "CutInfoRecord1" global variable.
    integer(int32), intent(in) :: ToDay
    
    CutInfoRecord1%ToDay = ToDay
end subroutine SetCutInfoRecord1_ToDay

subroutine SetCutInfoRecord1_IntervalInfo(IntervalInfo)
    !! Setter for the "CutInfoRecord1" global variable.
    integer(int32), intent(in) :: IntervalInfo
    
    CutInfoRecord1%IntervalInfo = IntervalInfo
end subroutine SetCutInfoRecord1_IntervalInfo

subroutine SetCutInfoRecord1_IntervalGDD(IntervalGDD)
    !! Setter for the "CutInfoRecord1" global variable.
    real(dp), intent(in) :: IntervalGDD
    
    CutInfoRecord1%IntervalGDD = IntervalGDD
end subroutine SetCutInfoRecord1_IntervalGDD

subroutine SetCutInfoRecord1_MassInfo(MassInfo)
    !! Setter for the "CutInfoRecord1" global variable.
    real(dp), intent(in) :: MassInfo
    
    CutInfoRecord1%MassInfo = MassInfo
end subroutine SetCutInfoRecord1_MassInfo


! CutInfoRecord2

logical function GetCutInfoRecord2_NoMoreInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_NoMoreInfo = CutInfoRecord2%NoMoreInfo
end function GetCutInfoRecord2_NoMoreInfo

integer(int32) function GetCutInfoRecord2_FromDay()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_FromDay = CutInfoRecord2%FromDay
end function GetCutInfoRecord2_FromDay

integer(int32) function GetCutInfoRecord2_ToDay()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_ToDay = CutInfoRecord2%ToDay
end function GetCutInfoRecord2_ToDay

integer(int32) function GetCutInfoRecord2_IntervalInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_IntervalInfo = CutInfoRecord2%IntervalInfo
end function GetCutInfoRecord2_IntervalInfo

real(dp) function GetCutInfoRecord2_IntervalGDD()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_IntervalGDD = CutInfoRecord2%IntervalGDD
end function GetCutInfoRecord2_IntervalGDD

real(dp) function GetCutInfoRecord2_MassInfo()
    !! Getter for the "CutInfoRecord2" global variable.
    
    GetCutInfoRecord2_MassInfo = CutInfoRecord2%MassInfo
end function GetCutInfoRecord2_MassInfo

subroutine SetCutInfoRecord2_NoMoreInfo(NoMoreInfo)
    !! Setter for the "CutInfoRecord2" global variable.
    logical, intent(in) :: NoMoreInfo
    
    CutInfoRecord2%NoMoreInfo = NoMoreInfo
end subroutine SetCutInfoRecord2_NoMoreInfo

subroutine SetCutInfoRecord2_FromDay(FromDay)
    !! Setter for the "CutInfoRecord2" global variable.
    integer(int32), intent(in) :: FromDay
    
    CutInfoRecord2%FromDay = FromDay
end subroutine SetCutInfoRecord2_FromDay

subroutine SetCutInfoRecord2_ToDay(ToDay)
    !! Setter for the "CutInfoRecord2" global variable.
    integer(int32), intent(in) :: ToDay
    
    CutInfoRecord2%ToDay = ToDay
end subroutine SetCutInfoRecord2_ToDay

subroutine SetCutInfoRecord2_IntervalInfo(IntervalInfo)
    !! Setter for the "CutInfoRecord2" global variable.
    integer(int32), intent(in) :: IntervalInfo
    
    CutInfoRecord2%IntervalInfo = IntervalInfo
end subroutine SetCutInfoRecord2_IntervalInfo

subroutine SetCutInfoRecord2_IntervalGDD(IntervalGDD)
    !! Setter for the "CutInfoRecord2" global variable.
    real(dp), intent(in) :: IntervalGDD
    
    CutInfoRecord2%IntervalGDD = IntervalGDD
end subroutine SetCutInfoRecord2_IntervalGDD

subroutine SetCutInfoRecord2_MassInfo(MassInfo)
    !! Setter for the "CutInfoRecord2" global variable.
    real(dp), intent(in) :: MassInfo
    
    CutInfoRecord2%MassInfo = MassInfo
end subroutine SetCutInfoRecord2_MassInfo

! Transfer

logical function GetTransfer_Store()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Store = Transfer%Store
end function GetTransfer_Store

logical function GetTransfer_Mobilize()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Mobilize = Transfer%Mobilize
end function GetTransfer_Mobilize

real(dp) function GetTransfer_ToMobilize()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_ToMobilize = Transfer%ToMobilize
end function GetTransfer_ToMobilize

real(dp) function GetTransfer_Bmobilized()
    !! Getter for the "Transfer" global variable.
    
    GetTransfer_Bmobilized = Transfer%Bmobilized
end function GetTransfer_Bmobilized

subroutine SetTransfer_Store(Store)
    !! Setter for the "Transfer" global variable.
    logical, intent(in) :: Store

    Transfer%Store = Store
end subroutine SetTransfer_Store

subroutine SetTransfer_Mobilize(Mobilize)
    !! Setter for the "Transfer" global variable.
    logical, intent(in) :: Mobilize

    Transfer%Mobilize = Mobilize
end subroutine SetTransfer_Mobilize

subroutine SetTransfer_ToMobilize(ToMobilize)
    !! Setter for the "Transfer" global variable.
    real(dp), intent(in) :: ToMobilize

    Transfer%ToMobilize = ToMobilize
end subroutine SetTransfer_ToMobilize

subroutine SetTransfer_Bmobilized(Bmobilized)
    !! Setter for the "Transfer" global variable.
    real(dp), intent(in) :: Bmobilized

    Transfer%Bmobilized = Bmobilized
end subroutine SetTransfer_Bmobilized

!TminDatSet

function GetTminDataSet() result(TminDataSet_out)
    !! Getter for the "TminDataSet" global variable.
    type(rep_DayEventDbl), dimension(31) :: TminDataSet_out

    TminDataSet_out = TminDataSet
end function GetTminDataSet

function GetTminDataSet_i(i) result(TminDataSet_i)
    !! Getter for individual elements of the "TminDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl) :: TminDataSet_i

    TminDataSet_i = TminDataSet(i)
end function GetTminDataSet_i

integer(int32) function GetTminDataSet_DayNr(i)
    integer(int32), intent(in) :: i

    GetTminDataSet_DayNr = TminDataSet(i)%DayNr
end function GetTminDataSet_DayNr

real(dp) function GetTminDataSet_Param(i)
    integer(int32), intent(in) :: i

    GetTminDataSet_Param = TminDataSet(i)%Param
end function GetTminDataSet_Param

subroutine SetTminDataSet(TminDataSet_in)
    !! Setter for the "TminDatSet" global variable.
    type(rep_DayEventDbl), dimension(31), intent(in) :: TminDataSet_in

    TminDataSet = TminDataSet_in
end subroutine SetTminDataSet

subroutine SetTminDataSet_i(i, TminDataSet_i)
    !! Setter for individual element for the "TminDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl), intent(in) :: TminDataSet_i

    TminDataSet(i) = TminDataSet_i
end subroutine SetTminDataSet_i

subroutine SetTminDataSet_DayNr(i, DayNr_in)
    integer(int32), intent(in) :: i
    integer(int32), intent(in) :: DayNr_in

    TminDataSet(i)%DayNr = DayNr_in
end subroutine SetTminDataSet_DayNr

subroutine SetTminDataSet_Param(i, Param_in)
    integer(int32), intent(in) :: i
    real(dp), intent(in) :: Param_in

    TminDataSet(i)%Param = Param_in
end subroutine SetTminDataSet_Param

! TmaxDataSet

function GetTmaxDataSet() result(TmaxDataSet_out)
    !! Getter for the "TmaxDataSet" global variable.
    type(rep_DayEventDbl), dimension(31) :: TmaxDataSet_out

    TmaxDataSet_out = TmaxDataSet
end function GetTmaxDataSet

function GetTmaxDataSet_i(i) result(TmaxDataSet_i)
    !! Getter for individual elements of the "TmaxDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl) :: TmaxDataSet_i

    TmaxDataSet_i = TmaxDataSet(i)
end function GetTmaxDataSet_i

integer(int32) function GetTmaxDataSet_DayNr(i)
    integer(int32), intent(in) :: i

    GetTmaxDataSet_DayNr = TmaxDataSet(i)%DayNr
end function GetTmaxDataSet_DayNr

real(dp) function GetTmaxDataSet_Param(i)
    integer(int32), intent(in) :: i

    GetTmaxDataSet_Param = TmaxDataSet(i)%Param
end function GetTmaxDataSet_Param

subroutine SetTmaxDataSet(TmaxDataSet_in)
    !! Setter for the "TmaxDatSet" global variable.
    type(rep_DayEventDbl), dimension(31), intent(in) :: TmaxDataSet_in

    TmaxDataSet = TmaxDataSet_in
end subroutine SetTmaxDataSet

subroutine SetTmaxDataSet_i(i, TmaxDataSet_i)
    !! Setter for individual element for the "TmaxDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl), intent(in) :: TmaxDataSet_i

    TmaxDataSet(i) = TmaxDataSet_i
end subroutine SetTmaxDataSet_i

subroutine SetTmaxDataSet_DayNr(i, DayNr_in)
    integer(int32), intent(in) :: i
    integer(int32), intent(in) :: DayNr_in

    TmaxDataSet(i)%DayNr = DayNr_in
end subroutine SetTmaxDataSet_DayNr

subroutine SetTmaxDataSet_Param(i, Param_in)
    integer(int32), intent(in) :: i
    real(dp), intent(in) :: Param_in

    TmaxDataSet(i)%Param = Param_in
end subroutine SetTmaxDataSet_Param

! END global variables section




subroutine AdjustForWatertable()

    real(dp) :: Ztot, Zi
    integer(int32) :: compi
    type(CompartmentIndividual) :: Compi_temp

    Ztot = 0.0_dp
    do compi = 1, GetNrCompartments() 
        Ztot = Ztot + GetCompartment_Thickness(compi)
        Zi = Ztot - GetCompartment_Thickness(compi)/2.0_dp
        if (Zi >= (GetZiAqua()/100.0_dp)) then
            ! compartment at or below groundwater table
            call SetCompartment_Theta(compi, &
                GetSoilLayer_SAT(GetCompartment_Layer(compi))/100.0_dp)
            Compi_temp = GetCompartment_i(compi)
            call DetermineSaltContent(GetECiAqua(), Compi_temp)
            call SetCompartment_i(compi, Compi_temp)
        end if
    end do
end subroutine AdjustForWatertable

subroutine ResetPreviousSum(PreviousSum, SumETo, SumGDD, PreviousSumETo, &
        PreviousSumGDD, PreviousBmob, PreviousBsto)
    type(rep_sum), intent(inout) :: PreviousSum
    real(dp), intent(inout) :: SumETo
    real(dp), intent(inout) :: SumGDD
    real(dp), intent(inout) :: PreviousSumETo
    real(dp), intent(inout) :: PreviousSumGDD
    real(dp), intent(inout) :: PreviousBmob
    real(dp), intent(inout) :: PreviousBsto

    PreviousSum%Epot = 0.0_dp
    PreviousSum%Tpot = 0.0_dp
    PreviousSum%Rain = 0.0_dp
    PreviousSum%Irrigation = 0.0_dp
    PreviousSum%Infiltrated = 0.0_dp
    PreviousSum%Runoff = 0.0_dp
    PreviousSum%Drain = 0.0_dp
    PreviousSum%Eact = 0.0_dp
    PreviousSum%Tact = 0.0_dp
    PreviousSum%TrW = 0.0_dp
    PreviousSum%ECropCycle = 0.0_dp
    PreviousSum%CRwater = 0.0_dp
    PreviousSum%Biomass = 0.0_dp
    PreviousSum%YieldPart = 0.0_dp
    PreviousSum%BiomassPot = 0.0_dp
    PreviousSum%BiomassUnlim = 0.0_dp
    PreviousSum%SaltIn = 0.0_dp
    PreviousSum%SaltOut = 0.0_dp
    PreviousSum%CRsalt = 0.0_dp
    SumETo = 0.0_dp
    SumGDD = 0.0_dp
    PreviousSumETo = 0.0_dp
    PreviousSumGDD = 0.0_dp
    PreviousBmob = 0.0_dp
    PreviousBsto = 0.0_dp
end subroutine ResetPreviousSum

subroutine GetGwtSet(DayNrIN, GwT)
    integer(int32), intent(in) :: DayNrIN
    type(rep_GwTable), intent(inout) :: GwT

    integer :: f0
    character(len=:), allocatable :: FileNameFull
    integer(int32) :: DayNr1Gwt, DNrini, rc
    integer(int32) :: i, dayi, monthi, yeari, Zini, yearACT
    real(dp) :: DayDouble, Zm, ECini
    character(len=255) :: StringREAD
    logical :: TheEnd

    ! FileNameFull
    if (GetGroundWaterFile() /= '(None)') then
        FileNameFull = GetGroundWaterFileFull()
    else
        FileNameFull = trim(GetPathNameProg())//'GroundWater.AqC'
    end if

    ! Get DayNr1Gwt
    open(newunit=f0, file=trim(FileNameFull), status='old', &
                 action='read', iostat=rc)
    read(f0, *, iostat=rc) ! description
    read(f0, *, iostat=rc) ! AquaCrop Version number
    read(f0, *, iostat=rc) ! Mode
    read(f0, *, iostat=rc) dayi
    read(f0, *, iostat=rc) monthi
    read(f0, *, iostat=rc) yeari
    call DetermineDayNr(dayi, monthi, yeari, DayNr1Gwt)

    ! Read first observation
    do i = 1, 3 
        read(f0, *, iostat=rc)
    end do
    read(f0, '(a)', iostat=rc) StringREAD
    call SplitStringInThreeParams(StringREAD, DayDouble, Zm, GwT%EC2)
    GwT%DNr2 = DayNr1Gwt + roundc(DayDouble, mold=1_int32) - 1
    GwT%Z2 = roundc(Zm * 100, mold=1_int32)
    if (rc == iostat_end) then
        TheEnd = .true.
    else
        TheEnd = .false.
    end if

    ! Read next observations
    if (TheEnd) then
        ! only one observation
        GwT%DNr1 = GetSimulation_FromDayNr()
        GwT%Z1 = GwT%Z2
        GwT%EC1 = GwT%EC2
        GwT%DNr2 = GetSimulation_ToDayNr()
    else
        ! defined year
        if (DayNr1Gwt > 365) then
            if (DayNrIN < GwT%DNr2) then
                ! DayNrIN before 1st observation
                GwT%DNr1 = GetSimulation_FromDayNr()
                GwT%Z1 = GwT%Z2
                GwT%EC1 = GwT%EC2
            else
                ! DayNrIN after or at 1st observation
                loop1: do
                    GwT%DNr1 = GwT%DNr2
                    GwT%Z1 = GwT%Z2
                    GwT%EC1 = GwT%EC2
                    read(f0, '(a)', iostat=rc) StringREAD
                    call SplitStringInThreeParams(StringREAD, DayDouble, Zm, GwT%EC2)
                    GwT%DNr2 = DayNr1Gwt + roundc(DayDouble, mold=1_int32) - 1
                    GwT%Z2 = roundc(Zm * 100, mold=1_int32)
                    if (DayNrIN < GwT%DNr2) then
                        TheEnd = .true.
                    end if
                    if (TheEnd .or. (rc == iostat_end)) exit loop1
                end do loop1
                if (.not. TheEnd) then
                    ! DayNrIN after last observation
                    GwT%DNr1 = GwT%DNr2
                    GwT%Z1 = GwT%Z2
                    GwT%EC1 = GwT%EC2
                    GwT%DNr2 = GetSimulation_ToDayNr()
                end if
            end if
        end if ! defined year
        
        ! undefined year
        if (DayNr1Gwt <= 365) then
            call DetermineDate(DayNrIN, dayi, monthi, yearACT)
            if (yearACT /= 1901) then
                ! make 1st observation defined
                call DetermineDate(GwT%DNr2, dayi, monthi, yeari)
                call DetermineDayNr(dayi, monthi, yearACT, GwT%DNr2)
            end if
            if (DayNrIN < GwT%DNr2) then
                ! DayNrIN before 1st observation
                loop2: do
                    read(f0, '(a)', iostat=rc) StringREAD
                    call SplitStringInThreeParams(StringREAD, DayDouble, Zm, GwT%EC1)
                    GwT%DNr1 = DayNr1Gwt + roundc(DayDouble, mold=1_int32) - 1
                    call DetermineDate(GwT%DNr1, dayi, monthi, yeari)
                    call DetermineDayNr(dayi, monthi, yearACT, GwT%DNr1)
                    GwT%Z1 = roundc(Zm * 100, mold=1_int32)
                    if (rc == iostat_end) exit loop2
                end do loop2
                GwT%DNr1 = GwT%DNr1 - 365
            else
                ! save 1st observation
                DNrini = GwT%DNr2
                Zini = GwT%Z2
                ECini = GwT%EC2
                ! DayNrIN after or at 1st observation
                loop3: do
                    GwT%DNr1 = GwT%DNr2
                    GwT%Z1 = GwT%Z2
                    GwT%EC1 = GwT%EC2
                    read(f0, '(a)', iostat=rc) StringREAD
                    call SplitStringInThreeParams(StringREAD, DayDouble, Zm, GwT%EC2)
                    GwT%DNr2 = DayNr1Gwt + roundc(DayDouble, mold=1_int32) - 1
                    if (yearACT /= 1901) then
                        ! make observation defined
                        call DetermineDate(GwT%DNr2, dayi, monthi, yeari)
                        call DetermineDayNr(dayi, monthi, yearACT, GwT%DNr2)
                    end if
                    GwT%Z2 = roundc(Zm * 100, mold=1_int32)
                    if (DayNrIN < GwT%DNr2) then
                        TheEnd = .true.
                    end if
                    if (TheEnd .or. (rc == iostat_end)) exit loop3
                end do loop3
                if (.not. TheEnd) then
                    ! DayNrIN after last observation
                    GwT%DNr1 = GwT%DNr2
                    GwT%Z1 = GwT%Z2
                    GwT%EC1 = GwT%EC2
                    GwT%DNr2 = DNrini + 365
                    GwT%Z2 = Zini
                    GwT%EC2 = ECini
                end if
            end if
        end if ! undefined year
    end if ! more than 1 observation
    close(f0)
end subroutine GetGwtSet

end module ac_run
