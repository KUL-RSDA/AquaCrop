module ac_run

use iso_fortran_env, only: iostat_end
use ac_kinds, only: dp, &
                    int8, &
                    int32, &
                    intEnum

use ac_global, only:    CompartmentIndividual, &
                        CompartmentIndividual, &
                        datatype_daily, &
                        datatype_decadely, &
                        datatype_monthly, &
                        DaysInMonth, &
                        DegreesDay, &
                        DetermineDate, &
                        DetermineDate, &
                        DetermineDayNr, &
                        DetermineDayNr, &
                        DetermineSaltContent, &
                        DetermineSaltContent, &
                        FileExists, &
                        GetCompartment_i, &
                        GetCompartment_i, &
                        GetCompartment_Layer, &
                        GetCompartment_Thickness, &
                        GetCrop_CCEffectEvapLate, &
                        GetCrop_CCo, &
                        getcrop_ccsaltdistortion, &
                        GetCrop_CCx, &
                        GetCrop_CDC, &
                        GetCrop_CGC, &
                        GetCrop_CGC, &
                        GetCrop_Day1, &
                        GetCrop_DayN, &
                        GetCrop_DaysToCCini, &
                        GetCrop_DaysToFlowering, &
                        GetCrop_DaysToFullCanopy, &
                        GetCrop_DaysToGermination, &
                        GetCrop_DaysToHarvest, &
                        GetCrop_DaysToSenescence, &
                        GetCrop_DeterminancyLinked, &
                        GetCrop_dHIdt, &
                        GetCrop_GDDaysToCCini, &
                        GetCrop_GDDaysToFlowering, &
                        GetCrop_GDDaysToFullCanopy, &
                        GetCrop_GDDaysToGermination, &
                        GetCrop_GDDaysToHarvest, &
                        GetCrop_GDDaysToSenescence, &
                        GetCrop_GDDCDC, &
                        GetCrop_GDDCGC, &
                        GetCrop_GDDLengthFlowering, &
                        GetCrop_GDtranspLow, &
                        GetCrop_GDtranspLow, &
                        GetCrop_HI, &
                        GetCrop_KcDecline, &
                        GetCrop_KcTop, &
                        GetCrop_Length_i, &
                        GetCrop_LengthFlowering, &
                        GetCrop_ModeCycle, &
                        GetCrop_StressResponse, &
                        GetCrop_StressResponse_Calibrated, &
                        GetCrop_subkind, &
                        GetCrop_Tbase, &
                        GetCrop_Tupper, &
                        GetCrop_Length_i, &
                        GetCrop_WP, &
                        GetCrop_WPy, &
                        GetECiAqua, &
                        GetETo, &
                        GetManagement_FertilityStress, &
                        GetGroundWaterFile, &
                        GetGroundWaterFileFull, &
                        GetIrriFile, &
                        GetIrriFilefull, &
                        GetIrriFirstDayNr, &
                        GetIrriMode, &
                        GetManagement_FertilityStress, &
                        GetNrCompartments, &
                        GetOutputAggregate, &
                        GetOutputName, &
                        GetPathNameOutp, &
                        GetPathNameProg, &
                        GetSimulation_DelayedDays, &
                        GetRain, &
                        GetSimulation_FromDayNr, &
                        GetSimulation_IrriECw, &
                        GetSimulation_SalinityConsidered, &
                        GetSimulation_SumGDD, &
                        GetSimulation_ToDayNr, &
                        GetSimulParam_GDDMethod, &
                        GetSimulParam_Tmax, &
                        GetSimulParam_Tmin, &
                        GetSoilLayer_SAT, &
                        GetSoilLayer_SAT, &
                        GetSumWaBal_Biomass, &
                        GetSumWaBal_BiomassUnlim, &
                        GetSumWaBal_SaltIn, &
                        GetSumWaBal_SaltOut, &
                        GetSumWaBal_CRsalt, &
                        GetTemperatureFile, &
                        GetTemperatureFilefull, &
                        GetTemperatureRecord_DataType, &
                        GetTemperatureRecord_FromDayNr, &
                        GetTmax, &
                        GetTmin, &
                        GetZiAqua, &
                        IrriMode_Generate, &
                        IrriMode_Manual, &
                        rep_DayEventDbl, &
                        LeapYear, &
                        rep_DayEventDbl, &
                        rep_sum, &
                        roundc, &
                        SetCompartment_i, &
                        SetCompartment_Theta, &
                        SetSimulation_IrriECw, &
                        SetSimulation_SumGDD, &
                        GetSimulation_DelayedDays, &
                        SetTmax, &
                        SetTmin, &
                        SplitStringInThreeParams, &
                        SplitStringInTwoParams, &
                        subkind_Grain, &
                        subkind_Tuber, &
                        typeproject_typenone, &
                        typeproject_typepro, &
                        typeproject_typeprm, &
                        undef_int


use ac_tempprocessing, only:    CCxSaltStressRelationship, &
                                GetDecadeTemperatureDataSet, &
                                GetMonthlyTemperaturedataset, &
                                StressBiomassRelationship


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


integer :: fDaily  ! file handle
integer :: fDaily_iostat  ! IO status
integer :: fRun  ! file handle
integer :: fRun_iostat  ! IO status
integer :: fIrri  ! file handle
integer :: fIrri_iostat  ! IO status
integer :: fEToSIM ! file handle
integer :: fEToSIM_iostat ! IO status
integer :: fRainSIM ! file handle
integer :: fRainSIM_iostat ! IO status
integer :: fTempSIM ! file handle
integer :: fTempSIM_iostat ! IO status
integer :: fCuts ! file handle
integer :: fCuts_iostat ! IO status
integer :: fObs ! file handle
integer :: fObs_iostat ! IO status


type(rep_GwTable) :: GwTable
type(rep_DayEventDbl), dimension(31) :: EToDataSet
type(rep_DayEventDbl), dimension(31) :: RainDataSet
type(rep_plotPar) :: PlotVarCrop
type(repIrriInfoRecord) :: IrriInfoRecord1, IrriInfoRecord2
type(rep_StressTot) :: StressTot
type(repCutInfoRecord) :: CutInfoRecord1, CutInfoRecord2
type(rep_Transfer) :: Transfer
type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet
type(rep_sum) :: PreviousSum

integer(int32) :: DayNri
integer(int32) :: IrriInterval
integer(int32) :: Tadj, GDDTadj
integer(int32) :: DayLastCut,NrCut,SumInterval
integer(int8)  :: PreviousStressLevel, StressSFadjNEW

real(dp) :: Bin
real(dp) :: Bout
real(dp) :: GDDayi
real(dp) :: CO2i
real(dp) :: FracBiomassPotSF
real(dp) :: CCxWitheredTpot,CCxWitheredTpotNoS
real(dp) :: Coeffb0,Coeffb1,Coeffb2
real(dp) :: Coeffb0Salt,Coeffb1Salt,Coeffb2Salt
real(dp) :: StressLeaf,StressSenescence !! stress for leaf expansion and senescence
real(dp) :: DayFraction,GDDayFraction
real(dp) :: CGCref,GDDCGCref 

logical :: GlobalIrriECw ! for versions before 3.2 where EC of 
                         ! irrigation water was not yet recorded


contains


subroutine open_file(fhandle, filename, mode, iostat)
    !! Opens a file in the given mode.
    integer, intent(out) :: fhandle
        !! file handle to be used for the open file
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')
    integer, intent(out) :: iostat
        !! IO status returned by open()

    logical :: file_exists

    inquire(file=filename, exist=file_exists)

    if (mode == 'r') then
        open(newunit=fhandle, file=trim(filename), status='old', &
             action='read', iostat=iostat)
    elseif (mode == 'a') then
        if (file_exists) then
            open(newunit=fhandle, file=trim(filename), status='old', &
                 position='append', action='write', iostat=iostat)
        else
            open(newunit=fhandle, file=trim(filename), status='replace', &
                 action='write', iostat=iostat)
        end if
    elseif (mode == 'w') then
        open(newunit=fhandle, file=trim(filename), status='new', &
             action='write', iostat=iostat)
    end if
end subroutine open_file


subroutine write_file(fhandle, line, advance, iostat)
    !! Writes one line to a file.
    integer, intent(in) :: fhandle
        !! file handle of an already-opened file
    character(len=*), intent(in) :: line
        !! line to write to the file
    logical, intent(in) :: advance
        !! whether or not to append a newline character
    integer, intent(out) :: iostat
        !! IO status returned by write()

    character(len=:), allocatable :: advance_str

    if (advance) then
        advance_str = 'yes'
    else
        advance_str = 'no'
    end if

    write(fhandle, '(a)', advance=advance_str, iostat=iostat) line
end subroutine write_file


function read_file(fhandle, iostat) result(line)
    !! Returns the next line read from the given file.
    integer, intent(in) :: fhandle
        !! file handle of an already-opened file
    integer, intent(out) :: iostat
        !! IO status returned by read()
    character(len=:), allocatable :: line
        !! string which will contain the content of the next line

    integer, parameter :: length = 1024  ! max. no of characters

    allocate(character(len=length) :: line)
    read(fhandle, '(a)', iostat=iostat) line
    line = trim(line)
end function read_file


!! Section for Getters and Setters for global variables

! fDaily


subroutine fDaily_open(filename, mode)
    !! Opens the given file, assigning it to the 'fDaily' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')

    call open_file(fDaily, filename, mode, fDaily_iostat)
end subroutine fDaily_open


subroutine fDaily_write(line, advance_in)
    !! Writes the given line to the fDaily file.
    character(len=*), intent(in) :: line
        !! line to write
    logical, intent(in), optional :: advance_in
        !! whether or not to append a newline character

    logical :: advance

    if (present(advance_in)) then
        advance = advance_in
    else
        advance = .true.
    end if
    call write_file(fDaily, line, advance, fDaily_iostat)
end subroutine fDaily_write


subroutine fDaily_close()
    close(fDaily)
end subroutine fDaily_close

! fRun

subroutine fRun_open(filename, mode)
    !! Opens the given file, assigning it to the 'fRun' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')

    call open_file(fRun, filename, mode, fRun_iostat)
end subroutine fRun_open


subroutine fRun_write(line, advance_in)
    !! Writes the given line to the fRun file.
    character(len=*), intent(in) :: line
        !! line to write
    logical, intent(in), optional :: advance_in
        !! whether or not to append a newline character

    logical :: advance

    if (present(advance_in)) then
        advance = advance_in
    else
        advance = .true.
    end if
    call write_file(fRun, line, advance, fRun_iostat)
end subroutine fRun_write


subroutine fRun_close()
    close(fRun)
end subroutine fRun_close


! fIrri

subroutine fIrri_open(filename, mode)
    !! Opens the given file, assigning it to the 'fIrri' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')

    call open_file(fIrri, filename, mode, fIrri_iostat)
end subroutine fIrri_open


function fIrri_read() result(line)
    !! Returns the next line read from the 'fIrri' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fIrri, fIrri_iostat)
end function fIrri_read


function fIrri_eof() result(eof)
    !! Returns whether the end of the 'fIrri' file has been reached.
    logical :: eof

    eof = fIrri_iostat == iostat_end
end function fIrri_eof


subroutine fIrri_close()
    close(fIrri)
end subroutine fIrri_close


! fEToSIM

subroutine fEToSIM_open(filename, mode)
    !! Opens the given file, assigning it to the 'fEToSIM' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')
    call open_file(fEToSIM, filename, mode, fEToSIM_iostat)
end subroutine fEToSIM_open


function fEToSIM_read() result(line)
    !! Returns the next line read from the 'fEToSIM' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fEToSIM, fEToSIM_iostat)
end function fEToSIM_read


subroutine fEToSIM_close()
    close(fEToSIM)
end subroutine fEToSIM_close


! fTempSIM

subroutine fTempSIM_open(filename, mode)
    !! Opens the given file, assigning it to the 'fTempSIM' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')
    call open_file(fTempSIM, filename, mode, fTempSIM_iostat)
end subroutine fTempSIM_open


function fTempSIM_read() result(line)
    !! Returns the next line read from the 'fTempSIM' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fTempSIM, fTempSIM_iostat)
end function fTempSIM_read


subroutine fTempSIM_close()
    close(fTempSIM)
end subroutine fTempSIM_close


! fRainSIM

subroutine fRainSIM_open(filename, mode)
    !! Opens the given file, assigning it to the 'fRainSIM' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')
    call open_file(fRainSIM, filename, mode, fRainSIM_iostat)
end subroutine fRainSIM_open


function fRainSIM_read() result(line)
    !! Returns the next line read from the 'fRainSIM' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fRainSIM, fRainSIM_iostat)
end function fRainSIM_read


subroutine fRainSIM_close()
    close(fRainSIM)
end subroutine fRainSIM_close


! fCuts

subroutine fCuts_open(filename, mode)
    !! Opens the given file, assigning it to the 'fCuts' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')
    call open_file(fCuts, filename, mode, fCuts_iostat)
end subroutine fCuts_open


function fCuts_read() result(line)
    !! Returns the next line read from the 'fCuts' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fCuts, fCuts_iostat)
end function fCuts_read


function fCuts_eof() result(eof)
    !! Returns whether the end of the 'fCuts' file has been reached.
    logical :: eof

    eof = fCuts_iostat == iostat_end
end function fCuts_eof


subroutine fCuts_close()
    close(fCuts)
end subroutine fCuts_close

! fObs

subroutine fObs_open(filename, mode)
    !! Opens the given file, assigning it to the 'fObs' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')

    call open_file(fObs, filename, mode, fObs_iostat)
end subroutine fObs_open


function fObs_read() result(line)
    !! Returns the next line read from the 'fObs' file.
    character(len=:), allocatable :: line
        !! name of the file to assign the file handle to

    line = read_file(fObs, fObs_iostat)
end function fObs_read


function fObs_eof() result(eof)
    !! Returns whether the end of the 'fObs' file has been reached.
    logical :: eof

    eof = fObs_iostat == iostat_end
end function fObs_eof


subroutine fObs_close()
    close(fObs)
end subroutine fObs_close

subroutine fObs_rewind()
    rewind(fObs)
end subroutine fObs_rewind


! Bin

real(dp) function GetBin()
    !! Getter for the "Bin" global variable.

    GetBin = Bin
end function GetBin

subroutine SetBin(Bin_in)
    !! Setter for the "Bin" global variable.
    real(dp), intent(in) :: Bin_in
    
    Bin = Bin_in
end subroutine SetBin

! Bout

real(dp) function GetBout()
    !! Getter for the "Bout" global variable.

    GetBout = Bout
end function GetBout

subroutine SetBout(Bout_in)
    !! Setter for the "Bout" global variable.
    real(dp), intent(in) :: Bout_in
    
    Bout = Bout_in
end subroutine SetBout

! GDDayi

real(dp) function GetGDDayi()
    !! Getter for the "GDDayi" global variable.

    GetGDDayi = GDDayi
end function GetGDDayi

subroutine SetGDDayi(GDDayi_in)
    !! Setter for the "GDDayi" global variable.
    real(dp), intent(in) :: GDDayi_in
    
    GDDayi = GDDayi_in
end subroutine SetGDDayi

! FracBiomass

real(dp) function GetFracBiomassPotSF()
    !! Getter for the "FracBiomassPotSF" global variable.

    GetFracBiomassPotSF = FracBiomassPotSF
end function GetFracBiomassPotSF

subroutine SetFracBiomassPotSF(FracBiomassPotSF_in)
    !! Setter for the "FracBiomassPotSF" global variable.
    real(dp), intent(in) :: FracBiomassPotSF_in
    
    FracBiomassPotSF = FracBiomassPotSF_in
end subroutine SetFracBiomassPotSF


! CO2i

real(dp) function GetCO2i()
    !! Getter for the "CO2i" global variable.

    GetCO2i = CO2i
end function GetCO2i

subroutine SetCO2i(CO2i_in)
    !! Setter for the "CO2i" global variable.
    real(dp), intent(in) :: CO2i_in
    
    CO2i = CO2i_in
end subroutine SetCO2i


! PreviousSum
type(rep_sum) function GetPreviousSum()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum = PreviousSum
end function GetPreviousSum

real(dp) function GetPreviousSum_Epot()
    !! Getter for the "PreviousSum" global variable.

     GetPreviousSum_Epot = PreviousSum%Epot
end function GetPreviousSum_Epot

real(dp) function GetPreviousSum_Tpot()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Tpot = PreviousSum%Tpot
end function GetPreviousSum_Tpot

real(dp) function GetPreviousSum_Rain()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Rain = PreviousSum%Rain
end function GetPreviousSum_Rain

real(dp) function GetPreviousSum_Irrigation()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Irrigation = PreviousSum%Irrigation
end function GetPreviousSum_Irrigation

real(dp) function GetPreviousSum_Infiltrated()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Infiltrated = PreviousSum%Infiltrated
end function GetPreviousSum_Infiltrated

real(dp) function GetPreviousSum_Runoff()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Runoff = PreviousSum%Runoff
end function GetPreviousSum_Runoff

real(dp) function GetPreviousSum_Drain()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Drain = PreviousSum%Drain
end function GetPreviousSum_Drain

real(dp) function GetPreviousSum_Eact()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Eact = PreviousSum%Eact
end function GetPreviousSum_Eact

real(dp) function GetPreviousSum_Tact()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Tact = PreviousSum%Tact
end function GetPreviousSum_Tact

real(dp) function GetPreviousSum_TrW()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_TrW = PreviousSum%TrW
end function GetPreviousSum_TrW

real(dp) function GetPreviousSum_ECropCycle()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_ECropCycle = PreviousSum%ECropCycle
end function GetPreviousSum_ECropCycle

real(dp) function GetPreviousSum_CRwater()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_CRwater = PreviousSum%CRwater
end function GetPreviousSum_CRwater

real(dp) function GetPreviousSum_Biomass()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_Biomass = PreviousSum%Biomass
end function GetPreviousSum_Biomass

real(dp) function GetPreviousSum_YieldPart()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_YieldPart = PreviousSum%YieldPart
end function GetPreviousSum_YieldPart

real(dp) function GetPreviousSum_BiomassPot()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_BiomassPot = PreviousSum%BiomassPot
end function GetPreviousSum_BiomassPot

real(dp) function GetPreviousSum_BiomassUnlim()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_BiomassUnlim = PreviousSum%BiomassUnlim
end function GetPreviousSum_BiomassUnlim

real(dp) function GetPreviousSum_BiomassTot()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_BiomassTot = PreviousSum%BiomassTot
end function GetPreviousSum_BiomassTot

real(dp) function GetPreviousSum_SaltIn()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_SaltIn = PreviousSum%SaltIn
end function GetPreviousSum_SaltIn

real(dp) function GetPreviousSum_SaltOut()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_SaltOut = PreviousSum%SaltOut
end function GetPreviousSum_SaltOut

real(dp) function GetPreviousSum_CRSalt()
    !! Getter for the "PreviousSum" global variable.

    GetPreviousSum_CRSalt = PreviousSum%CRSalt
end function GetPreviousSum_CRSalt

subroutine SetPreviousSum(PreviousSum_in)
    !! Setter for the "PreviousSum" global variable.
    type(rep_sum), intent(in) :: PreviousSum_in

    PreviousSum = PreviousSum_in
end subroutine SetPreviousSum

subroutine SetPreviousSum_Epot(Epot)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Epot

    PreviousSum%Epot = Epot
end subroutine SetPreviousSum_Epot

subroutine SetPreviousSum_Tpot(Tpot)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Tpot

    PreviousSum%Tpot = Tpot
end subroutine SetPreviousSum_Tpot

subroutine SetPreviousSum_Rain(Rain)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Rain

    PreviousSum%Rain = Rain
end subroutine SetPreviousSum_Rain

subroutine SetPreviousSum_Irrigation(Irrigation)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Irrigation

    PreviousSum%Irrigation = Irrigation
end subroutine SetPreviousSum_Irrigation

subroutine SetPreviousSum_Infiltrated(Infiltrated)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Infiltrated

    PreviousSum%Infiltrated = Infiltrated
end subroutine SetPreviousSum_Infiltrated

subroutine SetPreviousSum_Runoff(Runoff)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Runoff

    PreviousSum%Runoff = Runoff
end subroutine SetPreviousSum_Runoff

subroutine SetPreviousSum_Drain(Drain)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Drain

    PreviousSum%Drain = Drain
end subroutine SetPreviousSum_Drain

subroutine SetPreviousSum_Eact(Eact)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Eact

    PreviousSum%Eact = Eact
end subroutine SetPreviousSum_Eact

subroutine SetPreviousSum_Tact(Tact)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Tact

    PreviousSum%Tact = Tact
end subroutine SetPreviousSum_Tact

subroutine SetPreviousSum_TrW(TrW)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: TrW

    PreviousSum%TrW = TrW
end subroutine SetPreviousSum_TrW

subroutine SetPreviousSum_ECropCycle(ECropCycle)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: ECropCycle

    PreviousSum%ECropCycle = ECropCycle
end subroutine SetPreviousSum_ECropCycle

subroutine SetPreviousSum_CRwater(CRwater)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: CRwater

    PreviousSum%CRwater = CRwater
end subroutine SetPreviousSum_CRwater

subroutine SetPreviousSum_Biomass(Biomass)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: Biomass

    PreviousSum%Biomass = Biomass
end subroutine SetPreviousSum_Biomass

subroutine SetPreviousSum_YieldPart(YieldPart)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: YieldPart

    PreviousSum%YieldPart = YieldPart
end subroutine SetPreviousSum_YieldPart

subroutine SetPreviousSum_BiomassPot(BiomassPot)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: BiomassPot

    PreviousSum%BiomassPot = BiomassPot
end subroutine SetPreviousSum_BiomassPot

subroutine SetPreviousSum_BiomassUnlim(BiomassUnlim)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: BiomassUnlim

    PreviousSum%BiomassUnlim = BiomassUnlim
end subroutine SetPreviousSum_BiomassUnlim

subroutine SetPreviousSum_BiomassTot(BiomassTot)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: BiomassTot

    PreviousSum%BiomassTot = BiomassTot
end subroutine SetPreviousSum_BiomassTot

subroutine SetPreviousSum_SaltIn(SaltIn)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: SaltIn

    PreviousSum%SaltIn = SaltIn
end subroutine SetPreviousSum_SaltIn

subroutine SetPreviousSum_SaltOut(SaltOut)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: SaltOut

    PreviousSum%SaltOut = SaltOut
end subroutine SetPreviousSum_SaltOut

subroutine SetPreviousSum_CRSalt(CRSalt)
    !! Setter for the "PreviousSum" global variable.
    real(dp), intent(in) :: CRSalt

    PreviousSum%CRSalt = CRSalt
end subroutine SetPreviousSum_CRSalt

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

integer(int32) function GetDayNri()

    GetDayNri = DayNri
end function GetDayNri

subroutine SetDayNri(DayNri_in)
    integer(int32), intent(in) :: DayNri_in

    DayNri = DayNri_in
end subroutine SetDayNri


! EToDataSet

function GetEToDataSet() result(EToDataSet_out)
    !! Getter for the "EToDataSet" global variable.
    type(rep_DayEventDbl), dimension(31) :: EToDataSet_out

    EToDataSet_out = EToDataSet
end function GetEToDataSet

function GetEToDataSet_i(i) result(EToDataSet_i)
    !! Getter for individual elements of the "EToDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl) :: EToDataSet_i

    EToDataSet_i = EToDataSet(i)
end function GetEToDataSet_i

integer(int32) function GetEToDataSet_DayNr(i)
    integer(int32), intent(in) :: i

    GetEToDataSet_DayNr = EToDataSet(i)%DayNr
end function GetEToDataSet_DayNr

real(dp) function GetEToDataSet_Param(i)
    integer(int32), intent(in) :: i

    GetEToDataSet_Param = EToDataSet(i)%Param
end function GetEToDataSet_Param

subroutine SetEToDataSet(EToDataSet_in)
    !! Setter for the "EToDatSet" global variable.
    type(rep_DayEventDbl), dimension(31), intent(in) :: EToDataSet_in

    EToDataSet = EToDataSet_in
end subroutine SetEToDataSet

subroutine SetEToDataSet_i(i, EToDataSet_i)
    !! Setter for individual element for the "EToDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl), intent(in) :: EToDataSet_i

    EToDataSet(i) = EToDataSet_i
end subroutine SetEToDataSet_i

subroutine SetEToDataSet_DayNr(i, DayNr_in)
    integer(int32), intent(in) :: i
    integer(int32), intent(in) :: DayNr_in

    EToDataSet(i)%DayNr = DayNr_in
end subroutine SetEToDataSet_DayNr

subroutine SetEToDataSet_Param(i, Param_in)
    integer(int32), intent(in) :: i
    real(dp), intent(in) :: Param_in

    EToDataSet(i)%Param = Param_in
end subroutine SetEToDataSet_Param

! RainDataSet

function GetRainDataSet() result(RainDataSet_out)
    !! Getter for the "RainDataSet" global variable.
    type(rep_DayEventDbl), dimension(31) :: RainDataSet_out

    RainDataSet_out = RainDataSet
end function GetRainDataSet

function GetRainDataSet_i(i) result(RainDataSet_i)
    !! Getter for individual elements of the "RainDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl) :: RainDataSet_i

    RainDataSet_i = RainDataSet(i)
end function GetRainDataSet_i

integer(int32) function GetRainDataSet_DayNr(i)
    integer(int32), intent(in) :: i

    GetRainDataSet_DayNr = RainDataSet(i)%DayNr
end function GetRainDataSet_DayNr

real(dp) function GetRainDataSet_Param(i)
    integer(int32), intent(in) :: i

    GetRainDataSet_Param = RainDataSet(i)%Param
end function GetRainDataSet_Param

subroutine SetRainDataSet(RainDataSet_in)
    !! Setter for the "RainDatSet" global variable.
    type(rep_DayEventDbl), dimension(31), intent(in) :: RainDataSet_in

    RainDataSet = RainDataSet_in
end subroutine SetRainDataSet

subroutine SetRainDataSet_i(i, RainDataSet_i)
    !! Setter for individual element for the "RainDataSet" global variable.
    integer(int32), intent(in) :: i
    type(rep_DayEventDbl), intent(in) :: RainDataSet_i

    RainDataSet(i) = RainDataSet_i
end subroutine SetRainDataSet_i

subroutine SetRainDataSet_DayNr(i, DayNr_in)
    integer(int32), intent(in) :: i
    integer(int32), intent(in) :: DayNr_in

    RainDataSet(i)%DayNr = DayNr_in
end subroutine SetRainDataSet_DayNr

subroutine SetRainDataSet_Param(i, Param_in)
    integer(int32), intent(in) :: i
    real(dp), intent(in) :: Param_in

    RainDataSet(i)%Param = Param_in
end subroutine SetRainDataSet_Param

logical function GetGlobalIrriECw()
    !! Getter for the GlobalIrriECw global variable

    GetGlobalIrriECw = GlobalIrriECw
end function GetGlobalIrriECw

subroutine SetGlobalIrriECw(GlobalIrriECw_in)
    !! Setter for the GlobalIrriECw global variable
    logical, intent(in) :: GlobalIrriECw_in

    GlobalIrriECw = GlobalIrriECw_in
end subroutine SetGlobalIrriECw

integer(int32) function GetIrriInterval()
    !! Getter for the "IrriInterval" global variable.

    GetIrriInterval = IrriInterval
end function GetIrriInterval

subroutine SetIrriInterval(IrriInterval_in)
    !! Setter for the "IrriInterval" global variable.
    integer(int32), intent(in) :: IrriInterval_in

    IrriInterval = IrriInterval_in
end subroutine SetIrriInterval

integer(int32) function GetTadj()
    !! Getter for the "Tadj" global variable.

    GetTadj = Tadj
end function GetTadj

subroutine SetTadj(Tadj_in)
    !! Setter for the "Tadj" global variable. 
    integer(int32), intent(in) :: Tadj_in

    Tadj = Tadj_in 
end subroutine SetTadj

integer(int32) function GetGDDTadj()
    !! Getter for the "GDDTadj" global variable.

    GetGDDTadj = GDDTadj
end function GetGDDTadj

subroutine SetGDDTadj(GDDTadj_in)
    !! Setter for the "GDDTadj" global variable.
    integer(int32), intent(in) :: GDDTadj_in

    GDDTadj = GDDTadj_in
end subroutine SetGDDTadj

integer(int32) function GetDayLastCut()
    !! Getter for the "DayLastCut" global variable.

    GetDayLastCut = DayLastCut
end function GetDayLastCut

subroutine SetDayLastCut(DayLastCut_in)
    !! Setter for the "DayLastCut" global variable.
    integer(int32), intent(in) :: DayLastCut_in

    DayLastCut = DayLastCut_in
end subroutine SetDayLastCut

integer(int32) function GetNrCut()
    !! Getter for the "NrCut" global variable.

    GetNrCut = NrCut
end function GetNrCut

subroutine SetNrCut(NrCut_in)
    !! Setter for the "NrCut" global variable. 
    integer(int32), intent(in) :: NrCut_in

    NrCut = NrCut_in 
end subroutine SetNrCut

integer(int32) function GetSumInterval()
    !! Getter for the "SumInterval" global variable.

    GetSumInterval = SumInterval
end function GetSumInterval

subroutine SetSumInterval(SumInterval_in)
    !! Setter for the "SumInterval" global variable.
    integer(int32), intent(in) :: SumInterval_in

    SumInterval = SumInterval_in
end subroutine SetSumInterval

integer(int32) function GetPreviousStressLevel()
    !! Getter for the "PreviousStressLevel" global variable.

    GetPreviousStressLevel = PreviousStressLevel
end function GetPreviousStressLevel

subroutine SetPreviousStressLevel(PreviousStressLevel_in)
    !! Setter for the "PreviousStressLevel" global variable.
    integer(int32), intent(in) :: PreviousStressLevel_in

    PreviousStressLevel = PreviousStressLevel_in
end subroutine SetPreviousStressLevel

integer(int32) function GetStressSFadjNEW()
    !! Getter for the "StressSFadjNEW" global variable.

    GetStressSFadjNEW = StressSFadjNEW
end function GetStressSFadjNEW

subroutine SetStressSFadjNEW(StressSFadjNEW_in)
    !! Setter for the "StressSFadjNEW" global variable. 
    integer(int32), intent(in) :: StressSFadjNEW_in

    StressSFadjNEW = StressSFadjNEW_in 
end subroutine SetStressSFadjNEW

real(dp) function GetCCxWitheredTpot()
    !! Getter for the "CCxWitheredTpot" global variable.

    GetCCxWitheredTpot = CCxWitheredTpot
end function GetCCxWitheredTpot

subroutine SetCCxWitheredTpot(CCxWitheredTpot_in)
    !! Setter for the "CCxWitheredTpot" global variable.
    real(dp), intent(in) :: CCxWitheredTpot_in

    CCxWitheredTpot = CCxWitheredTpot_in
end subroutine SetCCxWitheredTpot

real(dp) function GetCCxWitheredTpotNoS()
    !! Getter for the "CCxWitheredTpotNoS" global variable.

    GetCCxWitheredTpotNoS = CCxWitheredTpotNoS
end function GetCCxWitheredTpotNoS

subroutine SetCCxWitheredTpotNoS(CCxWitheredTpotNoS_in)
    !! Setter for the "CCxWitheredTpotNoS" global variable.
    real(dp), intent(in) :: CCxWitheredTpotNoS_in

    CCxWitheredTpotNoS = CCxWitheredTpotNoS_in
end subroutine SetCCxWitheredTpotNoS

real(dp) function GetCoeffb0()
    !! Getter for the "Coeffb0" global variable.

    GetCoeffb0 = Coeffb0
end function GetCoeffb0

subroutine SetCoeffb0(Coeffb0_in)
    !! Setter for the "Coeffb0" global variable.  
    real(dp), intent(in) :: Coeffb0_in

    Coeffb0 = Coeffb0_in 
end subroutine SetCoeffb0

real(dp) function GetCoeffb1()
    !! Getter for the "Coeffb1" global variable.

    GetCoeffb1 = Coeffb1
end function GetCoeffb1

subroutine SetCoeffb1(Coeffb1_in)
    !! Setter for the "Coeffb1" global variable.  
    real(dp), intent(in) :: Coeffb1_in

    Coeffb1 = Coeffb1_in 
end subroutine SetCoeffb1

real(dp) function GetCoeffb2()
    !! Getter for the "Coeffb2" global variable.

    GetCoeffb2 = Coeffb2
end function GetCoeffb2

subroutine SetCoeffb2(Coeffb2_in)
    !! Setter for the "Coeffb2" global variable.  
    real(dp), intent(in) :: Coeffb2_in

    Coeffb2 = Coeffb2_in 
end subroutine SetCoeffb2

real(dp) function GetCoeffb0Salt()
    !! Getter for the "Coeffb0Salt" global variable.

    GetCoeffb0Salt = Coeffb0Salt
end function GetCoeffb0Salt

subroutine SetCoeffb0Salt(Coeffb0Salt_in)
    !! Setter for the "Coeffb0Salt" global variable.
    real(dp), intent(in) :: Coeffb0Salt_in

    Coeffb0Salt = Coeffb0Salt_in
end subroutine SetCoeffb0Salt

real(dp) function GetCoeffb1Salt()
    !! Getter for the "Coeffb1Salt" global variable.

    GetCoeffb1Salt = Coeffb1Salt
end function GetCoeffb1Salt

subroutine SetCoeffb1Salt(Coeffb1Salt_in)
    !! Setter for the "Coeffb1Salt" global variable.
    real(dp), intent(in) :: Coeffb1Salt_in

    Coeffb1Salt = Coeffb1Salt_in
end subroutine SetCoeffb1Salt

real(dp) function GetCoeffb2Salt()
    !! Getter for the "Coeffb2Salt" global variable.

    GetCoeffb2Salt = Coeffb2Salt
end function GetCoeffb2Salt

subroutine SetCoeffb2Salt(Coeffb2Salt_in)
    !! Setter for the "Coeffb2Salt" global variable.
    real(dp), intent(in) :: Coeffb2Salt_in

    Coeffb2Salt = Coeffb2Salt_in
end subroutine SetCoeffb2Salt

real(dp) function GetStressLeaf()
    !! Getter for the "StressLeaf" global variable.

    GetStressLeaf = StressLeaf
end function GetStressLeaf

subroutine SetStressLeaf(StressLeaf_in)
    !! Setter for the "StressLeaf" global variable.
    real(dp), intent(in) :: StressLeaf_in

    StressLeaf = StressLeaf_in
end subroutine SetStressLeaf

real(dp) function GetStressSenescence()
    !! Getter for the "StressSenescence" global variable.

    GetStressSenescence = StressSenescence
end function GetStressSenescence

subroutine SetStressSenescence(StressSenescence_in)
    !! Setter for the "StressSenescence" global variable.
    real(dp), intent(in) :: StressSenescence_in

    StressSenescence = StressSenescence_in
end subroutine SetStressSenescence

real(dp) function GetDayFraction()
    !! Getter for the "DayFraction" global variable.

    GetDayFraction = DayFraction
end function GetDayFraction

subroutine SetDayFraction(DayFraction_in)
    !! Setter for the "DayFraction" global variable.
    real(dp), intent(in) :: DayFraction_in

    DayFraction = DayFraction_in
end subroutine SetDayFraction

real(dp) function GetGDDayFraction()
    !! Getter for the "GDDayFraction" global variable.

    GetGDDayFraction = GDDayFraction
end function GetGDDayFraction

subroutine SetGDDayFraction(GDDayFraction_in)
    !! Setter for the "GDDayFraction" global variable.
    real(dp), intent(in) :: GDDayFraction_in

    GDDayFraction = GDDayFraction_in
end subroutine SetGDDayFraction

real(dp) function GetCGCref()
    !! Getter for the "CGCref" global variable.

    GetCGCref = CGCref
end function GetCGCref

subroutine SetCGCref(CGCref_in)
    !! Setter for the "CGCref" global variable.
    real(dp), intent(in) :: CGCref_in

    CGCref = CGCref_in
end subroutine SetCGCref

real(dp) function GetGDDCGCref()
    !! Getter for the "GDDCGCref" global variable.

    GetGDDCGCref = GDDCGCref
end function GetGDDCGCref

subroutine SetGDDCGCref(GDDCGCref_in)
    !! Setter for the "GDDCGCref" global variable.
    real(dp), intent(in) :: GDDCGCref_in

    GDDCGCref = GDDCGCref_in
end subroutine SetGDDCGCref


!! END section global variables


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

subroutine ResetPreviousSum(SumETo, SumGDD, PreviousSumETo, &
        PreviousSumGDD, PreviousBmob, PreviousBsto)
    real(dp), intent(inout) :: SumETo
    real(dp), intent(inout) :: SumGDD
    real(dp), intent(inout) :: PreviousSumETo
    real(dp), intent(inout) :: PreviousSumGDD
    real(dp), intent(inout) :: PreviousBmob
    real(dp), intent(inout) :: PreviousBsto

    call SetPreviousSum_Epot(0.0_dp)
    call SetPreviousSum_Tpot(0.0_dp)
    call SetPreviousSum_Rain(0.0_dp)
    call SetPreviousSum_Irrigation(0.0_dp)
    call SetPreviousSum_Infiltrated(0.0_dp)
    call SetPreviousSum_Runoff(0.0_dp)
    call SetPreviousSum_Drain(0.0_dp)
    call SetPreviousSum_Eact(0.0_dp)
    call SetPreviousSum_Tact(0.0_dp)
    call SetPreviousSum_TrW(0.0_dp)
    call SetPreviousSum_ECropCycle(0.0_dp)
    call SetPreviousSum_CRwater(0.0_dp)
    call SetPreviousSum_Biomass(0.0_dp)
    call SetPreviousSum_YieldPart(0.0_dp)
    call SetPreviousSum_BiomassPot(0.0_dp)
    call SetPreviousSum_BiomassUnlim(0.0_dp)
    call SetPreviousSum_SaltIn(0.0_dp)
    call SetPreviousSum_SaltOut(0.0_dp)
    call SetPreviousSum_CRsalt(0.0_dp)
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



subroutine GetSumGDDBeforeSimulation(SumGDDtillDay, SumGDDtillDayM1)
    real(dp), intent(inout) :: SumGDDtillDay
    real(dp), intent(inout) :: SumGDDtillDayM1

    character(len=:), allocatable :: totalname
    integer :: fTemp
    integer(int32) :: i
    character(len=255) :: StringREAD
    integer(int32) :: DayX
    real(dp) :: Tmin_temp, Tmax_temp
    type(rep_DayEventDbl), dimension(31) :: TmaxDataSet_temp, &
                                            TminDataSet_temp

    call SetSimulation_SumGDD(0._dp)
    if (GetTemperatureFile() /= '(None)') then
        totalname = GetTemperatureFilefull()

        if (FileExists(totalname)) then
            select case (GetTemperatureRecord_DataType())
            case (datatype_daily)
                open(newunit=fTemp, file=trim(totalname), status='old', &
                                                          action='read')
                read(fTemp, *) ! description
                read(fTemp, *) ! time step
                read(fTemp, *) ! day
                read(fTemp, *) ! month
                read(fTemp, *) ! year
                read(fTemp, *)
                read(fTemp, *)
                read(fTemp, *)
                ! days before first day of simulation (= DayNri)
                do i = GetTemperatureRecord_FromDayNr(), (DayNri - 1) 
                    if (i < GetCrop_Day1()) then
                        read(fTemp, *)
                    else
                        read(fTemp, '(a)') StringREAD
                        Tmin_temp = GetTmin()
                        Tmax_temp = GetTmax()
                        call SplitStringInTwoParams(StringREAD, Tmin_temp, Tmax_temp)
                        call SetTmin(Tmin_temp)
                        call SetTmax(Tmax_temp)
                        call SetSimulation_SumGDD(GetSimulation_SumGDD() &
                                + DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                             GetTmin(), GetTmax(), &
                                             GetSimulParam_GDDMethod()))
                    end if
                end do
                close(fTemp)

            case (datatype_decadely)
                DayX = GetCrop_Day1()
                ! first day of cropping
                TminDataSet_temp = GetTminDataSet()
                TmaxDataSet_temp = GetTmaxDataSet()
                call GetDecadeTemperatureDataSet(DayX, TminDataSet_temp, &
                                                 TmaxDataSet_temp)
                call SetTminDataSet(TminDataSet_temp)
                call SetTmaxDataSet(TmaxDataSet_temp)
                i = 1
                do while (GetTminDataSet_DayNr(i) /= DayX) 
                    i = i+1
                end do
                call SetTmin(GetTminDataSet_Param(i))
                call SetTmax(GetTmaxDataSet_Param(i))
                call SetSimulation_SumGDD(DegreesDay(GetCrop_Tbase(), &
                                GetCrop_Tupper(), GetTmin(), GetTmax(), &
                                GetSimulParam_GDDMethod()))
                ! next days
                do while (DayX < DayNri) 
                    DayX = DayX + 1
                    if (DayX > GetTminDataSet_DayNr(31)) then
                        TminDataSet_temp = GetTminDataSet()
                        TmaxDataSet_temp = GetTmaxDataSet()
                        call GetDecadeTemperatureDataSet(DayX, &
                                TminDataSet_temp, TmaxDataSet_temp)
                        call SetTminDataSet(TminDataSet_temp)
                        call SetTmaxDataSet(TmaxDataSet_temp)
                        i = 0
                    end if
                    i = i+1
                    call SetTmin(GetTminDataSet_Param(i))
                    call SetTmax(GetTmaxDataSet_Param(i))
                    call SetSimulation_SumGDD(GetSimulation_SumGDD() &
                                + DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                             GetTmin(), GetTmax(), &
                                             GetSimulParam_GDDMethod()))
                end do
            case (datatype_monthly)
                DayX = GetCrop_Day1()
                ! first day of cropping
                TminDataSet_temp = GetTminDataSet()
                TmaxDataSet_temp = GetTmaxDataSet()
                call GetMonthlyTemperatureDataSet(DayX, TminDataSet_temp, &
                                                  TmaxDataSet_temp)
                call SetTminDataSet(TminDataSet_temp)
                call SetTmaxDataSet(TmaxDataSet_temp)
                i = 1
                do while (GetTminDataSet_DayNr(i) /= DayX) 
                    i = i+1
                end do
                call SetTmin(GetTminDataSet_Param(i))
                call SetTmax(GetTmaxDataSet_Param(i))
                call SetSimulation_SumGDD(&
                        DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                   GetTmin(), GetTmax(), &
                                   GetSimulParam_GDDMethod()))
                ! next days
                do while (DayX < DayNri) 
                    DayX = DayX + 1
                    if (DayX > GetTminDataSet_DayNr(31)) then
                        TminDataSet_temp = GetTminDataSet()
                        TmaxDataSet_temp = GetTmaxDataSet()
                        call GetMonthlyTemperatureDataSet(&
                                DayX, TminDataSet_temp, TmaxDataSet_temp)
                        call SetTminDataSet(TminDataSet_temp)
                        call SetTmaxDataSet(TmaxDataSet_temp)
                        i = 0
                    end if
                    i = i+1
                    call SetTmin(GetTminDataSet_Param(i))
                    call SetTmax(GetTmaxDataSet_Param(i))
                    call SetSimulation_SumGDD(GetSimulation_SumGDD() &
                            + DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                         GetTmin(), GetTmax(), &
                                         GetSimulParam_GDDMethod()))
                end do
            end select
        end if
    end if
    if (GetTemperatureFile() == '(None)') then
        call SetSimulation_SumGDD(DegreesDay(&
                                 GetCrop_Tbase(), GetCrop_Tupper(), &
                                 GetSimulParam_Tmin(), GetSimulParam_Tmax(), &
                                 GetSimulParam_GDDMethod()) &
                                 * (DayNri - GetCrop_Day1() + 1))
        if (GetSimulation_SumGDD() < 0._dp) then
            call SetSimulation_SumGDD(0._dp)
        end if
        SumGDDtillDay = GetSimulation_SumGDD()
        SumGDDtillDayM1 = DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                     GetSimulParam_Tmin(), GetSimulParam_Tmax(), &
                                     GetSimulParam_GDDMethod()) &
                          * (DayNri - GetCrop_Day1())
        if (SumGDDtillDayM1 < 0._dp) then
            SumGDDtillDayM1 = 0._dp
        end if
    else
        SumGDDtillDay = GetSimulation_SumGDD()
        SumGDDtillDayM1 = SumGDDtillDay &
                         - DegreesDay(GetCrop_Tbase(), GetCrop_Tupper(), &
                                      GetTmin(), GetTmax(), &
                                      GetSimulParam_GDDMethod())
    end if
end subroutine GetSumGDDBeforeSimulation 




subroutine RelationshipsForFertilityAndSaltStress()

    real(dp) :: Coeffb0_temp
    real(dp) :: Coeffb1_temp
    real(dp) :: Coeffb2_temp
    real(dp) :: Coeffb0Salt_temp
    real(dp) :: Coeffb1Salt_temp
    real(dp) :: Coeffb2Salt_temp

    real(dp) :: X10, X20, X30, X40, X50, X60, X70, X80, X90
    integer(int8) :: BioTop, BioLow
    real(dp) :: StrTop, StrLow

    ! 1. Soil fertility
    call SetFracBiomassPotSF(1._dp)

    ! 1.a Soil fertility (Coeffb0,Coeffb1,Coeffb2 : Biomass-Soil Fertility stress)
    if (GetCrop_StressResponse_Calibrated()) then
        call StressBiomassRelationship(GetCrop_DaysToCCini(), GetCrop_GDDaysToCCini(), &
                                  GetCrop_DaysToGermination(), &
                                  GetCrop_DaysToFullCanopy(), &
                                  GetCrop_DaysToSenescence(), &
                                  GetCrop_DaysToHarvest(), &
                                  GetCrop_DaysToFlowering(), &
                                  GetCrop_LengthFlowering(), &
                                  GetCrop_GDDaysToGermination(), &
                                  GetCrop_GDDaysToFullCanopy(), &
                                  GetCrop_GDDaysToSenescence(), &
                                  GetCrop_GDDaysToHarvest(), &
                                  GetCrop_WPy(), GetCrop_HI(), &
                                  GetCrop_CCo(), GetCrop_CCx(), &
                                  GetCrop_CGC(), GetCrop_GDDCGC(), &
                                  GetCrop_CDC(), GetCrop_GDDCDC(), &
                                  GetCrop_KcTop(), GetCrop_KcDecline(), &
                                  real(GetCrop_CCEffectEvapLate(), kind= dp), &
                                  GetCrop_Tbase(), &
                                  GetCrop_Tupper(), GetSimulParam_Tmin(), &
                                  GetSimulParam_Tmax(), GetCrop_GDtranspLow(), &
                                  GetCrop_WP(), GetCrop_dHIdt(), GetCO2i(), &
                                  GetCrop_Day1(), GetCrop_DeterminancyLinked(), &
                                  GetCrop_StressResponse(),GetCrop_subkind(), &
                                  GetCrop_ModeCycle(), Coeffb0_temp, Coeffb1_temp, &
                                  Coeffb2_temp, X10, X20, X30, X40, X50, X60, X70)
        call SetCoeffb0(Coeffb0_temp)
        call SetCoeffb1(Coeffb1_temp)
        call SetCoeffb2(Coeffb2_temp)
    else
        call SetCoeffb0(real(undef_int, kind=dp))
        call SetCoeffb1(real(undef_int, kind=dp))
        call SetCoeffb2(real(undef_int, kind=dp))
    end if

    ! 1.b Soil fertility : FracBiomassPotSF
    if ((GetManagement_FertilityStress() /= 0._dp) .and. &
                                     GetCrop_StressResponse_Calibrated()) then
        BioLow = 100_int8
        StrLow = 0._dp
        loop: do
            BioTop = BioLow
            StrTop = StrLow
            BioLow = BioLow - 1_int8
            StrLow = GetCoeffb0() + GetCoeffb1()*BioLow + GetCoeffb2()*BioLow*BioLow
            if (((StrLow >= GetManagement_FertilityStress()) &
                         .or. (BioLow <= 0) .or. (StrLow >= 99.99_dp))) exit loop
        end do loop
        if (StrLow >= 99.99_dp) then
            StrLow = 100._dp
        end if
        if (abs(StrLow-StrTop) < 0.001_dp) then
            call SetFracBiomassPotSF(real(BioTop, kind=dp))
        else
            call SetFracBiomassPotSF(real(BioTop, kind=dp) - (GetManagement_FertilityStress() &
                                                    - StrTop)/(StrLow-StrTop))
        end if
    call SetFracBiomassPotSF(GetFracBiomassPotSF()/100._dp)
    end if

    ! 2. soil salinity (Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : CCx/KsSto - Salt stress)
    if (GetSimulation_SalinityConsidered() .eqv. .true.) then
        call CCxSaltStressRelationship(GetCrop_DaysToCCini(), &
                                  GetCrop_GDDaysToCCini(), &
                                  GetCrop_DaysToGermination(), &
                                  GetCrop_DaysToFullCanopy(), &
                                  GetCrop_DaysToSenescence(), &
                                  GetCrop_DaysToHarvest(), &
                                  GetCrop_DaysToFlowering(), & 
                                  GetCrop_LengthFlowering(), &
                                  GetCrop_GDDaysToFlowering(), & 
                                  GetCrop_GDDLengthFlowering(), &
                                  GetCrop_GDDaysToGermination(), &
                                  GetCrop_GDDaysToFullCanopy(), &
                                  GetCrop_GDDaysToSenescence(), &
                                  GetCrop_GDDaysToHarvest(), &
                                  GetCrop_WPy(), GetCrop_HI(), &
                                  GetCrop_CCo(), GetCrop_CCx(), &
                                  GetCrop_CGC(), GetCrop_GDDCGC(), &
                                  GetCrop_CDC(), GetCrop_GDDCDC(), &
                                  GetCrop_KcTop(), GetCrop_KcDecline(), & 
                                  real(GetCrop_CCEffectEvapLate(), kind=dp),  &
                                  GetCrop_Tbase(), GetCrop_Tupper(), &
                                  GetSimulParam_Tmin(), GetSimulParam_Tmax(), &
                                  GetCrop_GDtranspLow(), GetCrop_WP(), &
                                  GetCrop_dHIdt(), GetCO2i(), GetCrop_Day1(), &
                                  GetCrop_DeterminancyLinked(), &
                                  GetCrop_subkind(), GetCrop_ModeCycle(), &
                                  GetCrop_CCsaltDistortion(),Coeffb0Salt_temp, &
                                  Coeffb1Salt_temp, Coeffb2Salt_temp, X10, X20, X30, &
                                  X40, X50, X60, X70, X80, X90)
        call SetCoeffb0Salt(Coeffb0Salt_temp)
        call SetCoeffb1Salt(Coeffb1Salt_temp)
        call SetCoeffb2Salt(Coeffb2Salt_temp) 
    else
        call SetCoeffb0Salt(real(undef_int, kind=dp))
        call SetCoeffb1Salt(real(undef_int, kind=dp))
        call SetCoeffb2Salt(real(undef_int, kind=dp))
    end if
end subroutine RelationshipsForFertilityAndSaltStress


! extra for output of daily results  -----------------------------
subroutine DetermineGrowthStage(Dayi, CCiPrev, Code)
    integer(int32), intent(in) :: Dayi
    real(dp), intent(in) :: CCiPrev
    integer(int8), intent(inout)  :: Code

    integer(int32) :: VirtualDay

    VirtualDay = Dayi - GetSimulation_DelayedDays() - GetCrop_Day1()
    if (VirtualDay < 0) then
        Code = 0_int8 ! before cropping period
    else
        if (VirtualDay < GetCrop_DaysToGermination()) then
            Code = 1_int8 ! sown --> emergence OR transplant recovering
        else
            Code = 2_int8 ! vegetative development
            if ((GetCrop_subkind() == subkind_Grain) .and. &
                (VirtualDay >= GetCrop_DaysToFlowering())) then
                if (VirtualDay < (GetCrop_DaysToFlowering() + &
                                  GetCrop_LengthFlowering())) then
                    Code = 3_int8 ! flowering
                else
                    Code = 4_int8 ! yield formation
                end if
            end if
            if ((GetCrop_subkind() == subkind_Tuber) .and. &
                (VirtualDay >= GetCrop_DaysToFlowering())) then
                Code = 4_int8 ! yield formation
            end if
            if ((VirtualDay > GetCrop_DaysToGermination()) .and.&
                (CCiPrev < epsilon(0._dp))) then
                Code = int(undef_int, kind=int8)  ! no growth stage
            end if
            if (VirtualDay >= &
                (GetCrop_Length_i(1)+GetCrop_Length_i(2)+ &
                 GetCrop_Length_i(3)+GetCrop_Length_i(4))) then
                Code = 0_int8 ! after cropping period
            end if
        end if
    end if
end subroutine DetermineGrowthStage


subroutine OpenIrrigationFile()

    character(len=:), allocatable :: totalname
    character(len=255) :: StringREAD
    integer(int32) :: i, DNr
    real(dp) :: Ir1, Ir2
    real(dp) :: VersionNr
    integer(int32) :: FromDay_temp, TimeInfo_temp, DepthInfo_temp
    real(dp) :: IrriECw_temp
    character(len=1025) :: TempString

    if ((GetIrriMode() == IrriMode_Manual) &
        .or. (GetIrriMode() == IrriMode_Generate)) then
        if (GetIrriFile() /= '(None)') then
            totalname = GetIrriFileFull()
        else
            totalname = GetPathNameProg() // 'IrriSchedule.AqC'
        end if
        call fIrri_open(totalname, 'r')
        TempString = fIrri_read() ! description
        TempString = fIrri_read() ! AquaCrop version
        read(TempString, *) VersionNr
        
        if (roundc(VersionNr*10, mold=1) < 32) then
            call SetGlobalIrriECw(.true.)
        else
            call SetGlobalIrriECw(.false.)
        end if
        do i = 1, 6 
            TempString = fIrri_read()  ! irrigation info (already loaded)
        end do
        select case (GetIrriMode())
        case (IrriMode_Manual)
            if (GetIrriFirstDayNr() == undef_int) then
                DNr = GetDayNri() - GetCrop_Day1() + 1
            else
                DNr = GetDayNri() - GetIrriFirstDayNr() + 1
            end if
            loop: do
                StringREAD = fIrri_read()
                if (fIrri_eof()) then
                    call SetIrriInfoRecord1_NoMoreInfo(.true.)
                else
                    call SetIrriInfoRecord1_NoMoreInfo(.false.)
                    if (GetGlobalIrriECw()) then
                        call SplitStringInTwoParams(StringREAD, Ir1, Ir2)
                    else
                        IrriECw_temp = GetSimulation_IrriECw()
                        call SplitStringInThreeParams(StringREAD, Ir1, Ir2, &
                                                      IrriECw_temp)
                        call SetSimulation_IrriECw(IrriECw_temp)
                    end if
                    call SetIrriInfoRecord1_TimeInfo(roundc(Ir1, mold=1))
                    call SetIrriInfoRecord1_DepthInfo(roundc(Ir2, mold=1))
                end if
                if ((GetIrriInfoRecord1_NoMoreInfo()) &
                    .or. (GetIrriInfoRecord1_TimeInfo() >= DNr)) exit loop
            end do loop
        case(IrriMode_Generate)
            do i = 1, 2 
                TempString = fIrri_read() 
                ! time and depth criterion (already loaded)
            end do
            call SetIrriInfoRecord1_NoMoreInfo(.false.)
            if (roundc(VersionNr*10, mold=1) < 32) then
                TempString = fIrri_read()
                read(TempString, *) FromDay_temp, TimeInfo_temp, &
                                    DepthInfo_temp
                call SetIrriInfoRecord1_FromDay(FromDay_temp)
                call SetIrriInfoRecord1_TimeInfo(TimeInfo_temp)
                call SetIrriInfoRecord1_DepthInfo(DepthInfo_temp)
            else
                TempString = fIrri_read()
                read(TempString, *) FromDay_temp, TimeInfo_temp, &
                                    DepthInfo_temp, IrriECw_temp
                call SetIrriInfoRecord1_FromDay(FromDay_temp)
                call SetIrriInfoRecord1_TimeInfo(TimeInfo_temp)
                call SetIrriInfoRecord1_DepthInfo(DepthInfo_temp)
                call SetSimulation_IrriECw(IrriECw_temp)
            end if
            
            TempString = fIrri_read()
            if (fIrri_eof()) then
                call SetIrriInfoRecord1_ToDay(GetCrop_DayN() &
                                              - GetCrop_Day1() + 1)
            else
                call SetIrriInfoRecord2_NoMoreInfo(.false.)
                if (GetGlobalIrriECw()) then
                    read(TempString, *) FromDay_temp, TimeInfo_temp, &
                                        DepthInfo_temp
                    call SetIrriInfoRecord2_FromDay(FromDay_temp)
                    call SetIrriInfoRecord2_TimeInfo(TimeInfo_temp)
                    call SetIrriInfoRecord2_DepthInfo(DepthInfo_temp)
                else
                    read(TempString, *) FromDay_temp, TimeInfo_temp, &
                                        DepthInfo_temp, IrriEcw_temp
                    call SetIrriInfoRecord2_FromDay(FromDay_temp)
                    call SetIrriInfoRecord2_TimeInfo(TimeInfo_temp)
                    call SetIrriInfoRecord2_DepthInfo(DepthInfo_temp)
                    call SetSimulation_IrriECw(IrriECw_temp)
                end if
                call SetIrriInfoRecord1_ToDay(GetIrriInfoRecord2_FromDay() - 1)
            end if
        end select
    end if
end subroutine OpenIrrigationFile


subroutine OpenOutputRun(TheProjectType)
    integer(intEnum), intent(in) :: TheProjectType

    character(len=:), allocatable :: totalname
    character(len=1025) :: tempstring
    integer, dimension(8) :: d

    select case (TheProjectType)
    case(typeproject_TypePRO)
        totalname = GetPathNameOutp() // GetOutputName() // 'PROseason.OUT'
    case(typeproject_TypePRM)
        totalname = GetPathNameOutp() // GetOutputName() // 'PRMseason.OUT'
    end select
    call fRun_open(totalname, 'w')
    call date_and_time(values=d)
    write(tempstring, '(a, i2, a, i2, a, i4, a, i2, a, i2, a, i2)') &
    'AquaCrop 7.0 (October 2021) - Output created on (date) : ', d(3), '-', d(2), &
    '-', d(1), '   at (time) : ', d(5), ':', d(6), ':', d(7)
    call fRun_write(trim(tempstring))
    call fRun_write('')
    call fRun_write('    RunNr     Day1   Month1    Year1     Rain      ETo       GD     CO2' // &
    '      Irri   Infilt   Runoff    Drain   Upflow        E     E/Ex       Tr      TrW   Tr/Trx' // &
    '    SaltIn   SaltOut    SaltUp  SaltProf' // &
    '     Cycle   SaltStr  FertStr  WeedStr  TempStr   ExpStr   StoStr' // &
    '  BioMass  Brelative   HI    Y(dry)  Y(fresh)    WPet      Bin     Bout     DayN   MonthN    YearN')
    call fRun_write('                                           mm       mm  degC.day    ppm' // &
    '        mm       mm       mm       mm       mm       mm        %       mm       mm        %' // &
    '    ton/ha    ton/ha    ton/ha    ton/ha' // &
    '      days       %        %        %        %        %        %  ' // &
    '  ton/ha        %       %    ton/ha   ton/ha    kg/m3   ton/ha   ton/ha')
end subroutine OpenOutputRun


subroutine OpenOutputDaily(TheProjectType)
    integer(intEnum), intent(in) :: TheProjectType

    character(len=:), allocatable :: totalname
    character(len=1025) :: tempstring
    integer, dimension(8) :: d

    select case (TheProjectType)
    case(typeproject_TypePRO)
        totalname = GetPathNameOutp() // GetOutputName() // 'PROday.OUT'
    case(typeproject_TypePRM)
        totalname = GetPathNameOutp() // GetOutputName() // 'PRMday.OUT'
    end select
    call date_and_time(values=d)
    call fDaily_open(totalname, 'w')
    write(tempstring, '(a, i2, a, i2, a, i4, a, i2, a, i2, a, i2)') &
    'AquaCrop 7.0 (October 2021) - Output created on (date) : ', d(3), '-', d(2), &
    '-', d(1), '   at (time) : ', d(5), ':', d(6), ':', d(7)
    call fDaily_write(trim(tempstring))
end subroutine OpenOutputDaily


end module ac_run
