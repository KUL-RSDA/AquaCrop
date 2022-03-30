module ac_interface_simul
use ac_kinds, only: dp, &
                    int32
use ac_simul, only: AdjustpSenescenceToETo, &
                    BUDGET_module, &
                    DetermineCCi, &
                    DetermineCCiGDD, &
                    ExtractWaterFromEvapLayer


implicit none


contains


subroutine AdjustpSenescenceToETo_wrap(EToMean, TimeSenescence, WithBeta, pSenAct)
    real(dp), intent(in) :: EToMean
    real(dp), intent(in) :: TimeSenescence
    logical(1), intent(in) :: WithBeta
    real(dp), intent(inout) :: pSenAct

    logical :: WithBeta_f

    WithBeta_f = WithBeta
    call AdjustpSenescenceToETo(EToMean, TimeSenescence, WithBeta_f, pSenAct)
end subroutine AdjustpSenescenceToETo_wrap


subroutine DetermineCCiGDD_wrap(CCxTotal, CCoTotal, &
                           StressLeaf, FracAssim, MobilizationON, &
                           StorageON, SumGDDAdjCC, VirtualTimeCC, &
                           StressSenescence, TimeSenescence, NoMoreCrop, &
                           CDCTotal, CGCAdjustmentAfterCutting, GDDayFraction, &
                           GDDayi, GDDCDCTotal, GDDTadj)
    real(dp), intent(in) :: CCxTotal
    real(dp), intent(in) :: CCoTotal
    real(dp), intent(inout) :: StressLeaf
    real(dp), intent(in) :: FracAssim
    logical(1), intent(in) :: MobilizationON
    logical(1), intent(in) :: StorageON
    real(dp), intent(in) :: SumGDDAdjCC
    integer(int32), intent(in) :: VirtualTimeCC
    real(dp), intent(inout) :: StressSenescence
    real(dp), intent(inout) :: TimeSenescence
    logical(1), intent(inout) :: NoMoreCrop
    real(dp), intent(in) :: CDCTotal
    logical(1), intent(inout) :: CGCAdjustmentAfterCutting
    real(dp), intent(in) :: GDDayFraction
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: GDDCDCTotal
    integer(int32), intent(in) :: GDDTadj

    logical :: MobilizationON_f, StorageON_f, NoMoreCrop_f, &
               CGCAdjustmentAfterCutting_f

    MobilizationON_f = MobilizationON
    StorageON_f = StorageON
    NoMoreCrop_f = NoMoreCrop
    CGCAdjustmentAfterCutting_f = CGCAdjustmentAfterCutting

    call DetermineCCiGDD(CCxTotal, CCoTotal, &
                           StressLeaf, FracAssim, MobilizationON_f, &
                           StorageON_f, SumGDDAdjCC, VirtualTimeCC, &
                           StressSenescence, TimeSenescence, NoMoreCrop_f, &
                           CDCTotal, CGCAdjustmentAfterCutting_f, GDDayFraction, &
                           GDDayi, GDDCDCTotal, GDDTadj)
    NoMoreCrop = NoMoreCrop_f
    CGCAdjustmentAfterCutting = CGCAdjustmentAfterCutting_f
end subroutine DetermineCCiGDD_wrap


subroutine ExtractWaterFromEvapLayer_wrap(EvapToLose, Zact, Stg1)
    real(dp), intent(in) :: EvapToLose
    real(dp), intent(in) :: Zact
    logical(1), intent(in) :: Stg1

    logical :: Stg1_f

    Stg1_f = Stg1

    call ExtractWaterFromEvapLayer(EvapToLose, Zact, Stg1_f)
end subroutine ExtractWaterFromEvapLayer_wrap


subroutine DetermineCCi_wrap(CCxTotal, CCoTotal, StressLeaf, FracAssim, &
                        MobilizationON, StorageON, Tadj, VirtualTimeCC, &
                        StressSenescence, TimeSenescence, NoMoreCrop, &
                        CDCTotal, CGCAdjustmentAfterCutting, DayFraction, &
                        GDDCDCTotal, TESTVAL)
    real(dp), intent(in) :: CCxTotal
    real(dp), intent(in) :: CCoTotal
    real(dp), intent(inout) :: StressLeaf
    real(dp), intent(in) :: FracAssim
    logical, intent(in) :: MobilizationON
    logical, intent(in) :: StorageON
    integer(int32), intent(in) :: Tadj
    integer(int32), intent(in) :: VirtualTimeCC
    real(dp), intent(inout) :: StressSenescence
    real(dp), intent(inout) :: TimeSenescence
    logical, intent(inout) :: NoMoreCrop
    real(dp), intent(in) :: CDCTotal
    logical, intent(inout) :: CGCAdjustmentAfterCutting
    real(dp), intent(in) :: DayFraction
    real(dp), intent(in) :: GDDCDCTotal
    real(dp), intent(inout) :: TESTVAL

    logical :: MobilizationON_f, StorageON_f, NoMoreCrop_f, &
               CGCAdjustmentAfterCutting_f

    MobilizationON_f = MobilizationON
    StorageON_f = StorageON
    NoMoreCrop_f = NoMoreCrop
    CGCAdjustmentAfterCutting_f = CGCAdjustmentAfterCutting

    call DetermineCCi(CCxTotal, CCoTotal, StressLeaf, FracAssim, &
                        MobilizationON_f, StorageON_f, Tadj, VirtualTimeCC, &
                        StressSenescence, TimeSenescence, NoMoreCrop_f, &
                        CDCTotal, CGCAdjustmentAfterCutting_f, DayFraction, &
                        GDDCDCTotal, TESTVAL)
    NoMoreCrop = NoMoreCrop_f
    CGCAdjustmentAfterCutting = CGCAdjustmentAfterCutting_f
end subroutine DetermineCCi_wrap


subroutine BUDGET_module_wrap(dayi, TargetTimeVal, TargetDepthVal, VirtualTimeCC, &
                         SumInterval, DayLastCut, NrDayGrow, Tadj, GDDTadj, &
                         GDDayi, CGCref, GDDCGCref, CO2i, CCxTotal, CCoTotal, &
                         CDCTotal, GDDCDCTotal, SumGDDadjCC, Coeffb0Salt, &
                         Coeffb1Salt, Coeffb2Salt, StressTotSaltPrev, &
                         DayFraction, GDDayFraction, FracAssim, &
                         StressSFadjNEW, StorageON, MobilizationON, &
                         StressLeaf, StressSenescence, TimeSenescence, &
                         NoMoreCrop, CGCadjustmentAfterCutting, TESTVAL)
    integer(int32), intent(in) :: dayi
    integer(int32), intent(in) :: TargetTimeVal
    integer(int32), intent(in) :: TargetDepthVal
    integer(int32), intent(in) :: VirtualTimeCC
    integer(int32), intent(in) :: SumInterval
    integer(int32), intent(in) :: DayLastCut
    integer(int32), intent(in) :: NrDayGrow
    integer(int32), intent(in) :: Tadj
    integer(int32), intent(in) :: GDDTadj
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: CGCref
    real(dp), intent(in) :: GDDCGCref
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: CCxTotal
    real(dp), intent(in) :: CCoTotal
    real(dp), intent(in) :: CDCTotal
    real(dp), intent(in) :: GDDCDCTotal
    real(dp), intent(in) :: SumGDDadjCC
    real(dp), intent(in) :: Coeffb0Salt
    real(dp), intent(in) :: Coeffb1Salt
    real(dp), intent(in) :: Coeffb2Salt
    real(dp), intent(in) :: StressTotSaltPrev
    real(dp), intent(in) :: DayFraction
    real(dp), intent(in) :: GDDayFraction
    real(dp), intent(in) :: FracAssim
    integer(int8), intent(in) :: StressSFadjNEW
    logical(1), intent(in) :: StorageON
    logical(1), intent(in) :: MobilizationON
    real(dp), intent(inout) :: StressLeaf
    real(dp), intent(inout) :: StressSenescence
    real(dp), intent(inout) :: TimeSenescence
    logical(1), intent(inout) :: NoMoreCrop
    logical(1), intent(inout) :: CGCadjustmentAfterCutting
    real(dp), intent(inout) :: TESTVAL

    logical :: StorageON_f, MobilizationON_f, &
               NoMoreCrop_f, CGCadjustmentAfterCutting_f

    StorageON_f = StorageON
    MobilizationON_f = MobilizationON
    NoMoreCrop_f = NoMoreCrop
    CGCadjustmentAfterCutting_f = CGCadjustmentAfterCutting

    call BUDGET_module(dayi, TargetTimeVal, TargetDepthVal, VirtualTimeCC, &
                         SumInterval, DayLastCut, NrDayGrow, Tadj, GDDTadj, &
                         GDDayi, CGCref, GDDCGCref, CO2i, CCxTotal, CCoTotal, &
                         CDCTotal, GDDCDCTotal, SumGDDadjCC, Coeffb0Salt, &
                         Coeffb1Salt, Coeffb2Salt, StressTotSaltPrev, &
                         DayFraction, GDDayFraction, FracAssim, &
                         StressSFadjNEW, StorageON_f, MobilizationON_f, &
                         StressLeaf, StressSenescence, TimeSenescence, &
                         NoMoreCrop_f, CGCadjustmentAfterCutting_f, TESTVAL)

    NoMoreCrop = NoMoreCrop_f
    CGCadjustmentAfterCutting = CGCadjustmentAfterCutting_f
end subroutine BUDGET_module_wrap


end module ac_interface_simul
