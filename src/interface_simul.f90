module ac_interface_simul
use ac_kinds, only: dp, &
                    int32
use ac_simul, only: AdjustpSenescenceToETo, &
                    DetermineCCi, &
                    DetermineCCiGDD, &
                    ExtractWaterFromEvapLayer


use ac_simul, only: DetermineBiomassAndYield

use ac_kinds, only: dp, &
                    int32, &
                    int8

implicit none


contains

subroutine DetermineBiomassAndYield_wrap(dayi, ETo, TminOnDay, TmaxOnDay, CO2i, &
                                    GDDayi, Tact, SumKcTop, CGCref, GDDCGCref, &
                                    Coeffb0, Coeffb1, Coeffb2, FracBiomassPotSF, &
                                    Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, &
                                    AverageSaltStress, SumGDDadjCC, CCtot, &
                                    FracAssim, VirtualTimeCC, SumInterval, &
                                    Biomass, BiomassPot, BiomassUnlim, &
                                    BiomassTot, YieldPart, WPi, HItimesBEF, &
                                    ScorAT1, ScorAT2, HItimesAT1, HItimesAT2, &
                                    HItimesAT, alfa, alfaMax, SumKcTopStress, &
                                    SumKci, CCxWitheredTpot, CCxWitheredTpotNoS, &
                                    WeedRCi, CCw, Trw, StressSFadjNEW, &
                                    PreviousStressLevel, StoreAssimilates, &
                                    MobilizeAssimilates, AssimToMobilize, &
                                    AssimMobilized, Bin, Bout, TESTVAL)

    integer(int32), intent(in) :: dayi
    real(dp), intent(in) :: ETo
    real(dp), intent(in) :: TminOnDay
    real(dp), intent(in) :: TmaxOnDay
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: Tact
    real(dp), intent(in) :: SumKcTop
    real(dp), intent(in) :: CGCref
    real(dp), intent(in) :: GDDCGCref
    real(dp), intent(in) :: Coeffb0
    real(dp), intent(in) :: Coeffb1
    real(dp), intent(in) :: Coeffb2
    real(dp), intent(in) :: FracBiomassPotSF
    real(dp), intent(in) :: Coeffb0Salt
    real(dp), intent(in) :: Coeffb1Salt
    real(dp), intent(in) :: Coeffb2Salt
    real(dp), intent(in) :: AverageSaltStress
    real(dp), intent(in) :: SumGDDadjCC
    real(dp), intent(in) :: CCtot
    real(dp), intent(inout) :: FracAssim
    integer(int32), intent(in) :: VirtualTimeCC
    integer(int32), intent(in) :: SumInterval
    real(dp), intent(inout) :: Biomass
    real(dp), intent(inout) :: BiomassPot
    real(dp), intent(inout) :: BiomassUnlim
    real(dp), intent(inout) :: BiomassTot
    real(dp), intent(inout) :: YieldPart
    real(dp), intent(inout) :: WPi
    real(dp), intent(inout) :: HItimesBEF
    real(dp), intent(inout) :: ScorAT1
    real(dp), intent(inout) :: ScorAT2
    real(dp), intent(inout) :: HItimesAT1
    real(dp), intent(inout) :: HItimesAT2
    real(dp), intent(inout) :: HItimesAT
    real(dp), intent(inout) :: alfa
    real(dp), intent(inout) :: alfaMax
    real(dp), intent(inout) :: SumKcTopStress
    real(dp), intent(inout) :: SumKci
    real(dp), intent(inout) :: CCxWitheredTpot
    real(dp), intent(inout) :: CCxWitheredTpotNoS
    real(dp), intent(inout) :: WeedRCi
    real(dp), intent(inout) :: CCw
    real(dp), intent(inout) :: Trw
    integer(int8), intent(inout) :: StressSFadjNEW
    integer(int8), intent(inout) :: PreviousStressLevel
    logical, intent(inout) :: StoreAssimilates
    logical, intent(inout) :: MobilizeAssimilates
    real(dp), intent(inout) :: AssimToMobilize
    real(dp), intent(inout) :: AssimMobilized
    real(dp), intent(inout) :: Bin
    real(dp), intent(inout) :: Bout
    real(dp), intent(inout) :: TESTVAL

    logical :: StoreAssimilates_f
    logical :: MobilizeAssimilates_f

    StoreAssimilates_f = StoreAssimilates
    MobilizeAssimilates_f = MobilizeAssimilates

    call DetermineBiomassAndYield(dayi, ETo, TminOnDay, TmaxOnDay, CO2i, &
                                    GDDayi, Tact, SumKcTop, CGCref, GDDCGCref, &
                                    Coeffb0, Coeffb1, Coeffb2, FracBiomassPotSF, &
                                    Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, &
                                    AverageSaltStress, SumGDDadjCC, CCtot, &
                                    FracAssim, VirtualTimeCC, SumInterval, &
                                    Biomass, BiomassPot, BiomassUnlim, &
                                    BiomassTot, YieldPart, WPi, HItimesBEF, &
                                    ScorAT1, ScorAT2, HItimesAT1, HItimesAT2, &
                                    HItimesAT, alfa, alfaMax, SumKcTopStress, &
                                    SumKci, CCxWitheredTpot, CCxWitheredTpotNoS, &
                                    WeedRCi, CCw, Trw, StressSFadjNEW, &
                                    PreviousStressLevel, StoreAssimilates_f, &
                                    MobilizeAssimilates_f, AssimToMobilize, &
                                    AssimMobilized, Bin, Bout, TESTVAL)

    StoreAssimilates = StoreAssimilates_f
    MobilizeAssimilates = MobilizeAssimilates_f
end subroutine DetermineBiomassAndYield_wrap


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


end module ac_interface_simul
