module ac_interface_simul

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
end subroutine DetermineBiomassAndYield_wrap


end module ac_interface_simul
