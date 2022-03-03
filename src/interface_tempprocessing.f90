module ac_interface_tempprocessing

use ac_kinds,  only: dp, &
                     int8, &
                     int16, &
                     int32, &
                     intEnum

use ac_tempprocessing, only: Bnormalized

implicit none


contains


real(dp) function Bnormalized_wrap(&
            TheDaysToCCini,TheGDDaysToCCini,&
            L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,&
            GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,&
            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,&
            KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,&
            Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,&
            StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress,&
            DeltaWeedStress,&
            StrResCDecline,ShapeFweed,&
            TheModeCycle,&
            FertilityStressOn,&
            TestRecord)
    !! Wrapper for the booleans
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    integer(int32), intent(in) :: WPyield
    integer(int32), intent(in) :: DaysYieldFormation
    integer(int32), intent(in) :: tSwitch
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: WPbio
    real(dp), intent(in) :: TheCO2
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: RatDGDD
    real(dp), intent(in) :: SumKcTop
    integer(int8), intent(in) :: StressInPercent
    integer(int8), intent(in) :: StrResRedCGC
    integer(int8), intent(in) :: StrResRedCCx
    integer(int8), intent(in) :: StrResRedWP
    integer(int8), intent(in) :: StrResRedKsSto
    integer(int8), intent(in) :: WeedStress
    integer(int32), intent(in) :: DeltaWeedStress
    real(dp), intent(in) :: StrResCDecline
    real(dp), intent(in) :: ShapeFweed
    integer(intEnum), intent(in) :: TheModeCycle
    logical(1), intent(in) :: FertilityStressOn
    logical(1), intent(in) :: TestRecord
   
    logical :: bool_fertilitystresson, bool_testrecord

    bool_fertilitystresson = FertilityStressOn
    bool_testrecord = TestRecord 

    Bnormalized_wrap = Bnormalized(TheDaysToCCini,TheGDDaysToCCini,&
            L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,&
            GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,&
            CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,&
            KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,&
            Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,&
            StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress,&
            DeltaWeedStress,&
            StrResCDecline,ShapeFweed,&
            TheModeCycle,&
            bool_fertilitystresson,&
            bool_testrecord)
end function Bnormalized_wrap

end module ac_interface_tempprocessing
