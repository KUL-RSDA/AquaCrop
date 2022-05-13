module ac_interface_tempprocessing

use ac_global, only: rep_EffectStress, &
                     rep_Shapes
use ac_kinds,  only: dp, &
                     int8, &
                     int16, &
                     int32, &
                     intEnum
use ac_tempprocessing, only: Bnormalized, BiomassRatio,&
                             CCxSaltStressRelationship, &
                             LoadSimulationRunProject, &
                             StressBiomassRelationship
use ac_utils, only: pointer2string
use, intrinsic :: iso_c_binding, only: c_ptr
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

subroutine LoadSimulationRunProject_wrap(NameFileFull, strlen, NrRun)
    !! Wrapper
    type(c_ptr), intent(in) :: NameFileFull
    integer(int32), intent(in) :: strlen
    integer(int32), intent(in) :: NrRun

    character(len=strlen) :: string

    string = pointer2string(NameFileFull, strlen)
    call LoadSimulationRunProject(string, NrRun)
end subroutine LoadSimulationRunProject_wrap

real(dp) function BiomassRatio_wrap(TempDaysToCCini, TempGDDaysToCCini,&
           TempCCo, TempCGC, TempCCx, TempCDC, TempGDDCGC, &
           TempGDDCDC, TempdHIdt, TempL0, TempL12, L12SF,&
           TempL123, TempHarvest, TempFlower, TempGDDL0, &
           GDDL12SF, TempGDDL12, TempGDDL123, TempGDDHarvest,&
           TempHI, TempWPy, TempKc, TempKcDecline, TempCCeffect,&
           TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow,&
           TempWP, ShapeFweed, TempModeCycle, SFInfo, SFInfoStress,&
           WeedStress, DeltaWeedStress, DeterminantCropType, FertilityStressOn)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    real(dp), intent(in) :: TempCCo
    real(dp), intent(in) :: TempCGC
    real(dp), intent(in) :: TempCCx
    real(dp), intent(in) :: TempCDC
    real(dp), intent(in) :: TempGDDCGC
    real(dp), intent(in) :: TempGDDCDC
    real(dp), intent(in) :: TempdHIdt
    integer(int32), intent(in) :: TempL0
    integer(int32), intent(in) :: TempL12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: TempL123
    integer(int32), intent(in) :: TempHarvest
    integer(int32), intent(in) :: TempFlower
    integer(int32), intent(in) :: TempGDDL0
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: TempGDDL12
    integer(int32), intent(in) :: TempGDDL123
    integer(int32), intent(in) :: TempGDDHarvest
    integer(int32), intent(in) :: TempHI
    integer(int32), intent(in) :: TempWPy
    real(dp), intent(in) :: TempKc
    real(dp), intent(in) :: TempKcDecline
    real(dp), intent(in) :: TempCCeffect
    real(dp), intent(in) :: TempTbase
    real(dp), intent(in) :: TempTupper
    real(dp), intent(in) :: TempTmin
    real(dp), intent(in) :: TempTmax
    real(dp), intent(in) :: TempGDtranspLow
    real(dp), intent(in) :: TempWP
    real(dp), intent(in) :: ShapeFweed
    integer(intEnum), intent(in) :: TempModeCycle
    type(rep_EffectStress), intent(in) :: SFInfo
    integer(int8), intent(in) :: SFInfoStress
    integer(int8), intent(in) :: WeedStress
    integer(int32), intent(in) :: DeltaWeedStress
    logical(1), intent(in) :: DeterminantCropType
    logical(1), intent(in) :: FertilityStressOn

    logical :: bool_determinantcroptype,bool_fertilitystresson

    bool_determinantcroptype = DeterminantCropType
    bool_fertilitystresson = FertilityStressOn

    BiomassRatio_wrap = BiomassRatio(TempDaysToCCini, TempGDDaysToCCini,&
           TempCCo, TempCGC, TempCCx, TempCDC, TempGDDCGC, &
           TempGDDCDC, TempdHIdt, TempL0, TempL12, L12SF,&
           TempL123, TempHarvest, TempFlower, TempGDDL0, &
           GDDL12SF, TempGDDL12, TempGDDL123, TempGDDHarvest,&
           TempHI, TempWPy, TempKc, TempKcDecline, TempCCeffect,&
           TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow,&
           TempWP, ShapeFweed, TempModeCycle, SFInfo, SFInfoStress,&
           WeedStress, DeltaWeedStress, bool_determinantcroptype, bool_fertilitystresson)
end function BiomassRatio_wrap


subroutine StressBiomassRelationship_wrap(TheDaysToCCini, TheGDDaysToCCini,&
            L0, L12, L123, L1234, LFlor, LengthFlor, GDDL0, GDDL12,&
            GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC, GDDCGC,&
            CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent,&
            Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, WPveg, RatedHIdt,&
            CO2Given, CropDNr1, CropDeterm, CropSResp, TheCropType,&
            TheModeCycle, b0, b1, b2, &
            BM10, BM20, BM30, BM40, BM50, BM60, BM70)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: LengthFlor
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    integer(int32), intent(in) :: WPyield
    integer(int32), intent(in) :: RefHI
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: WPveg
    real(dp), intent(in) :: RatedHIdt
    real(dp), intent(in) :: CO2Given
    integer(int32), intent(in) :: CropDNr1
    logical(1), intent(in) :: CropDeterm
    type(rep_Shapes), intent(in) :: CropSResp
    integer(intEnum), intent(in) :: TheCropType
    integer(intEnum), intent(in) :: TheModeCycle
    real(dp), intent(inout) :: b0
    real(dp), intent(inout) :: b1
    real(dp), intent(inout) :: b2
    real(dp), intent(inout) :: BM10
    real(dp), intent(inout) :: BM20
    real(dp), intent(inout) :: BM30
    real(dp), intent(inout) :: BM40
    real(dp), intent(inout) :: BM50
    real(dp), intent(inout) :: BM60
    real(dp), intent(inout) :: BM70

    logical :: bool_cropdeterm
 
    bool_cropdeterm = CropDeterm

    call StressBiomassRelationship(TheDaysToCCini, TheGDDaysToCCini,&
            L0, L12, L123, L1234, LFlor, LengthFlor, GDDL0, GDDL12,&
            GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC, GDDCGC,&
            CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent,&
            Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, WPveg, RatedHIdt,&
            CO2Given, CropDNr1, bool_cropdeterm, CropSResp, TheCropType,&
            TheModeCycle, b0, b1, b2, &
            BM10, BM20, BM30, BM40, BM50, BM60, BM70)
end subroutine StressBiomassRelationship_wrap


subroutine CCxSaltStressRelationship_wrap(TheDaysToCCini, TheGDDaysToCCini,&
       L0, L12, L123, L1234, LFlor, LengthFlor, GDDFlor, GDDLengthFlor,&
       GDDL0, GDDL12, GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC,&
       GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase,&
       Tupper, TDayMin, TDayMax, GDbioLow, WPveg, RatedHIdt, CO2Given,&
       CropDNr1, CropDeterm, TheCropType, TheModeCycle, TheCCsaltDistortion,&
       Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, Salt10, Salt20, Salt30,&
       Salt40, Salt50, Salt60, Salt70, Salt80, Salt90)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: LengthFlor
    integer(int32), intent(in) :: GDDFlor
    integer(int32), intent(in) :: GDDLengthFlor
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    integer(int32), intent(in) :: WPyield
    integer(int32), intent(in) :: RefHI
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDbioLow
    real(dp), intent(in) :: WPveg
    real(dp), intent(in) :: RatedHIdt
    real(dp), intent(in) :: CO2Given
    integer(int32), intent(in) :: CropDNr1
    logical(1), intent(in) :: CropDeterm
    integer(intEnum), intent(in) :: TheCropType
    integer(intEnum), intent(in) :: TheModeCycle
    integer(int8), intent(in) :: TheCCsaltDistortion
    real(dp), intent(inout) :: Coeffb0Salt
    real(dp), intent(inout) :: Coeffb1Salt
    real(dp), intent(inout) :: Coeffb2Salt
    real(dp), intent(inout) :: Salt10
    real(dp), intent(inout) :: Salt20
    real(dp), intent(inout) :: Salt30
    real(dp), intent(inout) :: Salt40
    real(dp), intent(inout) :: Salt50
    real(dp), intent(inout) :: Salt60
    real(dp), intent(inout) :: Salt70
    real(dp), intent(inout) :: Salt80
    real(dp), intent(inout) :: Salt90

    logical :: bool_cropdeterm

    bool_cropdeterm = CropDeterm

    call CCxSaltStressRelationship(TheDaysToCCini, TheGDDaysToCCini,&
       L0, L12, L123, L1234, LFlor, LengthFlor, GDDFlor, GDDLengthFlor,&
       GDDL0, GDDL12, GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC,&
       GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase,&
       Tupper, TDayMin, TDayMax, GDbioLow, WPveg, RatedHIdt, CO2Given,&
       CropDNr1, bool_cropdeterm, TheCropType, TheModeCycle, TheCCsaltDistortion,&
       Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, Salt10, Salt20, Salt30,&
       Salt40, Salt50, Salt60, Salt70, Salt80, Salt90)
end subroutine CCxSaltStressRelationship_wrap


end module ac_interface_tempprocessing
