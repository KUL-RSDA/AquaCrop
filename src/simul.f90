module ac_simul

use ac_kinds, only:  dp, int32, int8
use ac_global, only: BMRange, &
                     CalculateETpot, CanopyCoverNoStressSF, &
                     CompartmentIndividual, &
                     CO2Ref, CCiActual, &
                     datatype_daily, &
                     datatype_decadely, &
                     datatype_monthly, &
                     DetermineCNIandIII, &
                     DetermineRootZoneWC, DetermineDate, &
                     fAdjustedForCO2, &
                     GetCompartment, &
                     GetCompartment_FCadj, &
                     GetCompartment_fluxout, &
                     GetCompartment_Layer, &
                     GetCompartment_theta, &
                     GetCompartment_Thickness, &
                     GetCompartment_WFactor, &
                     GetCrop_pLeafDefLL, GetCrop_pLeafDefUL, &
                     GetCrop_pdef, &
                     GetCrop_subkind, GetCrop_HI, &
                     GetCrop_pMethod, pMethod_FAOCorrection, &
                     pMethod_NoCorrection, &
                     GetCrop_aCoeff, GetCrop_ModeCycle, &
                     GetCrop_AdaptedToCO2, GetCrop_KsShapeFactorLeaf, & 
                     GetCrop_DaysToGermination, &
                     GetCrop_DaysToSenescence, &
                     GetCrop_DaysToHarvest, &
                     GetCrop_GDDaysToGermination, &
                     GetCrop_GDDaysToSenescence, &
                     GetCrop_GDDaysToHarvest, &
                     GetCrop_DaysToFullCanopy, &
                     GetCrop_DaysToSenescence, &
                     GetCrop_CCo, GetCrop_CCx, &
                     GetCrop_CCxRoot, getcrop_fexcess, & 
                     GetCrop_Tcold, getcrop_theat, &
                     GetCrop_CGC, GetCrop_YearCCx, &
                     GetCrop_CDC, GetCrop_GDDCGC, &
                     GetCrop_GDDCDC,getCrop_Day1, &
                     GetCrop_KsShapeFactorStomata, &
                     GetCrop_Planting, GetCrop_LengthFlowering, &
                     GetCrop_Tbase, GetCrop_Tupper, &
                     GetCrop_DaysToGermination, &    
                     GetCrop_DaysToHarvest, GetCrop_KcTop, &
                     getcrop_hiincrease, &
                     GetCrop_KcDecline,GetCrop_GDtranspLow, &
                     GetCrop_CCEffectEvapLate, GetCrop_WP, &
                     GetCrop_WPy, GetCrop_dHIdt, &
                     GetCrop_DaysToFlowering, &
                     getcrop_daystofullcanopysf, &
                     GetCrop_CCxAdjusted, GetCrop_DeterminancyLinked, &
                     GetCrop_GDDaysToFullCanopySF, &
                     SetCrop_pPollination, &
                     GetCrop_pPollination, &
                     getcrop_bcoeff, getcrop_dhimax, &
                     getcrop_stressresponse_calibrated, &
                     GetETo, &
                     GetDrain, &
                     GetManagement_CNcorrection, &
                     GetManagement_FertilityStress, &
                     GetManagement_WeedDeltaRC, GetManagement_WeedAdj, &
                     GetNrCompartments, &
                     GetRain, &
                     GetRainRecord_DataType, &
                     GetRunoff, &
                     GetRootZoneWC_Actual, &
                     GetRootZoneWC_FC, &
                     GetRootZoneWC_WP, &
                     GetRootZoneWC_ZtopAct, &
                     GetRootZoneWC_ZtopFC, &
                     GetRootZoneWC_ZtopWP, &
                     GetSimulation_DelayedDays, &
                     GetCrop_DaysToFlowering, &
                     GetSimulation_EffectStress_RedWP, &
                     GetSimulation_HIfinal, &
                     GetSimulation_RCadj, &
                     GetSimulation_SWCtopSoilConsidered, &
                     GetSimulation_Storage_Btotal, &
                     GetSimulation_YearSeason, &
                     GetSimulParam_CNcorrection, &
                     GetSimulParam_EffectiveRain_ShowersInDecade, &
                     GetSimulParam_IniAbstract, &
                     GetSimulParam_pAdjFAO, &
                     GetSimulParam_PercCCxHIfinal, &
                     GetSimulParam_Tmin, &
                     GetSimulParam_Tmax, &
                     GetSoil_CNvalue, &
                     GetSoilLayer_FC, &
                     GetSoilLayer_GravelVol, &
                     GetSoilLayer_InfRate, &
                     GetSoilLayer_SAT, &
                     GetSoilLayer_tau, &
                     GetSoilLayer_WP, &
                     GetWeedRC, &
                     HarvestIndexDay, &
                     HImultiplier, &
                     KsAny,KsTemperature, &
                     max_No_compartments, &
                     modeCycle_CalendarDays, &
                     MultiplierCCxSelfThinning, &
                     plant_regrowth, &
                     roundc, Rootingdepth, undef_int, &
                     SetCompartment, &
                     SetCompartment_fluxout, &
                     SetCompartment_theta, &
                     SetCompartment_WFactor, &
                     SetDrain, &
                     SetManagement_WeedDeltaRC, &
                     SetRunoff, &
                     SetSimulation_HIfinal, &
                     SetSimulation_SWCtopSoilConsidered, &
                     SetSimulation_Storage_Btotal, &
                     Subkind_Forage, subkind_Grain,  &
                     subkind_Tuber,subkind_Vegetative
                      
use ac_tempprocessing, only: SumCalendarDays


implicit none


contains


real(dp) function GetCDCadjustedNoStressNew(CCx, CDC, CCxAdjusted)
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: CCxAdjusted

    real(dp) :: CDCadjusted

    CDCadjusted = CDC * ((CCxadjusted+2.29_dp)/(CCx+2.29_dp))
    GetCDCadjustedNoStressNew = CDCadjusted

end function GetCDCadjustedNoStressNew

subroutine AdjustpLeafToETo(EToMean, pLeafULAct, pLeafLLAct)
    real(dp), intent(in) :: EToMean
    real(dp), intent(inout) :: pLeafULAct
    real(dp), intent(inout) :: pLeafLLAct


    pLeafLLAct = GetCrop_pLeafDefLL()
    pLeafULAct = GetCrop_pLeafDefUL()
    if (GetCrop_pMethod() == pMethod_FAOCorrection) then
        pLeafLLAct = GetCrop_pLeafDefLL() + GetSimulParam_pAdjFAO()* 0.04_dp &
                        *(5._dp-EToMean)*log10(10._dp-9._dp*GetCrop_pLeafDefLL())
        if (pLeafLLAct > 1.0) then
            pLeafLLAct = 1.0_dp
        end if
        if (pLeafLLAct < 0) then
            pLeafLLAct = 0._dp
        end if
        pLeafULAct = GetCrop_pLeafDefUL() + GetSimulParam_pAdjFAO()* 0.04_dp &
                        *(5._dp-EToMean)*log10(10._dp-9._dp*GetCrop_pLeafDefUL())
        if (pLeafULAct > 1.0) then
            pLeafULAct = 1.0_dp
        end if
        if (pLeafULAct < 0) then
            pLeafULAct = 0._dp
        end if
    end if
end subroutine AdjustpLeafToETo

subroutine DeterminePotentialBiomass(VirtualTimeCC, SumGDDadjCC, CO2i, GDDayi, &
                                              CCxWitheredTpotNoS, BiomassUnlim)
    integer(int32), intent(in) :: VirtualTimeCC
    real(dp), intent(in) :: SumGDDadjCC
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: GDDayi
    real(dp), intent(inout) :: CCxWitheredTpotNoS
    real(dp), intent(inout) :: BiomassUnlim

    real(dp) :: CCiPot,  WPi, fSwitch, TpotForB, EpotTotForB
    integer(int32) :: DAP, DaysYieldFormation, DayiAfterFlowering
    real(dp) :: Tmin_local, Tmax_local

    ! potential biomass - unlimited soil fertiltiy
    ! 1. - CCi
    CCiPot = CanopyCoverNoStressSF((VirtualTimeCC + GetSimulation_DelayedDays() + 1), &
                                  GetCrop_DaysToGermination(), GetCrop_DaysToSenescence(), &
                                  GetCrop_DaysToHarvest(), GetCrop_GDDaysToGermination(), &
                                  GetCrop_GDDaysToSenescence(), GetCrop_GDDaysToHarvest(), &
                                  GetCrop_CCo(), GetCrop_CCx(), GetCrop_CGC(), &
                                  GetCrop_CDC(), GetCrop_GDDCGC(), GetCrop_GDDCDC(), &
                                  SumGDDadjCC, GetCrop_ModeCycle(), 0_int8, 0_int8)
    if (CCiPot < 0._dp) then
        CCiPot = 0._dp
    end if
    if (CCiPot > CCxWitheredTpotNoS) then
        CCxWitheredTpotNoS = CCiPot
    end if

    ! 2. - Calculation of Tpot
    if (GetCrop_ModeCycle() == modeCycle_CalendarDays) then
        DAP = VirtualTimeCC
    else
        ! growing degree days
        Tmin_local = GetSimulParam_Tmin()
        Tmax_local = GetSimulParam_Tmax()
        DAP = SumCalendarDays(roundc(SumGDDadjCC, mold=1), GetCrop_Day1(), GetCrop_Tbase(), &
                    GetCrop_Tupper(), Tmin_local, Tmax_local)
        DAP = DAP + GetSimulation_DelayedDays() ! are not considered when working with GDDays
    end if
    call CalculateETpot(DAP, GetCrop_DaysToGermination(), GetCrop_DaysToFullCanopy(), &
                   GetCrop_DaysToSenescence(), GetCrop_DaysToHarvest(), 0, CCiPot, &
                   GetETo(), GetCrop_KcTop(), GetCrop_KcDecline(), GetCrop_CCx(), &
                   CCxWitheredTpotNoS, real(GetCrop_CCEffectEvapLate(), kind=dp), CO2i, GDDayi, &
                   GetCrop_GDtranspLow(), TpotForB, EpotTotForB)

    ! 3. - WPi for that day
    ! 3a - given WPi
    WPi = (GetCrop_WP()/100._dp)
    ! 3b - WPi decline in reproductive stage  (works with calendar days)
    if (((GetCrop_subkind() == subkind_Grain) .or. (GetCrop_subkind() == subkind_Tuber)) &
        .and. (GetCrop_WPy() < 100._dp) .and. (GetCrop_dHIdt() > 0._dp) &
        .and. (VirtualTimeCC >= GetCrop_DaysToFlowering())) then
        ! WPi in reproductive stage
        fSwitch = 1._dp
        DaysYieldFormation = roundc(GetCrop_HI()/GetCrop_dHIdt(), mold=1)
        DayiAfterFlowering = VirtualTimeCC - GetCrop_DaysToFlowering()
        if ((DaysYieldFormation > 0) .and. (DayiAfterFlowering < &
                                              (DaysYieldFormation/3._dp))) then
            fSwitch = DayiAfterFlowering/(DaysYieldFormation/3._dp)
        end if
        WPi =  WPi * (1._dp - (1._dp-GetCrop_WPy()/100._dp)*fSwitch)
    end if
    ! 3c - adjustment WPi for CO2
    if (roundc(100._dp*CO2i, mold=1) /= roundc(100._dp*CO2Ref, mold=1)) then
        WPi = WPi * fAdjustedForCO2(CO2i, GetCrop_WP(), GetCrop_AdaptedToCO2())
    end if

    ! 4. - Potential Biomass
    if (GetETo() > 0._dp) then
        BiomassUnlim = BiomassUnlim + WPi * TpotForB/real(GetETo(), kind=dp) ! ton/ha
    end if

end subroutine DeterminePotentialBiomass

subroutine DetermineBiomassAndYield(dayi, ETo, TminOnDay, TmaxOnDay, CO2i, &
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

    real(dp), parameter :: TempRange = 5._dp
    real(dp), parameter :: k = 2._dp

    real(dp) :: RatioBM, RBM, HItimesTotal, pLeafULAct, pLeafLLAct, &
                pStomatULAct, pLL, Ksleaf, Ksstomatal, KsPolWS, KsPolCs, &
                KsPolHs, KsPol, Wrel, Dcor, fFlor, fSwitch, fCCx,WPsf, WPunlim, &
                BioAdj,CCtotStar, CCwStar, croppol_temp
    integer(int32) :: tmax1, tmax2, DayCor, DayiAfterFlowering, &
                      DaysYieldFormation, wdrc_temp, HIfinal_temp
    integer(int8) :: PercentLagPhase
    logical :: SWCtopSoilConsidered_temp



    TESTVAL = undef_int

    ! 0. Reference HarvestIndex for that day (alfa in percentage) + Information on PercentLagPhase (for estimate WPi)
    if ((GetCrop_subkind() == Subkind_Tuber) .or. (GetCrop_Subkind() == Subkind_grain) &
        .or. (GetCrop_Subkind() == Subkind_Vegetative) .or. &
                                    (GetCrop_Subkind() == Subkind_Forage)) then
        ! DaysToFlowering corresponds with Tuberformation
        if ((GetCrop_Subkind() == Subkind_Vegetative) .and. &
                                (GetCrop_Planting() == plant_Regrowth) .or. &
                                (GetCrop_Subkind() == Subkind_Forage) .and. &
                                (GetCrop_Planting() == plant_Regrowth)) then
            alfa = GetCrop_HI()
        else
            HIfinal_temp = GetSimulation_HIfinal()
            alfa = HarvestIndexDay((dayi-GetCrop_Day1()), GetCrop_DaysToFlowering(), &
                                   GetCrop_HI(), GetCrop_dHIdt(), CCiactual, &
                                   GetCrop_CCxAdjusted(), GetSimulParam_PercCCxHIfinal(), &
                                   GetCrop_Planting(), PercentLagPhase, HIfinal_temp)
            call SetSimulation_HIfinal(HIfinal_temp)
        end if
    end if

    ! WPi := undef_int; ! for the case ETo is zero, WPi is not determined and hence not displayed
    WPi = (GetCrop_WP()/100._dp)

    ! 1. biomass
    if (ETo > 0) then
        ! 1.1 WPi for that day
        ! 1.1a - given WPi
        WPi = (GetCrop_WP()/100._dp)
        ! 1.1b - adjustment WPi for reproductive stage (works with calendar days)
        if (((GetCrop_subkind() == Subkind_Tuber) .or. &
                    (GetCrop_Subkind() == Subkind_grain)) .and. (alfa > 0)) then
            ! WPi switch to WP for reproductive stage
            fSwitch = 1._dp
            DaysYieldFormation = roundc(GetCrop_HI()/GetCrop_dHIdt(), mold=1)
            if (DaysYieldFormation > 0) then
                if (GetCrop_DeterminancyLinked()) then
                    fSwitch = PercentLagPhase/100._dp
                else
                    DayiAfterFlowering = dayi - GetSimulation_DelayedDays() - &
                                      GetCrop_Day1() - GetCrop_DaysToFlowering()
                    if (DayiAfterFlowering < (DaysYieldFormation/3._dp)) then
                        fSwitch = DayiAfterFlowering/(DaysYieldFormation/3._dp)
                    end if
                end if
            end if
            WPi =  WPi * (1._dp - (1._dp-GetCrop_WPy()/100)*fSwitch)  ! switch in Lag Phase
        end if
        
        ! 1.1c - adjustment WPi for CO2
        if (roundc(100._dp*CO2i, mold=1) /= roundc(100._dp*CO2Ref, mold=1)) then
            WPi = WPi * fAdjustedForCO2(CO2i, GetCrop_WP(), GetCrop_AdaptedToCO2())
        end if
        
        
        ! 1.1d - adjustment WPi for Soil Fertility
        WPsf = WPi          ! no water stress, but fertility stress
        WPunlim = WPi       ! no water stress, no fertiltiy stress
        if (GetSimulation_EffectStress_RedWP() > 0._dp) then ! Reductions are zero if no fertility stress
            ! water stress and fertility stress
            if ((SumKci/SumKcTopStress) < 1._dp) then
                if (ETo > 0) then
                    SumKci = SumKci + Tact/ETo
                end if
                if (SumKci > 0._dp) then
                    WPi = WPi * (1._dp - (GetSimulation_EffectStress_RedWP()/100._dp) &
                                            * exp(k*log(SumKci/SumKcTopStress)) )
                end if
            else
                WPi = WPi * (1._dp - GetSimulation_EffectStress_RedWP()/100._dp)
            end if
        elseif (ETo > 0) then
            SumKci = SumKci + Tact/ETo
        end if
        
        
        
        ! 1.2 actual biomass
        if ((GetSimulation_RCadj() > 0) .and. (roundc(CCtot*10000._dp, mold=1) > 0._dp)) then
            ! weed infestation
            ! green canopy cover of the crop in weed-infested field
            if (GetManagement_WeedDeltaRC() /= epsilon(1._dp)) then
                if (GetCrop_subkind() == Subkind_Forage) then
                    fCCx = MultiplierCCxSelfThinning(int(GetSimulation_YearSeason(), kind=int32), &
                                           int(GetCrop_YearCCx(), kind=int32), GetCrop_CCxRoot())
                else
                    fCCx = 1._dp
                end if
                wdrc_temp = GetManagement_WeedDeltaRC()
                WeedRCi = GetWeedRC(VirtualTimeCC, SumGDDadjCC, fCCx, &
                GetSimulation_RCadj(), GetManagement_WeedAdj(), wdrc_temp, &
                GetCrop_DaysToFullCanopySF(), GetCrop_DaysToSenescence(), &
                GetCrop_GDDaysToFullCanopySF(), GetCrop_GDDaysToSenescence(), &
                GetCrop_ModeCycle())
                call SetManagement_WeedDeltaRC(wdrc_temp)
            else
                WeedRCi = GetSimulation_RCadj()
            end if
            CCw = CCtot * (1._dp-WeedRCi/100._dp)
            ! correction for micro-advection
            CCtotStar = 1.72_dp*CCtot - 1._dp*(CCtot*CCtot) + 0.30_dp*(CCtot*CCtot*CCtot)
            if (CCtotStar < 0) then
                CCtotStar = 0._dp
            end if
            if (CCtotStar > 1) then
                CCtotStar = 1._dp
            end if
            if (CCw > 0.0001_dp) then
                CCwStar = CCw + (CCtotStar - CCtot)
            else
                CCwStar = 0._dp
            end if
            ! crop transpiration in weed-infested field
            if (CCtotStar <= 0.0001_dp) then
                TrW = 0._dp
            else
                TrW = Tact * (CCwStar/CCtotStar)
            end if
            ! crop biomass in weed-infested field
            Biomass = Biomass + WPi *(TrW/ETo)  ! ton/ha
        else
            WeedRCi = 0.0_dp
            CCw = CCtot
            TrW = Tact
            Biomass = Biomass + WPi *(Tact/ETo)  ! ton/ha
        end if
        
        ! Transfer of assimilates
        if (GetCrop_subkind() == subkind_Forage) then
            ! only for perennial herbaceous forage crops
            ! 1. Mobilize assimilates at start of season
            if (MobilizeAssimilates .eqv. .true.) then
                ! mass to mobilize
                if (FracAssim < 0.05_dp) then
                    FracAssim = 0.05_dp
                end if
                Bin = FracAssim * WPi *(TrW/ETo)  ! ton/ha
                if ((AssimMobilized + Bin) > AssimToMobilize) then
                    Bin = AssimToMobilize - AssimMobilized
                end if
                ! cumulative mass mobilized
                AssimMobilized = AssimMobilized + Bin
                ! switch mobilize off when all mass is transfered
                if (roundc(1000._dp*AssimToMobilize, mold=1) <= roundc(1000._dp &
                                                 *AssimMobilized, mold=1)) then
                    MobilizeAssimilates = .false.
                end if
            end if
            ! 2. Store assimilates at end of season
            if (StoreAssimilates .eqv. .true.) then
                ! mass to store
                Bout = FracAssim * WPi *(TrW/ETo)  ! ton/ha
                ! cumulative mass stored
                call SetSimulation_Storage_Btotal(GetSimulation_Storage_Btotal() + Bout)
            end if
            TESTVAL = FracAssim
        end if
        
        Biomass = Biomass + Bin - Bout  ! ton/ha ! correction for transferred assimilates
        
        ! actual total biomass (crop and weeds)
        BiomassTot = BiomassTot + WPi *(Tact/ETo)  ! ton/ha  for dynamic adjustment of soil fertility stress
        BiomassTot = BiomassTot + Bin - Bout ! correction for transferred assimilates
        
        ! 1.3 potential biomass - unlimited soil fertiltiy
        BiomassUnlim = BiomassUnlim + Bin - Bout ! correction for transferred assimilates

    end if

    ! 1.4 potential biomass for given soil fertility
    BiomassPot =  FracBiomassPotSF * BiomassUnlim ! ton/ha

    ! 2. yield
    tmax1 = undef_int
    if ((GetCrop_subkind() == subkind_Tuber) .or. (GetCrop_Subkind() == subkind_Grain)) then
        ! DaysToFlowering corresponds with Tuberformation
        if (dayi > (GetSimulation_DelayedDays() + GetCrop_Day1() + GetCrop_DaysToFlowering())) then
            ! calculation starts when flowering has started
            
            ! 2.2 determine HImultiplier at the start of flowering
            ! effect of water stress before flowering (HItimesBEF)
            if (HItimesBEF < - 0.1_dp) then
                ! i.e. undefined at the start of flowering
                if (BiomassPot < 0.0001_dp) then
                    HItimesBEF = 1._dp
                else
                    RatioBM = Biomass/BiomassPot
                    ! Not correct if weed infestation and no fertility stress
                    ! for that case BiomassPot might be larger (but cannot be calculated since WP is unknown)
                    if (RatioBM > 1._dp) then
                        RatioBM = 1_dp
                    end if
                    RBM = BMRange(int(GetCrop_HIincrease(), kind=int32))
                    HItimesBEF = HImultiplier(RatioBM, RBM, GetCrop_HIincrease())
                end if
                if (CCiActual <= 0.01_dp) then
                    HItimesBEF = 0._dp ! no green canopy cover left at start of flowering;
                end if
            end if
            
            ! 2.3 Relative water content for that day
            SWCtopSoilConsidered_temp = GetSimulation_SWCtopSoilConsidered()
            call DetermineRootZoneWC(RootingDepth, SWCtopSoilConsidered_temp)
            call SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp)
            if (GetSimulation_SWCtopSoilConsidered() .eqv. .true.) then ! top soil is relative wetter than total root zone
                Wrel = (GetRootZoneWC_ZtopFC() - GetRootZoneWC_ZtopAct())/ &
                       (GetRootZoneWC_ZtopFC() - GetRootZoneWC_ZtopWP()) ! top soil
            else
                Wrel = (GetRootZoneWC_FC() - GetRootZoneWC_Actual())/ &
                       (GetRootZoneWC_FC() - GetRootZoneWC_WP()) ! total root zone
            end if
            
            ! 2.4 Failure of Pollination during flowering (alfaMax in percentage)
            if (GetCrop_Subkind() == Subkind_grain) then ! - only valid for fruit/grain crops (flowers)
                if ((dayi <= (GetSimulation_DelayedDays() + GetCrop_Day1() + & 
                   GetCrop_DaysToFlowering() + GetCrop_LengthFlowering())) & ! calculation limited to flowering period
                    .and. ((CCiactual*100._dp) > GetSimulParam_PercCCxHIfinal())) then
                    ! sufficient green canopy remains
                    ! 2.4a - Fraction of flowers which are flowering on day  (fFlor)
                    fFlor = FractionFlowering(dayi)
                    ! 2.4b - Ks(pollination) water stress
                    pLL = 1._dp
                    KsPolWS = KsAny(Wrel, croppol_temp, pLL, 0._dp)
                    call SetCrop_pPollination(croppol_temp)
                    ! 2.4c - Ks(pollination) cold stress
                    KsPolCS = KsTemperature((GetCrop_Tcold()-TempRange), real(GetCrop_Tcold(), kind=dp), TminOnDay)
                    ! 2.4d - Ks(pollination) heat stress
                    KsPolHS = KsTemperature((GetCrop_Theat()+TempRange), real(GetCrop_Theat(), kind=dp), TmaxOnDay)
                    ! 2.4e - Adjust alfa
                    KsPol = KsPolWS
                    if (KsPol > KsPolCS) then
                        KsPol = KsPolCS
                    end if
                    if (KsPol > KsPolHS) then
                        KsPol = KsPolHS
                    end if
                    alfaMax = alfaMax + (KsPol * (1 + GetCrop_fExcess()/100._dp) * fFlor * GetCrop_HI())
                    if (alfaMax > GetCrop_HI()) then
                        alfaMax = GetCrop_HI()
                    end if
                end if
            else
                alfaMax = GetCrop_HI() ! for Tuber crops (no flowering)
            end if
            
            ! 2.5 determine effect of water stress affecting leaf expansion after flowering
            ! from start flowering till end of determinancy
            if (GetCrop_DeterminancyLinked()) then
                tmax1 = roundc(GetCrop_LengthFlowering()/2._dp, mold=1)
            else
                tmax1 = (GetCrop_DaysToSenescence() - GetCrop_DaysToFlowering())
            end if
            if ((HItimesBEF > 0.99_dp) & ! there is green canopy cover at start of flowering;
                .and. (dayi <= (GetSimulation_DelayedDays() + GetCrop_Day1() &
                      + GetCrop_DaysToFlowering()+ tmax1)) & ! and not yet end period
                .and. (tmax1 > 0) & ! otherwise no effect 
                .and. (roundc(GetCrop_aCoeff(), mold=1) /= Undef_int) & ! otherwise no effect
                .and. (CCiactual > 0.001_dp)) then ! and as long as green canopy cover remains (for correction to stresses)
                ! determine KsLeaf
                call AdjustpLeafToETo(ETo, pLeafULAct, pLeafLLAct)
                Ksleaf = KsAny(Wrel, pLeafULAct, pLeafLLAct, GetCrop_KsShapeFactorLeaf())
                ! daily correction
                Dcor = (1._dp + (1._dp-Ksleaf)/GetCrop_aCoeff())
                ! weighted correction
                ScorAT1 = ScorAT1 + Dcor/tmax1
                DayCor = dayi - (GetSimulation_DelayedDays() + GetCrop_Day1() + GetCrop_DaysToFlowering())
                HItimesAT1  = (tmax1/DayCor) * ScorAT1
            end if
            
            ! 2.6 determine effect of water stress affecting stomatal closure after flowering
            ! during yield formation
            if (GetCrop_dHIdt() > 99._dp) then
                tmax2 = 0._dp
            else
                tmax2 = roundc(GetCrop_HI()/GetCrop_dHIdt(), mold=1)
            end if
            if ((HItimesBEF > 0.99_dp) & ! there is green canopy cover at start of flowering;
                .and. (dayi <= (GetSimulation_DelayedDays() + GetCrop_Day1() &
                      + GetCrop_DaysToFlowering() + tmax2)) & ! and not yet end period
                .and. (tmax2 > 0._dp) & ! otherwise no effect
                .and. (roundc(GetCrop_bCoeff(), mold=1) /= Undef_int) & ! otherwise no effect
                .and. (CCiactual > 0.001_dp)) then ! and as long as green canopy cover remains (for correction to stresses)
                ! determine KsStomatal
                call AdjustpStomatalToETo(ETo, pStomatULAct)
                pLL = 1._dp
                Ksstomatal = KsAny(Wrel, pStomatULAct, pLL, GetCrop_KsShapeFactorStomata())
                ! daily correction
                if (Ksstomatal > 0.001_dp) then
                    Dcor = (exp(0.10_dp*log(Ksstomatal))) * (1._dp-(1._dp-Ksstomatal)/GetCrop_bCoeff())
                else
                    Dcor = 0._dp
                end if
                ! weighted correction
                ScorAT2 = ScorAT2 + Dcor/tmax2
                DayCor = dayi - (GetSimulation_DelayedDays() + GetCrop_Day1() + GetCrop_DaysToFlowering())
                HItimesAT2  = (tmax2/DayCor) * ScorAT2
            end if
            
            ! 2.7 total multiplier after flowering
            if ((tmax2 == epsilon(1._dp)) .and. (tmax1 == epsilon(1._dp))) then
                HItimesAT = 1._dp
            else
                if (tmax2 == epsilon(1._dp)) then
                    HItimesAT = HItimesAT1
                else
                    if (tmax1 == epsilon(1._dp)) then
                        HItimesAT = HItimesAT2
                    elseif (tmax1 <= tmax2) then
                        HItimesAT = HItimesAT2 * ((tmax1*HItimesAT1 + (tmax2-tmax1))/tmax2)
                        if (roundc(GetCrop_bCoeff(), mold=1) == Undef_int) then
                            HItimesAT = HItimesAT1
                        end if
                        if (roundc(GetCrop_aCoeff(), mold=1) == Undef_int) then
                            HItimesAT = HItimesAT2
                        end if
                    else
                        HItimesAT = HItimesAT1 * ((tmax2*HItimesAT2 + (tmax1-tmax2))/tmax1)
                        if (roundc(GetCrop_bCoeff(), mold=1) == Undef_int) then
                            HItimesAT = HItimesAT1
                        end if
                        if (roundc(GetCrop_aCoeff(), mold=1) == Undef_int) then
                            HItimesAT = HItimesAT2
                        end if
                    end if
                end if
            end if
            
            ! 2.8 Limit HI to allowable maximum increase
            HItimesTotal = HItimesBEF * HItimesAT
            if (HItimesTotal > (1._dp +(GetCrop_DHImax()/100._dp))) then
                HItimesTotal = 1._dp +(GetCrop_DHImax()/100._dp)
            end if
            
            ! 2.9 Yield
            if (alfaMax >= alfa) then
                YieldPart = Biomass * HItimesTotal*(alfa/100._dp)
            else
                YieldPart = Biomass * HItimesTotal*(alfaMax/100._dp)
            end if
            ! (Crop.subkind = Tuber) OR (Crop.Subkind = grain)
        end if
        
        ! 2bis. yield leafy vegetable crops
        if ((GetCrop_subkind() == subkind_Vegetative) .or. (GetCrop_subkind() == subkind_Forage)) then
            if (dayi >= (GetSimulation_DelayedDays() + GetCrop_Day1() + GetCrop_DaysToFlowering())) then
                ! calculation starts at crop day 1 (since days to flowering is 0)
                if ((100._dp*CCw) < GetSimulParam_PercCCxHIfinal()) then
                    alfa = 0._dp
                end if
                if (roundc(100._dp*ETo, mold=1)> 0._dp) then
                    ! with correction for transferred assimilates
                    if (GetSimulation_RCadj() > 0._dp) then 
                        YieldPart = YieldPart + (WPi*(TrW/ETo) + Bin - Bout) * (alfa/100._dp)
                        
                    else
                        YieldPart = YieldPart + (WPi*(Tact/ETo) + Bin - Bout) * (alfa/100._dp)
                    end if
                end if
            end if
        end if
        
        
        
        ! 3. Dynamic adjustment of soil fertility stress
        if ((GetManagement_FertilityStress() > 0) .and. (BiomassUnlim > 0.001_dp) &
                                .and. GetCrop_StressResponse_Calibrated()) then
            BioAdj = 100._dp * (FracBiomassPotSF + (FracBiomassPotSF - BiomassTot/BiomassUnlim))
            if (BioAdj >= 100._dp) then
                StressSFadjNEW = 0._dp
            else
                if (BioAdj <= epsilon(1._dp)) then
                    StressSFadjNEW = 80._dp
                else
                    StressSFadjNEW = roundc(Coeffb0 + Coeffb1*BioAdj + Coeffb2*BioAdj*BioAdj, mold=1)
                    if (StressSFadjNEW < 0) then
                        StressSFadjNEW = GetManagement_FertilityStress()
                    end if
                    if (StressSFadjNEW > 80._dp) then
                        StressSFadjNEW = 80._dp
                    end if
                end if
                if (StressSFadjNEW > GetManagement_FertilityStress()) then
                    StressSFadjNEW = GetManagement_FertilityStress()
                end if
            end if
            if ((GetCrop_Subkind() == Subkind_grain) .and. GetCrop_DeterminancyLinked() &
                .and. (dayi > (GetSimulation_DelayedDays() + GetCrop_Day1() &
                                    + GetCrop_DaysToFlowering() + tmax1))) then
                ! potential vegetation period is exceeded
                if (StressSFadjNEW < PreviousStressLevel) then
                    StressSFadjNEW = PreviousStressLevel
                end if
                if (StressSFadjNEW > GetManagement_FertilityStress()) then
                    StressSFadjNEW = GetManagement_FertilityStress()
                end if
            end if
        else
            StressSFadjNEW = 0._dp
        end if
        PreviousStressLevel = StressSFadjNEW
        SumKcTopStress = (1._dp - StressSFadjNEW/100._dp) * SumKcTop
        
    end if

    contains

    real(dp) function FractionFlowering(Dayi)
      integer(int32), intent(in) :: Dayi

      real(dp) :: f1, f2, F
      integer(int32) :: DiFlor

      if (GetCrop_LengthFlowering() <= 1._dp) then
          F = 1._dp
      else
          DiFlor = dayi - (GetSimulation_DelayedDays() + &
                            GetCrop_Day1() + GetCrop_DaysToFlowering())
          f2 = FractionPeriod(DiFlor)
          DiFlor = (dayi-1) - (GetSimulation_DelayedDays() + &
                            GetCrop_Day1() + GetCrop_DaysToFlowering())
          f1 = FractionPeriod(DiFlor)
          if (abs(f1-f2) < 0.0000001_dp) then
              F = 0._dp
          else
              F = (100._dp * ((f1+f2)/2._dp)/GetCrop_LengthFlowering())
          end if
      end if
      FractionFlowering = F
    end function FractionFlowering

    real(dp) function FractionPeriod(DiFlor)
        integer(int32), intent(in) :: DiFlor

        real(dp) :: fi, TimePerc

        if (DiFlor <= epsilon(1._dp)) then
            fi = 0._dp
        else
            TimePerc = 100._dp * (DiFlor/GetCrop_LengthFlowering())
            if (TimePerc > 100) then
                fi = 1._dp
            else
                fi = 0.00558_dp * exp(0.63_dp*log(TimePerc)) - &
                     0.000969_dp * TimePerc - 0.00383_dp
                if (fi < 0) then
                    fi = 0._dp
                end if
            end if
        end if
        FractionPeriod = fi
    end function FractionPeriod

    integer(int32) function YearWeighingFactor(CropFirstDayNr)
        integer(int32), intent(in) :: CropFirstDayNr
        
        integer(int32) :: Dayi, Monthi, Yeari

        call DetermineDate(CropFirstDayNr, Dayi, Monthi, Yeari)
        YearWeighingFactor = Yeari
    end function YearWeighingFactor 

end subroutine DetermineBiomassAndYield

subroutine AdjustpStomatalToETo(MeanETo, pStomatULAct)
    real(dp), intent(in) :: MeanETo
    real(dp), intent(inout) :: pStomatULAct


    select case (GetCrop_pMethod())
        case (pMethod_NoCorrection)
            pStomatULAct = GetCrop_pdef() 

        case (pMethod_FAOCorrection)
             pStomatULAct = GetCrop_pdef() + GetSimulParam_pAdjFAO() * &
             (0.04_dp *(5._dp-MeanETo))*log10(10._dp-9._dp*GetCrop_pdef())
    end select
    if (pStomatULAct > 1) then
        pStomatULAct = 1._dp
    end if
    if (pStomatULAct < 0) then
        pStomatULAct = 0._dp
    end if
end subroutine AdjustpStomatalToETo




!-----------------------------------------------------------------------------
! BUDGET_module
!-----------------------------------------------------------------------------

real(dp) function calculate_delta_theta(theta_in, thetaAdjFC, NrLayer)
    real(dp), intent(in) :: theta_in
    real(dp), intent(in) :: thetaAdjFC
    integer(int32), intent(in) :: NrLayer

    real(dp) :: DeltaX, theta, theta_sat, theta_fc

    theta = theta_in
    theta_sat = GetSoilLayer_SAT(NrLayer) / 100.0_dp
    theta_fc = GetSoilLayer_FC(NrLayer) / 100.0_dp
    if (theta > theta_sat) then
        theta = theta_sat
    end if
    if (theta <= thetaAdjFC/100.0_dp) then
        DeltaX = 0.0_dp
    else
        DeltaX = GetSoilLayer_tau(NrLayer)&
                 * (theta_sat - theta_fc)&
                 * (exp(theta - theta_fc) - 1.0_dp)&
                 / (exp(theta_sat - theta_fc) - 1.0_dp)
        if ((theta - DeltaX) < thetaAdjFC) then
            DeltaX = theta - thetaAdjFC
        end if
    end if
    calculate_delta_theta = DeltaX
end function calculate_delta_theta


real(dp) function calculate_theta(delta_theta, thetaAdjFC, NrLayer)
    real(dp), intent(in) :: delta_theta
    real(dp), intent(in) :: thetaAdjFC
    integer(int32), intent(in) :: NrLayer

    real(dp) :: ThetaX, theta_sat, theta_fc, tau

    theta_sat = GetSoilLayer_SAT(NrLayer) / 100.0_dp
    theta_fc = GetSoilLayer_FC(NrLayer) / 100.0_dp
    tau = GetSoilLayer_tau(NrLayer)
    if (delta_theta <= epsilon(0.0_dp)) then
        ThetaX = thetaAdjFC
    elseif (tau > 0.0_dp) then
        ThetaX = theta_fc&
            + log(1.0_dp&
                  + delta_theta&
                  * (exp(theta_sat - theta_fc) - 1.0_dp)&
                  / (tau * (theta_sat - theta_fc)))
        if (ThetaX < thetaAdjFC) then
            ThetaX = thetaAdjFC
        end if
    else
        ! to stop draining
        ThetaX = theta_sat + 0.1_dp
    end if
    calculate_theta = ThetaX
end function calculate_theta


subroutine calculate_drainage()
    integer(int32) ::  i, compi, layeri, pre_nr
    real(dp) :: drainsum, delta_theta, drain_comp, drainmax, theta_x, excess
    real(dp) :: pre_thick
    logical :: drainability

    drainsum = 0.0_dp
    do compi=1, GetNrCompartments()
        ! 1. Calculate drainage of compartment
        ! ====================================
        layeri = GetCompartment_Layer(compi)
        if (GetCompartment_theta(compi) &
                 > GetCompartment_FCadj(compi)/100.0_dp) then
            delta_theta = calculate_delta_theta(GetCompartment_theta(compi), &
                (GetCompartment_FCadj(compi)/100.0_dp), layeri)
        else
            delta_theta = 0.0_dp
        end if
        drain_comp = delta_theta * 1000.0_dp * GetCompartment_Thickness(compi) &
                      * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)


        ! 2. Check drainability
        ! =====================
        excess = 0.0_dp
        pre_thick = 0.0_dp
        do i = 1, (compi-1)
            pre_thick = pre_thick + GetCompartment_Thickness(i)
        end do
        drainmax = delta_theta * 1000.0_dp * pre_thick &
                   * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
        if (drainsum <= drainmax) then
            drainability = .true.
        else
            drainability = .false.
        end if

        ! 3. Drain compartment
        ! ====================
        if (drainability) then
            call SetCompartment_theta(compi, &
                     GetCompartment_theta(compi)-delta_theta)
            drainsum = drainsum + drain_comp
            call CheckDrainsum(layeri, drainsum, excess)
        else  ! drainability == .false.
            delta_theta = drainsum/(1000.0_dp * pre_thick&
                                    *(1-GetSoilLayer_GravelVol(layeri)/100.0_dp))
            theta_x = calculate_theta(delta_theta, &
                (GetCompartment_FCadj(compi)/100.0_dp), layeri)

            if (theta_x <= GetSoilLayer_SAT(layeri)/100.0_dp) then
                call SetCompartment_theta(compi, &
                         GetCompartment_theta(compi) &
                         + drainsum/(1000.0_dp*GetCompartment_Thickness(compi) &
                                     *(1-GetSoilLayer_GravelVol(layeri)/100.0_dp)))
                if (GetCompartment_theta(compi) > theta_x) then
                    drainsum = (GetCompartment_theta(compi) - theta_x) &
                               * 1000.0_dp * GetCompartment_Thickness(compi) &
                               * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
                    delta_theta = calculate_delta_theta(theta_x, &
                        (GetCompartment_FCadj(compi)/100.0_dp), layeri)
                    drainsum = drainsum +  delta_theta * 1000.0_dp &
                                           * GetCompartment_Thickness(compi) &
                                           * (1 - GetSoilLayer_GravelVol(layeri)&
                                                  /100.0_dp)
                    call CheckDrainsum(layeri, drainsum, excess)
                    call SetCompartment_theta(compi, theta_x - delta_theta)
                elseif (GetCompartment_theta(compi) &
                         > GetCompartment_FCadj(compi)/100.0_dp) then
                    delta_theta = calculate_delta_theta(&
                        GetCompartment_theta(compi), &
                        (GetCompartment_FCadj(compi)/100.0_dp), &
                        layeri)
                    call SetCompartment_theta(compi, &
                             GetCompartment_theta(compi) - delta_theta)
                    drainsum = delta_theta * 1000.0_dp &
                               * GetCompartment_Thickness(compi) &
                               * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
                    call CheckDrainsum(layeri, drainsum, excess)
                else
                    drainsum = 0.0_dp
                end if
            end if ! theta_x <= SoilLayer[layeri].SAT/100

            if (theta_x > GetSoilLayer_SAT(layeri)/100.0_dp) then
                call SetCompartment_theta(compi,&
                         GetCompartment_theta(compi)&
                         + drainsum/(1000.0_dp*GetCompartment_Thickness(compi) &
                                     *(1-GetSoilLayer_GravelVol(layeri)/100.0_dp)))
                if (GetCompartment_theta(compi) &
                         <= GetSoilLayer_SAT(layeri)/100.0_dp) then
                    if (GetCompartment_theta(compi) &
                            > GetCompartment_FCadj(compi)/100.0_dp) then
                        delta_theta = calculate_delta_theta(&
                            GetCompartment_theta(compi), &
                            (GetCompartment_FCadj(compi)/100.0_dp),&
                            layeri)
                        call SetCompartment_theta(compi, &
                                 GetCompartment_theta(compi) - delta_theta)
                        drainsum = delta_theta * 1000.0_dp &
                                   * GetCompartment_Thickness(compi) &
                                   *(1-GetSoilLayer_GravelVol(layeri)/100.0_dp)
                        call CheckDrainsum(layeri, drainsum, excess)
                    else
                        drainsum = 0.0_dp
                    end if
                end if
                if (GetCompartment_theta(compi)&
                        > GetSoilLayer_SAT(layeri)/100.0_dp) then
                    excess = (GetCompartment_theta(compi)&
                               - (GetSoilLayer_SAT(layeri)/100.0_dp)) &
                             * 1000.0_dp * GetCompartment_Thickness(compi) &
                             * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
                    delta_theta = calculate_delta_theta(&
                         GetCompartment_theta(compi), &
                         (GetCompartment_FCadj(compi)/100),&
                         layeri)
                    call SetCompartment_theta(compi, &
                             GetSoilLayer_SAT(layeri)/100.0_dp - delta_theta)
                    drain_comp = delta_theta * 1000.0_dp&
                                 * GetCompartment_Thickness(compi)&
                                 * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
                    drainmax = delta_theta * 1000.0_dp * pre_thick&
                               * (1 - GetSoilLayer_GravelVol(layeri)/100.0_dp)
                    if (drainmax > excess) then
                        drainmax = excess
                    end if
                    excess = excess - drainmax
                    drainsum = drainmax + drain_comp
                    call CheckDrainsum(layeri, drainsum, excess)
                end if
            end if ! theta_x > SoilLayer[layeri].SAT/100
        end if ! drainability = false

        call SetCompartment_fluxout(compi, drainsum)


        ! 4. Redistribute excess
        ! ======================
        if (excess > 0.0_dp) then
            pre_nr = compi + 1
            loop: do
                pre_nr = pre_nr - 1
                layeri = GetCompartment_Layer(pre_nr)
                if (pre_nr < compi) then
                    call SetCompartment_fluxout(pre_nr,&
                             GetCompartment_fluxout(pre_nr) - excess)
                end if
                call SetCompartment_theta(pre_nr,&
                    GetCompartment_theta(pre_nr)&
                    + excess&
                    / (1000.0_dp*GetCompartment_Thickness(pre_nr)&
                       *(1-GetSoilLayer_GravelVol(GetCompartment_Layer(pre_nr))&
                           /100.0_dp)))
                if (GetCompartment_theta(pre_nr) &
                        > GetSoilLayer_SAT(layeri)/100.0_dp) then
                    excess = (GetCompartment_theta(pre_nr) &
                              - GetSoilLayer_SAT(layeri)/100) &
                             * 1000.0_dp * GetCompartment_Thickness(pre_nr) &
                             * (1-GetSoilLayer_GravelVol(GetCompartment_Layer(pre_nr))&
                                    /100.0_dp)
                    call SetCompartment_theta(pre_nr,&
                             GetSoilLayer_SAT(layeri)/100.0_dp)
                else
                    excess = 0.0_dp
                end if
                if ((excess == 0) .or. (pre_nr == 1)) exit loop
            end do loop
            ! redistribute excess
        end if

    !Do-loop
    end do
    call SetDrain(drainsum)

contains

    subroutine CheckDrainsum(layeri, drainsum, excess)
        integer(int32), intent(in) :: layeri
        real(dp), intent(inout) :: drainsum
        real(dp), intent(inout) :: excess

        if (drainsum > GetSoilLayer_InfRate(layeri)) then
            excess = excess + drainsum - GetSoilLayer_InfRate(layeri)
            drainsum = GetSoilLayer_InfRate(layeri)
        end if
    end subroutine CheckDrainsum

end subroutine calculate_drainage


subroutine calculate_weighting_factors(Depth, Compartment)
    real(dp), intent(in) :: Depth
    type(CompartmentIndividual), dimension(max_No_compartments), intent(inout) :: Compartment

    integer(int32) :: i, compi
    real(dp) :: CumDepth, xx, wx

    CumDepth = 0.0_dp
    xx = 0.0_dp
    compi = 0
    loop: do
        compi = compi + 1
        CumDepth = CumDepth + Compartment(compi)%Thickness
        if (CumDepth > Depth) then
            CumDepth = Depth
        end if
        wx = 1.016_dp * (1.0_dp - EXP(-4.16_dp * CumDepth/Depth))
        Compartment(compi)%WFactor = wx - xx
        if (Compartment(compi)%WFactor > 1.0_dp) then
            Compartment(compi)%WFactor = 1.0_dp
        end if
        if (Compartment(compi)%WFactor < 0.0_dp) then
            Compartment(compi)%WFactor = 0.0_dp
        end if
        xx = wx
        if ((CumDepth >= Depth) .or. (compi == GetNrCompartments())) exit loop
    enddo loop
    do i = (compi + 1), GetNrCompartments()
        Compartment(i)%WFactor = 0.0_dp
    end do
end subroutine calculate_weighting_factors


subroutine calculate_runoff(MaxDepth)
    real(dp), intent(in) :: MaxDepth

    real(dp) :: SUM, CNA, Shower, term, S
    integer(int8) :: CN2, CN1, CN3

    CN2 = roundc(GetSoil_CNvalue()&
                 * (100 + GetManagement_CNcorrection())/100.0_dp,&
                 mold=1_int8)
    if (GetRainRecord_DataType() == datatype_daily) then
        if (GetSimulParam_CNcorrection()) then
            call calculate_relative_wetness_topsoil(SUM)
            call DetermineCNIandIII(CN2, CN1, CN3)
            CNA = real(roundc(CN1+(CN3-CN1)*SUM, mold=1_int32), kind=dp)
        else
            CNA = real(CN2, kind=dp)
        end if
        Shower = GetRain()
    else
        CNA = real(CN2, kind=dp)
        Shower = (GetRain()*10.0_dp) &
                 / GetSimulParam_EffectiveRain_ShowersInDecade()
    end if
    S = 254.0_dp * (100.0_dp/CNA - 1.0_dp)
    term = Shower - (GetSimulParam_IniAbstract()/100.0_dp) * S
    if (term <= epsilon(0.0_dp)) then
        call SetRunoff(0.0_dp);
    else
        call SetRunoff(term**2&
             / (Shower + (1.0_dp - (GetSimulParam_IniAbstract()/100.0_dp)) * S))
    end if
    if ((GetRunoff() > 0.0_dp) .and. ( &
            (GetRainRecord_DataType() == datatype_decadely)&
             .or. (GetRainRecord_DataType() == datatype_monthly))) then
        if (GetRunoff() >= Shower) then
            call SetRunoff(GetRain())
        else
            call SetRunoff(GetRunoff() &
                   * (GetSimulParam_EffectiveRain_ShowersInDecade()/10.14_dp))
            if (GetRunoff() > GetRain()) then
                call SetRunoff(GetRain())
            end if
        end if
    end if

    contains

    subroutine calculate_relative_wetness_topsoil(SUM)
        real(dp), intent(inout) :: SUM

        real(dp) :: CumDepth, theta
        integer(int32) :: compi, layeri
        type(CompartmentIndividual), dimension(max_No_compartments) :: Compartment_temp

        Compartment_temp = GetCompartment()
        call calculate_weighting_factors(MaxDepth, Compartment_temp)
        call SetCompartment(Compartment_temp)
        SUM = 0.0_dp
        compi = 0
        CumDepth = 0.0_dp

        loop : do
            compi = compi + 1
            layeri = GetCompartment_Layer(compi)
            CumDepth = CumDepth + GetCompartment_Thickness(compi)
            if (GetCompartment_theta(compi) < GetSoilLayer_WP(layeri)/100.0_dp) then
                theta = GetSoilLayer_WP(layeri)/100.0_dp
            else
                theta = GetCompartment_theta(compi)
            end if
            SUM = SUM + GetCompartment_WFactor(compi) &
                 * (theta-GetSoilLayer_WP(layeri)/100.0_dp) &
                 / (GetSoilLayer_FC(layeri)/100.0_dp - GetSoilLayer_WP(layeri)/100.0_dp)
            if ((CumDepth >= MaxDepth) .or. (compi == GetNrCompartments())) exit loop
        end do loop

        if (SUM < 0.0_dp) then
            SUM = 0.0_dp
        end if
        if (SUM > 1.0_dp) then
            SUM = 1.0_dp
        end if

    end subroutine calculate_relative_wetness_topsoil

end subroutine calculate_runoff




!-----------------------------------------------------------------------------
! end BUDGET_module
!-----------------------------------------------------------------------------

end module ac_simul
