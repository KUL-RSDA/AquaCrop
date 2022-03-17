module ac_simul

use ac_kinds, only:  dp, int32, int8
use ac_global, only: CalculateETpot, CanopyCoverNoStressSF, &
                     CCiNoWaterStressSF, &
                     CompartmentIndividual, &
                     CO2Ref, &
                     datatype_daily, &
                     datatype_decadely, &
                     datatype_monthly, &
                     DetermineCNIandIII, &
                     DetermineRootzoneWC, &
                     EffectiveRainMethod_Percentage, &
                     EffectiveRainMethod_USDA, &
                     GenerateDepthMode_FixDepth, &
                     GenerateTimeMode_AllDepl, &
                     GenerateTimeMode_AllRAW, &
                     GetCCiPrev, &
                     GetCCiTopEarlySen, &
                     GetCompartment, &
                     GetCompartment_FCadj, &
                     GetCompartment_fluxout, &
                     GetCompartment_Layer, &
                     GetCompartment_theta, &
                     GetCompartment_Thickness, &
                     GetCompartment_WFactor, &
                     GetCrop_pLeafDefLL, GetCrop_pLeafDefUL, &
                     GetCrop_pLeafAct, &
                     GetCrop_pSenAct, &
                     GetCrop_subkind, GetCrop_HI, &
                     GetCrop_pMethod, pMethod_FAOCorrection, &
                     GetCrop_pSenescence, &
                     GetCrop_ModeCycle, &
                     GetCrop_AdaptedToCO2, & 
                     GetCrop_DaysToGermination, &
                     GetCrop_DaysToSenescence, &
                     GetCrop_DaysToHarvest, &
                     GetCrop_DeterminancyLinked, &
                     GetCrop_GDDCGC, &
                     GetCrop_GDDaysToFlowering, &
                     GetCrop_GDDaysToFullCanopy, &
                     GetCrop_GDDaysToFullCanopySF, &
                     GetCrop_GDDaysToGermination, &
                     GetCrop_GDDaysToSenescence, &
                     GetCrop_GDDaysToHarvest, &
                     GetCrop_GDDLengthFlowering, &
                     GetCrop_Day1, &
                     GetCrop_DayN, &
                     GetCrop_DaysToCCini, &
                     GetCrop_DaysToFullCanopy, &
                     GetCrop_DaysToFullCanopySF, &
                     GetCrop_DaysToSenescence, &
                     GetCrop_CCo, GetCrop_CCx, &
                     GetCrop_CCoAdjusted, &
                     GetCrop_CCxAdjusted, &
                     GetCrop_CCxWithered, &
                     GetCrop_CGC, GetCrop_CCx, &
                     GetCrop_CDC, GetCrop_GDDCGC, &
                     GetCrop_GDDCDC,getCrop_Day1, &
                     GetCrop_Tbase, GetCrop_Tupper, &
                     GetCrop_DaysToGermination, &    
                     GetCrop_DaysToHarvest, GetCrop_KcTop, &
                     GetCrop_KcDecline,GetCrop_GDtranspLow, &
                     GetCrop_CCEffectEvapLate, GetCrop_WP, &
                     GetCrop_KsShapeFactorLeaf, &
                     GetCrop_KsShapeFactorSenescence, &
                     GetCrop_WPy, GetCrop_dHIdt, &
                     GetCrop_DaysToFlowering, &
                     GetCrop_SumEToDelaySenescence, &
                     GetECstorage, &
                     GetEpot, &
                     GetETO, &
                     GetDrain, &
                     GetGenerateDepthMode, &
                     GetGenerateTimeMode, &
                     GetIrriECw_PostSeason, &
                     GetIrriECw_PreSeason, &
                     GetIrrigation, &
                     GetManagement_CNcorrection, &
                     GetManagement_Cuttings_CGCPlus, &
                     GetManagement_BundHeight, &
                     GetNrCompartments, &
                     GetRain, &
                     GetRainRecord_DataType, &
                     GetRootingDepth, &
                     GetRootZoneWC_Actual, &
                     GetRootZoneWC_FC, &
                     GetRootZoneWC_Thresh, &
                     GetRootZoneWC_WP, &
                     GetRootZoneWC_ZtopAct, &
                     GetRootZoneWC_ZtopFC, &
                     GetRootZoneWC_ZtopWP, &
                     GetRunoff, &
                     GetSimulation_DelayedDays, &
                     GetSimulation_EffectStress_CDecline, &
                     GetSimulation_EffectStress_RedCCX, &
                     GetSimulation_EffectStress_RedCGC, &
                     GetSimulation_IrriECw, &
                     GetSimulation_ProtectedSeedling, &
                     GetSimulation_SumEToStress, &
                     GetSimulation_SWCtopSoilConsidered, &
                     GetSimulParam_Beta, &
                     GetSimulParam_CNcorrection, &
                     GetSimulParam_EffectiveRain_Method, &
                     GetSimulParam_EffectiveRain_PercentEffRain, &
                     GetSimulParam_EffectiveRain_ShowersInDecade, &
                     GetSimulParam_EvapZmax, &
                     GetSimulParam_IniAbstract, &
                     GetSimulParam_pAdjFAO, &
                     GetSimulParam_Tmin, &
                     GetSimulParam_Tmax, &
                     GetSoil_CNvalue, &
                     GetSoilLayer_FC, &
                     GetSoilLayer_GravelVol, &
                     GetSoilLayer_InfRate, &
                     GetSoilLayer_SAT, &
                     GetSoilLayer_tau, &
                     GetSoilLayer_WP, &
                     GetSurfaceStorage, &
                     GetTpot, &
                     KsAny, &
                     fAdjustedForCO2, &
                     LengthCanopyDecline, &
                     max_No_compartments, &
                     modeCycle_CalendarDays, &
                     roundc, &
                     SetCCiPrev, &
                     SetCCiTopEarlySen, &
                     SetCompartment, &
                     SetCompartment_fluxout, &
                     SetCompartment_theta, &
                     SetCompartment_WFactor, &
                     SetCrop_CCoAdjusted, &
                     SetCrop_CCxAdjusted, &
                     SetCrop_CCxWithered, &
                     SetCrop_pLeafAct, &
                     SetCrop_pSenAct, &
                     SetDrain, &
                     SetECstorage, &
                     SetIrrigation, &
                     SetRunoff, &
                     SetSimulation_EvapLimitON, &
                     SetSimulation_ProtectedSeedling, &
                     SetSimulation_SumEToStress, &
                     SetSimulation_SWCtopSoilConsidered, &
                     SetSurfaceStorage, &
                     subkind_Grain, subkind_Tuber, &
                     subkind_Forage, &
                     undef_double, &
                     undef_int
                      
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


subroutine AdjustpSenescenceToETo(EToMean, TimeSenescence, WithBeta, pSenAct)
    real(dp), intent(in) :: EToMean
    real(dp), intent(in) :: TimeSenescence
    logical, intent(in) :: WithBeta
    real(dp), intent(inout) :: pSenAct

    pSenAct = GetCrop_pSenescence()
    if (GetCrop_pMethod() == pMethod_FAOCorrection) then
        pSenAct = GetCrop_pSenescence() + GetSimulParam_pAdjFAO() &
                            * 0.04_dp*(5._dp-EToMean) &
                            * log10(10._dp-9._dp*GetCrop_pSenescence())
        if ((TimeSenescence > 0.0001_dp) .and. WithBeta) then
            pSenAct = pSenAct * (1._dp-GetSimulParam_Beta()/100._dp)
        end if
        if (pSenAct < 0._dp) then
            pSenAct = 0._dp
        end if
        if (pSenAct >= 1.0_dp) then
            pSenAct = 0.98_dp ! otherwise senescence is not possible at WP
        end if
    end if
end subroutine AdjustpSenescenceToETo


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
        Shower = (GetRain()*10.0_dp)&
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


subroutine Calculate_irrigation(SubDrain, TargetTimeVal, TargetDepthVal)
    real(dp), intent(inout) :: SubDrain
    integer(int32), intent(inout) :: TargetTimeVal
    integer(int32), intent(in) :: TargetDepthVal

    real(dp) :: ZrWC, RAWi
    logical :: SWCtopSoilConsidered_temp

    ! total root zone is considered
    SWCtopSoilConsidered_temp = GetSimulation_SWCtopSoilConsidered()
    call DetermineRootZoneWC(GetRootingDepth(), SWCtopSoilConsidered_temp)
    call SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp)
    ZrWC = GetRootZoneWC_Actual() - GetEpot() - GetTpot() &
           + GetRain() - GetRunoff() - SubDrain
    if (GetGenerateTimeMode() == GenerateTimeMode_AllDepl) then
        if ((GetRootZoneWC_FC() - ZrWC) >= TargetTimeVal) then
            TargetTimeVal = 1
        else
            TargetTimeVal = 0
        end if
    end if
    if (GetGenerateTimeMode() == GenerateTimeMode_AllRAW) then
        RAWi = TargetTimeVal/100._dp &
                * (GetRootZoneWC_FC() - GetRootZoneWC_Thresh())
        if ((GetRootZoneWC_FC() - ZrWC) >= RAWi) then
            TargetTimeVal = 1
        else
            TargetTimeVal = 0
        end if
    end if
    if (TargetTimeVal == 1) then
        if (GetGenerateDepthMode() == GenerateDepthMode_FixDepth) then
            call SetIrrigation(real(TargetDepthVal, kind=dp))
        else
            call SetIrrigation((GetRootZoneWC_FC() - ZrWc) &
                                            + TargetDepthVal)
            if (GetIrrigation() < 0._dp) then
                call SetIrrigation(0._dp)
            end if
        end if
    else
        call SetIrrigation(0._dp)
    end if
end subroutine Calculate_irrigation



subroutine CalculateEffectiveRainfall(SubDrain)
    real(dp), intent(inout) :: SubDrain

    real(dp) :: EffecRain, ETcropMonth, RainMonth, &
                DrainMax, Zr, depthi, DTheta, RestTheta
    integer(int32) :: compi

    if (GetRain() > 0._dp) then
        ! 1. Effective Rainfall
        EffecRain = (GetRain()-GetRunoff())
        select case (GetSimulParam_EffectiveRain_Method())
        case (EffectiveRainMethod_Percentage)
            EffecRain = (GetSimulParam_EffectiveRain_PercentEffRain()/100._dp) &
                                * (GetRain()-GetRunoff())
        case (EffectiveRainMethod_USDA)
            ETcropMonth = ((GetEpot()+GetTpot())*30._dp)/25.4_dp ! inch/month
            RainMonth = ((GetRain()-GetRunoff())*30._dp)/25.4_dp ! inch/Month
            if (RainMonth > 0.1_dp) then
                EffecRain = (0.70917_dp*exp(0.82416_dp*log(RainMonth))-0.11556_dp) &
                                * (exp(0.02426_dp*ETcropMonth*log(10._dp))) 
                                                                 ! inch/month
            else
                EffecRain = RainMonth
            end if
            EffecRain = EffecRain*(25.4_dp/30._dp) ! mm/day
        end select
    end if
    if (EffecRain < 0._dp) then
        EffecRain = 0._dp
    end if
    if (EffecRain > (GetRain()-GetRunoff())) then
        EffecRain = (GetRain()-GetRunoff())
    end if
    SubDrain = (GetRain()-GetRunoff()) - EffecRain

    ! 2. Verify Possibility of SubDrain
    if (SubDrain > 0._dp) then
        DrainMax = GetSoilLayer_InfRate(1)
        if (GetSurfaceStorage() > 0._dp) then
            DrainMax = 0._dp
        else
            Zr = GetRootingDepth()
            if (Zr <= epsilon(0._dp)) then
                Zr = (GetSimulParam_EvapZmax()/100._dp)
            end if
            compi = 0
            depthi = 0._dp
            DTheta = (EffecRain/Zr)/1000._dp
            loop: do
                compi = compi + 1
                depthi = depthi + GetCompartment_Thickness(compi)
                RestTheta = GetSoilLayer_SAT(GetCompartment_Layer(compi)) &
                            /100._dp - (GetCompartment_theta(compi) + DTheta)
                if (RestTheta <= epsilon(0._dp)) then
                    DrainMax = 0._dp
                end if
                if (GetSoilLayer_InfRate(GetCompartment_Layer(compi)) &
                                                        < DrainMax) then
                    DrainMax = GetSoilLayer_InfRate(GetCompartment_Layer(compi))
                end if
                if ((depthi >= Zr) &
                    .or. (compi >= GetNrCompartments())) exit loop
            end do loop
        end if
        if (SubDrain > DrainMax) then
            if (GetManagement_Bundheight() < 0.001_dp) then
                call SetRunoff(GetRunoff() + (SubDrain-DrainMax))
            end if
            SubDrain = DrainMax
        end if
    end if
end subroutine CalculateEffectiveRainfall


subroutine calculate_Extra_runoff(InfiltratedRain, InfiltratedIrrigation, &
                                  InfiltratedStorage, SubDrain)
    real(dp), intent(inout) :: InfiltratedRain
    real(dp), intent(inout) :: InfiltratedIrrigation
    real(dp), intent(inout) :: InfiltratedStorage
    real(dp), intent(inout) :: SubDrain

    real(dp) :: FracSubDrain

    InfiltratedStorage = 0._dp
    InfiltratedRain = GetRain() - GetRunoff()
    if (InfiltratedRain > 0._dp) then
        FracSubDrain = SubDrain/InfiltratedRain
    else
        FracSubDrain = 0._dp
    end if
    if ((GetIrrigation()+InfiltratedRain) &
            > GetSoilLayer_InfRate(GetCompartment_Layer(1))) then
        if (GetIrrigation() > GetSoilLayer_InfRate(GetCompartment_Layer(1))) then
            InfiltratedIrrigation = GetSoilLayer_InfRate(GetCompartment_Layer(1))
            call SetRunoff(GetRain() + (GetIrrigation()-InfiltratedIrrigation))
            InfiltratedRain = 0._dp
            SubDrain = 0._dp
        else
            InfiltratedIrrigation = GetIrrigation()
            InfiltratedRain = GetSoilLayer_InfRate(GetCompartment_Layer(1)) &
                                - InfiltratedIrrigation
            SubDrain = FracSubDrain*InfiltratedRain
            call SetRunoff(GetRain() - InfiltratedRain)
        end if
    else
        InfiltratedIrrigation = GetIrrigation()
    end if
end subroutine calculate_Extra_runoff


subroutine calculate_surfacestorage(InfiltratedRain, InfiltratedIrrigation, &
                                    InfiltratedStorage, ECinfilt, SubDrain, &
                                    dayi)
    real(dp), intent(inout) :: InfiltratedRain
    real(dp), intent(inout) :: InfiltratedIrrigation
    real(dp), intent(inout) :: InfiltratedStorage
    real(dp), intent(inout) :: ECinfilt
    real(dp), intent(in) :: SubDrain
    integer(int32), intent(in) :: dayi

    real(dp) :: Sum
    real(dp) :: ECw

    InfiltratedRain = 0._dp
    InfiltratedIrrigation = 0._dp
    if (GetRainRecord_DataType() == datatype_Daily) then
        Sum = GetSurfaceStorage() + GetIrrigation() + GetRain()
    else
        Sum = GetSurfaceStorage() + GetIrrigation() + GetRain() &
              - GetRunoff() - SubDrain
    end if
    if (Sum > 0._dp) then
        ! quality of irrigation water
        if (dayi < GetCrop_Day1()) then
            ECw = GetIrriECw_PreSeason()
        else
            ECw = GetSimulation_IrriECw()
            if (dayi > GetCrop_DayN()) then
                ECw = GetIrriECw_PostSeason()
            end if
        end if
        ! quality of stored surface water
        call SetECstorage((GetECstorage() &
                            * GetSurfaceStorage() &
                            + ECw*GetIrrigation()) /Sum)
        ! quality of infiltrated water (rain and/or irrigation and/or stored surface water)
        ECinfilt = GetECstorage()
        ! surface storage
        if (Sum > GetSoilLayer_InfRate(GetCompartment_Layer(1))) then
            InfiltratedStorage = GetSoilLayer_InfRate(GetCompartment_Layer(1))
            call SetSurfaceStorage(Sum - InfiltratedStorage)
        else
            if (GetRainRecord_DataType() == datatype_Daily) then
                InfiltratedStorage = Sum
            else
                InfiltratedStorage = GetSurfaceStorage() + GetIrrigation()
                InfiltratedRain = GetRain() - GetRunoff()
            end if
            call SetSurfaceStorage(0._dp)
        end if
        ! extra run-off
        if (GetSurfaceStorage() > (GetManagement_BundHeight()*1000._dp)) then
            call SetRunoff(GetRunoff() &
                            + (GetSurfaceStorage() &
                            - GetManagement_BundHeight()*1000._dp))
            call SetSurfaceStorage(GetManagement_BundHeight()*1000._dp)
        end if
    else
        InfiltratedStorage = 0._dp
        call SetECstorage(0._dp)
    end if
end subroutine calculate_surfacestorage



subroutine calculate_infiltration(InfiltratedRain, InfiltratedIrrigation, &
                                  InfiltratedStorage, SubDrain)
    real(dp), intent(inout) :: InfiltratedRain
    real(dp), intent(inout) :: InfiltratedIrrigation
    real(dp), intent(inout) :: InfiltratedStorage
    real(dp), intent(inout) :: SubDrain

    integer(int32) :: compi, layeri, pre_comp
    real(dp) :: RunoffIni, amount_still_to_store, factor, &
                delta_theta_nul, delta_theta_SAT, theta_nul, &
                drain_max, diff, excess
    real(dp) :: EffecRain, Zr, depthi, DeltaZ, StorableMM


    ! calculate_infiltration 
    ! A -  INFILTRATION versus STORAGE in Rootzone (= EffecRain)
    if (GetRainRecord_DataType() == datatype_Daily) then
        amount_still_to_store = InfiltratedRain + InfiltratedIrrigation &
                                + InfiltratedStorage
        EffecRain = 0._dp
    else
        amount_still_to_store = InfiltratedIrrigation + InfiltratedStorage
        EffecRain = InfiltratedRain - SubDrain
    end if

    ! B - INFILTRATION through TOP soil surface
    if (amount_still_to_store > 0._dp) then
        RunoffIni = GetRunoff()
        compi = 0
        
        loop1: do
            compi = compi + 1
            layeri = GetCompartment_Layer(compi)
            
            !1. Calculate multiplication factor
            !====================================
            factor = calculate_factor(layeri, compi)
            
            !2. Calculate theta nul
            !========================
            delta_theta_nul = amount_still_to_store &
                             /(1000._dp * GetCompartment_Thickness(compi) &
                              * (1._dp-GetSoilLayer_GravelVol(layeri)/100._dp))
            delta_theta_SAT = calculate_delta_theta(&
                                GetSoilLayer_SAT(layeri)/100._dp, &
                                GetSoilLayer_FC(layeri)/100._dp, &
                                layeri)
            
            if (delta_theta_nul < delta_theta_SAT) then
                theta_nul = calculate_theta(delta_theta_nul, &
                                            GetSoilLayer_FC(layeri)/100._dp, &
                                            layeri)
                if (theta_nul <= (GetCompartment_FCadj(compi)/100._dp)) then
                    theta_nul = GetCompartment_FCadj(compi)/100._dp
                    delta_theta_nul = calculate_delta_theta( &
                                        theta_nul, &
                                        GetSoilLayer_FC(layeri)/100._dp, &
                                        layeri)
                end if
                if (theta_nul > GetSoilLayer_SAT(layeri)/100._dp) then
                    theta_nul = GetSoilLayer_SAT(layeri)/100._dp
                end if
            else
                theta_nul = GetSoilLayer_SAT(layeri)/100._dp
                delta_theta_nul = delta_theta_SAT
            end if
            
            
            !3. Calculate drain max
            !========================
            drain_max = factor * delta_theta_nul * 1000._dp &
                            * GetCompartment_Thickness(compi) &
                            * (1._dp-GetSoilLayer_GravelVol(layeri)/100._dp)
            if ((GetCompartment_fluxout(compi) + drain_max) &
                        > GetSoilLayer_InfRate(layeri)) then
                drain_max = GetSoilLayer_InfRate(layeri) &
                            - GetCompartment_fluxout(compi)
            end if
            
            
            !4. Store water
            !================
            diff = theta_nul - GetCompartment_theta(compi)
            if (diff > 0._dp) then
                call SetCompartment_theta(compi, GetCompartment_theta(compi) &
                           + amount_still_to_store &
                             /(1000._dp * GetCompartment_Thickness(compi) &
                                 * (1._dp &
                                    - GetSoilLayer_GravelVol(layeri)/100._dp)))
                if (GetCompartment_theta(compi) > theta_nul) then
                    amount_still_to_store = (GetCompartment_theta(compi) &
                                                - theta_nul) &
                               * 1000._dp &
                               * GetCompartment_Thickness(compi) &
                               * (1._dp-GetSoilLayer_GravelVol(layeri)/100._dp)
                    call SetCompartment_theta(compi, theta_nul)
                else
                    amount_still_to_store = 0.0_dp
                end if
            end if
            call SetCompartment_fluxout(compi, GetCompartment_fluxout(compi) &
                                               + amount_still_to_store)
            
            
            !5. Redistribute excess
            !========================
            excess = amount_still_to_store - drain_max
            if (excess < 0._dp) then
                excess = 0._dp
            end if
            amount_still_to_store = amount_still_to_store - excess
            
            if (excess > 0._dp) then
                pre_comp = compi + 1
                loop2: do
                    pre_comp = pre_comp - 1
                    layeri = GetCompartment_Layer(pre_comp)
                    call SetCompartment_fluxout(pre_comp, &
                                    GetCompartment_fluxout(pre_comp) &
                                                           - excess)
                    call SetCompartment_theta(&
                            pre_comp, GetCompartment_theta(pre_comp) &
                               + excess/(1000._dp &
                                  * GetCompartment_Thickness(pre_comp) &
                                  * (1._dp &
                        - GetSoilLayer_GravelVol(GetCompartment_Layer(pre_comp))&
                                                                    /100._dp)))
                    if (GetCompartment_theta(pre_comp) &
                            > GetSoilLayer_SAT(layeri)/100._dp) then
                        excess = (GetCompartment_theta(pre_comp) &
                            - GetSoilLayer_SAT(layeri)/100._dp) * 1000._dp &
                                * GetCompartment_Thickness(pre_comp) &
                                * (1._dp &
                        -GetSoilLayer_GravelVol(GetCompartment_Layer(pre_comp))&
                                                                      /100._dp)
                        call SetCompartment_theta( &
                                pre_comp, &
                                GetSoilLayer_SAT(layeri)/100._dp)
                    else
                        excess = 0.0_dp
                    end if
                    if ((excess == epsilon(0._dp)) .or. (pre_comp == 1)) exit loop2
                end do loop2
                if (excess > 0._dp) then
                    call SetRunoff(GetRunoff() + excess)
                end if
            end if
            
            if ((amount_still_to_store <= epsilon(0._dp)) &
                    .or. (compi == GetNrCompartments())) exit loop1
        end do loop1
        if (amount_still_to_store > 0._dp) then
            call SetDrain(GetDrain() + amount_still_to_store)
        end if
        
        !6. Adjust infiltrated water
        !=============================
        if (GetRunoff() > RunoffIni) then
            if (GetManagement_Bundheight() >= 0.01_dp) then
                call SetSurfaceStorage(GetSurfaceStorage() &
                                        + (GetRunoff() &
                                        - RunoffIni))
                InfiltratedStorage = InfiltratedStorage &
                                     - (GetRunoff()-RunoffIni)
                if (GetSurfaceStorage() &
                            > GetManagement_BundHeight()*1000._dp) then
                    call SetRunoff(RunoffIni &
                                   + (GetSurfaceStorage() &
                                        - GetManagement_BundHeight()*1000._dp))
                    call SetSurfaceStorage(GetManagement_BundHeight()*1000._dp)
                else
                    call SetRunoff(RunoffIni)
                end if
            else
                InfiltratedRain = InfiltratedRain - (GetRunoff()-RunoffIni)
                if (InfiltratedRain < 0._dp) then
                    InfiltratedIrrigation = InfiltratedIrrigation &
                                            + InfiltratedRain
                    InfiltratedRain = 0._dp
                end if
            end if
            
            ! INFILTRATION through TOP soil surface 
        end if
    end if
        
        
    ! C - STORAGE in Subsoil (= SubDrain)
    if (SubDrain > 0._dp) then
        amount_still_to_store = SubDrain
        
        ! Where to store 
        Zr = GetRootingDepth()
        if (Zr <= 0._dp) then
            Zr = GetSimulParam_EvapZmax()/100._dp
        end if
        compi = 0
        depthi = 0._dp
        loop3: do
            compi = compi + 1
            depthi = depthi + GetCompartment_Thickness(compi)
            if ((depthi >= Zr) &
                .or. (compi >= GetNrCompartments())) exit loop3
        end do loop3
        if (depthi > Zr) then
            DeltaZ = (depthi - Zr)
        else
            DeltaZ = 0._dp
        end if
        
        ! Store 
        do while((amount_still_to_store > 0._dp) &
                .and. ((compi < GetNrCompartments()) &
                    .or. (DeltaZ > 0._dp))) 
            if (DeltaZ == epsilon(0._dp)) then
                compi = compi + 1
                DeltaZ = GetCompartment_Thickness(compi)
            end if
            StorableMM = (GetSoilLayer_SAT(GetCompartment_Layer(compi))&
                                                            /100._dp &
                            - GetCompartment_Theta(compi)) * 1000._dp &
                                 * DeltaZ * (1._dp &
                     - GetSoilLayer_GravelVol(GetCompartment_Layer(compi))&
                                                                 /100._dp)
            if (StorableMM > amount_still_to_store) then
               call SetCompartment_theta(&
                      compi, &
                      GetCompartment_Theta(compi) &
                       + (amount_still_to_store)&
                          /(1000._dp*GetCompartment_Thickness(compi) &
                            * (1._dp &
                    - GetSoilLayer_GravelVol(GetCompartment_Layer(compi))&
                                                                /100._dp)))
                amount_still_to_store = 0._dp
            else
                amount_still_to_store = amount_still_to_store - StorableMM
                call SetCompartment_theta(&
                        compi, &
                        GetCompartment_Theta(compi) &
                        + (StorableMM)/(1000._dp &
                            * GetCompartment_Thickness(compi) &
                            * (1._dp &
                     - GetSoilLayer_GravelVol(GetCompartment_Layer(compi))&
                                                                /100._dp)))
            end if
            DeltaZ = 0._dp
            if (amount_still_to_store &
                  > GetSoilLayer_InfRate(GetCompartment_Layer(compi))) then
                SubDrain = SubDrain &
                            - (amount_still_to_store &
                        - GetSoilLayer_InfRate(GetCompartment_Layer(compi)))
                EffecRain = EffecRain &
                            + (amount_still_to_store &
                        - GetSoilLayer_InfRate(GetCompartment_Layer(compi)))
                amount_still_to_store = GetSoilLayer_InfRate(&
                                            GetCompartment_Layer(compi))
            end if
        end do
        
        ! excess 
        if (amount_still_to_store > 0._dp) then
            call SetDrain(GetDrain() + amount_still_to_store)
        end if
        ! STORAGE in Subsoil (= SubDrain) 
    end if
        
    ! D - STORAGE in Rootzone (= EffecRain)
    if (EffecRain > 0._dp) then
        Zr = GetRootingDepth()
        if (Zr <= epsilon(0._dp)) then
            Zr = GetSimulParam_EvapZmax()/100._dp
        end if
        amount_still_to_store = EffecRain
        
        ! Store 
        ! step 1 fill to FC (from top to bottom) 
        compi = 0
        depthi = 0._dp
        loop4: do
            compi = compi + 1
            depthi = depthi + GetCompartment_Thickness(compi)
            if (depthi <= Zr) then
                DeltaZ = GetCompartment_Thickness(compi)
            else
                DeltaZ = GetCompartment_Thickness(compi) &
                         - (depthi-Zr)
            end if
            StorableMM = (GetCompartment_FCadj(compi)/100._dp &
                            - GetCompartment_Theta(compi))*1000._dp*DeltaZ &
                            * (1._dp &
                      - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                                  /100._dp)
            if (StorableMM < 0._dp) then
                StorableMM = 0._dp
            end if
            if (StorableMM > amount_still_to_store) then
                call SetCompartment_theta(&
                        compi, &
                        GetCompartment_Theta(compi) &
                        + amount_still_to_store &
                            /(1000._dp*GetCompartment_Thickness(compi) &
                              *(1._dp &
                       - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                                 /100._dp)))
                amount_still_to_store = 0._dp
            elseif (StorableMM > 0._dp) then
                call SetCompartment_theta(&
                        compi, &
                        GetCompartment_Theta(compi) &
                        + StorableMM &
                            /(1000._dp*GetCompartment_Thickness(compi) &
                                * (1._dp &
                    - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                              /100._dp)))
                amount_still_to_store = amount_still_to_store - StorableMM
            end if
            if ((depthi >= Zr) &
                    .or. (compi >= GetNrCompartments()) &
                    .or. (amount_still_to_store <= epsilon(0._dp))) &
                            exit loop4
        end do loop4
        
        ! step 2 fill to SATURATION (from bottom to top) 
        if (amount_still_to_store > 0._dp) then
            loop5: do
                if (depthi > Zr) then
                    DeltaZ = GetCompartment_Thickness(compi) - (depthi-Zr)
                else
                    DeltaZ = GetCompartment_Thickness(compi)
                end if
                StorableMM = (GetSoilLayer_SAT(GetCompartment_Layer(compi)) &
                                                                   /100._dp &
                             - GetCompartment_Theta(compi))*1000._dp*DeltaZ &
                                * (1._dp &
                      - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                                  /100._dp)
                if (StorableMM < 0._dp) then
                    StorableMM = 0._dp
                end if
                if (StorableMM > amount_still_to_store) then
                    call SetCompartment_theta(&
                            compi, &
                            GetCompartment_theta(compi) &
                            + amount_still_to_store &
                                /(1000._dp*GetCompartment_Thickness(compi) &
                                    * (1._dp &
                      - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                                /100._dp)))
                    amount_still_to_store = 0._dp
                elseif (StorableMM > 0._dp) then
                    call SetCompartment_theta(&
                            compi, &
                            GetCompartment_Theta(compi) &
                            + StorableMM &
                                /(1000._dp*GetCompartment_Thickness(compi) &
                                    *(1._dp &
                      - GetSoilLayer_GravelVol(GetCompartment_Layer(compi)) &
                                                                /100._dp)))
                    amount_still_to_store = amount_still_to_store &
                                           - StorableMM
                end if
                compi = compi - 1
                depthi = depthi - GetCompartment_Thickness(compi)
                if ((compi == 0) &
                    .or. (amount_still_to_store <= epsilon(0._dp))) &
                            exit loop5
            end do loop5
        end if
        
        ! excess 
        if (amount_still_to_store > 0._dp) then
            if (InfiltratedRain > 0._dp) then
                InfiltratedRain = InfiltratedRain - amount_still_to_store
            end if
            if (GetManagement_Bundheight() >= 0.01_dp) then
                call SetSurfaceStorage(GetSurfaceStorage() &
                                      + amount_still_to_store)
                if (GetSurfaceStorage() &
                        > (GetManagement_BundHeight()*1000._dp)) then
                    call SetRunoff(GetRunoff() &
                                   + (GetSurfaceStorage() &
                                   - GetManagement_BundHeight()*1000._dp))
                    call SetSurfaceStorage(GetManagement_BundHeight() &
                                                            *1000._dp)
                end if
            else
                call SetRunoff(GetRunoff() + amount_still_to_store)
            end if
        end if
        ! STORAGE in Rootzone (= EffecRain) 
    end if


    contains

    real(dp) function Calculate_factor(layeri, compi)
        integer(int32), intent(in) :: layeri
        integer(int32), intent(in) :: compi

        real(dp) :: delta_theta_SAT
 
        delta_theta_SAT = calculate_delta_theta(GetSoilLayer_SAT(layeri)/100._dp, &
                                                GetSoilLayer_FC(layeri)/100._dp, &
                                                layeri)
        if (delta_theta_SAT > 0._dp) then
            Calculate_factor = GetSoilLayer_InfRate(layeri)&
                                /(delta_theta_SAT * 1000._dp &
                                    * GetCompartment_Thickness(compi) &
                                    * (1._dp-GetSoilLayer_GravelVol(layeri) &
                                                                    /100._dp))
        else
            Calculate_factor = 1._dp
        end if
    end function Calculate_factor 

end subroutine calculate_infiltration


subroutine DetermineCCiGDD(CCxTotal, CCoTotal, CCiActual, &
                           StressLeaf, FracAssim, MobilizationON, &
                           StorageON, SumGDDAdjCC, VirtualTimeCC, &
                           StressSenescence, TimeSenescence, NoMoreCrop, &
                           CDCTotal, CGCAdjustmentAfterCutting, GDDayFraction, &
                           GDDayi, GDDCDCTotal, GDDTadj)
    real(dp), intent(in) :: CCxTotal
    real(dp), intent(in) :: CCoTotal
    real(dp), intent(inout) :: CCiActual
    real(dp), intent(inout) :: StressLeaf
    real(dp), intent(in) :: FracAssim
    logical, intent(in) :: MobilizationON
    logical, intent(in) :: StorageON
    real(dp), intent(in) :: SumGDDAdjCC
    integer(int32), intent(in) :: VirtualTimeCC
    real(dp), intent(inout) :: StressSenescence
    real(dp), intent(inout) :: TimeSenescence
    logical, intent(inout) :: NoMoreCrop
    real(dp), intent(in) :: CDCTotal
    logical, intent(inout) :: CGCAdjustmentAfterCutting
    real(dp), intent(in) :: GDDayFraction
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: GDDCDCTotal
    integer(int32), intent(in) :: GDDTadj

    real(dp), parameter :: CCdormant = 0.05_dp

    real(dp) :: pLeafLLAct , GDDCGCadjusted, GDDCDCadjusted, &
                CCiSen, GDDtTemp, CCxSF, CGCGDDSF, CCxSFCD, &
                RatDGDD, KsRED, CCibis
    integer(int32) :: GDDtFinalCCx
    logical :: WithBeta
    logical :: TheSenescenceON

    real(dp) :: KsSen
        !! test Version 6.2
    real(dp) :: Crop_pLeafAct_temp
    real(dp) :: Crop_pSenAct_temp
    real(dp) :: Crop_CCxAdjusted_temp

    if ((SumGDDadjCC <= GetCrop_GDDaysToGermination()) &
          .or. (roundc(SumGDDadjCC, mold=1) > GetCrop_GDDaysToHarvest())) then
        CCiActual = 0._dp
    else
        ! growing season (once germinated)
        ! 1. find some parameters
        CGCGDDSF = GetCrop_GDDCGC() &
                    * (1._dp - GetSimulation_EffectStress_RedCGC()/100._dp)
        GDDCGCadjusted = CGCGDDSF
        
        RatDGDD = 1._dp
        if (GetCrop_GDDaysToFullCanopySF() < GetCrop_GDDaysToSenescence()) then
            RatDGDD = (GetCrop_DaysToSenescence() &
                        - GetCrop_DaysToFullCanopySF()) &
                      /(GetCrop_GDDaysToSenescence() &
                        - GetCrop_GDDaysToFullCanopySF())
        end if
        
        CCxSF = CCxTotal*(1._dp - GetSimulation_EffectStress_RedCCX()/100._dp)
        ! maximum canopy cover than can be reached 
        ! (considering soil fertility/salinity, weed stress)
        if (SumGDDadjCC <= GetCrop_GDDaysToFullCanopySF()) then
            CCxSFCD = CCxSF ! no canopy decline before max canopy can be reached
        else
            ! canopy decline due to soil fertility
            if (SumGDDadjCC < GetCrop_GDDaysToSenescence()) then
                CCxSFCD = CCiNoWaterStressSF(&
                            (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                            GetCrop_DaysToGermination(), &
                            GetCrop_DaysToFullCanopySF(), &
                            GetCrop_DaysToSenescence(), &
                            GetCrop_DaysToHarvest(), &
                            GetCrop_GDDaysToGermination(), &
                            GetCrop_GDDaysToFullCanopySF(), &
                            GetCrop_GDDaysToSenescence(), &
                            GetCrop_GDDaysToHarvest(), &
                            CCoTotal, CCxTotal, GetCrop_CGC(), &
                            GetCrop_GDDCGC(), CDCTotal, GDDCDCTotal, &
                            SumGDDadjCC, RatDGDD, &
                            GetSimulation_EffectStress_RedCGC(), &
                            GetSimulation_EffectStress_RedCCX(), &
                            GetSimulation_EffectStress_CDecline(), &
                            GetCrop_ModeCycle())
            else
                CCxSFCD = CCxSF &
                          - (RatDGDD &
                                * GetSimulation_EffectStress_CDecline()/100._dp) &
                          * (GetCrop_GDDaysToSenescence() &
                                - GetCrop_GDDaysToFullCanopySF())
            end if
            if (CCxSFCD < 0._dp) then
                CCxSFCD = 0._dp
            end if
        end if
        StressLeaf = undef_int
        if ((int(SumGDDadjCC, kind=int32) == GetCrop_GDDaysToGermination()) &
                .and. (GetCrop_DaysToCCini() == 0)) then
            call SetCCiPrev(CCoTotal)
        end if
        
        ! time of potential vegetative growth
        GDDtFinalCCx = GetCrop_GDDaysToSenescence() ! non determinant crop
        if ((GetCrop_subkind() == subkind_Grain) &
                .and. (GetCrop_DeterminancyLinked())) then
            ! determinancy
            ! reduce GDDtFinalCCx in f(determinancy of crop)
            if (GetCrop_DaysToCCini() /= 0) then
                ! regrowth
                GDDtFinalCCx = GetCrop_GDDaysToFullCanopy() &
                               + roundc(GDDayFraction &
                                        * (GetCrop_GDDaysToFlowering() &
                                            + (GetCrop_GDDLengthFlowering() &
                                                                    /2._dp) &
                                            + GDDTadj &
                                            + GetCrop_GDDaysToGermination() &
                                            - GetCrop_GDDaysToFullCanopy()), &
                                mold=1) ! slow down
            else
                ! sown or transplant
                GDDtFinalCCx = GetCrop_GDDaysToFlowering() &
                               + roundc(GetCrop_GDDLengthFlowering()/2._dp, &
                                                                    mold=1)
            end if
            if (GDDtFinalCCx > GetCrop_GDDaysToSenescence()) then
                GDDtFinalCCx = GetCrop_GDDaysToSenescence()
            end if
        end if
        
        ! Crop.pLeafAct and Crop.pSenAct for plotting root zone depletion in RUN
        Crop_pLeafAct_temp = GetCrop_pLeafAct()
        call AdjustpLeafToETo(GetETo(), Crop_pLeafAct_temp, pLeafLLAct)
        call SetCrop_pLeafAct(Crop_pLeafAct_temp)
        WithBeta = .true.
        Crop_pSenAct_temp = GetCrop_pSenAct()
        call AdjustpSenescenceToETo(GetETo(), TimeSenescence, WithBeta, Crop_pSenAct_temp)
        call SetCrop_pSenAct(Crop_pSenAct_temp)
        
        ! 2. Canopy can still develop (stretched to GDDtFinalCCx)
        if (SumGDDadjCC < GDDtFinalCCx) then
            ! Canopy can stil develop (stretched to GDDtFinalCCx)
            if ((GetCCiPrev() <= GetCrop_CCoAdjusted()) &
                .or. (SumGDDadjCC <= GDDayi) &
                .or. ((GetSimulation_ProtectedSeedling()) &
                        .and. (GetCCiPrev() <= (1.25_dp * CCoTotal)))) then 
                ! 2.a First day or very small CC as a result of senescence 
                ! (no adjustment for leaf stress)
                CGCadjustmentAfterCutting = .false.
                if (GetSimulation_ProtectedSeedling()) then
                    CCiActual = CanopyCoverNoStressSF(&
                                (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                                 GetCrop_DaysToGermination(), &
                                 GetCrop_DaysToSenescence(), &
                                 GetCrop_DaysToHarvest(), &
                                 GetCrop_GDDaysToGermination(), &
                                 GetCrop_GDDaysToSenescence(), &
                                 GetCrop_GDDaysToHarvest(), &
                                 CCoTotal, CCxTotal, GetCrop_CGC(), &
                                 CDCTotal, GetCrop_GDDCGC(), &
                                 GDDCDCadjusted, SumGDDadjCC, &
                                 GetCrop_ModeCycle(), &
                                 GetSimulation_EffectStress_RedCGC(), &    
                                 GetSimulation_EffectStress_RedCCX())
                    if (CCiActual > (1.25_dp * CCoTotal)) then
                        call SetSimulation_ProtectedSeedling(.false.)
                    end if
                else
                    CCiActual = GetCrop_CCoAdjusted() * exp(CGCGDDSF * GDDayi)
                end if
                ! 2.b CC > CCo
            else
                if (GetCCiPrev() < (0.97999_dp*CCxSF)) then
                    call DetermineGDDCGCadjusted(GDDCGCadjusted)
                    if (GDDCGCadjusted > 0.00000001_dp) then
                        ! Crop.GDDCGC or GDDCGCadjusted > 0
                        Crop_CCxAdjusted_temp = GetCrop_CCxAdjusted()
                        call DetermineCCxAdjusted(Crop_CCxAdjusted_temp)
                        call SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp)
                        if (GetCrop_CCxAdjusted() < 0._dp) then
                            CCiActual = GetCCiPrev()
                        elseif (abs(GetCCiPrev() - 0.97999_dp*CCxSF) &
                                    < 0.001_dp) then ! THEN CCiActual := CCxSF
                            CCiActual = CanopyCoverNoStressSF(&
                                (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                                GetCrop_DaysToGermination(), &
                                GetCrop_DaysToSenescence(), &
                                GetCrop_DaysToHarvest(), &
                                GetCrop_GDDaysToGermination(), &
                                GetCrop_GDDaysToSenescence(), &
                                GetCrop_GDDaysToHarvest(), &
                                CCoTotal, CCxTotal, GetCrop_CGC(), &
                                CDCTotal, GetCrop_GDDCGC(), GDDCDCadjusted, &
                                SumGDDadjCC, GetCrop_ModeCycle(), &
                                GetSimulation_EffectStress_RedCGC(), &
                                GetSimulation_EffectStress_RedCCX())
                        else
                            GDDtTemp = RequiredGDD(GetCCiprev(), &
                                                   GetCrop_CCoAdjusted(), &
                                                   GetCrop_CCxAdjusted(), &
                                                   GDDCGCadjusted)
                            if (GDDtTemp < 0._dp) then
                                CCiActual = GetCCiPrev()
                            else
                                GDDtTemp = GDDtTemp + GDDayi
                                CCiActual = CCatGDDTime(GDDtTemp, &
                                                        GetCrop_CCoAdjusted(), &
                                                        GDDCGCadjusted, &
                                                        GetCrop_CCxAdjusted())
                            end if
                        end if
                    else
                        ! GDDCGCadjusted = 0 - too dry for leaf expansion
                        CCiActual = GetCCiPrev()
                        if (CCiActual > GetCrop_CCoAdjusted()) then
                            call SetCrop_CCoAdjusted(CCoTotal)
                        else
                            call SetCrop_CCoAdjusted(CCiActual)
                        end if
                    end if
                else
                    CCiActual = CanopyCoverNoStressSF(&
                            (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                            GetCrop_DaysToGermination(), &
                            GetCrop_DaysToSenescence(), &
                            GetCrop_DaysToHarvest(), &
                            GetCrop_GDDaysToGermination(), &
                            GetCrop_GDDaysToSenescence(), &
                            GetCrop_GDDaysToHarvest(), &
                            CCoTotal, CCxTotal, GetCrop_CGC(), CDCTotal, &
                            GetCrop_GDDCGC(), GDDCDCadjusted, SumGDDadjCC, &
                            GetCrop_ModeCycle(), &
                            GetSimulation_EffectStress_RedCGC(), &
                            GetSimulation_EffectStress_RedCCX())
                    call SetCrop_CCoAdjusted(CCoTotal)
                    StressLeaf = -33._dp ! maximum canopy is reached;
                    CGCadjustmentAfterCutting = .false.   
                        ! no increase of Canopy development after Cutting
                end if
                if (CCiActual > CCxSFCD) then
                    CCiActual = CCxSFCD
                    StressLeaf = -33._dp ! maximum canopy is reached;
                    CGCadjustmentAfterCutting = .false.
                        ! no increase of Canopy development after Cutting
                end if
            end if
            call SetCrop_CCxAdjusted(CCiActual)
            
            ! 3. Canopy can no longer develop 
            ! (Mid-season (from tFinalCCx) or Late season stage)
        else
            StressLeaf = -33._dp ! maximum canopy is reached;
            CGCadjustmentAfterCutting = .false.   
                ! no increase of Canopy development after Cutting
            if (GetCrop_CCxAdjusted() < 0._dp) then
                call SetCrop_CCxAdjusted(GetCCiPrev())
            end if
            
            if (SumGDDadjCC < GetCrop_GDDaysToSenescence()) then ! mid-season
                if (GetCrop_CCxAdjusted() > 0.97999_dp*CCxSF) then
                    CCiActual = CanopyCoverNoStressSF(&
                                (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                                GetCrop_DaysToGermination(), &
                                GetCrop_DaysToSenescence(), &
                                GetCrop_DaysToHarvest(), &
                                GetCrop_GDDaysToGermination(), &
                                GetCrop_GDDaysToSenescence(), &
                                GetCrop_GDDaysToHarvest(), &
                                CCoTotal, CCxTotal, GetCrop_CGC(), &
                                CDCTotal, GetCrop_GDDCGC(), &
                                GDDCDCadjusted, SumGDDadjCC, &
                                GetCrop_ModeCycle(), &
                                GetSimulation_EffectStress_RedCGC(), &
                                GetSimulation_EffectStress_RedCCX())
                    call SetCrop_CCxAdjusted(CCiActual)
                else
                    CCiActual = CanopyCoverNoStressSF(&
                                (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                                GetCrop_DaysToGermination(), &
                                GetCrop_DaysToSenescence(), &
                                GetCrop_DaysToHarvest(), &
                                GetCrop_GDDaysToGermination(), &
                                GetCrop_GDDaysToSenescence(), &
                                GetCrop_GDDaysToHarvest(), &
                                CCoTotal, &
                                (GetCrop_CCxAdjusted() &
                                    /(1._dp &
                                       - GetSimulation_EffectStress_RedCCx() &
                                                                   /100._dp)), &
                                GetCrop_CGC(), CDCTotal, GetCrop_GDDCGC(), &
                                GDDCDCadjusted, SumGDDadjCC, &
                                GetCrop_ModeCycle(), &
                                GetSimulation_EffectStress_RedCGC(), &
                                GetSimulation_EffectStress_RedCCX())
                end if
                if (CCiActual > CCxSFCD) then
                    CCiActual = CCxSFCD
                end if
                ! late season
            else      
                StressSenescence = undef_int ! to avoid display of zero stress 
                                             ! in late season
                if (GetCrop_CCxAdjusted() > CCxSFCD) then
                    call SetCrop_CCxAdjusted(CCxSFCD)
                end if
                if (GetCrop_CCxAdjusted() < 0.01_dp) then
                    CCiActual = 0._dp
                else
                    ! calculate CC in late season
                    ! CCibis = CC which canopy declines 
                    ! (soil fertility/salinity stress) further in late season
                    CCibis = CCxSF &
                            - (RatDGDD*GetSimulation_EffectStress_CDecline() &
                                                                   /100._dp) &
                            * (exp(2._dp &
                                  * log(SumGDDadjCC &
                                        - GetCrop_GDDaysToFullCanopySF())) &
                                /(GetCrop_GDDaysToSenescence() &
                                    - GetCrop_GDDaysToFullCanopySF()))
                    if (CCibis < 0._dp) then
                        CCiActual = 0._dp
                    else
                        ! CCiActual = CC with natural senescence in late season
                        GDDCDCadjusted = GetGDDCDCadjustedNoStress(&
                                                    CCxTotal, &
                                                    GDDCDCTotal, &
                                                    GetCrop_CCxAdjusted())
                        if (SumGDDadjCC &
                                < (GetCrop_GDDaysToSenescence() &
                                    + LengthCanopyDecline(&
                                                    GetCrop_CCxAdjusted(), &
                                                    GDDCDCadjusted))) then
                            CCiActual = GetCrop_CCxAdjusted() &
                                 * (1._dp - 0.05_dp &
                                    * (exp((SumGDDadjCC &
                                            - GetCrop_GDDaysToSenescence()) &
                                          * 3.33_dp &
                                          * GDDCDCadjusted &
                                                /(GetCrop_CCxAdjusted() &
                                                            + 2.29_dp)) &
                                        - 1._dp))
                            ! CCiActual becomes CCibis, when canopy decline is more severe
                            if (CCibis < CCiActual) then
                                CCiActual = CCibis
                            end if
                        else
                            CCiActual = 0._dp
                        end if
                    end if
                end if
                ! late season
            end if
            ! 3. Canopy can no longer develop (Mid-season (from tFinalCCx) 
            ! or Late season stage)
        end if
        
        
        ! 4. Canopy senescence due to water stress ?
        if ((SumGDDadjCC < GetCrop_GDDaysToSenescence()) &
                            ! not yet late season stage
            .or. (TimeSenescence > 0._dp)) then 
            ! in late season with ongoing early senesence  
            ! (TimeSenescence in GDD)
            StressSenescence = 0._dp
            WithBeta = .true.
            Crop_pSenAct_temp = GetCrop_pSenAct()
            call AdjustpSenescenceToETo(GetETo(), TimeSenescence, &
                                        WithBeta, Crop_pSenAct_temp)
            call SetCrop_pSenAct(Crop_pSenAct_temp)
            KsRED = 1._dp ! effect of soil salinity 
                          ! on the threshold for senescence
            if (GetSimulation_SWCtopSoilConsidered()) then
                ! top soil is relative wetter than total root zone
                if ((GetRootZoneWC_ZtopAct() &
                        < (GetRootZoneWC_ZtopFC() &
                            - GetCrop_pSenAct() * KsRED &
                                * (GetRootZoneWC_ZtopFC() &
                                    - GetRootZoneWC_ZtopWP()))) &
                    .and. (GetSimulation_ProtectedSeedling() .eqv. .false.)) then
                    TheSenescenceON = .true.
                else
                    TheSenescenceON = .false.
                end if
            else
                if ((GetRootZoneWC_Actual() &
                        < (GetRootZoneWC_FC() &
                            - GetCrop_pSenAct() * KsRED &
                                * (GetRootZoneWC_FC() - GetRootZoneWC_WP()))) &
                    .and. (GetSimulation_ProtectedSeedling() .eqv. .false.)) then
                    TheSenescenceON = .true.
                else
                    TheSenescenceON = .false.
                end if
            end if
            
            if (TheSenescenceON) then
                ! CanopySenescence
                CGCadjustmentAfterCutting = .false.
                call SetSimulation_EvapLimitON(.true.) 
                ! consider withered crop when not yet in late season
                if (TimeSenescence == epsilon(0._dp)) then
                    call SetCCiTopEarlySen(CCiActual) ! CC before canopy decline
                end if
                TimeSenescence = TimeSenescence + GDDayi
                call DetermineGDDCDCadjustedWaterStress(GDDCDCadjusted, KsSen)
                if (GetCCiTopEarlySen() < 0.001_dp) then
                    if ((GetSimulation_SumEToStress() &
                            > GetCrop_SumEToDelaySenescence()) &
                      .or. (GetCrop_SumEToDelaySenescence() == epsilon(0._dp))) then
                        CCiSen = 0._dp ! no crop anymore
                    else
                        if (CCdormant > GetCrop_CCo()) then
                            CCiSen = GetCrop_CCo() &
                                    + (1._dp &
                                       - GetSimulation_SumEToStress() &
                                        / GetCrop_SumEToDelaySenescence()) &
                                    * (CCdormant - GetCrop_CCo())
                        else
                            CCiSen = GetCrop_CCo()
                        end if
                    end if
                else
                    if (((TimeSenescence*GDDCDCadjusted*3.33_dp) &
                                /(GetCCiTopEarlySen()+2.29_dp) > 100._dp) &
                        ! e power too large and in any case CCisen << 0
                        .or. (GetCCiprev() >= 1.05_dp * GetCCiTopEarlySen())) then 
                        ! Ln of negative or zero value
                        if ((GetSimulation_SumEToStress() &
                            > GetCrop_SumEToDelaySenescence()) &
                            .or. (GetCrop_SumEToDelaySenescence() == epsilon(0._dp))) then
                            CCiSen = 0._dp ! no crop anymore
                        else
                            if (CCdormant > GetCrop_CCo()) then
                                CCiSen = GetCrop_CCo() &
                                         + (1._dp &
                                            - GetSimulation_SumEToStress() &
                                                /GetCrop_SumEToDelaySenescence()) &
                                         * (CCdormant - GetCrop_CCo())
                            else
                                CCiSen = GetCrop_CCo()
                            end if
                        end if
                    else
                        ! GDDCDC is adjusted to degree of stress
                        ! time required to reach CCiprev with GDDCDCadjusted
                        GDDtTemp = (log(1._dp &
                                        + (1._dp - GetCCiprev()/GetCCiTopEarlySen()) &
                                                                     /0.05_dp)) &
                                    / (GDDCDCadjusted &
                                        * 3.33_dp/(GetCCiTopEarlySen() + 2.29_dp))
                        ! add 1 day to tTemp and calculate CCiSen with CDCadjusted
                        CCiSen = GetCCiTopEarlySen() &
                                * (1._dp &
                                    - 0.05_dp &
                                        * (exp((GDDtTemp+GDDayi) &
                                                * GDDCDCadjusted &
                                                * 3.33_dp &
                                                / (GetCCiTopEarlySen()+2.29_dp)) &
                                           -1._dp))
                    end if
                    if (CCiSen < 0._dp) then
                        CCiSen = 0._dp
                    end if
                    if ((GetCrop_SumEToDelaySenescence() > 0._dp) &
                        .and. (GetSimulation_SumEToStress() &
                                <= GetCrop_SumEToDelaySenescence())) then
                        if ((CCiSen < GetCrop_CCo()) &
                            .or. (CCiSen < CCdormant)) then
                            if (CCdormant > GetCrop_CCo()) then
                                CCiSen = GetCrop_CCo() &
                                         + (1._dp &
                                            - GetSimulation_SumEToStress() &
                                               /GetCrop_SumEToDelaySenescence()) &
                                            * (CCdormant - GetCrop_CCo())
                            else
                                CCiSen = GetCrop_CCo()
                            end if
                        end if
                    end if
                end if
                if (SumGDDadjCC < GetCrop_GDDaysToSenescence()) then
                    ! before late season
                    if (CCiSen > CCxSFCD) then
                        CCiSen = CCxSFCD
                    end if
                    CCiActual = CCiSen
                    if (CCiActual > GetCCiPrev()) then
                        CCiActual = GetCCiPrev() ! to avoid jump in CC
                    end if
                    ! when GDDCGCadjusted increases as a result of watering
                    call SetCrop_CCxAdjusted(CCiActual)
                    if (CCiActual < CCoTotal) then
                        call SetCrop_CCoAdjusted(CCiActual)
                    else
                        call SetCrop_CCoAdjusted(CCoTotal)
                    end if
                else
                    ! in late season
                    if (CCiSen < CCiActual) then
                        CCiActual = CCiSen
                    end if
                end if

                if ((roundc(10000._dp*CCiSen, mold=1) <= (10000._dp*CCdormant)) &
                    .or. (roundc(10000._dp*CCiSen, mold=1) &
                            <= roundc(10000._dp*GetCrop_CCo(), mold=1))) then
                    call SetSimulation_SumEToStress(GetSimulation_SumEToStress() &
                                                    + GetETo())
                end if
            else
                ! no water stress, resulting in canopy senescence
                if ((TimeSenescence > 0._dp) &
                    .and. (SumGDDadjCC > GetCrop_GDDaysToSenescence())) then
                    ! rewatering in late season of an early declining canopy
                    Crop_CCxAdjusted_temp = GetCrop_CCxAdjusted()
                    call GetNewCCxandGDDCDC(GetCCiprev(), GDDCDCTotal, &
                                            CCxSF, Crop_CCxAdjusted_temp, &
                                            GDDCDCadjusted)
                    call SetCrop_CCxAdjusted(Crop_CCxAdjusted_temp)
                    CCiActual = CanopyCoverNoStressSF(&
                                (VirtualTimeCC+GetSimulation_DelayedDays()+1), &
                                GetCrop_DaysToGermination(), &
                                GetCrop_DaysToSenescence(), &
                                GetCrop_DaysToHarvest(), &
                                GetCrop_GDDaysToGermination(), &
                                GetCrop_GDDaysToSenescence(), &
                                GetCrop_GDDaysToHarvest(), &
                                CCoTotal, &
                                (GetCrop_CCxAdjusted() &
                                    /(1._dp - GetSimulation_EffectStress_RedCCx() &
                                                                       /100._dp)), &
                                GetCrop_CGC(), CDCTotal, GetCrop_GDDCGC(), &
                                GDDCDCadjusted,SumGDDadjCC, &
                                GetCrop_ModeCycle(), &
                                GetSimulation_EffectStress_RedCGC(), &
                                GetSimulation_EffectStress_RedCCX())
                end if
                TimeSenescence = 0._dp  ! No early senescence or back to normal
                StressSenescence = 0._dp
                call SetSimulation_SumEToStress(0._dp)
            end if
        end if
        
        ! 5. Adjust Crop.CCxWithered - required for correction 
        ! of Transpiration of dying green canopy
        if (CCiActual > GetCrop_CCxWithered()) then
            call SetCrop_CCxWithered(CCiActual)
        end if
        
        ! 6. correction for late-season stage for rounding off errors
        if (SumGDDadjCC > GetCrop_GDDaysToSenescence()) then
            if (CCiActual > GetCCiprev()) then
                CCiActual = GetCCiprev()
            end if
            
            ! 7. no crop as a result of fertiltiy and/or water stress
            if (roundc(1000._dp*CCiActual, mold=1) <= 0) then
                NoMoreCrop = .true.
            end if
            
        end if 
    end if

    contains

    subroutine DetermineGDDCGCadjusted(GDDCGCadjusted)
        real(dp), intent(inout) :: GDDCGCadjusted

        real(dp) :: Wrelative
        real(dp) :: KsLeaf, MaxVal
        real(dp) :: SWCeffectiveRootZone, FCeffectiveRootZone, &
                    WPeffectiveRootZone

        ! determine FC and PWP
        if (GetSimulation_SWCtopSoilConsidered()) then
            ! top soil is relative wetter than total root zone
            SWCeffectiveRootZone = GetRootZoneWC_ZtopAct()
            Wrelative = (GetRootZoneWC_ZtopFC() &
                         - GetRootZoneWC_ZtopAct()) &
                            /(GetRootZoneWC_ZtopFC() - GetRootZoneWC_ZtopWP()) 
                                                                    ! top soil
            FCeffectiveRootZone = GetRootZoneWC_ZtopFC()
            WPeffectiveRootZone = GetRootZoneWC_ZtopWP()
        else
            SWCeffectiveRootZone = GetRootZoneWC_Actual()
            Wrelative = (GetRootZoneWC_FC() - GetRootZoneWC_Actual()) &
                            /(GetRootZoneWC_FC() - GetRootZoneWC_WP()) 
                                                        ! total root zone
            FCeffectiveRootZone = GetRootZoneWC_FC()
            WPeffectiveRootZone = GetRootZoneWC_WP()
        end if

        ! Canopy stress and effect of water stress on CGCGDD
        if (SWCeffectiveRootZone >= FCeffectiveRootZone) then
            GDDCGCadjusted = CGCGDDSF
            StressLeaf = 0._dp
        else
            if (SWCeffectiveRootZone <= WPeffectiveRootZone) then
                GDDCGCadjusted = 0._dp
                StressLeaf = 100._dp
            else
                if (Wrelative <= GetCrop_pLeafAct()) then
                    GDDCGCadjusted = CGCGDDSF
                    StressLeaf = 0._dp
                elseif (Wrelative >= pLeafLLAct) then
                    GDDCGCadjusted = 0._dp
                    StressLeaf = 100._dp
                else
                    Crop_pLeafAct_temp = GetCrop_pLeafAct()
                    KsLeaf = KsAny(Wrelative, Crop_pLeafAct_temp, &
                                   pLeafLLAct, GetCrop_KsShapeFactorLeaf())
                    call SetCrop_pLeafAct(Crop_pLeafAct_temp)
                    GDDCGCadjusted = CGCGDDSF * KsLeaf
                    StressLeaf = 100._dp * (1._dp - KsLeaf)
                end if
            end if
        end if

        ! effect of transfer of assimilates on CGCGDD
        if ((GDDCGCadjusted > 0.000001_dp) & ! CGCGDD can be adjusted
            .and. (((GetCrop_subkind() == subkind_Forage) &
                    .and. ((StorageON) .or. (MobilizationON))) & 
                                        ! transfer assimilates
                    .or. (CGCadjustmentAfterCutting))) then 
                    ! increase of Canopy development after Cutting
            ! decrease CGC during storage
            if ((GetCrop_subkind() == subkind_Forage) .and. (StorageON)) then
                GDDCGCadjusted = GDDCGCadjusted * (1._dp - FracAssim)
            end if
            ! increase CGC after cutting
            if ((CGCadjustmentAfterCutting) &
                    .and. (StorageON .eqv. .false.)) then
                GDDCGCadjusted = GDDCGCadjusted &
                                * (1._dp + GetManagement_Cuttings_CGCPlus() &
                                                                  /100._dp)
            end if
            ! increase CGC during mobilization
            if ((GetCrop_subkind() == subkind_Forage) &
                    .and. (MobilizationON) &
                    .and. (CGCadjustmentAfterCutting .eqv. .false.)) then
                if ((CCxSFCD <= epsilon(0._dp)) &
                    .or. (GetCCiPrev() >= 0.9*CCxSFCD)) then
                    MaxVal = 0._dp
                else
                    MaxVal = (1._dp - GetCCiPrev()/(0.9_dp*CCxSFCD))
                    if (MaxVal > 1._dp) then
                        MaxVal = 1._dp
                    end if
                    if (MaxVal < 0._dp) then
                        MaxVal = 0._dp
                    end if
                end if
                if (MaxVal > (FracAssim/2._dp)) then
                    MaxVal = FracAssim/2._dp
                end if
                GDDCGCadjusted = GDDCGCadjusted * (1._dp + Maxval)
            end if
        end if
    end subroutine DetermineGDDCGCadjusted



    real(dp) function RequiredGDD(CCiToFind, CCo, CCx, GDDCGCadjusted)
        real(dp), intent(in) :: CCiToFind
        real(dp), intent(in) :: CCo
        real(dp), intent(in) :: CCx
        real(dp), intent(in) :: GDDCGCadjusted

        real(dp) :: GDDCGCx

        ! Only when SumGDDadj > GDDayi
        ! and CCx < CCiToFind
        ! 1. GDDCGCx to reach CCiToFind on previous day (= SumGDDadj - GDDayi )
        if (CCiToFind <= CCx/2._dp) then
            GDDCGCx = (log(CCiToFind/CCo))/(SumGDDadjCC-GDDayi)
        else
            GDDCGCx = (log((0.25_dp*CCx*CCx/CCo) &
                                /(CCx-CCiToFind)))/(SumGDDadjCC-GDDayi)
        end if
        ! 2. Required GDD
        RequiredGDD = (SumGDDadjCC-GDDayi) * GDDCGCx/GDDCGCadjusted
    end function RequiredGDD



    real(dp) function CCatGDDTime(GDDtfictive, CCoGiven, GDDCGCGiven, CCxGiven)
        real(dp), intent(in) :: GDDtfictive
        real(dp), intent(in) :: CCoGiven
        real(dp), intent(in) :: GDDCGCGiven
        real(dp), intent(in) :: CCxGiven

        real(dp) :: CCi

        CCi = CCoGiven * exp(GDDCGCGiven * GDDtfictive)
        if (CCi > CCxGiven/2._dp) then
            CCi = CCxGiven &
                  - 0.25_dp * (CCxGiven/CCoGiven) &
                            * CCxGiven &
                            * exp(-GDDCGCGiven*GDDtfictive)
        end if
        CCatGDDTime = CCi
    end function CCatGDDTime


    subroutine DetermineCCxAdjusted(CCxAdjusted)
        real(dp), intent(inout) :: CCxAdjusted

        real(dp) :: GDDtfictive

        ! 1. find time (GDDtfictive) required to reach CCiPrev 
        ! (CCi of previous day) with GDDCGCadjusted
        GDDtfictive = RequiredGDD(GetCCiprev(), GetCrop_CCoAdjusted(), &
                                  CCxSF, GDDCGCadjusted)

        ! 2. Get CCxadjusted (reached at end of stretched crop development)
        if (GDDtfictive > 0._dp) then
            GDDtfictive = GDDtfictive &
                          + (GDDtFinalCCx - SumGDDadjCC) &
                          + GDDayi
            CCxAdjusted = CCatGDDTime(GDDtfictive, GetCrop_CCoAdjusted(), &
                                      GDDCGCadjusted, CCxSF)
        else
            CCxAdjusted = undef_double ! this means CCiActual := CCiPrev
        end if
    end subroutine DetermineCCxAdjusted



    real(dp) function GetGDDCDCadjustedNoStress(CCx, GDDCDC, CCxAdjusted)
        real(dp), intent(in) :: CCx
        real(dp), intent(in) :: GDDCDC
        real(dp), intent(in) :: CCxAdjusted

        real(dp) :: GDDCDCadjusted

        GDDCDCadjusted = GDDCDC * ((CCxadjusted+2.29_dp)/(CCx+2.29_dp))
        GetGDDCDCadjustedNoStress = GDDCDCadjusted
    end function GetGDDCDCadjustedNoStress


    subroutine DetermineGDDCDCadjustedWaterStress(GDDCDCadjusted, KsSen)
        real(dp), intent(inout) :: GDDCDCadjusted
        real(dp), intent(inout) :: KsSen

        real(dp) :: Wrelative
            !! KsSen : double;
        real(dp) :: pSenLL
        real(dp) :: pSenAct
        logical :: WithBeta

        pSenLL = 0.999_dp ! WP
        if (GetSimulation_SWCtopSoilConsidered()) then 
        ! top soil is relative wetter than total root zone
            Wrelative = (GetRootZoneWC_ZtopFC() - GetRootZoneWC_ZtopAct()) &
                        /(GetRootZoneWC_ZtopFC() - GetRootZoneWC_ZtopWP()) 
                                                                ! top soil
        else
            Wrelative = (GetRootZoneWC_FC() - GetRootZoneWC_Actual()) &
                        /(GetRootZoneWC_FC() - GetRootZoneWC_WP()) 
                                                ! total root zone
        end if

        WithBeta = .false.
        call AdjustpSenescenceToETo(GetETo(), TimeSenescence, &
                                    WithBeta, pSenAct)
        if (Wrelative <= pSenAct) then
            GDDCDCadjusted = 0.0001_dp ! extreme small decline
            StressSenescence = 0._dp
            KsSen = 1._dp
        elseif (Wrelative >= pSenLL) then
            GDDCDCadjusted = GDDCDCTotal &
                            * ((CCxSFCD+2.29_dp)/(CCxTotal+2.29_dp)) 
                                                        ! full speed
            StressSenescence = 100._dp
            KsSen = 0._dp
        else
            KsSen = KsAny(Wrelative, pSenAct, pSenLL, &
                          GetCrop_KsShapeFactorSenescence())
            if (KsSen > 0.000001_dp) then
                GDDCDCadjusted = GDDCDCTotal &
                                 * ((CCxSFCD+2.29_dp)/(CCxTotal+2.29_dp)) &
                                 * (1._dp - exp(8._dp*log(KsSen)))
                StressSenescence = 100._dp * (1._dp - KsSen)
            else
                GDDCDCadjusted = 0._dp
                StressSenescence = 0._dp
            end if
        end if
    end subroutine DetermineGDDCDCadjustedWaterStress



    subroutine GetNewCCxandGDDCDC(CCiPrev, GDDCDC, CCx, CCxAdjusted, &
                                 GDDCDCadjusted)
        real(dp), intent(in) :: CCiPrev
        real(dp), intent(in) :: GDDCDC
        real(dp), intent(in) :: CCx
        real(dp), intent(inout) :: CCxAdjusted
        real(dp), intent(inout) :: GDDCDCadjusted

        CCxAdjusted = CCiPrev &
                        /(1._dp - 0.05_dp &
                                *(exp((SumGDDadjCC - GDDayi &
                                       - GetCrop_GDDaysToSenescence()) &
                                      * GDDCDC * 3.33_dp/(CCX+2.29_dp))-1._dp))
        GDDCDCadjusted = GDDCDC * (CCxAdjusted+2.29_dp)/(CCx+2.29_dp)
    end subroutine GetNewCCxandGDDCDC

end subroutine DetermineCCiGDD




!-----------------------------------------------------------------------------
! end BUDGET_module
!-----------------------------------------------------------------------------

end module ac_simul
