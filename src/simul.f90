module ac_simul

use ac_kinds, only:  dp, int16, int32
use ac_global, only: CompartmentIndividual, &
                     GetCompartment_Thickness, &
                     GetCompartment_WFactor, &
                     GetCrop_pLeafDefLL, GetCrop_pLeafDefUL, &
                     GetCrop_pMethod, pMethod_FAOCorrection, &
                     GetSimulParam_pAdjFAO, &
                     GetSoilLayer_FC, &
                     GetSoilLayer_SAT, &
                     GetSoilLayer_tau, &
                     max_No_compartments, &
                     roundc, &
                     SetCompartment_WFactor

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


real(dp) function calculate_delta_theta(theta_in, thetaAdjFC, NrLayer_in)
    real(dp), intent(in) :: theta_in
    real(dp), intent(in) :: thetaAdjFC
    integer(int16), intent(in) :: NrLayer_in

    integer(int32) :: NrLayer
    real(dp) :: DeltaX, theta

    NrLayer = int(NrLayer_in, kind=int32)
    theta = theta_in
    if (theta > GetSoilLayer_SAT(NrLayer)/100.0_dp) then
        theta = GetSoilLayer_SAT(NrLayer)/100.0_dp
    end if
    if (theta <= thetaAdjFC/100.0_dp) then
        DeltaX = 0.0_dp
    else
        DeltaX = GetSoilLayer_tau(NrLayer) * (GetSoilLayer_SAT(NrLayer)/100.0_dp - GetSoilLayer_FC(NrLayer)/100.0_dp) &
        * (exp(theta - GetSoilLayer_FC(NrLayer)/100.0_dp) - 1.0_dp) &
        / (exp(GetSoilLayer_SAT(NrLayer)/100.0_dp - GetSoilLayer_FC(NrLayer)/100.0_dp) - 1.0_dp)
        if ((theta - DeltaX) < thetaAdjFC) then
            DeltaX = theta - thetaAdjFC
        end if
    end if
    calculate_delta_theta = DeltaX
end function calculate_delta_theta


real(dp) function calculate_theta(delta_theta, thetaAdjFC, NrLayer_in)
    real(dp), intent(in) :: delta_theta
    real(dp), intent(in) :: thetaAdjFC
    integer(int16), intent(in) :: NrLayer_in

    integer(int32) :: NrLayer
    real(dp) :: ThetaX

    NrLayer = int(NrLayer_in, kind=int32)
    if (delta_theta <= 0.0_dp) then
        ThetaX = thetaAdjFC
    elseif (GetSoilLayer_tau(NrLayer) > 0.0_dp) then
        ThetaX = GetSoilLayer_FC(NrLayer)/100.0_dp + log(1.0_dp + &
                     delta_theta * (exp(GetSoilLayer_SAT(NrLayer)/100.0_dp - GetSoilLayer_FC(NrLayer)/100.0_dp) - 1.0_dp) &
                     / (GetSoilLayer_tau(NrLayer) * (GetSoilLayer_SAT(NrLayer)/100.0_dp - GetSoilLayer_FC(NrLayer)/100.0_dp)))
        if (ThetaX < thetaAdjFC) then
            ThetaX = thetaAdjFC
        end if
    else
        ThetaX = GetSoilLayer_SAT(NrLayer)/100.0_dp + 0.1_dp     ! to stop draining
    end if
    calculate_theta = ThetaX
end function calculate_theta

end module ac_simul
