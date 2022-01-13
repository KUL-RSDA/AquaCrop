module ac_simul

use ac_kinds, only:  dp, &
                    int8, &
                    int32, &
                    sp
implicit none



contains


real(dp) function GetCDCadjustedNoStressNew(CCx, CDC, CCxAdjusted)
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: CCxAdjusted

    real(dp) :: CDCadjusted

    ! CDCadjusted := CDC * (CCxadjusted/CCx);
    CDCadjusted = CDC * ((CCxadjusted+2.29_dp)/(CCx+2.29_dp))
    GetCDCadjustedNoStressNew = CDCadjusted
    
end function GetCDCadjustedNoStressNew

end module ac_simul
