module ac_interface_simul
use ac_kinds, only: dp
use ac_simul, only: AdjustpSenescenceToETo


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

end module ac_interface_simul
