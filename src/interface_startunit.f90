module ac_interface_startunit


use ac_startunit, only: GetRequestDailyResults, &
                        GetRequestParticularResults

implicit none


contains


subroutine GetRequestDailyResults_wrap(Out1Wabal, Out2Crop, Out3Prof, &
                          Out4Salt, Out5CompWC, Out6CompEC, Out7Clim, OutDaily)
    logical, intent(inout) :: Out1Wabal
    logical, intent(inout) :: Out2Crop
    logical, intent(inout) :: Out3Prof
    logical, intent(inout) :: Out4Salt
    logical, intent(inout) :: Out5CompWC
    logical, intent(inout) :: Out6CompEC
    logical, intent(inout) :: Out7Clim
    logical, intent(inout) :: OutDaily

    logical :: Out1Wabal_f,Out2Crop_f,Out3Prof_f,Out4Salt_f,Out5CompWC_f, &
               Out6CompEC_f, Out7Clim_f, OutDaily_f

    Out1Wabal_f = Out1Wabal
    Out2Crop_f = Out2Crop
    Out3Prof_f = Out3Prof
    Out4Salt_f = Out4Salt
    Out5CompWC_f = Out5CompWC
    Out6CompEC_f = Out6CompEC
    Out7Clim_f = Out7Clim
    OutDaily_f = OutDaily

    call GetRequestDailyResults(Out1Wabal_f, Out2Crop_f, Out3Prof_f, &
                                    Out4Salt_f, Out5CompWC_f, Out6CompEC_f, &
                                                    Out7Clim_f, OutDaily_f)
    Out1Wabal = Out1Wabal_f
    Out2Crop = Out2Crop_f
    Out3Prof = Out3Prof_f
    Out4Salt = Out4Salt_f
    Out5CompWC = Out5CompWC_f
    Out6CompEC = Out6CompEC_f
    Out7Clim = Out7Clim_f
    OutDaily = OutDaily_f
end subroutine GetRequestDailyResults_wrap

subroutine GetRequestParticularResults_wrap(Part1Mult, Part2Eval)
    logical, intent(inout) :: Part1Mult
    logical, intent(inout) :: Part2Eval

    logical ::  Part1Mult_f, Part2Eval_f

    Part1Mult_f = Part1Mult
    Part2Eval_f = Part2Eval

    call GetRequestParticularResults(Part1Mult_f, Part2Eval_f)

    Part1Mult = Part1Mult_f
    Part2Eval = Part2Eval_f
end subroutine GetRequestParticularResults_wrap

end module ac_interface_startunit
