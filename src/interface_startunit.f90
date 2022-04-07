module ac_interface_startunit

use, intrinsic :: iso_c_binding, only: c_ptr

use ac_interface_global, only: pointer2string, &
                               string2pointer

use ac_kinds, only: int32

use ac_startunit, only: GetRequestDailyResults, &
                        GetRequestParticularResults, &
                        fProjects_open, &
                        fProjects_write

implicit none


contains

subroutine fProjects_open_wrap(filename_ptr, filename_len, mode_ptr, mode_len)
    type(c_ptr), intent(in) :: filename_ptr
    integer(int32), intent(in) :: filename_len
    type(c_ptr), intent(in) :: mode_ptr
    integer(int32), intent(in) :: mode_len

    character(len=filename_len) :: filename
    character(len=mode_len) :: mode

    filename = pointer2string(filename_ptr, filename_len)
    mode = pointer2string(mode_ptr, mode_len)
    call fProjects_open(filename, mode)
end subroutine fProjects_open_wrap


subroutine fProjects_write_wrap(line_ptr, line_len, advance)
    type(c_ptr), intent(in) :: line_ptr
    integer(int32), intent(in) :: line_len
    logical(1), intent(in) :: advance

    character(len=line_len) :: line
    logical :: advance_f

    line = pointer2string(line_ptr, line_len)
    advance_f = advance
    call fProjects_write(line, advance_f)
end subroutine fProjects_write_wrap

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
