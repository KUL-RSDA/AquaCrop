module ac_startunit

use ac_kinds, only: int32

use ac_global, only: GetPathNameSimul, &
                     FileExists

use iso_fortran_env, only: iostat_end

implicit none


contains

subroutine GetRequestDailyResults(Out1Wabal, Out2Crop, Out3Prof, Out4Salt, Out5CompWC, Out6CompEC, Out7Clim, OutDaily)
    logical, intent(inout) :: Out1Wabal
    logical, intent(inout) :: Out2Crop
    logical, intent(inout) :: Out3Prof
    logical, intent(inout) :: Out4Salt
    logical, intent(inout) :: Out5CompWC
    logical, intent(inout) :: Out6CompEC
    logical, intent(inout) :: Out7Clim
    logical, intent(inout) :: OutDaily

    integer :: fhandle, rc

    character(len= 1025) :: FullFileName, TempString
    integer(int32) :: n, i

    Out1Wabal = .false.
    Out2Crop = .false.
    Out3Prof = .false.
    Out4Salt = .false.
    Out5CompWC = .false.
    Out6CompEC = .false.
    Out7Clim = .false.

    FullFileName = GetPathNameSimul() // 'DailyResults.SIM'
    if (FileExists(FullFileName) .eqv. .true.) then
        open(newunit=fhandle, file=trim(FullFileName), status='old', action='read')
        loop: do 
            read(fhandle, *,iostat=rc) TempString
            n = len(TempString)
            if (n > 0) then
                i = 1
                do while ((TempString(i:i) == ' ') .and. (i < n))
                    i = i + 1
                end do
                if (TempString(i:i) == '1') then
                    Out1Wabal = .true.
                end if
                if (TempString(i:i) == '2') then
                    Out2Crop = .true.
                end if
                if (TempString(i:i) == '3') then
                    Out3Prof = .true.
                end if
                if (TempString(i:i) == '4') then
                    Out4Salt = .true.
                end if
                if (TempString(i:i) == '5') then
                    Out5CompWC = .true.
                end if
                if (TempString(i:i) == '6') then
                    Out6CompEC = .true.
                end if
                if (TempString(i:i) == '7') then
                    Out7Clim = .true.
                end if
            end if
            if (rc == iostat_end) exit loop
        end do loop
        close(fhandle)
    end if
    if ((Out1Wabal .eqv. .true.) .or. (Out2Crop .eqv. .true.) &
        .or. (Out3Prof .eqv. .true.) .or. (Out4Salt .eqv. .true.) &
            .or. (Out5CompWC .eqv. .true.) .or. (Out6CompEC .eqv. .true.) &
                .or. (Out7Clim .eqv. .true.) ) then
        OutDaily = .true.
    else
        OutDaily = .false.
    end if
end subroutine GetRequestDailyResults

subroutine GetRequestParticularResults(Part1Mult, Part2Eval)
    logical, intent(inout) :: Part1Mult
    logical, intent(inout) :: Part2Eval


    integer :: fhandle, rc

    character(len= 1025) :: FullFileName, TempString
    integer(int32) :: n, i

    Part1Mult = .false.
    Part2Eval = .false.

    FullFileName = GetPathNameSimul() // 'ParticularResults.SIM'
    if (FileExists(FullFileName) .eqv. .true.) then
        open(newunit=fhandle, file=trim(FullFileName), status='old', action='read')
        loop: do
            read(fhandle, *,iostat=rc) TempString
            n = len(TempString)
            if (n > 0) then
                i = 1
                do while ((TempString(i:i) == ' ') .and. (i < n))
                    i = i + 1
                end do
                if (TempString(i:i) == '1') then
                    Part1Mult = .true.
                end if
                if (TempString(i:i) == '2') then
                    Part2Eval = .true.
                end if
            end if
        if (rc == iostat_end) exit loop
        end do loop
        close(fhandle)
    end if
end subroutine GetRequestParticularResults


end module ac_startunit
