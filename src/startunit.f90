module ac_startunit

use ac_kinds, only: int32,&
                    int8

use iso_fortran_env, only: iostat_end

use ac_global, only: GetPathNameSimul, &
                     FileExists

implicit none


contains

subroutine GetRequestDailyResults(Out1Wabal, Out2Crop, Out3Prof, Out4Salt, &
                                    Out5CompWC, Out6CompEC, Out7Clim, OutDaily)
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
    if ((Out1Wabal) .or. (Out2Crop ) .or. (Out3Prof) .or. (Out4Salt) &
            .or. (Out5CompWC) .or. (Out6CompEC) .or. (Out7Clim) ) then
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

subroutine GetTimeAggregationResults(OutputAggregate)
    integer(int8), intent(inout) :: OutputAggregate

    character(len=:), allocatable :: FullFileName , TempString
    integer(int32) :: f0, rc
    integer(int32) :: n, i
    logical :: file_exists

    OutputAggregate = 0_int8 ! simulation period 0: season
    FullFileName = trim(GetPathNameSimul()//'AggregationResults.SIM')
    inquire(file=FullFileName, exist=file_exists)
    if (file_exists) then
        open(newunit=f0, file=trim(FullFileName), &
                  status='old', action='read', iostat=rc)
        read(f0, *, iostat=rc) TempString
        n = len(TempString)
        if (n > 0) then
            i = 1
            do while ((TempString(i:i) == ' ') .and. (i < n))
                i = i + 1
            end do
            if (TempString(i:i) == '1') then
                OutputAggregate = 1_int8 ! 1: daily aggregation
            else
                if (TempString(i:i) == '2') then
                    OutputAggregate = 2_int8 ! 2 : 10-daily aggregation
                else
                    if (TempString(i:i) == '3') then
                        OutputAggregate = 3_int8 ! 3 : monthly aggregation
                    else
                        OutputAggregate = 0_int8 ! 0 : seasonal results only
                    end if
                end if
            end if
        end if
        close(f0)
    end if
end subroutine GetTimeAggregationResults


end module ac_startunit
