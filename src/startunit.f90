module ac_startunit

use ac_kinds, only: int32,&
                    int8

use iso_fortran_env, only: iostat_end

use ac_global, only: GetPathNameSimul, &
                     FileExists, &
                     SetOut1Wabal, &
                     SetOut2Crop, &
                     SetOut3Prof, &
                     SetOut4Salt, &
                     SetOut5CompWC, &
                     SetOut6CompEC, &
                     SetOut7Clim, &
                     SetOutDaily, &
                     SetPart1Mult, &
                     SetPart2Eval, &
                     SetOutputAggregate, &
                     GetOut1Wabal, &
                     GetOut2Crop, &
                     GetOut3Prof, &
                     GetOut4Salt, &
                     GetOut5CompWC, &
                     GetOut6CompEC, &
                     GetOut7Clim

use ac_run, only: open_file, &
                  write_file

implicit none

integer :: fProjects  ! file handle
integer :: fProjects_iostat  ! IO status

contains


!! Section for Getters and Setters for global variables

! fProjects

subroutine fProjects_open(filename, mode)
    !! Opens the given file, assigning it to the 'fProjects' file handle.
    character(len=*), intent(in) :: filename
        !! name of the file to assign the file handle to
    character, intent(in) :: mode
        !! open the file for reading ('r'), writing ('w') or appending ('a')

    call open_file(fProjects, filename, mode, fProjects_iostat)
end subroutine fProjects_open


subroutine fProjects_write(line, advance_in)
    !! Writes the given line to the fProjects file.
    character(len=*), intent(in) :: line
        !! line to write
    logical, intent(in), optional :: advance_in
        !! whether or not to append a newline character

    logical :: advance

    if (present(advance_in)) then
        advance = advance_in
    else
        advance = .true.
    end if
    call write_file(fProjects, line, advance, fProjects_iostat)
end subroutine fProjects_write


subroutine fProjects_close()
    close(fProjects)
end subroutine fProjects_close


subroutine GetRequestDailyResults()

    integer :: fhandle, rc
    character(len= 1025) :: FullFileName, TempString
    integer(int32) :: n, i

    call SetOut1Wabal(.false.)
    call SetOut2Crop(.false.)
    call SetOut3Prof(.false.)
    call SetOut4Salt(.false.)
    call SetOut5CompWC(.false.)
    call SetOut6CompEC(.false.)
    call SetOut7Clim(.false.)

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
                    call SetOut1Wabal(.true.)
                end if
                if (TempString(i:i) == '2') then
                    call SetOut2Crop(.true.)
                end if
                if (TempString(i:i) == '3') then
                    call SetOut3Prof(.true.)
                end if
                if (TempString(i:i) == '4') then
                    call SetOut4Salt(.true.)
                end if
                if (TempString(i:i) == '5') then
                    call SetOut5CompWC(.true.)
                end if
                if (TempString(i:i) == '6') then
                    call SetOut6CompEC(.true.)
                end if
                if (TempString(i:i) == '7') then
                    call SetOut7Clim(.true.)
                end if
            end if
            if (rc == iostat_end) exit loop
        end do loop
        close(fhandle)
    end if
    if ((GetOut1Wabal()) .or. (GetOut2Crop()) .or. (GetOut3Prof()) .or. (GetOut4Salt()) &
            .or. (GetOut5CompWC()) .or. (GetOut6CompEC()) .or. (GetOut7Clim()) ) then
        call SetOutDaily(.true.)
    else
        call SetOutDaily(.false.)
    end if
end subroutine GetRequestDailyResults

subroutine GetRequestParticularResults()

    integer :: fhandle, rc
    character(len= 1025) :: FullFileName, TempString
    integer(int32) :: n, i

    call SetPart1Mult(.false.)
    call SetPart2Eval(.false.)

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
                    call SetPart1Mult(.true.)
                end if
                if (TempString(i:i) == '2') then
                    call SetPart2Eval(.true.)
                end if
            end if
        if (rc == iostat_end) exit loop
        end do loop
        close(fhandle)
    end if
end subroutine GetRequestParticularResults

subroutine GetTimeAggregationResults()

    character(len=:), allocatable :: FullFileName
    character(len=1024) :: TempString
    integer(int32) :: f0, rc
    integer(int32) :: n, i
    logical :: file_exists

    call SetOutputAggregate(0_int8) ! simulation period 0: season
    FullFileName = trim(GetPathNameSimul()//'AggregationResults.SIM')
    inquire(file=FullFileName, exist=file_exists)
    if (file_exists) then
        open(newunit=f0, file=trim(FullFileName), &
                  status='old', action='read', iostat=rc)
        read(f0, *, iostat=rc) TempString
        n = len_trim(TempString)
        if (n > 0) then
            i = 1
            do while ((TempString(i:i) == ' ') .and. (i < n))
                i = i + 1
            end do
            if (TempString(i:i) == '1') then
                call SetOutputAggregate(1_int8) ! 1: daily aggregation
            else
                if (TempString(i:i) == '2') then
                    call SetOutputAggregate(2_int8) ! 2 : 10-daily aggregation
                else
                    if (TempString(i:i) == '3') then
                        call SetOutputAggregate(3_int8) ! 3 : monthly aggregation
                    else
                        call SetOutputAggregate(0_int8) ! 0 : seasonal results only
                    end if
                end if
            end if
        end if
        close(f0)
    end if
end subroutine GetTimeAggregationResults


end module ac_startunit
