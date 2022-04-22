module ac_startunit

use ac_kinds, only: int32,&
                    int8

use iso_fortran_env, only: iostat_end

use ac_global, only: assert, &
                     GetPathNameSimul, &
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
                     GetOut7Clim, &
                     GetPathNameOutp, &
                     GetOutputAggregate, &
                     GetPart1Mult, &
                     GetPart2Eval, &
                     GetPathNameList, &
                     GetOutDaily, &
                     SetPathNameProg, &
                     SetPathNameSimul, &
                     SetPathNameList, &
                     SetPathNameParam, &
                     SetPathNameOutp

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

subroutine PrepareReport()

    call fProjects_open(&
          (trim(GetPathNameOutp())//'ListProjectsLoaded.OUT'), 'w')
    call fProjects_write('Intermediate results: ', .false.)
    select case (GetOutputAggregate())
    case (1)
        call fProjects_write('daily results')
    case (2)
        call fProjects_write('10-daily results')
    case (3)
        call fProjects_write('monthly results')
    case default
        call fProjects_write('None created')
    end select
    call fProjects_write('')
    if (GetOutDaily()) then
        call fProjects_write('Daily output results:')
        if (GetOut1Wabal()) then
            call fProjects_write('1. - soil water balance')
        end if
        if (GetOut2Crop()) then
            call fProjects_write('2. - crop development and production')
        end if
        if (GetOut3Prof()) then
            call fProjects_write('3. - soil water content '// &
                                 'in the soil profile and root zone')
        end if
        if (GetOut4Salt()) then
            call fProjects_write('4. - soil salinity in the soil profile '// &
                                 'and root zone')
        end if
        if (GetOut5CompWC()) then
            call fProjects_write('5. - soil water content at various depths '// &
                                 'of the soil profile')
        end if
        if (GetOut6CompEC()) then
            call fProjects_write('6. - soil salinity at various depths '// &
                                 'of the soil profile')
        end if
        if (GetOut7Clim()) then
            call fProjects_write('7. - climate input parameters')
        end if
    else
        call fProjects_write('Daily output results: None created')
    end if
    call fProjects_write('')
    if (GetPart1Mult() .or.  GetPart2Eval()) then
        call fProjects_write('Particular results:')
        if (GetPart1Mult()) then
            call fProjects_write('1. - biomass and yield at multiple cuttings'//&
                                 '(for herbaceous forage crops)')
        end if
        if (GetPart2Eval()) then
            call fProjects_write('2. - evaluation of simulation results '//&
                                 '(when Field Data)')
        end if
    else
        call fProjects_write('Particular results: None created')
    end if
end subroutine PrepareReport

subroutine InitializeTheProgram()

!Decimalseparator = '.' GDL, 20220413, not used?
    call SetPathNameOutp('OUTP/')
    call SetPathNameSimul('SIMUL/')
    call SetPathNameList('LIST/')
    call SetPathNameParam('PARAM/')
    call SetPathNameProg('')

    call GetTimeAggregationResults()
    call GetRequestDailyResults()
    call GetRequestParticularResults()
    call PrepareReport()
end subroutine InitializeTheProgram


function GetListProjectsFile() result(ListProjectsFile)
    character(len=:), allocatable :: ListProjectsFile

    ListProjectsFile = GetPathNameList() // 'ListProjects.txt'

end function GetListProjectsFile


integer(int32) function GetNumberOfProjects()

    integer(int32) :: NrProjects
    character(len=:), allocatable :: ListProjectsFile
    logical :: ListProjectFileExist

    integer :: fhandle, rc

    ListProjectsFile = GetListProjectsFile()
    ListProjectFileExist = FileExists(ListProjectsFile)
    NrProjects = 0

    if (ListProjectFileExist) then
        open(newunit=fhandle, file=trim(ListProjectsFile), &
             status='old', action='read', iostat=rc)
        read(fhandle, *, iostat=rc)
        do while (rc /= iostat_end) 
            NrProjects = NrProjects + 1
            read(fhandle, *, iostat=rc)
        end do
        close(fhandle)
    end if
    GetNumberOfProjects = NrProjects
end function GetNumberOfProjects


function GetProjectFileName(iproject) result(ProjectFileName_out)
    integer(int32), intent(in) :: iproject
    character(len=:), allocatable :: ProjectFileName_out

    integer(int32) :: jproject
    character(len=:), allocatable :: ListProjectsFile
    character(len=1025) :: TheProjectFile
    integer :: fhandle

    ListProjectsFile = GetListProjectsFile()
    call assert(FileExists(ListProjectsFile), 'ListProjectsFile does not exist')

    open(newunit=fhandle, file=trim(ListProjectsFile), status='old', action='read')

    ! Read until we arrive at the selected project
    do jproject = 1, iproject 
        read(fhandle, *) TheProjectFile
    end do
    close(fhandle)

    ProjectFileName_out = trim(TheProjectFile)
end function GetProjectFileName

end module ac_startunit
