program aquacrop

use ac_global, only: GetSimulation_NrRuns, &
                     GetSimulation_ToDayNr, &
                     typeproject_typeprm, &
                     typeproject_typepro
use ac_kinds, only: int8, &
                    intEnum
use ac_run, only: AdvanceOneTimeStep, &
                  FinalizeRun1, &
                  FinalizeRun2, &
                  FinalizeSimulation, &
                  GetDayNri, &
                  InitializeRun, &
                  InitializeSimulation
use ac_startunit, only: FinalizeTheProgram, &
                        GetListProjectsFile, &
                        GetNumberOfProjects, &
                        GetProjectFileName, &
                        GetProjectType, &
                        InitializeProject, &
                        InitializeTheProgram, &
                        WriteProjectsInfo
implicit none

integer :: daynr, todaynr, iproject, nprojects, irun, nruns
integer(intEnum) :: TheProjectType
logical :: ListProjectFileExist
character(len=:), allocatable :: ListProjectsFile, TheProjectFile

! Everything below is the equivalent of "StartTheProgram()"

call InitializeTheProgram()

ListProjectsFile = GetListProjectsFile()
inquire(file=trim(ListProjectsFile), exist=ListProjectFileExist)
nprojects = GetNumberOfProjects()

if (nprojects > 0) then
    call WriteProjectsInfo('')
    call WriteProjectsInfo('Projects handled:')
end if

do iproject = 1, nprojects
    TheProjectFile = GetProjectFileName(iproject)
    call GetProjectType(TheProjectFile, TheProjectType)
    call InitializeProject(iproject, TheProjectFile, TheProjectType)

    ! The remainder of this loop is the equivalent of "RunSimulation()"

    call InitializeSimulation(TheProjectFile, TheProjectType)

    if (TheProjectType == typeproject_typepro) then
        nruns = 1
    elseif (TheProjectType == typeproject_typeprm) then
        nruns = GetSimulation_NrRuns()
    end if

    do irun = 1, nruns
        call InitializeRun(int(irun, kind=int8), TheProjectType);

        todaynr = GetSimulation_ToDayNr()

        time_loop: do
            call AdvanceOneTimeStep()
            daynr = GetDayNri()
            if ((daynr - 1) == todaynr) exit time_loop
        end do time_loop

        call FinalizeRun1(int(irun, kind=int8), TheProjectFile, TheProjectType)
        call FinalizeRun2(int(irun, kind=int8), TheProjectType)
    end do

    call FinalizeSimulation()
end do

if (nprojects == 0) then
    call WriteProjectsInfo('')

    if (ListProjectFileExist) then
        call WriteProjectsInfo('File ''ListProjects.txt'' does not contain ANY project file')
    else
        call WriteProjectsInfo('Missing File ''ListProjects.txt'' in LIST directory')
    end if
end if

call FinalizeTheProgram()

end program aquacrop
