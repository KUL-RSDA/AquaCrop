module ac_project_input

use ac_kinds, only: dp, &
                    int8, &
                    int32
implicit none


type ProjectInput_type
    !! Container for project file input data
    real(dp) :: VersionNr
        !! AquaCrop version number (common for all runs)
    character(len=:), allocatable :: Description
        !! Project description (common for all runs)
    integer(int8)  :: Simulation_YearSeason
        !! Year number of cultivation (1 = seeding/planting year)
    integer(int32) :: Simulation_DayNr1
        !! First day of simulation period
    integer(int32) :: Simulation_DayNrN
        !! Last day of simulation period
    integer(int32) :: Crop_Day1
        !! First day of cropping period
    integer(int32) :: Crop_DayN
        !! Last day of cropping period
    character(len=:), allocatable :: Climate_Info
        !! Climate info
    character(len=:), allocatable :: Climate_Filename
        !! Climate file name
    character(len=:), allocatable :: Climate_Directory
        !! Climate file directory
    character(len=:), allocatable :: Temperature_Info
        !! Temperature info
    character(len=:), allocatable :: Temperature_Filename
        !! Temperature file name
    character(len=:), allocatable :: Temperature_Directory
        !! Temperature file directory
    character(len=:), allocatable :: ETo_Info
        !! ETo info
    character(len=:), allocatable :: ETo_Filename
        !! ETo file name
    character(len=:), allocatable :: ETo_Directory
        !! ETo file directory
    character(len=:), allocatable :: Rain_Info
        !! Rain info
    character(len=:), allocatable :: Rain_Filename
        !! Rain file name
    character(len=:), allocatable :: Rain_Directory
        !! Rain file directory
    character(len=:), allocatable :: CO2_Info
        !! CO2 info
    character(len=:), allocatable :: CO2_Filename
        !! CO2 file name
    character(len=:), allocatable :: CO2_Directory
        !! CO2 file directory
    character(len=:), allocatable :: Calendar_Info
        !! Calendar info
    character(len=:), allocatable :: Calendar_Filename
        !! Calendar file name
    character(len=:), allocatable :: Calendar_Directory
        !! Calendar file directory
    character(len=:), allocatable :: Crop_Info
        !! Crop info
    character(len=:), allocatable :: Crop_Filename
        !! Crop file name
    character(len=:), allocatable :: Crop_Directory
        !! Crop file directory
    character(len=:), allocatable :: Irrigation_Info
        !! Irrigation info
    character(len=:), allocatable :: Irrigation_Filename
        !! Irrigation file name
    character(len=:), allocatable :: Irrigation_Directory
        !! Irrigation file directory
    character(len=:), allocatable :: Management_Info
        !! Management info
    character(len=:), allocatable :: Management_Filename
        !! Management file name
    character(len=:), allocatable :: Management_Directory
        !! Management file directory
    character(len=:), allocatable :: GroundWater_Info
        !! Groundwater info
    character(len=:), allocatable :: GroundWater_Filename
        !! Groundwater file name
    character(len=:), allocatable :: GroundWater_Directory
        !! Groundwater file directory
    character(len=:), allocatable :: Soil_Info
        !! Soil info
    character(len=:), allocatable :: Soil_Filename
        !! Soil file name
    character(len=:), allocatable :: Soil_Directory
        !! Soil file directory
    character(len=:), allocatable :: SWCIni_Info
        !! SWCIni info
    character(len=:), allocatable :: SWCIni_Filename
        !! SWCIni file name
    character(len=:), allocatable :: SWCIni_Directory
        !! SWCIni file directory
    character(len=:), allocatable :: OffSeason_Info
        !! OffSeason info
    character(len=:), allocatable :: OffSeason_Filename
        !! OffSeason file name
    character(len=:), allocatable :: OffSeason_Directory
        !! OffSeason file directory
    character(len=:), allocatable :: Observations_Info
        !! Observations info
    character(len=:), allocatable :: Observations_Filename
        !! Observations file name
    character(len=:), allocatable :: Observations_Directory
        !! Observations file directory
    contains
    procedure :: read_project_file
end type ProjectInput_type


contains


subroutine read_project_file(self, filename, NrRun)
    !! Reads in the project file contents that apply to the given run index.
    class(ProjectInput_type), intent(out) :: self
    character(len=*), intent(in) :: filename
        !! PRM or PRO file name
    integer, intent(in) :: NrRun
        !! Run index (should be 1 in the case of a PRO file)

    integer :: fhandle, i, rc, Runi
    character(len=1024) :: buffer

    open(newunit=fhandle, file=trim(filename), status='old', action='read', &
         iostat=rc)
    read(fhandle, '(a)', iostat=rc) buffer
    self%Description = trim(buffer)
    read(fhandle, *, iostat=rc) self%VersionNr ! AquaCrop version Nr

    if (NrRun > 1) then
        ! Skip sections belonging to previous runs
        do Runi = 1, (NrRun - 1)
            do i = 1, 47
                read(fhandle, *, iostat=rc) ! 5 + 42 lines with files
            end do
        end do
    end if

    ! 0. Year of cultivation and Simulation and Cropping period
    read(fhandle, *, iostat=rc) self%Simulation_YearSeason
    read(fhandle, *, iostat=rc) self%Simulation_DayNr1
    read(fhandle, *, iostat=rc) self%Simulation_DayNrN
    read(fhandle, *, iostat=rc) self%Crop_Day1
    read(fhandle, *, iostat=rc) self%Crop_DayN

    ! 1. Climate
    read(fhandle, '(a)', iostat=rc) buffer
    self%Climate_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Climate_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Climate_Directory = trim(buffer)

    ! 1.1 Temperature
    read(fhandle, '(a)', iostat=rc) buffer
    self%Temperature_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Temperature_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Temperature_Directory = trim(buffer)

    ! 1.2 ETo
    read(fhandle, '(a)', iostat=rc) buffer
    self%ETo_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%ETo_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%ETo_Directory = trim(buffer)

    ! 1.3 Rain
    read(fhandle, '(a)', iostat=rc) buffer
    self%Rain_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Rain_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Rain_Directory = trim(buffer)

    ! 1.4 CO2
    read(fhandle, '(a)', iostat=rc) buffer
    self%CO2_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%CO2_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%CO2_Directory = trim(buffer)

    ! 2. Calendar
    read(fhandle, '(a)', iostat=rc) buffer
    self%Calendar_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Calendar_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Calendar_Directory = trim(buffer)

    ! 3. Crop
    read(fhandle, '(a)', iostat=rc) buffer
    self%Crop_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Crop_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Crop_Directory = trim(buffer)

    ! 4. Irrigation
    read(fhandle, '(a)', iostat=rc) buffer
    self%Irrigation_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Irrigation_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Irrigation_Directory = trim(buffer)

    ! 5. Field Management
    read(fhandle, '(a)', iostat=rc) buffer
    self%Management_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Management_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Management_Directory = trim(buffer)

    ! 6. Soil Profile
    read(fhandle, '(a)', iostat=rc) buffer
    self%Soil_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Soil_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Soil_Directory = trim(buffer)

    ! 7. GroundWater
    read(fhandle, '(a)', iostat=rc) buffer
    self%GroundWater_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%GroundWater_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%GroundWater_Directory = trim(buffer)

    ! 8. Initial conditions
    read(fhandle, '(a)', iostat=rc) buffer
    self%SWCIni_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%SWCIni_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%SWCIni_Directory = trim(buffer)

    ! 9. Off-season conditions
    read(fhandle, '(a)', iostat=rc) buffer
    self%OffSeason_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%OffSeason_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%OffSeason_Directory = trim(buffer)

    ! 10. Field data
    read(fhandle, '(a)', iostat=rc) buffer
    self%Observations_Info = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Observations_Filename = trim(buffer)
    read(fhandle, *, iostat=rc) buffer
    self%Observations_Directory = trim(buffer)

    close(fhandle)
end subroutine read_project_file

end module ac_project_input
