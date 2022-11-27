module ac_interface_initialsettings

use ac_initialsettings, only: InitializeSettings
implicit none


contains


subroutine InitializeSettings_wrap(use_default_soil_file,use_default_crop_file)
    logical(1), intent(in) :: use_default_soil_file
    logical(1), intent(in) :: use_default_crop_file

    logical :: use_default_soil_file_f
    logical :: use_default_crop_file_f

    use_default_soil_file_f = use_default_soil_file
    use_default_crop_file_f = use_default_crop_file
    call InitializeSettings(use_default_soil_file_f,use_default_crop_file_f)
end subroutine InitializeSettings_wrap

end module ac_interface_initialsettings
