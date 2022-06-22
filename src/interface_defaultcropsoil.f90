module ac_interface_defaultcropsoil

use ac_defaultcropsoil, only: ResetDefaultSoil
implicit none


contains


subroutine ResetDefaultSoil_wrap(use_default_soil_file)
    logical(1), intent(in) :: use_default_soil_file

    logical :: use_default_soil_file_f

    use_default_soil_file_f = use_default_soil_file
    call ResetDefaultSoil(use_default_soil_file_f)
end subroutine ResetDefaultSoil_wrap

end module ac_interface_defaultcropsoil
