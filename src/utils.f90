module ac_utils
!! A place for various small utilities.

implicit none


contains


subroutine upper_case(str)
    !! Converts a string to upper case.
    !!
    !! Adapted from https://fortranwiki.org/fortran/show/m_strings.
    character(len=*), intent(inout) :: str

    integer :: i

    do i = 1, len(str)
        select case(str(i:i))
        case("a":"z")
            str(i:i) = achar(iachar(str(i:i)) - 32)
        end select
    end do
end subroutine upper_case

end module ac_utils
