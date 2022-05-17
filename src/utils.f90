module ac_utils
!! A place for various small utilities.

use ac_kinds, only: dp, &
                    int8, &
                    int32
implicit none


interface roundc
    module procedure roundc_int8
    module procedure roundc_int32
end interface roundc


contains


subroutine assert(condition, message)
    !! Prints an error message if the condition is not met,
    !! and then shuts down the whole program.
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) then
        print *, 'ABORT: ', message
        stop 1
    end if
end subroutine assert


function roundc_int32(x, mold) result(y)
    !! Returns commercial rounds, following Pascal's banker's rules for rounding
    real(dp), intent(in) :: x
        !! Value to be rounded to an integer
    integer(int32), intent(in) :: mold
        !! Integer determining the kind of the integer result
    integer(int32) :: y

   if (abs(x - floor(x, kind=int32) - 0.5_dp) < epsilon(0._dp)) then
       if (x > 0) then
          if (mod(abs(trunc(x)),2) == 0) then
              y = floor(x, kind=int32)
          else
              y = ceiling(x, kind=int32)
          end if
       else
          if (mod(abs(trunc(x)),2) == 0) then
              y = ceiling(x, kind=int32)
          else
              y = floor(x, kind=int32)
          end if
       end if
    else !standard round for values not ending on 0.5
       y = nint(x, kind=int32)
    end if
end function roundc_int32


function roundc_int8(x, mold) result(y)
    !! Returns commercial rounds, following Pascal's banker's rules for rounding
    real(dp), intent(in) :: x
        !! Value to be rounded to an integer
    integer(int8), intent(in) :: mold
        !! Integer determining the kind of the integer result
    integer(int8) :: y

    if (abs(x - floor(x, kind=int32) - 0.5_dp) < epsilon(0._dp)) then
       if (x > 0) then
          if (mod(abs(trunc(x)),2) == 0) then
             y = floor(x, kind=int8)
          else
              y = ceiling(x, kind=int8)
          end if
       else
          if (mod(abs(trunc(x)),2) == 0) then
              y = ceiling(x, kind=int8)
          else
              y = floor(x, kind=int8)
          end if
       end if
    else !standard round for values not ending on 0.5
       y = nint(x, kind=int8)
    end if
end function roundc_int8


function trunc(x) result(y)
    !! Returns the integer part of x, which is always smaller than (or equal to) x
    !! in absolute value.
    real(dp), intent(in) :: x
    integer(int32) :: y

    if (x > 0) then
        y = floor(x, kind=int32)
    else
        y = ceiling(x, kind=int32)
    end if
end function trunc


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
