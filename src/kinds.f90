module ac_kinds

use iso_fortran_env, only: int8, &
                           int16, &
                           int32, &
                           real32
implicit none


integer, parameter :: sp = real32
    !! single precision real kind, the only real kind used (as in LIS)
integer, parameter :: intEnum = int8
    !! integer kind for enumerated types

end module ac_kinds
