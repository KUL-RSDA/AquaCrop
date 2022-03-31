module ac_startunit

use ac_kinds, only: int32,&
                    int8

use iso_fortran_env, only: iostat_end

use ac_global , only: GetPathNameSimul


implicit none


contains


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
