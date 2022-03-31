module ac_startunit

use ac_kinds, only: int8, &
                    int32

use ac_global, only: FileExists, &
                     GetPathNameSimul

implicit none


contains

subroutine GetTimeAggregationResults(OutputAggregate)
    integer(int8), intent(inout) :: OutputAggregate

    integer :: fhandle
    character(len=1025) :: FullFileName, TempString

    integer(int32) :: n, i

    OutputAggregate = 0 ! simulation period 0: season
    FullFileName = GetPathNameSimul() // 'AggregationResults.SIM'
    if (FileExists(FullFileName) .eqv. .true.) then
        open(newunit=fhandle, file=trim(FullFileName), status='old', action='read')
        read(fhandle, *) TempString
        n = len(TempString)
        if (n > 0) then
            i = 1
            do while ((TempString(i:i) == ' ') .and. (i < n))
                i = i + 1
            end do
            if (TempString(i:i) == '1') then
                OutputAggregate = 1 ! 1: daily aggregation
            else
                if (TempString(i:i) == '2') then
                    OutputAggregate = 2 ! 2 : 10-daily aggregation
                else
                    if (TempString(i:i) == '3') then
                        OutputAggregate = 3 ! 3 : monthly aggregation
                    else
                        OutputAggregate = 0 ! 0 : seasonal results only
                    end if
                end if
            end if
        end if
        close(fhandle)
    end if
end subroutine GetTimeAggregationResults


end module ac_startunit
