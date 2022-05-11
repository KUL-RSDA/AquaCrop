module ac_interface_inforesults

use, intrinsic :: iso_c_binding, only: c_ptr

use ac_kinds, only: dp, &
                    intEnum, &
                    int32

use ac_interface_global, only:  pointer2string
use ac_inforesults, only:   StatisticAnalysis, &
                            rep_EventObsSim, &
                            WriteAssessmentSimulation

implicit none


contains

subroutine StatisticAnalysis_wrap(TypeObsSim, RangeObsMin, RangeObsMax, StrNr_ptr, &
                             strlen, Nobs, ObsAver, SimAver, PearsonCoeff, RMSE, &
                             NRMSE, NScoeff, IndexAg, ArrayObsSim)
    integer(intEnum), intent(in) :: TypeObsSim
    integer(int32), intent(in) :: RangeObsMin
    integer(int32), intent(in) :: RangeObsMax
    type(c_ptr), intent(in) :: StrNr_ptr
    integer(int32), intent(in) :: strlen
    integer(int32), intent(inout) :: Nobs
    real(dp), intent(inout) :: ObsAver
    real(dp), intent(inout) :: SimAver
    real(dp), intent(inout) :: PearsonCoeff
    real(dp), intent(inout) :: RMSE
    real(dp), intent(inout) :: NRMSE
    real(dp), intent(inout) :: NScoeff
    real(dp), intent(inout) :: IndexAg
    type(rep_EventObsSim), dimension(100), intent(inout) :: ArrayObsSim

    character(len=strlen) :: string

    string = pointer2string(StrNr_ptr, strlen)
    call StatisticAnalysis(TypeObsSim, RangeObsMin, RangeObsMax, string, &
                           Nobs, ObsAver, SimAver, PearsonCoeff, RMSE, &
                           NRMSE, NScoeff, IndexAg, ArrayObsSim)
end subroutine StatisticAnalysis_wrap


subroutine WriteAssessmentSimulation_wrap(p1, p2, strlen1, strlen2, &
                                          TheProjectType, RangeMin, RangeMax)
    !! Wrapper for WriteAssessmentSimulation for foreign languages.
    !!
    !! NOTE: issues are likely to arise if one of the two pointer/strlen
    !! combinations corresponds to an empty string.
    type(c_ptr), intent(in) :: p1
    type(c_ptr), intent(in) :: p2
    integer(int32), intent(in) :: strlen1
    integer(int32), intent(in) :: strlen2
    integer(intEnum), intent(in) :: TheProjectType
    integer(int32), intent(in) :: RangeMin
    integer(int32), intent(in) :: RangeMax

    character(len=strlen1) :: string1
    character(len=strlen2) :: string2

    string1 = pointer2string(p1, strlen1)
    string2 = pointer2string(p2, strlen2)
    call WriteAssessmentSimulation(string1, string2, &
                                   TheProjectType, RangeMin, RangeMax)
end subroutine WriteAssessmentSimulation_wrap

end module ac_interface_inforesults
