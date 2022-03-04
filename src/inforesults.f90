module ac_inforesults

use ac_kinds, only: dp, &
                    int8, &
                    int16, &
                    int32, &
                    intEnum
use iso_fortran_env, only: iostat_end
use ac_global, only:    typeObsSim_ObsSimCC, &
                        typeObsSim_ObsSimB, &
                        typeObsSim_ObsSimSWC, &
                        undef_int, &
                        GetPathNameSimul, &
                        GetSimulation_FromDayNr, &
                        roundc
                     
implicit none

type rep_EventObsSim 
    real(dp) :: Obsi
        !! Undocumented
    real(dp) :: StdObsi
        !! Undocumented
    real(dp) :: Simi
        !! Undocumented
    integer(int8) :: DDi
        !! Undocumented
    integer(int8) :: MMi
        !! Undocumented
    integer(int32) :: YYYYi
        !! Undocumented
end type rep_EventObsSim 


contains

subroutine StatisticAnalysis(TypeObsSim, RangeObsMin, RangeObsMax, StrNr, &
                             Nobs, ObsAver, SimAver, PearsonCoeff, RMSE, &
                             NRMSE, NScoeff, IndexAg, ArrayObsSim)
    integer(intEnum), intent(in) :: TypeObsSim
    integer(int32), intent(in) :: RangeObsMin
    integer(int32), intent(in) :: RangeObsMax
    character(len=*), intent(in) :: StrNr
    integer(int32), intent(inout) :: Nobs
    real(dp), intent(inout) :: ObsAver
    real(dp), intent(inout) :: SimAver
    real(dp), intent(inout) :: PearsonCoeff
    real(dp), intent(inout) :: RMSE
    real(dp), intent(inout) :: NRMSE
    real(dp), intent(inout) :: NScoeff
    real(dp), intent(inout) :: IndexAg
    type(rep_EventObsSim), dimension(100), intent(inout) :: ArrayObsSim

    integer(int32) :: Nri
    real(dp) :: dDeNom, R2tel, SumSqrDobs, SumSqrDsim, SumSqrDiv, DeNom

    ! get data
    call GetObsSim(RangeObsMin, RangeObsMax, StrNr, Nobs, ArrayObsSim, &
                   ObsAver, SimAver)
    ! statistical evaluation
    if (Nobs > 1) then
        R2tel = 0._dp
        SumSqrDobs = 0._dp
        SumSqrDsim = 0._dp
        SumSqrDiv = 0._dp
        dDeNom = 0._dp
        do Nri = 1, Nobs 
            R2tel = R2tel + (ArrayObsSim(Nri)%Obsi-ObsAver) &
                            * (ArrayObsSim(Nri)%Simi-SimAver)
            SumSqrDobs = SumSqrDobs + (ArrayObsSim(Nri)%Obsi-ObsAver)**2
            SumSqrDsim = SumSqrDsim + (ArrayObsSim(Nri)%Simi-SimAver)**2
            SumSqrDiv = SumSqrDiv &
                        + (ArrayObsSim(Nri)%Obsi-ArrayObsSim(Nri)%Simi)**2
            dDeNom = dDeNom + (abs(ArrayObsSim(Nri)%Simi-ObsAver) &
                               + abs(ArrayObsSim(Nri)%Obsi-ObsAver))**2
        end do
        ! R2
        DeNom = sqrt(SumSqrDobs*SumSqrDsim)
        if (DeNom > 0._dp) then
            PearsonCoeff = R2tel/DeNom
        else
            PearsonCoeff = real(undef_int, kind=dp)
        end if
        ! RMSE
        RMSE = sqrt(SumSqrDiv/NObs)
        ! NRMSE
        if (ObsAver > 0._dp) then
            NRMSE = 100._dp*(RMSE/ObsAver)
        else
            NRMSE = real(undef_int, kind=dp)
        end if
        ! Nash-Sutcliffe coefficient (EF)
        if (SumSqrDobs > 0._dp) then
            NScoeff = 1._dp - (SumSqrDiv/SumSqrDobs)
        else
            NScoeff = real(undef_int, kind=dp)
        end if
        ! Index of agreement (d)
        if (dDeNom > 0._dp) then
            IndexAg = 1._dp - (SumSqrDiv/dDeNom)
        else
            IndexAg = real(undef_int, kind=dp)
        end if
    else
        ObsAver = real(undef_int, kind=dp)
        SimAver = real(undef_int, kind=dp)
        PearsonCoeff = real(undef_int, kind=dp)
        RMSE = real(undef_int, kind=dp)
        NRMSE = real(undef_int, kind=dp)
        NScoeff = real(undef_int, kind=dp)
        IndexAg = real(undef_int, kind=dp)
    end if


    contains


    subroutine GetObsSim(RangeObsMin, RangeObsMax, StrNr, Nobs, &
                         ArrayObsSim, ObsAver, SimAver)
        integer(int32), intent(in) :: RangeObsMin
        integer(int32), intent(in) :: RangeObsMax
        character(len=*), intent(in) :: StrNr
        integer(int32), intent(inout) :: Nobs
        type(rep_EventObsSim), dimension(100), intent(inout) :: ArrayObsSim
        real(dp), intent(inout) :: ObsAver
        real(dp), intent(inout) :: SimAver

        character(len=:), allocatable :: OutputName
        integer :: fOut
        integer(int8) :: Dayi, Monthi
        integer(int32) :: SkipLines, NCi, NCobs, Yeari
        integer(int32) :: i, rc
        real(dp) :: VarObsi, VarSimi, VarStdi

        Nobs = 0
        ObsAver = 0._dp
        SimAver = 0._dp

        ! open file
        OutputName = GetPathNameSimul() // 'EvalData' // trim(StrNr) // '.OUT'
        open(newunit=fOut, file=trim(OutputName), status='old', action='read')
        read(fOut, *) ! AquaCrop Version - Date and Time
        read(fOut, *) ! title
        read(fOut, *) ! 
        read(fOut, *) ! list of variables
        read(fOut, *) ! units
        ! find first day
        SkipLines = RangeObsMin - GetSimulation_FromDayNr()
        do i = 1, SkipLines 
            read(fOut, *, iostat = rc)
        end do

        ! get Sim and Obs in range
        select case (TypeObsSim)
            case(typeObsSim_ObsSimCC)
                NCobs = 2
            case (typeObsSim_ObsSimB)
                NCobs = 5
            case (typeObsSim_ObsSimSWC)
                NCobs = 8
            case default
                NCobs = 5
        end select
        do i = RangeObsMin, RangeObsMax 
            if (rc /= iostat_end) then
                read(fOut, *) Dayi, Monthi, Yeari
                do NCi = 1, NCobs 
                    read(fOut, *) VarSimi
                end do
                read(fOut, *) VarSimi, VarObsi, VarStdi
                if ((roundc(VarObsi, mold=1) /= undef_int) &
                                .and. (Nobs < 100)) then
                    Nobs = Nobs + 1
                    ArrayObsSim(Nobs)%DDi = Dayi
                    ArrayObsSim(Nobs)%MMi = Monthi
                    ArrayObsSim(Nobs)%YYYYi = Yeari
                    ArrayObsSim(Nobs)%Simi = VarSimi
                    ArrayObsSim(Nobs)%Obsi = VarObsi
                    ArrayObsSim(Nobs)%StdObsi = VarStdi
                    SimAver = SimAver + VarSimi
                    ObsAver = ObsAver + VarObsi
                end if
            end if
        end do
        ! close file
        close(fOut)

        ! calculate averages
        if (Nobs > 0) then
            ObsAver = ObsAver/real(Nobs, kind=dp)
            SimAver = SimAver/real(Nobs, kind=dp)
        end if
    end subroutine GetObsSim
end subroutine StatisticAnalysis



end module ac_inforesults
