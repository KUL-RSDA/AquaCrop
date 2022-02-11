module ac_tempprocessing

use ac_kinds,  only: dp, &
                     int8, &
                     int16, &   
                     int32

use iso_fortran_env, only: iostat_end

use ac_global , only: undef_int, &
                      roundc, &
                      DaysinMonth, &
                      rep_DayEventDbl, &
                      DetermineDayNr, &
                      DetermineDate, &
                      LeapYear, &
                      SplitStringInTwoParams, &
                      KsTemperature, &
                      DegreesDay, &
                      LengthCanopyDecline, &
                      GetPathNameSimul, &
                      GetTemperatureFile, &                     
                      GetTemperatureFilefull, &
                      GetTemperatureRecord_FromD, &
                      GetTemperatureRecord_FromM, &
                      GetTemperatureRecord_FromY, &
                      GetTemperatureRecord_NrObs, &
                      GetTemperatureRecord_ToD, &
                      GetTemperatureRecord_ToM, &
                      GetTemperatureRecord_ToY, &
                      GetTemperatureRecord_DataType, &
                      GetTemperatureRecord_FromDayNr, &
                      GetTemperatureRecord_ToDayNr, &
                      GetSimulParam_GDDMethod, &
                      FullUndefinedRecord

implicit none


contains


subroutine AdjustMONTHandYEAR(MFile, Yfile)
    integer(int32), intent(inout) :: MFile
    integer(int32), intent(inout) :: Yfile

    Mfile = Mfile - 12
    YFile = Yfile + 1
end subroutine AdjustMONTHandYEAR


subroutine AdjustDecadeMONTHandYEAR(DecFile, Mfile, Yfile)
    integer(int32), intent(inout) :: DecFile
    integer(int32), intent(inout) :: Mfile
    integer(int32), intent(inout) :: Yfile

    DecFile = 1
    Mfile = Mfile + 1
    if (Mfile > 12) then
        Mfile = 1
        YFile = Yfile + 1
    end if
end subroutine AdjustDecadeMONTHandYEAR


subroutine SetDayNrToYundef(DayNri)
    integer(int32), intent(inout) :: DayNri

    integer(int32) :: Dayi, Monthi, Yeari

    call DetermineDate(DayNri, Dayi, Monthi, Yeari)
    Yeari = 1901
    call DetermineDayNr(Dayi, Monthi, Yeari, DayNri)
end subroutine SetDayNrToYundef


subroutine GetDecadeTemperatureDataSet(DayNri, TminDataSet, TmaxDataSet)
    integer(int32), intent(in) :: DayNri
    type(rep_DayEventDbl), dimension(31) , intent(inout) :: TminDataSet
    type(rep_DayEventDbl), dimension(31) , intent(inout) :: TmaxDataSet

    integer(int32) :: Nri, ni, Dayi, Deci, Monthi, Yeari, DayN
    integer(int32) :: DNR
    real(dp) :: C1Min, C1Max, C2Min, C2Max, C3Min, C3Max
    real(dp) :: UlMin, LLMin, MidMin, UlMax, LLMax, MidMax

    call DetermineDate(DayNri, Dayi, Monthi, Yeari)
    if (Dayi > 20) then
        Deci = 3
        Dayi = 21
        DayN = DaysInMonth(Monthi)
        if ((Monthi == 2) .and. LeapYear(Yeari)) then
            DayN = DayN + 1
        end if
        ni = DayN - Dayi + 1
    elseif (Dayi > 10) then
        Deci = 2
        Dayi = 11
        DayN = 20
        ni = 10
    else
        Deci = 1
        Dayi = 1
        DayN = 10
        ni = 10
    end if
    call GetSetofThree(DayN, Deci, Monthi, Yeari, &
               C1Min, C1Max, C2Min, C2Max, C3Min, C3Max)
    call DetermineDayNr(Dayi, Monthi, Yeari, DNR)

    call GetParameters(C1Min, C2Min, C3Min, ULMin, LLMin, MidMin)
    do Nri = 1, ni
        TMinDataSet(Nri)%DayNr = DNR+Nri-1
        if (Nri <= (ni/2._dp+0.01_dp)) then
            TMinDataSet(Nri)%Param = (2._dp*ULMin + &
                          (MidMin-ULMin)*(2._dp*Nri-1._dp)/(ni/2._dp))/2._dp
        else
            if (((ni == 11) .or. (ni == 9)) .and. (Nri < (ni+1.01_dp)/2._dp)) then
                TminDataSet(Nri)%Param = MidMin
            else
                TminDataSet(Nri)%Param = (2._dp*MidMin + &
                          (LLMin-MidMin)*(2._dp*Nri-(ni+1))/(ni/2._dp))/2._dp
            end if
        end if
    end do

    call GetParameters(C1Max, C2Max, C3Max, ULMax, LLMax, MidMax)
    do Nri = 1, ni
        TMaxDataSet(Nri)%DayNr = DNR+Nri-1
        if (Nri <= (ni/2._dp+0.01_dp)) then
            TMaxDataSet(Nri)%Param = (2._dp*ULMax + &
                          (MidMax-ULMax)*(2._dp*Nri-1)/(ni/2._dp))/2._dp
        else
            if (((ni == 11) .or. (ni == 9)) .and. (Nri < (ni+1.01_dp)/2._dp)) then
                 TmaxDataSet(Nri)%Param = MidMax
            else
                TmaxDataSet(Nri)%Param = (2._dp*MidMax + &
                          (LLMax-MidMax)*(2._dp*Nri-(ni+1))/(ni/2._dp))/2._dp
            end if
        end if
    end do

    do Nri = (ni+1), 31
        TminDataSet(Nri)%DayNr = DNR+ni-1
        TminDataSet(Nri)%Param = 0._dp
        TmaxDataSet(Nri)%DayNr = DNR+ni-1
        TmaxDataSet(Nri)%Param = 0._dp
    end do

    contains

    subroutine GetSetofThree(DayN, Deci, Monthi, Yeari, &
                     C1Min, C1Max, C2Min, C2Max, C3Min, C3Max)
        integer(int32), intent(in) :: DayN
        integer(int32), intent(in) :: Deci
        integer(int32), intent(in) :: Monthi
        integer(int32), intent(in) :: Yeari
        real(dp), intent(inout) :: C1Min
        real(dp), intent(inout) :: C1Max
        real(dp), intent(inout) :: C2Min
        real(dp), intent(inout) :: C2Max
        real(dp), intent(inout) :: C3Min
        real(dp), intent(inout) :: C3Max

        integer(int32) :: fhandle
        integer(int32) :: DecFile, Mfile, Yfile, Nri, Obsi, rc
        logical :: OK3
        character(len=255) :: StringREAD

        !! 1 = previous decade, 2 = Actual decade, 3 = Next decade;
        open(newunit=fhandle, file=trim(GetTemperatureFilefull()), &
                     status='old', action='read', iostat=rc)
        read(fhandle, *, iostat=rc) ! description
        read(fhandle, *, iostat=rc) ! time step
        read(fhandle, *, iostat=rc) ! day
        read(fhandle, *, iostat=rc) ! month
        read(fhandle, *, iostat=rc) ! year
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)

        if (GetTemperatureRecord_FromD() > 20) then
            DecFile = 3
        elseif (GetTemperatureRecord_FromD() > 10) then
            DecFile = 2
        else
            DecFile = 1
        end if
        Mfile = GetTemperatureRecord_FromM()
        if (GetTemperatureRecord_FromY() == 1901) then
            Yfile = Yeari
        else
            Yfile = GetTemperatureRecord_FromY()
        end if
        OK3 = .false.

        if (GetTemperatureRecord_NrObs() <= 2) then
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            select case (GetTemperatureRecord_NrObs())
            case (0)
                C2Min = C1Min
                C2Max = C2Max
                C3Min = C1Min
                C3Max = C1Max
            case (1)
                DecFile = DecFile + 1
                if (DecFile > 3) then
                    call AdjustDecadeMONTHandYEAR(DecFile, Mfile, Yfile)
                end if
                read(fhandle, '(a)', iostat=rc) StringREAD
                call SplitStringInTwoParams(StringREAD, C3Min, C3Max)
                if (Deci == DecFile) then
                    C2Min = C3Min
                    C2Max = C3Max
                    C3Min = C2Min+(C2Min-C1Min)/4._dp
                    C3Max = C2Max+(C2Max-C1Max)/4._dp
                else
                    C2Min = C1Min
                    C2Max = C1Max
                    C1Min = C2Min + (C2Min-C3Min)/4._dp
                    C1Max = C2Max + (C2Max-C3Max)/4._dp
                end if
            end select
            OK3 = .true.
        end if

       if ((.not. OK3) .and. ((Deci == DecFile) .and. (Monthi == Mfile) &
            .and. (Yeari == Yfile))) then
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            C2Min = C1Min
            C2Max = C1Max
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C3Min, C3Max)
            C1Min = C2Min + (C2Min-C3Min)/4._dp
            C1Max = C2Max + (C2Max-C3Max)/4._dp
            OK3 = .true.
        end if

        if ((.not. OK3) .and. ((DayN == GetTemperatureRecord_ToD()) &
             .and. (Monthi == GetTemperatureRecord_ToM()))) then
            if ((GetTemperatureRecord_FromY() == 1901) .or. &
                (Yeari == GetTemperatureRecord_ToY())) then
                do Nri = 1, (GetTemperatureRecord_NrObs()-2)
                     read(fhandle, *, iostat=rc)
                end do
                read(fhandle, '(a)', iostat=rc) StringREAD
                call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
                read(fhandle, '(a)', iostat=rc) StringREAD
                call SplitStringInTwoParams(StringREAD, C2Min, C2Max)
                C3Min = C2Min+(C2Min-C1Min)/4._dp
                C3Max = C2Max+(C2Max-C1Max)/4._dp
                OK3 = .true.
            end if
        end if

        if (.not. OK3) then
            Obsi = 1
            do while (.not. OK3)
                if ((Deci == DecFile) .and. (Monthi == Mfile) &
                    .and. (Yeari == Yfile)) then
                    OK3 = .true.
                else
                    DecFile = DecFile + 1
                    if (DecFile > 3) then
                        call AdjustDecadeMONTHandYEAR(DecFile, Mfile, Yfile)
                    end if
                    Obsi = Obsi + 1
                end if
            end do
            if (GetTemperatureRecord_FromD() > 20) then
                DecFile = 3
            elseif (GetTemperatureRecord_FromD() > 10) then
                DecFile = 2
            else
                DecFile = 1
            end if
            do Nri = 1, (Obsi-2)
                read(fhandle, *, iostat=rc)
            end do
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C2Min, C2Max)
            read(fhandle, '(a)', iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C3Min, C3Max)
        end if
        close(fhandle)
    end subroutine GetSetofThree

    subroutine GetParameters(C1, C2, C3, UL, LL, Mid)
        real(dp), intent(in) :: C1
        real(dp), intent(in) :: C2
        real(dp), intent(in) :: C3
        real(dp), intent(inout) :: UL
        real(dp), intent(inout) :: LL
        real(dp), intent(inout) :: Mid

        UL = (C1+C2)/2._dp
        LL = (C2+C3)/2._dp
        Mid = 2._dp*C2 - (UL+LL)/2._dp
        ! --previous decade-->/UL/....... Mid ......../LL/<--next decade--
    end subroutine GetParameters

end subroutine GetDecadeTemperatureDataSet


subroutine GetMonthlyTemperatureDataSet(DayNri, TminDataSet, TmaxDataSet)
    integer(int32), intent(in) :: DayNri
    type(rep_DayEventDbl), dimension(31) , intent(inout) :: TminDataSet
    type(rep_DayEventDbl), dimension(31) , intent(inout) :: TmaxDataSet

    integer(int32) :: Dayi, Monthi, Yeari, DayN
    integer(int32) :: DNR
    integer(int32) :: X1, X2, X3, t1, t2
    real(dp) :: C1Min, C2Min, C3Min
    real(dp) :: C1Max, C2Max, C3Max
    real(dp) :: aOver3Min, bOver2Min, cMin
    real(dp) :: aOver3Max, bOver2Max, cMax

    call DetermineDate(DayNri, Dayi, Monthi, Yeari)
    call GetSetofThreeMonths(Monthi, Yeari, &
         C1Min, C2Min, C3Min, C1Max, C2Max, C3Max, X1, X2, X3, t1)

    Dayi = 1
    call DetermineDayNr(Dayi, Monthi, Yeari, DNR)
    DayN = DaysInMonth(Monthi)
    if ((Monthi == 2) .and. LeapYear(Yeari)) then
        DayN = DayN + 1
    end if

    call GetInterpolationParameters(C1Min, C2Min, C3Min, &
                   aOver3Min, bOver2Min, cMin)
    call GetInterpolationParameters(C1Max, C2Max, C3Max, &
                   aOver3Max, bOver2Max, cMax)
    do Dayi = 1, DayN
        t2 = t1 + 1
        TminDataSet(Dayi)%DayNr = DNR+Dayi-1
        TmaxDataSet(Dayi)%DayNr = DNR+Dayi-1
        TminDataSet(Dayi)%Param = aOver3Min*(t2*t2*t2-t1*t1*t1) &
            + bOver2Min*(t2*t2-t1*t1) + cMin*(t2-t1)
        TmaxDataSet(Dayi)%Param = aOver3Max*(t2*t2*t2-t1*t1*t1) &
            + bOver2Max*(t2*t2-t1*t1) + cMax*(t2-t1)
        t1 = t2
    end do
    do Dayi = (DayN+1), 31
        TminDataSet(Dayi)%DayNr = DNR+DayN-1
        TmaxDataSet(Dayi)%DayNr = DNR+DayN-1
        TminDataSet(Dayi)%Param = 0._dp
        TmaxDataSet(Dayi)%Param = 0._dp
    end do

    contains

    subroutine GetSetofThreeMonths(Monthi, Yeari, &
            C1Min, C2Min, C3Min, C1Max, C2Max, C3Max, X1, X2, X3, t1)
        integer(int32), intent(in) :: Monthi
        integer(int32), intent(in) :: Yeari
        real(dp), intent(inout) :: C1Min
        real(dp), intent(inout) :: C2Min
        real(dp), intent(inout) :: C3Min
        real(dp), intent(inout) :: C1Max
        real(dp), intent(inout) :: C2Max
        real(dp), intent(inout) :: C3Max
        integer(int32), intent(inout) :: X1
        integer(int32), intent(inout) :: X2
        integer(int32), intent(inout) :: X3
        integer(int32), intent(inout) :: t1

        integer(int32) :: fhandle
        integer(int32) :: Mfile, Yfile, n1, n2, n3, Nri, Obsi, rc
        logical :: OK3

        ! 1. Prepare record
        open(newunit=fhandle, file=trim(GetTemperatureFilefull()), &
                     status='old', action='read', iostat=rc)
        read(fhandle, *, iostat=rc) ! description
        read(fhandle, *, iostat=rc) ! time step
        read(fhandle, *, iostat=rc) ! day
        read(fhandle, *, iostat=rc) ! month
        read(fhandle, *, iostat=rc) ! year
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)

        Mfile = GetTemperatureRecord_FromM()
        if (GetTemperatureRecord_FromY() == 1901) then
            Yfile = Yeari
        else
            Yfile = GetTemperatureRecord_FromY()
        end if
        OK3 = .false.

        ! 2. IF 3 or less records
        if (GetTemperatureRecord_NrObs() <= 3) then
            call ReadMonth(C1Min, C1Max, fhandle, rc)
            X1 = n1
            select case (GetTemperatureRecord_NrObs())
            case (0)
                t1 = X1
                X2 = X1 + n1
                C2Min = C1Min
                C2Max = C1Max
                X3 = X2 + n1
                C3Min = C1Min
                C3Max = C1Max
            case (1)
                t1 = X1
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(C3Min, C3Max, fhandle, rc)
                if (Monthi == Mfile) then
                    C2Min = C3Min
                    C2Max = C3Max
                    X2 = X1 + n3
                    X3 = X2 + n3
                else
                    C2Min = C1Min
                    C2Max = C1Max
                    X2 = X1 + n1
                    X3 = X2 + n3
               end if
            case (2)
               if (Monthi == Mfile) then
                   t1 = 0
               end if
               Mfile = Mfile + 1
               if (Mfile > 12) then
                   call AdjustMONTHandYEAR(Mfile, Yfile)
               end if
               call ReadMonth(C2Min, C2Max, fhandle, rc)
               X2 = X1 + n2
               if (Monthi == Mfile) then
                   t1 = X1
               end if
               Mfile = Mfile + 1
               if (Mfile > 12) then
                   call AdjustMONTHandYEAR(Mfile, Yfile)
               end if
               call ReadMonth(C3Min, C3Max, fhandle, rc)
               X3 = X2 + n3
               if (Monthi == Mfile) then
                   t1 = X2
               end if
           end select
           OK3 = .true.
        end if

        ! 3. If first observation
        if ((.not. OK3) .and. ((Monthi == Mfile) .and. (Yeari == Yfile))) then
            t1 = 0
            call ReadMonth(C1Min, C1Max, fhandle, rc)
            X1 = n1
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(C2Min, C2Max, fhandle, rc)
            X2 = X1 + n2
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(C3Min, C3Max, fhandle, rc)
            X3 = X2 + n3
            OK3 = .true.
        end if

        ! 4. If last observation
        if ((.not. OK3) .and. (Monthi == GetTemperatureRecord_ToM())) then
            if ((GetTemperatureRecord_FromY() == 1901) &
                .or. (Yeari == GetTemperatureRecord_ToY())) then
                do Nri = 1, (GetTemperatureRecord_NrObs()-3)
                    read(fhandle, *, iostat=rc)
                    Mfile = Mfile + 1
                    if (Mfile > 12) then
                        call AdjustMONTHandYEAR(Mfile, Yfile)
                    end if
                end do
                call ReadMonth(C1Min, C1Max, fhandle, rc)
                X1 = n1
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(C2Min, C2Max, fhandle, rc)
                X2 = X1 + n2
                t1 = X2
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(C3Min, C3Max, fhandle, rc)
                X3 = X2 + n3
                OK3 = .true.
            end if
        end if

        ! 5. IF not previous cases
        if (.not. OK3) then
            Obsi = 1
            do while (.not. OK3)
                if ((Monthi == Mfile) .and. (Yeari == Yfile)) then
                   OK3 = .true.
                else
                   Mfile = Mfile + 1
                   if (Mfile > 12) then
                       call AdjustMONTHandYEAR(Mfile, Yfile)
                   end if
                  Obsi = Obsi + 1
                end if
            end do
            Mfile = GetTemperatureRecord_FromM()
            do Nri = 1, (Obsi-2)
                read(fhandle, *, iostat=rc)
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
            end do
            call ReadMonth(C1Min, C1Max, fhandle, rc)
            X1 = n1
            t1 = X1
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(C2Min, C2Max, fhandle, rc)
            X2 = X1 + n2
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(C3Min, C3Max, fhandle, rc)
            X3 = X2 + n3
        end if

        close(fhandle)
    end subroutine GetSetofThreeMonths

    subroutine ReadMonth(CiMin, CiMax, fhandle, rc)
        real(dp), intent(inout) :: CiMin
        real(dp), intent(inout) :: CiMax
        integer(int32), intent(in) :: fhandle
        integer(int32), intent(inout) :: rc

        integer(int32), parameter :: ni = 30
        character(len=255) :: StringREAD

        read(fhandle, '(a)', iostat=rc) StringREAD
        call SplitStringInTwoParams(StringREAD, CiMin, CiMax)
        ! simplification give better results for all cases
        CiMin = CiMin * ni
        CiMax = CiMax * ni
    end subroutine ReadMonth

    subroutine GetInterpolationParameters(C1, C2, C3, &
                          aOver3, bOver2, c)
        real(dp), intent(in) :: C1
        real(dp), intent(in) :: C2
        real(dp), intent(in) :: C3
        real(dp), intent(inout) :: aOver3
        real(dp), intent(inout) :: bOver2
        real(dp), intent(inout) :: c

        ! n1=n2=n3=30 --> better parabola
        aOver3 = (C1-2._dp*C2+C3)/(6._dp*30._dp*30._dp*30._dp)
        bOver2 = (-6._dp*C1+9._dp*C2-3._dp*C3)/(6._dp*30._dp*30._dp)
        c = (11._dp*C1-7._dp*C2+2._dp*C3)/(6._dp*30._dp)
    end subroutine GetInterpolationParameters

end subroutine GetMonthlyTemperatureDataSet


integer(int32) function GrowingDegreeDays(ValPeriod, &
               FirstDayPeriod, Tbase, Tupper, TDayMin, TDayMax)
    integer(int32), intent(in) :: ValPeriod
    integer(int32), intent(in) :: FirstDayPeriod
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(inout) :: TDayMin
    real(dp), intent(inout) :: TDayMax

    integer(int32) :: i, RemainingDays
    character(len=:), allocatable :: totalname
    integer(int32) :: fhandle, rc
    integer(int32) :: DayNri
    real(dp)       :: GDDays, DayGDD
    type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet
    character(len=255) :: StringREAD
    logical :: AdjustDayNri, file_exists

    GDDays = 0
    if (ValPeriod > 0) then
        if (GetTemperatureFile() == '(None)') then
            ! given average Tmin and Tmax
            DayGDD = DegreesDay(Tbase, Tupper, &
                     TDayMin, TDayMax, GetSimulParam_GDDMethod())
            GDDays = roundc(ValPeriod * DayGDD, mold=1_int32)
        else
            ! temperature file
            DayNri = FirstDayPeriod
            if (FullUndefinedRecord(GetTemperatureRecord_FromY(),&
                  GetTemperatureRecord_FromD(), GetTemperatureRecord_FromM(),&
                  GetTemperatureRecord_ToD(), GetTemperatureRecord_ToM())) then
                AdjustDayNri = .true.
                call SetDayNrToYundef(DayNri)
            else
                AdjustDayNri = .false.
            end if
            totalname = GetTemperatureFilefull()
            inquire(file=trim(totalname), exist=file_exists)
            if (file_exists .and. (GetTemperatureRecord_ToDayNr() > DayNri) &
                .and. (GetTemperatureRecord_FromDayNr() <= DayNri)) then
                RemainingDays = ValPeriod
                select case (GetTemperatureRecord_DataType())
                case (0) !Daily
                    open(newunit=fhandle, file=trim(totalname), &
                     status='old', action='read', iostat=rc)
                    read(fhandle, *, iostat=rc) ! description
                    read(fhandle, *, iostat=rc) ! time step
                    read(fhandle, *, iostat=rc) ! day
                    read(fhandle, *, iostat=rc) ! month
                    read(fhandle, *, iostat=rc) ! year
                    read(fhandle, *, iostat=rc)
                    read(fhandle, *, iostat=rc)
                    read(fhandle, *, iostat=rc)
                    do i = GetTemperatureRecord_FromDayNr(), (DayNri - 1)
                         read(fhandle, *, iostat=rc)
                    end do
                    read(fhandle, '(a)', iostat=rc) StringREAD ! i.e. Crop.Day1
                    call SplitStringInTwoParams(StringREAD, TDayMin, TDayMax)
                    DayGDD = DegreesDay(Tbase, Tupper, &
                                 TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    GDDays = GDDays + DayGDD
                    RemainingDays = RemainingDays - 1
                    DayNri = DayNri + 1
                    do while ((RemainingDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) &
                        .or. AdjustDayNri))
                        if (rc == iostat_end) then
                            read(fhandle, *, iostat=rc) ! description
                            read(fhandle, *, iostat=rc) ! time step
                            read(fhandle, *, iostat=rc) ! day
                            read(fhandle, *, iostat=rc) ! month
                            read(fhandle, *, iostat=rc) ! year
                            read(fhandle, *, iostat=rc)
                            read(fhandle, *, iostat=rc)
                            read(fhandle, *, iostat=rc)
                            read(fhandle, '(a)', iostat=rc) StringREAD
                            call SplitStringInTwoParams(StringREAD, &
                                     TDayMin, TDayMax)
                        else
                            read(fhandle, '(a)', iostat=rc) StringREAD
                            call SplitStringInTwoParams(StringREAD, &
                                     TDayMin, TDayMax)
                        end if
                        DayGDD = DegreesDay(Tbase, Tupper, &
                                     TDayMin, TDayMax,&
                                     GetSimulParam_GDDMethod())
                        GDDays = GDDays + DayGDD
                        RemainingDays = RemainingDays - 1
                        DayNri = DayNri + 1
                    end do
                    if (RemainingDays > 0) then
                        GDDays = undef_int
                    end if
                    close(fhandle)
                case(1) !Decadely:
                    call GetDecadeTemperatureDataSet(DayNri, TminDataSet,&
                                TmaxDataSet)
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDaymin = TminDataSet(i)%Param
                    TDaymax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, &
                                 TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    GDDays = GDDays + DayGDD
                    RemainingDays = RemainingDays - 1
                    DayNri = DayNri + 1
                    do while ((RemainingDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) &
                               .or. AdjustDayNri))
                        if (DayNri > TminDataSet(31)%DayNr) then
                            call GetDecadeTemperatureDataSet(DayNri, &
                                    TminDataSet, TmaxDataSet)
                        end if
                        i = 1
                        do while (TminDataSet(i)%DayNr /= DayNri)
                            i = i+1
                        end do
                        TDayMin = TminDataSet(i)%Param
                        TDayMax = TmaxDataSet(i)%Param
                        DayGDD = DegreesDay(Tbase, Tupper, &
                                     TDayMin, TDayMax,&
                                     GetSimulParam_GDDMethod())
                        GDDays = GDDays + DayGDD
                        RemainingDays = RemainingDays - 1
                        DayNri = DayNri + 1
                    end do
                    if (RemainingDays > 0) then
                        GDDays = undef_int
                    end if
                case(2) !Monthly:
                    call GetMonthlyTemperatureDataSet(DayNri, &
                            TminDataSet, TmaxDataSet)
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDayMin = TminDataSet(i)%Param
                    TDayMax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, &
                                 TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    GDDays = GDDays + DayGDD
                    RemainingDays = RemainingDays - 1
                    DayNri = DayNri + 1
                    do while((RemainingDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) &
                        .or. AdjustDayNri))
                        if (DayNri > TminDataSet(31)%DayNr) then
                            call GetMonthlyTemperatureDataSet(DayNri, &
                                 TminDataSet, TmaxDataSet)
                        end if
                        i = 1
                        do while (TminDataSet(i)%DayNr /= DayNri)
                            i = i+1
                        end do
                        TDayMin = TminDataSet(i)%Param
                        TDayMax = TmaxDataSet(i)%Param
                        DayGDD = DegreesDay(Tbase, Tupper, &
                              TDayMin, TDayMax, GetSimulParam_GDDMethod())
                        GDDays = GDDays + DayGDD
                        RemainingDays = RemainingDays - 1
                        DayNri = DayNri + 1
                    end do
                    if (RemainingDays > 0) then
                        GDDays = undef_int
                    end if
                end select
            end if !if temperaturefull file exists
        end if !if temperature file
    else    
        GDDays = undef_int
    endif !end valperiod>0
    GrowingDegreeDays = roundc(GDDays, mold=1_int32)
end function GrowingDegreeDays


integer(int32) function SumCalendarDays(ValGDDays, FirstDayCrop, &
                           Tbase, Tupper, TDayMin, TDayMax)
    integer(int32), intent(in) :: ValGDDays
    integer(int32), intent(in) :: FirstDayCrop
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(inout) :: TDayMin
    real(dp), intent(inout) :: TDayMax

    integer(int32) :: i
    integer(int32) :: fhandle, rc
    integer(int32) :: NrCDays
    character(len=:), allocatable :: totalname
    real(dp) :: RemainingGDDays, DayGDD
    integer(int32) :: DayNri
    type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet
    logical :: AdjustDayNri, file_exists
    character(len=255) :: StringREAD

    NrCdays = 0
    if (ValGDDays > 0) then
        if (GetTemperatureFile() == '(None)') then
            ! given average Tmin and Tmax
            DayGDD = DegreesDay(Tbase, Tupper, &
                       TDayMin, TDayMax, GetSimulParam_GDDMethod())
            if (DayGDD == 0) then
                NrCDays = -9
            else
                NrCDays = roundc(ValGDDays/DayGDD, mold=1_int32)
            end if
        else
            DayNri = FirstDayCrop
            if (FullUndefinedRecord(GetTemperatureRecord_FromY(), &
                  GetTemperatureRecord_FromD(), GetTemperatureRecord_FromM(), &
                  GetTemperatureRecord_ToD(), GetTemperatureRecord_ToM())) then
                AdjustDayNri = .true.
                call SetDayNrToYundef(DayNri)
            else
                AdjustDayNri = .false.
            end if
            totalname = GetTemperatureFilefull()
            inquire(file=trim(totalname), exist=file_exists)
            if (file_exists .and. (GetTemperatureRecord_ToDayNr() > DayNri) &
                .and. (GetTemperatureRecord_FromDayNr() <= DayNri)) then
                RemainingGDDays = ValGDDays
                select case (GetTemperatureRecord_DataType())
                case (0) ! Daily 
                    open(newunit=fhandle, file=trim(totalname), &
                         status='old', action='read', iostat=rc)
                    read(fhandle, *, iostat=rc) ! description
                    read(fhandle, *, iostat=rc) ! time step
                    read(fhandle, *, iostat=rc) ! day
                    read(fhandle, *, iostat=rc) ! month
                    read(fhandle, *, iostat=rc) ! year
                    read(fhandle, *, iostat=rc)
                    read(fhandle, *, iostat=rc)
                    read(fhandle, *, iostat=rc)
                    do i = GetTemperatureRecord_FromDayNr(), (DayNri - 1)
                         read(fhandle, *, iostat=rc)
                    end do
                    read(fhandle, '(a)', iostat=rc) StringREAD ! i.e. Crop.Day1
                    call SplitStringInTwoParams(StringREAD, TDayMin, TDayMax)
                    DayGDD = DegreesDay(Tbase, Tupper, &
                                 TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    NrCDays = NrCDays + 1
                    RemainingGDDays = RemainingGDDays - DayGDD
                    DayNri = DayNri + 1
                    do while ((RemainingGDDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) &
                        .or. AdjustDayNri))
                        if (rc == iostat_end) then
                            read(fhandle, *, iostat=rc) ! description
                            read(fhandle, *, iostat=rc) ! time step
                            read(fhandle, *, iostat=rc) ! day
                            read(fhandle, *, iostat=rc) ! month
                            read(fhandle, *, iostat=rc) ! year
                            read(fhandle, *, iostat=rc)
                            read(fhandle, *, iostat=rc)
                            read(fhandle, *, iostat=rc)
                            read(fhandle, '(a)', iostat=rc) StringREAD
                            call SplitStringInTwoParams(StringREAD, &
                                     TDayMin, TDayMax)
                        else
                            read(fhandle, '(a)', iostat=rc) StringREAD
                            call SplitStringInTwoParams(StringREAD, &
                                     TDayMin, TDayMax)
                        end if
                        DayGDD = DegreesDay(Tbase, Tupper, &
                                     TDayMin, TDayMax,&
                                     GetSimulParam_GDDMethod())
                        NrCDays = NrCDays + 1
                        RemainingGDDays = RemainingGDDays - DayGDD
                        DayNri = DayNri + 1
                    end do
                    if (RemainingGDDays > 0) then
                        NrCDays = undef_int
                    end if
                    close(fhandle)
                case(1) !Decadely
                    call GetDecadeTemperatureDataSet(DayNri, &
                      TminDataSet, TmaxDataSet)
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDaymin = TminDataSet(i)%Param
                    TDaymax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, &
                               TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    NrCDays = NrCDays + 1
                    RemainingGDDays = RemainingGDDays - DayGDD
                    DayNri = DayNri + 1
                    do while ((RemainingGDDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) &
                         .or. AdjustDayNri))
                        if (DayNri > TminDataSet(31)%DayNr) then
                            call GetDecadeTemperatureDataSet(DayNri, &
                              TminDataSet, TmaxDataSet)
                        end if
                        i = 1
                        do while (TminDataSet(i)%DayNr /= DayNri)
                            i = i+1
                        end do
                        TDayMin = TminDataSet(i)%Param
                        TDayMax = TmaxDataSet(i)%Param
                        DayGDD = DegreesDay(Tbase, Tupper, &
                             TDayMin, TDayMax, GetSimulParam_GDDMethod())
                        NrCDays = NrCDays + 1
                        RemainingGDDays = RemainingGDDays - DayGDD
                        DayNri = DayNri + 1
                    end do
                    if (RemainingGDDays > 0) then
                        NrCDays = undef_int
                    end if
                case(2) !Monthly :
                    call GetMonthlyTemperatureDataSet(DayNri, &
                           TminDataSet, TmaxDataSet)
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDayMin = TminDataSet(i)%Param
                    TDayMax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, &
                               TDayMin, TDayMax, GetSimulParam_GDDMethod())
                    NrCDays = NrCDays + 1
                    RemainingGDDays = RemainingGDDays - DayGDD
                    DayNri = DayNri + 1
                    do while ((RemainingGDDays > 0) &
                        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) & 
                         .or. AdjustDayNri))
                        if (DayNri > TminDataSet(31)%DayNr) then
                            call GetMonthlyTemperatureDataSet(DayNri, &
                                   TminDataSet, TmaxDataSet)
                        end if
                        i = 1
                        do while (TminDataSet(i)%DayNr /= DayNri)
                            i = i+1
                        end do
                        TDayMin = TminDataSet(i)%Param
                        TDayMax = TmaxDataSet(i)%Param
                        DayGDD = DegreesDay(Tbase, Tupper, &
                             TDayMin, TDayMax, GetSimulParam_GDDMethod())
                        NrCDays = NrCDays + 1
                        RemainingGDDays = RemainingGDDays - DayGDD
                        DayNri = DayNri + 1
                    end do
                    if (RemainingGDDays > 0) then
                        NrCDays = undef_int
                    end if
                end select
            else
                NrCDays = undef_int
            endif
        endif
    endif
    SumCalendarDays = NrCDays
end function SumCalendarDays


subroutine GDDCDCToCDC(PlantDayNr, D123, GDDL123, &
                       GDDHarvest, CCx, GDDCDC, Tbase, Tupper, &
                       NoTempFileTMin, NoTempFileTMax, CDC)
    integer(int32), intent(in) :: PlantDayNr
    integer(int32), intent(in) :: D123
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDHarvest
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(inout) :: NoTempFileTMin
    real(dp), intent(inout) :: NoTempFileTMax
    real(dp), intent(inout) :: CDC

    integer(int32) :: ti, GDDi
    real(dp) :: CCi

    GDDi = LengthCanopyDecline(CCx, GDDCDC)
    if ((GDDL123+GDDi) <= GDDHarvest) then
        CCi = 0._dp ! full decline
    else
        ! partly decline
        if (GDDL123 < GDDHarvest) then
            GDDi = GDDHarvest - GDDL123
        else
            GDDi = 5._dp
        end if
        CCi = CCx * (1._dp - 0.05 * (exp(GDDi*(GDDCDC*3.33)/(CCx+2.29))-1_dp) )
       ! CC at time ti
    end if
    ti = SumCalendarDays(GDDi, (PlantDayNr+D123),&
              Tbase, Tupper, NoTempFileTMin, NoTempFileTMax)
    if (ti > 0) then
        CDC = (((CCx+2.29_dp)/ti) &
                * log(1._dp + ((1._dp-CCi/CCx)/0.05_dp)))/3.33_dp
    else
        CDC = undef_int
    end if
end subroutine GDDCDCToCDC


subroutine HIadjColdHeat(TempFlower, TempLengthFlowering, &
                         TempHI, TempTmin, TempTmax, &
                         TempTcold, TempTheat, TempfExcess, &
                         HIadjusted, ColdStress, HeatStress)
    integer(int32), intent(in) :: TempFlower
    integer(int32), intent(in) :: TempLengthFlowering
    integer(int32), intent(in) :: TempHI
    real(dp), intent(in) :: TempTmin
    real(dp), intent(in) :: TempTmax
    integer(int8), intent(in) :: TempTcold
    integer(int8), intent(in) :: TempTheat
    integer(int16), intent(in) :: TempfExcess
    real(dp), intent(inout) :: HIadjusted
    logical, intent(inout) :: ColdStress
    logical, intent(inout) :: HeatStress

    integer(int32), parameter :: TempRange = 5
    integer(int32) :: fhandle
    integer(int32) :: Dayi,rc
    real(dp) :: Tndayi, Txdayi, KsPol, KsPolCS, KsPolHS, fFlor

    ! 1. Open Temperature file
    if (GetTemperatureFile() /= '(None)') then
        open(newunit=fhandle, &
             file=trim(GetPathNameSimul())//trim('TCrop.SIM'),&
             status='old', action='read', iostat=rc)
        do Dayi = 1, (TempFlower-1)
            read(fhandle, *, iostat=rc)
        end do
    end if

    ! 2. Initialize
    HIadjusted = 0._dp
    ColdStress = .false.
    HeatStress = .false.

    ! 3. Cold or Heat stress affecting pollination
    do Dayi = 1, TempLengthFlowering
        ! 3.1 Read air temperature
        if (GetTemperatureFile() /= '(None)') then
            read(fhandle, *, iostat=rc) Tndayi, Txdayi
        else
            Tndayi = TempTmin
            Txdayi = TempTmax
        end if
        ! 3.2 Fraction of flowers which are flowering on day  (fFlor)
        fFlor = FractionFlowering(dayi)
        ! 3.3 Ks(pollination) cold stress
        KsPolCS = KsTemperature(real(TempTcold-TempRange, kind=dp), &
                      real(TempTcold, kind=dp), Tndayi)
        if (roundc(10000*KsPolCS, mold=1_int32) < 10000) then
            ColdStress = .true.
        end if
        ! 3.4 Ks(pollination) heat stress
        KsPolHS = KsTemperature(real(TempTheat+TempRange, kind=dp), &
                     real(TempTheat, kind=dp), Txdayi)
        if (roundc(10000*KsPolHS, mold=1_int32) < 10000) then
            HeatStress = .true.
        end if
        ! 3.5 Adjust HI
        KsPol = 1
        if (KsPol > KsPolCS) then
            KsPol = KsPolCS
        end if
        if (KsPol > KsPolHS) then
            KsPol = KsPolHS
        end if
        HIadjusted = HIadjusted + (KsPol * (1._dp + TempfExcess/100._dp) &
                                         * fFlor * TempHI)
        if (HIadjusted > TempHI) then
            HIadjusted = TempHI
        end if
    end do

    ! 3. Close Temperature file
    if (GetTemperatureFile() /= '(None)') then
        close(fhandle)
    end if

    contains

    real(dp) function FractionFlowering(Dayi)
      integer(int32), intent(in) :: Dayi

      real(dp) :: f1, f2, F
      integer(int32) :: DiFlor

      if (TempLengthFlowering <=1) then
          F = 1._dp
      else
          DiFlor = Dayi
          f2 = FractionPeriod(DiFlor,TempLengthFlowering)
          DiFlor = Dayi-1
          f1 = FractionPeriod(DiFlor,TempLengthFlowering)
          f1 = FractionPeriod(DiFlor,TempLengthFlowering)
          if (abs(f1-f2) < 0.0000001) then
              F = 0._dp
          else
              F = ((f1+f2)/2._dp)* 100._dp/TempLengthFlowering
          end if
      end if
      FractionFlowering = F
    end function FractionFlowering

    real(dp) function FractionPeriod(DiFlor,TempLengthFlowering)
        integer(int32), intent(in) :: DiFlor,TempLengthFlowering

        real(dp) :: fi, TimePerc

        if (DiFlor <= 0) then
            fi = 0._dp
        else
            TimePerc = 100._dp * (DiFlor/TempLengthFlowering)
            if (TimePerc > 100) then
                fi = 1._dp
            else
                fi = 0.00558 * exp(0.63*log(TimePerc)) - &
                     0.000969 * TimePerc - 0.00383
                if (fi < 0) then
                    fi = 0._dp
                end if
            end if
        end if
        FractionPeriod = fi
    end function FractionPeriod

end subroutine HIadjColdHeat


integer(int32) function ResetCropDay1(CropDay1IN, SwitchToYear1)
    integer(int32), intent(in) :: CropDay1IN
    logical, intent(in) :: SwitchToYear1

    integer(int32) :: CropDay1OUT
    integer(int32) :: dayi, monthi, yeari

    call DetermineDate(CropDay1IN, dayi, monthi, yeari)
    if (GetTemperatureRecord_FromY() == 1901) then
        yeari = 1901
        call DetermineDayNr(Dayi, Monthi, Yeari, CropDay1OUT)
    else
        if (SwitchToYear1) then
            call DetermineDayNr(Dayi, Monthi, &
                 GetTemperatureRecord_FromY(), CropDay1OUT)
        else
            CropDay1OUT = CropDay1IN
        end if
    end if
    ResetCropDay1 = CropDay1OUT
end function ResetCropDay1

subroutine TemperatureFileCoveringCropPeriod(CropFirstDay, CropLastDay)
    integer(int32), intent(in) :: CropFirstDay
    integer(int32), intent(in) :: CropLastDay

    character(len=:), allocatable :: totalname, totalnameOUT
    integer(int32) :: fhandle1, fhandle2
    integer(int32) :: i, RunningDay, rc
    character(len=255) :: StringREAD
    type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet
    real(dp) :: Tlow, Thigh
    logical  :: file_exists

    totalname = GetTemperatureFilefull()
    inquire(file=trim(totalname), exist=file_exists)
    if (file_exists) then
        ! open file and find first day of cropping period
        select case (GetTemperatureRecord_DataType())
        case (0) !Daily   
            open(newunit=fhandle1, file=trim(totalname), &
                     status='old', action='read', iostat=rc)
            read(fhandle1, *, iostat=rc) ! description
            read(fhandle1, *, iostat=rc) ! time step
            read(fhandle1, *, iostat=rc) ! day
            read(fhandle1, *, iostat=rc) ! month
            read(fhandle1, *, iostat=rc) ! year
            read(fhandle1, *, iostat=rc)
            read(fhandle1, *, iostat=rc)
            read(fhandle1, *, iostat=rc)
            do i = GetTemperatureRecord_FromDayNr(), (CropFirstDay - 1)
                read(fhandle1, *, iostat=rc)
            end do
            read(fhandle1, '(a)', iostat=rc) StringREAD ! i.e. Crop.Day1
            call SplitStringInTwoParams(StringREAD, Tlow, Thigh)
        case (1) !Decadely
            call GetDecadeTemperatureDataSet(CropFirstDay, TminDataSet, &
                        TmaxDataSet)
            i = 1
            do while (TminDataSet(i)%DayNr /= CropFirstDay)
                i = i+1
            end do
            Tlow = TminDataSet(i)%Param
            Thigh = TmaxDataSet(i)%Param
        case (2) !Monthly 
            call GetMonthlyTemperatureDataSet(CropFirstDay, TminDataSet, TmaxDataSet)
            i = 1
            do while (TminDataSet(i)%DayNr /= CropFirstDay)
                i = i+1
            end do
            Tlow = TminDataSet(i)%Param
            Thigh = TmaxDataSet(i)%Param
        end select
        ! create SIM file and record first day
        totalnameOUT = trim(GetPathNameSimul())//'TCrop.SIM'
        open(newunit=fhandle2, file=trim(totalnameOUT), &
             action='write')
        write(fhandle2, '(2f10.4)') Tlow, Thigh
        ! next days of simulation period
        do RunningDay = (CropFirstDay + 1), CropLastDay
            select case (GetTemperatureRecord_DataType())
            case (0) !Daily   
                if (rc == iostat_end) then
                    read(fhandle1, *, iostat=rc) ! description
                    read(fhandle1, *, iostat=rc) ! time step
                    read(fhandle1, *, iostat=rc) ! day
                    read(fhandle1, *, iostat=rc) ! month
                    read(fhandle1, *, iostat=rc) ! year
                    read(fhandle1, *, iostat=rc)
                    read(fhandle1, *, iostat=rc)
                    read(fhandle1, *, iostat=rc)
                    read(fhandle1, '(a)', iostat=rc) StringREAD
                    call SplitStringInTwoParams(StringREAD, Tlow, Thigh)
                else
                    read(fhandle1, *, iostat=rc) Tlow, Thigh
                end if
            case (1) !Decadely:
                if (RunningDay > TminDataSet(31)%DayNr) then
                    call GetDecadeTemperatureDataSet(RunningDay, TminDataSet,&
                        TmaxDataSet)
                end if
                i = 1
                do while (TminDataSet(i)%DayNr /= RunningDay)
                    i = i+1
                end do
                Tlow = TminDataSet(i)%Param
                Thigh = TmaxDataSet(i)%Param
            case (2) !Monthly :
               if (RunningDay > TminDataSet(31)%DayNr) then
                    call GetMonthlyTemperatureDataSet(RunningDay, TminDataSet,&
                        TmaxDataSet)
               end if
               i = 1
               do while (TminDataSet(i)%DayNr /= RunningDay)
                   i = i+1
               end do
               Tlow = TminDataSet(i)%Param
               Thigh = TmaxDataSet(i)%Param
            end select
            write(fhandle2, '(2f10.4)') Tlow, Thigh
        end do
        ! Close files
        if (GetTemperatureRecord_DataType() == 0) then !Daily
           close(fhandle1)
        end if
        close(fhandle2)
    else
        write(*,*) 'ERROR: no valid air temperature file'
        return
        ! fatal error if no air temperature file
    endif
end subroutine TemperatureFileCoveringCropPeriod

end module ac_tempprocessing
