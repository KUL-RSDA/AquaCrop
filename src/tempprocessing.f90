module ac_tempprocessing

use ac_kinds,  only: int32, dp

use ac_global , only: DaysinMonth, &
                      TemperatureRecord, &
                      TemperatureFile, &
                      TemperatureFileFull, &
                      rep_DayEventDbl, &
                      DetermineDayNr, &
                      DetermineDate, &
                      LeapYear


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
        open(newunit=fhandle, file=trim(TemperatureFilefull), &
                     status='old', action='read', iostat=rc)
        read(fhandle, *, iostat=rc) ! description
        read(fhandle, *, iostat=rc) ! time step
        read(fhandle, *, iostat=rc) ! day
        read(fhandle, *, iostat=rc) ! month
        read(fhandle, *, iostat=rc) ! year
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)

        if (TemperatureRecord%FromD > 20) then
            DecFile = 3
        elseif (TemperatureRecord%FromD > 10) then
            DecFile = 2
        else
            DecFile = 1
        end if
        Mfile = TemperatureRecord%FromM
        if (TemperatureRecord%FromY == 1901) then
            Yfile = Yeari
        else
            Yfile = TemperatureRecord%FromY
        end if
        OK3 = .false.

        if (TemperatureRecord%NrObs <= 2) then
            read(fhandle, iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            select case (TemperatureRecord%NrObs)
            case (1)
                C2Min = C1Min
                C2Max = C2Max
                C3Min = C1Min
                C3Max = C1Max
            case (2)
                DecFile = DecFile + 1
                if (DecFile > 3) then
                    call AdjustDecadeMONTHandYEAR(DecFile, Mfile, Yfile)
                end if
                read(fhandle, iostat=rc) StringREAD
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
            read(fhandle, iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            C2Min = C1Min
            C2Max = C1Max
            read(fhandle, iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C3Min, C3Max)
            C1Min = C2Min + (C2Min-C3Min)/4._dp
            C1Max = C2Max + (C2Max-C3Max)/4._dp
            OK3 = .true.
        end if

        if ((.not. OK3) .and. ((DayN == TemperatureRecord%ToD) &
             .and. (Monthi == TemperatureRecord%ToM))) then
            if ((TemperatureRecord%FromY == 1901) .or. &
                (Yeari == TemperatureRecord%ToY)) then
                do Nri = 1, (TemperatureRecord%NrObs-2)
                     read(fhandle, *, iostat=rc)
                end do
                read(fhandle, iostat=rc) StringREAD
                call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
                read(fhandle, iostat=rc) StringREAD
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
            if (TemperatureRecord%FromD > 20) then
                DecFile = 3
            elseif (TemperatureRecord%FromD > 10) then
                DecFile = 2
            else
                DecFile = 1
            end if
            do Nri = 1, (Obsi-2)
                read(fhandle, *, iostat=rc)
            end do
            read(fhandle, iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C1Min, C1Max)
            read(fhandle, iostat=rc) StringREAD
            call SplitStringInTwoParams(StringREAD, C2Min, C2Max)
            read(fhandle, iostat=rc) StringREAD
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
                   X1, X2, X3, aOver3Min, bOver2Min, cMin)
    call GetInterpolationParameters(C1Max, C2Max, C3Max, &
                   X1, X2, X3, aOver3Max, bOver2Max, cMax)
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
        character(len=255) :: StringREAD

        ! 1. Prepare record
        open(newunit=fhandle, file=trim(TemperatureFilefull), &
                     status='old', action='read', iostat=rc)
        read(fhandle, *, iostat=rc) ! description
        read(fhandle, *, iostat=rc) ! time step
        read(fhandle, *, iostat=rc) ! day
        read(fhandle, *, iostat=rc) ! month
        read(fhandle, *, iostat=rc) ! year
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)
        read(fhandle, *, iostat=rc)

        Mfile = TemperatureRecord%FromM
        if (TemperatureRecord%FromY == 1901) then
            Yfile = Yeari
        else
            Yfile = TemperatureRecord%FromY
        end if
        OK3 = .false.

        ! 2. IF 3 or less records
        if (TemperatureRecord%NrObs <= 3) then
            call ReadMonth(Mfile, Yfile, n1, C1Min, C1Max, fhandle, rc)
            X1 = n1
            select case (TemperatureRecord%NrObs)
            case (1)
                t1 = X1
                X2 = X1 + n1
                C2Min = C1Min
                C2Max = C1Max
                X3 = X2 + n1
                C3Min = C1Min
                C3Max = C1Max
            case (2)
                t1 = X1
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(Mfile, Yfile, n3, C3Min, C3Max, fhandle, rc)
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
            case (3)
               if (Monthi == Mfile) then
                   t1 = 0
               end if
               Mfile = Mfile + 1
               if (Mfile > 12) then
                   call AdjustMONTHandYEAR(Mfile, Yfile)
               end if
               call ReadMonth(Mfile, Yfile, n2, C2Min, C2Max, fhandle, rc)
               X2 = X1 + n2
               if (Monthi == Mfile) then
                   t1 = X1
               end if
               Mfile = Mfile + 1
               if (Mfile > 12) then
                   call AdjustMONTHandYEAR(Mfile, Yfile)
               end if
               call ReadMonth(Mfile, Yfile, n3, C3Min, C3Max, fhandle, rc)
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
            call ReadMonth(Mfile, Yfile, n1, C1Min, C1Max, fhandle, rc)
            X1 = n1
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(Mfile, Yfile, n2, C2Min, C2Max, fhandle, rc)
            X2 = X1 + n2
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(Mfile, Yfile, n3, C3Min, C3Max, fhandle, rc)
            X3 = X2 + n3
            OK3 = .true.
        end if

        ! 4. If last observation
        if ((.not. OK3) .and. (Monthi == TemperatureRecord%ToM)) then
            if ((TemperatureRecord%FromY == 1901) &
                .or. (Yeari == TemperatureRecord%ToY)) then
                do Nri = 1, (TemperatureRecord%NrObs-3)
                    read(fhandle, *, iostat=rc)
                    Mfile = Mfile + 1
                    if (Mfile > 12) then
                        call AdjustMONTHandYEAR(Mfile, Yfile)
                    end if
                end do
                call ReadMonth(Mfile, Yfile, n1, C1Min, C1Max, fhandle, rc)
                X1 = n1
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(Mfile, Yfile, n2, C2Min, C2Max, fhandle, rc)
                X2 = X1 + n2
                t1 = X2
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
                call ReadMonth(Mfile, Yfile, n3, C3Min, C3Max, fhandle, rc)
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
            Mfile = TemperatureRecord%FromM
            do Nri = 1, (Obsi-2)
                read(fhandle, *, iostat=rc)
                Mfile = Mfile + 1
                if (Mfile > 12) then
                    call AdjustMONTHandYEAR(Mfile, Yfile)
                end if
            end do
            call ReadMonth(Mfile, Yfile, n1, C1Min, C1Max, fhandle, rc)
            X1 = n1
            t1 = X1
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(Mfile, Yfile, n2, C2Min, C2Max, fhandle, rc)
            X2 = X1 + n2
            Mfile = Mfile + 1
            if (Mfile > 12) then
                call AdjustMONTHandYEAR(Mfile, Yfile)
            end if
            call ReadMonth(Mfile, Yfile, n3, C3Min, C3Max, fhandle, rc)
            X3 = X2 + n3
        end if

        close(fhandle)
    end subroutine GetSetofThreeMonths

    subroutine ReadMonth(Mfile, Yfile, ni, CiMin, CiMax, &
                  fhandle, rc)
        integer(int32), intent(in) :: Mfile
        integer(int32), intent(in) :: Yfile
        integer(int32), intent(inout) :: ni
        real(dp), intent(inout) :: CiMin
        real(dp), intent(inout) :: CiMax
        integer(int32), intent(in) :: fhandle
        integer(int32), intent(inout) :: rc

        character(len=255) :: StringREAD

        read(fhandle, iostat=rc) StringREAD
        call SplitStringInTwoParams(StringREAD, CiMin, CiMax)
        ! simplification give better results for all cases
        ni = 30
        CiMin = CiMin * ni
        CiMax = CiMax * ni
    end subroutine ReadMonth


    subroutine GetInterpolationParameters(C1, C2, C3, &
                          X1, X2, X3, aOver3, bOver2, c)
        real(dp), intent(in) :: C1
        real(dp), intent(in) :: C2
        real(dp), intent(in) :: C3
        integer(int32), intent(in) :: X1
        integer(int32), intent(in) :: X2
        integer(int32), intent(in) :: X3
        real(dp), intent(inout) :: aOver3
        real(dp), intent(inout) :: bOver2
        real(dp), intent(inout) :: c

        ! n1=n2=n3=30 --> better parabola
        aOver3 = (C1-2._dp*C2+C3)/(6._dp*30._dp*30._dp*30._dp)
        bOver2 = (-6._dp*C1+9._dp*C2-3._dp*C3)/(6._dp*30._dp*30._dp)
        c = (11._dp*C1-7._dp*C2+2._dp*C3)/(6._dp*30._dp)
    end subroutine GetInterpolationParameters

end subroutine GetMonthlyTemperatureDataSet

end module ac_tempprocessing
