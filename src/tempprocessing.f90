module ac_tempprocessing

use ac_kinds,  only: dp, &
                     int8, &
                     int16, &   
                     int32, &
                     intEnum

use iso_fortran_env, only: iostat_end

use ac_global , only: undef_int, &
                      roundc, &
                      modeCycle_GDDays, &
                      modeCycle_CalendarDays, &
                      DaysinMonth, &
                      rep_DayEventDbl, &
                      rep_CropFileSet, &
                      rep_EffectStress, &
                      rep_Shapes, &
                      subkind_Vegetative, &
                      subkind_Grain, &
                      subkind_Tuber, &
                      subkind_Forage, &
                      datatype_daily, &
                      datatype_decadely, &
                      datatype_monthly, &
                      CalculateETpot, &
                      CCiNoWaterStressSF, &
                      CanopyCoverNoStressSF, &
                      DetermineDayNr, &
                      DetermineDate, &
                      GetDaySwitchToLinear, &
                      HarvestIndexGrowthCoefficient, &
                      LeapYear, &
                      SeasonalSumOfKcPot, &
                      SplitStringInTwoParams, &
                      KsTemperature, &
                      DegreesDay, &
                      LengthCanopyDecline, &
                      DetermineLengthGrowthStages, &
                      CanopyCoverNoStressSF, &
                      CCmultiplierWeed, &
                      CCiNoWaterStressSF, &
                      CalculateETpot, &
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
                      FullUndefinedRecord, &
                      SetCrop_GDDaysToFullCanopy, &
                      SetCrop_GDDaysToHIo, &
                      SetCrop_DaysToHarvest, &
                      SetCrop_DaysToSenescence, &
                      SetCrop_CDC, &
                      SetCrop_DaysToHIo, &
                      SetCrop_DaysToGermination, &
                      SetCrop_LengthFlowering, &
                      SetCrop_Length, &    
                      SetCrop_DaysToFullCanopy, &
                      SetCrop_dHIdt, &
                      SetCrop_DaysToMaxRooting, &
                      SetCrop_CGC, &
                      SetCrop_DaysToFlowering, &
                      SetSimulation_DelayedDays, &
                      SetSimulation_DelayedDays, &
                      GetCrop_GDDaysToGermination, &
                      GetCrop_GDDaysToFullCanopy, &
                      GetCrop_GDDaysToHarvest,   GetCrop_GDDaysToHIo, &
                      GetCrop_GDDaysToFlowering, GetCrop_GDDLengthFlowering, &
                      GetCrop_GDDaysToSenescence,GetCrop_GDDaysToHarvest, &
                      GetCrop_GDDaysToMaxRooting, &
                      GetCrop_DaysToGermination, &
                      GetCrop_DaysToFullCanopy, &
                      GetCrop_DaysToFlowering, & 
                      GetCrop_LengthFlowering, &
                      GetCrop_DaysToSenescence, &
                      GetCrop_DaysToHarvest, &
                      GetCrop_DaysToMaxRooting, &
                      GetCrop_DaysToHIo, &
                      GetCrop_Length, &
                      GetCrop_GDDCGC, GetCrop_GDDCDC, GetCrop_CCo, &
                      GetCrop_CCx, GetCrop_HI, &
                      GetCrop_CGC, &
                      GetCrop_CDC, &
                      GetCrop_dHIdt, &
                      GetCrop_DaysToCCini, GetCrop_Planting, &
                      GetCrop_subkind, &
                      GetCrop_ModeCycle, &
                      GetCrop_Tbase, & 
                      GetCrop_Tupper, &
                      GetSimulParam_Tmin, GetSimulParam_Tmax, &
                      SetSimulation_DelayedDays,&
                      GetManagement_WeedAdj, &
                      GetSimulParam_Tmin, GetSimulParam_Tmax,&
                      GetWeedRC, &
                      DaysToReachCCwithGivenCGC, &
                      timetomaxcanopysf, &
                      cropstressparameterssoilfertility

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

        integer(int32), parameter :: n1 = 30
        integer(int32), parameter :: n2 = 30
        integer(int32), parameter :: n3 = 30
        integer(int32) :: fhandle
        integer(int32) :: Mfile, Yfile, Nri, Obsi, rc
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
                case (datatype_daily)
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
                            rewind(fhandle)
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
                case(datatype_decadely)
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
                case(datatype_monthly)
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
            if (abs(DayGDD) < epsilon(1._dp)) then
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
                case (datatype_daily)
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
                            rewind(fhandle)
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
                case(datatype_decadely)
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
                case(datatype_monthly)
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


real(dp) function MaxAvailableGDD(FromDayNr, Tbase, Tupper, TDayMin, TDayMax)
    integer(int32), intent(inout) :: FromDayNr
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(inout) :: TDayMin
    real(dp), intent(inout) :: TDayMax

    integer(int32) :: i
    integer(int32) :: fhandle, rc
    real(dp) :: MaxGDDays, DayGDD
    character(len=:), allocatable :: totalname
    integer(int32) :: DayNri
    type(rep_DayEventDbl), dimension(31) :: TminDataSet, TmaxDataSet
    character(len=255) :: StringREAD
    logical :: file_exists

    MaxGDDays = 100000._dp
    if (GetTemperatureFile() == '(None)') then
        DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, &
                       GetSimulParam_GDDMethod())
        if (DayGDD <= epsilon(1._dp)) then
            MaxGDDays = 0._dp
        end if
    else
        MaxGDDays = 0._dp
        if (FullUndefinedRecord(GetTemperatureRecord_FromY(),&
               GetTemperatureRecord_FromD(), GetTemperatureRecord_FromM(),&
               GetTemperatureRecord_ToD(), GetTemperatureRecord_ToM())) then
            FromDayNr = GetTemperatureRecord_FromDayNr()  ! since we have 365 days anyway
        end if
        DayNri = FromDayNr
        totalname = GetTemperatureFilefull()
        inquire(file=trim(totalname), exist=file_exists)
        if (file_exists .and. (GetTemperatureRecord_ToDayNr() > FromDayNr) &
             .and. (GetTemperatureRecord_FromDayNr() <= FromDayNr)) then
            select case (GetTemperatureRecord_DataType())
            case (datatype_daily)
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
                do i = GetTemperatureRecord_FromDayNr(), (FromDayNr - 1)
                    read(fhandle, *, iostat=rc)
                end do
                read(fhandle, '(a)', iostat=rc) StringREAD
                call SplitStringInTwoParams(StringREAD, TDayMin, TDayMax)
                DayNri = DayNri + 1
                DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, &
                            GetSimulParam_GDDMethod())
                MaxGDDays = MaxGDDays + DayGDD
                do while (DayNri < GetTemperatureRecord_ToDayNr())
                    if (rc == iostat_end) then
                        rewind(fhandle)
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
                    DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, &
                                GetSimulParam_GDDMethod())
                    MaxGDDays = MaxGDDays + DayGDD
                    DayNri = DayNri + 1
                end do
            case (datatype_decadely)
                call GetDecadeTemperatureDataSet(DayNri, TminDataSet,&
                         TmaxDataSet)
                i = 1
                do while (TminDataSet(i)%DayNr /= DayNri)
                    i = i+1
                end do
                TDaymin = TminDataSet(i)%Param
                TDaymax = TmaxDataSet(i)%Param
                DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, &
                              GetSimulParam_GDDMethod())
                MaxGDDays = MaxGDDays + DayGDD
                DayNri = DayNri + 1
                do while(DayNri < GetTemperatureRecord_ToDayNr())
                    if (DayNri > TminDataSet(31)%DayNr) then
                        call GetDecadeTemperatureDataSet(DayNri, TminDataSet,&
                                TmaxDataSet)
                    end if
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDayMin = TminDataSet(i)%Param
                    TDayMax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax,&
                                 GetSimulParam_GDDMethod())
                    MaxGDDays = MaxGDDays + DayGDD
                    DayNri = DayNri + 1
                end do
            case (datatype_monthly)
                call GetMonthlyTemperatureDataSet(DayNri, TminDataSet,&
                           TmaxDataSet)
                i = 1
                do while (TminDataSet(i)%DayNr /= DayNri)
                    i = i+1
                end do
                TDayMin = TminDataSet(i)%Param
                TDayMax = TmaxDataSet(i)%Param
                DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax,&
                             GetSimulParam_GDDMethod())
                MaxGDDays = MaxGDDays + DayGDD
                DayNri = DayNri + 1
                do while (DayNri < GetTemperatureRecord_ToDayNr())
                    if (DayNri > TminDataSet(31)%DayNr) then
                        call GetMonthlyTemperatureDataSet(DayNri, TminDataSet,&
                                  TmaxDataSet)
                    end if
                    i = 1
                    do while (TminDataSet(i)%DayNr /= DayNri)
                        i = i+1
                    end do
                    TDayMin = TminDataSet(i)%Param
                    TDayMax = TmaxDataSet(i)%Param
                    DayGDD = DegreesDay(Tbase, Tupper, TDayMin, TDayMax,&
                                 GetSimulParam_GDDMethod())
                    MaxGDDays = MaxGDDays + DayGDD
                    DayNri = DayNri + 1
                end do
            end select
        end if
    end if
    MaxAvailableGDD = MaxGDDays
end function MaxAvailableGDD


subroutine AdjustCalendarDays(PlantDayNr, InfoCropType,&
              Tbase, Tupper, NoTempFileTMin, NoTempFileTMax,&
              GDDL0, GDDL12, GDDFlor, GDDLengthFlor, GDDL123,&
              GDDHarvest, GDDLZmax, GDDHImax, GDDCGC, GDDCDC,&
              CCo, CCx, IsCGCGiven, HIndex, TheDaysToCCini,&
              ThePlanting, D0, D12, DFlor, LengthFlor,&
              D123, DHarvest, DLZmax, LHImax, StLength,&
              CGC, CDC, dHIdt, Succes)
    integer(int32), intent(in) :: PlantDayNr
    integer(intEnum), intent(in) :: InfoCropType
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: NoTempFileTMin
    real(dp), intent(in) :: NoTempFileTMax
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDFlor
    integer(int32), intent(in) :: GDDLengthFlor
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDHarvest
    integer(int32), intent(in) :: GDDLZmax
    integer(int32), intent(inout) :: GDDHImax
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    logical, intent(in) :: IsCGCGiven
    integer(int32), intent(in) :: HIndex
    integer(int32), intent(in) :: TheDaysToCCini
    integer(intEnum), intent(in) :: ThePlanting
    integer(int32), intent(inout) :: D0
    integer(int32), intent(inout) :: D12
    integer(int32), intent(inout) :: DFlor
    integer(int32), intent(inout) :: LengthFlor
    integer(int32), intent(inout) :: D123
    integer(int32), intent(inout) :: DHarvest
    integer(int32), intent(inout) :: DLZmax
    integer(int32), intent(inout) :: LHImax
    integer(int32), dimension(4), intent(inout) :: StLength
    real(dp), intent(inout) :: CGC
    real(dp), intent(inout) :: CDC
    real(dp), intent(inout) :: dHIdt
    logical, intent(inout) :: Succes

    real(dp) :: tmp_NoTempFileTMin,tmp_NoTempFileTMax

    tmp_NoTempFileTMin = NoTempFileTMin
    tmp_NoTempFileTMax = NoTempFileTMax

    Succes = .true.
    D0 = SumCalendarDays(GDDL0, PlantDayNr, &
            Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
    D12 = SumCalendarDays(GDDL12, PlantDayNr,&
            Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
    if (InfoCropType /= subkind_Forage) then
        D123 = SumCalendarDays(GDDL123, PlantDayNr,&
                 Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
        DHarvest = SumCalendarDays(GDDHarvest, PlantDayNr,&
                     Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
    end if
    DLZmax = SumCalendarDays(GDDLZmax, PlantDayNr,&
               Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
    select case (InfoCropType)
    case (subkind_Grain, subkind_Tuber)
        DFlor = SumCalendarDays(GDDFlor, PlantDayNr,&
                  Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
        if (DFlor /= undef_int) then
            if (InfoCropType == subkind_Grain) then 
                LengthFlor = SumCalendarDays(GDDLengthFlor, (PlantDayNr+DFlor),&
                   Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
            else
                LengthFlor = 0
            end if
            LHImax = SumCalendarDays(GDDHImax, (PlantDayNr+DFlor),&
                       Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
            if ((LengthFlor == undef_int) .or. (LHImax == undef_int)) then
                Succes = .false.
            end if
        else
            LengthFlor = undef_int
            LHImax = undef_int
            Succes = .false.
        end if
    case (subkind_Vegetative, subkind_Forage)
        LHImax = SumCalendarDays(GDDHImax, PlantDayNr,&
                   Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax)
    end select
    if ((D0 == undef_int) .or. (D12 == undef_int) .or. &
        (D123 == undef_int) .or. (DHarvest == undef_int) .or. &
        (DLZmax == undef_int)) then
        Succes = .false.
    end if

    if (Succes) then
        CGC = (real(GDDL12, kind=dp)/real(D12, kind=dp)) * GDDCGC
        call GDDCDCToCDC(PlantDayNr, D123, GDDL123, GDDHarvest,&
               CCx, GDDCDC, Tbase, Tupper, tmp_NoTempFileTMin, tmp_NoTempFileTMax, CDC)
        call DetermineLengthGrowthStages(CCo, CCx, CDC, D0, DHarvest,&
               IsCGCGiven, TheDaysToCCini, &
               ThePlanting, D123, StLength, D12, CGC)
        if ((InfoCropType == subkind_Grain) .or. (InfoCropType == subkind_Tuber)) then
            dHIdt = real(HIndex, kind=dp)/real(LHImax, kind=dp)
        end if
        if ((InfoCropType == subkind_Vegetative) &
            .or. (InfoCropType == subkind_Forage)) then
            if (LHImax > 0) then
                if (LHImax > DHarvest) then
                    dHIdt = real(HIndex, kind=dp)/real(DHarvest, kind=dp)
                else
                    dHIdt = real(HIndex, kind=dp)/real(LHImax, kind=dp)
                end if
                if (dHIdt > 100) then
                    dHIdt = 100 ! 100 is maximum TempdHIdt (See SetdHIdt)
                    LHImax = 0
                end if
            else
                dHIdt = 100 ! 100 is maximum TempdHIdt (See SetdHIdt)
                LHImax = 0
            end if
        end if
    end if
end subroutine AdjustCalendarDays


subroutine AdjustCalendarCrop(FirstCropDay)
    integer(int32), intent(in) :: FirstCropDay

    logical :: succes
    logical :: CGCisGiven
    integer(int32) :: Crop_GDDaysToHIo_temp
    integer(int32) :: Crop_DaysToGermination_temp
    integer(int32) :: Crop_DaysToFullCanopy_temp
    integer(int32) :: Crop_DaysToFlowering_temp
    integer(int32) :: Crop_LengthFlowering_temp
    integer(int32) :: Crop_DaysToSenescence_temp
    integer(int32) :: Crop_DaysToHarvest_temp
    integer(int32) :: Crop_DaysToMaxRooting_temp
    integer(int32) :: Crop_DaysToHIo_temp
    integer(int32), dimension(4) :: Crop_Length_temp
    real(dp) :: Crop_CGC_temp
    real(dp) :: Crop_CDC_temp
    real(dp) :: Crop_dHIdt_temp

    CGCisGiven = .true.

    select case (GetCrop_ModeCycle())
    case (modeCycle_GDDays)
        call SetCrop_GDDaysToFullCanopy(GetCrop_GDDaysToGermination() &
           + roundc(log((0.25_dp*GetCrop_CCx()*GetCrop_CCx()/GetCrop_CCo()) &
               /(GetCrop_CCx()-(0.98_dp*GetCrop_CCx())))/GetCrop_GDDCGC(), &
               mold=int32))
        if (GetCrop_GDDaysToFullCanopy() > GetCrop_GDDaysToHarvest()) then
            call SetCrop_GDDaysToFullCanopy(GetCrop_GDDaysToHarvest())
        end if
        Crop_GDDaysToHIo_temp = GetCrop_GDDaysToHIo()
        Crop_DaysToGermination_temp = GetCrop_DaysToGermination()
        Crop_DaysToFullCanopy_temp = GetCrop_DaysToFullCanopy()
        Crop_DaysToFlowering_temp = GetCrop_DaysToFlowering()
        Crop_LengthFlowering_temp = GetCrop_LengthFlowering()
        Crop_DaysToSenescence_temp = GetCrop_DaysToSenescence()
        Crop_DaysToHarvest_temp = GetCrop_DaysToHarvest()
        Crop_DaysToMaxRooting_temp = GetCrop_DaysToMaxRooting()
        Crop_DaysToHIo_temp = GetCrop_DaysToHIo()
        Crop_Length_temp = GetCrop_Length()
        Crop_CGC_temp = GetCrop_CGC()
        Crop_CDC_temp = GetCrop_CDC()
        Crop_dHIdt_temp = GetCrop_dHIdt()
        call AdjustCalendarDays(FirstCropDay, GetCrop_subkind(), &
          GetCrop_Tbase(), GetCrop_Tupper(), &
          GetSimulParam_Tmin(), GetSimulParam_Tmax(), &
          GetCrop_GDDaysToGermination(), GetCrop_GDDaysToFullCanopy(), &
          GetCrop_GDDaysToFlowering(), GetCrop_GDDLengthFlowering(), &
          GetCrop_GDDaysToSenescence(), GetCrop_GDDaysToHarvest(), &
          GetCrop_GDDaysToMaxRooting(), Crop_GDDaysToHIo_temp, &
          GetCrop_GDDCGC(), GetCrop_GDDCDC(), GetCrop_CCo(), &
          GetCrop_CCx(), CGCisGiven, GetCrop_HI(), &
          GetCrop_DaysToCCini(), GetCrop_Planting(), &
          Crop_DaysToGermination_temp, Crop_DaysToFullCanopy_temp,&
          Crop_DaysToFlowering_temp, Crop_LengthFlowering_temp, &
          Crop_DaysToSenescence_temp, Crop_DaysToHarvest_temp, &
          Crop_DaysToMaxRooting_temp, Crop_DaysToHIo_temp,&
          Crop_Length_temp, Crop_CGC_temp, &
          Crop_CDC_temp, Crop_dHIdt_temp, Succes)
        call SetCrop_GDDaysToHIo(Crop_GDDaysToHIo_temp)
        call SetCrop_DaysToGermination(Crop_DaysToGermination_temp)
        call SetCrop_DaysToFullCanopy(Crop_DaysToFullCanopy_temp)
        call SetCrop_DaysToFlowering(Crop_DaysToFlowering_temp)
        call SetCrop_LengthFlowering(Crop_LengthFlowering_temp)
        call SetCrop_DaysToSenescence(Crop_DaysToSenescence_temp)
        call SetCrop_DaysToHarvest(Crop_DaysToHarvest_temp)
        call SetCrop_DaysToMaxRooting(Crop_DaysToMaxRooting_temp)
        call SetCrop_DaysToHIo(Crop_DaysToHIo_temp)
        call SetCrop_Length(Crop_Length_temp)
        call SetCrop_CGC(Crop_CGC_temp)
        call SetCrop_CDC(Crop_CDC_temp)
        call SetCrop_dHIdt(Crop_dHIdt_temp)
    case default
        Succes = .true.
    end select
    if (.not. Succes) then
        write(*,*) "ERROR AdjustCalendarCrop: looks like we ran out of ideas"
        return
    end if
end subroutine AdjustCalendarCrop


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
        CCi = CCx * (1._dp - 0.05_dp  &
                 * (exp(real(GDDi,kind=dp)*(GDDCDC*3.33_dp)/(CCx+2.29_dp))-1._dp) )
       ! CC at time ti
    end if
    ti = SumCalendarDays(GDDi, (PlantDayNr+D123),&
              Tbase, Tupper, NoTempFileTMin, NoTempFileTMax)
    if (ti > 0) then
        CDC = (((CCx+2.29_dp)/real(ti, kind=dp)) &
                * log(1._dp + ((1._dp-CCi/CCx)/0.05_dp)))/3.33_dp
    else
        CDC = undef_int
    end if
end subroutine GDDCDCToCDC


integer(int32) function RoundedOffGDD(PeriodGDD, PeriodDay,& 
           FirstDayPeriod, TempTbase, TempTupper, TempTmin, TempTmax)
    integer(int32), intent(in) :: PeriodGDD
    integer(int32), intent(in) :: PeriodDay
    integer(int32), intent(in) :: FirstDayPeriod
    real(dp), intent(in) :: TempTbase
    real(dp), intent(in) :: TempTupper
    real(dp), intent(in) :: TempTmin
    real(dp), intent(in) :: TempTmax

    integer(int32) :: DayMatch, PeriodUpdatedGDD
    real(dp) :: TempTmin_t, TempTmax_t

    TempTmin_t = TempTmin
    TempTmax_t = TempTmax

    if (PeriodGDD > 0) then
        DayMatch = SumCalendarDays(PeriodGDD, FirstDayPeriod, &
                     TempTbase, TempTupper, TempTmin_t, TempTmax_t)
        PeriodUpdatedGDD = GrowingDegreeDays(PeriodDay, FirstDayPeriod, &
                     TempTbase, TempTupper, TempTmin_t, TempTmax_t)
        if (PeriodDay == DayMatch) then
            RoundedOffGDD = PeriodGDD
        else
            RoundedOffGDD = PeriodUpdatedGDD
        end if
    else
        RoundedOffGDD = GrowingDegreeDays(PeriodDay, FirstDayPeriod,& 
                     TempTbase, TempTupper, TempTmin_t, TempTmax_t)
    end if
end function RoundedOffGDD


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
        if (roundc(10000._dp*KsPolCS, mold=1_int32) < 10000) then
            ColdStress = .true.
        end if
        ! 3.4 Ks(pollination) heat stress
        KsPolHS = KsTemperature(real(TempTheat+TempRange, kind=dp), &
                     real(TempTheat, kind=dp), Txdayi)
        if (roundc(10000._dp*KsPolHS, mold=1_int32) < 10000) then
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


subroutine CropStressParametersSoilSalinity(CCxRed, CCdistortion, &
             CCo, CCx, CGC, GDDCGC, CropDeterm, L12, LFlor, &
             LengthFlor, L123, GDDL12, GDDLFlor, GDDLengthFlor, &
             GDDL123, TheModeCycle, StressResponse)
    integer(int8), intent(in) :: CCxRed
    integer(int8), intent(in) :: CCdistortion
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    logical, intent(in) :: CropDeterm
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: LengthFlor
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDLFlor
    integer(int32), intent(in) :: GDDLengthFlor
    integer(int32), intent(in) :: GDDL123
    integer(intEnum), intent(in) :: TheModeCycle
    type(rep_EffectStress), intent(inout) :: StressResponse

    real(dp) :: CCToReach, CCxAdj, L12Double, L12SS, &
                CGCadjMax, CGCAdjMin, L12SSmax, CGCadj, CCxFinal, &
                GDDL12Double, GDDCGCadjMax, GDDL12SSmax, GDDCGCAdjMin, GDDCGCadj

    ! initialize
    StressResponse%RedCCX = CCxRed
    StressResponse%RedWP = 0_int8
    L12Double = L12
    L12SSmax = L12
    GDDL12Double = GDDL12

    ! CGC reduction
    CCToReach = 0.98_dp * CCx
    if ((CCo > CCToReach) .or. (CCo >= CCx) .or. (CCxRed == 0)) then
        StressResponse%RedCGC = 0_int8
    else
        StressResponse%RedCGC = undef_int
        ! reference for no salinity stress
        if (TheModeCycle == modeCycle_CalendarDays) then
            L12Double = log((0.25_dp*CCx*CCx/CCo)/(CCx-CCToReach))/CGC
            if (L12Double <= epsilon(1._dp)) then
                StressResponse%RedCGC = 0_int8
            end if
        else
            GDDL12Double = log((0.25_dp*CCx*CCx/CCo)/(CCx-CCToReach))/GDDCGC
            if (GDDL12Double <= epsilon(1._dp)) then
                StressResponse%RedCGC = 0_int8
            end if
        end if
        ! with salinity stress
        CCxAdj = 0.90_dp * CCx * (1._dp - CCxRed/100._dp)
        CCToReach = 0.98_dp * CCxAdj
        if ((StressResponse%RedCGC /= 0) .and. &
            ((CCxAdj-CCToReach) >= 0.0001_dp)) then
            if (TheModeCycle == modeCycle_CalendarDays) then
                CGCadjMax = log((0.25_dp*CCxAdj*CCxAdj/CCo)&
                                /(CCxAdj-CCToReach))/L12Double
                L12SSmax = L12 + (L123 - L12)/2._dp
                if (CropDeterm .and. (L12SSmax > &
                       (LFlor + roundc(LengthFlor/2._dp, mold=1_int32)))) then
                    L12SSmax = LFlor + roundc(LengthFlor/2._dp, mold=1_int32)
                end if
                if (L12SSmax > L12Double) then
                    CGCAdjMin = log((0.25_dp*CCxAdj*CCxAdj/CCo)&
                                    /(CCxAdj-CCToReach))/L12SSmax
                else
                    CGCAdjMin = CGCadjMax
                end if
                if (CCxRed < 10) then ! smooth start required
                    CGCadj = CGCadjMax - (CGCadjMax-CGCAdjMin)&
                     *(exp(CCxRed*log(1.5_dp))/exp(10*log(1.5_dp)))&
                     *(CCdistortion/100._dp)
                else
                    CGCadj = CGCadjMax - (CGCadjMax-CGCAdjMin)&
                                         *(CCdistortion/100._dp)
                end if
                StressResponse%RedCGC = &
                  roundc(100._dp*(CGC-CGCadj)/CGC, mold=1_int8)
            else
                GDDCGCadjMax = log((0.25_dp*CCxAdj*CCxAdj/CCo) &
                                   /(CCxAdj-CCToReach))/GDDL12Double
                GDDL12SSmax = GDDL12 + (GDDL123 - GDDL12)/2._dp
                if (CropDeterm .and. (GDDL12SSmax > &
                      (GDDLFlor + roundc(LengthFlor/2._dp, mold=1_int32)))) then
                    GDDL12SSmax = GDDLFlor + &
                       roundc(GDDLengthFlor/2._dp, mold=1_int32)
                end if
                if (GDDL12SSmax > GDDL12Double) then
                    GDDCGCAdjMin = log((0.25_dp*CCxAdj*CCxAdj/CCo) &
                                       /(CCxAdj-CCToReach))/GDDL12SSmax
                else
                    GDDCGCAdjMin = GDDCGCadjMax
                end if
                if (CCxRed < 10) then ! smooth start required
                    GDDCGCadj = GDDCGCadjMax - (GDDCGCadjMax-GDDCGCAdjMin)*&
                                   (exp(real(CCxRed, kind=dp))/&
                                    exp(10._dp))*(CCdistortion/100._dp)
                else
                    GDDCGCadj = GDDCGCadjMax - &
                      (GDDCGCadjMax-GDDCGCAdjMin)*(CCdistortion/100._dp)
                end if
                StressResponse%RedCGC = &
                      roundc(100._dp*(GDDCGC-GDDCGCadj)/GDDCGC, mold=1_int8)
           end if
        else
            StressResponse%RedCGC = 0_int8
        end if
    end if

    ! Canopy decline
    if (CCxRed == 0) then
        StressResponse%CDecline = 0._dp
    else
        CCxAdj = 0.98_dp*CCx*(1._dp - CCxRed/100._dp)
        L12SS = L12SSmax - (L12SSmax-L12Double) * (CCdistortion/100._dp)
        if ((L123 > L12SS) .and. (CCdistortion > 0)) then
            if (CCxRed < 10) then ! smooth start required
                CCxFinal = CCxAdj - &
                  (exp(CCxRed*log(1.5_dp))/exp(10._dp*log(1.5_dp)))* &
                  (0.5_dp*CCdistortion/100._dp)*(CCxAdj - CCo)
            else
                CCxFinal = CCxAdj - (0.5_dp*CCdistortion/100._dp)*(CCxAdj - CCo)
            end if
            if (CCxFinal < CCo) then
                CCxFinal = CCo
            end if
            StressResponse%CDecline = &
                100._dp*(CCxAdj - CCxFinal)/real(L123 - L12SS, kind=dp)
            if (StressResponse%CDecline > 1) then
                StressResponse%CDecline = 1.0_dp
            end if
            if (StressResponse%CDecline <= epsilon(1._dp)) then
                StressResponse%CDecline = 0.001_dp
            end if
        else
            StressResponse%CDecline = 0.001_dp ! no shift of maturity
        end if
    end if

    ! Stomata closure
    StressResponse%RedKsSto = CCxRed
end subroutine CropStressParametersSoilSalinity


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
        case (datatype_daily) 
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
        case (datatype_decadely)
            call GetDecadeTemperatureDataSet(CropFirstDay, TminDataSet, &
                        TmaxDataSet)
            i = 1
            do while (TminDataSet(i)%DayNr /= CropFirstDay)
                i = i+1
            end do
            Tlow = TminDataSet(i)%Param
            Thigh = TmaxDataSet(i)%Param
        case (datatype_monthly)
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
            case (datatype_daily)  
                if (rc == iostat_end) then
                    rewind(fhandle1)
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
            case (datatype_decadely)
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
            case (datatype_monthly)
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
        if (GetTemperatureRecord_DataType() == datatype_daily) then
           close(fhandle1)
        end if
        close(fhandle2)
    else
        write(*,*) 'ERROR: no valid air temperature file'
        return
        ! fatal error if no air temperature file
    endif
end subroutine TemperatureFileCoveringCropPeriod


subroutine AdjustCropFileParameters(TheCropFileSet, LseasonDays,& 
                TheCropDay1, TheModeCycle, TheTbase, TheTupper,& 
                L123, L1234, GDD123, GDD1234)
    type(rep_CropFileSet), intent(in) :: TheCropFileSet
    integer(int32), intent(in) :: LseasonDays
    integer(int32), intent(in) :: TheCropDay1
    integer(intEnum), intent(in) :: TheModeCycle
    real(dp), intent(in) :: TheTbase
    real(dp), intent(in) :: TheTupper
    integer(int32), intent(inout) :: L123
    integer(int32), intent(inout) :: L1234
    integer(int32), intent(inout) :: GDD123
    integer(int32), intent(inout) :: GDD1234

    real(dp) :: Tmin_tmp, Tmax_tmp

    ! Adjust some crop parameters (CROP.*) as specified by the generated length
    ! season (LseasonDays)
    ! time to maturity
    L1234 = LseasonDays ! days
    if (TheModeCycle == modeCycle_GDDays) then
        Tmin_tmp = GetSimulParam_Tmin()
        Tmax_tmp = GetSimulParam_Tmax()
        GDD1234 = GrowingDegreeDays(LseasonDays, TheCropDay1,&
                       TheTbase, TheTupper, &
                       Tmin_tmp, Tmax_tmp)
    else
        GDD1234 = undef_int
    end if

    ! time to senescence  (reference is given in TheCropFileSet
    if (TheModeCycle == modeCycle_GDDays) then
        GDD123 = GDD1234 - TheCropFileSet%GDDaysFromSenescenceToEnd
        Tmin_tmp = GetSimulParam_Tmin()
        Tmax_tmp = GetSimulParam_Tmax()
        L123 = SumCalendarDays(GDD123, TheCropDay1,&
                    TheTbase, TheTupper,&
                    Tmin_tmp, Tmax_tmp)
    else
        L123 = L1234 - TheCropFileSet%DaysFromSenescenceToEnd
        GDD123 = undef_int
    end if
end subroutine AdjustCropFileParameters


subroutine BTransferPeriod(TheDaysToCCini, TheGDDaysToCCini,&
              L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234,&
              CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, &
              KcDeclAgeing, CCeffectProcent, WPbio, TheCO2,&
              Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, RatDGDD,&
              TheModeCycle, TempAssimPeriod, TempAssimStored,&
              SumBtot, SumBstored)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: WPbio
    real(dp), intent(in) :: TheCO2
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: RatDGDD
    integer(intEnum), intent(in) :: TheModeCycle
    integer(int32), intent(in) :: TempAssimPeriod
    integer(int8), intent(in) :: TempAssimStored
    real(dp), intent(inout) :: SumBtot
    real(dp), intent(inout) :: SumBstored

    real(dp), parameter :: EToStandard = 5._dp

    integer(int32) :: fTemp, rc
    real(dp) :: SumGDDfromDay1, SumGDDforPlot, SumGDD, DayFraction, &
                GDDayFraction, CCinitial, Tndayi, Txdayi, GDDi, CCi, &
                CCxWitheredForB, TpotForB, EpotTotForB
    logical :: GrowthON
    integer(int32) :: GDDTadj, Tadj, DayCC, Dayi, StartStorage

    ! 1. Open Temperature file
    if (GetTemperatureFile() /= '(None)') then
        open(newunit=fTemp, file=trim(GetPathNameSimul()//'TCrop.SIM'), &
             status='old', action='read', iostat=rc)
    end if
     ! 2. initialize
    call SetSimulation_DelayedDays(0) ! required for CalculateETpot
    SumBtot = 0._dp
    SumBstored = 0._dp
    SumGDDforPlot = undef_int
    SumGDD = undef_int
    SumGDDfromDay1 = 0._dp
    GrowthON = .false.
    GDDTadj = undef_int
    DayFraction = undef_int
    GDDayFraction = undef_int
    StartStorage = L1234 - TempAssimPeriod + 1
    CCxWitheredForB = 0._dp

    ! 3. Initialise 1st day
    if (TheDaysToCCini /= 0) then
       ! regrowth which starts on 1st day
        GrowthON = .true.
        if (TheDaysToCCini == undef_int) then
            ! CCx on 1st day
            Tadj = L12 - L0
            if (TheModeCycle == modeCycle_GDDays) then
                GDDTadj = GDDL12 - GDDL0
                SumGDD = GDDL12
            end if
            CCinitial = CCx
        else
            ! CC on 1st day is < CCx
            Tadj = TheDaysToCCini
            DayCC = Tadj + L0
            if (TheModeCycle == modeCycle_GDDays) then
                GDDTadj = TheGDDaysToCCini
                SumGDD = GDDL0 + TheGDDaysToCCini
                SumGDDforPlot = SumGDD
            end if
            CCinitial = CanopyCoverNoStressSF(DayCC, L0, L123, L1234,&
                GDDL0, GDDL123, GDDL1234, CCo, CCx, CGC, CDC,&
                GDDCGC, GDDCDC, SumGDDforPlot, TheModeCycle, 0_int8, 0_int8)
        end if
        ! Time reduction for days between L12 and L123
        DayFraction = (L123-L12) *1._dp/ &
                      real(Tadj + L0 + (L123-L12), kind=dp)
        if (TheModeCycle == modeCycle_GDDays) then
            GDDayFraction = (GDDL123-GDDL12) *1._dp/&
                            real(GDDTadj + GDDL0 + (GDDL123-GDDL12), kind=dp)
        end if
    else
        ! growth starts after germination/recover
        Tadj = 0
        if (TheModeCycle == modeCycle_GDDays) then
            GDDTadj = 0._dp
            SumGDD = 0._dp
        end if
        CCinitial = CCo
    end if

    ! 4. Calculate Biomass
    do Dayi = 1, L1234
        ! 4.1 growing degrees for dayi
        if (GetTemperatureFile() /= '(None)') then
            read(fTemp, *, iostat=rc) Tndayi, Txdayi
            GDDi = DegreesDay(Tbase, Tupper, Tndayi, Txdayi, &
                              GetSimulParam_GDDMethod())
        else
            GDDi = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, &
                              GetSimulParam_GDDMethod())
        end if
        if (TheModeCycle == modeCycle_GDDays) then
            SumGDD = SumGDD + GDDi
            SumGDDfromDay1 = SumGDDfromDay1 + GDDi
        end if

        ! 4.2 green Canopy Cover (CC)
        DayCC = Dayi
        if (GrowthON .eqv. .false.) then
            ! not yet canopy development
            CCi = 0._dp
            if (TheDaysToCCini /= 0) then
                ! regrowth
                CCi = CCinitial
                GrowthON = .true.
            else
                ! sowing or transplanting
                if (TheModeCycle == modeCycle_CalendarDays) then
                    if (Dayi == (L0+1)) then
                        CCi = CCinitial
                        GrowthON = .true.
                    end if
                else
                    if (SumGDD > GDDL0) then
                        CCi = CCinitial
                        GrowthON = .true.
                    end if
                end if
            end if
        else
            if (TheDaysToCCini == 0) then
                DayCC = Dayi
            else
                DayCC = Dayi + Tadj + L0 ! adjusted time scale
                if (DayCC > L1234) then
                    DayCC = L1234 ! special case where L123 > L1234
                end if
                if (DayCC > L12) then
                    if (Dayi <= L123) then
                        DayCC = L12 + roundc(DayFraction *&
                             real(Dayi+Tadj+L0 - L12, kind=dp),mold=1) ! slow down
                    else
                        DayCC = Dayi ! switch time scale
                    end if
                end if
            end if
            if (TheModeCycle == modeCycle_GDDays) then
                if (TheGDDaysToCCini == 0) then
                    SumGDDforPlot = SumGDDfromDay1
                else
                    SumGDDforPlot = SumGDD
                    if (SumGDDforPlot > GDDL1234) then
                        SumGDDforPlot = GDDL1234 ! special case where L123 > L1234
                    end if
                    if (SumGDDforPlot > GDDL12) then
                        if (SumGDDfromDay1 <= GDDL123) then
                            SumGDDforPlot = GDDL12 + real(GDDayFraction * &
                              real(SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12,&
                                   kind=dp)) ! slow down
                        else
                            SumGDDforPlot = SumGDDfromDay1 ! switch time scale
                        end if
                    end if
                    CCi = CCiNoWaterStressSF(DayCC, L0, L12, L123, L1234,&
                        GDDL0, GDDL12, GDDL123, GDDL1234,&
                        CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, SumGDDforPlot,&
                        RatDGDD, 0_int8, 0_int8, 0._dp, TheModeCycle)
                end if
                if (CCi > CCxWitheredForB) then
                     CCxWitheredForB = CCi
                end if

                ! 4.3 potential transpiration (TpotForB)
                if (CCi > 0.0001_dp) then
                    ! 5.3 potential transpiration of total canopy cover
                    call CalculateETpot(DayCC, L0, L12, L123, L1234, (0), CCi,&
                         EToStandard, KcTop, KcDeclAgeing,&
                         CCx, CCxWitheredForB, CCeffectProcent, TheCO2, GDDi, &
                         GDtranspLow, TpotForB, EpotTotForB)
                else
                    TpotForB = 0._dp
                end if

                ! 4.4 Biomass (B)
                if (Dayi >= StartStorage) then
                    SumBtot = SumBtot +  WPbio * (TpotForB/EToStandard)
                    SumBstored = SumBstored + WPbio*(TpotForB/EToStandard)*&
                            (0.01_dp*TempAssimStored)*&
                            ((Dayi-StartStorage+1._dp)/&
                             real(TempAssimPeriod, kind=dp))
               end if
           end if

           ! 5. Close Temperature file
           if (GetTemperatureFile() /= '(None)') then
               close(fTemp)
           end if
       end if
    end do
end subroutine BTransferPeriod

real(dp) function Bnormalized(TheDaysToCCini, TheGDDaysToCCini,&
            L0, L12, L12SF, L123, L1234, LFlor, &
            GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, &
            WPyield, DaysYieldFormation, tSwitch, CCo, CCx, &
            CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, &
            CCeffectProcent, WPbio, TheCO2, Tbase, Tupper, &
            TDayMin, TDayMax, GDtranspLow, RatDGDD, SumKcTop, &
            StressInPercent, StrResRedCGC, StrResRedCCx, StrResRedWP, &
            StrResRedKsSto, WeedStress, DeltaWeedStress, StrResCDecline, &
            ShapeFweed, TheModeCycle, FertilityStressOn, TestRecord)
     integer(int32), intent(in) :: TheDaysToCCini
     integer(int32), intent(in) :: TheGDDaysToCCini
     integer(int32), intent(in) :: L0
     integer(int32), intent(in) :: L12
     integer(int32), intent(in) :: L12SF
     integer(int32), intent(in) :: L123
     integer(int32), intent(in) :: L1234
     integer(int32), intent(in) :: LFlor
     integer(int32), intent(in) :: GDDL0
     integer(int32), intent(in) :: GDDL12
     integer(int32), intent(in) :: GDDL12SF
     integer(int32), intent(in) :: GDDL123
     integer(int32), intent(in) :: GDDL1234
     integer(int32), intent(in) :: WPyield
     integer(int32), intent(in) :: DaysYieldFormation
     integer(int32), intent(in) :: tSwitch
     real(dp), intent(in) :: CCo
     real(dp), intent(in) :: CCx
     real(dp), intent(in) :: CGC
     real(dp), intent(in) :: GDDCGC
     real(dp), intent(in) :: CDC
     real(dp), intent(in) :: GDDCDC
     real(dp), intent(in) :: KcTop
     real(dp), intent(in) :: KcDeclAgeing
     real(dp), intent(in) :: CCeffectProcent
     real(dp), intent(in) :: WPbio
     real(dp), intent(in) :: TheCO2
     real(dp), intent(in) :: Tbase
     real(dp), intent(in) :: Tupper
     real(dp), intent(in) :: TDayMin
     real(dp), intent(in) :: TDayMax
     real(dp), intent(in) :: GDtranspLow
     real(dp), intent(in) :: RatDGDD
     real(dp), intent(in) :: SumKcTop
     integer(int8), intent(in) :: StressInPercent
     integer(int8), intent(in) :: StrResRedCGC
     integer(int8), intent(in) :: StrResRedCCx
     integer(int8), intent(in) :: StrResRedWP
     integer(int8), intent(in) :: StrResRedKsSto
     integer(int8), intent(in) :: WeedStress
     integer(int32), intent(in) :: DeltaWeedStress
     real(dp), intent(in) :: StrResCDecline
     real(dp), intent(in) :: ShapeFweed
     integer(intEnum), intent(in) :: TheModeCycle
     logical, intent(in) :: FertilityStressOn
     logical, intent(in) :: TestRecord

     real(dp), parameter :: EToStandard = 5._dp
     integer(int32), parameter :: k = 2

     integer(int32) ::  fTemp, fOUT, rc
     real(dp) :: SumGDD, Tndayi, Txdayi, GDDi, CCi,&
                 CCxWitheredForB, TpotForB, EpotTotForB, SumKCi,&
                 fSwitch, WPi, SumBnor, SumKcTopSF, fCCx
     integer(int32) :: Dayi, DayCC, Tadj, GDDTadj
     real(dp) :: CCoadj, CCxadj, CDCadj, GDDCDCadj, CCw, CCtotStar, CCwStar
     real(dp) :: SumGDDfromDay1, SumGDDforPlot, CCinitial,&
                 DayFraction, GDDayFraction, fWeed, WeedCorrection
     logical :: GrowthON
     integer(int32) :: DeltaWeedStress_local

     ! 1. Adjustment for weed infestation
     if (WeedStress > 0) then
         if (StressInPercent > 0) then ! soil fertility stress
             fWeed = 1._dp  ! no expansion of canopy cover possible
         else
             fWeed = CCmultiplierWeed(WeedStress, CCx, ShapeFweed)
         end if
         CCoadj = CCo*fWeed
         CCxadj = CCx*fWeed
         CDCadj = CDC*(fWeed*CCx + 2.29_dp)/(CCx + 2.29_dp)
         GDDCDCadj = GDDCDC*(fWeed*CCx + 2.29_dp)/(CCx + 2.29_dp)
     else
         CCoadj = CCo
         CCxadj = CCx
         CDCadj = CDC
         GDDCDCadj = GDDCDC
     end if

     ! TEST
     if (TestRecord .eqv. .true.) then
         open(newunit=fOUT, file=trim(GetPathNameSimul()//'TestBio.SIM'), &
              action='write', status='replace')
     end if

     ! 2. Open Temperature file
     if (GetTemperatureFile() /= '(None)') then
         open(newunit=fTemp, file=trim(GetPathNameSimul()//'TCrop.SIM'), &
                      status='old', action='read', iostat=rc)
     end if

     ! 3. Initialize
     SumKcTopSF = (1._dp - real(StressInPercent, kind=dp)/100._dp) * SumKcTop
     !! only required for soil fertility stress

     ! test
     if (TestRecord .eqv. .true.) then
        write(fOUT, '(f10.1)') SumKcTopSF
     end if

     call SetSimulation_DelayedDays(0) ! required for CalculateETpot
     SumKci = 0._dp
     SumBnor = 0._dp
     SumGDDforPlot = undef_int
     SumGDD = undef_int
     SumGDDfromDay1 = 0._dp
     GrowthON = .false.
     GDDTadj = undef_int
     DayFraction = undef_int
     GDDayFraction = undef_int
     CCxWitheredForB = 0._dp

     ! 4. Initialise 1st day
     if (TheDaysToCCini /= 0) then
         ! regrowth which starts on 1st day
         GrowthON = .true.
         if (TheDaysToCCini == undef_int) then
             ! CCx on 1st day
             Tadj = L12 - L0
             if (TheModeCycle == modeCycle_GDDays) then
                 GDDTadj = GDDL12 - GDDL0
                 SumGDD = GDDL12
             end if
             CCinitial = CCxadj * (1._dp-StrResRedCCX/100._dp)
         else
         ! CC on 1st day is < CCx
             Tadj = TheDaysToCCini
             DayCC = Tadj + L0
             if (TheModeCycle == modeCycle_GDDays) then
                 GDDTadj = TheGDDaysToCCini
                 SumGDD = GDDL0 + TheGDDaysToCCini
                 SumGDDforPlot = SumGDD
             end if
             CCinitial = CanopyCoverNoStressSF(DayCC, L0, L123, L1234,&
                 GDDL0, GDDL123, GDDL1234, CCoadj, CCxadj, CGC, CDCadj,&
                 GDDCGC, GDDCDCadj, SumGDDforPlot, TheModeCycle, &
                 StrResRedCGC, StrResRedCCX)
         end if
         ! Time reduction for days between L12 and L123
         DayFraction = (L123-L12) * 1._dp/&
                       real(Tadj + L0 + (L123-L12) ,kind=dp)
         if (TheModeCycle == modeCycle_GDDays) then
             GDDayFraction = (GDDL123-GDDL12) * 1._dp/&
                             (GDDTadj + GDDL0 + (GDDL123-GDDL12))
         end if
     else
         ! growth starts after germination/recover
         Tadj = 0
         if (TheModeCycle == modeCycle_GDDays) then
             GDDTadj = 0
             SumGDD = 0._dp
         end if
         CCinitial = CCoadj
     end if

     ! 5. Calculate Bnormalized
     do Dayi = 1, L1234
         ! 5.1 growing degrees for dayi
         if (GetTemperatureFile() /= '(None)') then
             read(fTemp, *, iostat=rc) Tndayi, Txdayi
             GDDi = DegreesDay(Tbase, Tupper, Tndayi, Txdayi,&
                               GetSimulParam_GDDMethod())
         else
             GDDi = DegreesDay(Tbase, Tupper, TDayMin, TDayMax,&
                               GetSimulParam_GDDMethod())
         end if
         if (TheModeCycle == modeCycle_GDDays) then
             SumGDD = SumGDD + GDDi
             SumGDDfromDay1 = SumGDDfromDay1 + GDDi
         end if

         ! 5.2 green Canopy Cover (CC)
         DayCC = Dayi
         if (GrowthON .eqv. .false.) then
             ! not yet canopy development
             CCi = 0._dp
             if (TheDaysToCCini /= 0) then
                 ! regrowth
                 CCi = CCinitial
                 GrowthON = .true.
             else
                 ! sowing or transplanting
                 if (TheModeCycle == modeCycle_CalendarDays) then
                     if (Dayi == (L0+1)) then
                         CCi = CCinitial
                         GrowthON = .true.
                     end if
                 else
                     if (SumGDD > GDDL0) then
                         CCi = CCinitial
                         GrowthON = .true.
                     end if
                 end if
             end if
         else
             if (TheDaysToCCini == 0) then
                 DayCC = Dayi
             else
                 DayCC = Dayi + Tadj + L0 ! adjusted time scale
                 if (DayCC > L1234) then
                     DayCC = L1234 ! special case where L123 > L1234
                 end if
                 if (DayCC > L12) then
                     if (Dayi <= L123) then
                          DayCC = L12 + roundc(DayFraction * &
                                     (Dayi+Tadj+L0 - L12), mold=1) ! slow down
                     else
                         DayCC = Dayi ! switch time scale
                     end if
                 end if
             end if

             if (TheModeCycle == modeCycle_GDDays) then
                 if (TheGDDaysToCCini == 0) then
                     SumGDDforPlot = SumGDDfromDay1
                 else
                     SumGDDforPlot = SumGDD
                     if (SumGDDforPlot > GDDL1234) then
                         SumGDDforPlot = GDDL1234 
                         ! special case where L123 > L1234
                     end if
                     if (SumGDDforPlot > GDDL12) then
                         if (SumGDDfromDay1 <= GDDL123) then
                             SumGDDforPlot = GDDL12 + roundc(GDDayFraction * &
                                 (SumGDDfromDay1+GDDTadj+GDDL0 - &
                                 GDDL12),mold=1) ! slow down
                         else
                             SumGDDforPlot = SumGDDfromDay1 ! switch time scale
                         end if
                     end if
                 end if
             endif
             CCi = CCiNoWaterStressSF(DayCC, L0, L12SF, L123, L1234,&
                         GDDL0, GDDL12SF, GDDL123, GDDL1234,&
                         CCoadj, CCxadj, CGC, GDDCGC, CDCadj, GDDCDCadj, &
                         SumGDDforPlot, RatDGDD,&
                         StrResRedCGC, StrResRedCCX, StrResCDecline,&
                         TheModeCycle)
         end if

         if (CCi > CCxWitheredForB) then
             CCxWitheredForB = CCi
         end if
         if (DayCC >= L12SF) then
             CCxWitheredForB = CCxadj*(1._dp-StrResRedCCX/100._dp)
         end if
         CCw = CCi

         if (CCi > 0.0001_dp) then
             ! 5.3 potential transpiration of total canopy cover (crop and weed)
             call CalculateETpot(DayCC, L0, L12, L123, L1234, (0), CCi, &
                            EToStandard, KcTop, KcDeclAgeing,&
                            CCxadj, CCxWitheredForB, CCeffectProcent, TheCO2,&
                            GDDi, GDtranspLow, TpotForB, EpotTotForB)

             ! 5.4 Sum of Kc (only required for soil fertility stress)
             SumKci = SumKci + (TpotForB/EToStandard)

             ! 5.5 potential transpiration of crop canopy cover (without weed)
             if (WeedStress > 0) then
                 ! green canopy cover of the crop (CCw) in weed-infested field
                 ! (CCi is CC of crop and weeds)
                 fCCx = 1.0_dp ! only for non perennials (no self-thinning)
                 if (DeltaWeedStress /= 0) then
                     DeltaWeedStress_local = DeltaWeedStress
                     WeedCorrection = GetWeedRC(DayCC, SumGDDforPlot, fCCx,&
                            WeedStress, GetManagement_WeedAdj(),&
                            DeltaWeedStress_local, L12SF, L123, &
                            GDDL12SF, GDDL123, TheModeCycle)
                 else
                     WeedCorrection = WeedStress
                 end if
                 CCw = CCi * (1._dp - WeedCorrection/100._dp)
                 ! correction for micro-advection
                 CCtotStar = 1.72_dp*CCi - 1._dp*(CCi*CCi) + &
                                 0.30_dp*(CCi*CCi*CCi)
                 if (CCtotStar < 0._dp) then
                     CCtotStar = 0._dp
                 end if
                 if (CCtotStar > 1._dp) then
                     CCtotStar = 1._dp
                 end if
                 if (CCw > 0.0001_dp) then
                     CCwStar = CCw + (CCtotStar - CCi)
                 else
                     CCwStar = 0._dp
                 end if
                 ! crop transpiration in weed-infested field
                 if (CCtotStar <= 0.0001_dp) then
                     TpotForB = 0._dp
                 else
                     TpotForB = TpotForB * (CCwStar/CCtotStar)
                 end if
             end if
         else
             TpotForB = 0._dp
         end if

         ! 5.6 biomass water productivity (WP)
         WPi = WPbio ! vegetative stage
         ! 5.6a. vegetative versus yield formation stage
         if (((GetCrop_subkind() == subkind_Tuber) &
             .or. (GetCrop_subkind() == subkind_Grain)) .and.&
             (WPyield < 100) .and. (Dayi > LFlor)) then
             ! yield formation stage
             fSwitch = 1._dp
             if ((DaysYieldFormation > 0) .and. (tSwitch > 0)) then
                 fSwitch = (Dayi-LFlor) * 1._dp/real(tSwitch, kind=dp)
                 if (fSwitch > 1) then
                     fSwitch = 1._dp
                 end if
             end if
             WPi = WPi * (1._dp - (1._dp - WPyield/100._dp)*fSwitch)
         end if

         ! 5.7 Biomass (B)
         if (FertilityStressOn) then
             ! 5.7a - reduction for soil fertiltiy
             if ((StrResRedWP > 0) .and. (SumKci > 0._dp) &
                 .and. (SumKcTopSF > epsilon(1.0))) then
                 if (SumKci < SumKcTopSF) then
                     if (SumKci > 0) then
                         WPi = WPi * (1._dp - (StrResRedWP/100._dp) *&
                                 exp(k*log(SumKci/SumKcTopSF)))
                     end if
                 else
                     WPi = WPi * (1._dp - StrResRedWP/100._dp)
                 end if
             end if
             ! 5.7b - Biomass (B)
             SumBnor = SumBnor +  WPi * (TpotForB/EToStandard)
         else
             SumBnor = SumBnor +  WPi * (1._dp - StrResRedKsSto/100._dp) *&
                           (TpotForB/EToStandard) ! for salinity stress
         end if

         ! test
         if (TestRecord .eqv. .true.) then
             write(fOUT,'(i10, 2f10.1, 3f10.2, f10.1)') &
                     Dayi, (100._dp*CCi), (100._dp*CCw), &
                     WeedCorrection, TpotForB, WPi, SumKci
         end if
     enddo

     ! 4. Close Temperature file
     if (GetTemperatureFile() /= '(None)') then
         close(fTemp)
     end if

     if (TestRecord .eqv. .true.) then
         close(fOUT)
     end if

     ! 5. Export
     Bnormalized = SumBnor
end function Bnormalized


real(dp) function BiomassRatio(TempDaysToCCini, TempGDDaysToCCini,&
           TempCCo, TempCGC, TempCCx, TempCDC, TempGDDCGC, &
           TempGDDCDC, TempdHIdt, TempL0, TempL12, L12SF,&
           TempL123, TempHarvest, TempFlower, TempGDDL0, &
           GDDL12SF, TempGDDL12, TempGDDL123, TempGDDHarvest,&
           TempHI, TempWPy, TempKc, TempKcDecline, TempCCeffect,&
           TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow,&
           TempWP, ShapeFweed, TempModeCycle, SFInfo, SFInfoStress,&
           WeedStress, DeltaWeedStress, DeterminantCropType, FertilityStressOn)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    real(dp), intent(in) :: TempCCo
    real(dp), intent(in) :: TempCGC
    real(dp), intent(in) :: TempCCx
    real(dp), intent(in) :: TempCDC
    real(dp), intent(in) :: TempGDDCGC
    real(dp), intent(in) :: TempGDDCDC
    real(dp), intent(in) :: TempdHIdt
    integer(int32), intent(in) :: TempL0
    integer(int32), intent(in) :: TempL12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: TempL123
    integer(int32), intent(in) :: TempHarvest
    integer(int32), intent(in) :: TempFlower
    integer(int32), intent(in) :: TempGDDL0
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: TempGDDL12
    integer(int32), intent(in) :: TempGDDL123
    integer(int32), intent(in) :: TempGDDHarvest
    integer(int32), intent(in) :: TempHI
    integer(int32), intent(in) :: TempWPy
    real(dp), intent(in) :: TempKc
    real(dp), intent(in) :: TempKcDecline
    real(dp), intent(in) :: TempCCeffect
    real(dp), intent(in) :: TempTbase
    real(dp), intent(in) :: TempTupper
    real(dp), intent(in) :: TempTmin
    real(dp), intent(in) :: TempTmax
    real(dp), intent(in) :: TempGDtranspLow
    real(dp), intent(in) :: TempWP
    real(dp), intent(in) :: ShapeFweed
    integer(intEnum), intent(in) :: TempModeCycle
    type(rep_EffectStress), intent(in) :: SFInfo
    integer(int8), intent(in) :: SFInfoStress
    integer(int8), intent(in) :: WeedStress
    integer(int32), intent(in) :: DeltaWeedStress
    logical, intent(in) :: DeterminantCropType
    logical, intent(in) :: FertilityStressOn

    integer(int32), parameter :: k = 2
    real(dp), parameter :: CO2iLocal = 369.41_dp

    real(dp) :: SumKcTop, HIGC, HIGClinear
    real(dp) :: RatDGDD, SumBPot, SumBSF
    integer(int32) :: tSwitch, DaysYieldFormation

    ! 1. Initialize
    ! 1 - a. Maximum sum Kc
    SumKcTop = SeasonalSumOfKcPot(TempDaysToCCini, TempGDDaysToCCini,&
        TempL0, TempL12, TempL123, TempHarvest, TempGDDL0, TempGDDL12,&
        TempGDDL123, TempGDDHarvest, TempCCo, TempCCx, TempCGC,&
        TempGDDCGC, TempCDC, TempGDDCDC, TempKc, TempKcDecline, TempCCeffect,&
        TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow, CO2iLocal,&
        TempModeCycle)
    ! 1 - b. Prepare for growing degree days
    RatDGDD = 1._dp
    if ((TempModeCycle == modeCycle_GDDays) .and. (SFInfoStress > 0_int8) &
        .and. (GDDL12SF < TempGDDL123)) then
        RatDGDD = (TempL123-L12SF)/real(TempGDDL123-GDDL12SF, kind=dp)
    end if
    ! 1 - c. Get PercentLagPhase (for estimate WPi during yield formation)
    DaysYieldFormation = undef_int
    if ((GetCrop_subkind() == subkind_Tuber) .or. &
        (GetCrop_subkind() == subkind_Grain)) then
        ! DaysToFlowering corresponds with Tuberformation
        DaysYieldFormation = roundc(TempHI/TempdHIdt, mold=1)
        if (DeterminantCropType) then
            HIGC = HarvestIndexGrowthCoefficient(real(TempHI,kind=dp), TempdHIdt)
            call GetDaySwitchToLinear(TempHI, TempdHIdt, &
                     HIGC, tSwitch, HIGClinear)
        else
            tSwitch = roundc(DaysYieldFormation/3._dp, mold=1)
        end if
    end if

    ! 2. potential biomass - no soil fertiltiy stress - no weed stress
    SumBPot = Bnormalized(TempDaysToCCini, TempGDDaysToCCini,&
        TempL0, TempL12, TempL12, TempL123, TempHarvest, TempFlower,&
        TempGDDL0, TempGDDL12, TempGDDL12, TempGDDL123, TempGDDHarvest,&
        TempWPy, DaysYieldFormation, tSwitch,&
        TempCCo, TempCCx, TempCGC, TempGDDCGC, TempCDC, TempGDDCDC,&
        TempKc, TempKcDecline, TempCCeffect, TempWP, CO2iLocal,&
        TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow, 1._dp,&
        SumKcTop, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8,&
        0, 0._dp, -0.01_dp, &
        TempModeCycle, FertilityStressOn, .false.)

    ! 3. potential biomass - soil fertiltiy stress and weed stress
    SumBSF = Bnormalized(TempDaysToCCini, TempGDDaysToCCini,&
        TempL0, TempL12, L12SF, TempL123, TempHarvest, TempFlower,&
        TempGDDL0, TempGDDL12, GDDL12SF, TempGDDL123, TempGDDHarvest, &
        TempWPy, DaysYieldFormation, tSwitch,&
        TempCCo, TempCCx, TempCGC, TempGDDCGC, TempCDC, TempGDDCDC,&
        TempKc, TempKcDecline, TempCCeffect, TempWP, CO2iLocal,&
        TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow, RatDGDD,&
        SumKcTop, SFInfoStress, SFInfo%RedCGC, SFInfo%RedCCX, SFInfo%RedWP,&
        SFInfo%RedKsSto, WeedStress, DeltaWeedStress, &
        SFInfo%CDecline, ShapeFweed, TempModeCycle, &
        FertilityStressOn, .false.)

    BiomassRatio = SumBSF/SumBPot
end function BiomassRatio


subroutine StressBiomassRelationship(TheDaysToCCini, TheGDDaysToCCini,&
            L0, L12, L123, L1234, LFlor, LengthFlor, GDDL0, GDDL12,&
            GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC, GDDCGC,&
            CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent,&
            Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, WPveg, RatedHIdt,&
            CO2Given, CropDNr1, CropDeterm, CropSResp, TheCropType,&
            TheModeCycle, b0, b1, b2, &
            BM10, BM20, BM30, BM40, BM50, BM60, BM70)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: LengthFlor
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    integer(int32), intent(in) :: WPyield
    integer(int32), intent(in) :: RefHI
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: WPveg
    real(dp), intent(in) :: RatedHIdt
    real(dp), intent(in) :: CO2Given
    integer(int32), intent(in) :: CropDNr1
    logical, intent(in) :: CropDeterm
    type(rep_Shapes), intent(in) :: CropSResp
    integer(intEnum), intent(in) :: TheCropType
    integer(intEnum), intent(in) :: TheModeCycle
    real(dp), intent(inout) :: b0
    real(dp), intent(inout) :: b1
    real(dp), intent(inout) :: b2
    real(dp), intent(inout) :: BM10
    real(dp), intent(inout) :: BM20
    real(dp), intent(inout) :: BM30
    real(dp), intent(inout) :: BM40
    real(dp), intent(inout) :: BM50
    real(dp), intent(inout) :: BM60
    real(dp), intent(inout) :: BM70

    real(dp), parameter :: EToStandard = 5._dp
    integer(int32), parameter :: k = 2

    type StressIndexes
        integer(int8) :: StressProc
            !! Undocumented
        real(dp) :: BioMProc
            !! Undocumented
        real(dp) :: BioMSquare
            !! Undocumented
    end type StressIndexes

    type(StressIndexes), dimension(8) :: StressMatrix
    integer(int8) :: Si
    integer(int32) :: L12SF, GDDL12SF
    type(rep_EffectStress) :: StressResponse
    real(dp) :: RatDGDD, BNor, BNor100, Yavg, X1avg, X2avg,&
                y, x1, x2, x1y, x2y, x1Sq, x2Sq, x1x2, &
                SUMx1y, SUMx2y, SUMx1Sq, SUMx2Sq, SUMx1x2
    integer(int8) :: SiPr
    real(dp) :: SumKcTop, HIGC, HIGClinear
    integer(int32) :: DaysYieldFormation, tSwitch
    real(dp) :: TDayMax_temp, TDayMin_temp

    ! 1. initialize
    call SetSimulation_DelayedDays(0) ! required for CalculateETpot
    L12SF = L12 ! to calculate SumKcTop (no stress)
    GDDL12SF = GDDL12 ! to calculate SumKcTop (no stress)
    ! Maximum sum Kc (no stress)
    SumKcTop = SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini,&
        L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234,&
        CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing,&
        CCeffectProcent, Tbase, Tupper, TDayMin, TDayMax, &
        GDtranspLow, CO2Given, TheModeCycle)

    ! Get PercentLagPhase (for estimate WPi during yield formation)
    if ((TheCropType == subkind_Tuber) .or. (TheCropType == subkind_grain)) then
        ! DaysToFlowering corresponds with Tuberformation
        DaysYieldFormation = roundc(RefHI/RatedHIdt, mold=1)
        if (CropDeterm) then
            HIGC = HarvestIndexGrowthCoefficient(real(RefHI, kind=dp), RatedHIdt)
            call GetDaySwitchToLinear(RefHI, RatedHIdt, HIGC, tSwitch,&
                  HIGClinear)
        else
            tSwitch = roundc(DaysYieldFormation/3._dp, mold=1)
        end if
    end if

    ! 2. Biomass production for various stress levels
    do Si = 1, 8
        ! various stress levels
        ! stress effect
        SiPr = 10*(Si-1)
        StressMatrix(Si)%StressProc = SiPr
        call CropStressParametersSoilFertility(CropSResp, SiPr, StressResponse)
        ! adjusted length of Max canopy cover
        RatDGDD = 1
        if ((StressResponse%RedCCX == 0) .and. &
            (StressResponse%RedCGC == 0))then
            L12SF = L12
            GDDL12SF = GDDL12
        else
            call TimeToMaxCanopySF(CCo, CGC, CCx, L0, L12, L123, LFlor,&
                   LengthFlor, CropDeterm, L12SF, StressResponse%RedCGC,&
                   StressResponse%RedCCX, SiPr)
            if (TheModeCycle == modeCycle_GDDays) then
                TDayMin_temp = TDayMin
                TDayMax_temp = TDayMax
                GDDL12SF = GrowingDegreeDays(L12SF, CropDNr1, Tbase, Tupper,&
                                 TDayMin_temp, TDayMax_temp)
            end if
            if ((TheModeCycle == modeCycle_GDDays) .and. (GDDL12SF < GDDL123)) then
                RatDGDD = (L123-L12SF)*1._dp/real(GDDL123-GDDL12SF, kind=dp)
            end if
        end if
        ! biomass production
        BNor = Bnormalized(TheDaysToCCini, TheGDDaysToCCini,&
                L0, L12, L12SF, L123, L1234, LFlor,&
                GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, WPyield, &
                DaysYieldFormation, tSwitch, CCo, CCx, CGC, GDDCGC, CDC,&
                GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, WPveg, CO2Given,&
                Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, RatDGDD,&
                SumKcTop, SiPr, StressResponse%RedCGC, StressResponse%RedCCX,&
                StressResponse%RedWP, StressResponse%RedKsSto, 0_int8, 0 ,&
                StressResponse%CDecline, -0.01_dp, TheModeCycle, .true.,&
                .false.)
        if (Si == 1) then
            BNor100 = BNor
            StressMatrix(1)%BioMProc = 100._dp
        else
            if (BNor100 > 0.00001_dp) then
                StressMatrix(Si)%BioMProc = 100._dp * BNor/BNor100
            else
                StressMatrix(Si)%BioMProc = 100._dp
            end if
        end if
        StressMatrix(Si)%BioMSquare =&
             StressMatrix(Si)%BioMProc *&
             StressMatrix(Si)%BioMProc
        ! end stress level
    end do

    ! 5. Stress - Biomass relationship
    Yavg = 0._dp
    X1avg = 0._dp
    X2avg = 0._dp
    do Si = 1, 8
        ! various stress levels
        Yavg = Yavg + StressMatrix(Si)%StressProc
        X1avg = X1avg + StressMatrix(Si)%BioMProc
        X2avg = X2avg + StressMatrix(Si)%BioMSquare
    end do
    Yavg  = Yavg/8._dp
    X1avg = X1avg/8._dp
    X2avg = X2avg/8._dp
    SUMx1y  = 0._dp
    SUMx2y  = 0._dp
    SUMx1Sq = 0._dp
    SUMx2Sq = 0._dp
    SUMx1x2 = 0._dp
    do Si = 1, 8
        ! various stress levels
        y     = StressMatrix(Si)%StressProc - Yavg
        x1    = StressMatrix(Si)%BioMProc - X1avg
        x2    = StressMatrix(Si)%BioMSquare - X2avg
        x1y   = x1 * y
        x2y   = x2 * y
        x1Sq  = x1 * x1
        x2Sq  = x2 * x2
        x1x2  = x1 * x2
        SUMx1y  = SUMx1y + x1y
        SUMx2y  = SUMx2y + x2y
        SUMx1Sq = SUMx1Sq + x1Sq
        SUMx2Sq = SUMx2Sq + x2Sq
        SUMx1x2 = SUMx1x2 + x1x2
    end do

    if (abs(roundc(SUMx1x2*1000._dp, mold=1)) /= 0) then
        b2 = (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/&
             (SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2)
        b1 = (SUMx1y - b2 * SUMx1x2)/SUMx1Sq
        b0 = Yavg - b1*X1avg - b2*X2avg

        BM10 =  StressMatrix(2)%BioMProc
        BM20 =  StressMatrix(3)%BioMProc
        BM30 =  StressMatrix(4)%BioMProc
        BM40 =  StressMatrix(5)%BioMProc
        BM50 =  StressMatrix(6)%BioMProc
        BM60 =  StressMatrix(7)%BioMProc
        BM70 =  StressMatrix(8)%BioMProc
    else
        b2 = real(undef_int, kind=dp)
        b1 = real(undef_int, kind=dp)
        b0 = real(undef_int, kind=dp)
    end if
end subroutine StressBiomassRelationship

subroutine CCxSaltStressRelationship(TheDaysToCCini, TheGDDaysToCCini,&
       L0, L12, L123, L1234, LFlor, LengthFlor, GDDFlor, GDDLengthFlor,&
       GDDL0, GDDL12, GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC,&
       GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase,&
       Tupper, TDayMin, TDayMax, GDbioLow, WPveg, RatedHIdt, CO2Given,&
       CropDNr1, CropDeterm, TheCropType, TheModeCycle, TheCCsaltDistortion,&
       Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, Salt10, Salt20, Salt30,&
       Salt40, Salt50, Salt60, Salt70, Salt80, Salt90)
    integer(int32), intent(in) :: TheDaysToCCini
    integer(int32), intent(in) :: TheGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: LFlor
    integer(int32), intent(in) :: LengthFlor
    integer(int32), intent(in) :: GDDFlor
    integer(int32), intent(in) :: GDDLengthFlor
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    integer(int32), intent(in) :: WPyield
    integer(int32), intent(in) :: RefHI
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: KcTop
    real(dp), intent(in) :: KcDeclAgeing
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDbioLow
    real(dp), intent(in) :: WPveg
    real(dp), intent(in) :: RatedHIdt
    real(dp), intent(in) :: CO2Given
    integer(int32), intent(in) :: CropDNr1
    logical, intent(in) :: CropDeterm
    integer(intEnum), intent(in) :: TheCropType
    integer(intEnum), intent(in) :: TheModeCycle
    integer(int8), intent(in) :: TheCCsaltDistortion
    real(dp), intent(inout) :: Coeffb0Salt
    real(dp), intent(inout) :: Coeffb1Salt
    real(dp), intent(inout) :: Coeffb2Salt
    real(dp), intent(inout) :: Salt10
    real(dp), intent(inout) :: Salt20
    real(dp), intent(inout) :: Salt30
    real(dp), intent(inout) :: Salt40
    real(dp), intent(inout) :: Salt50
    real(dp), intent(inout) :: Salt60
    real(dp), intent(inout) :: Salt70
    real(dp), intent(inout) :: Salt80
    real(dp), intent(inout) :: Salt90

    type StressIndexes
        integer(int8) :: CCxReduction
            !! Undocumented
        real(dp) :: SaltProc
            !! Undocumented
        real(dp) :: SaltSquare
            !! Undocumented
    end type StressIndexes

    integer(int32) :: L12SS, GDDL12SS, DaysYieldFormation, tSwitch
    real(dp) :: SumKcTop, HIGC, HIGClinear, CCToReach
    integer(int8) :: Si, SiPr
    type(StressIndexes), dimension(10) :: StressMatrix
    type(rep_EffectStress) :: StressResponse
    real(dp) :: RatDGDD, BNor, BNor100, BioMProc
    real(dp) :: Yavg, X1avg, X2avg, SUMx1y, SUMx2y, SUMx1Sq, &
         SUMx2Sq, SUMx1x2, y, x1, x2, x1y, x2y, x1Sq, x2Sq, x1x2
    real(dp) :: TDayMax_temp, TDayMin_temp

    ! 1. initialize
    call SetSimulation_DelayedDays(0) ! required for CalculateETpot
    GDDL12SS = GDDL12 ! to calculate SumKcTop (no stress)
    BNor100 = real(undef_int, kind=dp)
    ! Maximum sum Kc (no stress)
    SumKcTop = SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini,&
        L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234,&
        CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, &
        CCeffectProcent,Tbase, Tupper, TDayMin, TDayMax, GDbioLow, &
        CO2Given, TheModeCycle)
    ! Get PercentLagPhase (for estimate WPi during yield formation)
    if ((TheCropType == subkind_Tuber) .or. (TheCropType == subkind_grain)) then
        ! DaysToFlowering corresponds with Tuberformation
        DaysYieldFormation = roundc(RefHI/RatedHIdt, mold=1)
        if (CropDeterm) then
            HIGC = HarvestIndexGrowthCoefficient(real(RefHI, kind=dp), RatedHIdt)
            call GetDaySwitchToLinear(RefHI, RatedHIdt, &
                    HIGC, tSwitch, HIGClinear)
        else
            tSwitch = roundc(DaysYieldFormation/3._dp, mold=1)
        end if
    end if

    ! 2. Biomass production (or Salt stress) for various CCx reductions
    do Si = 1, 10
        ! various CCx reduction
        ! CCx reduction
        SiPr = 10*(Si-1)
        StressMatrix(Si)%CCxReduction = SiPr
        ! adjustment CC
        call CropStressParametersSoilSalinity(SiPr, TheCCsaltDistortion, &
            CCo, CCx, CGC, GDDCGC, CropDeterm, L12, LFlor, LengthFlor, L123,&
            GDDL12, GDDFlor, GDDLengthFlor, GDDL123, TheModeCycle,&
            StressResponse)
        ! adjusted length of Max canopy cover
        RatDGDD = 1
        if ((StressResponse%RedCCX == 0) .and.&
            (StressResponse%RedCGC == 0)) then
            L12SS = L12
            GDDL12SS = GDDL12
        else
            CCToReach = 0.98_dp*(1._dp-StressResponse%RedCCX/100._dp)*CCx
            L12SS = DaysToReachCCwithGivenCGC(CCToReach, CCo, &
                 (1._dp-StressResponse%RedCCX/100._dp)*CCx,&
                 CGC*(1._dp-StressResponse%RedCGC/100._dp), L0)
            if (TheModeCycle == modeCycle_GDDays) then
                TDayMax_temp = TDayMax
                TDayMin_temp = TDayMin
                GDDL12SS = GrowingDegreeDays(L12SS, CropDNr1, Tbase, &
                           Tupper, TDayMin_temp, TDayMax_temp)
            end if
            if ((TheModeCycle == modeCycle_GDDays) .and.&
                (GDDL12SS < GDDL123)) then
                RatDGDD = (L123-L12SS)*1._dp/(GDDL123-GDDL12SS)
            end if
        end if

        ! biomass production
        BNor = Bnormalized(TheDaysToCCini, TheGDDaysToCCini,&
                L0, L12, L12SS, L123, L1234, LFlor,&
                GDDL0, GDDL12, GDDL12SS, GDDL123, GDDL1234,&
                WPyield, DaysYieldFormation, tSwitch,&
                CCo, CCx, CGC, GDDCGC, CDC, GDDCDC,&
                KcTop, KcDeclAgeing, CCeffectProcent, WPveg, CO2Given,&
                Tbase, Tupper, TDayMin, TDayMax, GDbioLow, RatDGDD, SumKcTop,&
                SiPr, StressResponse%RedCGC, StressResponse%RedCCX,&
                StressResponse%RedWP, StressResponse%RedKsSto, &
                0_int8, 0, StressResponse%CDecline, -0.01_dp,&
                TheModeCycle, .false., .false.)
        if (Si == 1) then
            BNor100 = BNor
            BioMProc = 100._dp
            StressMatrix(1)%SaltProc = 0._dp
        else
            if (BNor100 > 0.00001_dp) then
                BioMProc = 100._dp * BNor/BNor100
                StressMatrix(Si)%SaltProc = 100._dp - BioMProc
            else
                StressMatrix(Si)%SaltProc = 0._dp
            end if
        end if
        StressMatrix(Si)%SaltSquare = &
             StressMatrix(Si)%SaltProc *&
             StressMatrix(Si)%SaltProc
        ! end stress level
    end do

    ! 3. CCx - Salt stress relationship
    Yavg = 0._dp
    X1avg = 0._dp
    X2avg = 0._dp
    do Si = 1, 10
        ! various CCx reduction
        Yavg = Yavg + StressMatrix(Si)%CCxReduction
        X1avg = X1avg + StressMatrix(Si)%SaltProc
        X2avg = X2avg + StressMatrix(Si)%SaltSquare
    end do
    Yavg  = Yavg/10._dp
    X1avg = X1avg/10._dp
    X2avg = X2avg/10._dp
    SUMx1y  = 0._dp
    SUMx2y  = 0._dp
    SUMx1Sq = 0._dp
    SUMx2Sq = 0._dp
    SUMx1x2 = 0._dp
    do Si = 1, 10
        ! various CCx reduction
        y     = StressMatrix(Si)%CCxReduction - Yavg
        x1    = StressMatrix(Si)%SaltProc - X1avg
        x2    = StressMatrix(Si)%SaltSquare - X2avg
        x1y   = x1 * y
        x2y   = x2 * y
        x1Sq  = x1 * x1
        x2Sq  = x2 * x2
        x1x2  = x1 * x2
        SUMx1y  = SUMx1y + x1y
        SUMx2y  = SUMx2y + x2y
        SUMx1Sq = SUMx1Sq + x1Sq
        SUMx2Sq = SUMx2Sq + x2Sq
        SUMx1x2 = SUMx1x2 + x1x2
    end do

    if (abs(roundc(SUMx1x2*1000._dp, mold=1)) /= 0) then
        Coeffb2Salt = (SUMx1y - (SUMx2y * SUMx1Sq)/SUMx1x2)/&
                      (SUMx1x2 - (SUMx1Sq * SUMx2Sq)/SUMx1x2)
        Coeffb1Salt = (SUMx1y - Coeffb2Salt * SUMx1x2)/SUMx1Sq
        Coeffb0Salt = Yavg - Coeffb1Salt*X1avg - Coeffb2Salt*X2avg

        Salt10 =  StressMatrix(2)%SaltProc
        Salt20 =  StressMatrix(3)%SaltProc
        Salt30 =  StressMatrix(4)%SaltProc
        Salt40 =  StressMatrix(5)%SaltProc
        Salt50 =  StressMatrix(5)%SaltProc
        Salt60 =  StressMatrix(7)%SaltProc
        Salt70 =  StressMatrix(8)%SaltProc
        Salt80 =  StressMatrix(9)%SaltProc
        Salt90 =  StressMatrix(10)%SaltProc
    else
        Coeffb2Salt = real(undef_int, kind=dp)
        Coeffb1Salt = real(undef_int, kind=dp)
        Coeffb0Salt = real(undef_int, kind=dp)
    end if
end subroutine CCxSaltStressRelationship

end module ac_tempprocessing
