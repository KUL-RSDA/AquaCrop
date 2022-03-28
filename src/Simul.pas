unit Simul;

interface

uses Global, interface_global, Math, TempProcessing, interface_tempprocessing, interface_simul;


PROCEDURE BUDGET_module(dayi : LongInt;
                        TargetTimeVal, TargetDepthVal, VirtualTimeCC, SumInterval, DayLastCut,NrDayGrow,
                        Tadj, GDDTadj : Integer;
                        GDDayi,CGCref,GDDCGCref,CO2i,CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
                        Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTotSaltPrev,
                        DayFraction,GDDayFraction,FracAssim : double;
                        StressSFadjNEW : ShortInt;
                        StorageON,MobilizationON : BOOLEAN;
                        VAR StressLeaf,StressSenescence : double;
                        VAR TimeSenescence : double;
                        VAR NoMoreCrop,CGCadjustmentAfterCutting : BOOLEAN;
                        VAR TESTVAL : double);


implementation



PROCEDURE BUDGET_module(dayi : LongInt;
                        TargetTimeVal, TargetDepthVal, VirtualTimeCC, SumInterval, DayLastCut,NrDayGrow,
                        Tadj, GDDTadj : Integer;
                        GDDayi,CGCref,GDDCGCref,CO2i,CCxTotal,CCoTotal,CDCTotal,GDDCDCTotal,SumGDDadjCC,
                        Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,StressTotSaltPrev,
                        DayFraction,GDDayFraction,FracAssim : double;
                        StressSFadjNEW : ShortInt;
                        StorageON,MobilizationON : BOOLEAN;
                        VAR StressLeaf,StressSenescence : double;
                        VAR TimeSenescence : double;
                        VAR NoMoreCrop,CGCadjustmentAfterCutting : BOOLEAN;
                        VAR TESTVAL : double);

VAR  control : rep_control;
     InfiltratedRain,
     InfiltratedIrrigation,
     InfiltratedStorage,
     EpotTot,SubDrain : double;
     DAP : INTEGER;
     ECInfilt : double ; //EC of the infiltrated water (surface storage)
     WaterTableInProfile : BOOLEAN;
     HorizontalWaterFlow,HorizontalSaltFlow : double;
     EffectStress_temp : rep_EffectStress;
     SWCtopSoilConsidered_temp : boolean;
     EvapWCsurf_temp, CRwater_temp, Tpot_temp, Epot_temp : double;
     Comp_temp : rep_Comp;
     Crop_pActStom_temp : double;
     CRsalt_temp, ECdrain_temp, Tact_temp : double;



PROCEDURE ExtractWaterFromEvapLayer(EvapToLose : double;
                                    Zact : double;
                                    Stg1 : Boolean);
VAR EvapLost,Wx,Wairdry,AvailableW,Ztot,fracZ,StillToExtract : double;
    compi : ShortInt;

BEGIN
EvapLost := 0;
compi := 0;
Ztot := 0;
REPEAT
  compi := compi + 1;
  IF ((Ztot + GetCompartment_Thickness(compi)) > Zact)
     THEN fracZ := (Zact-Ztot)/GetCompartment_Thickness(compi)
     ELSE fracZ := 1;
  Wairdry := 10 * GetSoilLayer_i(GetCompartment_Layer(compi)).WP/2 * GetCompartment_Thickness(compi)
             * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
  Wx := 1000 * GetCompartment_Theta(compi) * GetCompartment_Thickness(compi)
        * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
  AvailableW := (Wx-Wairdry)*fracZ;
  StillToExtract := (EvapToLose-EvapLost);
  IF (AvailableW > 0) THEN
     BEGIN
     IF (AvailableW > StillToExtract)
        THEN BEGIN
             SetEact(GetEact() + StillToExtract);
             EvapLost := EvapLost + StillToExtract;
             Wx := Wx - StillToExtract;
             END
        ELSE BEGIN
             SetEact(GetEact() + AvailableW);
             EvapLost := EvapLost + AvailableW;
             Wx := Wx - AvailableW;
             END;
     SetCompartment_Theta(compi, Wx/
        (1000*GetCompartment_Thickness(compi)*(1-GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100)));
     END;
  Ztot := Ztot + fracZ * (GetCompartment_Thickness(compi));
UNTIL ((Compi >= GetNrCompartments())
       OR (Abs(StillToExtract) < 0.0000001)
       OR (Ztot >= 0.999999*Zact));
IF Stg1 THEN
   BEGIN
   SetSimulation_EvapWCsurf(GetSimulation_EvapWCsurf() - EvapLost);
   IF (Abs(EvapToLose-EvapLost) > 0.0001) // not enough water left in the compartment to store WCsurf
         THEN SetSimulation_EvapWCsurf(0);
   END;
END; (* ExtractWaterFromEvapLayer *)




PROCEDURE CalculateSoilEvaporationStage1;
VAR Eremaining : double;
    Stg1 : BOOLEAN;
BEGIN
Stg1 := true;
Eremaining := GetEpot() - GetEact();
IF (GetSimulation_EvapWCsurf() > Eremaining)
   THEN ExtractWaterFromEvapLayer(Eremaining,EvapZmin,Stg1)
   ELSE ExtractWaterFromEvapLayer(GetSimulation_EvapWCsurf(),EvapZmin,Stg1);
IF (GetSimulation_EvapWCsurf() <0.0000001) THEN PrepareStage2;
END; (* CalculateSoilEvaporationStage1 *)



PROCEDURE CalculateSoilEvaporationStage2;
CONST NrOfStepsInDay = 20;
      FractionWtoExpandZ = 0.4;
TYPE repArrayD = Array[2..12] of double;
     repArraySI = Array[2..12] of ShortInt;
VAR AtTheta : rep_WhichTheta;
    Wupper, Wlower, Wact, Eremaining, Wrel, Kr, Elost,
    MaxSaltExDepth, SX, Zi, SaltDisplaced, UL, DeltaX  : double;
    i, compi, SCell1, SCellEnd  : ShortInt;
    Stg1, BoolCell : BOOLEAN;
    ThetaIniEvap : repArrayD;
    SCellIniEvap : repArraySI;

    PROCEDURE GetLimitsEvapLayer(xProc : double;
                                 VAR Wupper,Wlower : double);
    VAR AtTheta : rep_WhichTheta;
        WSAT,WFC : double;
    BEGIN
    AtTheta := AtSat;
    WSAT := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    AtTheta := AtFC;
    WFC := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    Wupper := (xProc/100) * (WSAT - (WFC-GetSoil().REW)) + (WFC-GetSoil().REW);
    AtTheta := AtWP;
    Wlower := WCEvapLayer(GetSimulation_EvapZ(),AtTheta)/2;
    END; (* GetLimitsEvapLayer *)


    FUNCTION SaltTransportFactor(theta : double) : double;
    VAR x : double ;
    BEGIN
    IF (theta <= GetSoilLayer_i(1).WP/200)
       THEN SaltTransportFactor := 0
       ELSE BEGIN
            x := (theta*100 - GetSoilLayer_i(1).WP/2)/(GetSoilLayer_i(1).SAT - GetSoilLayer_i(1).WP/2);
            SaltTransportFactor := EXP(x*LN(10)+LN(x/10));
            END;
    END; (* SaltTransportFactor *)



BEGIN (* CalculateSoilEvaporationStage2 *)

// Step 1. Conditions before soil evaporation
compi := 1;
MaxSaltExDepth := GetCompartment_Thickness(1);
WHILE ((MaxSaltExDepth < GetSimulParam_EvapZmax()) AND (compi < GetNrCompartments())) DO
  BEGIN
  compi := compi + 1;
  ThetaIniEvap[compi] := GetCompartment_Theta(compi);
  SCellIniEvap[compi] := ActiveCells(GetCompartment_i(compi));
  MaxSaltExDepth := MaxSaltExDepth + GetCompartment_Thickness(compi);
  END;

// Step 2. Soil evaporation
Stg1 := false;
Eremaining := GetEpot()-GetEact();
GetLimitsEvapLayer(GetSimulation_EvapStartStg2(),Wupper,Wlower);
FOR i := 1 TO NrOfStepsInDay DO
    BEGIN
    AtTheta := AtAct;
    Wact := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
    Wrel := (Wact-Wlower)/(Wupper-Wlower);
    IF (GetSimulParam_EvapZmax() > EvapZmin) THEN
       WHILE ((Wrel < (FractionWtoExpandZ*(GetSimulParam_EvapZmax()-(100*GetSimulation_EvapZ()))/(GetSimulParam_EvapZmax()-EvapZmin)))
          AND (GetSimulation_EvapZ() < GetSimulParam_EvapZmax()/100)) DO
          BEGIN
          SetSimulation_EvapZ(GetSimulation_EvapZ() + 0.001); // add 1 mm
          GetLimitsEvapLayer(GetSimulation_EvapStartStg2(),Wupper,Wlower);
          AtTheta := AtAct;
          Wact := WCEvapLayer(GetSimulation_EvapZ(),AtTheta);
          Wrel := (Wact-Wlower)/(Wupper-Wlower);
          END;
    Kr := SoilEvaporationReductionCoefficient(Wrel,GetSimulParam_EvapDeclineFactor());
    IF (Abs(getETo() - 5) > 0.01) THEN // correction for evaporative demand
       BEGIN
       // adjustment of Kr (not considered yet)
       END;
    Elost := Kr * (Eremaining/NrOfStepsInDay);
    ExtractWaterFromEvapLayer(Elost,GetSimulation_EvapZ(),Stg1);
    END;

// Step 3. Upward salt transport
SX := SaltTransportFactor(Getcompartment_Theta(1));
IF (SX > 0.01) THEN
   BEGIN
   SCell1 := ActiveCells(GetCompartment_i(1));
   compi := 2;
   Zi := GetCompartment_Thickness(1) + GetCompartment_Thickness(2);
   WHILE ((ROUND(Zi*100) <= ROUND(MaxSaltExDepth*100)) AND (compi <= GetNrCompartments())
          AND (ROUND(ThetaIniEvap[compi]*100000) <> ROUND(GetCompartment_theta(compi)*100000))) DO
      BEGIN  // move salt to compartment 1
      SCellEnd := ActiveCells(GetCompartment_i(compi));
      BoolCell := false;
      UL := GetSoilLayer_i(GetCompartment_Layer(compi)).UL;
      DeltaX := GetSoilLayer_i(GetCompartment_Layer(compi)).Dx;
      REPEAT
      IF (SCellEnd < SCellIniEvap[compi])
         THEN BEGIN
              SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi]);
              SetCompartment_Salt(compi, SCellIniEvap[compi], GetCompartment_Salt(compi, SCellIniEvap[compi]) - SaltDisplaced);
              SCellIniEvap[compi] := SCellIniEvap[compi] - 1;
              ThetaIniEvap[compi] := DeltaX * SCellIniEvap[compi];
              END
         ELSE BEGIN
              BoolCell := true;
              IF (SCellEnd = GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1)
                 THEN SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi])
                      * (ThetaIniEvap[compi] - GetCompartment_theta(compi))/(ThetaIniEvap[compi]-UL)
                 ELSE SaltDisplaced := SX * GetCompartment_Salt(compi, SCellIniEvap[compi])
                      * (ThetaIniEvap[compi] - GetCompartment_theta(compi))/(ThetaIniEvap[compi]-(DeltaX*(SCellEnd-1)));
              SetCompartment_Salt(compi, SCellIniEvap[compi], GetCompartment_Salt(compi, SCellIniEvap[compi]) - SaltDisplaced);
              END;
      SetCompartment_Salt(1, SCell1, GetCompartment_Salt(1, SCell1) + SaltDisplaced);
      UNTIL BoolCell;
      compi := compi + 1;
      IF (compi <= GetNrCompartments()) THEN Zi := Zi + GetCompartment_Thickness(compi);
      END;
   END;

END; (* CalculateSoilEvaporationStage2 *)



PROCEDURE calculate_transpiration(Tpot : double);

VAR WtoExtract, theta_critical, alfa, sinkMM : double;
    compi, layeri, pre_layer : INTEGER;
    DeltaWC, InetThreshold : double;
    TpotMAX, RedFact, RedFactECsw : double;
    Wrel,WrelSalt,pStomatLLAct : double;
    CompiECe,CompiECsw,CompiECswFC : double;
    SWCtopSoilConsidered_temp : boolean;
    Comp_temp : rep_Comp;
    Compi_temp : CompartmentIndividual;



PROCEDURE calculate_theta_critical(layeri : INTEGER;
                                   VAR theta_critical : double);

VAR theta_TAW : double;

BEGIN
theta_TAW := GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100;
theta_critical := GetSoilLayer_i(layeri).FC/100 - theta_TAW * GetCrop().pActStom;
END; (*calculate_theta_critical*)



PROCEDURE calculate_rootfraction_compartment(RootingDepth : double;
                                             VAR Compartment : rep_Comp);

VAR       frac_value, cumdepth : double;
          compi,i : INTEGER;

BEGIN
cumdepth := 0;
compi := 0;
REPEAT
  compi := compi + 1;
  cumdepth := cumdepth + Compartment[compi].Thickness;
  IF (cumdepth <= RootingDepth)
     THEN Compartment[compi].WFactor := 1
     ELSE BEGIN
          frac_value := RootingDepth - (cumdepth - Compartment[compi].Thickness);
          IF (frac_value > 0)
             THEN Compartment[compi].WFactor := frac_value/Compartment[compi].Thickness
             ELSE Compartment[compi].WFactor := 0;
          END;
UNTIL (cumdepth >= RootingDepth) OR (compi = GetNrCompartments());
FOR i := compi+1 TO GetNrCompartments() DO Compartment[i].WFactor := 0;
END; (* calculate_rootfraction_compartment *)



PROCEDURE calculate_sink_values(Tpot,RootingDepth : double;
                                VAR Compartment : rep_Comp;
                                Crop : rep_crop);

VAR       sink_value, StopComp, SbotComp,
          cumdepth : double;
          compi, i : INTEGER;

BEGIN
IF (GetIrriMode() = Inet)
   THEN BEGIN
        sink_value := (GetCrop().SmaxTop + GetCrop().SmaxBot)/2;
        for compi := 1 to GetNrCompartments() DO Compartment[compi].Smax := sink_value;
        END
   ELSE BEGIN
        cumdepth := 0;
        compi := 0;
        SbotComp := GetCrop().SmaxTop;
        REPEAT
           compi := compi + 1;
           StopComp := SbotComp;
           cumdepth := cumdepth + Compartment[compi].Thickness;
           IF (cumdepth <= RootingDepth)
              THEN SbotComp := GetCrop().SmaxBot * GetSimulation_SCor() + (GetCrop().SmaxTop - GetCrop().SmaxBot*GetSimulation_SCor())
                                      * (RootingDepth - cumdepth)/RootingDepth
              ELSE SbotComp := GetCrop().SmaxBot*GetSimulation_SCor();
           Compartment[compi].Smax := ((StopComp + SbotComp)/2);
           IF (Compartment[compi].Smax > 0.06) THEN Compartment[compi].Smax := 0.06;
        UNTIL (cumdepth >= RootingDepth) OR (compi = GetNrCompartments());
        FOR i := (compi + 1) TO GetNrCompartments() DO Compartment[i].Smax := 0;
        END;
END; (* calculate_sink_values *)



PROCEDURE Correction_Anaeroby(VAR Comp : CompartmentIndividual;
                              VAR alfa : double);
VAR alfaAN : double;
    ini : INTEGER;
BEGIN
IF ((GetDaySubmerged() >= GetSimulParam_DelayLowOxygen()) AND (GetCrop().AnaeroPoint > 0))
   THEN alfaAN := 0
   ELSE IF (Comp.theta > (GetSoilLayer_i(Comp.Layer).SAT - GetCrop().AnaeroPoint)/100)
           THEN BEGIN
                Comp.DayAnaero := Comp.DayAnaero + 1;
                IF (Comp.DayAnaero >= GetSimulParam_DelayLowOxygen())
                   THEN BEGIN
                        ini := 0;
                        Comp.DayAnaero := GetSimulParam_DelayLowOxygen();
                        END
                   ELSE ini := 1;
                alfaAN := (GetSoilLayer_i(Comp.Layer).SAT/100 - Comp.theta)/(GetCrop().AnaeroPoint/100);
                IF (alfaAN < 0) THEN alfaAN := 0;
                IF (GetSimulParam_DelayLowOxygen() > 1)
                   THEN alfaAN := (ini+(Comp.DayAnaero-1)*alfaAN)/(ini+Comp.DayAnaero-1);
                END
           ELSE BEGIN
                alfaAN := 1;
                Comp.DayAnaero := 0;
                END;
IF (alfa > alfaAN) THEN alfa := alfaAN;
END; (* Correction_Anaeroby *)




PROCEDURE DetermineRootZoneAnaeroConditions(Wsat,Wact,AnaeVol,Zr : double;
                                            VAR RedFact : double);
VAR SATVol,ACTVol : double;
BEGIN
RedFact := 1;
IF ((AnaeVol > 0) AND (Zr > 0))
   THEN BEGIN
        SATVol := Wsat/(10*Zr);
        ACTVol := Wact/(10*Zr);
        IF (ACTVol > SATVol) THEN ACTVol := SATVol;
        IF (ActVol > (SatVol-AnaeVol))
           THEN BEGIN
                SetSimulation_DayAnaero(GetSimulation_DayAnaero() + 1);
                IF (GetSimulation_DayAnaero() > GetSimulParam_DelayLowOxygen()) THEN SetSimulation_DayAnaero(GetSimulParam_DelayLowOxygen());
                RedFact := 1 - (1-((SATVol - ACTVol)/AnaeVol))* (GetSimulation_DayAnaero()/GetSimulParam_DelayLowOxygen());
                END
           ELSE SetSimulation_DayAnaero(0);
        END
   ELSE SetSimulation_DayAnaero(0);
END; (* DetermineRootZoneAnaeroConditions *)



BEGIN (* calculate_transpiration *)
SetTact(0.0);

IF (Tpot > 0) THEN
   BEGIN
   // 1. maximum transpiration in actual root zone
   IF (GetIrriMode() = Inet)
      THEN BEGIN
           // salinity stress not considered
           TpotMAX := Tpot;
           END
      ELSE BEGIN // IrriMode = NOT Inet
           // 1.a effect of water stress and salinity stress
           SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
           DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
           SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);

           // --- 1. Effect of water stress and ECe (total rootzone)
           WrelSalt := (GetRootZoneWC().FC-GetRootZoneWC().Actual)/(GetRootZoneWC().FC-GetRootZoneWC().WP);

           // --- 2. Effect of water stress
           pStomatLLAct := 1;
           IF (GetSimulation_SWCtopSoilConsidered() = true)
              THEN BEGIN // top soil is relative wetter than total root zone
                   IF (GetRootZoneWC().ZtopAct < (0.999 * GetRootZoneWC().ZtopThresh))
                      THEN BEGIN
                           Wrel := (GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopAct)/(GetRootZoneWC().ZtopFC - GetRootZoneWC().ZtopWP);
                           RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,(0.0)); // where (0.0) is linear
                           END
                      ELSE RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                   END
              ELSE BEGIN // total root zone
                   IF (GetRootZoneWC().Actual < (0.999 * GetRootZoneWC().Thresh))
                      THEN BEGIN
                           // THE 3 LINES BELOW GIVE THE CORRECT WAY
                              //Wrel := (RootZoneWC.FC-RootZoneWC.Actual)/(RootZoneWC.FC-RootZoneWC.WP);
                              //pStomatLLAct := 1;
                              //RedFact := KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,GetCrop().KsShapeFactorStomata);
                           // THIS LINE IS THE OLD WAY SINCE RELEASE (version 3.0)
                              //RedFact := 1 - (RootZoneWC.Thresh - RootZoneWC.Actual)/(RootZoneWC.Thresh - RootZoneWC.WP);
                              //IF (RedFact < 0) THEN RedFact := 0;
                           // These lines are the new way following the old procedure (but adjusted for soil salinity)
                           Wrel := (GetRootZoneWC().FC-GetRootZoneWC().Actual)/(GetRootZoneWC().FC-GetRootZoneWC().WP);
                           RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,(0.0)); // where (0.0) is linear
                           END
                      ELSE RedFact := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                   END;

           IF (RedFact < 0) THEN RedFact := 0;
           IF (RedFact > 1) THEN RedFact := 1;

           // --- 3. Extra effect of ECsw (salt in total root zone is considered)
           IF GetSimulation_SalinityConsidered()
              THEN RedFactECsw := AdjustedKsStoToECsw(GetCrop().ECemin,GetCrop().ECemax,GetCrop().ResponseECsw,
                             GetRootZoneSalt().ECe,GetRootZoneSalt().ECsw,GetRootZoneSalt().ECswFC,
                             WrelSalt,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,RedFact)
              ELSE RedFactECsw := RedFact;

           // --- 4. Conclusion (adjustment of TpotMAX considering Water and Salt stress)
           TpotMAX := RedFactECsw * Tpot;

           // 1.b anaerobic conditions in root zone (total root zone is considered)
           DetermineRootZoneAnaeroConditions(GetRootZoneWC().SAT,GetRootZoneWC().Actual,GetCrop().AnaeroPoint,GetRootingDepth(),RedFact);
           TpotMAX := RedFact * TpotMax;
           END;

   // 2. extraction of TpotMax out of the compartments
   // 2.a initial settings
   Comp_temp := GetCompartment();
   calculate_rootfraction_compartment(GetRootingDepth(),Comp_temp);
   calculate_sink_values(TpotMAX,GetRootingDepth(),Comp_temp,GetCrop());
   SetCompartment(Comp_temp);
   compi := 0;
   pre_layer := 0;
   REPEAT
     compi := compi + 1;
     layeri := GetCompartment_Layer(compi);
     IF (layeri > pre_layer) THEN
        BEGIN
        calculate_theta_critical(layeri,theta_critical);
        pre_layer := layeri;
        END;
     // 2.b calculate alfa
     IF (GetIrriMode() = Inet)
        THEN alfa := 1
        ELSE BEGIN
             // effect of water stress and ECe
             IF (GetCompartment_theta(compi) >= (theta_critical))
                THEN alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100)
                ELSE IF (GetCompartment_theta(compi) > (GetSoilLayer_i(layeri).WP/100))
                        THEN BEGIN
                             IF (theta_critical > GetSoilLayer_i(layeri).WP/100)
                                THEN BEGIN
                                     Wrel := (GetSoilLayer_i(layeri).FC/100 - GetCompartment_theta(compi))
                                             /(GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100);
                                     pStomatLLAct := 1;
                                     alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100) * KsAny(Wrel,GetCrop().pActStom,pStomatLLAct,GetCrop().KsShapeFactorStomata);
                                     END
                                ELSE alfa := (1 - GetSimulation_EffectStress_RedKsSto()/100);
                             END
                        ELSE alfa := 0;
             // extra effect of ECsw
             IF GetSimulation_SalinityConsidered()
                THEN BEGIN
                     WrelSalt := (GetSoilLayer_i(layeri).FC/100 - GetCompartment_theta(compi))
                               /(GetSoilLayer_i(layeri).FC/100 - GetSoilLayer_i(layeri).WP/100);
                     CompiECe := ECeComp(GetCompartment_i(compi));
                     CompiECsw := ECswComp(GetCompartment_i(compi),(false));
                     CompiECswFC := ECswComp(GetCompartment_i(compi),(true));
                     RedFactECsw := AdjustedKsStoToECsw(GetCrop().ECemin,GetCrop().ECemax,GetCrop().ResponseECsw,
                              CompiECe,CompiECsw,CompiECswFC,
                              WrelSalt,Coeffb0Salt,Coeffb1Salt,Coeffb2Salt,alfa);
                     END
                ELSE RedFactECsw := alfa;
             alfa := RedFactECsw;
             END;
     IF (GetCrop().AnaeroPoint > 0) THEN BEGIN
                                    Compi_temp := GetCompartment_i(compi);
                                    Correction_Anaeroby(Compi_temp,alfa);
                                    SetCompartment_i(compi, Compi_temp);
                                    END;
     // 2.c extract water
     //sink := alfa * Compartment[compi].WFactor * Compartment[compi].Smax;
     sinkMM := 1000 * (alfa * GetCompartment_WFactor(compi) * GetCompartment_Smax(compi)) * GetCompartment_Thickness(compi);
     //theta_to_extract := ((TpotMAX-GetTact())/1000)/Compartment[compi].Thickness;
     WtoExtract := TpotMAX-GetTact();
     //IF (theta_to_extract < sink) THEN sink := theta_to_extract;
     IF (WtoExtract < sinkMM) THEN sinkMM := WtoExtract;
     //Compartment[compi].theta := Compartment[compi].theta - sink;
     SetCompartment_theta(compi, GetCompartment_theta(compi)
          - sinkMM/(1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100)));
     //theta_to_extract := theta_to_extract - sink;
     WtoExtract := WtoExtract - sinkMM;
     //Tact := Tact + sink * 1000 * Compartment[compi].Thickness;
     SetTact(GetTact() + sinkMM);
   //UNTIL ((theta_to_extract <= 0) OR (compi = Nrcompartments));
   UNTIL ((WtoExtract <= 0) OR (compi = GetNrCompartments()));

   // 3. add net irrigation water requirement
   IF (GetIrriMode() = Inet) THEN
     BEGIN // total root zone is considered
     SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
     DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
     SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
     InetThreshold := GetRootZoneWC().FC - GetSimulParam_PercRAW()/100*(GetRootZoneWC().FC - GetRootZoneWC().Thresh);
     IF (GetRootZoneWC().Actual < InetThreshold) THEN
        BEGIN
        pre_layer := 0;
        FOR compi := 1 TO GetNrCompartments() DO
           BEGIN
           layeri := GetCompartment_Layer(compi);
           IF (layeri > pre_layer) THEN
              BEGIN
              calculate_theta_critical(layeri,theta_critical);
              InetThreshold := GetSoilLayer_i(layeri).FC/100 - GetSimulParam_PercRAW()/100*(GetSoilLayer_i(layeri).FC/100 - theta_critical);
              pre_layer := layeri;
              END;
           //DeltaWC := Compartment[compi].WFactor * (InetThreshold - Compartment[compi].Theta)*1000*Compartment[compi].Thickness;
           DeltaWC := GetCompartment_WFactor(compi) * (InetThreshold - GetCompartment_Theta(compi))
                      *1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100);
           //Compartment[compi].Theta := Compartment[compi].Theta + DeltaWC/(1000*Compartment[compi].Thickness);
           SetCompartment_Theta(compi, GetCompartment_theta(compi) + DeltaWC
                                       /(1000*GetCompartment_Thickness(compi)*(1 - GetSoilLayer_i(layeri).GravelVol/100)));
           SetIrrigation(GetIrrigation() + DeltaWC);
           END;
        END;
     END; // (Irrimode = Inet)
   END; // (Tpot > 0)
END; (* calculate_transpiration *)



PROCEDURE surface_transpiration;
VAR Textra, Part : double;
    compi : INTEGER;
    KsReduction,SaltSurface : double;
    Tact_temp : double;
BEGIN
SetDaySubmerged(GetDaySubmerged() + 1);
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    SetCompartment_DayAnaero(compi, GetCompartment_DayAnaero(compi) + 1);
    IF (GetCompartment_DayAnaero(compi) > GetSimulParam_DelayLowOxygen())
       THEN SetCompartment_DayAnaero(compi, GetSimulParam_DelayLowOxygen());
    END;
IF (GetCrop().AnaeroPoint > 0) THEN Part := (1-GetDaySubmerged()/GetSimulParam_DelayLowOxygen())
                          ELSE Part := 1;
KsReduction := KsSalinity(GetSimulation_SalinityConsidered(),GetCrop().ECemin,GetCrop().ECemax,GetECstorage(),(0.0));
SaltSurface := GetSurfaceStorage()*GetECstorage()*Equiv;
IF (GetSurfaceStorage() > KsReduction*Part*GetTpot())
   THEN BEGIN
        SetSurfaceStorage(GetSurfaceStorage() - KsReduction*Part*GetTpot());
        SetTact(KsReduction*Part*GetTpot());
        //NEW
        SetECstorage(SaltSurface/(GetSurfaceStorage()*Equiv)); //salinisation of surface storage layer
        END
   ELSE BEGIN
        SetTact(GetSurfaceStorage() -0.1);
        SetSurfaceStorage(0.1); // zero give error in already updated salt balance
        END;
IF (GetTact() < KsReduction*Part*GetTpot()) THEN
   BEGIN
   Tact_temp := GetTact(); (*Protect Tact from changes in the next routine*)
   calculate_transpiration((KsReduction*Part*GetTpot()-GetTact()));
   SetTact(Tact_temp + GetTact());
   END;
END; (* surface_transpiration *)


PROCEDURE FeedbackCC;
BEGIN
IF (((GetCCiActual() - GetCCiPrev()) > 0.005)  // canopy is still developing
    AND (GetTact() = 0))                  // due to aeration stress or ETo = 0
THEN SetCCiActual(GetCCiPrev());           // no transpiration, no crop developmentc
END; (* FeedbackCC *)



PROCEDURE HorizontalInflowGWTable(DepthGWTmeter : double);
Var Ztot, Zi, DeltaTheta, SaltAct,SaltAdj : double;
    compi,celli : INTEGER;
    Compi_temp : CompartmentIndividual;
BEGIN
Ztot := 0;
FOR compi := 1 TO GetNrCompartments() DO
    BEGIN
    Ztot := Ztot + GetCompartment_Thickness(compi);
    Zi := Ztot - GetCompartment_Thickness(compi)/2;
    IF (Zi >= DepthGWTmeter) THEN
       BEGIN
       // soil water content is at saturation
       IF (GetCompartment_Theta(compi) < GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100) THEN
          BEGIN
          DeltaTheta := GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100 - GetCompartment_Theta(compi);
          SetCompartment_theta(compi, GetSoilLayer_i(GetCompartment_Layer(compi)).SAT/100);
          HorizontalWaterFlow := HorizontalWaterFlow + 1000 * DeltaTheta * GetCompartment_Thickness(compi)
                                 * (1 - GetSoilLayer_i(GetCompartment_Layer(compi)).GravelVol/100);
          END;
       // ECe is equal to the EC of the groundwater table
       IF (Abs(ECeComp(GetCompartment_i(compi)) - GetECiAqua()) > 0.0001) THEN
          BEGIN
          SaltAct := 0;
          FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
              SaltAct := SaltAct + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100; // Mg/ha
          Compi_temp := GetCompartment_i(compi);
          DetermineSaltContent(GetECiAqua(),Compi_temp);
          SetCompartment_i(compi, Compi_temp);
          SaltAdj := 0;
          FOR celli := 1 TO GetSoilLayer_i(GetCompartment_Layer(compi)).SCP1 DO
              SaltAdj := SaltAdj + (GetCompartment_Salt(compi, celli) + GetCompartment_Depo(compi, celli))/100; // Mg/ha
          HorizontalSaltFlow := HorizontalSaltFlow + (SaltAdj - SaltAct);
          END;
       END;
    END;
END; (* HorizontalInflowGWTable *)




BEGIN (* BUDGET_module *)


// 1. Soil water balance
control := begin_day;
ECdrain_temp := GetECdrain();
CheckWaterSaltBalance(dayi, InfiltratedRain, control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain_temp,HorizontalWaterFlow, HorizontalSaltFlow, SubDrain);
SetECdrain(ECdrain_temp);

// 2. Adjustments in presence of Groundwater table
CheckForWaterTableInProfile((GetZiAqua()/100),GetCompartment(),WaterTableInProfile);
Comp_temp := GetCompartment();
CalculateAdjustedFC((GetZiAqua()/100),Comp_temp);
SetCompartment(Comp_temp);

// 3. Drainage
calculate_drainage;

// 4. Runoff
IF (GetManagement_Bundheight() < 0.001) THEN
   BEGIN
   SetDaySubmerged(0);
   IF ((GetManagement_RunoffON() = true) AND (GetRain() > 0.1)) THEN calculate_runoff(GetSimulParam_RunoffDepth());
   END;

// 5. Infiltration (Rain and Irrigation)
IF ((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   THEN CalculateEffectiveRainfall(SubDrain);
IF (((GetIrriMode() = Generate) AND (GetIrrigation() = 0)) AND (TargetTimeVal <> -999))
   THEN Calculate_irrigation(SubDrain, TargetTimeVal, TargetDepthVal);
IF (GetManagement_Bundheight() >= 0.01)
   THEN calculate_surfacestorage(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage,ECinfilt, SubDrain, dayi)
   ELSE calculate_Extra_runoff(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain);
calculate_infiltration(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain);

// 6. Capillary Rise
CRwater_temp := GetCRwater();
CRsalt_temp := GetCRsalt();
calculate_CapillaryRise(CRwater_temp,CRsalt_temp);
SetCRwater(CRwater_temp);
SetCRsalt(CRsalt_temp);

// 7. Salt balance
calculate_saltcontent(InfiltratedRain,InfiltratedIrrigation,InfiltratedStorage, SubDrain, dayi);


// 8. Check Germination
IF ((GetSimulation_Germinate() = false) AND (dayi >=GetCrop().Day1)) THEN CheckGermination;

// 9. Determine effect of soil fertiltiy and soil salinity stress
// EffectSoilFertilitySalinityStress(Simulation.EffectStress);
IF (NoMoreCrop = false) THEN EffectSoilFertilitySalinityStress(StressSFadjNEW, Coeffb0Salt, Coeffb1Salt,
                                            Coeffb2Salt, NrDayGrow, StressTotSaltPrev, VirtualTimeCC);


// 10. Canopy Cover (CC)
IF (NoMoreCrop = false) THEN
   BEGIN
   // determine water stresses affecting canopy cover
   SWCtopSoilConsidered_temp := GetSimulation_SWCtopSoilConsidered();
   DetermineRootZoneWC(GetRootingDepth(),SWCtopSoilConsidered_temp);
   SetSimulation_SWCtopSoilConsidered(SWCtopSoilConsidered_temp);
   // determine canopy cover
   CASE GetCrop_ModeCycle() OF
           GDDays : DetermineCCiGDD(CCxTotal,CCoTotal, StressLeaf, FracAssim, MobilizationON, StorageON, SumGDDAdjCC, VirtualTimeCC, StressSenescence, TimeSenescence, NoMoreCrop, CDCTotal, CGCAdjustmentAfterCutting, GDDayFraction, GDDayi, GDDCDCTotal, GDDTadj);
           else DetermineCCi(CCxTotal, CCoTotal, StressLeaf, FracAssim,MobilizationON, StorageON, Tadj, VirtualTimeCC, StressSenescence, TimeSenescence, NoMoreCrop, CDCTotal, CGCAdjustmentAfterCutting, DayFraction, GDDCDCTotal, TESTVAL);
           end;
   END;

// 11. Determine Tpot and Epot
// 11.1 Days after Planting
IF (GetCrop_ModeCycle() = Calendardays)
   THEN DAP := VirtualTimeCC
   ELSE BEGIN // growing degree days - to position correctly where in cycle
        DAP := SumCalendarDays(ROUND(SumGDDadjCC),
                      GetCrop().Day1,GetCrop().Tbase,GetCrop().Tupper,GetSimulParam_Tmin(),GetSimulParam_Tmax());
        DAP := DAP + GetSimulation_DelayedDays(); // are not considered when working with GDDays
        END;
// 11.2 Calculation
Tpot_temp := GetTpot();
CalculateETpot(DAP,GetCrop().DaysToGermination,GetCrop().DaysToFullCanopy,GetCrop().DaysToSenescence,GetCrop().DaysToHarvest,DayLastCut,
               GetCCiActual(),GetETo(),GetCrop().KcTop,GetCrop().KcDecline,GetCrop().CCxAdjusted,GetCrop().CCxWithered,GetCrop().CCEffectEvapLate,CO2i,
               GDDayi,GetCrop().GDtranspLow,Tpot_temp,EpotTot);
SetTpot(Tpot_temp);
SetEpot(EpotTot);    // adjustment Epot for mulch and partial wetting in next step
Crop_pActStom_temp := GetCrop().pActStom;
AdjustpStomatalToETo(GetETo(),Crop_pActStom_temp);
SetCrop_pActStom(Crop_pActStom_temp);

// 12. Evaporation
IF (GetPreDay() = false) THEN PrepareStage2; // Initialize Simulation.EvapstartStg2 (REW is gone)
IF ((GetRain() > 0) OR
   ((GetIrrigation() > 0) AND (GetIrriMode() <> Inet)))
   THEN PrepareStage1;
EvapWCsurf_temp := GetSimulation_EvapWCsurf();
Epot_temp := GetEpot();
AdjustEpotMulchWettedSurface(dayi,EpotTot,Epot_temp,EvapWCsurf_temp);
SetEpot(Epot_temp);
SetSimulation_EvapWCsurf(EvapWCsurf_temp);
IF (((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   AND (GetSimulParam_EffectiveRain_RootNrEvap() > 0)) // reduction soil evaporation
 THEN SetEpot(GetEpot() * (exp((1/GetSimulParam_EffectiveRain_RootNrEvap())*ln((GetSoil().REW+1)/20))));
// actual evaporation
SetEact(0);
IF (GetEpot() > 0) THEN
   BEGIN
   // surface water
   IF (GetSurfaceStorage() > 0) THEN CalculateEvaporationSurfaceWater;
   // stage 1 evaporation
   IF ((ABS(GetEpot() - GetEact()) > 0.0000001) AND (GetSimulation_EvapWCsurf() > 0))
      THEN CalculateSoilEvaporationStage1;
   // stage 2 evaporation
   IF (ABS(GetEpot() - GetEact()) > 0.0000001) THEN CalculateSoilEvaporationStage2;
   END;
// Reset redcution Epot for 10-day or monthly rainfall data
IF (((GetRainRecord_DataType() = Decadely) OR (GetRainRecord_DataType() = Monthly))
   AND (GetSimulParam_EffectiveRain_RootNrEvap() > 0))
 THEN SetEpot(GetEpot()/(exp((1/GetSimulParam_EffectiveRain_RootNrEvap())*ln((GetSoil().REW+1)/20))));


// 13. Transpiration
IF ((NoMoreCrop = false) AND (GetRootingDepth() > 0.0001)) THEN
   BEGIN
//   Tact_temp := GetTact();
   IF ((GetSurfaceStorage() > 0) AND
       ((GetCrop().AnaeroPoint = 0) OR (GetDaySubmerged() < GetSimulParam_DelayLowOxygen())))
       THEN surface_transpiration
       ELSE calculate_transpiration(GetTpot());
   END;
//   SetTact(Tact_temp);
IF (GetSurfaceStorage() <= 0) THEN SetDaySubmerged(0);
FeedbackCC;

// 14. Adjustment to groundwater table
IF WaterTableInProfile THEN HorizontalInflowGWTable(GetZiAqua()/100);

// 15. Salt concentration
ConcentrateSalts;

// 16. Soil water balance
control := end_day;
ECdrain_temp := GetECdrain();
CheckWaterSaltBalance(dayi,InfiltratedRain, control,InfiltratedIrrigation,InfiltratedStorage,Surf0,ECInfilt,ECdrain_temp,HorizontalWaterFlow,HorizontalSaltFlow, SubDrain);
SetECdrain(ECdrain_temp);
END; (* BUDGET_module *)




end.
