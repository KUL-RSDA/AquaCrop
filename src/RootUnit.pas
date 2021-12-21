unit RootUnit;

interface

uses Global, interface_global;

FUNCTION AdjustedRootingDepth(CCAct,CCpot,Tpot,Tact,StressLeaf : double;
                              StressSenescence : double;
                              DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234 : INTEGER;
                              SumGDDPrev,SumGDD,Zmin,Zmax,Ziprev : double;
                              ShapeFactor : ShortInt;
                              TypeDays : rep_modeCycle) : double;




implementation


FUNCTION AdjustedRootingDepth(CCAct,CCpot,Tpot,Tact,StressLeaf : double;
                              StressSenescence : double;
                              DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234 : INTEGER;
                              SumGDDPrev,SumGDD,Zmin,Zmax,Ziprev : double;
                              ShapeFactor : ShortInt;
                              TypeDays : rep_modeCycle) : double;

VAR Zi,ZiUnlimM1,ZiUnlim,Dz,ZiTest,Zsoil,ThetaTreshold,TAWcompi,Wrel,pZexp,
    Zlimit,ZiMax : double;
    compi : ShortInt;
BEGIN
IF (ROUND(Ziprev) = undef_int)
   THEN Zi := ActualRootingDepth(DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234,
                         SumGDD,Zmin,Zmax,ShapeFactor,TypeDays)
   ELSE BEGIN
        // 1. maximum rooting depth (ZiMax) that could have been reached at time t
        // -- 1.1 Undo effect of restrictive soil layer(s)
        IF (ROUND(Soil.RootMax*1000) < ROUND(Zmax*1000))
           THEN BEGIN
                Zlimit := Soil.RootMax;
                Soil.RootMax := Zmax;
                END
           ELSE Zlimit := Zmax;
        // -- 1.2 Calculate ZiMax
        ZiMax := ActualRootingDepth(DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234,
                         SumGDD,Zmin,Zmax,ShapeFactor,TypeDays);
        // -- 1.3 Restore effect of restrive soil layer(s)
        Soil.RootMax := Zlimit;

        // 2. increase (dZ) at time t
        ZiUnlimM1 := ActualRootingDepth((DAP-1),L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234,
                         SumGDDPrev,Zmin,Zmax,ShapeFactor,TypeDays);
        ZiUnlim := ActualRootingDepth(DAP,L0,LZmax,L1234,GDDL0,GDDLZmax,GDDL1234,
                         SumGDD,Zmin,Zmax,ShapeFactor,TypeDays);
        dZ := ZiUnlim - ZiUnlimM1;

        // 3. corrections of dZ
        // -- 3.1 correction for restrictive soil layer is already considered in ActualRootingDepth

        // -- 3.2 correction for stomatal closure
        IF ((Tpot > 0) AND (Tact < Tpot) AND (SimulParam.KsShapeFactorRoot <> undef_int))
           THEN BEGIN
                IF (SimulParam.KsShapeFactorRoot >= 0)
                   THEN dZ := dZ * (Tact/Tpot)   //linear
                   ELSE dZ := dZ * (Exp((Tact/Tpot)*SimulParam.KsShapeFactorRoot)-1)
                                   /(Exp(SimulParam.KsShapeFactorRoot)-1); // exponential
                END;

         // -- 3.2 correction for dry soil at expansion front of actual root zone
         IF (dZ > 0.001) THEN
            BEGIN
            pZexp := Crop.pdef + (1-Crop.pdef)/2; // soil water depletion threshold for root deepening
            ZiTest := Ziprev + dZ; // restrictive soil layer is considered by ActualRootingDepth
            compi := 0;
            Zsoil := 0;
            While ((Zsoil < ZiTest) AND (compi < NrCompartments)) DO
               BEGIN
               compi := compi + 1;
               Zsoil := Zsoil + Compartment[compi].Thickness;
               END;
            TAWcompi := SoilLayer[Compartment[compi].Layer].FC/100 - SoilLayer[Compartment[compi].Layer].WP/100;
            ThetaTreshold := SoilLayer[Compartment[compi].Layer].FC/100 - pZexp * TAWcompi;
            IF (Compartment[compi].theta < ThetaTreshold) THEN
               BEGIN // expansion is limited due to soil water content at expansion front
               IF (Compartment[compi].theta <= SoilLayer[Compartment[compi].Layer].WP/100)
                  THEN dZ := 0
                  ELSE BEGIN
                       Wrel := (SoilLayer[Compartment[compi].Layer].FC/100 - Compartment[compi].theta)/TAWcompi;
                       dZ := dZ * KsAny(Wrel,pZexp,(1),Crop.KsShapeFactorStomata);
                       END;
               END;
            END;

        // -- 3.3 correction for early senescence
        IF ((CCact <= 0) AND (CCpot > 50)) THEN dZ := 0;

        // -- 3.4 correction for no germination
        IF NOT Simulation.Germinate THEN dZ := 0;

        // 4. actual rooting depth (Zi)
        Zi := Ziprev + dZ;

        // 5. Correction for root density if root deepening is restricted (dry soil and/or restricitive layers)
        IF (ROUND(Zi*1000) < ROUND(ZiMax*1000))
          THEN BEGIN
               // Total extraction in restricted root zone (Zi) and max root zone (ZiMax) should be identical
               Simulation.SCor := (2*(ZiMax/Zi)*((Crop.SmaxTop+Crop.SmaxBot)/2)-Crop.SmaxTop)/Crop.SmaxBot;
               // consider part of the restricted deepening due to water stress (= less roots)
               IF (SumWabal.Tpot > 0) THEN
                  BEGIN
                  Simulation.SCor := Simulation.SCor * (SumWabal.Tact/SumWabal.Tpot);
                  IF (Simulation.SCor < 1) THEN Simulation.SCor := 1;
                  END;
               END
          ELSE Simulation.SCor := 1;
        END;
AdjustedRootingDepth := Zi;
END; (* AdjustedRootingDepth *)



end.
