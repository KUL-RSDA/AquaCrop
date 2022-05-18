unit interface_tempprocessing;


interface


uses interface_global;


procedure AdjustMONTHandYEAR(
            VAR Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustmonthandyear';

procedure AdjustDecadeMONTHandYEAR(
            VAR DecFile,Mfile,Yfile : INTEGER);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustdecademonthandyear';

procedure SetDayNrToYundef(
            VAR DayNri : LongInt);
        external 'aquacrop' name '__ac_tempprocessing_MOD_setdaynrtoyundef';

procedure GetDecadeTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getdecadetemperaturedataset';

procedure GetMonthlyTemperatureDataSet(
            constref DayNri : LongInt;
            VAR TminDataSet,TmaxDataSet : rep_SimulationEventsDbl);
        external 'aquacrop' name '__ac_tempprocessing_MOD_getmonthlytemperaturedataset';

function GrowingDegreeDays(
            constref ValPeriod : INTEGER;
            constref FirstDayPeriod : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : integer;
        external 'aquacrop' name '__ac_tempprocessing_MOD_growingdegreedays';

function SumCalendarDays(
            constref ValGDDays : INTEGER;
            constref FirstDayCrop : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : integer;
        external 'aquacrop' name '__ac_tempprocessing_MOD_sumcalendardays';

FUNCTION MaxAvailableGDD(
            constref FromDayNr : LongInt;
            constref Tbase,Tupper,TDayMin,TDayMax : double) : Double;
         external 'aquacrop' name '__ac_tempprocessing_MOD_maxavailablegdd';

procedure GDDCDCToCDC(
            constref PlantDayNr : LongInt;
            constref D123,GDDL123,GDDHarvest : INTEGER;
            constref CCx,GDDCDC,Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
            VAR CDC : double);
        external 'aquacrop' name '__ac_tempprocessing_MOD_gddcdctocdc';

procedure AdjustCalendarDays(
            constref PlantDayNr : LongInt;
            constref InfoCropType : rep_subkind;
            constref Tbase,Tupper,NoTempFileTMin,NoTempFileTMax : double;
            constref GDDL0,GDDL12,GDDFlor,GDDLengthFlor,GDDL123,GDDHarvest,GDDLZmax : INTEGER;
            VAR GDDHImax : INTEGER;
            constref GDDCGC,GDDCDC,CCo,CCx : double;
            constref IsCGCGiven : BOOLEAN;
            constref HIndex : INTEGER;
            constref TheDaysToCCini : INTEGER;
            constref ThePlanting : rep_planting;
            VAR D0,D12,DFlor,LengthFlor,D123,DHarvest,DLZmax,LHImax : INTEGER;
            VAR StLength : rep_int_array;
            VAR CGC,CDC,dHIdt : double;
            VAR Succes : BOOLEAN);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcalendardays';

procedure AdjustCalendarCrop(
            constref FirstCropDay : LongInt);
        external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcalendarcrop';

function RoundedOffGDD(
            constref PeriodGDD,PeriodDay : INTEGER;
            constref FirstDayPeriod : LongInt;
            constref TempTbase,TempTupper,TempTmin,TempTmax : double) : INTEGER;
         external 'aquacrop' name '__ac_tempprocessing_MOD_roundedoffgdd';

procedure HIadjColdHeat(
            constref TempHarvest,TempFlower,TempLengthFlowering,TempHI : INTEGER;
            constref TempTmin,TempTmax : double;
            constref TempTcold,TempTheat : shortInt;
            constref TempfExcess : smallInt;
            VAR HIadjusted : double;
            VAR ColdStress,HeatStress : BOOLEAN);
         external 'aquacrop' name '__ac_tempprocessing_MOD_hiadjcoldheat';

function ResetCropDay1(
            constref CropDay1IN : LongInt;
            constref SwitchToYear1 : BOOLEAN) : LongInt;
         external 'aquacrop' name '__ac_tempprocessing_MOD_resetcropday1';

procedure CropStressParametersSoilSalinity(
            constref CCxRed,CCdistortion : ShortInt;
            constref CCo,CCx,CGC,GDDCGC : double;
            constref CropDeterm : BOOLEAN;
            constref L12,LFlor,LengthFlor,L123 : INTEGER;
            constref GDDL12,GDDLFlor,GDDLengthFlor,GDDL123 : INTEGER;
            constref TheModeCycle : rep_modeCycle;
            VAR StressResponse : rep_EffectStress);
         external 'aquacrop' name '__ac_tempprocessing_MOD_cropstressparameterssoilsalinity';

procedure TemperatureFileCoveringCropPeriod(
            constref CropFirstDay,CropLastDay : LongInt);
         external 'aquacrop' name '__ac_tempprocessing_MOD_temperaturefilecoveringcropperiod';

procedure AdjustCropFileParameters(
            constref TheCropFileSet : rep_CropFileSet;
            constref LseasonDays : INTEGER;
            constref TheCropDay1 : LongInt;
            constref TheModeCycle  : rep_modeCycle;
            constref TheTbase,TheTupper  : double;
            VAR L123,L1234,GDD123,GDD1234 : INTEGER);
         external 'aquacrop' name '__ac_tempprocessing_MOD_adjustcropfileparameters';

procedure LoadSimulationRunProject(
            constref NameFileFull : string;
            constref NrRun : INTEGER);

procedure LoadSimulationRunProject_wrap(
            constref IrriFileFull : PChar;
            constref strlen1 : integer;
            constref NrRun : INTEGER);
         external 'aquacrop' name '__ac_interface_tempprocessing_MOD_loadsimulationrunproject_wrap';

procedure BTransferPeriod(
            constref TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD : double;
            constref TheModeCycle : rep_modeCycle;
            constref TempAssimPeriod : INTEGER;
            constref TempAssimStored : ShortInt;
            VAR SumBtot,SumBstored : double);

procedure BTransferPeriod_wrap(
            constref TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD : double;
            constref TheModeCycle : integer;
            constref TempAssimPeriod : INTEGER;
            constref TempAssimStored : ShortInt;
            VAR SumBtot,SumBstored : double);
         external 'aquacrop' name '__ac_tempprocessing_MOD_btransferperiod';

function Bnormalized(
            constref TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : rep_modeCycle;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;

function Bnormalized_wrap(
            constref TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : integer;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;
         external 'aquacrop' name '__ac_interface_tempprocessing_MOD_bnormalized_wrap';

function  BiomassRatio_wrap(
            constref TempDaysToCCini,TempGDDaysToCCini : INTEGER;
            constref TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
            constref TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                     TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
            constref TempKc,TempKcDecline,TempCCeffect,
                     TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
            constref  TempModeCycle : integer;
            constref  SFInfo : rep_EffectStress;
            constref  SFInfoStress,WeedStress : ShortInt;
            constref  DeltaWeedStress : INTEGER;
            constref  DeterminantCropType,FertilityStressOn : BOOLEAN) : double;
         external 'aquacrop' name '__ac_interface_tempprocessing_MOD_biomassratio_wrap';

function  BiomassRatio(
            constref TempDaysToCCini,TempGDDaysToCCini : INTEGER;
            constref TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
            constref TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                     TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
            constref TempKc,TempKcDecline,TempCCeffect,
                     TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
            constref  TempModeCycle : rep_modeCycle;
            constref  SFInfo : rep_EffectStress;
            constref  SFInfoStress,WeedStress : ShortInt;
            constref  DeltaWeedStress : INTEGER;
            constref  DeterminantCropType,FertilityStressOn : BOOLEAN) : double;


procedure StressBiomassRelationship_wrap(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref CropSResp : rep_Shapes;
            constref TheCropType : integer;
            constref TheModeCycle : integer;
            VAR b0,b1,b2 : double;
            VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);
         external 'aquacrop' name '__ac_interface_tempprocessing_MOD_stressbiomassrelationship_wrap';

procedure StressBiomassRelationship(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref CropSResp : rep_Shapes;
            constref TheCropType : rep_subkind;
            constref TheModeCycle : rep_modeCycle;
            VAR b0,b1,b2 : double;
            VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);

procedure CCxSaltStressRelationship_wrap(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref TheCropType : integer;
            constref TheModeCycle : integer;
            constref TheCCsaltDistortion : ShortInt;
            VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
            VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);
     external 'aquacrop' name '__ac_interface_tempprocessing_MOD_ccxsaltstressrelationship_wrap';

procedure CCxSaltStressRelationship(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref TheCropType : rep_subkind;
            constref TheModeCycle : rep_modeCycle;
            constref TheCCsaltDistortion : ShortInt;
            VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
            VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);


implementation


procedure LoadSimulationRunProject(
            constref NameFileFull : string;
            constref NrRun : INTEGER);
var
    p1 : PChar;
    strlen1: integer;
begin;
    p1 := PChar(NameFileFull);
    strlen1 := Length(NameFileFull);
    LoadSimulationRunProject_wrap(p1, strlen1, NrRun);
end;

procedure BTransferPeriod(
            constref TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L123,L1234,GDDL0,GDDL12,GDDL123,GDDL1234 : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD : double;
            constref TheModeCycle : rep_modeCycle;
            constref TempAssimPeriod : INTEGER;
            constref TempAssimStored : ShortInt;
            VAR SumBtot,SumBstored : double);
var
    int_modeCycle : integer;
begin;
    int_modeCycle := ord(TheModeCycle);
    BTransferPeriod_wrap(TheDaysToCCini, TheGDDaysToCCini,
             L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234,
             CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, 
             KcDeclAgeing, CCeffectProcent, WPbio, TheCO2,
             Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, RatDGDD,
             int_modeCycle, TempAssimPeriod, TempAssimStored, SumBtot, SumBstored);
end;

function Bnormalized(
            constref TheDaysToCCini,TheGDDaysToCCini,
                     L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,
                     GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,
                     CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,
                     GDtranspLow,RatDGDD,SumKcTop : double;
            constref StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,
                     StrResRedKsSto,WeedStress : ShortInt;
            constref DeltaWeedStress : INTEGER;
            constref StrResCDecline,ShapeFweed : Double;
            constref TheModeCycle : rep_modeCycle;
            constref FertilityStressOn : BOOLEAN;
            constref TestRecord : BOOLEAN) : DOUBLE;
var
   int_modeCycle : integer;
begin
   int_modeCycle := ord(TheModeCycle);
   Bnormalized := Bnormalized_wrap(
                      TheDaysToCCini,TheGDDaysToCCini,L0,L12,L12SF,L123,L1234,LFlor,GDDL0,GDDL12,GDDL12SF,GDDL123,GDDL1234,WPyield,DaysYieldFormation,tSwitch,
                      CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,KcTop,KcDeclAgeing,CCeffectProcent,WPbio,TheCO2,Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,RatDGDD,SumKcTop,
                      StressInPercent,StrResRedCGC,StrResRedCCx,StrResRedWP,StrResRedKsSto,WeedStress,
                      DeltaWeedStress,
                      StrResCDecline,ShapeFweed,
                      int_modeCycle,
                      FertilityStressOn, TestRecord); 
end;

function  BiomassRatio(
            constref TempDaysToCCini,TempGDDaysToCCini : INTEGER;
            constref TempCCo,TempCGC,TempCCx,TempCDC,TempGDDCGC,TempGDDCDC,TempdHIdt : double;
            constref TempL0,TempL12,L12SF,TempL123,TempHarvest,TempFlower,
                     TempGDDL0,GDDL12SF,TempGDDL12,TempGDDL123,TempGDDHarvest,TempHI,TempWPy : INTEGER;
            constref TempKc,TempKcDecline,TempCCeffect,
                     TempTbase,TempTupper,TempTmin,TempTmax,TempGDtranspLow,TempWP,ShapeFweed : double;
            constref  TempModeCycle : rep_modeCycle;
            constref  SFInfo : rep_EffectStress;
            constref  SFInfoStress,WeedStress : ShortInt;
            constref  DeltaWeedStress : INTEGER;
            constref  DeterminantCropType,FertilityStressOn : BOOLEAN) : double;
var
   int_modeCycle : integer;
begin
   int_modeCycle := ord(TempModeCycle);
   BiomassRatio := BiomassRatio_wrap(TempDaysToCCini, TempGDDaysToCCini,
           TempCCo, TempCGC, TempCCx, TempCDC, TempGDDCGC, 
           TempGDDCDC, TempdHIdt, TempL0, TempL12, L12SF,
           TempL123, TempHarvest, TempFlower, TempGDDL0, 
           GDDL12SF, TempGDDL12, TempGDDL123, TempGDDHarvest,
           TempHI, TempWPy, TempKc, TempKcDecline, TempCCeffect,
           TempTbase, TempTupper, TempTmin, TempTmax, TempGDtranspLow,
           TempWP, ShapeFweed, int_modeCycle, SFInfo, SFInfoStress,
           WeedStress, DeltaWeedStress, DeterminantCropType, FertilityStressOn);
end;

procedure StressBiomassRelationship(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDtranspLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref CropSResp : rep_Shapes;
            constref TheCropType : rep_subkind;
            constref TheModeCycle : rep_modeCycle;
            VAR b0,b1,b2 : double;
            VAR BM10,BM20,BM30,BM40,BM50,BM60,BM70 : double);
var
   int_modeCycle : integer;
   int_subkind : integer;
begin
   int_modeCycle := ord(TheModeCycle);
   int_subkind   := ord(theCropType);
   StressBiomassRelationship_wrap(TheDaysToCCini, TheGDDaysToCCini,
            L0, L12, L123, L1234, LFlor, LengthFlor, GDDL0, GDDL12,
            GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC, GDDCGC,
            CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent,
            Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, WPveg, RatedHIdt,
            CO2Given, CropDNr1, CropDeterm, CropSResp, int_subkind,
            int_modeCycle, b0, b1, b2, 
            BM10, BM20, BM30, BM40, BM50, BM60, BM70);
end;

procedure CCxSaltStressRelationship(
            constref TheDaysToCCini,TheGDDaysToCCini : INTEGER;
            constref L0,L12,L123,L1234,
                     LFlor,LengthFlor,GDDFlor,GDDLengthFlor,
                     GDDL0,GDDL12,GDDL123,GDDL1234,WPyield,RefHI : INTEGER;
            constref CCo,CCx,CGC,GDDCGC,CDC,GDDCDC,
                     KcTop,KcDeclAgeing,CCeffectProcent,
                     Tbase,Tupper,TDayMin,TDayMax,GDbioLow,WPveg,RatedHIdt,CO2Given : double;
            constref CropDNr1 : LongInt;
            constref CropDeterm : BOOLEAN;
            constref TheCropType : rep_subkind;
            constref TheModeCycle : rep_modeCycle;
            constref TheCCsaltDistortion : ShortInt;
            VAR Coeffb0Salt,Coeffb1Salt,Coeffb2Salt : double;
            VAR Salt10,Salt20,Salt30,Salt40,Salt50,Salt60,Salt70,Salt80,Salt90 : double);
var
   int_modeCycle : integer;
   int_subkind : integer;
begin
   int_modeCycle := ord(TheModeCycle);
   int_subkind   := ord(theCropType);
   CCxSaltStressRelationship_wrap(TheDaysToCCini, TheGDDaysToCCini,
       L0, L12, L123, L1234, LFlor, LengthFlor, GDDFlor, GDDLengthFlor,
       GDDL0, GDDL12, GDDL123, GDDL1234, WPyield, RefHI, CCo, CCx, CGC,
       GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase,
       Tupper, TDayMin, TDayMax, GDbioLow, WPveg, RatedHIdt, CO2Given,
       CropDNr1, CropDeterm, int_subkind, int_modeCycle, TheCCsaltDistortion,
       Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, Salt10, Salt20, Salt30,
       Salt40, Salt50, Salt60, Salt70, Salt80, Salt90);
end;

initialization


end.

