UNIT GLOBAL;

INTERFACE

USES SYSUTILS, INTERFACE_GLOBAL;


Const max_No_compartments = 12
Equiv = 0.64 !! conversion factor: 1 dS/m = 0.64 g/l

NameMonth : ARRAY[1..12] OF character(len=STRING_LENGTH) = ('January','February','March','April','May','June','July','August','September','October','November','December')

DaysInMonth : ARRAY[1..12] OF integer(int32) = (31,28,31,30,31,30,31,31,30,31,30,31)
EvapZmin = 15 !! cm  minimum soil depth for water extraction by evaporation

repstring17 = string[17] !! Date string
rep_string3  = string[3] !! Read/Write ProfFile

type CompartmentIndividual 
    real(dp) :: Thickness
        !! meter
    real(dp) :: theta
        !! m3/m3
    real(dp) :: fluxout
        !! mm/day
    integer(int32) :: Layer
        !! Undocumented
    real(dp) :: Smax
        !! Maximum root extraction m3/m3.day
    real(dp) :: FCadj
        !! Vol % at Field Capacity adjusted to Aquifer
    integer(int32) :: DayAnaero
        !! number of days under anaerobic conditions
    real(dp) :: WFactor
        !! weighting factor 0 ... 1
        !! Importance of compartment in calculation of
        !! - relative wetness (RUNOFF)
        !! - evaporation process
        !! - transpiration process *)
    !! salinity factors
    type(rep_salt) :: Salt
        !! salt content in solution in cells (g/m2)
    type(rep_salt) :: Depo
        !! salt deposit in cells (g/m2)
end type CompartmentIndividual 


rep_Comp = ARRAY[1.. max_No_compartments] OF CompartmentIndividual

rep_subkind = (Vegetative,Grain,Tuber,Forage)
rep_pMethod = (NoCorrection,FAOCorrection)


type rep_Assimilates 
    logical :: On
        !! Undocumented
    integer(int32) :: Period
        !! Number of days at end of season during which assimilates are stored in root system
    integer(int8) :: Stored
        !! Percentage of assimilates, transferred to root system at last day of season
    integer(int8) :: Mobilized
        !! Percentage of stored assimilates, transferred to above ground parts in next season
end type rep_Assimilates 


type rep_Crop 
    type(rep_subkind) :: subkind
        !! Undocumented
    type(rep_modeCycle) :: ModeCycle
        !! Undocumented
    type(rep_Planting) :: Planting
        !! 1 = sown, 0 = transplanted, -9 = regrowth
    type(rep_pMethod) :: pMethod
        !! Undocumented
    real(dp) :: pdef
        !! soil water depletion fraction for no stomatal stress as defined (ETo = 5 mm/day)
    real(dp) :: pActStom
        !! actual p for no stomatal stress for ETo of the day
    real(dp) :: KsShapeFactorLeaf, KsShapeFactorStomata, KsShapeFactorSenescence
        !! Undocumented
    real(dp) :: pLeafDefUL, pLeafDefLL
        !! soil water depletion fraction for leaf expansion (ETo = 5 mm/day)
    real(dp) :: pLeafAct
        !! actual p for upper limit leaf expansion for ETo of the day
    real(dp) :: pSenescence
        !! soil water depletion fraction for canopys senescence (ETo = 5 mm/day)
    real(dp) :: pSenAct
        !! actual p for canopy senescence for ETo of the day
    real(dp) :: pPollination
        !! soil water depletion fraction for failure of pollination
    integer(int32) :: SumEToDelaySenescence
        !! Undocumented
    integer(int32) :: AnaeroPoint
        !! (SAT - [vol%]) at which deficient aeration
    type(rep_Shapes) :: StressResponse
        !! is reponse to soil fertility stress
    integer(int8) :: ECemin
        !! lower threshold for salinity stress (dS/m)
    integer(int8) :: ECemax
        !! upper threshold for salinity stress (dS/m)
    integer(int8) :: CCsaltDistortion
        !! distortion canopy cover for calibration for simulation of effect of salinity stress (%)
    integer(int32) :: ResponseECsw
        !! Response of Ks stomata to ECsw for calibration: From 0 (none) to +200 (very strong)
    real(dp) :: SmaxTopQuarter
        !! Smax Top 1/4 root zone HOOGLAND
    real(dp) :: SmaxBotQuarter
        !! Smax Bottom 1/4 root zone HOOGLAND
    real(dp) :: SmaxTop
        !! Smax Top root zone HOOGLAND
    real(dp) :: SmaxBot
        !! Smax Bottom root zone HOOGLAND
    real(dp) :: KcTop
        !! Undocumented
    real(dp) :: KcDecline
        !! Reduction Kc (%CCx/day) as result of ageing effects, nitrogen defficiency, etc.
    integer(int32) :: CCEffectEvapLate
        !! %
    integer(int32) :: Day1
        !! Daynummer: first day of croping period starting from sowing/transplanting
    integer(int32) :: DayN
        !! Daynummer: last day = harvest day
    type(rep_int_array) :: Length
        !! 1 .. 4 :  = the four growth stages
    real(dp) :: RootMin, RootMax
        !! rooting depth in meter
    integer(int8) :: RootShape
        !! 10 times the root of the root function
    real(dp) :: Tbase
        !! Base Temperature (degC)
    real(dp) :: Tupper
        !! Upper temperature threshold (degC)
    integer(int8) :: Tcold
        !! Minimum air temperature below which pollination starts to fail (cold stress) (degC)
    integer(int8) :: Theat
        !! Maximum air temperature above which pollination starts to fail (heat stress) (degC)
    real(dp) :: GDtranspLow
        !! Minimum growing degrees required for full crop transpiration (degC - day)
    real(dp) :: SizeSeedling
        !! Canopy cover per seedling (cm2)
    real(dp) :: SizePlant
        !! Canopy cover of plant on 1st day (cm2) when regrowth
    integer(int32) :: PlantingDens
        !! number of plants per hectare
    real(dp) :: CCo
        !! starting canopy size  (fraction canopy cover)
    real(dp) :: CCini
        !! starting canopy size for regrowth (fraction canopy cover)
    real(dp) :: CGC
        !! Canopy growth coefficient (increase of CC in fraction per day)
    real(dp) :: GDDCGC
        !! Canopy growth coefficient (increase of CC in fraction per growing-degree day)
    real(dp) :: CCx
        !! expected maximum canopy cover  (fraction canopy cover)
    real(dp) :: CDC
        !! Canopy Decline Coefficient (decrease of CC in fraction per day)
    real(dp) :: GDDCDC
        !! Canopy Decline Coefficient (decrease of CC in fraction per growing-degree day)
    real(dp) :: CCxAdjusted
        !! maximum canopy cover given water stress
    real(dp) :: CCxWithered
        !! maximum existed CC during season (for correction Evap for withered canopy)
    real(dp) :: CCoAdjusted
        !! initial canopy size after soil water stress
    integer(int32) :: DaysToCCini
        !! required for regrowth (if CCini > CCo)
    integer(int32) :: DaysToGermination
        !! given or calculated from GDD
    integer(int32) :: DaysToFullCanopy
        !! given or calculated from GDD
    integer(int32) :: DaysToFullCanopySF
        !! adjusted to soil fertility
    integer(int32) :: DaysToFlowering
        !! given or calculated from GDD
    integer(int32) :: LengthFlowering
        !! given or calculated from GDD
    integer(int32) :: DaysToSenescence
        !! given or calculated from GDD
    integer(int32) :: DaysToHarvest
        !! given or calculated from GDD
    integer(int32) :: DaysToMaxRooting
        !! given or calculated from GDD
    integer(int32) :: DaysToHIo
        !! given or calculated from GDD
    integer(int32) :: GDDaysToCCini
        !! required for regrowth (if CCini > CCo)
    integer(int32) :: GDDaysToGermination
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToFullCanopy
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToFullCanopySF
        !! adjusted to soil fertility
    integer(int32) :: GDDaysToFlowering
        !! given or calculated from Calendar Days
    integer(int32) :: GDDLengthFlowering
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToSenescence
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToHarvest
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToMaxRooting
        !! given or calculated from Calendar Days
    integer(int32) :: GDDaysToHIo
        !! given or calculated from Calendar Days
    real(dp) :: WP
        !! (normalized) water productivity (gram/m2)
    integer(int32) :: WPy
        !! (normalized) water productivity during yield formation (Percent WP)
    integer(int8) :: AdaptedToCO2
        !! Crop performance under elevated atmospheric CO2 concentration (%)
    integer(int32) :: HI
        !! HI harvest index (percentage)
    real(dp) :: dHIdt
        !! average rate of change in harvest index (% increase per calendar day)
    integer(int8) :: HIincrease
        !! possible increase (%) of HI due to water stress before flowering
    real(dp) :: aCoeff
        !! coefficient describing impact of restricted vegetative growth at flowering on HI
    real(dp) :: bCoeff
        !! coefficient describing impact of stomatal closure at flowering on HI
    integer(int8) :: DHImax
        !! allowable maximum increase (%) of specified HI
    logical :: DeterminancyLinked
        !! linkage of determinancy with flowering
    integer(int16) :: fExcess
        !! potential excess of fruits (%) ranging form
    integer(int8) :: DryMatter
        !! dry matter content (%) of fresh yield
    real(dp) :: RootMinYear1
        !! minimum rooting depth in first year in meter (for perennial crops)
    logical :: SownYear1
        !! True = Sown, False = transplanted (for perennial crops)
    integer(int8) :: YearCCx
        !! number of years at which CCx declines to 90 % of its value due to self-thinning - Perennials
    real(dp) :: CCxRoot
        !! shape factor of the decline of CCx over the years due to self-thinning - Perennials
    type(rep_Assimilates) :: Assimilates
        !! Undocumented
end type rep_Crop 


rep_EffectiveRainMethod = (Full,USDA,Percentage)
rep_MonthInteger = ARRAY[1..12] OF integer(int32)

type rep_EffectiveRain 
    !!  for 10-day or monthly rainfall data
    type(rep_EffectiveRainMethod) :: Method
        !! Undocumented
    integer(int8) :: PercentEffRain
        !! IF Method = Percentage
    integer(int8) :: ShowersInDecade
        !! adjustment of surface run-off
    integer(int8) :: RootNrEvap
        !! Root for reduction in soil evaporation
end type rep_EffectiveRain 


type rep_param 
    !!  DEFAULT.PAR
    !! crop parameters IN CROP.PAR - with Reset option
    integer(int8) :: EvapDeclineFactor
        !! exponential decline with relative soil water [1 = small ... 8 = sharp]
    real(dp) :: KcWetBare
        !! Soil evaporation coefficients from wet bare soil
    integer(int8) :: PercCCxHIfinal
        !! CC threshold below which HI no longer increase (% of 100)
    integer(int32) :: RootPercentZmin
        !! starting depth of root sine function in % of Zmin (sowing depth)
    real(dp) :: MaxRootZoneExpansion
        !! maximum root zone expansion in cm/day - fixed at 5 cm/day
    integer(int8) :: KsShapeFactorRoot
        !! shape factro for the effect of water stress on root zone expansion
    integer(int8) :: TAWGermination
        !! Soil water content (% TAW) required at sowing depth for germination
    real(dp) :: pAdjFAO
        !! Adjustment factor for FAO-adjustment of soil water depletion (p) for various ET
    integer(int32) :: DelayLowOxygen
        !! delay [days] for full effect of anaeroby
    real(dp) :: ExpFsen
        !! exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
    integer(int8) :: Beta
        !! Percentage decrease of p(senescence) once early canopy senescence is triggered
    integer(int8) :: ThicknessTopSWC
        !! Thickness of top soil for determination of its Soil Water Content (cm)
    !! Field parameter IN FIELD.PAR  - with Reset option
    integer(int8) :: EvapZmax
        !! cm  maximum soil depth for water extraction by evaporation
    !! Runoff parameters IN RUNOFF.PAR  - with Reset option
    real(dp) :: RunoffDepth
        !! considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment
    logical :: CNcorrection
        !! correction Antecedent Moisture Class (On/Off)
    !! Temperature parameters IN TEMPERATURE.PAR  - with Reset option
    real(dp) :: Tmin, Tmax
        !! Default Minimum and maximum air temperature (degC) if no temperature file
    integer(int8) :: GDDMethod
        !! 1 for Method 1, 2 for Method 2, 3 for Method 3
    !! General parameters IN GENERAL.PAR
    integer(int32) :: PercRAW
        !! allowable percent RAW depletion for determination Inet
    real(dp) :: CompDefThick
        !! Default thickness of soil compartments [m]
    integer(int32) :: CropDay1
        !! First day after sowing/transplanting (DAP = 1)
    real(dp) :: Tbase, Tupper
        !! Default base and upper temperature (degC) assigned to crop
    integer(int8) :: IrriFwInSeason
        !! Percentage of soil surface wetted by irrigation in crop season
    integer(int8) :: IrriFwOffSeason
        !! Percentage of soil surface wetted by irrigation off-season
    !! Showers parameters (10-day or monthly rainfall) IN SHOWERS.PAR
    type(rep_MonthInteger) :: ShowersInDecade
        !! 10-day or Monthly rainfall --> Runoff estimate
    type(rep_EffectiveRain) :: EffectiveRain
        !! 10-day or Monthly rainfall --> Effective rainfall
    !! Salinity
    integer(int8) :: SaltDiff
        !! salt diffusion factor (capacity for salt diffusion in micro pores) [%]
    integer(int8) :: SaltSolub
        !! salt solubility [g/liter]
    !! Groundwater table
    logical :: ConstGwt
        !! groundwater table is constant (or absent) during the simulation period
    !! Capillary rise
    integer(int8) :: RootNrDF
        !! Undocumented
    !! Initial abstraction for surface runoff
    integer(int8) :: IniAbstract
        !! Undocumented
end type rep_param 


rep_datatype = (Daily,Decadely,Monthly)
type rep_clim 
    type(rep_datatype) :: DataType
        !! Undocumented
    integer(int32) :: FromD, FromM, FromY
        !! D = day or decade, Y=1901 is not linked to specific year
    integer(int32) :: ToD, ToM, ToY
        !! Undocumented
    integer(int32) :: FromDayNr, ToDayNr
        !! daynumber
    character(len=STRING_LENGTH) :: FromString, ToString
        !! Undocumented
    integer(int32) :: NrObs
        !! number of observations
end type rep_clim 


rep_IniComp =  ARRAY[1.. max_No_compartments] OF real(dp)
type rep_IniSWC 
    logical :: AtDepths
        !! at specific depths or for specific layers
    integer(int8) :: NrLoc
        !! number of depths or layers considered
    type(rep_IniComp) :: Loc
        !! depth or layer thickness [m]
    type(rep_IniComp) :: VolProc
        !! soil water content (vol%)
    type(rep_IniComp) :: SaltECe
        !! ECe in dS/m
    logical :: AtFC
        !! If iniSWC is at FC
end type rep_IniSWC 


type rep_storage 
    real(dp) :: Btotal
        !! assimilates (ton/ha) stored in root systemn by CropString in Storage-Season
    character(len=STRING_LENGTH) :: CropString
        !! full name of crop file which stores Btotal during Storage-Season
    integer(int8) :: Season
        !! season in which Btotal is stored
end type rep_storage 


type rep_sim 
    integer(int32) :: FromDayNr, ToDayNr
        !! daynumber
    type(rep_IniSWC) :: IniSWC
        !! Undocumented
    type(rep_IniComp) :: ThetaIni
        !! Undocumented
    type(rep_IniComp) :: ECeIni
        !! dS/m
    real(dp) :: SurfaceStorageIni
        !! Undocumented
    real(dp) :: ECStorageIni
        !! Undocumented
    real(dp) :: CCini
        !! Undocumented
    real(dp) :: Bini
        !! Undocumented
    real(dp) :: Zrini
        !! Undocumented
    logical :: LinkCropToSimPeriod
        !! Undocumented
    logical :: ResetIniSWC
        !! soil water and salts
    integer(int32) :: InitialStep
        !! Undocumented
    logical :: EvapLimitON
        !! soil evap is before late season stage limited due to sheltering effect of (partly) withered canopy cover
    real(dp) :: EvapWCsurf
        !! remaining water (mm) in surface soil layer for stage 1 evaporation [REW .. 0]
    integer(int8) :: EvapStartStg2
        !! % extra to define upper limit of soil water content at start of stage 2 [100 .. 0]
    real(dp) :: EvapZ
        !! actual soil depth (m) for water extraction by evaporation  [EvapZmin/100 .. EvapZmax/100]
    integer(int32) :: HIfinal
        !! final Harvest Index might be smaller than HImax due to early canopy decline
    integer(int32) :: DelayedDays
        !! delayed days since sowing/planting due to water stress (crop cannot germinate)
    logical :: Germinate
        !! germinate is false when crop cannot germinate due to water stress
    real(dp) :: SumEToStress
        !! Sum ETo during stress period to delay canopy senescence
    real(dp) :: SumGDD
        !! Sum of Growing Degree-days
    real(dp) :: SumGDDfromDay1
        !! Sum of Growing Degree-days since Crop.Day1
    real(sp) :: SCor
        !! correction factor for Crop.SmaxBot if restrictive soil layer inhibit root development
    logical :: MultipleRun
        !! Project with a sequence of simulation runs
    integer(int32) :: NrRuns
        !! Undocumented
    logical :: MultipleRunWithKeepSWC
        !! Project with a sequence of simulation runs and initial SWC is once or more KeepSWC
    real(dp) :: MultipleRunConstZrx
        !! Maximum rooting depth for multiple projects with KeepSWC
    real(dp) :: IrriECw
        !! quality of irrigation water (dS/m)
    integer(int8) :: DayAnaero
        !! number of days under anaerobic conditions
    type(rep_EffectStress) :: EffectStress
        !! effect of soil fertility and salinity stress on CC, WP and KsSto
    logical :: SalinityConsidered
        !! Undocumented
    logical :: ProtectedSeedling
        !! IF protected (before CC = 1.25 CC0), seedling triggering of early senescence is switched off
    logical :: SWCtopSoilConsidered
        !! Top soil is relative wetter than root zone and determines water stresses
    integer(int32) :: LengthCuttingInterval
        !! Default length of cutting interval (days)
    integer(int8) :: YearSeason
        !! year number for perennials (1 = 1st year, 2, 3, 4, max = 127)
    integer(int8) :: RCadj
        !! adjusted relative cover of weeds with self thinning for perennials
    type(rep_storage) :: Storage
        !! Undocumented
    integer(int32) :: YearStartCropCycle
        !! calendar year in which crop cycle starts
    integer(int32) :: CropDay1Previous
        !! previous daynumber at the start of teh crop cycle
end type rep_sim 


type rep_DayEventInt 
    integer(int32) :: DayNr
        !! Undocumented
    integer(int32) :: Param
        !! Undocumented
end type rep_DayEventInt 

type rep_DayEventDbl 
    integer(int32) :: DayNr
        !! Undocumented
    real(dp) :: Param
        !! Undocumented
end type rep_DayEventDbl 

rep_SimulationEventsDbl = ARRAY[1..31] OF Rep_DayEventDbl !! for processing 10-day monthly climatic data

rep_IrriOutSeasonEvents = ARRAY[1..5] OF Rep_DayEventInt

repCriterion = (CumulRain, RainPeriod, RainDecade, RainVsETo)
repAirTCriterion = (TminPeriod,TmeanPeriod,GDDPeriod,CumulGDD)

type rep_Onset 
    logical :: GenerateOn
        !! by rainfall or temperature criterion
    logical :: GenerateTempOn
        !! by temperature criterion
    type(repCriterion) :: Criterion
        !! Undocumented
    type(repAirTCriterion) :: AirTCriterion
        !! Undocumented
    integer(int32) :: StartSearchDayNr, StopSearchDayNr
        !! daynumber
    integer(int32) :: LengthSearchPeriod
        !! days
end type rep_Onset 


type rep_EndSeason 
    integer(int32) :: ExtraYears
        !! to add to YearStartCropCycle
    logical :: GenerateTempOn
        !! by temperature criterion
    type(repAirTCriterion) :: AirTCriterion
        !! Undocumented
    integer(int32) :: StartSearchDayNr, StopSearchDayNr
        !! daynumber
    integer(int32) :: LengthSearchPeriod
        !! days
end type rep_EndSeason 


type rep_PerennialPeriod 
    logical :: GenerateOnset
        !! onset is generated by air temperature criterion
    type(repAirTCriterion) :: OnsetCriterion
        !! Undocumented
    integer(int32) :: OnsetFirstDay, OnsetFirstMonth
        !! Undocumented
    integer(int32) :: OnsetStartSearchDayNr, OnsetStopSearchDayNr
        !! daynumber
    integer(int32) :: OnsetLengthSearchPeriod
        !! days
    real(dp) :: OnsetThresholdValue
        !! degC or degree-days
    integer(int32) :: OnsetPeriodValue
        !! number of successive days
    integer(int8) :: OnsetOccurrence
        !! number of occurrences (1,2 or 3)
    logical :: GenerateEnd
        !! end is generate by air temperature criterion
    type(repAirTCriterion) :: EndCriterion
        !! Undocumented
    integer(int32) :: EndLastDay, EndLastMonth
        !! Undocumented
    integer(int32) :: ExtraYears
        !! number of years to add to the onset year
    integer(int32) :: EndStartSearchDayNr, EndStopSearchDayNr
        !! daynumber
    integer(int32) :: EndLengthSearchPeriod
        !! days
    real(dp) :: EndThresholdValue
        !! degC or degree-days
    integer(int32) :: EndPeriodValue
        !! number of successive days
    integer(int8) :: EndOccurrence
        !! number of occurrences (1,2 or 3)
    integer(int32) :: GeneratedDayNrOnset, GeneratedDayNrEnd
        !! Undocumented
end type rep_PerennialPeriod 


rep_TypeObsSim =(ObsSimCC,ObsSimB,ObsSimSWC)


logical :: DataPath, ObsPath
character(len=STRING_LENGTH) :: TemperatureFile
character(len=STRING_LENGTH) :: TemperatureFileFull, SWCiniFileFull, ProjectFileFull, MultipleProjectFileFull, FullFileNameProgramParameters
character(len=STRING_LENGTH) :: ProfDescription, ClimateDescription, CalendarDescription, CropDescription, ClimDescription, TemperatureDescription, IrriDescription, ManDescription, SWCiniDescription, ProjectDescription, MultipleProjectDescription, OffSeasonDescription, GroundWaterDescription

type(rep_clim) :: ClimRecord, EToRecord, RainRecord, TemperatureRecord
type(rep_sim) :: Simulation
integer(int32) :: IrriFirstDayNr
type(rep_SoilLayer) :: SoilLayer
type(rep_Comp) :: Compartment
integer(int32) :: NrCompartments
type(rep_Crop) :: Crop
real(dp) :: RootingDepth
real(dp) :: CCiActual, CCiPrev, CCiTopEarlySen

integer(int32) :: SenStage
integer(int32) :: DaySubmerged
real(dp) :: ETo, Epot, Tpot, Rain, Irrigation, Infiltrated, CRwater
    !! mm/day
real(dp) :: Tmin, Tmax
    !! degC
real(dp) :: SurfaceStorage, Runoff, Drain, Eact, Tact, TactWeedInfested
    !! mm/day
logical :: EvapoEntireSoilSurface
    !! True of soil wetted by RAIN (false = IRRIGATION and fw < 1)
character(len=STRING_LENGTH) :: OutputName
logical :: PreDay
type(rep_param) :: SimulParam
real(dp) :: Surf0
    !! surface water [mm] begin day
integer(int32) :: NrC, NrD
    !! formats REAL
real(dp) :: MinReal, MaxReal
integer(int32) :: MinInt, MaxInt
type(rep_IrriOutSeasonEvents) :: IrriBeforeSeason, IrriAfterSeason
integer(int32) :: MaxPlotNew
integer(int8) :: MaxPlotTr
type(rep_Onset) :: Onset
type(rep_EndSeason) :: EndSeason
integer(int8) :: IniPercTAW
    !! Default Value for Percentage TAW for Initial Soil Water Content Menu
real(dp) :: ECstorage
    !! salinity
    !! EC surface storage dS/m
real(dp) :: ECdrain
    !! EC drain water dS/m
real(dp) :: SaltInfiltr
    !! salt infiltrated in soil profile Mg/ha
real(dp) :: CRsalt
    !! gram/m2
integer(int32) :: ZiAqua
    !! Depth of Groundwater table below soil surface in centimeter
real(dp) :: ECiAqua
    !! EC of the groundwater table in dS/m
type(rep_PerennialPeriod) :: PerennialPeriod



integer(int8) :: OutputAggregate
    !! Extra for stand alone procedure
character(len=STRING_LENGTH) :: PathNameList, PathNameParam
logical :: Out1Wabal, Out2Crop, Out3Prof, Out4Salt, Out5CompWC, Out6CompEC, Out7Clim, OutDaily, Part1Mult, Part2Eval

repTypeProject = (TypePRO,TypePRM,TypeNone)

real(dp) function ActualRootingDepth(DAP, L0, LZmax, L1234, GDDL0, GDDLZmax, GDDL1234, SumGDD, Zmin, Zmax, ShapeFactor, TypeDays)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: LZmax
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDLZmax
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: Zmin
    real(dp), intent(in) :: Zmax
    integer(int8), intent(in) :: ShapeFactor
    type(rep_modeCycle), intent(in) :: TypeDays
end function ActualRootingDepth

subroutine CalculateETpot(DAP, L0, L12, L123, LHarvest, DayLastCut, CCi, EToVal, KcVal, KcDeclineVal, CCx, CCxWithered, CCeffectProcent, CO2i, GDDayi, TempGDtranspLow, TpotVal, EpotVal)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LHarvest
    integer(int32), intent(in) :: DayLastCut
    real(dp), intent(in) :: CCi
    real(dp), intent(in) :: EToVal
    real(dp), intent(in) :: KcVal
    real(dp), intent(in) :: KcDeclineVal
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CCxWithered
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: TempGDtranspLow
    real(dp), intent(inout) :: TpotVal
    real(dp), intent(inout) :: EpotVal
end subroutine CalculateETpot


subroutine GlobalZero(SumWabal)
    type(rep_sum), intent(inout) :: SumWabal
end subroutine GlobalZero

subroutine TimeToMaxCanopySF(CCo, CGC, CCx, L0, L12, L123, LToFlor, LFlor, DeterminantCrop, L12SF, RedCGC, RedCCx, ClassSF)
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CCx
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LToFlor
    integer(int32), intent(in) :: LFlor
    logical, intent(in) :: DeterminantCrop
    integer(int32), intent(inout) :: L12SF
    integer(int8), intent(inout) :: RedCGC
    integer(int8), intent(inout) :: RedCCx
    integer(int8), intent(inout) :: ClassSF
end subroutine TimeToMaxCanopySF

subroutine NoManagement()

end subroutine NoManagement

subroutine LoadManagement(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadManagement


subroutine NoIrrigation()

end subroutine NoIrrigation

subroutine NoManagementOffSeason()

end subroutine NoManagementOffSeason

subroutine LoadOffSeason(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadOffSeason


subroutine LoadIrriScheduleInfo(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadIrriScheduleInfo

subroutine DetermineNrandThicknessCompartments()

end subroutine DetermineNrandThicknessCompartments

subroutine CalculateAdjustedFC(DepthAquifer, CompartAdj)
    real(dp), intent(in) :: DepthAquifer
    type(rep_Comp), intent(inout) :: CompartAdj
end subroutine CalculateAdjustedFC

subroutine DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_Comp), intent(inout) :: Compartment
end subroutine DesignateSoilLayerToCompartments

subroutine DeclareInitialCondAtFCandNoSalt()

end subroutine DeclareInitialCondAtFCandNoSalt

subroutine specify_soil_layer(NrCompartments, NrSoilLayers, SoilLayer, Compartment, TotalWaterContent)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_SoilLayer), intent(inout) :: SoilLayer
    type(rep_Comp), intent(inout) :: Compartment
    type(rep_Content), intent(inout) :: TotalWaterContent
end subroutine specify_soil_layer


subroutine DetermineParametersCR(SoilClass, KsatMM, aParam, bParam)
    integer(int8), intent(in) :: SoilClass
    real(dp), intent(in) :: KsatMM
    real(dp), intent(inout) :: aParam
    real(dp), intent(inout) :: bParam
end subroutine DetermineParametersCR

integer(int32) function ActiveCells(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ActiveCells

subroutine Calculate_Saltmobility(layer, SaltDiffusion, Macro, Mobil)
    integer(int32), intent(in) :: layer
    integer(int8), intent(in) :: SaltDiffusion
    integer(int8), intent(in) :: Macro
    type(rep_salt), intent(inout) :: Mobil
end subroutine Calculate_Saltmobility

real(dp) function ECeComp(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ECeComp

real(dp) function ECswComp(Comp, atFC)
    type(CompartmentIndividual), intent(in) :: Comp
    logical, intent(in) :: atFC
end function ECswComp

subroutine SaltSolutionDeposit(mm, SaltSolution, SaltDeposit)
    real(dp), intent(in) :: mm
    real(dp), intent(inout) :: SaltSolution
    real(dp), intent(inout) :: SaltDeposit
end subroutine SaltSolutionDeposit

subroutine DetermineSaltContent(ECe, Comp)
    real(dp), intent(in) :: ECe
    type(CompartmentIndividual), intent(inout) :: Comp
end subroutine DetermineSaltContent


subroutine CompleteProfileDescription()

end subroutine CompleteProfileDescription

subroutine LoadProfile(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadProfile


real(dp) function CCiniTotalFromTimeToCCini(TempDaysToCCini, TempGDDaysToCCini, L0, L12, L12SF, L123, L1234, GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, fWeed, TheModeCycle)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    real(dp), intent(in) :: fWeed
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiniTotalFromTimeToCCini



subroutine CompleteCropDescription()

end subroutine CompleteCropDescription

subroutine LoadCrop(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadCrop

subroutine CompleteClimateDescription(ClimateRecord)
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine CompleteClimateDescription

subroutine LoadClimate(FullName, ClimateDescription, TempFile, EToFile, RainFile, CO2File)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    character(len=STRING_LENGTH), intent(inout) :: TempFile
    character(len=STRING_LENGTH), intent(inout) :: EToFile
    character(len=STRING_LENGTH), intent(inout) :: RainFile
    character(len=STRING_LENGTH), intent(inout) :: CO2File
end subroutine LoadClimate

subroutine LoadClim(FullName, ClimateDescription, ClimateRecord)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine LoadClim

subroutine SaveProfile(totalname)
    character(len=STRING_LENGTH), intent(in) :: totalname
end subroutine SaveProfile

subroutine AppendCropFilePerennials(totalname, GenrateTheOnset, GenerateTheEnd, CriterionNrOnset, Day1Onset, Month1Onset, LengthOnset, SuccessiveDaysOnset, OccurrenceOnset, CriterionNrEnd, DayNEnd, MonthNEnd, ExtraYearEnd, LengthEnd, SuccessiveDaysEnd, OccurrenceEnd, ThresholdOnset, ThresholdEnd)
    character(len=STRING_LENGTH), intent(in) :: totalname
    logical, intent(in) :: GenrateTheOnset
    logical, intent(in) :: GenerateTheEnd
    integer(int32), intent(in) :: CriterionNrOnset
    integer(int32), intent(in) :: Day1Onset
    integer(int32), intent(in) :: Month1Onset
    integer(int32), intent(in) :: LengthOnset
    integer(int32), intent(in) :: SuccessiveDaysOnset
    integer(int32), intent(in) :: OccurrenceOnset
    integer(int32), intent(in) :: CriterionNrEnd
    integer(int32), intent(in) :: DayNEnd
    integer(int32), intent(in) :: MonthNEnd
    integer(int32), intent(in) :: ExtraYearEnd
    integer(int32), intent(in) :: LengthEnd
    integer(int32), intent(in) :: SuccessiveDaysEnd
    integer(int32), intent(in) :: OccurrenceEnd
    real(dp), intent(in) :: ThresholdOnset
    real(dp), intent(in) :: ThresholdEnd
end subroutine AppendCropFilePerennials

subroutine SaveCrop(totalname)
    character(len=STRING_LENGTH), intent(in) :: totalname
end subroutine SaveCrop

character(len=STRING_LENGTH) function EndGrowingPeriod(Day1, DayN)
    integer(int32), intent(in) :: Day1
    integer(int32), intent(inout) :: DayN
end function EndGrowingPeriod

subroutine DetermineLinkedSimDay1(CropDay1, SimDay1)
    integer(int32), intent(in) :: CropDay1
    integer(int32), intent(inout) :: SimDay1
end subroutine DetermineLinkedSimDay1

subroutine AdjustCropYearToClimFile(CDay1, CDayN)
    integer(int32), intent(inout) :: CDay1
    integer(int32), intent(inout) :: CDayN
end subroutine AdjustCropYearToClimFile

subroutine AdjustClimRecordTo(CDayN)
    integer(int32), intent(in) :: CDayN
end subroutine AdjustClimRecordTo

subroutine ResetSWCToFC()

end subroutine ResetSWCToFC

subroutine AdjustSimPeriod()

end subroutine AdjustSimPeriod

subroutine AdjustOnsetSearchPeriod()

end subroutine AdjustOnsetSearchPeriod

subroutine SetClimData()

end subroutine SetClimData

subroutine DetermineRootZoneWC(RootingDepth, ZtopSWCconsidered)
    real(dp), intent(in) :: RootingDepth
    logical, intent(inout) :: ZtopSWCconsidered
end subroutine DetermineRootZoneWC

type(repstring17) function DayString(DNr)
    integer(int32), intent(in) :: DNr
end function DayString

real(dp) function CanopyCoverNoStressSF(DAP, L0, L123, LMaturity, GDDL0, GDDL123, GDDLMaturity, CCo, CCx, CGC, CDC, GDDCGC, GDDCDC, SumGDD, TypeDays, SFRedCGC, SFRedCCx)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LMaturity
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDLMaturity
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: SumGDD
    type(rep_modeCycle), intent(in) :: TypeDays
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
end function CanopyCoverNoStressSF


subroutine ReadSoilSettings()

end subroutine ReadSoilSettings

real(dp) function HarvestIndexDay(DAP, DaysToFlower, HImax, dHIdt, CCi, CCxadjusted, PercCCxHIfinal, TempPlanting, PercentLagPhase, HIfinal)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: DaysToFlower
    integer(int32), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt
    real(dp), intent(in) :: CCi
    real(dp), intent(in) :: CCxadjusted
    integer(int8), intent(in) :: PercCCxHIfinal
    type(rep_Planting), intent(in) :: TempPlanting
    integer(int8), intent(inout) :: PercentLagPhase
    integer(int32), intent(inout) :: HIfinal
end function HarvestIndexDay

subroutine ReadRainfallSettings()

end subroutine ReadRainfallSettings

subroutine ReadCropSettingsParameters()

end subroutine ReadCropSettingsParameters

subroutine ReadFieldSettingsParameters()

end subroutine ReadFieldSettingsParameters

subroutine ReadTemperatureSettingsParameters()

end subroutine ReadTemperatureSettingsParameters

real(dp) function AdjustedKsStoToECsw(ECeMin, ECeMax, ResponseECsw, ECei, ECswi, ECswFCi, Wrel, Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, KsStoIN)
    integer(int8), intent(in) :: ECeMin
    integer(int8), intent(in) :: ECeMax
    integer(int32), intent(in) :: ResponseECsw
    real(dp), intent(in) :: ECei
    real(dp), intent(in) :: ECswi
    real(dp), intent(in) :: ECswFCi
    real(dp), intent(in) :: Wrel
    real(dp), intent(in) :: Coeffb0Salt
    real(dp), intent(in) :: Coeffb1Salt
    real(dp), intent(in) :: Coeffb2Salt
    real(dp), intent(in) :: KsStoIN
end function AdjustedKsStoToECsw


subroutine DetermineRootZoneSaltContent(RootingDepth, ZrECe, ZrECsw, ZrECswFC, ZrKsSalt)
    real(dp), intent(in) :: RootingDepth
    real(dp), intent(inout) :: ZrECe
    real(dp), intent(inout) :: ZrECsw
    real(dp), intent(inout) :: ZrECswFC
    real(dp), intent(inout) :: ZrKsSalt
end subroutine DetermineRootZoneSaltContent

real(dp) function CO2ForSimulationPeriod(FromDayNr, ToDayNr)
    integer(int32), intent(in) :: FromDayNr
    integer(int32), intent(in) :: ToDayNr
end function CO2ForSimulationPeriod


real(dp) function CCiNoWaterStressSF(Dayi, L0, L12SF, L123, L1234, GDDL0, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, SumGDD, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, TheModeCycle)
    integer(int32), intent(in) :: Dayi
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiNoWaterStressSF

real(dp) function SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini, L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, CO2i, TheModeCycle)
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
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: CO2i
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function SeasonalSumOfKcPot

subroutine TranslateIniLayersToSWProfile(NrLay, LayThickness, LayVolPr, LayECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLay
    type(rep_IniComp), intent(in) :: LayThickness
    type(rep_IniComp), intent(in) :: LayVolPr
    type(rep_IniComp), intent(in) :: LayECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniLayersToSWProfile


subroutine TranslateIniPointsToSWProfile(NrLoc, LocDepth, LocVolPr, LocECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLoc
    type(rep_IniComp), intent(in) :: LocDepth
    type(rep_IniComp), intent(in) :: LocVolPr
    type(rep_IniComp), intent(in) :: LocECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniPointsToSWProfile

subroutine LoadInitialConditions(SWCiniFileFull, IniSurfaceStorage, IniSWCRead)
    character(len=STRING_LENGTH), intent(in) :: SWCiniFileFull
    real(dp), intent(inout) :: IniSurfaceStorage
    type(rep_IniSWC), intent(inout) :: IniSWCRead
end subroutine LoadInitialConditions

subroutine LoadProjectDescription(FullNameProjectFile, DescriptionOfProject)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    character(len=STRING_LENGTH), intent(inout) :: DescriptionOfProject
end subroutine LoadProjectDescription

subroutine ComposeOutputFileName(TheProjectFileName)
    character(len=STRING_LENGTH), intent(in) :: TheProjectFileName
end subroutine ComposeOutputFileName

subroutine CheckForKeepSWC(FullNameProjectFile, TotalNrOfRuns, RunWithKeepSWC, ConstZrxForRun)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    integer(int32), intent(in) :: TotalNrOfRuns
    logical, intent(inout) :: RunWithKeepSWC
    real(dp), intent(inout) :: ConstZrxForRun
end subroutine CheckForKeepSWC

subroutine AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
    integer(int8), intent(in) :: PrevNrComp
    type(rep_IniComp), intent(in) :: PrevThickComp
    type(rep_IniComp), intent(in) :: PrevVolPrComp
    type(rep_IniComp), intent(in) :: PrevECdSComp
end subroutine AdjustThetaInitial

subroutine AdjustSizeCompartments(CropZx)
    real(dp), intent(in) :: CropZx
end subroutine AdjustSizeCompartments

subroutine CheckForWaterTableInProfile(DepthGWTmeter, ProfileComp, WaterTableInProfile)
    real(dp), intent(in) :: DepthGWTmeter
    type(rep_comp), intent(in) :: ProfileComp
    logical, intent(inout) :: WaterTableInProfile
end subroutine CheckForWaterTableInProfile

subroutine LoadGroundWater(FullName, AtDayNr, Zcm, ECdSm)
    character(len=STRING_LENGTH), intent(in) :: FullName
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine LoadGroundWater

real(dp) function CCmultiplierWeedAdjusted(ProcentWeedCover, CCxCrop, FshapeWeed, fCCx, Yeari, MWeedAdj, RCadj)
    integer(int8), intent(in) :: ProcentWeedCover
    real(dp), intent(in) :: CCxCrop
    real(dp), intent(in) :: FshapeWeed
    real(dp), intent(in) :: fCCx
    integer(int8), intent(in) :: Yeari
    integer(int8), intent(in) :: MWeedAdj
    integer(int8), intent(inout) :: RCadj
end function CCmultiplierWeedAdjusted


subroutine AdjustYearPerennials(TheYearSeason, Sown1stYear, TheCycleMode, Zmax, ZminYear1, TheCCo, TheSizeSeedling, TheCGC, TheCCx, TheGDDCGC, ThePlantingDens, TypeOfPlanting, Zmin, TheSizePlant, TheCCini, TheDaysToCCini, TheGDDaysToCCini)
    integer(int8), intent(in) :: TheYearSeason
    logical, intent(in) :: Sown1stYear
    type(rep_modeCycle), intent(in) :: TheCycleMode
    real(dp), intent(in) :: Zmax
    real(dp), intent(in) :: ZminYear1
    real(dp), intent(in) :: TheCCo
    real(dp), intent(in) :: TheSizeSeedling
    real(dp), intent(in) :: TheCGC
    real(dp), intent(in) :: TheCCx
    real(dp), intent(in) :: TheGDDCGC
    integer(int32), intent(in) :: ThePlantingDens
    type(rep_Planting), intent(inout) :: TypeOfPlanting
    real(dp), intent(inout) :: Zmin
    real(dp), intent(inout) :: TheSizePlant
    real(dp), intent(inout) :: TheCCini
    integer(int32), intent(inout) :: TheDaysToCCini
    integer(int32), intent(inout) :: TheGDDaysToCCini
end subroutine AdjustYearPerennials






subroutine NoCropCalendar()

end subroutine NoCropCalendar

subroutine LoadCropCalendar(FullName, GetOnset, GetOnsetTemp, DayNrStart, YearStart)
    character(len=STRING_LENGTH), intent(in) :: FullName
    logical, intent(inout) :: GetOnset
    logical, intent(inout) :: GetOnsetTemp
    integer(int32), intent(inout) :: DayNrStart
    integer(int32), intent(in) :: YearStart
end subroutine LoadCropCalendar

subroutine GetFileForProgramParameters(TheFullFileNameProgram, FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: TheFullFileNameProgram
    character(len=STRING_LENGTH), intent(inout) :: FullFileNameProgramParameters
end subroutine GetFileForProgramParameters

subroutine LoadProgramParametersProject(FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: FullFileNameProgramParameters
end subroutine LoadProgramParametersProject


IMPLEMENTATION


real(dp) function ActualRootingDepth(DAP, L0, LZmax, L1234, GDDL0, GDDLZmax, GDDL1234, SumGDD, Zmin, Zmax, ShapeFactor, TypeDays)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: LZmax
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDLZmax
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: Zmin
    real(dp), intent(in) :: Zmax
    integer(int8), intent(in) :: ShapeFactor
    type(rep_modeCycle), intent(in) :: TypeDays
end function ActualRootingDepth

real(dp) :: Zini, Zr
integer(int32) :: VirtualDay, T0



real(dp) function ActualRootingDepthDays(DAP, L0, LZmax, L1234, Zmin, Zmax)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: LZmax
    integer(int32), intent(in) :: L1234
    real(dp), intent(in) :: Zmin
    real(dp), intent(in) :: Zmax
end function ActualRootingDepthDays

! Actual rooting depth at the end of Dayi
VirtualDay = DAP - Simulation%DelayedDays
if ((VirtualDay < 1) .or. (VirtualDay > L1234)) then
    ActualRootingDepthDays = 0
elseif (VirtualDay >= LZmax) then
    ActualRootingDepthDays = Zmax
elseif (Zmin < Zmax) then
    Zini = ZMin * (SimulParam%RootPercentZmin/100)
    T0 = nint(L0/2)
    if (LZmax <= T0) then
        Zr = Zini + (Zmax-Zini)*VirtualDay/LZmax
    elseif (VirtualDay <= T0) then
        Zr = Zini
    else
        Zr = Zini + (Zmax-Zini)
        * TimeRootFunction(VirtualDay, ShapeFactor, LZmax, T0)
    end if
    if (Zr > ZMin) then
        ActualRootingDepthDays = Zr
    else
        ActualRootingDepthDays = ZMin
    end if
else
    ActualRootingDepthDays = ZMax
end if
! ActualRootingDepthDays 



real(dp) function ActualRootingDepthGDDays(DAP, L1234, GDDL0, GDDLZmax, GDDL1234, SumGDD, Zmin, Zmax)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDLZmax
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: Zmin
    real(dp), intent(in) :: Zmax
end function ActualRootingDepthGDDays

real(dp) :: GDDT0
! after sowing the crop has roots even when SumGDD = 0
VirtualDay = DAP - Simulation%DelayedDays
if ((VirtualDay < 1) .or. (VirtualDay > L1234)) then
    ActualRootingDepthGDDays = 0
elseif (SumGDD >= GDDLZmax) then
    ActualRootingDepthGDDays = Zmax
elseif (Zmin < Zmax) then
    Zini = ZMin * (SimulParam%RootPercentZmin/100)
    GDDT0 = GDDL0/2
    if (GDDLZmax <= GDDT0) then
        Zr = Zini + (Zmax-Zini)*SumGDD/GDDLZmax
    else
        if (SumGDD <= GDDT0) then
            Zr = Zini
        else
            Zr = Zini + (Zmax-Zini)
            * TimeRootFunction(SumGDD, ShapeFactor, GDDLZmax, GDDT0)
        end if
    end if
    if (Zr > ZMin) then
        ActualRootingDepthGDDays = Zr
    else
        ActualRootingDepthGDDays = ZMin
    end if
else
    ActualRootingDepthGDDays = ZMax
end if
! ActualRootingDepthGDDays 



! ActualRootingDepth 
CASE TypeDays OF
    GDDays  : Zr = ActualRootingDepthGDDays(DAP, L1234, GDDL0, GDDLZmax, GDDL1234, SumGDD, Zmin, Zmax)
end case
else
Zr = ActualRootingDepthDays(DAP, L0, LZmax, L1234, Zmin, Zmax)
! restrictive soil layer
Simulation%SCor = 1
if (nint(GetSoil()%RootMax*1000) < nint(Zmax*1000)) then
    ZrAdjustedToRestrictiveLayers(Zr, GetSoil()%NrSoilLayers, SoilLayer, Zr)
end if
! assign
ActualRootingDepth= Zr
! ActualRootingDepth 




subroutine CalculateETpot(DAP, L0, L12, L123, LHarvest, DayLastCut, CCi, EToVal, KcVal, KcDeclineVal, CCx, CCxWithered, CCeffectProcent, CO2i, GDDayi, TempGDtranspLow, TpotVal, EpotVal)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LHarvest
    integer(int32), intent(in) :: DayLastCut
    real(dp), intent(in) :: CCi
    real(dp), intent(in) :: EToVal
    real(dp), intent(in) :: KcVal
    real(dp), intent(in) :: KcDeclineVal
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CCxWithered
    real(dp), intent(in) :: CCeffectProcent
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: TempGDtranspLow
    real(dp), intent(inout) :: TpotVal
    real(dp), intent(inout) :: EpotVal
end subroutine CalculateETpot

real(dp) :: EpotMin, EpotMax, CCiAdjusted, Multiplier, KsTrCold
integer(int32) :: VirtualDay

! CalculateETpot
VirtualDay = DAP - Simulation%DelayedDays
if ( ((VirtualDay < L0) .and. (Round(100*CCi) = 0)) .or. (VirtualDay > LHarvest)) then ! To handlle Forage crops: Round(100*CCi) = 0
    TpotVal = 0
    EpotVal = SimulParam%KcWetBare*EToVal
else
    ! Correction for micro-advection 
    CCiAdjusted = 1.72*CCi - 1*(CCi*CCi) + 0.30*(CCi*CCi*CCi)
    if (CCiAdjusted < 0) then
        CCiAdjusted = 0
    end if
    if (CCiAdjusted > 1) then
        CCiAdjusted = 1
    end if
    
    ! Correction for ageing effects - is a function of calendar days 
    ! IF (VirtualDay > (L12+5)) THEN KcVal := KcVal - (VirtualDay-(L12+5))*(KcDeclineVal/100)*CCxWithered;
    if ((VirtualDay-DayLastCut) > (L12+5)) then
        KcVal = KcVal - (VirtualDay-DayLastCut-(L12+5))*(KcDeclineVal/100)*CCxWithered
    end if
    
    ! Correction for elevated atmospheric CO2 concentration 
    if (CO2i > 369.41) then
        KcVal = KcVal * (1 - 0.05 * (CO2i-369.41)/(550-369.41))
    end if
    
    ! Correction for Air temperature stress 
    if ((CCiAdjusted <= 0.0000001) .or. (nint(GDDayi) < 0)) then
        KsTrCold = 1
    else
        KsTrCold = KsTemperature((0), TempGDtranspLow, GDDayi)
    end if
    
    ! First estimate of Epot and Tpot 
    TpotVal = CCiAdjusted * KsTrCold * KcVal * EToVal
    EpotVal = SimulParam%KcWetBare * (1 - CCiAdjusted) * EToVal
    
    ! Maximum Epot with withered canopy as a result of (early) senescence
    EpotMax = SimulParam%KcWetBare * EToVal * (1 - CCxWithered * CCEffectProcent/100)
    
    ! Correction Epot for dying crop in late-season stage 
    if ((VirtualDay > L123) .and. (CCx > 0)) then
        if (CCi > (CCx/2)) then
            ! not yet full effect 
            if (CCi > CCx) then
                Multiplier = 0  ! no effect
            else
                Multiplier = (CCx-CCi)/(CCx/2)
            end if
        else
            Multiplier = 1 ! full effect
        end if
        EpotVal = EpotVal * (1 - CCx * (CCEffectProcent/100) * Multiplier)
        EpotMin = SimulParam%KcWetBare * (1 - 1.72*CCx + 1*(CCx*CCx) - 0.30*(CCx*CCx*CCx)) * EToVal
        if (EpotMin < 0) then
            EpotMin = 0
        end if
        if (EpotVal < EpotMin) then
            EpotVal = EpotMin
        end if
        if (EpotVal > EpotMax) then
            EpotVal = EpotMax
        end if
    end if
    
    ! Correction for canopy senescence before late-season stage 
    if (Simulation%EvapLimitON then
        if (EpotVal > EpotMax) then
            EpotVal = EpotMax
        end if
        
        ! Correction for drop in photosynthetic capacity of a dying green canopy 
        if (CCi < CCxWithered) then
            if (CCxWithered > 0.01) .and. (CCi > 0.001) then
                TpotVal = TpotVal * exp(SimulParam%ExpFsen*Ln(CCi/CCxWithered))
            end if
        end if
    end if
    ! CalculateETpot 
end if



subroutine GlobalZero(SumWabal)
    type(rep_sum), intent(inout) :: SumWabal
end subroutine GlobalZero

integer(int32) :: i
WITH SumWabal
Epot = 0.0
Tpot = 0.0
Rain = 0.0
Irrigation = 0.0
Infiltrated = 0.0
Runoff = 0.0
Drain = 0.0
Eact = 0.0
Tact = 0.0
TrW = 0.0
ECropCycle = 0.0
Biomass = 0
BiomassPot = 0
BiomassUnlim = 0
BiomassTot = 0 ! crop and weeds (for soil fertility stress)
YieldPart = 0
SaltIn = 0
SaltOut = 0
CRwater = 0
CRsalt = 0
SetTotalWaterContent_BeginDay(0)
do i =1, NrCompartments
    SetTotalWaterContent_BeginDay(GetTotalWaterContent()%BeginDay
    + Compartment(i)%theta*1000*Compartment(i)%Thickness)
end do
! GlobalZero 




subroutine TimeToMaxCanopySF(CCo, CGC, CCx, L0, L12, L123, LToFlor, LFlor, DeterminantCrop, L12SF, RedCGC, RedCCx, ClassSF)
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CCx
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LToFlor
    integer(int32), intent(in) :: LFlor
    logical, intent(in) :: DeterminantCrop
    integer(int32), intent(inout) :: L12SF
    integer(int8), intent(inout) :: RedCGC
    integer(int8), intent(inout) :: RedCCx
    integer(int8), intent(inout) :: ClassSF
end subroutine TimeToMaxCanopySF

real(dp) :: CCToReach
integer(int32) :: L12SFmax
if ((ClassSF = 0) .or. ((RedCCx = 0) .and. (RedCGC = 0))) then
    L12SF = L12
else
    CCToReach = 0.98*(1-RedCCX/100)*CCx
    L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100)*CCx), (CGC*(1-(RedCGC)/100)), L0)
    ! determine L12SFmax
    if (DeterminantCrop then
        L12SFmax = LToFlor + nint(LFlor/2)
    else
        L12SFmax = L123
    end if
    ! check for L12SFmax
    if (L12SF > L12SFmax) then
        ! full canopy cannot be reached in potential period for vegetative growth
        ! ClassSF := undef_int; ! swithc to user defined soil fertility
        ! 1. increase CGC(soil fertility)
        do while ((L12SF > L12SFmax) .and. (RedCGC > 0))
            RedCGC = RedCGC - 1
            L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100)*CCx), (CGC*(1-(RedCGC)/100)), L0)
        end do
        ! 2. if not sufficient decrease CCx(soil fertility)
        do while ((L12SF > L12SFmax) .and. ( ((1-RedCCX/100)*CCx) > 0.10) .and. (RedCCx <= 50))
            RedCCx = RedCCx + 1
            CCToReach = 0.98*(1-RedCCX/100)*CCx
            L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100)*CCx), (CGC*(1-(RedCGC)/100)), L0)
        end do
    end do
end do
! TimeToMaxCanopySF 



subroutine NoManagement()

end subroutine NoManagement

ManDescription = 'No specific field management'
! mulches
SetManagement_Mulch(0)
SetManagement_EffectMulchInS(50)
! soil fertility
SetManagement_FertilityStress(0)
CropStressParametersSoilFertility(Crop%StressResponse, GetManagement_FertilityStress(), Simulation%EffectStress)
! soil bunds
SetManagement_BundHeight(0)
Simulation%SurfaceStorageIni = 0.0
Simulation%ECStorageIni = 0.0
! surface run-off
SetManagement_RunoffOn(true)
SetManagement_CNcorrection(0)
! weed infestation
SetManagement_WeedRC(0)
SetManagement_WeedDeltaRC(0)
SetManagement_WeedShape(- 0.01)
SetManagement_WeedAdj(100)
! multiple cuttings
SetManagement_Cuttings_Considered(false)
SetManagement_Cuttings_CCcut(30)
SetManagement_Cuttings_CGCPlus(20)
SetManagement_Cuttings_Day1(1)
SetManagement_Cuttings_NrDays(undef_int)
SetManagement_Cuttings_Generate(false)
SetManagement_Cuttings_Criterion(NA)
SetManagement_Cuttings_HarvestEnd(false)
SetManagement_Cuttings_FirstDayNr(undef_int)
! NoManagement 


subroutine LoadManagement(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadManagement

type(TextFile) :: f0
integer(int8) :: i
real(dp) :: VersionNr
integer(int8) :: TempShortInt
integer(int32) :: TempInt
real(dp) :: TempDouble
Assign(f0, FullName)
Reset(f0)
READLN(f0, ManDescription)
READLN(f0, VersionNr) ! AquaCrop Version
! mulches
READLN(f0, TempShortInt)
SetManagement_Mulch(TempShortInt)
READLN(f0, TempShortInt)
SetManagement_EffectMulchInS(TempShortInt)
! soil fertility
READLN(f0, TempShortInt) ! effect is crop specific
SetManagement_FertilityStress(TempShortInt)
CropStressParametersSoilFertility(Crop%StressResponse, GetManagement_FertilityStress(), Simulation%EffectStress)
! soil bunds
READLN(f0, TempDouble)
SetManagement_BundHeight(TempDouble)
Simulation%SurfaceStorageIni = 0.0
Simulation%ECStorageIni = 0.0
! surface run-off
READLN(f0, i)
if (i = 1) then
    SetManagement_RunoffON(false)   ! prevention of surface runoff
else
    SetManagement_RunoffON(true)   ! surface runoff is not prevented
end if
if (nint(VersionNr*10) < 50) then ! UPDATE required for CN adjustment
    SetManagement_CNcorrection(0)
else
    READLN(f0, TempInt) ! project increase/decrease of CN
    SetManagement_CNcorrection(TempInt)
end if
! weed infestation
if (nint(VersionNr*10) < 50) then ! UPDATE required for Version 3.0, 3.1 and 4.0
    SetManagement_WeedRC(0) ! relative cover of weeds (%)
    SetManagement_WeedDeltaRC(0)
    SetManagement_WeedShape(-0.01) ! shape factor of the CC expansion fucntion in a weed infested field
else
    READLN(f0, TempShortInt) ! relative cover of weeds (%)
    SetManagement_WeedRC(TempShortInt)
    if (nint(VersionNr*10) < 51) then
        SetManagement_WeedDeltaRC(0)
    else
        READLN(f0, TempInt)
        SetManagement_WeedDeltaRC(TempInt)
    end if
    READLN(f0, TempDouble) ! shape factor of the CC expansion fucntion in a weed infested field
    SetManagement_WeedShape(TempDouble)
end if
if (nint(VersionNr*10) < 70) then ! UPDATE required for versions below 7
    SetManagement_WeedAdj(100) ! replacement (%) by weeds of the self-thinned part of the Canopy Cover - only for perennials
else
    READLN(f0, TempShortInt)
    SetManagement_WeedAdj(TempShortInt)
end if
! multiple cuttings
if (nint(VersionNr*10) >= 70) then ! UPDATE required for multiple cuttings
    READLN(f0, i)  ! Consider multiple cuttings: True or False
    if (i = 0) then
        SetManagement_Cuttings_Considered(false)
    else
        SetManagement_Cuttings_Considered(true)
    end if
    READLN(f0, TempInt)  ! Canopy cover (%) after cutting
    SetManagement_Cuttings_CCcut(TempInt)
    READLN(f0, TempInt) ! Increase (percentage) of CGC after cutting
    SetManagement_Cuttings_CGCPlus(TempInt)
    READLN(f0, TempInt)  ! Considered first day when generating cuttings (1 = start of growth cycle)
    SetManagement_Cuttings_Day1(TempInt)
    READLN(f0, TempInt)  ! Considered number owhen generating cuttings (-9 = total growth cycle)
    SetManagement_Cuttings_NrDays(TempInt)
    READLN(f0, i)  ! Generate multiple cuttings: True or False
    if (i = 1) then
        SetManagement_Cuttings_Generate(true)
    else
        SetManagement_Cuttings_Generate(false)
    end if
    READLN(f0, i)  ! Time criterion for generating cuttings
    CASE i OF
        0  : SetManagement_Cuttings_Criterion(NA) ! not applicable
    end case
    1  : SetManagement_Cuttings_Criterion(IntDay) ! interval in days
    2  : SetManagement_Cuttings_Criterion(IntGDD) ! interval in Growing Degree Days
    3  : SetManagement_Cuttings_Criterion(DryB) ! produced dry above ground biomass (ton/ha)
    4  : SetManagement_Cuttings_Criterion(DryY) ! produced dry yield (ton/ha)
    5  : SetManagement_Cuttings_Criterion(FreshY) ! produced fresh yield (ton/ha)
else
    SetManagement_Cuttings_Criterion(NA) ! not applicable
end case
READLN(f0, i)  ! final harvest at crop maturity: True or False (When generating cuttings)
if (i = 1) then
    SetManagement_Cuttings_HarvestEnd(true)
else
    SetManagement_Cuttings_HarvestEnd(false)
end if
READLN(f0, TempInt) ! dayNr for Day 1 of list of cuttings (-9 = Day1 is start growing cycle)
SetManagement_Cuttings_FirstDayNr(TempInt)
else
SetManagement_Cuttings_Considered(false)
SetManagement_Cuttings_CCcut(30)
SetManagement_Cuttings_CGCPlus(20)
SetManagement_Cuttings_Day1(1)
SetManagement_Cuttings_NrDays(undef_int)
SetManagement_Cuttings_Generate(false)
SetManagement_Cuttings_Criterion(NA)
SetManagement_Cuttings_HarvestEnd(false)
SetManagement_Cuttings_FirstDayNr(undef_int)
Close(f0)
! LoadManagement 






subroutine NoIrrigation()

end subroutine NoIrrigation

integer(int32) :: Nri
SetIrriMode(NoIrri)
IrriDescription = 'Rainfed cropping'
SetIrriMethod(MSprinkler)
Simulation%IrriECw = 0.0 ! dS/m
SetGenerateTimeMode(AllRAW)
SetGenerateDepthMode(ToFC)
IrriFirstDayNr = undef_int
do Nri = 1, 5
    IrriBeforeSeason(Nri)%DayNr = 0
    IrriBeforeSeason(Nri)%Param = 0
    IrriAfterSeason(Nri)%DayNr = 0
    IrriAfterSeason(Nri)%Param = 0
end do
SetIrriECw_PreSeason(0.0) ! dS/m
SetIrriECw_PostSeason(0.0) ! dS/m
! NoIrrigation 


subroutine NoManagementOffSeason()

end subroutine NoManagementOffSeason

integer(int32) :: Nri
OffSeasonDescription = 'No specific off-season conditions'
! mulches
SetManagement_SoilCoverBefore(0)
SetManagement_SoilCoverAfter(0)
SetManagement_EffectMulchOffS(50)
! off-season irrigation
SimulParam%IrriFwOffSeason = 100
SetIrriECw_PreSeason(0.0) ! dS/m
do Nri = 1, 5
    IrriBeforeSeason(Nri)%DayNr = 0
    IrriBeforeSeason(Nri)%Param = 0
end do
SetIrriECw_PostSeason(0.0) ! dS/m
do Nri = 1, 5
    IrriAfterSeason(Nri)%DayNr = 0
    IrriAfterSeason(Nri)%Param = 0
end do
! NoManagementOffSeason 



subroutine LoadOffSeason(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadOffSeason

type(TextFile) :: f0
integer(int32) :: Nri, NrEvents1, NrEvents2
character(len=STRING_LENGTH) :: ParamString
real(dp) :: Par1, Par2
real(dp) :: VersionNr
real(dp) :: PreSeason_in
real(dp) :: PostSeason_in
integer(int8) :: TempShortInt
Assign(f0, FullName)
Reset(f0)
READLN(f0, OffSeasonDescription)
READLN(f0, VersionNr) ! AquaCrop Version
! mulches
READLN(f0, TempShortInt)
SetManagement_SoilCoverBefore(TempShortInt)
READLN(f0, TempShortInt)
SetManagement_SoilCoverAfter(TempShortInt)
READLN(f0, TempShortInt)
SetManagement_EffectMulchOffS(TempShortInt)

! irrigation events - initialise
do Nri = 1, 5
    IrriBeforeSeason(Nri)%DayNr = 0
    IrriBeforeSeason(Nri)%Param = 0
    IrriAfterSeason(Nri)%DayNr = 0
    IrriAfterSeason(Nri)%Param = 0
end do
READLN(f0, NrEvents1) ! number of irrigation events BEFORE growing period
if (nint(10*VersionNr) < 32) then ! irrigation water quality BEFORE growing period
    SetIrriECw_PreSeason(0.0)
else
    READLN(f0, PreSeason_in)
    SetIrriECw_PreSeason(PreSeason_in)
end if
READLN(f0, NrEvents2) ! number of irrigation events AFTER growing period
if (nint(10*VersionNr) < 32) then ! irrigation water quality AFTER growing period
    SetIrriECw_PostSeason(0.0)
else
    READLN(f0, PostSeason_in)
    SetIrriECw_PostSeason(PostSeason_in)
end if
READLN(f0, SimulParam%IrriFwOffSeason) ! percentage of soil surface wetted
! irrigation events - get events before and after season
if (NrEvents1 > 0) .or. (NrEvents2 > 0) then
    do Nri = 1, 3
        READLN(f0) ! title
    end do
    if (NrEvents1 > 0) then
        do Nri = 1, NrEvents1
            ! events BEFORE growing period
            READLN(f0, ParamString)
            SplitStringInTwoParams(ParamString, Par1, Par2)
            IrriBeforeSeason(Nri)%DayNr = nint(Par1)
            IrriBeforeSeason(Nri)%Param = nint(Par2)
        end do
        if (NrEvents2 > 0) then
            do Nri = 1, NrEvents2
                ! events AFTER growing period
                READLN(f0, ParamString)
                SplitStringInTwoParams(ParamString, Par1, Par2)
                IrriAfterSeason(Nri)%DayNr = nint(Par1)
                IrriAfterSeason(Nri)%Param = nint(Par2)
            end do
            Close(f0)
        end do
        ! LoadOffSeason 
    end do


subroutine LoadIrriScheduleInfo(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadIrriScheduleInfo

type(TextFile) :: f0
integer(int32) :: i
real(dp) :: VersionNr

Assign(f0, FullName)
Reset(f0)
READLN(f0, IrriDescription)
READLN(f0, VersionNr)  ! AquaCrop version

! irrigation method
READLN(f0, i)
CASE i OF
    1 : SetIrriMethod(MSprinkler)
end case
2 : SetIrriMethod(MBasin)
3 : SetIrriMethod(MBorder)
4 : SetIrriMethod(MFurrow)
else
SetIrriMethod(MDrip)

! fraction of soil surface wetted
READLN(f0, SimulParam%IrriFwInSeason)

! irrigation mode and parameters
READLN(f0, i)
CASE i OF
    0 : SetIrriMode(NoIrri) ! rainfed
end case
1 : SetIrriMode(Manual)
2 : SetIrriMode(Generate)
else
SetIrriMode(Inet)

! 1. Irrigation schedule
if ((i = 1) .and. (nint(VersionNr*10) >= 70)) then
    READLN(f0, IrriFirstDayNr) ! line 6
else
    IrriFirstDayNr = undef_int ! start of growing period
end if


! 2. Generate
if (GetIrriMode() = Generate) then
    READLN(f0, i) ! time criterion
    Case i OF
    1 : SetGenerateTimeMode(FixInt)
    2 : SetGenerateTimeMode(AllDepl)
    3 : SetGenerateTimeMode(AllRAW)
    4 : SetGenerateTimeMode(WaterBetweenBunds)
else
    SetGenerateTimeMode(AllRAW)
end if
READLN(f0, i) ! depth criterion
Case i OF
1 : SetGenerateDepthMode(ToFc)
else
SetGenerateDepthMode(FixDepth)
IrriFirstDayNr = undef_int ! start of growing period

! 3. Net irrigation requirement
if (GetIrriMode() = Inet) then
    READLN(f0, SimulParam%PercRAW)
    IrriFirstDayNr = undef_int  ! start of growing period
end if

Close(f0)
! LoadIrriScheduleInfo 


subroutine DetermineNrandThicknessCompartments()

end subroutine DetermineNrandThicknessCompartments

real(dp) :: TotalDepthL, TotalDepthC, DeltaZ
integer(int32) :: i
TotalDepthL = 0
do i = 1, GetSoil()%NrSoilLayers
    TotalDepthL = TotalDepthL + SoilLayer(i)%Thickness
end do
TotalDepthC = 0
NrCompartments = 0
REPEAT
DeltaZ = (TotalDepthL - TotalDepthC)
NrCompartments = NrCompartments + 1
if (DeltaZ > SimulParam%CompDefThick) then
    Compartment(NrCompartments)%Thickness = SimulParam%CompDefThick
else
    Compartment(NrCompartments)%Thickness = DeltaZ
end if
TotalDepthC = TotalDepthC + Compartment(NrCompartments)%Thickness
UNTIL ((NrCompartments = max_No_compartments) .or. (Abs(TotalDepthC - TotalDepthL) < 0.0001))
! DetermineNrandThicknessCompartments 


subroutine CalculateAdjustedFC(DepthAquifer, CompartAdj)
    real(dp), intent(in) :: DepthAquifer
    type(rep_Comp), intent(inout) :: CompartAdj
end subroutine CalculateAdjustedFC

integer(int32) :: compi, ic
real(dp) :: Zi, Depth, DeltaV, DeltaFC, Xmax

real(dp) function NoAdjustment(FCvolPr)
    real(dp), intent(in) :: FCvolPr
end function NoAdjustment

real(dp) :: pF
if (FCvolPr <= 10) then
    NoAdjustment = 1
else
    if (FCvolPr >= 30) then
        NoAdjustment = 2
    else
        pF = 2 + 0.3 * (FCvolPr-10)/20
        NoAdjustment = (exp(pF*log(10)))/100
    end if
end if
! NoAdjustment 

!
! Depth := 0;
! FOR compi := 1 TO NrCompartments DO
!     Depth := Depth + CompartAdj[compi].Thickness;
!     Zi := Depth - CompartAdj[compi].Thickness/2;
!     IF ((DepthAquifer < 0)
!         OR ((DepthAquifer - Zi) >= 2)
!         OR (SoilLayer[CompartAdj[compi].Layer].FC >= SoilLayer[CompartAdj[compi].Layer].SAT)) THEN
!         CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC
!     ELSE
!         IF (Zi >= DepthAquifer) THEN
!             CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].SAT
!         ELSE
!             DeltaV := SoilLayer[CompartAdj[compi].Layer].SAT - SoilLayer[CompartAdj[compi].Layer].FC;
!             DeltaFC := (DeltaV/4) * (Zi - (DepthAquifer - 2)) * (Zi - (DepthAquifer - 2));
!             CompartAdj[compi].FCadj := SoilLayer[CompartAdj[compi].Layer].FC + DeltaFC;
!         end if
!     end if
!     ;  *)
    
    
    Depth = 0
end if
do compi = 1, NrCompartments
    Depth = Depth + CompartAdj(compi)%Thickness
end do
compi = NrCompartments
REPEAT
Zi = Depth - CompartAdj(compi)%Thickness/2
! Xmax := NoAdjustment(SoilLayer[CompartAdj[compi].Layer].SoilClass);
Xmax = NoAdjustment(SoilLayer(CompartAdj(compi)%Layer)%FC)
if ((DepthAquifer < 0) .or. ((DepthAquifer - Zi) >= Xmax)) then
    do ic = 1, compi
        CompartAdj(ic)%FCadj = SoilLayer(CompartAdj(ic)%Layer)%FC
    end do
    compi = 0
else
    if (SoilLayer(CompartAdj(compi)%Layer)%FC >= SoilLayer(CompartAdj(compi)%Layer)%SAT) then
        CompartAdj(compi)%FCadj = SoilLayer(CompartAdj(compi)%Layer)%FC
    else
        if (Zi >= DepthAquifer) then
            CompartAdj(compi)%FCadj = SoilLayer(CompartAdj(compi)%Layer)%SAT
        else
            DeltaV = SoilLayer(CompartAdj(compi)%Layer)%SAT - SoilLayer(CompartAdj(compi)%Layer)%FC
            DeltaFC = (DeltaV/Sqr(Xmax)) * Sqr(Zi - (DepthAquifer - Xmax))
            CompartAdj(compi)%FCadj = SoilLayer(CompartAdj(compi)%Layer)%FC + DeltaFC
        end if
    end if
    Depth = Depth - CompartAdj(compi)%Thickness
    compi = compi - 1
end if
UNTIL (compi < 1)
!  CalculateAdjustedFC 


subroutine DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_Comp), intent(inout) :: Compartment
end subroutine DesignateSoilLayerToCompartments

integer(int32) :: i, layeri, compi
real(dp) :: depth, depthi
logical :: finished, NextLayer
depth = 0
depthi = 0
layeri = 1
compi = 1
REPEAT
depth = depth + SoilLayer(layeri)%Thickness
REPEAT
depthi = depthi + Compartment(compi)%Thickness/2
if (depthi <= depth) then
    Compartment(compi)%Layer = layeri
    NextLayer = .false.
    depthi = depthi + Compartment(compi)%Thickness/2
    compi = compi + 1
    finished = (compi > NrCompartments)
else
    depthi = depthi - Compartment(compi)%Thickness/2
    NextLayer = .true.
    layeri = layeri + 1
    finished = (layeri > NrSoilLayers)
end if
UNTIL finished or NextLayer
UNTIL finished
do i = compi, NrCompartments
    Compartment(i)%Layer = NrSoilLayers
end do
do i = (NrCompartments+1), max_No_compartments
    Compartment(i)%Thickness = undef_double
end do
! DesignateSoilLayerToCompartments 


subroutine DeclareInitialCondAtFCandNoSalt()

end subroutine DeclareInitialCondAtFCandNoSalt

integer(int32) :: layeri, compi, celli
SetSWCiniFile('(None)')
SWCiniFileFull = GetSWCiniFile() ! no file 
SWCiniDescription = 'Soil water profile at Field Capacity'
Simulation%IniSWC%AtDepths = .false.
Simulation%IniSWC%NrLoc = GetSoil()%NrSoilLayers
do layeri = 1, GetSoil()%NrSoilLayers
    Simulation%IniSWC%Loc(layeri) = SoilLayer(layeri)%Thickness
    Simulation%IniSWC%VolProc(layeri) = SoilLayer(layeri)%FC
    Simulation%IniSWC%SaltECe(layeri) = 0
end do
Simulation%IniSWC%AtFC = .true.
do layeri = (GetSoil()%NrSoilLayers+1), max_No_compartments
    Simulation%IniSWC%Loc(layeri) = undef_double
    Simulation%IniSWC%VolProc(layeri) = undef_double
    Simulation%IniSWC%SaltECe(layeri) = undef_double
end do
do compi = 1, NrCompartments
    do celli = 1, SoilLayer(Compartment(compi)%Layer)%SCP1
        ! salinity in cells
        Compartment(compi)%Salt(celli) = 0.0
        Compartment(compi)%Depo(celli) = 0.0
    end do
    ! DeclareInitialCondAtFCandNoSalt 
end do





subroutine specify_soil_layer(NrCompartments, NrSoilLayers, SoilLayer, Compartment, TotalWaterContent)
    integer(int32), intent(in) :: NrCompartments
    integer(int32), intent(in) :: NrSoilLayers
    type(rep_SoilLayer), intent(inout) :: SoilLayer
    type(rep_Comp), intent(inout) :: Compartment
    type(rep_Content), intent(inout) :: TotalWaterContent
end subroutine specify_soil_layer

integer(int32) :: layeri, compi, celli
real(dp) :: Total

DesignateSoilLayerToCompartments(NrCompartments, NrSoilLayers, Compartment)

! Set soil layers and compartments at Field Capacity and determine Watercontent (mm)
! No salinity in soil layers and compartmens
! Absence of ground water table (FCadj = FC)
Total = 0
do layeri = 1, NrSoilLayers
    SoilLayer(layeri)%WaterContent = 0
end do
do compi = 1, NrCompartments
    Compartment(compi)%Theta = SoilLayer(Compartment(compi)%Layer)%FC/100
    Compartment(compi)%FCadj = SoilLayer(Compartment(compi)%Layer)%FC
    Compartment(compi)%DayAnaero = 0
    do celli = 1, SoilLayer(Compartment(compi)%Layer)%SCP1
        ! salinity in cells
        Compartment(compi)%Salt(celli) = 0.0
        Compartment(compi)%Depo(celli) = 0.0
    end do
    Simulation%ThetaIni(compi) = Compartment(compi)%Theta
    Simulation%ECeIni(compi) = 0 ! initial soil salinity in dS/m
    SoilLayer(Compartment(compi)%Layer)%WaterContent
    = SoilLayer(Compartment(compi)%Layer)%WaterContent
    + Simulation%ThetaIni(compi)*100*10*Compartment(compi)%Thickness
end do
do layeri = 1, NrSoilLayers
    Total = Total + SoilLayer(layeri)%WaterContent
end do
SetTotalWaterContent_BeginDay(Total)

! initial soil water content and no salts
DeclareInitialCondAtFCandNoSalt

! Number of days with RootZone Anaerobic Conditions
Simulation%DayAnaero = 0

! specify_soil_layer 

subroutine DetermineParametersCR(SoilClass, KsatMM, aParam, bParam)
    integer(int8), intent(in) :: SoilClass
    real(dp), intent(in) :: KsatMM
    real(dp), intent(inout) :: aParam
    real(dp), intent(inout) :: bParam
end subroutine DetermineParametersCR

! determine parameters
if (nint(KsatMM*1000) <= 0) then
    aParam = undef_int
    bParam = undef_int
else
    CASE SoilClass OF
        1 :
        ! sandy soils
        aParam = -0.3112 - KsatMM/100000
        bParam = -1.4936 + 0.2416*LN(KsatMM)
    end case
    2 :
    ! loamy soils
    aParam = -0.4986 + 9*KsatMM/100000
    bParam = -2.1320 + 0.4778*LN(KsatMM)
end case
3 :
! sandy clayey soils
aParam = -0.5677 - 4*KsatMM/100000
bParam = -3.7189 + 0.5922*LN(KsatMM)
else
! silty clayey soils
aParam = -0.6366 + 8*KsatMM/10000
bParam = -1.9165 + 0.7063*LN(KsatMM)
! DetermineParametersCR 



integer(int32) function ActiveCells(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ActiveCells

integer(int32) ::  celi

if (Comp%theta <= SoilLayer(Comp%Layer)%UL) then
    celi = 0
    do while (Comp%theta > (SoilLayer(Comp%Layer)%Dx) * celi)
        celi = celi + 1
    end do
else
    celi = SoilLayer(Comp%Layer)%SCP1
end do
ActiveCells = celi
! ActiveCells 


subroutine Calculate_Saltmobility(layer, SaltDiffusion, Macro, Mobil)
    integer(int32), intent(in) :: layer
    integer(int8), intent(in) :: SaltDiffusion
    integer(int8), intent(in) :: Macro
    type(rep_salt), intent(inout) :: Mobil
end subroutine Calculate_Saltmobility

integer(int32) :: i, CelMax
real(dp) :: Mix, a, b, xi, yi, UL

Mix = SaltDiffusion/100 ! global salt mobility expressed as a fraction
UL = SoilLayer(layer)%UL * 100 ! upper limit in VOL% of SC cell 

! 1. convert Macro (vol%) in SaltCelNumber
if (Macro > UL) then
    CelMax = SoilLayer(layer)%SCP1
else
    CelMax = nint((Macro/UL)*SoilLayer(layer)%SC)
end if
if (CelMax <= 0) then
    CelMax = 1
end if

! 2. find a and b
if (Mix < 0.5) then
    a = Mix * 2
    b = EXP(10*(0.5-Mix)*LN(10))
else
    a = 2 * (1- Mix)
    b = EXP(10*(Mix-0.5)*LN(10))
end if

! 3. calculate mobility for cells = 1 to Macro
do i = 1, (CelMax-1)
    xi = i/(CelMax-1)
    if (Mix > 0) then
        if (Mix < 0.5) then
            yi = EXP(LN(a)+xi*LN(b))
            Mobil(i) = (yi-a)/(a*b-a)
        elseif (Mix = 0.5) then
            Mobil(i) = xi
        elseif (Mix < 1) then
            yi = EXP(LN(a)+(1-xi)*LN(b))
            Mobil(i) = 1- (yi-a)/(a*b-a)
        else
            Mobil(i) = 1
        else
            Mobil(i) = 0
        end if
    end if
    
    ! 4. Saltmobility between Macro and SAT
    do i = CelMax, SoilLayer(layer)%SCP1
        Mobil(i) = 1
    end do
    
    ! Calculate_Saltmobility 
end do



real(dp) function ECeComp(Comp)
    type(CompartmentIndividual), intent(in) :: Comp
end function ECeComp

real(dp) :: volSat, TotSalt
integer(int32) :: i
volSAT = (SoilLayer(Comp%Layer)%SAT)
TotSalt = 0
do i = 1, SoilLayer(Comp%Layer)%SCP1
    TotSalt = TotSalt + Comp%Salt(i) + Comp%Depo(i) ! g/m2
end do
TotSalt = TotSalt/
(volSAT*10*Comp%Thickness*(1-SoilLayer(Comp%Layer)%GravelVol/100)) ! g/l
if (TotSalt > SimulParam%SaltSolub) then
    TotSalt = SimulParam%SaltSolub
end if
ECeComp = TotSalt/Equiv ! dS/m
! ECeComp 



real(dp) function ECswComp(Comp, atFC)
    type(CompartmentIndividual), intent(in) :: Comp
    logical, intent(in) :: atFC
end function ECswComp

real(dp) :: TotSalt
integer(int32) :: i

TotSalt = 0
do i = 1, SoilLayer(Comp%Layer)%SCP1
    TotSalt = TotSalt + Comp%Salt(i) + Comp%Depo(i) ! g/m2
end do
if (atFC == .true.) then
    TotSalt = TotSalt/
    (SoilLayer(Comp%Layer)%FC*10*Comp%Thickness*(1-SoilLayer(Comp%Layer)%GravelVol/100)) ! g/l
else
    TotSalt = TotSalt/
    (Comp%theta*1000*Comp%Thickness*(1-SoilLayer(Comp%Layer)%GravelVol/100)) ! g/l
end if
if (TotSalt > SimulParam%SaltSolub) then
    TotSalt = SimulParam%SaltSolub
end if
ECswComp = TotSalt/Equiv
! ECswComp 




subroutine SaltSolutionDeposit(mm, SaltSolution, SaltDeposit)
    real(dp), intent(in) :: mm
    real(dp), intent(inout) :: SaltSolution
    real(dp), intent(inout) :: SaltDeposit
end subroutine SaltSolutionDeposit

SaltSolution = SaltSolution + SaltDeposit
if (SaltSolution > SimulParam%SaltSolub * mm) then
    SaltDeposit = SaltSolution - SimulParam%SaltSolub * mm
    SaltSolution = SimulParam%SaltSolub * mm
else
    SaltDeposit = 0
end if
! SaltSolutionDeposit 




subroutine DetermineSaltContent(ECe, Comp)
    real(dp), intent(in) :: ECe
    type(CompartmentIndividual), intent(inout) :: Comp
end subroutine DetermineSaltContent

real(dp) :: TotSalt, SumDF, SAT, UL, Dx, mm, mm1, mmN
integer(int32) :: celn, i

TotSalt = ECe*Equiv*(SoilLayer(Comp%Layer)%SAT)*10*Comp%Thickness
celn = ActiveCells(Comp)
SAT = (SoilLayer(Comp%Layer)%SAT)/100  ! m3/m3 
UL = SoilLayer(Comp%Layer)%UL ! m3/m3   ! Upper limit of SC salt cel 
Dx = SoilLayer(Comp%Layer)%Dx  ! m3/m3  ! Size of salts cel (expect last one) 
mm1 = Dx*1000*Comp%Thickness
* (1 - SoilLayer(Comp%Layer)%GravelVol/100) ! g/l ! volume [mm]=[l/m2] of cells 
mmN = (SAT-UL)*1000*Comp%Thickness
* (1 - SoilLayer(Comp%Layer)%GravelVol/100) ! g/l ! volume [mm]=[l/m2] of last cell 
SumDF = 0
do i = 1, SoilLayer(Comp%Layer)%SCP1
    Comp%Salt(i) = 0
    Comp%Depo(i) = 0
end do
do i = 1, celn
    SumDF = SumDF + SoilLayer(Comp%Layer)%SaltMobility(i)
end do
do i = 1, celn
    Comp%Salt(i) = TotSalt * SoilLayer(Comp%Layer)%SaltMobility(i)/SumDF
    mm = mm1
    if (i = SoilLayer(Comp%Layer)%SCP1) then
        mm = mmN
    end if
    SaltSolutionDeposit(mm, Comp%Salt(i), Comp%Depo(i))
end if
! DetermineSaltContent 




subroutine CompleteProfileDescription()

end subroutine CompleteProfileDescription

integer(int32) :: i
type(rep_Content) :: TotalWaterContent_temp
do i= (GetSoil()%NrSoilLayers+1), max_SoilLayers
    set_layer_undef(SoilLayer(i))
end do
Simulation%ResetIniSWC = .true. ! soil water content and soil salinity
TotalWaterContent_temp = GetTotalWaterContent()
specify_soil_layer(NrCompartments, GetSoil()%NrSoilLayers, SoilLayer, Compartment, TotalWaterContent_temp)
SetTotalWaterContent(TotalWaterContent_temp)
! CompleteProfileDescription 


subroutine LoadProfile(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadProfile

type(TextFile) :: f0
integer(int32) :: i
type(rep_string3) :: blank
real(dp) :: VersionNr
integer(int8) :: TempShortInt
Assign(f0, FullName)
Reset(f0)
READLN(f0, ProfDescription)
READLN(f0, VersionNr)  ! AquaCrop version
READLN(f0, TempShortInt)
SetSoil_CNvalue(TempShortInt)
READLN(f0, TempShortInt)
SetSoil_REW(TempShortInt)
Simulation%SurfaceStorageIni = 0.0
Simulation%ECStorageIni = 0.0
READLN(f0, TempShortInt)
SetSoil_NrSoilLayers(TempShortInt)
READLN(f0) ! depth of restrictive soil layer which is no longer applicable
READLN(f0)
READLN(f0)
! Load characteristics of each soil layer
do i = 1, GetSoil()%NrSoilLayers
    ! Parameters for capillary rise missing in Versions 3.0 and 3.1
    if (nint(VersionNr*10) < 40) then
        READLN(f0, SoilLayer(i)%Thickness, SoilLayer(i)%SAT, SoilLayer(i)%FC,
        SoilLayer(i)%WP, SoilLayer(i)%InfRate, blank, SoilLayer(i)%Description)
        ! Default values for Penetrability and Gravel
        SoilLayer(i)%Penetrability = 100
        SoilLayer(i)%GravelMass = 0
        ! determine volume gravel
        SoilLayer(i)%GravelVol = 0
    else
        if (nint(VersionNr*10) < 60) then ! UPDATE required for Version 6.0
            READLN(f0, SoilLayer(i)%Thickness, SoilLayer(i)%SAT, SoilLayer(i)%FC,
            SoilLayer(i)%WP, SoilLayer(i)%InfRate, SoilLayer(i)%CRa, SoilLayer(i)%CRb,
            blank, SoilLayer(i)%Description)
            ! Default values for Penetrability and Gravel
            SoilLayer(i)%Penetrability = 100
            SoilLayer(i)%GravelMass = 0
            ! determine volume gravel
            SoilLayer(i)%GravelVol = 0
        else
            READLN(f0, SoilLayer(i)%Thickness, SoilLayer(i)%SAT, SoilLayer(i)%FC,
            SoilLayer(i)%WP, SoilLayer(i)%InfRate, SoilLayer(i)%Penetrability,
            SoilLayer(i)%GravelMass, SoilLayer(i)%CRa, SoilLayer(i)%CRb,
            blank, SoilLayer(i)%Description)
            ! determine volume gravel
            SoilLayer(i)%GravelVol = FromGravelMassToGravelVolume(SoilLayer(i)%SAT, SoilLayer(i)%GravelMass)
        end if
    end if
    ! determine drainage coefficient
    SoilLayer(i)%tau = TauFromKsat(SoilLayer(i)%InfRate)
    ! determine number of salt cells based on infiltration rate
    if (SoilLayer(i)%InfRate <= 112) then
        SoilLayer(i)%SCP1 = 11
    else
        SoilLayer(i)%SCP1 = nint(1.6 + 1000/SoilLayer(i)%InfRate)
        if (SoilLayer(i)%SCP1 < 2) then
            SoilLayer(i)%SCP1 = 2
        end if
        ! determine parameters for soil salinity
        SoilLayer(i)%SC = SoilLayer(i)%SCP1 -1
        SoilLayer(i)%Macro = nint(SoilLayer(i)%FC)
        SoilLayer(i)%UL = ((SoilLayer(i)%SAT)/100) * (SoilLayer(i)%SC/(SoilLayer(i)%SC+2)) ! m3/m3 
        SoilLayer(i)%Dx = (SoilLayer(i)%UL)/SoilLayer(i)%SC  ! m3/m3 
        Calculate_SaltMobility(i, SimulParam%SaltDiff, SoilLayer(i)%Macro, SoilLayer(i)%SaltMobility)
        ! determine default parameters for capillary rise if missing
        SoilLayer(i)%SoilClass = NumberSoilClass(SoilLayer(i)%SAT, SoilLayer(i)%FC, SoilLayer(i)%WP, SoilLayer(i)%InfRate)
        if (nint(VersionNr*10) < 40) then
            DetermineParametersCR(SoilLayer(i)%SoilClass, SoilLayer(i)%InfRate, SoilLayer(i)%CRa, SoilLayer(i)%CRb)
        end if
    end if
    DetermineNrandThicknessCompartments
    Close(f0)
    SetSoil_RootMax(RootMaxInSoilProfile(Crop%RootMax, GetSoil()%NrSoilLayers, SoilLayer))
    ! Loadprofile
end if


real(dp) function CCiniTotalFromTimeToCCini(TempDaysToCCini, TempGDDaysToCCini, L0, L12, L12SF, L123, L1234, GDDL0, GDDL12, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, fWeed, TheModeCycle)
    integer(int32), intent(in) :: TempDaysToCCini
    integer(int32), intent(in) :: TempGDDaysToCCini
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    real(dp), intent(in) :: fWeed
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiniTotalFromTimeToCCini


integer(int32) :: DayCC
real(dp) :: SumGDDforCCini, TempCCini
integer(int32) :: Tadj, GDDTadj
if (TempDaysToCCini /= 0) then
    ! regrowth
    SumGDDforCCini = undef_int
    GDDTadj = undef_int
    ! find adjusted calendar and GDD time
    if (TempDaysToCCini = undef_int) then
        ! CCx on 1st day
        Tadj = L12 - L0
        if (TheModeCycle = GDDays) then
            GDDTadj = GDDL12 - GDDL0
        end if
    else
        ! CC on 1st day is < CCx
        Tadj = TempDaysToCCini
        if (TheModeCycle = GDDays) then
            GDDTadj = TempGDDaysToCCini
        end if
    end if
    ! calculate CCini with adjusted time
    DayCC = L0 + Tadj
    if (TheModeCycle = GDDays) then
        SumGDDforCCini = GDDL0 + GDDTadj
    end if
    TempCCini = CCiNoWaterStressSF(DayCC, L0, L12SF, L123, L1234,
    GDDL0, GDDL12SF, GDDL123, GDDL1234,
    (CCo*fWeed), (CCx*fWeed), CGC, GDDCGC,
    (CDC*(fWeed*CCx+2.29)/(CCx+2.29)),
    (GDDCDC*(fWeed*CCx+2.29)/(CCx+2.29)), SumGDDforCCini, RatDGDD,
    SFRedCGC, SFRedCCx, SFCDecline, TheModeCycle)
    ! correction for fWeed is already in TempCCini (since DayCC > 0);
else
    TempCCini = (CCo*fWeed) ! sowing or transplanting
end if

CCiniTotalFromTimeToCCini = TempCCini
! CCiniTotalFromTimeToCCini 



subroutine CompleteCropDescription()

end subroutine CompleteCropDescription

logical :: CGCisGiven
integer(int8) :: FertStress
if ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage)) then
    if (Crop%DaysToHIo > 0) then
        if (Crop%DaysToHIo > Crop%DaysToHarvest) then
            Crop%dHIdt = Crop%HI/Crop%DaysToHarvest
        else
            Crop%dHIdt = Crop%HI/Crop%DaysToHIo
        end if
        if (Crop%dHIdt > 100) then
            Crop%dHIdt = 100
        end if
    else
        Crop%dHIdt = 100
    end if
else
    !  grain or tuber crops
    if (Crop%DaysToHIo > 0) then
        Crop%dHIdt = Crop%HI/Crop%DaysToHIo
    else
        Crop%dHIdt = undef_int
    end if
end if
if (Crop%ModeCycle = CalendarDays) then
    Crop%DaysToCCini = TimeToCCini(Crop%Planting, Crop%PlantingDens, Crop%SizeSeedling, Crop%SizePlant, Crop%CCx, Crop%CGC)
    Crop%DaysToFullCanopy =
    DaysToReachCCwithGivenCGC((0.98 * Crop%CCx), Crop%CCo, Crop%CCx, Crop%CGC, Crop%DaysToGermination)
    if (GetManagement_FertilityStress() /= 0) then
        FertStress = GetManagement_FertilityStress()
        TimeToMaxCanopySF(Crop%CCo, Crop%CGC, Crop%CCx,
        Crop%DaysToGermination, Crop%DaysToFullCanopy, Crop%DaysToSenescence,
        Crop%DaysToFlowering, Crop%LengthFlowering, Crop%DeterminancyLinked,
        Crop%DaysToFullCanopySF, Simulation%EffectStress%RedCGC,
        Simulation%EffectStress%RedCCX, FertStress)
        SetManagement_FertilityStress(FertStress)
    else
        Crop%DaysToFullCanopySF = Crop%DaysToFullCanopy
    end if
    Crop%GDDaysToCCini = undef_int
    Crop%GDDaysToGermination = undef_int
    Crop%GDDaysToFullCanopy = undef_int
    Crop%GDDaysToFullCanopySF = undef_int
    Crop%GDDaysToFlowering = undef_int
    Crop%GDDLengthFlowering = undef_int
    Crop%GDDaysToSenescence = undef_int
    Crop%GDDaysToHarvest = undef_int
    Crop%GDDaysToMaxRooting = undef_int
    Crop%GDDCGC = undef_int
    Crop%GDDCDC = undef_int
else
    Crop%GDDaysToCCini = TimeToCCini(Crop%Planting, Crop%PlantingDens, Crop%SizeSeedling, Crop%SizePlant, Crop%CCx, Crop%GDDCGC)
    Crop%DaysToCCini = TimeToCCini(Crop%Planting, Crop%PlantingDens, Crop%SizeSeedling, Crop%SizePlant, Crop%CCx, Crop%CGC)
    Crop%GDDaysToFullCanopy =
    DaysToReachCCwithGivenCGC((0.98 * Crop%CCx), Crop%CCo, Crop%CCx, Crop%GDDCGC, Crop%GDDaysToGermination)
    ! Crop.GDDaysToFullCanopySF is determined in RUN or ManagementUnit if required
end if

CGCisGiven = .true. ! required to adjust Crop.DaysToFullCanopy (does not exist)
DetermineLengthGrowthStages(Crop%CCo, Crop%CCx, Crop%CDC, Crop%DaysToGermination, Crop%DaysToHarvest, CGCisGiven,
Crop%DaysToCCini, Crop%Planting, Crop%DaysToSenescence,
Crop%Length, Crop%DaysToFullCanopy, Crop%CGC)

Crop%CCoAdjusted = Crop%CCo
Crop%CCxAdjusted = Crop%CCx
Crop%CCxWithered = Crop%CCx
SetSumWaBal_Biomass(0)
SetSumWaBal_BiomassPot(0)
SetSumWaBal_BiomassUnlim(0)
SetSumWaBal_BiomassTot(0) ! crop and weeds (for soil fertility stress)
SetSumWaBal_YieldPart(0)
Simulation%EvapLimitON = .false.
! CompleteCropDescription 




subroutine LoadCrop(FullName)
    character(len=STRING_LENGTH), intent(in) :: FullName
end subroutine LoadCrop

type(TextFile) :: f0
integer(int32) :: XX, YY
real(dp) :: VersionNr
Assign(f0, FullName)
Reset(f0)
READLN(f0, CropDescription)
WITH Crop
READLN(f0, VersionNr)  ! AquaCrop version
READLN(f0)  ! Protected or Open file

! subkind
READLN(f0, XX)
CASE XX of
    1 : subkind = Vegetative
end case
2 : subkind = Grain
3 : subkind = Tuber
4 : subkind = Forage

! type of planting
READLN(f0, XX)
CASE XX of
    1 : Planting = Seed
end case
0 : Planting = Transplant
-9 : Planting = Regrowth
else
Planting = Seed

! mode
READLN(f0, XX)
if (XX = 0) then
    ModeCycle = GDDays
else
    ModeCycle = CalendarDays
end if

! adjustment p to ETo
READLN(f0, YY)
if (YY = 0) then
    pMethod = NoCorrection
elseif (YY = 1) then
    pMethod = FAOCorrection
end if

! temperatures controlling crop development
READLN(f0, Tbase)
READLN(f0, Tupper)

! required growing degree days to complete the crop cycle (is identical as to maturity)
READLN(f0, GDDaysToHarvest)

! water stress
READLN(f0, pLeafDefUL)
READLN(f0, pLeafDefLL)
READLN(f0, KsShapeFactorLeaf)
READLN(f0, pdef)
READLN(f0, KsShapeFactorStomata)
READLN(f0, pSenescence)
READLN(f0, KsShapeFactorSenescence)
READLN(f0, SumEToDelaySenescence)
READLN(f0, pPollination)
READLN(f0, AnaeroPoint)

! soil fertility/salinity stress
READLN(f0, Crop%StressResponse%Stress)    ! Soil fertility stress at calibration (%)
READLN(f0, Crop%StressResponse%ShapeCGC)  ! Shape factor for the response of Canopy Growth Coefficient to soil fertility/salinity stress
READLN(f0, Crop%StressResponse%ShapeCCX)  ! Shape factor for the response of Maximum Canopy Cover to soil fertility/salinity stress
READLN(f0, Crop%StressResponse%ShapeWP)   ! Shape factor for the response of Crop Water Producitity to soil fertility stress
READLN(f0, Crop%StressResponse%ShapeCDecline)  ! Shape factor for the response of Decline of Canopy Cover to soil fertility/salinity stress
! extra response factor for salinity stress (Version 4.0 and higher)
! -----  UPDATE response factor
!
! IF (ROUND(VersionNr*10) < 40) THEN // UPDATE required for Version 3.0 and 3.1
!     Crop.StressResponse.ShapeKsSto := Crop.StressResponse.ShapeWP
! ELSE
!     READLN(f0,Crop.StressResponse.ShapeKsSto);  //Shape factor for the response of Stomatal Closure to soil salinity stress
! end if
! *)
if (nint(VersionNr*10) >= 40) then ! UPDATE required for Version 4.0 and next
    READLN(f0)  ! Shape factor for the response of Stomatal Closure to soil salinity stress NO LONGER VALID
end if
! continue with soil fertility/salinity stress
if ((Crop%StressResponse%ShapeCGC > 24.9) .and. (Crop%StressResponse%ShapeCCX > 24.9)
    .and. (Crop%StressResponse%ShapeWP > 24.9) .and. (Crop%StressResponse%ShapeCDecline > 24.9)) then
    Crop%StressResponse%Calibrated = .false.
else
    Crop%StressResponse%Calibrated = .true.
end if

! temperature stress
READLN(f0, Tcold) ! Minimum air temperature below which pollination starts to fail (cold stress) (degC)
READLN(f0, Theat) ! Maximum air temperature above which pollination starts to fail (heat stress) (degC)
READLN(f0, GDtranspLow) ! Minimum growing degrees required for full biomass production (degC - day)

! salinity stress (Version 3.2 and higher)
! -----  UPDATE salinity stress
if (nint(VersionNr*10) < 32) then ! UPDATE required for Version 3.0 and 3.1
    ECemin = 2 ! upper threshold ECe
    ECemax = 15 ! lower threhsold ECe
else
    READLN(f0, ECemin) ! upper threshold ECe
    READLN(f0, ECemax) ! lower threhsold ECe
    READLN(f0) ! WAS shape factor of the Ks(salinity) - soil saturation extract (ECe) relationship
end if
! -----  UPDATE salinity stress (Version 5.1 and higher)
if (nint(VersionNr*10) < 51) then ! UPDATE required for previous versions
    CCsaltDistortion = 25 ! distortion canopy cover for simulation of effect of salinity stress (%)
    ResponseECsw = 100 ! Response of Ks stomata to ECsw: From 0 (none) to +200 (very strong)
else
    READLN(f0, CCsaltDistortion)
    READLN(f0, ResponseECsw)
end if

! evapotranspiration
READLN(f0, KcTop)
READLN(f0, KcDecline)
READLN(f0, RootMin)
READLN(f0, RootMax)
if (RootMin > RootMax) then
    RootMin = RootMax ! security for sine function
end if
READLN(f0, RootShape)
READLN(f0, SmaxTopQuarter)
READLN(f0, SmaxBotQuarter)
DeriveSmaxTopBottom(Crop%SmaxTopQuarter, Crop%SmaxBotQuarter, Crop%SmaxTop, Crop%SmaxBot)
READLN(f0, CCEffectEvapLate)

! crop development
READLN(f0, SizeSeedling)
if (nint(VersionNr*10) < 50) then ! UPDATE required for Version not yet 5.0
    SizePlant = SizeSeedling
else
    READLN(f0, SizePlant) ! Canopy size of individual plant (re-growth) at 1st day (cm2)');
end if
READLN(f0, PlantingDens)
CCo = (PlantingDens/10000) * (SizeSeedling/10000)
CCini = (PlantingDens/10000) * (SizePlant/10000)
READLN(f0, CGC)

READLN(f0, YearCCx) ! Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials
READLN(f0, CCxRoot) ! Shape factor of the decline of CCx over the years due to self-thinning - for Perennials
! READLN(f0,CGCdx);  removed as crop parameter
! READLN(f0,CGCns);  removed as crop parameter
READLN(f0)  ! READLN(f0,CGCroot);  removed as crop parameter

READLN(f0, CCx)
READLN(f0, CDC)
READLN(f0, DaysToGermination)
READLN(f0, DaysToMaxRooting)
READLN(f0, DaysToSenescence)
READLN(f0, DaysToHarvest)
READLN(f0, DaysToFlowering)
READLN(f0, LengthFlowering)
! -----  UPDATE crop development for Version 3.1
! leafy vegetable crop has an Harvest Index which builds up starting from sowing
if ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage)) then
    DaysToFlowering = 0
    LengthFlowering = 0
end if

! Crop.DeterminancyLinked
READLN(f0, XX)
CASE XX of
    1 : Crop%DeterminancyLinked = .true.
end case
else
Crop%DeterminancyLinked = .false.

! Potential excess of fruits (%) and building up HI
if ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage)) then
    READLN(f0)  ! PercCycle no longer considered
    Crop%fExcess = undef_int
else
    READLN(f0, fExcess)
end if
READLN(f0, Crop%DaysToHIo)

! yield response to water
READLN(f0, WP)
READLN(f0, WPy)
! adaptation to elevated CO2 (Version 3.2 and higher)
! -----  UPDATE Crop performance under elevated atmospheric CO2 concentration (%)
if (nint(VersionNr*10) < 32) then ! UPDATE required for Version 3.0 and 3.1
    AdaptedToCO2 = 50
else
    READLN(f0, AdaptedToCO2)
end if
READLN(f0, HI)
READLN(f0, HIincrease) ! possible increase (%) of HI due to water stress before flowering
READLN(f0, aCoeff) ! coefficient describing impact of restricted vegetative growth at flowering on HI
READLN(f0, bCoeff) ! coefficient describing impact of stomatal closure at flowering on HI
READLN(f0, DHImax) ! allowable maximum increase (%) of specified HI
! -----  UPDATE yield response to water for Version 3.1
! leafy vegetable crop has an Harvest Index (default is 85 %)
if ((nint(VersionNr*10) = 30) .and. ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage))) then
    if (nint(Crop%HI) = undef_int) then
        HI = 85
    end if
    
    ! growing degree days
    READLN(f0, GDDaysToGermination)
end if
READLN(f0, GDDaysToMaxRooting)
READLN(f0, GDDaysToSenescence)
READLN(f0, GDDaysToHarvest)
READLN(f0, GDDaysToFlowering)
READLN(f0, GDDLengthFlowering)
READLN(f0, GDDCGC)
READLN(f0, GDDCDC)
READLN(f0, GDDaysToHIo)
! -----  UPDATE yield response to water for Version 3.1
! leafy vegetable crop has an Harvest Index which builds up starting from sowing
if ((ModeCycle = GDDays) .and. ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage))) then
    GDDaysToFlowering = 0
    GDDLengthFlowering = 0
end if

! extra version 6.2
if (nint(VersionNr*10) < 62) then ! UPDATE required for Version 6.2
    DryMatter = undef_int ! undefined
else
    READLN(f0, DryMatter) ! dry matter content (%) of fresh yield
end if

! extra version 7.0
if (nint(VersionNr*10) < 62) then ! UPDATE required for Version 7.0
    RootMinYear1 = RootMin ! regrowth not yet possible
    SownYear1 = (Planting = Seed)  ! type of planting first year
    ! transfer of assimilates
    Assimilates%On = .false. ! Transfer of assimilates between root system and above ground parts is NOT considered
    Assimilates%Period = 0
    Assimilates%Stored = 0
    Assimilates%Mobilized = 0
else
    READLN(f0, RootMinYear1) ! Minimum rooting depth in first year in meter (for regrowth)
    READLN(f0, XX)
    CASE XX of
        1 : Crop%SownYear1 = .true.  ! crop is sown in 1 st year (for perennials)
    end case
else
    Crop%SownYear1 = .false. ! crop is transplanted in 1st year (for regrowth)
end case
! transfer of assimilates
READLN(f0, XX)
CASE XX of
    1 : Assimilates%On = .true.  ! Transfer of assimilates from above ground parts to root system is considered
end case
else
Assimilates%On = .false. ! Transfer of assimilates from above ground parts to root system is NOT considered
READLN(f0, Assimilates%Period) ! Number of days at end of season during which assimilates are stored in root system
READLN(f0, Assimilates%Stored) ! Percentage of assimilates, transferred to root system at last day of season
READLN(f0, Assimilates%Mobilized) ! Percentage of stored assimilates, transferred to above ground parts in next season

if (subkind = Forage) then
    ! data for the determination of the growing period
    ! 1. Title
    do XX = 1 to 3
        READLN(f0)
    end do
    ! 2. ONSET
    READLN(f0, XX)
    if (XX = 0) then
        PerennialPeriod%GenerateOnset = .false. ! onset is fixed on a specific day
    else
        ! onset is generated by an air temperature criterion
        PerennialPeriod%GenerateOnset = .true.
        CASE XX OF
            12 : PerennialPeriod%OnsetCriterion = TMeanPeriod ! Criterion: mean air temperature
        end case
        13 : PerennialPeriod%OnsetCriterion = GDDPeriod ! Criterion: growing-degree days
    else
        PerennialPeriod%GenerateOnset = .false.
    end case
end case
READLN(f0, PerennialPeriod%OnsetFirstDay)
READLN(f0, PerennialPeriod%OnsetFirstMonth)
READLN(f0, PerennialPeriod%OnsetLengthSearchPeriod)
READLN(f0, PerennialPeriod%OnsetThresholdValue) ! Mean air temperature or Growing-degree days
READLN(f0, PerennialPeriod%OnsetPeriodValue) ! number of succesive days
READLN(f0, PerennialPeriod%OnsetOccurrence)  ! number of occurrence
if (PerennialPeriod%OnsetOccurrence > 3) then
    PerennialPeriod%OnsetOccurrence = 3
end if
! 3. END of growing period
READLN(f0, XX)
if (XX = 0) then
    PerennialPeriod%GenerateEnd = .false.  ! end is fixed on a specific day
else
    ! end is generated by an air temperature criterion
    PerennialPeriod%GenerateEnd = .true.
    CASE XX OF
        62 : PerennialPeriod%EndCriterion = TMeanPeriod ! Criterion: mean air temperature
    end case
    63 : PerennialPeriod%EndCriterion = GDDPeriod ! Criterion: growing-degree days
else
    PerennialPeriod%GenerateEnd = .false.
end case
READLN(f0, PerennialPeriod%EndLastDay)
READLN(f0, PerennialPeriod%EndLastMonth)
READLN(f0, PerennialPeriod%ExtraYears)
READLN(f0, PerennialPeriod%EndLengthSearchPeriod)
READLN(f0, PerennialPeriod%EndThresholdValue) ! Mean air temperature or Growing-degree days
READLN(f0, PerennialPeriod%EndPeriodValue) ! number of succesive days
READLN(f0, PerennialPeriod%EndOccurrence) ! number of occurrence
if (PerennialPeriod%EndOccurrence > 3) then
    PerennialPeriod%EndOccurrence = 3
end if
Close(f0)
! maximum rooting depth in given soil profile
SetSoil_RootMax(RootMaxInSoilProfile(Crop%RootMax, GetSoil()%NrSoilLayers, SoilLayer))

! copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(Crop%DaysToHarvest - Crop%DaysToSenescence)
SetCropFileSet_DaysToHarvest(Crop%DaysToHarvest)
if (Crop%ModeCycle = GDDays) then
    SetCropFileSet_GDDaysFromSenescenceToEnd(Crop%GDDaysToHarvest - Crop%GDDaysToSenescence)
    SetCropFileSet_GDDaysToHarvest(Crop%GDDaysToHarvest)
else
    SetCropFileSet_GDDaysFromSenescenceToEnd(undef_int)
    SetCropFileSet_GDDaysToHarvest(undef_int)
end if

! LoadCrop


subroutine CompleteClimateDescription(ClimateRecord)
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine CompleteClimateDescription

character(len=STRING_LENGTH) :: dayStr, yearStr
integer(int32) :: Deci
DetermineDayNr(ClimateRecord%FromD, ClimateRecord%FromM, ClimateRecord%FromY, ClimateRecord%FromDayNr)
CASE ClimateRecord%DataType OF
    Daily    :
    ClimateRecord%ToDayNr = ClimateRecord%FromDayNr + ClimateRecord%NrObs - 1
    DetermineDate(ClimateRecord%ToDayNr, ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY)
end case
Decadely :
Deci = nint((ClimateRecord%FromD+9)/10) + ClimateRecord%NrObs - 1
ClimateRecord%ToM = ClimateRecord%FromM
ClimateRecord%ToY = ClimateRecord%FromY
do while (Deci > 3)
    Deci = Deci - 3
    ClimateRecord%ToM = ClimateRecord%ToM + 1
    if (ClimateRecord%ToM > 12) then
        ClimateRecord%ToM = 1
        ClimateRecord%ToY = ClimateRecord%ToY  + 1
    end if
end if
ClimateRecord%ToD = 10
if (Deci = 2) then
    ClimateRecord%ToD = 20
end if
if (Deci = 3) then
    ClimateRecord%ToD = DaysInMonth(ClimateRecord%ToM)
    if ((ClimateRecord%ToM = 2) .and. LeapYear(ClimateRecord%ToY)) then
        ClimateRecord%ToD = ClimateRecord%ToD + 1
    end if
end if
DetermineDayNr(ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY, ClimateRecord%ToDayNr)
Monthly  :
ClimateRecord%ToY = ClimateRecord%FromY
ClimateRecord%ToM = ClimateRecord%FromM + ClimateRecord%NrObs - 1
do while (ClimateRecord%ToM > 12)
    ClimateRecord%ToY = ClimateRecord%ToY + 1
    ClimateRecord%ToM = ClimateRecord%ToM - 12
end do
ClimateRecord%ToD = DaysInMonth(ClimateRecord%ToM)
if ((ClimateRecord%ToM = 2) .and. LeapYear(ClimateRecord%ToY)) then
    ClimateRecord%ToD = ClimateRecord%ToD + 1
end if
DetermineDayNr(ClimateRecord%ToD, ClimateRecord%ToM, ClimateRecord%ToY, ClimateRecord%ToDayNr)
Str(ClimateRecord%FromD:2, dayStr)
if (ClimateRecord%FromY = 1901 then
    yearStr = ''
else
    Str(ClimateRecord%FromY:4, yearStr)
end if
ClimateRecord%FromString = CONCAT(dayStr, ' ', NameMonth(ClimateRecord%FromM), ' ', yearStr)
Str(ClimateRecord%ToD:2, dayStr)
if (ClimateRecord%FromY = 1901 then
    yearStr = ''
else
    Str(ClimateRecord%ToY:4, yearStr)
end if
ClimateRecord%ToString = CONCAT(dayStr, ' ', NameMonth(ClimateRecord%ToM), ' ', yearStr)
! CompleteClimateDescription 


subroutine LoadClimate(FullName, ClimateDescription, TempFile, EToFile, RainFile, CO2File)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    character(len=STRING_LENGTH), intent(inout) :: TempFile
    character(len=STRING_LENGTH), intent(inout) :: EToFile
    character(len=STRING_LENGTH), intent(inout) :: RainFile
    character(len=STRING_LENGTH), intent(inout) :: CO2File
end subroutine LoadClimate

type(TextFile) :: f0
Assign(f0, FullName)
Reset(f0)
READLN(f0, CLimateDescription)
READLN(f0) ! AquaCrop Version
READLN(f0, TempFile)
READLN(f0, EToFile)
READLN(f0, RainFile)
READLN(f0, CO2File)
Close(f0)
! LoadClimate 




subroutine LoadClim(FullName, ClimateDescription, ClimateRecord)
    character(len=STRING_LENGTH), intent(in) :: FullName
    character(len=STRING_LENGTH), intent(inout) :: ClimateDescription
    type(rep_clim), intent(inout) :: ClimateRecord
end subroutine LoadClim

type(TextFile) :: f0
integer(int32) :: Ni
Assign(f0, FullName)
Reset(f0)
READLN(f0, CLimateDescription)
READLN(f0, Ni)
if (Ni = 1) then
    ClimateRecord%DataType = Daily
elseif (Ni = 2) then
    ClimateRecord%DataType = Decadely
else
    ClimateRecord%DataType = Monthly
end if
READLN(f0, ClimateRecord%FromD)
READLN(f0, ClimateRecord%FromM)
READLN(f0, ClimateRecord%FromY)
READLN(f0)
READLN(f0)
READLN(f0)
ClimateRecord%NrObs = 0
do while (NOT Eof(f0)
    ClimateRecord%NrObs = ClimateRecord%NrObs + 1
    READLN(f0)
end do
Close(f0)
CompleteClimateDescription(ClimateRecord)
! LoadClim



subroutine SaveProfile(totalname)
    character(len=STRING_LENGTH), intent(in) :: totalname
end subroutine SaveProfile

type(TextFile) :: f
integer(int32) :: i
Assign(f, totalname)
Rewrite(f)
WRITELN(f, ProfDescription)
WRITELN(f, '        7.0                 : AquaCrop Version (June 2021)')    ! AquaCrop version
WRITELN(f, GetSoil()%CNvalue:9, '                   : CN (Curve Number)')
WRITELN(f, GetSoil()%REW:9, '                   : Readily evaporable water from top layer (mm)')
WRITELN(f, GetSoil()%NrSoilLayers:9, '                   : number of soil horizons')
WRITELN(f, undef_int:9, '                   : variable no longer applicable')
WRITELN(f, '  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description')
WRITELN(f, '  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------')
do i = 1, GetSoil()%NrSoilLayers
    WRITELN(f, SoilLayer(i)%Thickness:8:2, SoilLayer(i)%SAT:8:1, SoilLayer(i)%FC:6:1,
    SoilLayer(i)%WP:6:1, SoilLayer(i)%InfRate:8:1, SoilLayer(i)%Penetrability:11,
    SoilLayer(i)%GravelMass:10, SoilLayer(i)%CRa:14:6, SoilLayer(i)%CRb:10:6,
    '   ', SoilLayer(i)%Description:15)
end do
Close(f)

! maximum rooting depth in  soil profile for given crop
SetSoil_RootMax(RootMaxInSoilProfile(Crop%RootMax, GetSoil()%NrSoilLayers, SoilLayer))
! SaveProfile 



subroutine AppendCropFilePerennials(totalname, GenrateTheOnset, GenerateTheEnd, CriterionNrOnset, Day1Onset, Month1Onset, LengthOnset, SuccessiveDaysOnset, OccurrenceOnset, CriterionNrEnd, DayNEnd, MonthNEnd, ExtraYearEnd, LengthEnd, SuccessiveDaysEnd, OccurrenceEnd, ThresholdOnset, ThresholdEnd)
    character(len=STRING_LENGTH), intent(in) :: totalname
    logical, intent(in) :: GenrateTheOnset
    logical, intent(in) :: GenerateTheEnd
    integer(int32), intent(in) :: CriterionNrOnset
    integer(int32), intent(in) :: Day1Onset
    integer(int32), intent(in) :: Month1Onset
    integer(int32), intent(in) :: LengthOnset
    integer(int32), intent(in) :: SuccessiveDaysOnset
    integer(int32), intent(in) :: OccurrenceOnset
    integer(int32), intent(in) :: CriterionNrEnd
    integer(int32), intent(in) :: DayNEnd
    integer(int32), intent(in) :: MonthNEnd
    integer(int32), intent(in) :: ExtraYearEnd
    integer(int32), intent(in) :: LengthEnd
    integer(int32), intent(in) :: SuccessiveDaysEnd
    integer(int32), intent(in) :: OccurrenceEnd
    real(dp), intent(in) :: ThresholdOnset
    real(dp), intent(in) :: ThresholdEnd
end subroutine AppendCropFilePerennials

type(TextFile) :: f
character(len=STRING_LENGTH) :: TempString
Assign(f, totalname)
Append(f)
! 1. Title
WRITELN(f) ! empty line
WRITELN(f, ' Internal crop calendar')
WRITELN(f, ' ========================================================')

! 2. ONSET
! criterion number
if (GenrateTheOnset == .false.) then
    CriterionNrOnset = 0
    TempString = '         : The Restart of growth is fixed on a specific date'
else
    CASE CriterionNrOnset OF
        12 : TempString = '         : The Restart of growth is generated by average air temperature'
    end case
    13 : TempString = '         : The Restart of growth is generated by Growing-degree days'
else
    ! no valid criterion number
    GenrateTheOnset = .false.
    CriterionNrOnset = 0
    TempString = '         : The Restart of growth is fixed on a specific date'
end case
WRITELN(f, CriterionNrOnset:6, TempString)

! parameters ONSET
if (GenrateTheOnset == .false.) then
    WRITELN(f, Day1Onset:6, '         : First Day of growth')
    WRITELN(f, Month1Onset:6, '         : First Month of growth')
    WRITELN(f, LengthOnset:6, '         : Length (days) for the time window (Restart of growth): Not Applicable')
    WRITELN(f, ThresholdOnset:8:1, '       : Threshold for the Restart criterion: Not Applicable')
    WRITELN(f, SuccessiveDaysOnset:6, '         : Number of successive days for the Restart criterion: Not Applicable')
    WRITELN(f, OccurrenceOnset:6, '         : Number of occurrences before the Restart criterion applies: Not Applicable')
else
    WRITELN(f, Day1Onset:6, '         : First Day for the time window (Restart of growth)')
    WRITELN(f, Month1Onset:6, '         : First Month for the time window (Restart of growth)')
    WRITELN(f, LengthOnset:6, '         : Length (days) of the time window (Restart of growth)')
    CASE CriterionNrOnset OF
        12 : TempString = '       : Threshold for the Restart criterion: Average air temperature (degC)'
    end case
    13 : TempString = '       : Threshold for the Restart criterion: Growing-degree days'
end case
WRITELN(f, ThresholdOnset:8:1, TempString)
WRITELN(f, SuccessiveDaysOnset:6, '         : Number of successive days for the Restart criterion')
if (OccurrenceOnset > 3) then
    OccurrenceOnset = 3
end if
WRITELN(f, OccurrenceOnset:6, '         : Number of occurrences before the Restart criterion applies')

! 3. END of growing period
! criterion number
if (GenerateTheEnd == .false.) then
    CriterionNrEnd = 0
    TempString = '         : The End of growth is fixed on a specific date'
else
    CASE CriterionNrEnd OF
        62 : TempString = '         : The End of growth is generated by average air temperature'
    end case
    63 : TempString = '         : The End of growth is generated by Growing-degree days'
else
    ! no valid criterion number
    GenerateTheEnd = .false.
    CriterionNrEnd = 0
    TempString = '         : The End of growth is fixed on a specific date'
end case
WRITELN(f, CriterionNrEnd:6, TempString)

! parameters END of growing period
if (GenerateTheEnd == .false.) then
    WRITELN(f, DayNEnd:6, '         : Last Day of growth')
    WRITELN(f, MonthNEnd:6, '         : Last Month of growth')
    WRITELN(f, ExtraYearEnd:6, '         : Number of years to add to the Restart year')
    WRITELN(f, LengthEnd:6, '         : Length (days) for the time window (End of growth): Not Applicable')
    WRITELN(f, ThresholdEnd:8:1, '       : Threshold for the End criterion: Not Applicable')
    WRITELN(f, SuccessiveDaysEnd:6, '         : Number of successive days for the End criterion: Not Applicable')
    WRITELN(f, OccurrenceEnd:6, '         : Number of occurrences before the End criterion applies: Not Applicable')
else
    WRITELN(f, DayNEnd:6, '         : Last Day for the time window (End of growth)')
    WRITELN(f, MonthNEnd:6, '         : Last Month for the time window (End of growth)')
    WRITELN(f, ExtraYearEnd:6, '         : Number of years to add to the Onset year')
    WRITELN(f, LengthEnd:6, '         : Length (days) of the time window (End of growth)')
    CASE CriterionNrEnd OF
        62 : TempString = '       : Threshold for the End criterion: average air temperature (degC)'
    end case
    63 : TempString = '       : Threshold for the End criterion: Growing-degree days'
end case
WRITELN(f, ThresholdEnd:8:1, TempString)
WRITELN(f, SuccessiveDaysEnd:6, '         : Number of successive days for the End criterion')
if (OccurrenceEnd > 3) then
    OccurrenceEnd = 3
end if
WRITELN(f, OccurrenceEnd:6, '         : Number of occurrences before the End criterion applies')
Close(f)
! AppendCropFilePerennials 


subroutine SaveCrop(totalname)
    character(len=STRING_LENGTH), intent(in) :: totalname
end subroutine SaveCrop

type(TextFile) :: f
integer(int32) :: i, j
character(len=STRING_LENGTH) :: TempString
Assign(f, totalname)
Rewrite(f)
WRITELN(f, CropDescription)
WITH Crop
! AquaCrop version
WRITELN(f, '     7.0       : AquaCrop Version (June 2021)')
WRITELN(f, '     1         : File not protected')

! SubKind
i = 2
CASE subkind OF
    Vegetative :
    i = 1
    TempString = '         : leafy vegetable crop'
end case
Grain      :
i = 2
TempString = '         : fruit/grain producing crop'
Tuber      :
i = 3
TempString = '         : root/tuber crop'
Forage     :
i = 4
TempString = '         : forage crop'
WRITELN(f, i:6, TempString)

! Sown, transplanting or regrowth
if (Planting = Seed) then
    i = 1
    if (subkind = Forage) then
        WRITELN(f, i:6, '         : Crop is sown in 1st year')
    else
        WRITELN(f, i:6, '         : Crop is sown')
    end if
else
    if (Planting = Transplant) then
        i = 0
        if (subkind = Forage) then
            WRITELN(f, i:6, '         : Crop is transplanted in 1st year')
        else
            WRITELN(f, i:6, '         : Crop is transplanted')
        end if
    else
        i = -9
        WRITELN(f, i:6, '         : Crop is regrowth')
    end if
end if

! Mode (description crop cycle)
i = 1
TempString = '         : Determination of crop cycle : by calendar days'
if (ModeCycle = GDDays) then
    i = 0
    TempString = '         : Determination of crop cycle : by growing degree-days'
end if
WRITELN(f, i:6, TempString)

! p correction for ET
if (pMethod = NoCorrection) then
    j = 0
    WRITELN(f, j:6, '         : No adjustment by ETo of soil water depletion factors (p)')
else
    j = 1
    WRITELN(f, j:6, '         : Soil water depletion factors (p) are adjusted by ETo')
end if

! temperatures controlling crop development
WRITELN(f, Tbase:8:1, '       : Base temperature (degC) below which crop development does not progress')
WRITELN(f, Tupper:8:1, '       : Upper temperature (degC) above which crop development no longer increases with an increase in temperature')

! required growing degree days to complete the crop cycle (is identical as to maturity)
WRITELN(f, GDDaysToHarvest:6, '         : Total length of crop cycle in growing degree-days')

! water stress
WRITELN(f, pLeafDefUL:9:2, '      : Soil water depletion factor for canopy expansion (p-exp) - Upper threshold')
WRITELN(f, pLeafDefLL:9:2, '      : Soil water depletion factor for canopy expansion (p-exp) - Lower threshold')
WRITELN(f, KsShapeFactorLeaf:8:1, '       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)')
WRITELN(f, pdef:9:2, '      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold')
WRITELN(f, KsShapeFactorStomata:8:1, '       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)')
WRITELN(f, pSenescence:9:2, '      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold')
WRITELN(f, KsShapeFactorSenescence:8:1, '       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)')
WRITELN(f, SumEToDelaySenescence:6, '         : Sum(ETo) during dormant period to be exceeded before crop is permanently wilted')
if (pPollination = undef_int) then
    WRITELN(f, pPollination:9:2, '      : Soil water depletion factor for pollination - Not Applicable')
else
    WRITELN(f, pPollination:9:2, '      : Soil water depletion factor for pollination (p - pol) - Upper threshold')
end if
WRITELN(f, AnaeroPoint:6, '         : Vol% for Anaerobiotic point ! (SAT - [vol%]) at which deficient aeration occurs ');

! stress response
WRITELN(f, Crop%StressResponse%Stress:6, '         : Considered soil fertility stress for calibration of stress response (%)')
if (Crop%StressResponse%ShapeCGC > 24.9) then
    WRITELN(f, Crop%StressResponse%ShapeCGC:9:2, '      : Response of canopy expansion is not considered')
else
    WRITELN(f, Crop%StressResponse%ShapeCGC:9:2, '      : Shape factor for the response of canopy expansion to soil fertility stress')
end if
if (Crop%StressResponse%ShapeCCX > 24.9) then
    WRITELN(f, Crop%StressResponse%ShapeCCX:9:2, '      : Response of maximum canopy cover is not considered')
else
    WRITELN(f, Crop%StressResponse%ShapeCCX:9:2, '      : Shape factor for the response of maximum canopy cover to soil fertility stress')
end if
if (Crop%StressResponse%ShapeWP > 24.9) then
    WRITELN(f, Crop%StressResponse%ShapeWP:9:2, '      : Response of crop Water Productivity is not considered')
else
    WRITELN(f, Crop%StressResponse%ShapeWP:9:2, '      : Shape factor for the response of crop Water Productivity to soil fertility stress')
end if
if (Crop%StressResponse%ShapeCDecline > 24.9) then
    WRITELN(f, Crop%StressResponse%ShapeCDecline:9:2, '      : Response of decline of canopy cover is not considered')
else
    WRITELN(f, Crop%StressResponse%ShapeCDecline:9:2, '      : Shape factor for the response of decline of canopy cover to soil fertility stress')
end if
WRITELN(f, '    -9         : dummy - Parameter no Longer required')

! temperature stress
if (Round(Tcold) = undef_int) then
    WRITELN(f, Tcold:6, '         : Cold (air temperature) stress affecting pollination - not considered')
else
    WRITELN(f, Tcold:6, '         : Minimum air temperature below which pollination starts to fail (cold stress) (degC)')
end if
if (Round(Theat) = undef_int) then
    WRITELN(f, Theat:6, '         : Heat (air temperature) stress affecting pollination - not considered')
else
    WRITELN(f, Theat:6, '         : Maximum air temperature above which pollination starts to fail (heat stress) (degC)')
end if
if (Round(GDtranspLow) = undef_int) then
    WRITELN(f, GDtranspLow:8:1, '       : Cold (air temperature) stress on crop transpiration not considered')
else
    WRITELN(f, GDtranspLow:8:1, '       : Minimum growing degrees required for full crop transpiration (degC - day)')
end if

! salinity stress
WRITELN(f, ECemin:6, '         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)')
WRITELN(f, ECemax:6, '         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)')
WRITELN(f, '    -9         : Dummy - no longer applicable') ! shape factor Ks(salt)-ECe
WRITELN(f, CCsaltDistortion:6, '         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))')
WRITELN(f, ResponseECsw:6, '         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))')

! evapotranspiration
WRITELN(f, KcTop:9:2, '      : Crop coefficient when canopy is complete but prior to senescence (KcTr, x)')
WRITELN(f, KcDecline:10:3, '     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc%')
WRITELN(f, RootMin:9:2, '      : Minimum effective rooting depth (m)')
WRITELN(f, RootMax:9:2, '      : Maximum effective rooting depth (m)')
WRITELN(f, RootShape:6, '         : Shape factor describing root zone expansion')
WRITELN(f, SmaxTopQuarter:10:3, '     : Maximum root water extraction (m3water/m3soil%day) in top quarter of root zone')
WRITELN(f, SmaxBotQuarter:10:3, '     : Maximum root water extraction (m3water/m3soil%day) in bottom quarter of root zone')
WRITELN(f, CCEffectEvapLate:6, '         : Effect of canopy cover in reducing soil evaporation in late season stage')

! canopy development
WRITELN(f, SizeSeedling:9:2, '      : Soil surface covered by an individual seedling at 90 % emergence (cm2)')
WRITELN(f, SizePlant:9:2, '      : Canopy size of individual plant (re-growth) at 1st day (cm2)')
WRITELN(f, PlantingDens:9, '      : Number of plants per hectare')
WRITELN(f, CGC:12:5, '   : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)')
if (YearCCx = undef_int) then
    WRITELN(f, YearCCx:6, '         : Number of years at which CCx declines to 90 % of its value due to self-thinning - Not Applicable')
else
    WRITELN(f, YearCCx:6, '         : Number of years at which CCx declines to 90 % of its value due to self-thinning - for Perennials')
end if
if (Round(CCxRoot) = undef_int) then
    WRITELN(f, CCxRoot:9:2, '      : Shape factor of the decline of CCx over the years due to self-thinning - Not Applicable')
else
    WRITELN(f, CCxRoot:9:2, '      : Shape factor of the decline of CCx over the years due to self-thinning - for Perennials')
end if
WRITELN(f, '    -9         : dummy - Parameter no Longer required')

WRITELN(f, CCx:9:2, '      : Maximum canopy cover (CCx) in fraction soil cover')
WRITELN(f, CDC:12:5, '   : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)')
if (Planting = Seed) then
    WRITELN(f, DaysToGermination:6, '         : Calendar Days: from sowing to emergence')
    WRITELN(f, DaysToMaxRooting:6, '         : Calendar Days: from sowing to maximum rooting depth')
    WRITELN(f, DaysToSenescence:6, '         : Calendar Days: from sowing to start senescence')
    WRITELN(f, DaysToHarvest:6, '         : Calendar Days: from sowing to maturity (length of crop cycle)')
    if (subkind = Tuber) then
        WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from sowing to start of yield formation')
    else
        WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from sowing to flowering')
    end if
else
    if (Planting = Transplant) then
        WRITELN(f, DaysToGermination:6, '         : Calendar Days: from transplanting to recovered transplant')
        WRITELN(f, DaysToMaxRooting:6, '         : Calendar Days: from transplanting to maximum rooting depth')
        WRITELN(f, DaysToSenescence:6, '         : Calendar Days: from transplanting to start senescence')
        WRITELN(f, DaysToHarvest:6, '         : Calendar Days: from transplanting to maturity')
        if (subkind = Tuber) then
            WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from transplanting to start of yield formation')
        else
            WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from transplanting to flowering')
        end if
    else
        ! planting = regrowth
        WRITELN(f, DaysToGermination:6, '         : Calendar Days: from regrowth to recovering')
        WRITELN(f, DaysToMaxRooting:6, '         : Calendar Days: from regrowth to maximum rooting depth')
        WRITELN(f, DaysToSenescence:6, '         : Calendar Days: from regrowth to start senescence')
        WRITELN(f, DaysToHarvest:6, '         : Calendar Days: from regrowth to maturity')
        if (subkind = Tuber) then
            WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from regrowth to start of yield formation')
        else
            WRITELN(f, DaysToFlowering:6, '         : Calendar Days: from regrowth to flowering')
        end if
    end if
end if
WRITELN(f, LengthFlowering:6, '         : Length of the flowering stage (days)')

! Crop.DeterminancyLinked
if (DeterminancyLinked == .true.) then
    i = 1
    TempString = '         : Crop determinancy linked with flowering'
else
    i = 0
    TempString = '         : Crop determinancy unlinked with flowering'
end if
WRITELN(f, i:6, TempString)

! Potential excess of fruits (%)
if ((Crop%subkind = Vegetative) .or. (Crop%subkind = Forage)) then
    WRITELN(f, undef_int:6, '         : parameter NO LONGER required') ! Building up of Harvest Index (% of growing cycle)')
else
    WRITE(f, fExcess:6)
    if (fExcess = undef_int) then
        WRITELN(f, '         : Excess of potential fruits - Not Applicable')
    else
        WRITELN(f, '         : Excess of potential fruits (%)')
    end if
end if

! Building-up of Harvest Index
WRITE(f, DaysToHIo:6)
if (DaysToHIo = undef_int) then
    WRITELN(f, '         : Building up of Harvest Index - Not Applicable')
else
    CASE subkind OF
        Vegetative,
        Forage     : WRITELN(f, '         : Building up of Harvest Index starting at sowing/transplanting (days)')
    end case
    Grain      : WRITELN(f, '         : Building up of Harvest Index starting at flowering (days)')
    Tuber      : WRITELN(f, '         : Building up of Harvest Index starting at root/tuber enlargement (days)')
else
    WRITELN(f, '         : Building up of Harvest Index during yield formation (days)')
end case

! yield response to water
WRITELN(f, WP:8:1, '       : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)')
WRITELN(f, WPy:6, '         : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)')
WRITELN(f, AdaptedToCO2:6, '         : Crop performance under elevated atmospheric CO2 concentration (%)')
WRITELN(f, HI:6, '         : Reference Harvest Index (HIo) (%)')
if (subkind = Tuber) then
    WRITELN(f, HIincrease:6, '         : Possible increase (%) of HI due to water stress before start of yield formation')
else
    WRITELN(f, HIincrease:6, '         : Possible increase (%) of HI due to water stress before flowering')
end if
if (nint(aCoeff) = undef_int) then
    WRITELN(f, aCoeff:8:1, '       : No impact on HI of restricted vegetative growth during yield formation ')
else
    WRITELN(f, aCoeff:8:1, '       : Coefficient describing positive impact on HI of restricted vegetative growth during yield formation')
end if
if (nint(bCoeff) = undef_int) then
    WRITELN(f, bCoeff:8:1, '       : No effect on HI of stomatal closure during yield formation')
else
    WRITELN(f, bCoeff:8:1, '       : Coefficient describing negative impact on HI of stomatal closure during yield formation')
end if
WRITELN(f, DHImax:6, '         : Allowable maximum increase (%) of specified HI')

! growing degree days
if (Planting = Seed) then
    WRITELN(f, GDDaysToGermination:6, '         : GDDays: from sowing to emergence')
    WRITELN(f, GDDaysToMaxRooting:6, '         : GDDays: from sowing to maximum rooting depth')
    WRITELN(f, GDDaysToSenescence:6, '         : GDDays: from sowing to start senescence')
    WRITELN(f, GDDaysToHarvest:6, '         : GDDays: from sowing to maturity (length of crop cycle)')
    if (subkind = Tuber) then
        WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from sowing to start tuber formation')
    else
        WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from sowing to flowering')
    end if
else
    if (Planting = Transplant) then
        WRITELN(f, GDDaysToGermination:6, '         : GDDays: from transplanting to recovered transplant')
        WRITELN(f, GDDaysToMaxRooting:6, '         : GDDays: from transplanting to maximum rooting depth')
        WRITELN(f, GDDaysToSenescence:6, '         : GDDays: from transplanting to start senescence')
        WRITELN(f, GDDaysToHarvest:6, '         : GDDays: from transplanting to maturity')
        if (subkind = Tuber) then
            WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from transplanting to start yield formation')
        else
            WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from transplanting to flowering')
        end if
    else
        ! Planting = regrowth
        WRITELN(f, GDDaysToGermination:6, '         : GDDays: from regrowth to recovering')
        WRITELN(f, GDDaysToMaxRooting:6, '         : GDDays: from regrowth to maximum rooting depth')
        WRITELN(f, GDDaysToSenescence:6, '         : GDDays: from regrowth to start senescence')
        WRITELN(f, GDDaysToHarvest:6, '         : GDDays: from regrowth to maturity')
        if (subkind = Tuber) then
            WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from regrowth to start yield formation')
        else
            WRITELN(f, GDDaysToFlowering:6, '         : GDDays: from regrowth to flowering')
        end if
    end if
end if
WRITELN(f, GDDLengthFlowering:6, '         : Length of the flowering stage (growing degree days)')
WRITELN(f, GDDCGC:13:6, '  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)')
WRITELN(f, GDDCDC:13:6, '  : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)')
WRITELN(f, GDDaysToHIo:6, '         : GDDays: building-up of Harvest Index during yield formation')

! added to 6.2
WRITELN(f, DryMatter:6, '         : dry matter content (%) of fresh yield')

! added to 7.0 - Perennial crops
if (Crop%subkind = Forage) then
    WRITELN(f, RootMinYear1:9:2, '      : Minimum effective rooting depth (m) in first year (for perennials)')
else
    WRITELN(f, RootMinYear1:9:2, '      : Minimum effective rooting depth (m) in first year - required only in case of regrowth')
end if
if (Crop%SownYear1 == .true.) then
    i = 1
    if (Crop%subkind = Forage) then
        WRITELN(f, i:6, '         : Crop is sown in 1st year (for perennials)')
    else
        WRITELN(f, i:6, '         : Crop is sown in 1st year - required only in case of regrowth')
    end if
else
    i = 0
    if (Crop%subkind = Forage) then
        WRITELN(f, i:6, '         : Crop is transplanted in 1st year (for perennials)')
    else
        WRITELN(f, i:6, '         : Crop is transplanted in 1st year - required only in case of regrowth')
    end if
end if

! added to 7.0 - Assimilates
if (Crop%Assimilates%On == .false.) then
    i = 0
    WRITELN(f, i:6, '         : Transfer of assimilates from above ground parts to root system is NOT considered')
    WRITELN(f, i:6, '         : Number of days at end of season during which assimilates are stored in root system')
    WRITELN(f, i:6, '         : Percentage of assimilates transferred to root system at last day of season')
    WRITELN(f, i:6, '         : Percentage of stored assimilates transferred to above ground parts in next season')
else
    i = 1
    WRITELN(f, i:6, '         : Transfer of assimilates from above ground parts to root system is considered')
    WRITELN(f, Crop%Assimilates%Period:6, '         : Number of days at end of season during which assimilates are stored in root system')
    WRITELN(f, Crop%Assimilates%Stored:6, '         : Percentage of assimilates transferred to root system at last day of season')
    WRITELN(f, Crop%Assimilates%Mobilized:6, '         : Percentage of stored assimilates transferred to above ground parts in next season')
end if
Close(f)

! maximum rooting depth in given soil profile
SetSoil_RootMax(RootMaxInSoilProfile(Crop%RootMax, GetSoil()%NrSoilLayers, SoilLayer))

! copy to CropFileSet
SetCropFileSet_DaysFromSenescenceToEnd(Crop%DaysToHarvest - Crop%DaysToSenescence)
SetCropFileSet_DaysToHarvest(Crop%DaysToHarvest)
SetCropFileSet_GDDaysFromSenescenceToEnd(Crop%GDDaysToHarvest - Crop%GDDaysToSenescence)
SetCropFileSet_GDDaysToHarvest(Crop%GDDaysToHarvest)
! SaveCrop 








character(len=STRING_LENGTH) function EndGrowingPeriod(Day1, DayN)
    integer(int32), intent(in) :: Day1
    integer(int32), intent(inout) :: DayN
end function EndGrowingPeriod

integer(int32) :: dayi, monthi, yeari
character(len=STRING_LENGTH) :: Strday, StrMonth
! Deze functie bepaald Crop.DayN en de string
DayN = Day1 + Crop%DaysToHarvest - 1
if (DayN < Day1) then
    DayN = Day1
end if
DetermineDate(DayN, dayi, monthi, yeari)
Str(dayi:2, Strday)
StrMonth = NameMonth(monthi)
EndGrowingPeriod = CONCAT(Strday, ' ', StrMonth, '  ')
! EndGrowingPeriod 


subroutine DetermineLinkedSimDay1(CropDay1, SimDay1)
    integer(int32), intent(in) :: CropDay1
    integer(int32), intent(inout) :: SimDay1
end subroutine DetermineLinkedSimDay1

SimDay1 = CropDay1
if (GetClimFile() /= '(None)') then
    !
!     IF SimDay1 < ClimRecord.FromDayNr THEN
!         SimDay1 := ClimRecord.FromDayNr;
!     end if
!     IF SimDay1 > ClimRecord.ToDayNr THEN
!         Simulation.LinkCropToSimPeriod := false;
!         SimDay1 := ClimRecord.FromDayNr;
!         ; *)
        if ((SimDay1 < ClimRecord%FromDayNr) .or. (SimDay1 > ClimRecord%ToDayNr)) then
            Simulation%LinkCropToSimPeriod = .false.
            SimDay1 = ClimRecord%FromDayNr
        end if
    end if
    ! DetermineLinkedSimDay1 
end if


subroutine AdjustCropYearToClimFile(CDay1, CDayN)
    integer(int32), intent(inout) :: CDay1
    integer(int32), intent(inout) :: CDayN
end subroutine AdjustCropYearToClimFile

integer(int32) :: dayi, monthi, yeari
character(len=STRING_LENGTH) :: temp_str
DetermineDate(CDay1, dayi, monthi, yeari)
if (GetClimFile() = '(None)') then
    yeari = 1901  ! yeari = 1901 if undefined year
else
    yeari = ClimRecord%FromY ! yeari = 1901 if undefined year
end if
!
! ELSE
! yeari := Simulation.YearStartCropCycle;
! IF (CDay1 > ClimRecord.ToY) THEN
!     yeari := ClimRecord.FromY;
! end if
! ; *)
DetermineDayNr(dayi, monthi, yeari, CDay1)
temp_str = EndGrowingPeriod(CDay1, CDayN)
! AdjustCropYearToClimFile 


subroutine AdjustClimRecordTo(CDayN)
    integer(int32), intent(in) :: CDayN
end subroutine AdjustClimRecordTo

integer(int32) :: dayi, monthi, yeari
DetermineDate(CDayN, dayi, monthi, yeari)
ClimRecord%ToD = 31
ClimRecord%ToM = 12
ClimRecord%ToY = yeari
DetermineDayNr(ClimRecord%ToD, ClimRecord%ToM, ClimRecord%ToY, ClimRecord%ToDayNr)
! AdjustClimRecordTo 


subroutine ResetSWCToFC()

end subroutine ResetSWCToFC

integer(int8) :: Loci, layeri, compi, celli

Simulation%IniSWC%AtDepths = .false.
if (ZiAqua < 0) then ! no ground water table
    Simulation%IniSWC%NrLoc = GetSoil()%NrSoilLayers
    do layeri = 1, GetSoil()%NrSoilLayers
        Simulation%IniSWC%Loc(layeri) = SoilLayer(layeri)%Thickness
        Simulation%IniSWC%VolProc(layeri) = SoilLayer(layeri)%FC
        Simulation%IniSWC%SaltECe(layeri) = 0
    end do
    do layeri = (GetSoil()%NrSoilLayers+1), max_No_compartments
        Simulation%IniSWC%Loc(layeri) = undef_double
        Simulation%IniSWC%VolProc(layeri) = undef_double
        Simulation%IniSWC%SaltECe(layeri) = undef_double
    end do
else
    Simulation%IniSWC%NrLoc = NrCompartments
    do Loci = 1, Simulation%IniSWC%NrLoc
        Simulation%IniSWC%Loc(Loci) = Compartment(Loci)%Thickness
        Simulation%IniSWC%VolProc(Loci) = Compartment(Loci)%FCadj
        Simulation%IniSWC%SaltECe(Loci) = 0.0
    end do
end do
do compi = 1, NrCompartments
    Compartment(compi)%Theta = Compartment(compi)%FCadj/100
    Simulation%ThetaIni(compi) = Compartment(compi)%Theta
    do celli = 1, SoilLayer(Compartment(compi)%Layer)%SCP1
        ! salinity in cells
        Compartment(compi)%Salt(celli) = 0.0
        Compartment(compi)%Depo(celli) = 0.0
    end do
end do
! ResetSWCToFC 



subroutine AdjustSimPeriod()

end subroutine AdjustSimPeriod

integer(int32) :: IniSimFromDayNr
character(len=STRING_LENGTH) :: FullFileName
IniSimFromDayNr = Simulation%FromDayNr
CASE Simulation%LinkCropToSimPeriod OF
    true :
    DetermineLinkedSimDay1(Crop%Day1, Simulation%FromDayNr)
    if (Crop%Day1 = Simulation%FromDayNr) then
        Simulation%ToDayNr = Crop%DayN
    else
        Simulation%ToDayNr = Simulation%FromDayNr + 30 ! 30 days
    end if
    if (GetClimFile() /= '(None)') then
        if (Simulation%ToDayNr > ClimRecord%ToDayNr) then
            Simulation%ToDayNr = ClimRecord%ToDayNr
        end if
        if (Simulation%ToDayNr < ClimRecord%FromDayNr) then
            Simulation%ToDayNr = ClimRecord%FromDayNr
        end if
    end if
end if
false :
!
! IF ((GetClimFile() <> '(None)') AND (Simulation.FromDayNr < ClimRecord.FromDayNr)) THEN
!     Simulation.FromDayNr := ClimRecord.FromDayNr;
!     Simulation.ToDayNr := Simulation.FromDayNr + 30; // 30 days
!     ; *)
    if (Simulation%FromDayNr > Crop%Day1) then
        Simulation%FromDayNr = Crop%Day1
    end if
    Simulation%ToDayNr = Crop%DayN
end if
if ((GetClimFile() /= '(None)') AND
    ((Simulation%FromDayNr <= ClimRecord%FromDayNr) .or. (Simulation%FromDayNr >= ClimRecord%ToDayNr))) then
    Simulation%FromDayNr = ClimRecord%FromDayNr
    Simulation%ToDayNr = Simulation%FromDayNr + 30 ! 30 days
end if

! adjust initial depth and quality of the groundwater when required
if ((NOT SimulParam%ConstGwt) .and. (IniSimFromDayNr /= Simulation%FromDayNr)) then
    if (GetGroundWaterFile() = '(None)') then
        FullFileName = CONCAT(GetPathNameProg(), 'GroundWater%AqC')
    else
        FullFileName = GetGroundWaterFileFull()
    end if
    ! initialize ZiAqua and ECiAqua
    LoadGroundWater(FullFileName, Simulation%FromDayNr, ZiAqua, ECiAqua)
    CalculateAdjustedFC((ZiAqua/100), Compartment)
    if (Simulation%IniSWC%AtFC then
        ResetSWCToFC
    end if
end if
! AdjustSimPeriod 



subroutine AdjustOnsetSearchPeriod()

end subroutine AdjustOnsetSearchPeriod

if (GetClimFile() = '(None)') then
    Onset%StartSearchDayNr = 1
    Onset%StopSearchDayNr = Onset%StartSearchDayNr + Onset%LengthSearchPeriod - 1
    ! Onset.StopSearchDayNr := 365;
else
    ! Onset.StartSearchDayNr := ClimRecord.FromDayNr;
    ! Onset.StopSearchDayNr := ClimRecord.ToDayNr;
    DetermineDayNr((1), (1), Simulation%YearStartCropCycle, Onset%StartSearchDayNr) ! 1 January
    if (Onset%StartSearchDayNr < ClimRecord%FromDayNr) then
        Onset%StartSearchDayNr = ClimRecord%FromDayNr
    end if
    Onset%StopSearchDayNr = Onset%StartSearchDayNr + Onset%LengthSearchPeriod - 1
    if (Onset%StopSearchDayNr > ClimRecord%ToDayNr) then
        Onset%StopSearchDayNr = ClimRecord%ToDayNr
        Onset%LengthSearchPeriod = Onset%StopSearchDayNr - Onset%StartSearchDayNr + 1
    end if
end if
! AdjustOnsetSearchPeriod 


subroutine SetClimData()

end subroutine SetClimData

type(rep_clim) :: SetARecord, SetBRecord

ClimRecord%NrObs = 999 ! (heeft geen belang)
! IF 365 (= full undefined year)

! Part A - ETo and Rain files --> ClimFile
if ((GetEToFile() = '(None)') .and. (GetRainFile() = '(None)')) then
    SetClimFile('(None)')
    ClimDescription = 'Specify Climatic data when Running AquaCrop'
    WITH ClimRecord
    DataType = Daily
    FromString = 'any date'
    ToString = 'any date'
    FromY = 1901
end if
else
SetClimFile('EToRainTempFile')
ClimDescription = 'Read ETo/RAIN/TEMP data set'
if (GetEToFile() = '(None)') then
    WITH ClimRecord
    FromY = RainRecord%FromY
    FromDayNr = RainRecord%FromDayNr
    ToDayNr = RainRecord%ToDayNr
    FromString = RainRecord%FromString
    ToString = RainRecord%ToString
    if (FullUndefinedRecord(RainRecord%FromY, RainRecord%FromD, RainRecord%FromM, RainRecord%ToD, RainRecord%ToM) then
        NrObs = 365
    end if
end if
if (GetRainFile() = '(None)') then
    WITH ClimRecord
    FromY = EToRecord%FromY
    FromDayNr = EToRecord%FromDayNr
    ToDayNr = EToRecord%ToDayNr
    FromString = EToRecord%FromString
    ToString = EToRecord%ToString
    if (FullUndefinedRecord(EToRecord%FromY, EToRecord%FromD, EToRecord%FromM, EToRecord%ToD, EToRecord%ToM) then
        NrObs = 365
    end if
end if

if ((GetEToFile() /= '(None)') .and. (GetRainFile() /= '(None)')) then
    SetARecord = EToRecord
    SetBRecord = RainRecord
    if ((EToRecord%FromY = 1901)
        .and. FullUndefinedRecord(EToRecord%FromY, EToRecord%FromD, EToRecord%FromM, EToRecord%ToD, EToRecord%ToM))
        .and. ((RainRecord%FromY = 1901)
        .and. FullUndefinedRecord(RainRecord%FromY, RainRecord%FromD, RainRecord%FromM, RainRecord%ToD, RainRecord%ToM)) then
        ClimRecord%NrObs = 365
    end if
    
    if ((EToRecord%FromY = 1901) .and. (RainRecord%FromY /= 1901)) then
        ! Jaartal van RainRecord ---> SetARecord (= EToRecord)
        ! FromY + adjust FromDayNr and FromString
        SetARecord%FromY = RainRecord%FromY
        DetermineDayNr(EToRecord%FromD, EToRecord%FromM, SetARecord%FromY, SetARecord%FromDayNr)
        if (((SetARecord%FromDayNr < RainRecord%FromDayNr)) .and. (RainRecord%FromY < RainRecord%ToY)) then
            SetARecord%FromY = RainRecord%FromY + 1
            DetermineDayNr(EToRecord%FromD, EToRecord%FromM, SetARecord%FromY, SetARecord%FromDayNr)
        end if
        ClimRecord%FromY = SetARecord%FromY ! nodig voor DayString (werkt met ClimRecord)
        SetARecord%FromString = DayString(SetARecord%FromDayNr)
        ! ToY + adjust ToDayNr and ToString
        if (FullUndefinedRecord(EToRecord%FromY, EToRecord%FromD, EToRecord%FromM, EToRecord%ToD, EToRecord%ToM)) then
            SetARecord%ToY = RainRecord%ToY
        else
            SetARecord%ToY = SetARecord%FromY
        end if
        DetermineDayNr(EToRecord%ToD, EToRecord%ToM, SetARecord%ToY, SetARecord%ToDayNr)
        SetARecord%ToString = DayString(SetARecord%ToDayNr)
    end if
    
    if ((EToRecord%FromY /= 1901) .and. (RainRecord%FromY = 1901)) then
        ! Jaartal van EToRecord ---> SetBRecord (= RainRecord)
        ! FromY + adjust FromDayNr and FromString
        SetBRecord%FromY = EToRecord%FromY
        DetermineDayNr(RainRecord%FromD, RainRecord%FromM, SetBRecord%FromY, SetBRecord%FromDayNr)
        if (((SetBRecord%FromDayNr < EToRecord%FromDayNr)) .and. (EToRecord%FromY < EToRecord%ToY)) then
            SetBRecord%FromY = EToRecord%FromY + 1
            DetermineDayNr(RainRecord%FromD, RainRecord%FromM, SetBRecord%FromY, SetBRecord%FromDayNr)
        end if
        ClimRecord%FromY = SetBRecord%FromY ! nodig voor DayString (werkt met ClimRecord)
        SetBRecord%FromString = DayString(SetBRecord%FromDayNr)
        ! ToY + adjust ToDayNr and ToString
        if (FullUndefinedRecord(RainRecord%FromY, RainRecord%FromD, RainRecord%FromM, RainRecord%ToD, RainRecord%ToM)) then
            SetBRecord%ToY = EToRecord%ToY
        else
            SetBRecord%ToY = SetBRecord%FromY
        end if
        DetermineDayNr(RainRecord%ToD, RainRecord%ToM, SetBRecord%ToY, SetBRecord%ToDayNr)
        SetBRecord%ToString = DayString(SetBRecord%ToDayNr)
    end if
    
    ! bepaal characteristieken van ClimRecord
    WITH ClimRecord
    FromY = SetARecord%FromY
    FromDayNr = SetARecord%FromDayNr
    FromString = SetARecord%FromString
    if (FromDayNr < SetBRecord%FromDayNr) then
        FromY = SetBRecord%FromY
        FromDayNr = SetBRecord%FromDayNr
        FromString = SetBRecord%FromString
    end if
    ToDayNr = SetARecord%ToDayNr
    ToString = SetARecord%ToString
    if (ToDayNr > SetBRecord%ToDayNr) then
        ToDayNr = SetBRecord%ToDayNr
        ToString = SetBRecord%ToString
    end if
    if (ToDayNr < FromDayNr) then
        SetClimFile('(None)')
        ClimDescription = 'ETo data set <--NO OVERLAP--> RAIN data set'
        NrObs = 0
        FromY = 1901
    end if
end if


! Part B - ClimFile and Temperature files --> ClimFile
if (TemperatureFile = '(None)') then
    ! no adjustments are required
else
    if (GetClimFile() = '(None)') then
        SetClimFile('EToRainTempFile')
        ClimDescription = 'Read ETo/RAIN/TEMP data set'
        WITH ClimRecord
        FromY = TemperatureRecord%FromY
        FromDayNr = TemperatureRecord%FromDayNr
        ToDayNr = TemperatureRecord%ToDayNr
        FromString = TemperatureRecord%FromString
        ToString = TemperatureRecord%ToString
        if ((TemperatureRecord%FromY = 1901) .and. FullUndefinedRecord(TemperatureRecord%FromY, TemperatureRecord%FromD, TemperatureRecord%FromM, TemperatureRecord%ToD, TemperatureRecord%ToM)) then
            NrObs = 365
        else
            NrObs = TemperatureRecord%ToDayNr - TemperatureRecord%FromDayNr + 1
        end if
    end if
else
    DetermineDate(ClimRecord%FromDayNr, ClimRecord%FromD, ClimRecord%FromM, ClimRecord%FromY)
    DetermineDate(ClimRecord%ToDayNr, ClimRecord%ToD, ClimRecord%ToM, ClimRecord%ToY)
    SetARecord = ClimRecord
    SetBRecord = TemperatureRecord
    
    if ((ClimRecord%FromY = 1901) .and. (TemperatureRecord%FromY = 1901)
        .and. (ClimRecord%NrObs = 365)
        .and. FullUndefinedRecord(TemperatureRecord%FromY, TemperatureRecord%FromD, TemperatureRecord%FromM, TemperatureRecord%ToD, TemperatureRecord%ToM)) then
        ClimRecord%NrObs = 365
    else
        ClimRecord%NrObs = TemperatureRecord%ToDayNr - TemperatureRecord%FromDayNr + 1
    end if
    
    if ((ClimRecord%FromY = 1901) .and. (TemperatureRecord%FromY /= 1901)) then
        ! Jaartal van TemperatureRecord ---> SetARecord (= ClimRecord)
        ! FromY + adjust FromDayNr and FromString
        SetARecord%FromY = TemperatureRecord%FromY
        DetermineDayNr(ClimRecord%FromD, ClimRecord%FromM, SetARecord%FromY, SetARecord%FromDayNr)
        if (((SetARecord%FromDayNr < TemperatureRecord%FromDayNr)) .and. (TemperatureRecord%FromY < TemperatureRecord%ToY)) then
            SetARecord%FromY = TemperatureRecord%FromY + 1
            DetermineDayNr(ClimRecord%FromD, ClimRecord%FromM, SetARecord%FromY, SetARecord%FromDayNr)
        end if
        ! ClimRecord.FromY := SetARecord.FromY; ! nodig voor DayString (werkt met ClimRecord)
        SetARecord%FromString = DayString(SetARecord%FromDayNr)
        ! ToY + adjust ToDayNr and ToString
        if (FullUndefinedRecord(ClimRecord%FromY, ClimRecord%FromD, ClimRecord%FromM, ClimRecord%ToD, ClimRecord%ToM)) then
            SetARecord%ToY = TemperatureRecord%ToY
        else
            SetARecord%ToY = SetARecord%FromY
        end if
        DetermineDayNr(ClimRecord%ToD, ClimRecord%ToM, SetARecord%ToY, SetARecord%ToDayNr)
        SetARecord%ToString = DayString(SetARecord%ToDayNr)
    end if
    
    if ((ClimRecord%FromY /= 1901) .and. (TemperatureRecord%FromY = 1901)) then
        ! Jaartal van ClimRecord ---> SetBRecord (= TemperatureRecord)
        ! FromY + adjust FromDayNr and FromString
        SetBRecord%FromY = ClimRecord%FromY
        DetermineDayNr(TemperatureRecord%FromD, TemperatureRecord%FromM, SetBRecord%FromY, SetBRecord%FromDayNr)
        if (((SetBRecord%FromDayNr < ClimRecord%FromDayNr)) .and. (ClimRecord%FromY < ClimRecord%ToY)) then
            SetBRecord%FromY = ClimRecord%FromY + 1
            DetermineDayNr(TemperatureRecord%FromD, TemperatureRecord%FromM, SetBRecord%FromY, SetBRecord%FromDayNr)
        end if
        ! ClimRecord.FromY := SetBRecord.FromY; ! nodig voor DayString (werkt met ClimRecord)
        SetBRecord%FromString = DayString(SetBRecord%FromDayNr)
        ! ToY + adjust ToDayNr and ToString
        if (FullUndefinedRecord(TemperatureRecord%FromY, TemperatureRecord%FromD, TemperatureRecord%FromM, TemperatureRecord%ToD, TemperatureRecord%ToM)) then
            SetBRecord%ToY = ClimRecord%ToY
        else
            SetBRecord%ToY = SetBRecord%FromY
        end if
        DetermineDayNr(TemperatureRecord%ToD, TemperatureRecord%ToM, SetBRecord%ToY, SetBRecord%ToDayNr)
        SetBRecord%ToString = DayString(SetBRecord%ToDayNr)
    end if
    
    ! bepaal nieuwe characteristieken van ClimRecord
    WITH ClimRecord
    FromY = SetARecord%FromY
    FromDayNr = SetARecord%FromDayNr
    FromString = SetARecord%FromString
    if (FromDayNr < SetBRecord%FromDayNr) then
        FromY = SetBRecord%FromY
        FromDayNr = SetBRecord%FromDayNr
        FromString = SetBRecord%FromString
    end if
    ToDayNr = SetARecord%ToDayNr
    ToString = SetARecord%ToString
    if (ToDayNr > SetBRecord%ToDayNr) then
        ToDayNr = SetBRecord%ToDayNr
        ToString = SetBRecord%ToString
    end if
    if (ToDayNr < FromDayNr) then
        SetClimFile('(None)')
        ClimDescription = 'Clim data <--NO OVERLAP--> TEMPERATURE data'
        NrObs = 0
        FromY = 1901
    end if
end if
! SetClimData 


subroutine DetermineRootZoneWC(RootingDepth, ZtopSWCconsidered)
    real(dp), intent(in) :: RootingDepth
    logical, intent(inout) :: ZtopSWCconsidered
end subroutine DetermineRootZoneWC

real(dp) :: CumDepth, Factor, frac_value, DrRel, DZtopRel, TopSoilInMeter
integer(int32) :: compi
! calculate SWC in root zone
CumDepth = 0
compi = 0
SetRootZoneWC_Actual(0)
SetRootZoneWC_FC(0)
SetRootZoneWC_WP(0)
SetRootZoneWC_SAT(0)
SetRootZoneWC_Leaf(0)
SetRootZoneWC_Thresh(0)
SetRootZoneWC_Sen(0)
REPEAT
compi = compi + 1
CumDepth = CumDepth + Compartment(compi)%Thickness
if (CumDepth <= RootingDepth) then
    Factor = 1
else
    frac_value = RootingDepth - (CumDepth - Compartment(compi)%Thickness)
    if (frac_value > 0) then
        Factor = frac_value/Compartment(compi)%Thickness
    else
        Factor = 0
    end if
end if
SetRootZoneWC_Actual(GetRootZoneWC()%Actual
+ Factor * 1000 * Compartment(compi)%Theta * Compartment(compi)%Thickness
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
SetRootZoneWC_FC(GetRootZoneWC()%FC
+ Factor * 10 * SoilLayer(Compartment(compi)%Layer)%FC * Compartment(compi)%Thickness
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
SetRootZoneWC_Leaf(GetRootZoneWC()%Leaf
+ Factor * 10 * Compartment(compi)%Thickness * (SoilLayer(Compartment(compi)%Layer)%FC
- Crop%pLeafAct * (SoilLayer(Compartment(compi)%Layer)%FC-SoilLayer(Compartment(compi)%Layer)%WP))
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
sETRootZoneWC_Thresh(GetRootZoneWC()%Thresh
+ Factor * 10 * Compartment(compi)%Thickness * (SoilLayer(Compartment(compi)%Layer)%FC
- Crop%pActStom * (SoilLayer(Compartment(compi)%Layer)%FC-SoilLayer(Compartment(compi)%Layer)%WP))
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
SetRootZoneWC_Sen(GetRootZoneWC()%Sen
+ Factor * 10 * Compartment(compi)%Thickness * (SoilLayer(Compartment(compi)%Layer)%FC
- Crop%pSenAct * (SoilLayer(Compartment(compi)%Layer)%FC-SoilLayer(Compartment(compi)%Layer)%WP))
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
SetRootZoneWC_WP(GetRootZoneWC()%WP
+ Factor * 10 * SoilLayer(Compartment(compi)%Layer)%WP * Compartment(compi)%Thickness
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
SetRootZoneWC_SAT(GetRootZoneWC()%SAT
+ Factor * 10 * SoilLayer(Compartment(compi)%Layer)%SAT * Compartment(compi)%Thickness
* (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
UNTIL (CumDepth >= RootingDepth) .or. (compi = NrCompartments)

! calculate SWC in top soil (top soil in meter = SimulParam.ThicknessTopSWC/100)
if ((RootingDepth*100) <= SimulParam%ThicknessTopSWC) then
    SetRootZoneWC_ZtopAct(GetRootZoneWC()%Actual)
    SetRootZoneWC_ZtopFC(GetRootZoneWC()%FC)
    SetRootZoneWC_ZtopWP(GetRootZoneWC()%WP)
    SetRootZoneWC_ZtopThresh(GetRootZoneWC()%Thresh)
else
    CumDepth = 0
    compi = 0
    SetRootZoneWC_ZtopAct(0)
    SetRootZoneWC_ZtopFC(0)
    SetRootZoneWC_ZtopWP(0)
    SetRootZoneWC_ZtopThresh(0)
    TopSoilInMeter = SimulParam%ThicknessTopSWC/100
    REPEAT
    compi = compi + 1
    CumDepth = CumDepth + Compartment(compi)%Thickness
    if ((CumDepth*100) <= SimulParam%ThicknessTopSWC) then
        Factor = 1
    else
        frac_value = TopSoilInMeter - (CumDepth - Compartment(compi)%Thickness)
        if (frac_value > 0) then
            Factor = frac_value/Compartment(compi)%Thickness
        else
            Factor = 0
        end if
    end if
    SetRootZoneWC_ZtopAct(GetRootZoneWC()%ZtopAct
    + Factor * 1000 * Compartment(compi)%Theta * Compartment(compi)%Thickness
    * (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
    SetRootZoneWC_ZtopFC(GetRootZoneWC()%ZtopFC
    + Factor * 10 * SoilLayer(Compartment(compi)%Layer)%FC * Compartment(compi)%Thickness
    * (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
    SetRootZoneWC_ZtopWP(GetRootZoneWC()%ZtopWP
    + Factor * 10 * SoilLayer(Compartment(compi)%Layer)%WP * Compartment(compi)%Thickness
    * (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
    SetRootZoneWC_ZtopThresh(GetRootZoneWC()%ZtopThresh
    + Factor * 10 * Compartment(compi)%Thickness * (SoilLayer(Compartment(compi)%Layer)%FC
    - Crop%pActStom * (SoilLayer(Compartment(compi)%Layer)%FC-SoilLayer(Compartment(compi)%Layer)%WP))
    * (1 - SoilLayer(Compartment(compi)%Layer)%GravelVol/100))
    UNTIL (CumDepth >= TopSoilInMeter) .or. (compi = NrCompartments)
end if

! Relative depletion in rootzone and in top soil
if (nint(1000*(GetRootZoneWc()%FC - GetRootZoneWc()%WP)) > 0 then
    DrRel = (GetRootZoneWc()%FC - GetRootZoneWC()%Actual)/(GetRootZoneWc()%FC - GetRootZoneWc()%WP)
else
    DrRel = 0
end if
if (nint(1000*(GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopWP)) > 0 then
    DZtopRel = (GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopAct)/(GetRootZoneWC()%ZtopFC - GetRootZoneWc()%ZtopWP)
else
    DZtopRel = 0
end if

! Zone in soil profile considered for determining stress response
if (DZtopRel < DrRel) then
    ZtopSWCconsidered = .true.  ! top soil is relative wetter than root zone
else
    ZtopSWCconsidered = .false.
end if
! DetermineRootZoneWC 



type(repstring17) function DayString(DNr)
    integer(int32), intent(in) :: DNr
end function DayString

integer(int32) :: dayi, monthi, yeari
character(len=STRING_LENGTH) :: strA, strB
if (GetClimFile() = '(None)') then
    do while (DNr > 365)
        DNr = DNr - 365
    end do
    DetermineDate(DNr, dayi, monthi, yeari)
end do
Str(dayi:2, strA)
if (ClimRecord%FromY = 1901) then
    strB = ''
else
    Str(yeari:4, strB)
end if
StrB = CONCAT(TRIM(strA), ' ', Trim(NameMonth(monthi)), ' ', Trim(strB))
do while (Length(StrB) < 17)
    StrB = CONCAT(StrB, ' ')
end do
DayString = StrB
! DayString 


real(dp) function CanopyCoverNoStressSF(DAP, L0, L123, LMaturity, GDDL0, GDDL123, GDDLMaturity, CCo, CCx, CGC, CDC, GDDCGC, GDDCDC, SumGDD, TypeDays, SFRedCGC, SFRedCCx)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LMaturity
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDLMaturity
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: SumGDD
    type(rep_modeCycle), intent(in) :: TypeDays
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
end function CanopyCoverNoStressSF



real(dp) function CanopyCoverNoStressDaysSF(DAP, L0, L123, LMaturity, CCo, CCx, CGC, CDC, SFRedCGC, SFRedCCx)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: LMaturity
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: CDC
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
end function CanopyCoverNoStressDaysSF

real(dp) :: CC, CCxAdj, CDCadj
integer(int32) :: t

! CanopyCoverNoStressDaysSF 
CC = 0.0
t = DAP - Simulation%DelayedDays
! CC refers to canopy cover at the end of the day

if ((t >= 1) .and. (t <= LMaturity) .and. (CCo > 0)) then
    if (t <= L0) then ! before germination or recovering of transplant
        CC = 0
    else
        if (t < L123) then ! Canopy development and Mid-season stage
            CC = CCatTime((t-L0), CCo, ((1-SFRedCGC/100)*CGC), ((1-SFRedCCx/100)*CCx))
        else
            ! Late-season stage  (t <= LMaturity)
            if (CCx < 0.001) then
                CC = 0
            else
                CCxAdj = CCatTime((L123-L0), CCo, ((1-SFRedCGC/100)*CGC), ((1-SFRedCCx/100)*CCx))
                CDCadj = CDC*(CCxAdj+2.29)/(CCx+2.29)
                if (CCxAdj < 0.001) then
                    CC = 0
                    ! ELSE CC := CCxAdj * (1 - 0.05*(exp((t-L123)*CDC*3.33*((CCxAdj+2.29)/(CCx+2.29))/(CCxAdj+2.29))-1));
                else
                    CC = CCxAdj * (1 - 0.05*(exp((t-L123)*3.33*CDCAdj/(CCxAdj+2.29))-1))
                end if
            end if
        end if
    end if
end if
if (CC > 1) then
    CC = 1
end if
if (CC < 0) then
    CC = 0
end if
CanopyCoverNoStressDaysSF = CC
! CanopyCoverNoStressDaysSF 


! CanopyCoverNoStressSF 
CASE TypeDays OF
    GDDays : CanopyCoverNoStressSF = CanopyCoverNoStressGDDaysSF(GDDL0, GDDL123, GDDLMaturity,
    SumGDD, CCo, CCx, GDDCGC, GDDCDC, SFRedCGC, SFRedCCx)
end case
else
CanopyCoverNoStressSF = CanopyCoverNoStressDaysSF(DAP, L0, L123, LMaturity,
CCo, CCx, CGC, CDC, SFRedCGC, SFRedCCx)
! CanopyCoverNoStressSF 


subroutine ReadSoilSettings()

end subroutine ReadSoilSettings

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
integer(int8) :: i
FullName = CONCAT(GetPathNameSimul(), 'Soil%PAR')
Assign(f, FullName)
Reset(f)
READLN(f, SimulParam%RunoffDepth) ! considered depth (m) of soil profile for calculation of mean soil water content
READLN(f, i)   ! correction CN for Antecedent Moisture Class
if (i = 1) then
    SimulParam%CNcorrection = .true.
else
    SimulParam%CNcorrection = .false.
end if
READLN(f, SimulParam%SaltDiff) ! salt diffusion factor (%)
READLN(f, SimulParam%SaltSolub) ! salt solubility (g/liter)
READLN(f, SimulParam%RootNrDF) ! shape factor capillary rise factor
! new Version 4.1
READLN(f, SimulParam%IniAbstract) ! Percentage of S for initial abstraction for surface runoff
SimulParam%IniAbstract = 5 ! fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
Close(f)
! ReadSoilSettings 


real(dp) function HarvestIndexDay(DAP, DaysToFlower, HImax, dHIdt, CCi, CCxadjusted, PercCCxHIfinal, TempPlanting, PercentLagPhase, HIfinal)
    integer(int32), intent(in) :: DAP
    integer(int32), intent(in) :: DaysToFlower
    integer(int32), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt
    real(dp), intent(in) :: CCi
    real(dp), intent(in) :: CCxadjusted
    integer(int8), intent(in) :: PercCCxHIfinal
    type(rep_Planting), intent(in) :: TempPlanting
    integer(int8), intent(inout) :: PercentLagPhase
    integer(int32), intent(inout) :: HIfinal
end function HarvestIndexDay


HIo = 1
real(dp) :: HIGC, HIday, HIGClinear
integer(int32) :: t, tMax, tSwitch

t = DAP - Simulation%DelayedDays - DaysToFlower
! Simulation.WPyON := false;
PercentLagPhase = 0
if (t <= 0) then
    HIday = 0
else
    if ((Crop%Subkind = Vegetative) .and. (TempPlanting = Regrowth)) then
        dHIdt = 100
    end if
    if ((Crop%Subkind = Forage) .and. (TempPlanting = Regrowth)) then
        dHIdt = 100
    end if
    if (dHIdt > 99) then
        HIday = HImax
        PercentLagPhase = 100
    else
        HIGC = HarvestIndexGrowthCoefficient(HImax, dHIdt)
        GetDaySwitchToLinear(HImax, dHIdt, HIGC, tSwitch, HIGClinear)
        if (t < tSwitch) then
            PercentLagPhase = nint(100 * (t/tSwitch))
            HIday = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*t))
        else
            PercentLagPhase = 100
            if ((Crop%subkind = Tuber) .or. (Crop%subkind = Vegetative) .or. (Crop%subkind = Forage)) then
                ! continue with logistic equation
                HIday = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*t))
                if (HIday >= 0.9799*HImax) then
                    HIday = HImax
                end if
            else
                ! switch to linear increase
                HIday = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*tSwitch))
                HIday = Hiday + HIGClinear*(t-tSwitch)
            end if
        end if
        if (HIday > HImax) then
            HIday = HImax
        end if
        if (HIday <= (HIo + 0.4)) then
            HIday = 0
        end if
        if ((HImax - HIday) < 0.4) then
            HIday = HImax
        end if
    end if
    
    ! adjust HIfinal if required for inadequate photosynthesis (unsufficient green canopy)
    tMax = nint(HImax/dHIdt)
    if ((HIfinal = HImax) .and. (t <= tmax) .and. (CCi <= (PercCCxHIfinal/100))
        .and. (Crop%subkind /= Vegetative) .and. (Crop%subkind /= Forage)) then
        HIfinal = nint(HIday)
    end if
    if (HIday > HIfinal) then
        HIday = HIfinal
    end if
end if
HarvestIndexDay = HIday

! HarvestIndexDay 


subroutine ReadRainfallSettings()

end subroutine ReadRainfallSettings

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
integer(int8) :: NrM
FullName = CONCAT(GetPathNameSimul(), 'Rainfall%PAR')
Assign(f, FullName)
Reset(f)
READLN(f) ! Settings for processing 10-day or monthly rainfall data
WITH SimulParam
READLN(f, NrM)
Case NrM OF
0 : EffectiveRain%Method = Full
1 : EffectiveRain%Method = USDA
2 : EffectiveRain%Method = Percentage
READLN(f, EffectiveRain%PercentEffRain) ! IF Method is Percentage
READLN(f, EffectiveRain%ShowersInDecade)  ! For estimation of surface run-off
READLN(f, EffectiveRain%RootNrEvap) ! For reduction of soil evaporation
Close(f)
! ReadRainfallSettings 


subroutine ReadCropSettingsParameters()

end subroutine ReadCropSettingsParameters

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
FullName = CONCAT(GetPathNameSimul(), 'Crop%PAR')
Assign(f, FullName)
Reset(f)
WITH SimulParam
READLN(f, EvapDeclineFactor) ! evaporation decline factor in stage 2
READLN(f, KcWetBare) ! Kc wet bare soil [-]
READLN(f, PercCCxHIfinal) ! CC threshold below which HI no longer increase(% of 100)
READLN(f, RootPercentZmin) ! Starting depth of root sine function (% of Zmin)
READLN(f, MaxRootZoneExpansion) ! cm/day
MaxRootZoneExpansion = 5.00 ! fixed at 5 cm/day
READLN(f, KsShapeFactorRoot) ! Shape factor for effect water stress on rootzone expansion
READLN(f, TAWGermination)  ! Soil water content (% TAW) required at sowing depth for germination
READLN(f, pAdjFAO) ! Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
READLN(f, DelayLowOxygen) ! number of days for full effect of deficient aeration
READLN(f, ExpFsen) ! exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
READLN(f, Beta) ! Decrease (percentage) of p(senescence) once early canopy senescence is triggered
READLN(f, ThicknessTopSWC) ! Thickness top soil (cm) in which soil water depletion has to be determined
Close(f)
! ReadCropSettingsParameters 


subroutine ReadFieldSettingsParameters()

end subroutine ReadFieldSettingsParameters

type(textfile) :: f
character(len=STRING_LENGTH) :: FullName
FullName = CONCAT(GetPathNameSimul(), 'Field%PAR')
Assign(f, FullName)
Reset(f)
WITH SimulParam
READLN(f, EvapZmax) ! maximum water extraction depth by soil evaporation [cm]
Close(f)
! ReadFieldSettingsParameters 


subroutine ReadTemperatureSettingsParameters()

end subroutine ReadTemperatureSettingsParameters

type(text) :: f0
character(len=STRING_LENGTH) :: FullName
FullName = CONCAT(GetPathNameSimul(), 'Temperature%PAR')
Assign(f0, FullName)
Reset(f0)
READLN(f0)
WITH SimulParam
READLN(f0, Tmin)   ! Default minimum temperature (degC) if no temperature file is specified
READLN(f0, Tmax)   ! Default maximum temperature (degC) if no temperature file is specified
READLN(f0, GDDMethod) ! Default method for GDD calculations
if (GDDMethod > 3) then
    GDDMethod = 3
end if
if (GDDMethod < 1) then
    GDDMethod = 1
end if
Close(f0)
! ReadTemperatureSettingsParameters 

real(dp) function AdjustedKsStoToECsw(ECeMin, ECeMax, ResponseECsw, ECei, ECswi, ECswFCi, Wrel, Coeffb0Salt, Coeffb1Salt, Coeffb2Salt, KsStoIN)
    integer(int8), intent(in) :: ECeMin
    integer(int8), intent(in) :: ECeMax
    integer(int32), intent(in) :: ResponseECsw
    real(dp), intent(in) :: ECei
    real(dp), intent(in) :: ECswi
    real(dp), intent(in) :: ECswFCi
    real(dp), intent(in) :: Wrel
    real(dp), intent(in) :: Coeffb0Salt
    real(dp), intent(in) :: Coeffb1Salt
    real(dp), intent(in) :: Coeffb2Salt
    real(dp), intent(in) :: KsStoIN
end function AdjustedKsStoToECsw

real(dp) :: ECswRel, LocalKsShapeFactorSalt, KsSalti, SaltStressi, StoClosure, KsStoOut
if ((ResponseECsw > 0) .and. (Wrel > 0) .and. (Simulation%SalinityConsidered == .true.)) then
    ! adjustment to ECsw considered
    ECswRel = ECswi - (ECswFCi - ECei) + (ResponseECsw-100)*Wrel
    if ((ECswRel > ECeMin) .and. (ECswRel < ECeMax)) then
        ! stomatal closure at ECsw relative
        LocalKsShapeFactorSalt = +3 ! CONVEX give best ECsw response
        KsSalti = KsSalinity(Simulation%SalinityConsidered, ECeMin, ECeMax, ECswRel, LocalKsShapeFactorSalt)
        SaltStressi = (1-KsSalti)*100
        StoClosure = Coeffb0Salt + Coeffb1Salt * SaltStressi + Coeffb2Salt * SaltStressi * SaltStressi
        ! adjusted KsSto
        KsStoOut = (1 - StoClosure/100)
        if (KsStoOut < 0) then
            KsStoOut = 0
        end if
        if (KsStoOut > KsStoIN) then
            KsStoOut = KsStoIN
        end if
    else
        if (ECswRel >= ECeMax) then
            KsStoOut = 0 ! full stress
        else
            KsStoOut = KsStoIN ! no extra stress
        end if
    end if
else
    KsStoOut = KsStoIN  ! no adjustment to ECsw
end if
AdjustedKsStoToECsw = KsStoOut
! AdjustedKsStoToECsw 




subroutine DetermineRootZoneSaltContent(RootingDepth, ZrECe, ZrECsw, ZrECswFC, ZrKsSalt)
    real(dp), intent(in) :: RootingDepth
    real(dp), intent(inout) :: ZrECe
    real(dp), intent(inout) :: ZrECsw
    real(dp), intent(inout) :: ZrECswFC
    real(dp), intent(inout) :: ZrKsSalt
end subroutine DetermineRootZoneSaltContent

real(dp) :: CumDepth, Factor, frac_value
integer(int32) :: compi
CumDepth = 0
compi = 0
ZrECe = 0
ZrECsw = 0
ZrECswFC = 0
ZrKsSalt = 1
if (RootingDepth >= Crop%RootMin) then
    REPEAT
    compi = compi + 1
    CumDepth = CumDepth + Compartment(compi)%Thickness
    if (CumDepth <= RootingDepth) then
        Factor = 1
    else
        frac_value = RootingDepth - (CumDepth - Compartment(compi)%Thickness)
        if (frac_value > 0) then
            Factor = frac_value/Compartment(compi)%Thickness
        else
            Factor = 0
        end if
    end if
    Factor = Factor * (Compartment(compi)%Thickness)/RootingDepth ! weighting factor
    ZrECe = ZrECe + Factor * ECeComp(Compartment(compi))
    ZrECsw = ZrECsw + Factor * ECswComp(Compartment(compi), (false)) ! not at FC
    ZrECswFC = ZrECswFC + Factor * ECswComp(Compartment(compi), (true)) ! at FC
    UNTIL (CumDepth >= RootingDepth) .or. (compi = NrCompartments)
    if (((Crop%ECemin /= undef_int) .and. (Crop%ECemax /= undef_int)) .and. (Crop%ECemin < Crop%ECemax)) then
        ZrKsSalt = KsSalinity((true), Crop%ECemin, Crop%ECemax, ZrECe, (0.0))
    else
        ZrKsSalt = KsSalinity((false), Crop%ECemin, Crop%ECemax, ZrECe, (0.0))
    end if
else
    ZrECe = undef_int
    ZrECsw = undef_int
    ZrECswFC = undef_int
    ZrKsSalt = undef_int
end if
! DetermineRootZoneSaltContent 


real(dp) function CO2ForSimulationPeriod(FromDayNr, ToDayNr)
    integer(int32), intent(in) :: FromDayNr
    integer(int32), intent(in) :: ToDayNr
end function CO2ForSimulationPeriod

integer(int32) :: i, Dayi, Monthi, FromYi, ToYi
type(textfile) :: f0
character(len=STRING_LENGTH) :: TempString
real(dp) :: CO2From, CO2To, CO2a, CO2b, YearA, YearB
DetermineDate(FromDayNr, Dayi, Monthi, FromYi)
DetermineDate(ToDayNr, Dayi, Monthi, ToYi)
if ((FromYi = 1901) .or. (ToYi = 1901)) then
    CO2ForSimulationPeriod = CO2Ref
else
    Assign(f0, GetCO2FileFull())
    Reset(f0)
    do i= 1, 3
        READLN(f0) ! Description and Title
    end do
    ! from year
    READLN(f0, TempString)
    SplitStringInTwoParams(TempString, YearB, CO2b)
    if (nint(YearB) >= FromYi) then
        CO2From = CO2b
        YearA = YearB
        CO2a = CO2b
    else
        REPEAT
        YearA = YearB
        CO2a = Co2b
        READLN(f0, TempString)
        SplitStringInTwoParams(TempString, YearB, CO2b)
        UNTIL ((nint(YearB) >= FromYi) .or. EoF(f0))
        if (FromYi > nint(YearB)) then
            CO2From = CO2b
        else
            CO2From = CO2a + (CO2b-CO2a)*(nint(FromYi)-nint(YearA))/(nint(YearB)-nint(YearA))
        end if
    end if
    ! to year
    CO2To = CO2From
    if (ToYi > FromYi) .and. (ToYi > nint(YearA)) then
        if (nint(YearB) >= ToYi) then
            CO2To = CO2a + (CO2b-CO2a)*(nint(ToYi)-nint(YearA))/(nint(YearB)-nint(YearA))
        elseif (NOT EoF(f0)) then
            REPEAT
            YearA = YearB
            CO2a = Co2b
            READLN(f0, TempString)
            SplitStringInTwoParams(TempString, YearB, CO2b)
            UNTIL ((nint(YearB) >= ToYi) .or. EoF(f0))
            if (ToYi > nint(YearB)) then
                CO2To = CO2b
            else
                CO2To = CO2a + (CO2b-CO2a)*(nint(ToYi)-nint(YearA))/(nint(YearB)-nint(YearA))
            end if
        end if
    end if
    Close(f0)
    CO2ForSimulationPeriod = (CO2From+CO2To)/2
end if
! CO2ForSimulationPeriod 


real(dp) function CCiNoWaterStressSF(Dayi, L0, L12SF, L123, L1234, GDDL0, GDDL12SF, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, SumGDD, RatDGDD, SFRedCGC, SFRedCCx, SFCDecline, TheModeCycle)
    integer(int32), intent(in) :: Dayi
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: L123
    integer(int32), intent(in) :: L1234
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDL1234
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CGC
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: CDC
    real(dp), intent(in) :: GDDCDC
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: RatDGDD
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx
    real(dp), intent(in) :: SFCDecline
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function CCiNoWaterStressSF


real(dp) :: CCi, CCibis, CCxAdj, CDCadj, GDDCDCadj

! Calculate CCi
CCi = CanopyCoverNoStressSF(Dayi, L0, L123, L1234, GDDL0, GDDL123, GDDL1234,
CCo, CCx, CGC, CDC, GDDCGC, GDDCDC, SumGDD, TheModeCycle,
SFRedCGC, SFRedCCX)

! Consider CDecline for limited soil fertiltiy
! IF ((Dayi > L12SF) AND (SFCDecline > 0.000001))
if ((Dayi > L12SF) .and. (SFCDecline > 0.000001) .and. (L12SF < L123)) then
    if (Dayi < L123) then
        if (TheModeCycle = CalendarDays) then
            CCi = CCi - (SFCDecline/100) * exp(2*Ln(Dayi-L12SF))/(L123-L12SF)
        else
            if ((SumGDD > GDDL12SF) .and. (GDDL123 > GDDL12SF)) then
                CCi = CCi - (RatDGDD*SFCDecline/100)
                * exp(2*Ln(SumGDD-GDDL12SF))/(GDDL123-GDDL12SF)
            end if
        end if
        if (CCi < 0) then
            CCi = 0
        end if
    else
        if (TheModeCycle = CalendarDays) then
            CCi = CCatTime((L123-L0), CCo, (CGC*(1-SFRedCGC/100)), ((1-SFRedCCX/100)*CCx))
            ! CCibis is CC in late season when Canopy decline continues
            CCibis = CCi  - (SFCDecline/100) * (exp(2*Ln(Dayi-L12SF))/(L123-L12SF))
            if (CCibis < 0) then
                CCi = 0
            else
                CCi = CCi  - ((SFCDecline/100) * (L123-L12SF))
            end if
            if (CCi < 0.001) then
                CCi = 0
            else
                CCxAdj = CCi ! is CCx at start of late season, adjusted for canopy decline with soil fertility stress
                CDCadj = CDC * (CCxAdj + 2.29)/(CCx + 2.29)
                if (Dayi < (L123 + LengthCanopyDecline(CCxAdj, CDCadj))) then
                    CCi = CCxAdj * (1 - 0.05*(exp((Dayi-L123)*3.33*CDCadj/(CCxAdj+2.29))-1))
                    if (CCibis < CCi) then
                        CCi = CCibis ! accept smallest Canopy Cover
                    end if
                else
                    CCi = 0
                end if
            end if
        else
            CCi = CCatTime((GDDL123-GDDL0), CCo, (GDDCGC*(1-SFRedCGC/100)), ((1-SFRedCCX/100)*CCx))
            ! CCibis is CC in late season when Canopy decline continues
            if ((SumGDD > GDDL12SF) .and. (GDDL123 > GDDL12SF)) then
                CCibis = CCi  -
                (RatDGDD*SFCDecline/100) * (exp(2*Ln(SumGDD-GDDL12SF))/(GDDL123-GDDL12SF))
            else
                CCibis = CCi
            end if
            if (CCibis < 0) then
                CCi = 0
            else
                CCi = CCi - ((RatDGDD*SFCDecline/100) * (GDDL123-GDDL12SF))
            end if
            if (CCi < 0.001) then
                CCi = 0
            else
                CCxAdj = CCi ! is CCx at start of late season, adjusted for canopy decline with soil fertility stress
                GDDCDCadj = GDDCDC * (CCxAdj + 2.29)/(CCx + 2.29)
                if (SumGDD < (GDDL123 + LengthCanopyDecline(CCxAdj, GDDCDCadj))) then
                    CCi = CCxAdj * (1 - 0.05*(exp((SumGDD-GDDL123)*3.33*GDDCDCadj/(CCxAdj+2.29))-1))
                    if (CCibis < CCi) then
                        CCi = CCibis ! accept smallest Canopy Cover
                    end if
                else
                    CCi = 0
                end if
            end if
        end if
        if (CCi < 0) then
            CCi = 0
        end if
    end if
end if

CCiNoWaterStressSF = CCi
! CCiNoWaterStressSF 



real(dp) function SeasonalSumOfKcPot(TheDaysToCCini, TheGDDaysToCCini, L0, L12, L123, L1234, GDDL0, GDDL12, GDDL123, GDDL1234, CCo, CCx, CGC, GDDCGC, CDC, GDDCDC, KcTop, KcDeclAgeing, CCeffectProcent, Tbase, Tupper, TDayMin, TDayMax, GDtranspLow, CO2i, TheModeCycle)
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
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    real(dp), intent(in) :: GDtranspLow
    real(dp), intent(in) :: CO2i
    type(rep_modeCycle), intent(in) :: TheModeCycle
end function SeasonalSumOfKcPot

EToStandard = 5
real(dp) :: SumGDD, GDDi, SumKcPot, SumGDDforPlot, SumGDDfromDay1
real(dp) :: Tndayi, Txdayi, CCi, CCxWitheredForB, TpotForB, EpotTotForB
real(dp) :: CCinitial, DayFraction, GDDayFraction
integer(int32) :: DayCC, Tadj, GDDTadj
type(textFile) :: fTemp
integer(int32) :: Dayi
logical :: GrowthON

! 1. Open Temperature file
if (TemperatureFile /= '(None)') then
    Assign(fTemp, CONCAT(GetPathNameSimul(), 'TCrop%SIM'))
    Reset(fTemp)
end if

! 2. Initialise global settings
Simulation%DelayedDays = 0 ! required for CalculateETpot
SumKcPot = 0
SumGDDforPlot = undef_int
SumGDD = undef_int
SumGDDfromDay1 = 0
GrowthON = .false.
GDDTadj = undef_int
DayFraction = undef_int
GDDayFraction = undef_int
! 2.bis Initialise 1st day
if (TheDaysToCCini /= 0) then
    ! regrowth
    if (TheDaysToCCini = undef_int) then
        ! CCx on 1st day
        Tadj = L12 - L0
        if (TheModeCycle = GDDays) then
            GDDTadj = GDDL12 - GDDL0
            SumGDD = GDDL12
        end if
        CCinitial = CCx
    else
        ! CC on 1st day is < CCx
        Tadj = TheDaysToCCini
        DayCC = Tadj + L0
        if (TheModeCycle = GDDays) then
            GDDTadj = TheGDDaysToCCini
            SumGDD = GDDL0 + TheGDDaysToCCini
            SumGDDforPlot = SumGDD
        end if
        CCinitial = CanopyCoverNoStressSF(DayCC, L0, L123, L1234,
        GDDL0, GDDL123, GDDL1234, CCo, CCx, CGC, CDC,
        GDDCGC, GDDCDC, SumGDDforPlot, TheModeCycle, (0), (0))
    end if
    ! Time reduction for days between L12 and L123
    DayFraction = (L123-L12)/(Tadj + L0 + (L123-L12) )
    if (TheModeCycle = GDDays) then
        GDDayFraction = (GDDL123-GDDL12)/(GDDTadj + GDDL0 + (GDDL123-GDDL12))
    end if
else
    ! sowing or transplanting
    Tadj = 0
    if (TheModeCycle = GDDays) then
        GDDTadj = 0
        SumGDD = 0
    end if
    CCinitial = CCo
end if

! 3. Calculate Sum
do Dayi = 1, L1234
    ! 3.1 calculate growing degrees for the day
    if (TemperatureFile /= '(None)') then
        READLN(fTemp, Tndayi, Txdayi)
        GDDi = DegreesDay(Tbase, Tupper, Tndayi, Txdayi, SimulParam%GDDMethod)
    else
        GDDi = DegreesDay(Tbase, Tupper, TDayMin, TDayMax, SimulParam%GDDMethod)
    end if
    if (TheModeCycle = GDDays) then
        SumGDD = SumGDD + GDDi
        SumGDDfromDay1 = SumGDDfromDay1 + GDDi
    end if
    
    ! 3.2 calculate CCi
    if (GrowthON == .false.) then
        ! not yet canopy development
        CCi = 0
        DayCC = Dayi
        if (TheDaysToCCini /= 0) then
            ! regrowth on 1st day
            CCi = CCinitial
            GrowthON = .true.
        else
            ! wait for day of germination or recover of transplant
            if (TheModeCycle = CalendarDays) then
                if (Dayi = (L0+1)) then
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
        if (TheDaysToCCini = 0) then
            DayCC = Dayi
        else
            DayCC = Dayi + Tadj + L0 ! adjusted time scale
            if (DayCC > L1234) then
                DayCC = L1234 ! special case where L123 > L1234
            end if
            if (DayCC > L12) then
                if (Dayi <= L123) then
                    DayCC = L12 + nint(DayFraction * (Dayi+Tadj+L0 - L12)) ! slow down
                else
                    DayCC = Dayi ! switch time scale
                end if
            end if
        end if
        if (TheModeCycle = GDDays) then
            if (TheGDDaysToCCini = 0) then
                SumGDDforPlot = SumGDDfromDay1
            else
                SumGDDforPlot = SumGDD
                if (SumGDDforPlot > GDDL1234) then
                    SumGDDforPlot = GDDL1234 ! special case where L123 > L1234
                end if
                if (SumGDDforPlot > GDDL12) then
                    if (SumGDDfromDay1 <= GDDL123) then
                        SumGDDforPlot = GDDL12 + nint(GDDayFraction * (SumGDDfromDay1+GDDTadj+GDDL0 - GDDL12)) ! slow down
                    else
                        SumGDDforPlot = SumGDDfromDay1 ! switch time scale
                    end if
                end if
                CCi = CanopyCoverNoStressSF(DayCC, L0, L123, L1234, GDDL0, GDDL123, GDDL1234,
                CCo, CCx, CGC, CDC, GDDCGC, GDDCDC, SumGDDforPlot, TheModeCycle, (0), (0))
            end if
            
            ! 3.3 calculate CCxWithered
            CCxWitheredForB = CCi
            if (Dayi >= L12) then
                CCxWitheredForB = CCx
            end if
            
            ! 3.4 Calculate Tpot + Adjust for Low temperature (no transpiration)
            if (CCi > 0.0001) then
                CalculateETpot(DayCC, L0, L12, L123, L1234, (0),
                CCi, EToStandard, KcTop, KcDeclAgeing, CCx, CCxWitheredForB, CCeffectProcent, CO2i, GDDi, GDtranspLow, TpotForB, EpotTotForB)
            else
                TpotForB = 0
            end if
            
            ! 3.5 Sum of Sum Of KcPot
            SumKcPot = SumKcPot + (TpotForB/EToStandard)
        end if
        
        ! 5. Close Temperature file
        if (TemperatureFile /= '(None)') then
            Close(fTemp)
        end if
        
        ! 6. final sum
        SeasonalSumOfKcPot = SumKcPot
        
        ! SeasonalSumOfKcPot 
    end if





subroutine TranslateIniLayersToSWProfile(NrLay, LayThickness, LayVolPr, LayECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLay
    type(rep_IniComp), intent(in) :: LayThickness
    type(rep_IniComp), intent(in) :: LayVolPr
    type(rep_IniComp), intent(in) :: LayECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniLayersToSWProfile

integer(int8) :: Compi, Layeri, i
real(dp) :: SDLay, SDComp, FracC
logical :: GoOn

! from specific layers to Compartments
do Compi = 1, NrComp
    Comp(Compi)%Theta = 0
    Comp(Compi)%WFactor = 0  ! used for ECe in this procedure
end do
Compi = 0
SDComp = 0
Layeri = 1
SDLay = LayThickness(1)
GoOn = .true.
do while (Compi < NrComp)
    FracC = 0
    Compi = Compi + 1
    SDComp = SDComp + Comp(compi)%Thickness
    if (SDLay >= SDComp) then
        Comp(Compi)%Theta = Comp(Compi)%Theta + (1-FracC)*LayVolPr(Layeri)/100
        Comp(Compi)%WFactor = Comp(Compi)%WFactor + (1-FracC)*LayECdS(Layeri)
    else
        ! go to next layer
        do while ((SDLay < SDComp) .and. GoOn)
            ! finish handling previous layer
            FracC = (SDLay - (SDComp-Comp(Compi)%Thickness))/(Comp(Compi)%Thickness) - FracC
            Comp(Compi)%Theta = Comp(Compi)%Theta + FracC*LayVolPr(Layeri)/100
            Comp(Compi)%WFactor = Comp(Compi)%WFactor + FracC*LayECdS(Layeri)
            FracC = (SDLay - (SDComp-Comp(Compi)%Thickness))/(Comp(Compi)%Thickness)
            ! add next layer
            if (Layeri < NrLay) then
                Layeri = Layeri + 1
                SDLay = SDLay + LayThickness(Layeri)
            else
                GoOn = .false.
            end if
        end if
        Comp(Compi)%Theta = Comp(Compi)%Theta + (1-FracC)*LayVolPr(Layeri)/100
        Comp(Compi)%WFactor = Comp(Compi)%WFactor + (1-FracC)*LayECdS(Layeri)
    end if
    ! next Compartment
end if
if (NOT GoOn) then
    do i = (Compi+1), NrComp
        Comp(i)%Theta = LayVolPr(NrLay)/100
        Comp(i)%WFactor = LayECdS(NrLay)
    end do
    
    ! final check of SWC
    do Compi = 1, NrComp
        if (Comp(Compi)%Theta > (SoilLayer(Comp(compi)%Layer)%SAT)/100) then
            Comp(Compi)%Theta = (SoilLayer(Comp(compi)%Layer)%SAT)/100
        end if
        ! salt distribution in cellls
        do Compi = 1, NrComp
            DetermineSaltContent(Comp(Compi)%WFactor, Comp(Compi))
        end do
        ! TranslateIniLayersToSWProfile 
    end do



subroutine TranslateIniPointsToSWProfile(NrLoc, LocDepth, LocVolPr, LocECdS, NrComp, Comp)
    integer(int8), intent(in) :: NrLoc
    type(rep_IniComp), intent(in) :: LocDepth
    type(rep_IniComp), intent(in) :: LocVolPr
    type(rep_IniComp), intent(in) :: LocECdS
    integer(int32), intent(in) :: NrComp
    type(rep_Comp), intent(inout) :: Comp
end subroutine TranslateIniPointsToSWProfile

integer(int8) :: Compi, Loci
real(dp) :: TotD, Depthi, D1, D2, Th1, Th2, DTopComp, ThTopComp, ThBotComp
real(dp) :: EC1, EC2, ECTopComp, ECBotComp
logical :: AddComp, TheEnd
TotD = 0
do Compi = 1, NrComp
    Comp(Compi)%Theta = 0
    Comp(Compi)%WFactor = 0  ! used for salt in (10*VolSat*dZ * EC)
    TotD = TotD + Comp(Compi)%Thickness
end do
Compi = 0
Depthi = 0
AddComp = .true.
Th2 = LocVolPr(1)
EC2 = LocECds(1)
D2 = 0
Loci = 0
do while ((Compi < NrComp) .or. ((Compi = NrComp) .and. (AddComp == .false.)))
    ! upper and lower boundaries location
    D1 = D2
    Th1 = Th2
    EC1 = EC2
    if (Loci < NrLoc) then
        Loci = Loci + 1
        D2 = LocDepth(Loci)
        Th2 = LocVolPr(Loci)
        EC2 = LocECdS(Loci)
    else
        D2 = TotD
    end if
    ! transfer water to compartment (SWC in mm) and salt in (10*VolSat*dZ * EC)
    TheEnd = .false.
    DTopComp = D1  ! Depthi is the bottom depth
    ThBotComp = Th1
    ECBotComp = EC1
    REPEAT
    ThTopComp = ThBotComp
    ECTopComp = ECBotComp
    if (AddComp then
        Compi = Compi + 1
        Depthi = Depthi + Comp(Compi)%Thickness
    end if
    if (Depthi < D2) then
        ThBotComp = Th1 + (Th2-Th1)*(Depthi-D1)/(D2-D1)
        Comp(Compi)%Theta = Comp(Compi)%Theta
        + 10*(Depthi-DTopComp)*((ThTopComp+ThBotComp)/2)
        ECBotComp = EC1 + (EC2-EC1)*(Depthi-D1)/(D2-D1)
        Comp(Compi)%WFactor = Comp(Compi)%WFactor
        + (10*(Depthi-DTopComp)*SoilLayer(Comp(Compi)%Layer)%SAT)*((ECTopComp+ECbotComp)/2)
        AddComp = .true.
        DTopComp = Depthi
        if (Compi = NrComp) then
            TheEnd = .true.
        end if
    else
        ThBotComp = Th2
        ECBotComp = EC2
        Comp(Compi)%Theta = Comp(Compi)%Theta
        + 10*(D2-DTopComp)*((ThTopComp+ThBotComp)/2)
        Comp(Compi)%WFactor = Comp(Compi)%WFactor
        + (10*(D2-DTopComp)*SoilLayer(Comp(Compi)%Layer)%SAT)*((ECTopComp+ECbotComp)/2)
        if (Depthi = D2) then
            AddComp = .true.
        else
            AddComp = .false.
        end if
        TheEnd = .true.
    end if
    UNTIL TheEnd
end if

do Compi = 1, NrComp
    ! from mm(water) to theta and final check
    Comp(Compi)%Theta = Comp(Compi)%Theta/(1000*Comp(Compi)%Thickness)
    if (Comp(Compi)%Theta > (SoilLayer(Comp(compi)%Layer)%SAT)/100) then
        Comp(Compi)%Theta = (SoilLayer(Comp(compi)%Layer)%SAT)/100
    end if
    if (Comp(Compi)%Theta < 0) then
        Comp(Compi)%Theta = 0
    end if
end if

do Compi = 1, NrComp
    ! from (10*VolSat*dZ * EC) to ECe and distribution in cellls
    Comp(Compi)%WFactor = Comp(Compi)%WFactor/(10*Comp(Compi)%Thickness*SoilLayer(Comp(Compi)%Layer)%SAT)
    DetermineSaltContent(Comp(Compi)%WFactor, Comp(Compi))
end do
! TranslateIniPointsToSWProfile 


subroutine LoadInitialConditions(SWCiniFileFull, IniSurfaceStorage, IniSWCRead)
    character(len=STRING_LENGTH), intent(in) :: SWCiniFileFull
    real(dp), intent(inout) :: IniSurfaceStorage
    type(rep_IniSWC), intent(inout) :: IniSWCRead
end subroutine LoadInitialConditions

type(TextFile) :: f0
integer(int8) :: i
character(len=STRING_LENGTH) :: StringParam
real(dp) :: VersionNr
Assign(f0, SWCiniFileFull)
Reset(f0)
READLN(f0, SWCiniDescription)
READLN(f0, VersionNr) ! AquaCrop Version
if (nint(10*VersionNr) < 41) then ! initial CC at start of simulation period
    Simulation%CCini = undef_int
else
    READLN(f0, Simulation%CCini)
end if
if (nint(10*VersionNr) < 41) then ! B produced before start of simulation period
    Simulation%Bini = 0.000
else
    READLN(f0, Simulation%Bini)
end if
if (nint(10*VersionNr) < 41) then ! initial rooting depth at start of simulation period
    Simulation%Zrini = undef_int
else
    READLN(f0, Simulation%Zrini)
end if
READLN(f0, IniSurfaceStorage)
if (nint(10*VersionNr) < 32) then ! EC of the ini surface storage
    Simulation%ECStorageIni = 0
else
    READLN(f0, Simulation%ECStorageIni)
end if
READLN(f0, i)
if (i = 1) then
    IniSWCRead%AtDepths = .true.
else
    IniSWCRead%AtDepths = .false.
end if
READLN(f0, IniSWCRead%NrLoc)
READLN(f0)
READLN(f0)
READLN(f0)
do i = 1, IniSWCRead%NrLoc
    READLN(f0, StringParam)
    if (nint(10*VersionNr) < 32) then ! ECe at the locations
        SplitStringInTwoParams(StringParam, IniSWCRead%Loc(i), IniSWCRead%VolProc(i))
        IniSWCRead%SaltECe(i) = 0
    else
        SplitStringInThreeParams(StringParam, IniSWCRead%Loc(i), IniSWCRead%VolProc(i), IniSWCRead%SaltECe(i))
    end if
end if
Close(f0)
Simulation%IniSWC%AtFC = .false.
! LoadInitialConditions 


subroutine LoadProjectDescription(FullNameProjectFile, DescriptionOfProject)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    character(len=STRING_LENGTH), intent(inout) :: DescriptionOfProject
end subroutine LoadProjectDescription

type(TextFile) :: f0
Assign(f0, FullNameProjectFile)
Reset(f0)
READLN(f0, DescriptionOfProject)
DescriptionOfProject = Trim(DescriptionOfProject)
Close(f0)
! LoadProjectDescription 


subroutine ComposeOutputFileName(TheProjectFileName)
    character(len=STRING_LENGTH), intent(in) :: TheProjectFileName
end subroutine ComposeOutputFileName

character(len=STRING_LENGTH) :: TempString
integer(int8) :: i
TempString = Trim(TheProjectFileName)
i = Length(TempString)
Delete(TempString, (i-3), (4))
OutputName = TempString
! ComposeOutputFileName 

subroutine CheckForKeepSWC(FullNameProjectFile, TotalNrOfRuns, RunWithKeepSWC, ConstZrxForRun)
    character(len=STRING_LENGTH), intent(in) :: FullNameProjectFile
    integer(int32), intent(in) :: TotalNrOfRuns
    logical, intent(inout) :: RunWithKeepSWC
    real(dp), intent(inout) :: ConstZrxForRun
end subroutine CheckForKeepSWC

type(TextFile) :: f0, fx
integer(int32) :: i, Runi
character(len=STRING_LENGTH) :: FileName, PathName, FullFileName
real(dp) :: Zrni, Zrxi, ZrSoili
real(dp) :: VersionNrCrop
integer(int8) :: TheNrSoilLayers
type(rep_SoilLayer) :: TheSoilLayer
character(len=STRING_LENGTH) :: PreviousProfFilefull

! 1. Initial settings
RunWithKeepSWC = .false.
ConstZrxForRun = undef_int

! 2. Open project file
Assign(f0, FullNameProjectFile)
Reset(f0)
READLN(f0) ! Description
READLN(f0)  ! AquaCrop version Nr
do i = 1, 5
    READLN(f0) ! Type Year, and Simulation and Cropping period of run 1
end do

! 3. Look for restrictive soil layer
! restricted to run 1 since with KeepSWC, the soil file has to be common between runs
PreviousProfFilefull = GetProfFilefull() ! keep name soil file (to restore after check)
do i = 1, 27
    READLN(f0) ! Climate (5x3 = 15),Calendar (3),Crop (3), Irri (3) and Field (3) file
end do
READLN(f0) ! info Soil file
READLN(f0, FileName)
READLN(f0, PathName)
PathName = StringReplace(PathName, '"', '', (rfReplaceAll))
FullFileName = CONCAT(Trim(PathName), Trim(FileName))
LoadProfile(FullFileName)
TheNrSoilLayers = GetSoil()%NrSoilLayers
TheSoilLayer = SoilLayer
! ZrRestrict := 1000; ! assumed not to be so far a restriction
!
! Assign(fx,FullFileName);
! Reset(fx);
! FOR i := 1 TO 5 DO
!     READLN(fx);
! end do
! READLN(fx,ZrRestrict); // depth restrictive soil layer inhibiting root zone expansion
! IF (ZrRestrict < 0) THEN
!     ZrRestrict := 1000 // undefined, HENCE very large (no restriction)
! ELSEIF (ZrRestrict <= 1.20) THEN
!     ConstZrxForRun := ZrRestrict;
! end if
! Close(fx); *)

! 3.bis  groundwater file
do i = 1, 3
    READLN(f0)
end do

! 4. Check if runs with KeepSWC exist
Runi = 1
do while (RunWithKeepSWC == .false.) .and. (Runi <= TotalNrOfRuns)
    if (Runi > 1) then
        do i = 1, 47
            READLN(f0)  ! 5 + 42 lines with files
        end do
        READLN(f0) ! info Initial Conditions file
    end do
    READLN(f0, FileName)
    READLN(f0) ! Pathname
    if (Trim(FileName) = 'KeepSWC') then
        RunWithKeepSWC = .true.
    end if
    Runi = Runi + 1
end if
if (RunWithKeepSWC == .false.) then
    ConstZrxForRun = undef_int ! reset
end if

! 5. Look for maximum root zone depth IF RunWithKeepSWC
! IF (RunWithKeepSWC AND (ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict))) THEN
if (RunWithKeepSWC == .true.) then
    Reset(f0)
    READLN(f0) ! Description
    READLN(f0)  ! AquaCrop version Nr
    do i = 1, 5
        READLN(f0) ! Type Year, and Simulation and Cropping period of run 1
    end do
    Runi = 1
    ! WHILE ((ROUND(100*ConstZrxForRun) < ROUND(100*ZrRestrict)) AND (Runi <= TotalNrOfRuns)) DO
    do while (Runi <= TotalNrOfRuns)
        ! Simulation and Cropping period
        if (Runi > 1) then
            do i = 1, 5
                READLN(f0)
            end do
            ! 5 Climate files (15) + Calendar file (3)
            do i = 1, 18
                READLN(f0)
            end do
            ! Crop file
            READLN(f0) ! Crop file title
        end do
        READLN(f0, FileName)
        READLN(f0, PathName)
        PathName = StringReplace(PathName, '"', '', (rfReplaceAll))
        FullFileName = CONCAT(Trim(PathName), Trim(FileName))
        Assign(fx, FullFileName)
        Reset(fx)
        READLN(fx) ! description
        READLN(fx, VersionNrCrop)
        if (Round(VersionNrCrop*10) <= 31) then
            do i = 1, 29
                READLN(fx)  ! no Salinity stress (No Reponse Stomata + ECemin + ECemax + ShapeKsSalt)
            else
                if (Round(VersionNrCrop*10) <= 50) then
                    do i = 1, 32
                        READLN(fx) ! no distortion to salinity and response to ECsw factor
                    else
                        do i = 1, 34
                            READLN(fx)
                        end do
                    end do
                    READLN(fx, Zrni) ! minimum rooting depth
                end do
                READLN(fx, Zrxi) ! maximum rooting depth
                ZrSoili = RootMaxInSoilProfile(Zrxi, TheNrSoilLayers, TheSoilLayer)
                if (ZrSoili > ConstZrxForRun) then
                    ConstZrxForRun = ZrSoili
                end if
                Close(fx)
                ! Remaining files: Irri (3), Field (3), Soil (3), Gwt (3), Inni (3), Off (3) and FieldData (3) file
                do i = 1, 21
                    READLN(f0)
                end do
                Runi = Runi + 1
            end do
        end do
        Close(f0)
        
        ! 6. Reload existing soil file
        SetProfFilefull(PreviousProfFilefull)
        LoadProfile(GetProfFilefull())
        ! CheckForKeepSWC 
    end do




subroutine AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
    integer(int8), intent(in) :: PrevNrComp
    type(rep_IniComp), intent(in) :: PrevThickComp
    type(rep_IniComp), intent(in) :: PrevVolPrComp
    type(rep_IniComp), intent(in) :: PrevECdSComp
end subroutine AdjustThetaInitial

integer(int32) :: layeri, compi
real(dp) :: TotDepthC, TotDepthL, Total

! 1. Actual total depth of compartments
TotDepthC = 0
do compi = 1, NrCompartments
    TotDepthC = TotDepthC + Compartment(compi)%Thickness
end do

! 2. Stretch thickness of bottom soil layer if required
TotDepthL = 0
do layeri = 1 to GetSoil()%NrSoilLayers
    TotDepthL = TotDepthL + SoilLayer(layeri)%Thickness
end do
if (TotDepthC > TotDepthL) then
    SoilLayer(GetSoil()%NrSoilLayers)%Thickness = SoilLayer(GetSoil()%NrSoilLayers)%Thickness + (TotDepthC - TotDepthL)
end if

! 3. Assign a soil layer to each soil compartment
DesignateSoilLayerToCompartments(NrCompartments, GetSoil()%NrSoilLayers, Compartment)

! 4. Adjust initial Soil Water Content of soil compartments
if (Simulation%ResetIniSWC then
    if (Simulation%IniSWC%AtDepths then
        TranslateIniPointsToSWProfile(Simulation%IniSWC%NrLoc, Simulation%IniSWC%Loc, Simulation%IniSWC%VolProc,
        Simulation%IniSWC%SaltECe, NrCompartments, Compartment)
    else
        TranslateIniLayersToSWProfile(Simulation%IniSWC%NrLoc, Simulation%IniSWC%Loc, Simulation%IniSWC%VolProc,
        Simulation%IniSWC%SaltECe, NrCompartments, Compartment)
    end if
else
    TranslateIniLayersToSWProfile(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp, NrCompartments, Compartment)
end if

! 5. Adjust watercontent in soil layers and determine ThetaIni
Total = 0
do layeri = 1, GetSoil()%NrSoilLayers
    SoilLayer(layeri)%WaterContent = 0
end do
do compi = 1, NrCompartments
    Simulation%ThetaIni(compi) = Compartment(compi)%Theta
    SoilLayer(Compartment(compi)%Layer)%WaterContent = SoilLayer(Compartment(compi)%Layer)%WaterContent
    + Simulation%ThetaIni(compi)*100*10*Compartment(compi)%Thickness
end do
do layeri = 1, GetSoil()%NrSoilLayers
    Total = Total + SoilLayer(layeri)%WaterContent
end do
SetTotalWaterContent_BeginDay(Total)
! AdjustThetaInitial 




subroutine AdjustSizeCompartments(CropZx)
    real(dp), intent(in) :: CropZx
end subroutine AdjustSizeCompartments

integer(int32) :: i , compi
real(dp) :: TotDepthC, fAdd
integer(int8) :: PrevNrComp
type(rep_IniComp) :: PrevThickComp, PrevVolPrComp, PrevECdSComp

! 1. Save intial soil water profile (required when initial soil water profile is NOT reset at start simulation - see 7.)
PrevNrComp = NrCompartments
do compi = 1, prevnrComp
    PrevThickComp(compi) = Compartment(compi)%Thickness
    PrevVolPrComp(compi) = 100*Compartment(compi)%Theta
end do

! 2. Actual total depth of compartments
TotDepthC = 0
do i = 1, NrCompartments
    TotDepthC = TotDepthC + Compartment(i)%Thickness
end do

! 3. Increase number of compartments (if less than 12)
if (NrCompartments < 12) then
    REPEAT
    NrCompartments = NrCompartments + 1
end if
if ((CropZx - TotDepthC) > SimulParam%CompDefThick) then
    Compartment(NrCompartments)%Thickness = SimulParam%CompDefThick
else
    Compartment(NrCompartments)%Thickness = CropZx - TotDepthC
end if
TotDepthC = TotDepthC + Compartment(NrCompartments)%Thickness
UNTIL ((NrCompartments = max_No_compartments) .or. ((TotDepthC + 0.00001) >= CropZx))

! 4. Adjust size of compartments (if total depth of compartments < rooting depth)
if ((TotDepthC + 0.00001) < CropZx) then
    NrCompartments = 12
    fAdd = (CropZx/0.1 - 12)/78
    do i = 1, 12
        Compartment(i)%Thickness = 0.1 * (1 + i*fAdd)
        Compartment(i)%Thickness = 0.05 * nint(Compartment(i)%Thickness * 20)
    end do
    TotDepthC = 0
    do i = 1, NrCompartments
        TotDepthC = TotDepthC + Compartment(i)%Thickness
    end do
    if (TotDepthC < CropZx) then
        REPEAT
        Compartment(12)%Thickness = Compartment(12)%Thickness + 0.05
    end if
    TotDepthC = TotDepthC + 0.05
    UNTIL (TotDepthC >= CropZx)
else
    do while ((TotDepthC - 0.04999999) >= CropZx)
        Compartment(12)%Thickness = Compartment(12)%Thickness - 0.05
        TotDepthC = TotDepthC - 0.05
    end do
end do

! 5. Adjust soil water content and theta initial
AdjustThetaInitial(PrevNrComp, PrevThickComp, PrevVolPrComp, PrevECdSComp)
! AdjustSizeCompartments 


subroutine CheckForWaterTableInProfile(DepthGWTmeter, ProfileComp, WaterTableInProfile)
    real(dp), intent(in) :: DepthGWTmeter
    type(rep_comp), intent(in) :: ProfileComp
    logical, intent(inout) :: WaterTableInProfile
end subroutine CheckForWaterTableInProfile

real(dp) :: Ztot, Zi
integer(int32) :: compi
WaterTableInProfile = .false.
Ztot = 0
compi = 0
if (DepthGWTmeter >= 0) then
    ! groundwater table is present
    REPEAT
    compi = compi + 1
end if
Ztot = Ztot + ProfileComp(compi)%Thickness
Zi = Ztot - ProfileComp(compi)%Thickness/2
if (Zi >= DepthGWTmeter) then
    WaterTableInProfile = .true.
end if
UNTIL ((WaterTableInProfile == .true.) .or. (compi >= NrCompartments))
! CheckForWaterTableInProfile 




subroutine LoadGroundWater(FullName, AtDayNr, Zcm, ECdSm)
    character(len=STRING_LENGTH), intent(in) :: FullName
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine LoadGroundWater

type(TextFile) :: f0
integer(int32) :: i, dayi, monthi, yeari, Year1Gwt
integer(int32) :: DayNr1Gwt, DayNr1, DayNr2, DayNrN
type(ShortString) :: StringREAD
real(dp) :: DayDouble, Z1, EC1, Z2, EC2, ZN, ECN
logical :: TheEnd

subroutine FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
    integer(int32), intent(in) :: AtDayNr
    integer(int32), intent(in) :: DayNr1
    integer(int32), intent(in) :: DayNr2
    real(dp), intent(in) :: Z1
    real(dp), intent(in) :: EC1
    real(dp), intent(in) :: Z2
    real(dp), intent(in) :: EC2
    integer(int32), intent(inout) :: Zcm
    real(dp), intent(inout) :: ECdSm
end subroutine FindValues

Zcm = nint(100 * (Z1 + (Z2-Z1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1)))
ECdSm = EC1 + (EC2-EC1)*(AtDayNr-DayNr1)/(DayNr2-Daynr1)
! FindValues 

! initialize
TheEnd = .false.
Year1Gwt = 1901
DayNr1 = 1
DayNr2 = 1
Assign(f0, FullName)
Reset(f0)
READLN(f0, GroundWaterDescription)
READLN(f0) ! AquaCrop Version

! mode groundwater table
READLN(f0, i)
CASE i OF
    0 :
    ! no groundwater table
    Zcm = undef_int
    ECdSm = undef_int
    SimulParam%ConstGwt = .true.
    TheEnd = .true.
end case
1 :
! constant groundwater table
SimulParam%ConstGwt = .true.
else
SimulParam%ConstGwt = .false.

! first day of observations (only for variable groundwater table)
if (NOT SimulParam%ConstGwt) then
    READLN(f0, dayi)
    READLN(f0, monthi)
    READLN(f0, Year1Gwt)
    DetermineDayNr(dayi, monthi, Year1Gwt, DayNr1Gwt)
end if

! single observation (Constant Gwt) or first observation (Variable Gwt)
if (i > 0) then
    ! groundwater table is present
    READLN(f0)
    READLN(f0)
    READLN(f0)
    READLN(f0, StringREAD)
    SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
    if ((i = 1) .or. (Eof(f0))) then
        ! Constant groundwater table or single observation
        Zcm = nint(100*Z2)
        ECdSm = EC2
        TheEnd = .true.
    else
        DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
    end if
end if

! other observations
if (NOT TheEnd) then
    ! variable groundwater table with more than 1 observation
    ! adjust AtDayNr
    DetermineDate(AtDayNr, dayi, monthi, yeari)
    if ((yeari = 1901) .and. (Year1Gwt /= 1901)) then
        ! Make AtDayNr defined
        DetermineDayNr(dayi, monthi, Year1Gwt, AtDayNr)
    end if
    if ((yeari /= 1901) .and. (Year1Gwt = 1901)) then
        ! Make AtDayNr undefined
        DetermineDayNr(dayi, monthi, Year1Gwt, AtDayNr)
    end if
    ! get observation at AtDayNr
    if (Year1Gwt /= 1901) then
        ! year is defined
        if (AtDayNr <= DayNr2) then
            Zcm = nint(100*Z2)
            ECdSm = EC2
        else
            do while (NOT TheEnd)
                DayNr1 = DayNr2
                Z1 = Z2
                EC1 = EC2
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
                DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
                if (AtDayNr <= DayNr2) then
                    FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
                    TheEnd = .true.
                end if
                if ((Eof(f0)) .and. (NOT TheEnd)) then
                    Zcm = nint(100*Z2)
                    ECdSm = EC2
                    TheEnd = .true.
                end if
            end if
        end if
    else
        ! year is undefined
        if (AtDayNr <= DayNr2) then
            DayNr2 = DayNr2 + 365
            AtDayNr = AtDayNr + 365
            do while (NOT Eof(f0))
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z1, EC1)
                DayNr1 = DayNr1Gwt + nint(DayDouble) - 1
            end do
            FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
        else
            DayNrN = DayNr2 + 365
            ZN = Z2
            ECN = EC2
            do while (NOT TheEnd)
                DayNr1 = DayNr2
                Z1 = Z2
                EC1 = EC2
                READLN(f0, StringREAD)
                SplitStringInThreeParams(StringREAD, DayDouble, Z2, EC2)
                DayNr2 = DayNr1Gwt + nint(DayDouble) - 1
                if (AtDayNr <= DayNr2) then
                    FindValues(AtDayNr, DayNr1, DayNr2, Z1, EC1, Z2, EC2, Zcm, ECdSm)
                    TheEnd = .true.
                end if
                if ((Eof(f0)) .and. (NOT TheEnd)) then
                    FindValues(AtDayNr, DayNr2, DayNrN, Z2, EC2, ZN, ECN, Zcm, ECdSm)
                    TheEnd = .true.
                end if
            end if
        end if
    end if
    ! variable groundwater table with more than 1 observation
end if
Close(f0)
! LoadGroundWater 


real(dp) function CCmultiplierWeedAdjusted(ProcentWeedCover, CCxCrop, FshapeWeed, fCCx, Yeari, MWeedAdj, RCadj)
    integer(int8), intent(in) :: ProcentWeedCover
    real(dp), intent(in) :: CCxCrop
    real(dp), intent(in) :: FshapeWeed
    real(dp), intent(in) :: fCCx
    integer(int8), intent(in) :: Yeari
    integer(int8), intent(in) :: MWeedAdj
    integer(int8), intent(inout) :: RCadj
end function CCmultiplierWeedAdjusted

real(dp) :: fWeedi, CCxTot100, CCxTot0, CCxTotM, fweedMax, RCadjD, FshapeMinimum

fWeedi = 1
RCadj = ProcentWeedCover
if (ProcentWeedCover > 0) then
    fweedi = CCmultiplierWeed(ProcentWeedCover, CCxCrop, FshapeWeed)
    ! FOR perennials when self-thinning
    if ((Crop%subkind = Forage) .and. (Yeari > 1) and (fCCx < 0.995)) then
        ! need for adjustment
        ! step 1 - adjusment of shape factor to degree of crop replacement by weeds
        FshapeMinimum = 10 - 20*( (exp(fCCx*3)-1)/(exp(3)-1) + sqr(MWeedAdj/100))
        if (nint(FshapeMinimum*10) = 0) then
            FshapeMinimum = 0.1
        end if
        if (FshapeWeed < FshapeMinimum) then
            FshapeWeed = FshapeMinimum
        end if
        
        ! step 2 - Estimate of CCxTot
        ! A. Total CC (crop and weeds) when self-thinning and 100% weed take over
        fweedi = CCmultiplierWeed(ProcentWeedCover, CCxCrop, FshapeWeed)
        CCxTot100 = fweedi * CCxCrop
        ! B. Total CC (crop and weeds) when self-thinning and 0% weed take over
        if (fCCx > 0.005) then
            fweedi = CCmultiplierWeed(nint(fCCx*ProcentWeedCover), (fCCx*CCxCrop), FshapeWeed)
        else
            fweedi = 1
        end if
        CCxTot0 = fweedi * (fCCx*CCxCrop)
        ! C. total CC (crop and weeds) with specified weed take over (MWeedAdj)
        CCxTotM = CCxTot0 + (CCxTot100 - CCxTot0)* MWeedAdj/100
        if (CCxTotM < (fCCx*CCxCrop*(1-ProcentWeedCover/100))) then
            CCxTotM = fCCx*CCxCrop*(1-ProcentWeedCover/100)
        end if
        if (fCCx > 0.005) then
            fweedi = CCxTotM/(fCCx*CCxCrop)
            fweedMax = 1/(fCCx*CCxCrop)
            if (nint(fweedi*1000) > nint(fWeedMax*1000)) then
                fweedi = fweedMax
            end if
        end if
        
        ! step 3 - Estimate of adjusted weed cover
        RCadjD = ProcentWeedCover + (1-fCCx)*CCxCrop*MWeedAdj
        if (fCCx > 0.005) then
            if (RCadjD < (100*(CCxTotM - fCCx*CCxCrop)/CCxTotM)) then
                RCadjD = 100*(CCxTotM - fCCx*CCxCrop)/CCxTotM
            end if
            if (RCadjD > (100 * (1- (fCCx*CCxCrop*(1-ProcentWeedCover/100)/CCxTotM)))) then
                RCadjD = 100*(1- fCCx*CCxCrop*(1-ProcentWeedCover/100)/CCxTotM)
            end if
        end if
        RCadj = nint(RCadjD)
        if (RCadj > 100) then
            RCadj = 100
        end if
    end if
end if
CCmultiplierWeedAdjusted = fWeedi
! CCmultiplierWeedAdjusted 


subroutine AdjustYearPerennials(TheYearSeason, Sown1stYear, TheCycleMode, Zmax, ZminYear1, TheCCo, TheSizeSeedling, TheCGC, TheCCx, TheGDDCGC, ThePlantingDens, TypeOfPlanting, Zmin, TheSizePlant, TheCCini, TheDaysToCCini, TheGDDaysToCCini)
    integer(int8), intent(in) :: TheYearSeason
    logical, intent(in) :: Sown1stYear
    type(rep_modeCycle), intent(in) :: TheCycleMode
    real(dp), intent(in) :: Zmax
    real(dp), intent(in) :: ZminYear1
    real(dp), intent(in) :: TheCCo
    real(dp), intent(in) :: TheSizeSeedling
    real(dp), intent(in) :: TheCGC
    real(dp), intent(in) :: TheCCx
    real(dp), intent(in) :: TheGDDCGC
    integer(int32), intent(in) :: ThePlantingDens
    type(rep_Planting), intent(inout) :: TypeOfPlanting
    real(dp), intent(inout) :: Zmin
    real(dp), intent(inout) :: TheSizePlant
    real(dp), intent(inout) :: TheCCini
    integer(int32), intent(inout) :: TheDaysToCCini
    integer(int32), intent(inout) :: TheGDDaysToCCini
end subroutine AdjustYearPerennials

if (TheYearSeason = 1) then
    if (Sown1stYear == .true.) then ! planting
        TypeOfPlanting = Seed
    else
        TypeOfPlanting = Transplant
    end if
    Zmin = ZminYear1  ! rooting depth
else
    TypeOfPlanting = Regrowth ! planting
    Zmin = Zmax  ! rooting depth
    ! plant size by regrowth
    if (nint(100*TheSizePlant) < nint(100*TheSizeSeedling)) then
        TheSizePlant = 10 * TheSizeSeedling
    end if
    if (nint(100*TheSizePlant) > nint((100*TheCCx*10000)/(ThePlantingDens/10000))) then
        TheSizePlant = (TheCCx*10000)/(ThePlantingDens/10000) ! adjust size plant to maximum possible
    end if
end if
TheCCini = (ThePlantingDens/10000) * (TheSizePlant/10000)
TheDaysToCCini = TimeToCCini(TypeOfPlanting, ThePlantingDens, TheSizeSeedling, TheSizePlant, TheCCx, TheCGC)
if (TheCycleMode = GDDays) then
    TheGDDaysToCCini = TimeToCCini(TypeOfPlanting, ThePlantingDens, TheSizeSeedling, TheSizePlant, TheCCx, TheGDDCGC)
else
    TheGDDaysToCCini = undef_int
end if
! AdjustYearPerennials 




subroutine NoCropCalendar()

end subroutine NoCropCalendar

SetCalendarFile('(None)')
SetCalendarFileFull(GetCalendarFile())  ! no file 
CalendarDescription = ''
Onset%GenerateOn = .false.
Onset%GenerateTempOn = .false.
EndSeason%GenerateTempOn = .false.
CalendarDescription = 'No calendar for the Seeding/Planting year'
! NoCropCalendar 



subroutine LoadCropCalendar(FullName, GetOnset, GetOnsetTemp, DayNrStart, YearStart)
    character(len=STRING_LENGTH), intent(in) :: FullName
    logical, intent(inout) :: GetOnset
    logical, intent(inout) :: GetOnsetTemp
    integer(int32), intent(inout) :: DayNrStart
    integer(int32), intent(in) :: YearStart
end subroutine LoadCropCalendar


type(TextFile) :: f0
integer(int8) :: Onseti
integer(int32) :: Dayi, Monthi, Yeari, CriterionNr
integer(int32) :: DayNr
GetOnset = .false.
GetOnsetTemp = .false.
Assign(f0, FullName)
Reset(f0)
READLN(f0, CalendarDescription)
READLN(f0) ! AquaCrop Version

! Specification of Onset and End growing season
READLN(f0, Onseti)  ! specification of the onset

! Onset growing season
if (Onseti = 0) then
    ! onset on a specific day
    READLN(f0) ! start search period - not applicable
    READLN(f0) ! length search period - not applicable
    READLN(f0, DayNr) ! day-number
    DetermineDate(DayNr, Dayi, Monthi, Yeari)
    DetermineDayNr(Dayi, Monthi, YearStart, DayNrStart)
else
    ! onset is generated
    GetOnset = .true.
    READLN(f0) ! start search period
    READLN(f0) ! length search period
    READLN(f0, CriterionNr) ! criterion number to decide if based on rainfall or air temperature
    if (CriterionNr > 10) then
        GetOnsetTemp = .true.
    end if
end if
Close(f0)
! LoadCropCalendar 


subroutine GetFileForProgramParameters(TheFullFileNameProgram, FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: TheFullFileNameProgram
    character(len=STRING_LENGTH), intent(inout) :: FullFileNameProgramParameters
end subroutine GetFileForProgramParameters

integer(int32) :: TheLength
character(len=STRING_LENGTH) :: TheExtension
FullFileNameProgramParameters = ''
TheLength = Length(TheFullFileNameProgram)
TheExtension = Copy(TheFullFileNameProgram, (TheLength-2), 3) ! PRO or PRM
FullFileNameProgramParameters = Copy(TheFullFileNameProgram, 1, (TheLength-3))
if (TheExtension = 'PRO') then
    FullFileNameProgramParameters = CONCAT(FullFileNameProgramParameters, 'PP1')
else
    FullFileNameProgramParameters = CONCAT(FullFileNameProgramParameters, 'PPn')
end if
! GetFileForProgramParameters 


subroutine LoadProgramParametersProject(FullFileNameProgramParameters)
    character(len=STRING_LENGTH), intent(in) :: FullFileNameProgramParameters
end subroutine LoadProgramParametersProject

type(TextFile) :: f0
integer(int32) :: i
if (FileExists(FullFileNameProgramParameters) then
    ! load set of program parameters
    Assign(f0, FullFileNameProgramParameters)
    Reset(f0)
    WITH SimulParam
    ! crop
    READLN(f0, EvapDeclineFactor) ! evaporation decline factor in stage 2
    READLN(f0, KcWetBare) ! Kc wet bare soil [-]
    READLN(f0, PercCCxHIfinal) ! CC threshold below which HI no longer increase(% of 100)
    READLN(f0, RootPercentZmin) ! Starting depth of root sine function (% of Zmin)
    READLN(f0, MaxRootZoneExpansion) ! cm/day
    MaxRootZoneExpansion = 5.00 ! fixed at 5 cm/day
    READLN(f0, KsShapeFactorRoot) ! Shape factor for effect water stress on rootzone expansion
    READLN(f0, TAWGermination)  ! Soil water content (% TAW) required at sowing depth for germination
    READLN(f0, pAdjFAO) ! Adjustment factor for FAO-adjustment soil water depletion (p) for various ET
    READLN(f0, DelayLowOxygen) ! number of days for full effect of deficient aeration
    READLN(f0, ExpFsen) ! exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
    READLN(f0, Beta) ! Decrease (percentage) of p(senescence) once early canopy senescence is triggered
    READLN(f0, ThicknessTopSWC)  ! Thickness top soil (cm) in which soil water depletion has to be determined
    ! field
    READLN(f0, EvapZmax) ! maximum water extraction depth by soil evaporation [cm]
    ! soil
    READLN(f0, SimulParam%RunoffDepth) ! considered depth (m) of soil profile for calculation of mean soil water content
    READLN(f0, i)   ! correction CN for Antecedent Moisture Class
    if (i = 1) then
        SimulParam%CNcorrection = .true.
    else
        SimulParam%CNcorrection = .false.
    end if
    READLN(f0, SimulParam%SaltDiff) ! salt diffusion factor (%)
    READLN(f0, SimulParam%SaltSolub) ! salt solubility (g/liter)
    READLN(f0, SimulParam%RootNrDF) ! shape factor capillary rise factor
    SimulParam%IniAbstract = 5 ! fixed in Version 5.0 cannot be changed since linked with equations for CN AMCII and CN converions
    ! Temperature
    READLN(f0, Tmin)   ! Default minimum temperature (degC) if no temperature file is specified
    READLN(f0, Tmax)   ! Default maximum temperature (degC) if no temperature file is specified
    READLN(f0, GDDMethod) ! Default method for GDD calculations
    if (GDDMethod > 3) then
        GDDMethod = 3
    end if
    if (GDDMethod < 1) then
        GDDMethod = 1
    end if
    ! Rainfall
    READLN(f0, i)
    Case i OF
    0 : EffectiveRain%Method = Full
    1 : EffectiveRain%Method = USDA
    2 : EffectiveRain%Method = Percentage
end if
READLN(f0, EffectiveRain%PercentEffRain) ! IF Method is Percentage
READLN(f0, EffectiveRain%ShowersInDecade)  ! For estimation of surface run-off
READLN(f0, EffectiveRain%RootNrEvap) ! For reduction of soil evaporation
! close
Close(f0)
else
! take the default set of program parameters
ReadSoilSettings
ReadRainfallSettings
ReadCropSettingsParameters
ReadFieldSettingsParameters
ReadTemperatureSettingsParameters
! LoadProgramParametersProject 


end%
