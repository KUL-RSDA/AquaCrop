module ac_global

use ac_kinds, only: dp, &
                    int8, &
                    int16, &
                    int32, &
                    intEnum, &
                    sp
use iso_fortran_env, only: iostat_end

implicit none


integer(int32), parameter :: max_SoilLayers = 5
real(dp), parameter :: undef_double = -9.9_dp
    !! value for 'undefined' real(dp) variables
integer(int32), parameter :: undef_int = -9
    !! value for 'undefined' int32 variables
real(dp), parameter :: PI = 3.1415926535_dp
real(dp), parameter :: CO2Ref = 369.41_dp; 
    !! reference CO2 in ppm by volume for year 2000 for Mauna Loa
    !! (Hawaii,USA)
real(dp), parameter :: eps =10E-08
real(dp), dimension(12), parameter :: ElapsedDays = [0._dp, 31._dp, 59.25_dp, &
                                                    90.25_dp, 120.25_dp, 151.25_dp, &
                                                    181.25_dp, 212.25_dp, 243.25_dp, &
                                                    273.25_dp, 304.25_dp, 334.25_dp]
integer(int32), dimension (12), parameter :: DaysInMonth = [31,28,31,30,31,30, &
                                                            31,31,30,31,30,31]

integer(intEnum), parameter :: modeCycle_GDDDays = 0
    !! index of GDDDays in modeCycle enumerated type
integer(intEnum), parameter :: modeCycle_CalendarDays = 1
    !! index of CalendarDays in modeCycle enumerated type

integer(intEnum), parameter :: pMethod_NoCorrection = 0
    !! index of NoCorrection in pMethod enumerated type
integer(intEnum), parameter :: pMethod_FAOCorrection = 1
    !! index of FAOCorrection in pMethod enumerated type

integer(intEnum), parameter :: subkind_Vegetative = 0
    !! index of Vegetative in subkind enumerated type
integer(intEnum), parameter :: subkind_Grain = 1
    !! index of Grain in subkind enumerated type
integer(intEnum), parameter :: subkind_Tuber = 2
    !! index of Tuber in subkind enumerated type
integer(intEnum), parameter :: subkind_Forage = 3
    !! index of Forage in subkind enumerated type

integer(intEnum), parameter :: plant_seed = 0
    !! index of seed in planting enumerated type
integer(intEnum), parameter :: plant_transplant = 1
    !! index of transplant in planting enumerated type
integer(intEnum), parameter :: plant_regrowth= 2
    !! index of regrowth in planting enumerated type

integer(intEnum), parameter :: Method_full = 0
    !! index of full in Method enumerated type
integer(intEnum), parameter :: Method_usda = 1
    !! index of usda in Method enumerated type
integer(intEnum), parameter :: Method_percentage= 2
    !! index of percentage in Method enumerated type

integer(intEnum), parameter :: EffectiveRainMethod_full = 0
    !! index of full in EffectiveRainMethod enumerated type
integer(intEnum), parameter :: EffectiveRainMethod_usda = 1
    !! index of usda in EffectiveRainMethod enumerated type
integer(intEnum), parameter :: EffectiveRainMethod_percentage= 2
    !! index of percentage in EffectiveRainMethod enumerated type

integer(intEnum), parameter :: TimeCuttings_NA = 0
    !! index of NA in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_IntDay = 1
    !! index of IntDay in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_IntGDD = 2
    !! index of IntGDD in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_DryB = 3
    !! index of DryB in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_DryY = 4
    !! index of DryY in TimeCuttings enumerated type
integer(intEnum), parameter :: TimeCuttings_FreshY= 5
    !! index of FreshY in TimeCuttings enumerated type

integer(intEnum), parameter :: Criterion_CumulRain = 0
    !! index of CumulRain in Criterion enumerated type
integer(intEnum), parameter :: Criterion_RainPeriod = 1
    !! index of RainPeriod in Criterion enumerated type
integer(intEnum), parameter :: Criterion_RainDecade = 2
    !! index of RainDecade in Criterion enumerated type
integer(intEnum), parameter :: Criterion_RainVsETo = 3
    !! index of RainVsETo in Criterion enumerated type

integer(intEnum), parameter :: AirTCriterion_TminPeriod = 0
    !! index of TminPeriod in AirTCriterion enumerated type
integer(intEnum), parameter :: AirTCriterion_TmeanPeriod = 1
    !! index of TmeanPeriod in AirTCriterion enumerated type
integer(intEnum), parameter :: AirTCriterion_GDDPeriod = 2
    !! index of GDDPeriod in AirTCriterion enumerated type
integer(intEnum), parameter :: AirTCriterion_CumulGDD = 3
    !! index of CumulGDD in AirTCriterion enumerated type

integer(intEnum), parameter :: GenerateTimeMode_FixInt = 0
    !! index of FixInt in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_AllDepl = 1
    !! index of AllDepl in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_AllRAW = 2
    !! index of AllRAW in GenerateTimeMode enumerated type
integer(intEnum), parameter :: GenerateTimeMode_WaterBetweenBuns = 3
    !! index of WaterBetweenBuns in GenerateTimeMode enumerated type

integer(intEnum), parameter :: GenerateDepthMode_ToFC = 0
    !! index of ToFC in GenerateDepthMode enumerated type
integer(intEnum), parameter :: GenerateDepthMode_FixDepth = 1
    !! index of FixDepth in GenerateDepthMode enumerated type

integer(intEnum), parameter :: IrriMode_NoIrri = 0
    !! index of NoIrri in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Manual = 1
    !! index of Manual in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Generate = 2
    !! index of Generate in IrriMode enumerated type
integer(intEnum), parameter :: IrriMode_Inet = 3
    !! index of inet in IrriMode enumerated type

integer(intEnum), parameter :: IrriMethod_MBasin = 0
    !! index of MBasin in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MBorder = 1
    !! index of MBorder in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MDrip = 2
    !! index of MDrip in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MFurrow = 3
    !! index of MFurrow in IrriMode enumerated type
integer(intEnum), parameter :: IrriMethod_MSprinkler = 4
    !! index of MSprinkler in IrriMode enumerated type

integer(intEnum), parameter :: datatype_daily = 0
    !! index of daily in datatype enumerated type
integer(intEnum), parameter :: datatype_decadely = 1
    !! index of decadely in datatype enumerated type
integer(intEnum), parameter :: datatype_monthly= 2
    !! index of monthly in datatype enumerated type

type SoilLayerIndividual
    character(len=25) :: Description
        !! Undocumented
    real(dp) :: Thickness
        !! meter
    real(dp) :: SAT
        !! Vol % at Saturation
    real(dp) :: FC
        !! Vol % at Field Capacity
    real(dp) :: WP
        !! Vol % at Wilting Point
    real(dp) :: tau
        !! drainage factor 0 ... 1
    real(dp) :: InfRate
        !! Infiltration rate at saturation mm/day
    integer(int8) :: Penetrability
        !! root zone expansion rate in percentage
    integer(int8) :: GravelMass
        !! mass percentage of gravel
    real(dp) :: GravelVol
        !! volume percentage of gravel
    real(dp) :: WaterContent
        !! mm
    ! salinity parameters (cells)
    integer(int8) :: Macro
        !! Macropores : from Saturation to Macro [vol%]
    real(dp), dimension(11) :: SaltMobility
        !! Mobility of salt in the various salt cellS
    integer(int8) :: SC
        !! number of Saltcels between 0 and SC/(SC+2)*SAT vol%
    integer(int8) :: SCP1
        !! SC + 1   (1 extra Saltcel between SC/(SC+2)*SAT vol% and SAT)
        !! THis last celL is twice as large as the other cels *)
    real(dp) :: UL
        !! Upper Limit of SC salt cells = SC/(SC+2) * (SAT/100) in m3/m3
    real(dp) :: Dx
        !! Size of SC salt cells [m3/m3] = UL/SC
    ! capilary rise parameters
    integer(int8) :: SoilClass
        !! 1 = sandy, 2 = loamy, 3 = sandy clayey, 4 - silty clayey soils
    real(dp) :: CRa, CRb
        !! coefficients for Capillary Rise
end type SoilLayerIndividual

type rep_Shapes
    integer(int8) :: Stress
        !! Percentage soil fertility stress for calibration
    real(dp) :: ShapeCGC
        !! Shape factor for the response of Canopy Growth Coefficient to soil
        !! fertility stress
    real(dp) :: ShapeCCX
        !! Shape factor for the response of Maximum Canopy Cover to soil
        !! fertility stress
    real(dp) :: ShapeWP
        !! Shape factor for the response of Crop Water Producitity to soil
        !! fertility stress
    real(dp) :: ShapeCDecline
        !! Shape factor for the response of Decline of Canopy Cover to soil
        !! fertility stress
    logical :: Calibrated
        !! Undocumented
end type rep_Shapes

type rep_soil
    integer(int8) :: REW 
        !! (* Readily evaporable water mm *)
    integer(int8) :: NrSoilLayers
    integer(int8) :: CNvalue
    real(sp) :: RootMax 
        !! maximum rooting depth in soil profile for selected crop
end type rep_soil

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

type rep_Onset 
    logical :: GenerateOn
        !! by rainfall or temperature criterion
    logical :: GenerateTempOn
        !! by temperature criterion
    integer(intEnum) :: Criterion
        !! Undocumented
    integer(intEnum) :: AirTCriterion
        !! Undocumented
    integer(int32) :: StartSearchDayNr
        !! daynumber
    integer(int32) :: StopSearchDayNr
        !! daynumber
    integer(int32) :: LengthSearchPeriod
        !! days
end type rep_Onset 

type rep_Content
    real(dp) :: BeginDay
        !! at the beginning of the day
    real(dp) :: EndDay
        !! at the end of the day
    real(dp) :: ErrorDay
        !! error on WaterContent or SaltContent over the day
end type rep_Content

type rep_EffectStress
    integer(int8) :: RedCGC
        !! Reduction of CGC (%)
    integer(int8) :: RedCCX
        !! Reduction of CCx (%)
    integer(int8) :: RedWP
        !! Reduction of WP (%)
    real(dp) :: CDecline
        !! Average decrease of CCx in mid season (%/day)
    integer(int8) :: RedKsSto
        !! Reduction of KsSto (%)
end type rep_EffectStress

type rep_EffectiveRain 
    !!  for 10-day or monthly rainfall data
    integer(intEnum) :: Method
        !! Undocumented
    integer(int8) :: PercentEffRain
        !! IF Method = Percentage
    integer(int8) :: ShowersInDecade
        !! adjustment of surface run-off
    integer(int8) :: RootNrEvap
        !! Root for reduction in soil evaporation
end type rep_EffectiveRain 

type rep_RootZoneWC 
    real(dp) :: Actual
        !! actual soil water content in rootzone [mm]
    real(dp) :: FC
        !! soil water content [mm] in rootzone at FC
    real(dp) :: WP
        !! soil water content [mm] in rootzone at WP
    real(dp) :: SAT
        !! soil water content [mm] in rootzone at Sat
    real(dp) :: Leaf
        !! soil water content [mm] in rootzone at upper Threshold for leaf
        !! expansion
    real(dp) :: Thresh
        !! soil water content [mm] in rootzone at Threshold for stomatal
        !! closure
    real(dp) :: Sen
        !! soil water content [mm] in rootzone at Threshold for canopy
        !! senescence
    real(dp) :: ZtopAct
        !! actual soil water content [mm] in top soil (= top compartment)
    real(dp) :: ZtopFC
        !! soil water content [mm] at FC in top soil (= top compartment)
    real(dp) :: ZtopWP
        !! soil water content [mm] at WP in top soil (= top compartment)
    real(dp) :: ZtopThresh
        !! soil water content [mm] at Threshold for stomatal closure in top
        !! soil
end type rep_RootZoneWC 

type rep_IrriECw 
    real(dp) :: PreSeason
        !! Undocumented
    real(dp) :: PostSeason
        !! Undocumented
end type rep_IrriECw 

type rep_clim
    integer(intEnum) :: DataType
        !! Undocumented
    integer(int32) :: FromD, FromM, FromY
        !! D = day or decade, Y=1901 is not linked to specific year
    integer(int32) :: ToD, ToM, ToY
        !! Undocumented
    integer(int32) :: FromDayNr, ToDayNr
        !! daynumber
    character(len=:), allocatable :: FromString, ToString
        !! Undocumented, note GDL: randomly chose 500
    integer(int32) :: NrObs
        !! number of observations
end type rep_clim

type rep_CropFileSet 
    integer(int32) :: DaysFromSenescenceToEnd
        !! Undocumented
    integer(int32) :: DaysToHarvest
        !! given or calculated from GDD
    integer(int32) :: GDDaysFromSenescenceToEnd
        !! Undocumented
    integer(int32) :: GDDaysToHarvest
        !! given or calculated from Calendar Days
end type rep_CropFileSet

type rep_Cuttings 
    logical :: Considered
        !! Undocumented
    integer(int32) :: CCcut
        !! Canopy cover (%) after cutting
    integer(int32) :: CGCPlus
        !! Increase (percentage) of CGC after cutting
    integer(int32) :: Day1
        !! first day after time window for generating cuttings (1 = start crop cycle)
    integer(int32) :: NrDays
        !! number of days of time window for generate cuttings (-9 is whole crop cycle)
    logical :: Generate
        !! ture: generate cuttings; false : schedule for cuttings
    integer(intEnum) :: Criterion
        !! time criterion for generating cuttings
    logical :: HarvestEnd
        !! final harvest at crop maturity
    integer(int32) :: FirstDayNr
        !! first dayNr of list of specified cutting events (-9 = onset growing cycle)
end type rep_Cuttings 


type rep_Manag 
    integer(int8) :: Mulch
        !! percent soil cover by mulch in growing period
    integer(int8) :: SoilCoverBefore
        !! percent soil cover by mulch before growing period
    integer(int8) :: SoilCoverAfter
        !! percent soil cover by mulch after growing period
    integer(int8) :: EffectMulchOffS
        !! effect Mulch on evaporation before and after growing period
    integer(int8) :: EffectMulchInS
        !! effect Mulch on evaporation in growing period
    integer(int8) :: FertilityStress
        !! Undocumented
    real(dp) :: BundHeight
        !! meter;
    logical :: RunoffOn
        !! surface runoff
    integer(int32) :: CNcorrection
        !! percent increase/decrease of CN
    integer(int8) :: WeedRC
        !! Relative weed cover in percentage at canopy closure
    integer(int32) :: WeedDeltaRC
        !! Increase/Decrease of Relative weed cover in percentage during mid season
    real(dp) :: WeedShape
        !! Shape factor for crop canopy suppression
    integer(int8) :: WeedAdj
        !! replacement (%) by weeds of the self-thinned part of the Canopy Cover - only for perennials
    type(rep_Cuttings) :: Cuttings
        !! Multiple cuttings
end type rep_Manag

type rep_sum 
    real(dp) :: Epot, Tpot, Rain, Irrigation, Infiltrated
        !! Undocumented
    real(dp) :: Runoff, Drain, Eact, Tact, TrW, ECropCycle, CRwater
        !! mm
    real(dp) :: Biomass, YieldPart, BiomassPot, BiomassUnlim, BiomassTot
        !! ton/ha
    real(dp) :: SaltIn, SaltOut, CRsalt
        !! ton/ha
end type rep_sum 

type rep_RootZoneSalt 
    real(dp) :: ECe
        !! Electrical conductivity of the saturated soil-paste extract (dS/m)
    real(dp) :: ECsw
        !! Electrical conductivity of the soil water (dS/m)
    real(dp) :: ECswFC
        !! Electrical conductivity of the soil water at Field Capacity(dS/m)
    real(dp) :: KsSalt
        !! stress coefficient for salinity
end type rep_RootZoneSalt

type rep_DayEventDbl
    integer(int32) :: DayNr
        !! Undocumented
    real(dp) :: Param
        !! Undocumented
end type rep_DayEventDbl

type rep_Crop 
    integer(intEnum) :: subkind
        !! Undocumented
    integer(intEnum) :: ModeCycle
        !! Undocumented
    integer(intEnum) :: Planting
        !! 1 = sown, 0 = transplanted, -9 = regrowth
    integer(intEnum) :: pMethod
        !! Undocumented
    real(dp) :: pdef
        !! soil water depletion fraction for no stomatal stress as defined (ETo = 5 mm/day)
    real(dp) :: pActStom
        !! actual p for no stomatal stress for ETo of the day
    real(dp) :: KsShapeFactorLeaf
        !! Undocumented
    real(dp) :: KsShapeFactorStomata
        !! Undocumented
    real(dp) :: KsShapeFactorSenescence
        !! Undocumented
    real(dp) :: pLeafDefUL
        !! soil water depletion fraction for leaf expansion (ETo = 5 mm/day)
    real(dp) :: pLeafDefLL
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
    integer(int32), dimension(4) :: Length
        !! Length : rep_int_array; @ 1 .. 4 :  = the four growth stages
    real(dp) :: RootMin
        !! rooting depth in meter
    real(dp) :: RootMax
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

character(len=:), allocatable :: RainFile
character(len=:), allocatable :: RainFileFull
character(len=:), allocatable :: RainDescription
character(len=:), allocatable :: EToFile
character(len=:), allocatable :: EToFileFull
character(len=:), allocatable :: EToDescription
character(len=:), allocatable :: CalendarFile
character(len=:), allocatable :: CalendarFileFull
character(len=:), allocatable :: CO2File
character(len=:), allocatable :: CO2FileFull
character(len=:), allocatable :: CO2Description
character(len=:), allocatable :: IrriFile
character(len=:), allocatable :: IrriFileFull
character(len=:), allocatable :: CropFile
character(len=:), allocatable :: CropFileFull
character(len=:), allocatable :: PathNameProg
character(len=:), allocatable :: PathNameOutp
character(len=:), allocatable :: PathNameSimul
character(len=:), allocatable :: ProfFile
character(len=:), allocatable :: ProfFilefull
character(len=:), allocatable :: ManFile
character(len=:), allocatable :: ManFilefull
character(len=:), allocatable :: ObservationsFile
character(len=:), allocatable :: ObservationsFilefull
character(len=:), allocatable :: ObservationsDescription
character(len=:), allocatable :: OffSeasonFile
character(len=:), allocatable :: OffSeasonFilefull
character(len=:), allocatable :: GroundWaterFile
character(len=:), allocatable :: GroundWaterFilefull
character(len=:), allocatable :: ClimateFile
character(len=:), allocatable :: ClimateFileFull
character(len=:), allocatable :: ClimFile
character(len=:), allocatable :: SWCiniFile
character(len=:), allocatable :: SWCiniFileFull
character(len=:), allocatable :: ProjectFile
character(len=:), allocatable :: ProjectFileFull
character(len=:), allocatable :: MultipleProjectFile
character(len=:), allocatable :: TemperatureFile
character(len=:), allocatable :: TemperatureFileFull
character(len=:), allocatable :: MultipleProjectFileFull

type(rep_IrriECw) :: IrriECw
type(rep_Manag) :: Management
type(rep_Cuttings) :: Cuttings
type(rep_Onset) :: onset
type(rep_Crop) :: crop
type(rep_Content) :: TotalSaltContent
type(rep_Content) :: TotalWaterContent
type(rep_EffectiveRain) :: effectiverain
type(rep_soil) :: Soil
type(rep_RootZoneWC) :: RootZoneWC
type(rep_CropFileSet) :: CropFileSet
type(rep_sum) :: SumWaBal
type(rep_RootZoneSalt) :: RootZoneSalt
type(rep_clim)  :: TemperatureRecord

integer(intEnum) :: GenerateTimeMode
integer(intEnum) :: GenerateDepthMode
integer(intEnum) :: IrriMode
integer(intEnum) :: IrriMethod


contains


function trunc(x) result(y)
    !! Returns the integer part of x, which is always smaller than (or equal to) x
    !! in absolute value.
    real(dp), intent(in) :: x
    integer(int32) :: y
    
    if (x > 0) then
        y = floor(x, kind=int32)
    else
        y = ceiling(x, kind=int32)
    end if
end function trunc


real(dp) function AquaCropVersion(FullNameXXFile)
    character(len=*), intent(in) :: FullNameXXFile

    integer :: fhandle
    real(dp) :: VersionNr

    open(newunit=fhandle, file=trim(FullNameXXFile), status='old', &
         action='read')

    read(fhandle, *)  ! Description
    read(fhandle, *) VersionNr  ! AquaCrop version

    close(fhandle)

    AquaCropVersion = VersionNr
end function AquaCropVersion


real(sp) function RootMaxInSoilProfile(ZmaxCrop, TheNrSoilLayers, TheSoilLayer)
    real(dp), intent(in) :: ZmaxCrop
    integer(int8), intent(in) :: TheNrSoilLayers
    type(SoilLayerIndividual), dimension(max_SoilLayers), intent(in) :: &
                                                                TheSoilLayer

    real(dp) :: Zmax
    real(dp) :: Zsoil
    integer :: layi

    Zmax = ZmaxCrop
    Zsoil = 0._dp

    layi = 0
    do while ((layi < TheNrSoilLayers) .and. (Zmax > 0))
        layi = layi + 1

        if ((TheSoilLayer(layi)%Penetrability < 100) .and. &
            (nint(Zsoil*1000) < nint(ZmaxCrop*1000))) then
            Zmax = undef_int
        end if

        Zsoil = Zsoil + TheSoilLayer(layi)%Thickness
    end do

    if (Zmax < 0) then
        call ZrAdjustedToRestrictiveLayers(ZmaxCrop, TheNrSoilLayers, &
                                           TheSoilLayer, Zmax)
    end if

    RootMaxInSoilProfile = real(Zmax, kind=sp)
end function RootMaxInSoilProfile


subroutine ZrAdjustedToRestrictiveLayers(ZrIN, TheNrSoilLayers, TheLayer, ZrOUT)
    real(dp), intent(in) :: ZrIN
    integer(int8), intent(in) :: TheNrSoilLayers
    type(SoilLayerIndividual), dimension(max_SoilLayers), intent(in) :: TheLayer
    real(dp), intent(inout) :: ZrOUT

    integer :: Layi
    real(dp) :: Zsoil, ZrAdj, ZrRemain, DeltaZ, ZrTest
    logical :: TheEnd

    ZrOUT = ZrIn

    ! initialize (layer 1)
    layi = 1
    Zsoil = TheLayer(layi)%Thickness
    ZrAdj = 0
    ZrRemain = ZrIN
    DeltaZ = Zsoil
    TheEnd = .false.

    ! check succesive layers
    do while (.not. TheEnd)
        ZrTest = ZrAdj + ZrRemain * (TheLayer(layi)%Penetrability/100._dp)

        if ((layi == TheNrSoilLayers) &
            .or. (TheLayer(layi)%Penetrability == 0) &
            .or. (nint(ZrTest*10000) <= nint(Zsoil*10000))) then
            ! no root expansion in layer
            TheEnd = .true.
            ZrOUT = ZrTest
        else
            ZrAdj = Zsoil
            ZrRemain = ZrRemain - DeltaZ/(TheLayer(layi)%Penetrability/100._dp)
            layi = layi + 1
            Zsoil = Zsoil + TheLayer(layi)%Thickness
            DeltaZ = TheLayer(layi)%Thickness
        end if
    end do
end subroutine ZrAdjustedToRestrictiveLayers


subroutine set_layer_undef(LayerData)
    type(SoilLayerIndividual), intent(inout) :: LayerData

    integer(int32) :: i

    LayerData%Description = ''
    LayerData%Thickness = undef_double
    LayerData%SAT = undef_double
    LayerData%FC = undef_double
    LayerData%WP = undef_double
    LayerData%tau = undef_double
    LayerData%InfRate = undef_double
    LayerData%Penetrability = undef_int
    LayerData%GravelMass = undef_int
    LayerData%GravelVol = undef_int
    LayerData%Macro = undef_int
    LayerData%UL = undef_double
    LayerData%Dx = undef_double
    do i = 1, 11
        LayerData%SaltMobility(i) = undef_double  ! maximum 11 salt cells
    end do
    LayerData%SoilClass = undef_int
    LayerData%CRa = undef_int
    LayerData%CRb = undef_int
    LayerData%WaterContent = undef_double
end subroutine set_layer_undef


subroutine CropStressParametersSoilFertility(CropSResp, &
                    StressLevel, StressOUT)
    type(rep_Shapes), intent(in) :: CropSResp
    integer(int8), intent(in)    :: StressLevel
    type(rep_EffectStress), intent(inout) :: StressOUT

    real(dp) :: Ksi, pULActual, pLLActual

    pLLActual = 1._dp

    ! decline canopy growth coefficient (CGC)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCGC)
    StressOUT%RedCGC = nint((1._dp-Ksi)*100._dp, int8)
    ! decline maximum canopy cover (CCx)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCCX)
    StressOUT%RedCCX = nint((1._dp-Ksi)*100._dp, int8)
    ! decline crop water productivity (WP)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeWP)
    StressOUT%RedWP = nint((1._dp-Ksi)*100._dp, int8)
    ! decline Canopy Cover (CDecline)
    pULActual = 0._dp
    Ksi = KsAny(StressLevel/100._dp, pULActual, pLLActual, CropSResp%ShapeCDecline)
    StressOUT%CDecline = 1._dp - Ksi
    ! inducing stomatal closure (KsSto) not applicable
    Ksi = 1._dp
    StressOUT%RedKsSto = nint((1._dp-Ksi)*100._dp, int8)
end subroutine CropStressParametersSoilFertility


real(dp) function TimeRootFunction(t, ShapeFactor, tmax, t0)
    real(dp), intent(in) :: t
    integer(int8), intent(in) :: ShapeFactor
    real(dp), intent(in) :: tmax
    real(dp), intent(in) :: t0

    TimeRootFunction = exp((10._dp / ShapeFactor) * log((t-t0) / (tmax-t0)))
end function TimeRootFunction


real(dp) function TimeToReachZroot(Zi, Zo, Zx, ShapeRootDeepening, Lo, LZxAdj)
    real(dp), intent(in) :: Zi
    real(dp), intent(in) :: Zo
    real(dp), intent(in) :: Zx
    integer(int8), intent(in) :: ShapeRootDeepening
    integer(int32), intent(in) :: Lo
    integer(int32), intent(in) :: LZxAdj

    real(dp) :: ti, T1

    ti = real(undef_int, kind=dp)

    if (nint(Zi*100) >= nint(Zx*100)) then
        ti = real(LZxAdj, kind=dp)
    else
        if (((Zo+0.0001_dp) < Zx) .and. (LZxAdj > Lo/2._dp) .and. (LZxAdj > 0) &
            .and. (ShapeRootDeepening > 0)) then
            T1 = exp((ShapeRootDeepening/10._dp) * log((Zi-Zo) / (Zx-Zo)))
            ti = T1 * (LZxAdj - Lo/2._dp) + Lo/2._dp
        end if
    end if

    TimeToReachZroot = ti
end function TimeToReachZroot


real(dp) function FromGravelMassToGravelVolume(PorosityPercent,&
                                               GravelMassPercent)
    real(dp), intent(in)      :: PorosityPercent
    integer(int8), intent(in) :: GravelMassPercent

    real(dp), parameter ::  MineralBD = 2.65 !! Mg/m3
    real(dp) :: MatrixBD, SoilBD

    if (GravelMassPercent > 0) then
        MatrixBD = MineralBD * (1._dp - PorosityPercent/100._dp)
        SoilBD = 100._dp/(GravelMassPercent/MineralBD + &
                          (100._dp-GravelMassPercent)/MatrixBD)
        FromGravelMassToGravelVolume = GravelMassPercent * (SoilBD/MineralBD)
   else
       FromGravelMassToGravelVolume = 0.0_dp
   end if
end function FromGravelMassToGravelVolume


real(dp) function GetWeedRC(TheDay, GDDayi, fCCx, TempWeedRCinput, TempWeedAdj,&
                            TempWeedDeltaRC, L12SF, TempL123, GDDL12SF, &
                            TempGDDL123, TheModeCycle)
    integer(int32), intent(in) :: TheDay
    real(dp), intent(in) :: GDDayi
    real(dp), intent(in) :: fCCx
    integer(int8), intent(in) :: TempWeedRCinput
    integer(int8), intent(in) :: TempWeedAdj
    integer(int32), intent(inout) :: TempWeedDeltaRC
    integer(int32), intent(in) :: L12SF
    integer(int32), intent(in) :: TempL123
    integer(int32), intent(in) :: GDDL12SF
    integer(int32), intent(in) :: TempGDDL123
    integer(intEnum), intent(in) :: TheModeCycle

    real(dp) :: WeedRCDayCalc

    WeedRCDayCalc = TempWeedRCinput

    if ((TempWeedRCinput > 0) .and. (TempWeedDeltaRC /= 0)) then
        ! daily RC when increase/decline of RC in season (i.e. TempWeedDeltaRC <> 0)
        ! adjust the slope of increase/decline of RC in case of self-thinning (i.e. fCCx < 1)
        if ((TempWeedDeltaRC /= 0) .and. (fCCx < 0.999_dp)) then
            ! only when self-thinning and there is increase/decline of RC
            if (fCCx < 0.005_dp) then
                TempWeedDeltaRC = 0
            else
                TempWeedDeltaRC = nint(TempWeedDeltaRC * exp( &
                                       log(fCCx) * (1+TempWeedAdj/100._dp)), &
                                       kind=int32)
            end if
        end if

        ! calculate WeedRCDay by considering (adjusted) decline/increase of RC
        if (TheModeCycle == modeCycle_CalendarDays) then
            if (TheDay > L12SF) then
                if (TheDay >= TempL123) then
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        TempWeedDeltaRC/100._dp)
                else
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        (TempWeedDeltaRC/100._dp) &
                                         * (TheDay-L12SF) / (TempL123-L12SF))
                end if
            end if
        else
            if (GDDayi > GDDL12SF) then
                if (GDDayi > TempGDDL123) then
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        TempWeedDeltaRC/100._dp)
                else
                    WeedRCDayCalc = TempWeedRCinput * (1 + &
                                        (TempWeedDeltaRC/100._dp) &
                                         * (GDDayi-GDDL12SF) &
                                         / (TempGDDL123-GDDL12SF))
                end if
            end if
        end if

        ! fine-tuning for over- or undershooting in case of self-thinning
        if (fCCx < 0.999_dp) then
            ! only for self-thinning
            if ((fCCx < 1) .and. (fCCx > 0) .and. (WeedRCDayCalc > 98)) then
                WeedRCDayCalc = 98._dp
            end if
            if (WeedRCDayCalc < 0) then
                WeedRCDayCalc = 0._dp
            end if
            if (fCCx <= 0) then
                WeedRCDayCalc = 100._dp
            end if
        end if
    end if

    GetWeedRC = WeedRCDayCalc
end function GetWeedRC

subroutine DetermineLengthGrowthStages(CCoVal, CCxVal, CDCVal, L0, TotalLength, &
                                        CGCgiven, TheDaysToCCini, ThePlanting, &
                                         Length123, StLength, Length12, CGCVal)
    real(dp), intent(in) :: CCoVal
    real(dp), intent(in) :: CCxVal
    real(dp), intent(in) :: CDCVal
    integer(int32), intent(in) :: L0
    integer(int32), intent(in) :: TotalLength
    logical, intent(in) :: CGCgiven
    integer(int32), intent(in) :: TheDaysToCCini
    integer(intEnum), intent(in) :: ThePlanting
    integer(int32), intent(inout) :: Length123
    integer(int32), dimension(4), intent(inout) :: StLength
    integer(int32), intent(inout) :: Length12
    real(dp), intent(inout) :: CGCVal

    real(dp) :: CCxVal_scaled
    real(dp) :: CCToReach
    integer(int32) :: L12Adj

    if (Length123 < Length12) then
        Length123 = Length12
    end if

    ! 1. Initial and 2. Crop Development stage
    ! CGC is given and Length12 is already adjusted to it
    ! OR Length12 is given and CGC has to be determined
    if ((CCoVal >= CCxVal) .or. (Length12 <= L0)) then
        Length12 = 0
        StLength(1) = 0
        StLength(2) = 0
        CGCVal = Undef_int
    else
        if (.not. CGCgiven) then ! Length12 is given and CGC has to be determined
            CGCVal = real((log((0.25_dp*CCxVal/CCoVal)/(1._dp-0.98_dp))/(Length12-L0)), kind=dp)
            ! Check if CGC < maximum value (0.40) and adjust Length12 if required
            if (CGCVal > 0.40_dp) then
                CGCVal = 0.40_dp
                CCxVal_scaled = 0.98_dp*CCxVal
                Length12 = DaysToReachCCwithGivenCGC(CCxVal_scaled , CCoVal, &
                                                             CCxVal, CGCVal, L0)
                if (Length123 < Length12) then
                    Length123 = Length12
                end if
            end if
        end if
        ! find StLength[1]
        CCToReach = 0.10_dp
        StLength(1) = DaysToReachCCwithGivenCGC(CCToReach, CCoVal, CCxVal, &
                                                                    CGCVal, L0)
        ! find StLength[2]
        StLength(2) = Length12 - StLength(1)
    end if
    L12Adj = Length12

    ! adjust Initial and Crop Development stage, in case crop starts as regrowth
    if (ThePlanting == plant_regrowth) then
        if (TheDaystoCCini == undef_int) then
            ! maximum canopy cover is already reached at start season
            L12Adj = 0
            StLength(1) = 0
            StLength(2) = 0
        else
            if (TheDaystoCCini == 0) then
                ! start at germination
                L12Adj = Length12 - L0
                StLength(1) = StLength(1) - L0
            else
                ! start after germination
                L12Adj = Length12 - (L0 + TheDaysToCCini)
                StLength(1) = StLength(1) - (L0 + TheDaysToCCini)
            end if
            if (StLength(1) < 0) then
                StLength(1) = 0
            end if
            StLength(2) = L12Adj - StLength(1)
        end if
    end if

    ! 3. Mid season stage
    StLength(3) = Length123 - L12Adj

    ! 4. Late season stage
    StLength(4) = LengthCanopyDecline(CCxVal, CDCVal)

    ! final adjustment
    if (StLength(1) > TotalLength) then
        StLength(1) = TotalLength
        StLength(2) = 0
        StLength(3) = 0
        StLength(4) = 0
    else
        if ((StLength(1)+StLength(2)) > TotalLength) then
            StLength(2) = TotalLength - StLength(1)
            StLength(3) = 0
            StLength(4) = 0
        else
            if ((StLength(1)+StLength(2)+StLength(3)) > TotalLength) then
                StLength(3) = TotalLength - StLength(1) - StLength(2)
                StLength(4) = 0
            elseif ((StLength(1)+StLength(2)+StLength(3)+StLength(4)) > &
                                                              TotalLength) then
                StLength(4) = TotalLength - StLength(1) - StLength(2) - StLength(3)
            end if
        end if
    end if

end subroutine DetermineLengthGrowthStages


integer(int32) function TimeToCCini(ThePlantingType, TheCropPlantingDens, &
                          TheSizeSeedling, TheSizePlant, TheCropCCx, TheCropCGC)
    integer(intEnum), intent(in) :: ThePlantingType
    integer(int32), intent(in) :: TheCropPlantingDens
    real(dp), intent(in) :: TheSizeSeedling
    real(dp), intent(in) :: TheSizePlant
    real(dp), intent(in) :: TheCropCCx
    real(dp), intent(in) :: TheCropCGC


    integer(int32) :: ElapsedTime
    real(dp) :: TheCropCCo
    real(dp) :: TheCropCCini

    if ((ThePlantingType == plant_seed) .or. (ThePlantingType == plant_transplant) &
                                   .or. (TheSizeSeedling >= TheSizePlant)) then
        ElapsedTime = 0
    else
        TheCropCCo = (TheCropPlantingDens/10000._dp) * (TheSizeSeedling/10000._dp)
        TheCropCCini = (TheCropPlantingDens/10000._dp) * (TheSizePlant/10000._dp)
        if (TheCropCCini >= (0.98_dp*TheCropCCx)) then
            ElapsedTime = undef_int
        else
            if (TheCropCCini <= TheCropCCx/2) then
                ElapsedTime = nint(((log(TheCropCCini/TheCropCCo))/TheCropCGC), kind=int32)
            else
                ElapsedTime = (-1)* nint(((log(((TheCropCCx-TheCropCCini)* &
                     TheCropCCo)/(0.25_dp*TheCropCCx*TheCropCCx)))/TheCropCGC), kind=int32)
            end if
        end if
    end if
    TimeToCCini = ElapsedTime
end function TimeToCCini

real(dp) function MultiplierCCxSelfThinning(Yeari, Yearx, ShapeFactor)
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(in) :: Yearx
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: fCCx, Year0
    
    fCCx = 1
    if ((Yeari >= 2) .and. (Yearx >= 2) .and. (nint(100._dp*ShapeFactor, &
                                                    int32) /= 0)) then
        Year0 = 1._dp + (Yearx-1._dp) * exp(ShapeFactor*log(10._dp))
        if (Yeari >= Year0) then
            fCCx = 0
        else
            fCCx = 0.9_dp + 0.1_dp * (1._dp - exp((1._dp/ShapeFactor) &
                        *log((Yeari-1._dp)/(Yearx-1._dp))))
        end if
        if (fCCx < 0) then
            fCCx = 0
        end if
    end if
    MultiplierCCxSelfThinning = fCCx     
end function MultiplierCCxSelfThinning

integer(int32) function DaysToReachCCwithGivenCGC(CCToReach, CCoVal, &
                                                        CCxVal, CGCVal, L0)
    real(dp), intent(inout) :: CCToReach
    real(dp), intent(in) :: CCoVal
    real(dp), intent(in) :: CCxVal
    real(dp), intent(in) :: CGCVal
    integer(int32), intent(in) :: L0

    real(dp) :: L
    if ((CCoVal > CCToReach) .or. (CCoVal >= CCxVal)) then
        L = 0
    else
        if (CCToReach > (0.98_dp*CCxVal)) then
            CCToReach = 0.98_dp*CCxVal
        end if
        if (CCToReach <= CCxVal/2._dp) then
            L = log(CCToReach/CCoVal)/CGCVal
        else
            L = log((0.25_dp*CCxVal*CCxVal/CCoVal)/(CCxVal-CCToReach))/CGCVal
        end if

    end if
    DaysToReachCCwithGivenCGC = L0 + nint(L, int32)
end function DaysToReachCCwithGivenCGC

integer(int32) function LengthCanopyDecline(CCx, CDC)
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: CDC

    integer(int32) :: ND

    ND = 0
    if (CCx > 0) then
        if (CDC <= epsilon(1._dp)) then
            ND = undef_int
        else
            ND = nint((((CCx+2.29_dp)/(CDC*3.33_dp))*log(1._dp + 1._dp/0.05_dp &
                     ) + 0.50_dp), int32)  ! + 0.50 to guarantee that CC is zero
        end if

    end if
    LengthCanopyDecline = ND
end function LengthCanopyDecline


real(dp) function HarvestIndexGrowthCoefficient(HImax, dHIdt)
    real(dp), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt

    real(dp) :: HIo, HIvar, HIGC, t

    HIo = 1

    if (HImax > HIo) then
        t = HImax/dHIdt
        HIGC = 0.001_dp
        HIGC = HIGC + 0.001_dp
        HIvar = (HIo*HImax)/(HIo+(HImax-HIo)*exp(-HIGC*t))
        do while (HIvar <= (0.98_dp*HImax))
            HIGC = HIGC + 0.001_dp
            HIvar = (HIo*HImax)/(HIo+(HImax-HIo)*exp(-HIGC*t))
        end do

        if (HIvar >= HImax) then
            HIGC = HIGC - 0.001_dp
        end if
    else
        HIGC = undef_int
    end if
    HarvestIndexGrowthCoefficient = HIGC

end function HarvestIndexGrowthCoefficient


real(dp) function TauFromKsat(Ksat)
    real(dp), intent(in) :: Ksat

    integer(int32) :: TauTemp

    if (abs(Ksat) < epsilon(1._dp)) then
        TauFromKsat = 0
    else
        TauTemp = nint(100.0_dp*0.0866_dp*exp(0.35_dp*log(Ksat)), kind=int32)
        if (TauTemp < 0) then
            TauTemp = 0
        end if
        if (TauTemp > 100) then
            TauTemp = 100
        end if
        TauFromKsat = TauTemp/100.0_dp
    end if
end function TauFromKsat


integer(int8) function NumberSoilClass(SatvolPro, FCvolPro, PWPvolPro, Ksatmm)
    real(dp), intent(in) :: SatvolPro
    real(dp), intent(in) :: FCvolPro
    real(dp), intent(in) :: PWPvolPro
    real(dp), intent(in) :: Ksatmm

    if (SATvolPro <= 55.0_dp) then
        if (PWPvolPro >= 20.0_dp) then
            if ((SATvolPro >= 49.0_dp) .and. (FCvolPro >= 40.0_dp)) then
                NumberSoilClass = 4  ! silty clayey soils
            else
                NumberSoilClass = 3  ! sandy clayey soils
            end if
        else
            if (FCvolPro < 23.0_dp) then
                NumberSoilClass = 1 ! sandy soils
            else
                if ((PWPvolPro > 16.0_dp) .and. (Ksatmm < 100.0_dp)) then
                    NumberSoilClass = 3 ! sandy clayey soils
                else
                    if ((PWPvolPro < 6.0_dp) .and. (FCvolPro < 28.0_dp) &
                        .and. (Ksatmm >750.0_dp)) then
                        NumberSoilClass = 1 ! sandy soils
                    else
                        NumberSoilClass = 2  ! loamy soils
                    end if
                end if
            end if
        end if
    else
        NumberSoilClass = 4 ! silty clayey soils
    end if
end function NumberSoilClass


subroutine DeriveSmaxTopBottom(SxTopQ, SxBotQ, SxTop, SxBot)
    real(dp), intent(in) :: SxTopQ
    real(dp), intent(in) :: SxBotQ
    real(dp), intent(inout) :: SxTop
    real(dp), intent(inout) :: SxBot

    real(dp) :: x, V1, V2, V11, V22

    V1 = SxTopQ
    V2 = SxBotQ
    if (abs(V1 - V2) < 1e-12_dp) then
        SxTop = V1
        SxBot = V2
    else
        if (SxTopQ < SxBotQ) then
            V1 = SxBotQ
            V2 = SxTopQ
        end if
        x = 3.0_dp * V2/(V1-V2)
        if (x < 0.5_dp) then
            V11 = (4.0_dp/3.5_dp) * V1
            V22 = 0.0_dp
        else
            V11 = (x + 3.5_dp) * V1/(x+3.0_dp)
            V22 = (x - 0.5_dp) * V2/x
        end if
        if (SxTopQ > SxBotQ) then
            SxTop = V11
            SxBot = V22
        else
            SxTop = V22
            SxBot = V11
        end if
    end if
end subroutine DeriveSmaxTopBottom


real(dp) function KsTemperature(T0, T1, Tin)
    real(dp), intent(in) :: T0
    real(dp), intent(in) :: T1
    real(dp), intent(in) :: Tin

    real(dp) :: M
    integer(int8) :: a

    M = 1._dp ! no correction applied (TO and/or T1 is undefined, or T0=T1)
    if (((nint(T0, kind=int32) /= undef_int) .and. &
         (nint(T1, kind=int32) /= undef_int)) .and. abs(T0-T1)> eps) then
        if (T0 < T1) then
            a =  1  ! cold stress
        else
            a = -1 ! heat stress
        end if
        if ((a*Tin > a*T0) .and. (a*Tin < a*T1)) then 
           ! within range for correction
            M = GetKs(T0, T1, Tin)
            if (M < 0) then
                M = 0._dp
            end if
            if (M > 1) then
                M = 1._dp
            end if
        else
            if (a*Tin <= a*T0) then
                M = 0._dp
            end if
            if (a*Tin >= a*T1) then
                M = 1._dp
            end if
        end if
    end if
    KsTemperature = M

    contains
    real(dp) function GetKs(T0, T1, Tin)
        real(dp), intent(in) :: T0
        real(dp), intent(in) :: T1
        real(dp), intent(in) :: Tin

        real(dp), parameter  :: Mo = 0.02_dp
        real(dp), parameter  :: Mx = 1.0_dp

        real(dp) :: MRate, Ksi, Trel

        Trel = (Tin-T0)/(T1-T0)
        ! derive rate of increase (MRate)
        MRate = (-1._dp)*(log((Mo*Mx-0.98_dp*Mo)/(0.98_dp*(Mx-Mo))))
        ! get Ks from logistic equation
        Ksi = (Mo*Mx)/(Mo+(Mx-Mo)*exp(-MRate*Trel))
        ! adjust for Mo
        Ksi = Ksi - Mo * (1._dp - Trel)
        GetKs = Ksi
     end function GetKs
end function KsTemperature


real(dp) function KsSalinity(SalinityResponsConsidered, &
                ECeN, ECeX, ECeVAR, KsShapeSalinity)
    logical, intent(in) :: SalinityResponsConsidered
    integer(int8), intent(in) :: ECeN
    integer(int8), intent(in) :: ECeX
    real(dp), intent(in) :: ECeVAR
    real(dp), intent(in) :: KsShapeSalinity

    real(dp) :: M, tmp_var

    M = 1._dp ! no correction applied
    if (SalinityResponsConsidered) then
        if ((ECeVAR > ECeN) .and. (ECeVar < ECeX)) then
            ! within range for correction
            if ((nint(KsShapeSalinity*10._dp) /= 0) .and. &
                (nint(KsShapeSalinity*10) /= 990)) then
                tmp_var = real(ECeN, kind=dp)
                M = KsAny(ECeVar, tmp_var, real(ECeX, kind=dp), KsShapeSalinity) 
                ! convex or concave
            else
                if (nint(KsShapeSalinity*10._dp) == 0) then
                    M = 1._dp - (ECeVAR-ECeN)/(ECeX-ECeN) 
                    ! linear (KsShapeSalinity = 0)
                else
                    M = KsTemperature(real(ECeX, kind=dp), real(ECeN, kind=dp), ECeVAR) 
                    ! logistic equation (KsShapeSalinity = 99)
                end if
            end if
        else
            if (ECeVAR <= ECeN) then
                M = 1._dp  ! no salinity stress
            end if
            if (ECeVar >= ECeX) then
                M = 0._dp  ! full salinity stress
            end if
        end if
    end if
    if (M > 1) then
        M = 1._dp
    end if
    if (M < 0) then
        M = 0._dp
    end if
    KsSalinity = M
end function KsSalinity

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
    
    real(dp) :: CCToReach
    integer(int32) :: L12SFmax

    if ((ClassSF == 0) .or. ((RedCCx == 0) .and. (RedCGC == 0))) then
        L12SF = L12
    else
        CCToReach = 0.98_dp*(1-RedCCX/100._dp)*CCx
        L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100._dp)*CCx), (CGC*(1-(RedCGC)/100._dp)), L0)
        ! determine L12SFmax
        if (DeterminantCrop) then
            L12SFmax = LToFlor + nint(LFlor/2._dp, kind=int32)
        else
            L12SFmax = L123
        end if
        ! check for L12SFmax
        if (L12SF > L12SFmax) then
            ! full canopy cannot be reached in potential period for vegetative growth
            ! ClassSF := undef_int; ! switch to user defined soil fertility
            ! 1. increase CGC(soil fertility)
            do while ((L12SF > L12SFmax) .and. (RedCGC > 0))
                RedCGC = RedCGC - 1
                L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100._dp)*CCx), (CGC*(1-(RedCGC)/100._dp)), L0)
            end do
            ! 2. if not sufficient decrease CCx(soil fertility)
            do while ((L12SF > L12SFmax) .and. ( ((1-RedCCX/100._dp)*CCx) > 0.10_dp) .and. (RedCCx <= 50))
                RedCCx = RedCCx + 1
                CCToReach = 0.98_dp*(1-RedCCX/100._dp)*CCx
                L12SF = DaysToReachCCwithGivenCGC(CCToReach, CCo, ((1-RedCCX/100._dp)*CCx), (CGC*(1-(RedCGC)/100._dp)), L0)
            end do
        end if
    end if
end subroutine TimeToMaxCanopySF


real(dp) function SoilEvaporationReductionCoefficient(Wrel, Edecline)
    real(dp), intent(in) :: Wrel
    real(dp), intent(in) :: Edecline

    if (Wrel <= 0.00001_dp) then
        SoilEvaporationReductionCoefficient = 0.0_dp
    else
        if (Wrel >= 0.99999_dp) then
            SoilEvaporationReductionCoefficient = 1.0_dp
        else
            SoilEvaporationReductionCoefficient =&
                (exp(Edecline*Wrel) - 1.0_dp)/(exp(Edecline) - 1.0_dp)
        end if
    end if
end function SoilEvaporationReductionCoefficient


real(dp) function MaxCRatDepth(ParamCRa, ParamCRb, Ksat, Zi, DepthGWT)
    real(dp), intent(in) :: ParamCRa
    real(dp), intent(in) :: ParamCRb
    real(dp), intent(in) :: Ksat
    real(dp), intent(in) :: Zi
    real(dp), intent(in) :: DepthGWT

    real(dp) :: CRmax

    CRmax = 0._dp
    if ((Ksat > 0._dp) .and. (DepthGWT > 0._dp) .and. ((DepthGWT-Zi) < 4._dp)) then
        if (Zi >= DepthGWT) then
            CRmax = 99._dp
        else
            CRmax = exp((log(DepthGWT - Zi) - ParamCRb)/ParamCRa)
            if (CRmax > 99._dp) then
                CRmax = 99._dp
            end if
        end if
    end if
    MaxCRatDepth = CRmax
end function MaxCRatDepth


real(dp) function CCmultiplierWeed(ProcentWeedCover, CCxCrop, FshapeWeed)
    integer(int8), intent(in) :: ProcentWeedCover
    real(dp), intent(in) :: CCxCrop
    real(dp), intent(in) :: FshapeWeed

    real(dp) :: fWeed

    if ((ProcentWeedCover > 0) .and. (CCxCrop < 0.9999_dp) .and. (CCxCrop > 0.001_dp)) then
        if (ProcentWeedCover == 100) then
            fWeed = 1._dp/CCxCrop
        else
            fWeed = 1._dp - (1._dp - 1._dp/CCxCrop) * &
              (exp(FshapeWeed*ProcentWeedCover/100._dp) - 1._dp)/(exp(FshapeWeed) - 1._dp)
            if (fWeed > (1._dp/CCxCrop)) then
                fWeed = 1._dp/CCxCrop
            end if
        end if
    else
        fWeed = 1._dp
    end if
    CCmultiplierWeed = fWeed
end function CCmultiplierWeed


real(dp) function BMRange(HIadj)
    integer(int32), intent(in) :: HIadj

    real(dp) :: BMR

    if (HIadj <= 0) then
        BMR = 0.0_dp
    else
        BMR = (log(real(HIadj, kind=dp))/0.0562_dp)/100.0_dp
    end if
    if (BMR > 1.0_dp) then
        BMR = 1.0_dp
    end if
    BMRange = BMR
end function BMRange


real(dp) function HImultiplier(RatioBM, RangeBM, HIadj)
    real(dp), intent(in) :: RatioBM
    real(dp), intent(in) :: RangeBM
    integer(int8), intent(in) :: HIadj

    real(dp) :: Rini, Rmax, Rend

    Rini = 1.0_dp - RangeBM
    REnd = 1.0_dp
    RMax = Rini + (2.0_dp/3.0_dp) * (REnd-Rini)
    if (RatioBM <= RIni) then
        HImultiplier = 1.0_dp
    elseif (RatioBM <= RMax) then
        HImultiplier = 1.0_dp&
            + (1.0_dp + sin(PI*(1.5_dp-(RatioBM-RIni)/(RMax-RIni))))&
            *(HIadj/200.0_dp)
    elseif (RatioBM <= REnd) then
        HImultiplier = 1.0_dp&
            + (1.0_dp + sin(PI*(0.5_dp+(RatioBM-RMax)/(REnd-RMax))))&
            *(HIadj/200.0_dp)
    else
        HImultiplier = 1.0_dp
    end if
end function HImultiplier


real(dp) function CCatTime(Dayi, CCoIN, CGCIN, CCxIN)
    integer(int32), intent(in) :: Dayi
    real(dp), intent(in) :: CCoIN
    real(dp), intent(in) :: CGCIN
    real(dp), intent(in) :: CCxIN

    real(dp) :: CCi

    CCi = CCoIN * exp(CGCIN * Dayi)
    if (CCi > CCxIN/2._dp) then
        CCi = CCxIN - 0.25_dp * (CCxIN/CCoIN) * CCxIN * exp(-CGCIN*Dayi)
    end if
    CCatTime = CCi
end function CCatTime


subroutine DetermineDayNr(Dayi, Monthi, Yeari, DayNr)
    integer(int32), intent(in) :: Dayi
    integer(int32), intent(in) :: Monthi
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(inout) :: DayNr

    DayNr = trunc((Yeari - 1901)*365.25_dp + ElapsedDays(Monthi) + Dayi + 0.05_dp)
    
end subroutine DetermineDayNr


subroutine DetermineDate(DayNr, Dayi, Monthi, Yeari)
    integer(int32), intent(in) :: DayNr
    integer(int32), intent(inout) :: Dayi
    integer(int32), intent(inout) :: Monthi
    integer(int32), intent(inout) :: Yeari

    real(dp) :: SumDayMonth

    Yeari = trunc((DayNr-0.05_dp)/365.25_dp)
    SumDayMonth = (DayNr - Yeari*365.25_dp)
    Yeari = 1901 + Yeari
    Monthi = 1

    do while (Monthi < 12)
        if (SumDayMonth <= ElapsedDays(Monthi+1)) exit
        Monthi = Monthi + 1
    end do
    Dayi = nint(SumDayMonth - ElapsedDays(Monthi) + 0.25_dp + 0.06_dp, kind=int32)
end subroutine DetermineDate


real(dp) function DegreesDay(Tbase, Tupper, TDayMin, TDayMax, GDDSelectedMethod)
    real(dp), intent(in) :: Tbase
    real(dp), intent(in) :: Tupper
    real(dp), intent(in) :: TDayMin
    real(dp), intent(in) :: TDayMax
    integer(int8), intent(in) :: GDDSelectedMethod

    real(dp) :: TstarMax, TstarMin
    real(dp) :: Tavg, DgrD

    select case (GDDSelectedMethod)
    case (1)
        ! Method 1. - No adjustemnt of Tmax, Tmin before calculation of Taverage
        Tavg = (TDayMax+TDayMin)/2._dp
        if (Tavg > Tupper) then
            Tavg = Tupper
        end if
        if (Tavg < Tbase) then
            Tavg = Tbase
        end if

    case (2)
        ! Method 2. -  Adjustment for Tbase before calculation of Taverage
        TstarMax = TDayMax
        if (TDayMax < Tbase) then
            TstarMax = Tbase
        end if
        if (TDayMax > Tupper) then
            TstarMax = Tupper
        end if
        TstarMin = TDayMin
        if (TDayMin < Tbase) then
            TstarMin = Tbase
        end if
        if (TDayMin > Tupper) then
            TstarMin = Tupper
        end if
        Tavg = (TstarMax+TstarMin)/2._dp

    case default
       ! Method 3.
        TstarMax = TDayMax
        if (TDayMax < Tbase) then
             TstarMax = Tbase
        end if
        if (TDayMax > Tupper) then
            TstarMax = Tupper
        end if
        TstarMin = TDayMin
        if (TDayMin > Tupper) then
            TstarMin = Tupper
        end if
        Tavg = (TstarMax+TstarMin)/2._dp
        if (Tavg < Tbase) then
            Tavg = Tbase
        end if
    end select
    DgrD =  Tavg - Tbase
    DegreesDay =  DgrD
end function DegreesDay



subroutine DetermineCNIandIII(CN2, CN1, CN3)
    integer(int8), intent(in) :: CN2
    integer(int8), intent(inout) :: CN1
    integer(int8), intent(inout) :: CN3

    CN1 = nint(1.4_dp*(exp(-14*log(10._dp))) + 0.507_dp*CN2 &
                - 0.00374_dp*CN2*CN2 + 0.0000867_dp*CN2*CN2*CN2, kind=int8)
    CN3 = nint(5.6_dp*(exp(-14*log(10._dp))) + 2.33_dp*CN2 &
               - 0.0209_dp*CN2*CN2 + 0.000076_dp*CN2*CN2*CN2, kind=int8)

    if (CN1 <= 0) then
        CN1 = 1
    elseif (CN1 > 100) then
        CN1 = 100
    end if
    if (CN3 <= 0) then
        CN3 = 1
    elseif (CN3 > 100) then
        CN3 = 100
    end if
    if (CN3 < CN2) then
        CN3 = CN2
    end if
end subroutine DetermineCNIandIII


subroutine DetermineCN_default(Infiltr, CN2)
    real(dp), intent(in) :: Infiltr
    integer(int8), intent(inout) :: CN2

    if (Infiltr > 864) then
        CN2 = 46
    elseif (Infiltr >= 347) then
        CN2 = 61
    elseif (Infiltr >= 36) then
        CN2 = 72
    else
        CN2 = 77
    end if
end subroutine DetermineCN_default


real(dp) function MultiplierCCoSelfThinning(Yeari, Yearx, ShapeFactor)
    integer(int32), intent(in) :: Yeari
    integer(int32), intent(in) :: Yearx
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: fCCo, Year0

    fCCo = 1._dp
    if ((Yeari >= 1) .and. (Yearx >= 2) .and. (nint(100*ShapeFactor) /= 0)) then
        Year0 = 1._dp + (Yearx-1) * exp(ShapeFactor*log(10._dp))
        if ((Yeari >= Year0) .or. (Year0 <= 1)) then
            fCCo = 0._dp
        else
            fCCo = 1._dp - (Yeari-1)/(Year0-1._dp)
        end if
        if (fCCo < 0._dp) then
            fCCo = 0._dp
        end if
    end if
    MultiplierCCoSelfThinning = fCCo
end function MultiplierCCoSelfThinning


real(dp) function KsAny(Wrel, pULActual, pLLActual, ShapeFactor)
    real(dp), intent(in) :: Wrel
    real(dp), intent(inout) :: pULActual
    real(dp), intent(in) :: pLLActual
    real(dp), intent(in) :: ShapeFactor

    real(dp) :: pRelativeLLUL, KsVal
    ! Wrel : WC in rootzone (negative .... 0=FC ..... 1=WP .... > 1)
    !        FC .. UpperLimit ... LowerLimit .. WP
    ! p relative (negative .... O=UpperLimit ...... 1=LowerLimit .....>1)

    if ((pLLActual - pULActual) < 0.0001_dp) then
        pULActual = pLLActual - 0.0001_dp
    end if

    pRelativeLLUL = (Wrel - pULActual)/(pLLActual - pULActual)

    if (pRelativeLLUL <= 0._dp) then
        KsVal = 1._dp
    elseif (pRelativeLLUL >= 1._dp) then
        KsVal = 0._dp
    else
        if (nint(10*ShapeFactor) == 0) then ! straight line
            KsVal = 1._dp - &
                    (exp(pRelativeLLUL*0.01_dp)-1._dp)/(exp(0.01_dp)-1._dp)
        else
            KsVal = 1._dp - &
                    (exp(pRelativeLLUL*ShapeFactor)-1._dp)/(exp(ShapeFactor)-1._dp)
        end if
        if (KsVal > 1._dp) then
            KsVal = 1._dp
        end if
        if (KsVal < 0._dp) then
            KsVal = 0._dp
        end if
    end if
    KsAny = KsVal
end function KsAny

real(dp) function CCatGDD(GDDi, CCoIN, GDDCGCIN, CCxIN)
    real(dp), intent(in) :: GDDi
    real(dp), intent(in) :: CCoIN
    real(dp), intent(in) :: GDDCGCIN
    real(dp), intent(in) :: CCxIN

    real(dp) :: CCi

    CCi = CCoIN * exp(GDDCGCIN * GDDi)
    if (CCi > CCxIN/2._dp) then
        CCi = CCxIN - 0.25_dp * (CCxIN/CCoIN) * CCxIN * exp(-GDDCGCIN*GDDi)
    end if
    CCatGDD = CCi
end function CCatGDD


real(dp) function CanopyCoverNoStressGDDaysSF(GDDL0, GDDL123, GDDLMaturity, SumGDD, &
        CCo, CCx, GDDCGC, GDDCDC, SFRedCGC, SFRedCCx)
    integer(int32), intent(in) :: GDDL0
    integer(int32), intent(in) :: GDDL123
    integer(int32), intent(in) :: GDDLMaturity
    real(dp), intent(in) :: SumGDD
    real(dp), intent(in) :: CCo
    real(dp), intent(in) :: CCx
    real(dp), intent(in) :: GDDCGC
    real(dp), intent(in) :: GDDCDC
    integer(int8), intent(in) :: SFRedCGC
    integer(int8), intent(in) :: SFRedCCx

    real(dp) :: CC, CCxAdj, GDDCDCadj

    ! SumGDD refers to the end of the day and Delayed days are not considered
    CC = 0._dp
    if ((SumGDD > 0._dp) .and. (nint(SumGDD, kind=int32) <= GDDLMaturity) .and. (CCo > 0._dp)) then
        if (SumGDD <= GDDL0) then ! before germination or recovering of transplant
            CC = 0._dp
        else
            if (SumGDD < GDDL123) then ! Canopy development and Mid-season stage
                CC = CCatGDD(real((SumGDD-GDDL0),dp), CCo, ((1._dp-SFRedCGC/100._dp)*GDDCGC), & 
                  ((1._dp-SFRedCCx/100._dp)*CCx))
            else
                ! Late-season stage  (SumGDD <= GDDLMaturity)
                if (CCx < 0.001_dp) then
                    CC = 0._dp
                else
                    CCxAdj = CCatGDD(real(GDDL123-GDDL0,dp), CCo, ((1_dp-SFRedCGC/100._dp)*GDDCGC), &
                      ((1._dp-SFRedCCx/100._dp)*CCx))
                    GDDCDCadj = GDDCDC*(CCxadj+2.29_dp)/(CCx+2.29_dp)
                    if (CCxAdj < 0.001_dp) then
                        CC = 0._dp
                    else
                        CC = CCxAdj * (1._dp - 0.05_dp*(exp((SumGDD-GDDL123)*3.33_dp*GDDCDCadj/&
                          (CCxAdj+2.29_dp))-1._dp))
                    end if
                end if
            end if
        end if
    end if
    if (CC > 1._dp) then
        CC = 1._dp
    end if
    if (CC < 0._dp) then
        CC = 0._dp
    end if
    CanopyCoverNoStressGDDaysSF = CC
end function CanopyCoverNoStressGDDaysSF


real(dp) function HIadjWStressAtFlowering(KsVeg, KsSto, a, b)
    real(dp), intent(in) :: KsVeg
    real(dp), intent(in) :: KsSto
    integer(int8), intent(in) :: a
    real(dp), intent(in) :: b

    if (a == undef_int) then
        if (nint(b, kind=int32) == undef_int) then
            HIadjWStressAtFlowering = 1._dp
        elseif (KsSto > 0.001_dp) then
            HIadjWStressAtFlowering = (exp(0.10_dp*log(KsSto))) * (1._dp-(1._dp-KsSto)/b)
        else
            HIadjWStressAtFlowering = 0.0_dp
        end if
    else
        if (nint(b, kind=int32) == undef_int) then
            HIadjWStressAtFlowering = (1._dp + (1._dp-KsVeg)/a)
        elseif (KsSto > 0.001_dp) then
            HIadjWStressAtFlowering = (1._dp + (1._dp-KsVeg)/a) * (exp(0.10_dp*log(KsSto))) &
              * (1._dp-(1._dp-KsSto)/b)
        else
            HIadjWStressAtFlowering = 0._dp
        end if
    end if
end function HIadjWStressAtFlowering


real(dp) function fAdjustedForCO2(CO2i, WPi, PercentA)
    real(dp), intent(in) :: CO2i
    real(dp), intent(in) :: WPi
    integer(int8), intent(in) :: PercentA

    real(dp) :: fW, fCO2Old, fType, fSink, fShape, CO2rel, fCO2adj, fCO2

    ! 1. Correction for crop type: fType
    if (WPi >= 40._dp) then
        fType = 0._dp ! no correction for C4 crops
    else
        if (WPi <= 20._dp) then
            fType = 1._dp ! full correction for C3 crops
        else
            fType = (40._dp-WPi)/(40._dp-20._dp)
        end if
    end if

    ! 2. crop sink strength coefficient: fSink
    fSink = PercentA/100._dp
    if (fSink < 0._dp) then
        fSink = 0._dp ! based on FACE expirements
    end if
    if (fSink > 1._dp) then
        fSink = 1._dp ! theoretical adjustment
    end if

    ! 3. Correction coefficient for CO2: fCO2Old
    fCO2Old = undef_int
    if (CO2i <= 550._dp) then
        ! 3.1 weighing factor for CO2
        if (CO2i <= CO2Ref) then
            fw = 0._dp
        else
            if (CO2i >= 550._dp) then
                fW = 1._dp
            else
                fw = 1._dp - (550._dp - CO2i)/(550._dp - CO2Ref)
            end if
        end if
        
        ! 3.2 adjustment for CO2
        fCO2Old = (CO2i/CO2Ref)/(1._dp+(CO2i-CO2Ref)*((1._dp-fW)*0.000138_dp&
            + fW*(0.000138_dp*fSink + 0.001165_dp*(1._dp-fSink))))
    end if

    ! 4. Adjusted correction coefficient for CO2: fCO2adj
    fCO2adj = undef_int
    if (CO2i > CO2Ref) then
        ! 4.1 Shape factor
        fShape = -4.61824_dp - 3.43831_dp*fSink - 5.32587_dp*fSink*fSink
        
        ! 4.2 adjustment for CO2
        if (CO2i >= 2000._dp) then
            fCO2adj = 1.58_dp  ! maximum is reached
        else
            CO2rel = (CO2i-CO2Ref)/(2000._dp-CO2Ref)
            fCO2adj = 1._dp + 0.58_dp * ((exp(CO2rel*fShape) - 1._dp)/&
                (exp(fShape) - 1._dp))
        end if
    end if

    ! 5. Selected adjusted coefficient for CO2: fCO2
    if (CO2i <= CO2Ref) then
        fCO2 = fCO2Old
    else
        fCO2 = fCO2adj
        if ((CO2i <= 550._dp) .and. (fCO2Old < fCO2adj)) then
            fCO2 = fCO2Old
        end if
    end if

    ! 6. final adjustment
    fAdjustedForCO2 = 1._dp + fType*(fCO2-1._dp)
end function fAdjustedForCO2


logical function FullUndefinedRecord(FromY, FromD, FromM, ToD, ToM)
    integer(int32), intent(in) :: FromY
    integer(int32), intent(in) :: FromD
    integer(int32), intent(in) :: FromM
    integer(int32), intent(in) :: ToD
    integer(int32), intent(in) :: ToM

    FullUndefinedRecord = ((FromY == 1901) .and. (FromD == 1)&
        .and. (FromM == 1) .and. (ToD == 31) .and. (ToM == 12))
end function FullUndefinedRecord


subroutine GenerateCO2Description(CO2FileFull, CO2Description)
    character(len=*), intent(in) :: CO2FileFull
    character(len=*), intent(inout) :: CO2Description

    integer :: fhandle

    open(newunit=fhandle, file=trim(CO2FileFull), status='old', &
         action='read')
    read(fhandle, *) CO2Description
    close(fhandle)

    if (trim(GetCO2File()) == 'MaunaLoa.CO2') then
        ! since this is an AquaCrop file, the Description is determined by AquaCrop
        CO2Description = 'Default atmospheric CO2 concentration from 1902 to 2099'
    end if
end subroutine GenerateCO2Description


subroutine GetIrriDescription(IrriFileFull, IrriDescription)
    character(len=*), intent(in) :: IrriFileFull
    character(len=*), intent(inout) :: IrriDescription

    integer :: fhandle

    open(newunit=fhandle, file=trim(IrriFileFull), status='old', &
         action='read')
    read(fhandle, *) IrriDescription
    close(fhandle)
end subroutine GetIrriDescription


subroutine GetDaySwitchToLinear(HImax, dHIdt, HIGC, tSwitch, HIGClinear)
    integer(int32), intent(in) :: HImax
    real(dp), intent(in) :: dHIdt
    real(dp), intent(in) :: HIGC
    integer(int32), intent(inout) :: tSwitch
    real(dp), intent(inout) :: HIGClinear

    real(dp) :: HIi, HiM1, HIfinal
    integer(int32) :: tmax, ti
    integer(int32), parameter :: HIo = 1

    tmax = nint(HImax/dHIdt, kind=int32)
    ti = 0
    HiM1 = HIo
    if (tmax > 0) then
        loop: do
            ti = ti + 1
            HIi = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*ti))
            HIfinal = HIi + (tmax - ti)*(HIi-HIM1)
            HIM1 = HIi
            if ((HIfinal > HImax) .or. (ti >= tmax)) exit loop
        end do loop 
        tSwitch = ti - 1
    else
        tSwitch = 0
    end if
    if (tSwitch > 0) then
        HIi = (HIo*HImax)/ (HIo+(HImax-HIo)*exp(-HIGC*tSwitch))
    else
        HIi = 0
    end if
    HIGClinear = (HImax-HIi)/(tmax-tSwitch)
end subroutine GetDaySwitchToLinear

subroutine GetNumberSimulationRuns(TempFileNameFull, NrRuns)
    character(len=*), intent(in) :: TempFileNameFull
    integer(int32), intent(inout) :: NrRuns

    integer :: fhandle
    integer(int32) :: NrFileLines, rc, i

    NrRuns = 1

    open(newunit=fhandle, file=trim(TempFileNameFull), status='old', &
         action='read', iostat=rc)
    read(fhandle, *, iostat=rc)  ! Description
    read(fhandle, *, iostat=rc)  ! AquaCrop version Nr

    do i = 1, 5 
        read(fhandle, *, iostat=rc) ! Type year and Simulation and Cropping period Run 1
    end do

    NrFileLines = 42 ! Clim(15),Calendar(3),Crop(3),Irri(3),Field(3),Soil(3),Gwt(3),Inni(3),Off(3),FieldData(3)
    do i = 1, NrFileLines 
        read(fhandle, *, iostat=rc) ! Files Run 1
    end do

    read_loop: do
        i = 0
        do while (i < (NrFileLines+5))
            read(fhandle, *, iostat=rc)
            if (rc == iostat_end) exit read_loop
            i = i + 1
        end do

        if (i == (NrFileLines+5)) then
            NrRuns = NrRuns + 1
        end if
    end do read_loop
    close(fhandle)
end subroutine GetNumberSimulationRuns

logical function FileExists(full_name)
    character(len=*), intent(in) :: full_name

    inquire(file=trim(full_name), exist=FileExists)
end function FileExists 


subroutine SplitStringInTwoParams(StringIN, Par1, Par2)
    character(len=*), intent(in) :: StringIN
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2

    integer(int32) :: LengthS, i, Parami
    character :: CharA
    character(len=255) :: StringNumber

    LengthS = len(StringIN)
    i = 0
    Parami = 0
    ! divide the line in parameters
    do while ((i < LengthS) .and. (Parami < 2)) 
        i = i + 1
        CharA = StringIN(i:i)
        if (ichar(CharA) > 32) then
            ! next Parameter
            Parami = Parami + 1
            StringNumber = ''
            do while ((ichar(CharA) > 32) .and. (i <= LengthS))
                StringNumber = trim(StringNumber) // CharA
                i = i + 1
                if (i <= LengthS) then
                    CharA = StringIN(i:i)
                end if
            end do
            if (Parami == 1) then
                read(StringNumber, *) Par1
            end if
            if (Parami == 2) then
                read(StringNumber, *) Par2
            end if
            ! next Parameter
        end if
        ! end of line
    end do
end subroutine SplitStringInTwoParams


subroutine SplitStringInThreeParams(StringIN, Par1, Par2, Par3)
    character(len=*), intent(in) :: StringIN
    real(dp), intent(inout) :: Par1
    real(dp), intent(inout) :: Par2
    real(dp), intent(inout) :: Par3

    integer(int32) :: LengthS, i, Parami
    character :: CharA
    character(len=255) :: StringNumber

    LengthS = len(StringIN)
    i = 0
    Parami = 0
    ! divide the line in parameters
    do while ((i < LengthS) .and. (Parami < 3)) 
        i = i + 1
        CharA = StringIN(i:i)
        if (ichar(CharA) > 32) then
            ! next Parameter
            Parami = Parami + 1
            StringNumber = ''
            do while ((ichar(CharA) > 32) .and. (i <= LengthS))
                StringNumber = trim(StringNumber) // CharA
                i = i + 1
                if (i <= LengthS) then
                    CharA = StringIN(i:i)
                end if
            end do
            if (Parami == 1) then
                read(StringNumber, *) Par1
            end if
            if (Parami == 2) then
                read(StringNumber, *) Par2
            end if
            if (Parami == 3) then
                read(StringNumber, *) Par3
            end if
            ! next Parameter
        end if
        ! end of line
    end do
end subroutine SplitStringInThreeParams


subroutine LoadClimate(FullName, ClimateDescription, TempFile, EToFile, RainFile, CO2File)
    character(len=*), intent(in) :: FullName
    character(len=*), intent(inout) :: ClimateDescription
    character(len=*), intent(inout) :: TempFile
    character(len=*), intent(inout) :: EToFile
    character(len=*), intent(inout) :: RainFile
    character(len=*), intent(inout) :: CO2File

    integer :: fhandle

    open(newunit=fhandle, file=trim(FullName), status='old', &
         action='read')
    read(fhandle, *) ClimateDescription
    read(fhandle) ! AquaCrop Version
    read(fhandle, *) TempFile
    read(fhandle, *) EToFile
    read(fhandle, *) RainFile
    read(fhandle, *) CO2File 
    close(fhandle)
end subroutine LoadClimate

!! Global variables section !!

function GetIrriFile() result(str)
    !! Getter for the "IrriFile" global variable.
    character(len=len(IrriFile)) :: str

    str = IrriFile
end function GetIrriFile


subroutine SetIrriFile(str)
    !! Setter for the "IrriFile" global variable.
    character(len=*), intent(in) :: str

    IrriFile = str
end subroutine SetIrriFile


function GetIrriFileFull() result(str)
    !! Getter for the "IrriFileFull" global variable.
    character(len=len(IrriFileFull)) :: str

    str = IrriFileFull
end function GetIrriFileFull


subroutine SetIrriFileFull(str)
    !! Setter for the "IrriFileFull" global variable.
    character(len=*), intent(in) :: str

    IrriFileFull = str
end subroutine SetIrriFileFull


function GetClimateFile() result(str)
    !! Getter for the "ClimateFile" global variable.
    character(len=len(ClimateFile)) :: str
    
    str = ClimateFile
end function GetClimateFile

subroutine SetClimateFile(str)
    !! Setter for the "ClimateFile" global variable.
    character(len=*), intent(in) :: str
    
    ClimateFile = str
end subroutine SetClimateFile

function GetClimateFileFull() result(str)
    !! Getter for the "ClimateFileFull" global variable.
    character(len=len(ClimateFileFull)) :: str
    
    str = ClimateFileFull
end function GetClimateFileFull

subroutine SetClimateFileFull(str)
    !! Setter for the "ClimateFileFull" global variable.
    character(len=*), intent(in) :: str
    
    ClimateFileFull = str
end subroutine SetClimateFileFull

function GetClimFile() result(str)
    !! Getter for the "ClimFile" global variable.
    character(len=len(ClimFile)) :: str
    
    str = ClimFile
end function GetClimFile

subroutine SetClimFile(str)
    !! Setter for the "ClimFile" global variable.
    character(len=*), intent(in) :: str
    
    ClimFile = str
end subroutine SetClimFile

function GetSWCiniFile() result(str)
    !! Getter for the "SWCiniFile" global variable.
    character(len=len(SWCiniFile)) :: str
    
    str = SWCiniFile
end function GetSWCiniFile

subroutine SetSWCiniFile(str)
    !! Setter for the "SWCiniFile" global variable.
    character(len=*), intent(in) :: str
    
    SWCiniFile = str
end subroutine SetSWCiniFile


function GetSWCiniFileFull() result(str)
    !! Getter for the "SWCiniFileFull" global variable.
    character(len=len(SWCiniFileFull)) :: str
    
    str = SWCiniFileFull
end function GetSWCiniFileFull

subroutine SetSWCiniFileFull(str)
    !! Setter for the "SWCiniFileFull" global variable.
    character(len=*), intent(in) :: str
    
    SWCiniFileFull = str
end subroutine SetSWCiniFileFull


function GetPathNameProg() result(str)
    !! Getter for the "PathNameProg" global variable.
    character(len=len(PathNameProg)) :: str
    
    str = PathNameProg
end function GetPathNameProg

subroutine SetPathNameProg(str)
    !! Setter for the "PathNameProg" global variable.
    character(len=*), intent(in) :: str
    
    PathNameProg = str
end subroutine SetPathNameProg

function GetPathNameOutp() result(str)
    !! Getter for the "PathNameOutp" global variable.
    character(len=len(PathNameOutp)) :: str
    
    str = PathNameOutp
end function GetPathNameOutp

subroutine SetPathNameOutp(str)
    !! Setter for the "PathNameOutp" global variable.
    character(len=*), intent(in) :: str
    
    PathNameOutp = str
end subroutine SetPathNameOutp

function GetPathNameSimul() result(str)
    !! Getter for the "PathNameSimul" global variable.
    character(len=len(PathNameSimul)) :: str
    
    str = PathNameSimul
end function GetPathNameSimul

subroutine SetPathNameSimul(str)
    !! Setter for the "PathNameSimul" global variable.
    character(len=*), intent(in) :: str
    
    PathNameSimul = str
end subroutine SetPathNameSimul


function GetProjectFile() result(str)
    !! Getter for the "ProjectFile" global variable.
    character(len=len(ProjectFile)) :: str
    
    str = ProjectFile
end function GetProjectFile

subroutine SetProjectFile(str)
    !! Setter for the "ProjectFile" global variable.
    character(len=*), intent(in) :: str
    
    ProjectFile = str
end subroutine SetProjectFile

function GetProjectFileFull() result(str)
    !! Getter for the "ProjectFileFull" global variable.
    character(len=len(ProjectFileFull)) :: str
    
    str = ProjectFileFull
end function GetProjectFileFull

subroutine SetProjectFileFull(str)
    !! Setter for the "ProjectFileFull" global variable.
    character(len=*), intent(in) :: str
    
    ProjectFileFull = str
end subroutine SetProjectFileFull

function GetMultipleProjectFile() result(str)
    !! Getter for the "MultipleProjectFile" global variable.
    character(len=len(MultipleProjectFile)) :: str
    
    str = MultipleProjectFile
end function GetMultipleProjectFile

subroutine SetMultipleProjectFile(str)
    !! Setter for the "MultipleProjectFile" global variable.
    character(len=*), intent(in) :: str
    
    MultipleProjectFile = str
end subroutine SetMultipleProjectFile

function GetMultipleProjectFileFull() result(str)
    !! Getter for the "MultipleProjectFileFull" global variable.
    character(len=len(MultipleProjectFileFull)) :: str
    
    str = MultipleProjectFileFull
end function GetMultipleProjectFileFull

subroutine SetMultipleProjectFileFull(str)
    !! Setter for the "MultipleProjectFileFull" global variable.
    character(len=*), intent(in) :: str
    
    MultipleProjectFileFull = str
end subroutine SetMultipleProjectFileFull


logical function LeapYear(Year)
    integer(int32), intent(in) :: Year

    LeapYear = .false.
    if (frac(Year/4._dp) <= 0.01_dp) then
        LeapYear = .true.
    end if

    contains

    real(dp) function frac(val)
        real(dp), intent(in) :: val

        frac = val - floor(val)
    end function frac 
end function LeapYear


subroutine LoadProjectDescription(FullNameProjectFile, DescriptionOfProject)
    character(len=*), intent(in) :: FullNameProjectFile
    character(len=*), intent(inout) :: DescriptionOfProject

    integer :: fhandle

    open(newunit=fhandle, file=trim(FullNameProjectFile), status='old', action='read')
    read(fhandle, *) DescriptionOfProject
    DescriptionOfProject = trim(DescriptionOfProject)

    close(fhandle)
end subroutine LoadProjectDescription


subroutine CheckFilesInProject(TempFullFilename, Runi, AllOK)
    character(len=*), intent(in) :: TempFullFilename
    integer(int32), intent(in) :: Runi
    logical, intent(inout) :: AllOK

    integer :: fhandle
    character(len=:), allocatable :: TempFileName, TempPathName, TempFullName
    character(len=1024) :: buffer
    integer(int32) :: i, TotalFiles
  
    AllOK = .true.
    open(newunit=fhandle, file=trim(TempFullFilename), status='old', &
        action='read')
    read(fhandle, *) ! Description
    read(fhandle, *)  ! AquaCrop version Nr
    
    ! Prepare
    if (Runi > 1) then
        do i = 1, 5 
            read(fhandle, *) ! Type year and Simulation and Cropping period of run 1
        end do
        do i = 1, 42 
            read(fhandle, *) ! files previous runs
        end do
    end if

    ! Type Year and Simulation and Cropping period of the run
    do i = 1, 5 
        read(fhandle, *)
    end do

    ! Check the 14 files
    i = 1
    TotalFiles = 14
    do while (AllOK .and. (i <= TotalFiles))
        read(fhandle, *) ! Info
        read(fhandle, *) buffer  ! FileName
        TempFileName = trim(buffer)
        if (trim(TempFileName) == '(None)') then
            read(fhandle, *)
        else
            if ((i == (TotalFiles-2)) .and. (trim(TempFileName) == 'KeepSWC')) then ! file initial conditions
                read(fhandle, *) ! Keep initial SWC
            else
                read(fhandle, *) buffer ! PathName
                TempPathName = trim(buffer)
                TempFullName = trim(TempPathName) // trim(TempFileName)
                if (FileExists(trim(TempFullName)) .eqv. .false.) then
                    AllOK = .false.
                end if
            end if
        end if
        i = i + 1
    end do
    close(fhandle)
end subroutine CheckFilesInProject

!! Global variables section !!

function GetCO2File() result(str)
    !! Getter for the "CO2File" global variable.
    character(len=len(CO2File)) :: str

    str = CO2File
end function GetCO2File

subroutine SetCO2File(str)
    !! Setter for the "CO2File" global variable.
    character(len=*), intent(in) :: str

    CO2File = str
end subroutine SetCO2File

function GetCO2FileFull() result(str)
    !! Getter for the "CO2FileFull" global variable.
    character(len=len(CO2FileFull)) :: str

    str = CO2FileFull
end function GetCO2FileFull

subroutine SetCO2FileFull(str)
    !! Setter for the "CO2FileFull" global variable.
    character(len=*), intent(in) :: str

    CO2FileFull = str
end subroutine SetCO2FileFull

function GetCO2Description() result(str)
    !! Getter for the "CO2Description" global variable.
    character(len=len(CO2Description)) :: str

    str = CO2Description
end function GetCO2Description

subroutine SetCO2Description(str)
    !! Setter for the "CO2Description" global variable.
    character(len=*), intent(in) :: str

    CO2Description = str
end subroutine SetCO2Description

type(rep_RootZoneWC) function GetRootZoneWC()
    !! Getter for the "RootZoneWC" global variable.

    GetRootZoneWC = RootZoneWC
end function GetRootZoneWC

subroutine SetRootZoneWC_Actual(Actual)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Actual

    RootZoneWC%Actual = Actual
end subroutine SetRootZoneWC_Actual

subroutine SetRootZoneWC_FC(FC)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: FC

    RootZoneWC%FC = FC
end subroutine SetRootZoneWC_FC

subroutine SetRootZoneWC_WP(WP)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: WP

    RootZoneWC%WP = WP
end subroutine SetRootZoneWC_WP

subroutine SetRootZoneWC_SAT(SAT)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: SAT

    RootZoneWC%SAT = SAT
end subroutine SetRootZoneWC_SAT

subroutine SetRootZoneWC_Leaf(Leaf)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Leaf

    RootZoneWC%Leaf = Leaf
end subroutine SetRootZoneWC_Leaf

subroutine SetRootZoneWC_Thresh(Thresh)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Thresh

    RootZoneWC%Thresh = Thresh
end subroutine SetRootZoneWC_Thresh

subroutine SetRootZoneWC_Sen(Sen)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: Sen

    RootZoneWC%Sen = Sen
end subroutine SetRootZoneWC_Sen

subroutine SetRootZoneWC_ZtopAct(ZtopAct)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopAct

    RootZoneWC%ZtopAct = ZtopAct
end subroutine SetRootZoneWC_ZtopAct

subroutine SetRootZoneWC_ZtopFC(ZtopFC)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopFC

    RootZoneWC%ZtopFC = ZtopFC
end subroutine SetRootZoneWC_ZtopFC

subroutine SetRootZoneWC_ZtopWP(ZtopWP)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopWP

    RootZoneWC%ZtopWP = ZtopWP
end subroutine SetRootZoneWC_ZtopWP

subroutine SetRootZoneWC_ZtopThresh(ZtopThresh)
    !! Setter for the "RootZoneWC" global variable.
    real(dp), intent(in) :: ZtopThresh

    RootZoneWC%ZtopThresh = ZtopThresh
end subroutine SetRootZoneWC_ZtopThresh

function GetCalendarFile() result(str)
    !! Getter for the "CalendarFile" global variable.
    character(len=len(CalendarFile)) :: str

    str = CalendarFile
end function GetCalendarFile

subroutine SetCalendarFile(str)
    !! Setter for the "CalendarFile" global variable.
    character(len=*), intent(in) :: str

    CalendarFile = str
end subroutine SetCalendarFile

function GetCalendarFileFull() result(str)
    !! Getter for the "CalendarFileFull" global variable.
    character(len=len(CalendarFileFull)) :: str

    str = CalendarFileFull
end function GetCalendarFileFull

subroutine SetCalendarFileFull(str)
    !! Setter for the "CalendarFileFull" global variable.
    character(len=*), intent(in) :: str

    CalendarFileFull = str
end subroutine SetCalendarFileFull

function GetCropFile() result(str)
    !! Getter for the "CropFile" global variable.
    character(len=len(CropFile)) :: str

    str = CropFile
end function GetCropFile

subroutine SetCropFile(str)
    !! Setter for the "CropFile" global variable.
    character(len=*), intent(in) :: str

    CropFile = str
end subroutine SetCropFile

function GetCropFileFull() result(str)
    !! Getter for the "CropFile" global variable.
    character(len=len(CropFileFull)) :: str

    str = CropFileFull
end function GetCropFileFull

subroutine SetCropFileFull(str)
    !! Setter for the "CropFile" global variable.
    character(len=*), intent(in) :: str

    CropFileFull = str
end subroutine SetCropFileFull

type(rep_IrriECw) function GetIrriECw()
    !! Getter for the "IrriECw" global variable.

    GetIrriECw = IrriECw
end function GetIrriECw

subroutine SetIrriECw_PreSeason(PreSeason)
    !! Setter for the "soil" global variable.
    real(dp), intent(in) :: PreSeason

    IrriECw%PreSeason = PreSeason
end subroutine SetIrriECw_PreSeason

subroutine SetIrriECw_PostSeason(PostSeason)
    !! Setter for the "soil" global variable.
    real(dp), intent(in) :: PostSeason

    IrriECw%PostSeason = PostSeason
end subroutine SetIrriECw_PostSeason

function GetProfFile() result(str)
    !! Getter for the "ProfFile" global variable.
    character(len=len(ProfFile)) :: str

    str = ProfFile
end function GetProfFile

subroutine SetProfFile(str)
    !! Setter for the "ProfFile" global variable.
    character(len=*), intent(in) :: str

    ProfFile = str
end subroutine SetProfFile

function GetProfFilefull() result(str)
    !! Getter for the "ProfFilefull" global variable.
    character(len=len(ProfFilefull)) :: str

    str = ProfFilefull
end function GetProfFilefull

subroutine SetProfFilefull(str)
    !! Setter for the "ProfFilefull" global variable.
    character(len=*), intent(in) :: str

    ProfFilefull = str
end subroutine SetProfFilefull

function GetManFile() result(str)
    !! Getter for the "ManFile" global variable.
    character(len=len(ManFile)) :: str

    str = ManFile
end function GetManFile

subroutine SetManFile(str)
    !! Setter for the "ManFile" global variable.
    character(len=*), intent(in) :: str

    ManFile = str
end subroutine SetManFile

function GetManFilefull() result(str)
    !! Getter for the "ManFilefull" global variable.
    character(len=len(ManFilefull)) :: str

    str = ManFilefull
end function GetManFilefull

subroutine SetManFilefull(str)
    !! Setter for the "ManFilefull" global variable.
    character(len=*), intent(in) :: str

    ManFilefull = str
end subroutine SetManFilefull

function GetOffSeasonFile() result(str)
    !! Getter for the "OffSeasonFile" global variable.
    character(len=len(OffSeasonFile)) :: str

    str = OffSeasonFile
end function GetOffSeasonFile

subroutine SetOffSeasonFile(str)
    !! Setter for the "OffSeasonFile" global variable.
    character(len=*), intent(in) :: str

    OffSeasonFile = str
end subroutine SetOffSeasonFile

function GetOffSeasonFilefull() result(str)
    !! Getter for the "OffSeasonFilefull" global variable.
    character(len=len(OffSeasonFilefull)) :: str

    str = OffSeasonFilefull
end function GetOffSeasonFilefull

subroutine SetOffSeasonFilefull(str)
    !! Setter for the "OffSeasonFilefull" global variable.
    character(len=*), intent(in) :: str

    OffSeasonFilefull = str
end subroutine SetOffSeasonFilefull

function GetObservationsFile() result(str)
    !! Getter for the "ObservationsFile" global variable.
    character(len=len(ObservationsFile)) :: str

    str = ObservationsFile
end function GetObservationsFile

subroutine SetObservationsFile(str)
    !! Setter for the "ObservationsFile" global variable.
    character(len=*), intent(in) :: str

    ObservationsFile = str
end subroutine SetObservationsFile

function GetObservationsFilefull() result(str)
    !! Getter for the "ObservationsFilefull" global variable.
    character(len=len(ObservationsFilefull)) :: str

    str = ObservationsFilefull
end function GetObservationsFilefull

subroutine SetObservationsFilefull(str)
    !! Setter for the "ObservationsFilefull" global variable.
    character(len=*), intent(in) :: str

    ObservationsFilefull = str
end subroutine SetObservationsFilefull

function GetObservationsDescription() result(str)
    !! Getter for the "ObservationsDescription" global variable.
    character(len=len(ObservationsDescription)) :: str

    str = ObservationsDescription
end function GetObservationsDescription

subroutine SetObservationsDescription(str)
    !! Setter for the "ObservationsDescription" global variable.
    character(len=*), intent(in) :: str

    ObservationsDescription = str
end subroutine SetObservationsDescription

function GetGroundWaterFile() result(str)
    !! Getter for the "GroundWaterFile" global variable.
    character(len=len(GroundWaterFile)) :: str

    str = GroundWaterFile
end function GetGroundWaterFile

subroutine SetGroundWaterFile(str)
    !! Setter for the "GroundWaterFile" global variable.
    character(len=*), intent(in) :: str

    GroundWaterFile = str
end subroutine SetGroundWaterFile

function GetGroundWaterFilefull() result(str)
    !! Getter for the "GroundWaterFilefull" global variable.
    character(len=len(GroundWaterFilefull)) :: str

    str = GroundWaterFilefull
end function GetGroundWaterFilefull

subroutine SetGroundWaterFilefull(str)
    !! Setter for the "GroundWaterFilefull" global variable.
    character(len=*), intent(in) :: str

    GroundWaterFilefull = str
end subroutine SetGroundWaterFilefull

type(rep_CropFileSet) function GetCropFileSet()
    !! Getter for the "CropFileSet" global variable.

    GetCropFileSet = CropFileSet
end function GetCropFileSet

subroutine SetCropFileSet_DaysFromSenescenceToEnd(DaysFromSenescenceToEnd)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: DaysFromSenescenceToEnd

    CropFileSet%DaysFromSenescenceToEnd = DaysFromSenescenceToEnd
end subroutine SetCropFileSet_DaysFromSenescenceToEnd

subroutine SetCropFileSet_DaysToHarvest(DaysToHarvest)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: DaysToHarvest

    CropFileSet%DaysToHarvest = DaysToHarvest
end subroutine SetCropFileSet_DaysToHarvest

subroutine SetCropFileSet_GDDaysFromSenescenceToEnd(GDDaysFromSenescenceToEnd)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: GDDaysFromSenescenceToEnd

    CropFileSet%GDDaysFromSenescenceToEnd = GDDaysFromSenescenceToEnd
end subroutine SetCropFileSet_GDDaysFromSenescenceToEnd

subroutine SetCropFileSet_GDDaysToHarvest(GDDaysToHarvest)
    !! Setter for the "CropFileSet" global variable.
    integer(int32), intent(in) :: GDDaysToHarvest

    CropFileSet%GDDaysToHarvest = GDDaysToHarvest
end subroutine SetCropFileSet_GDDaysToHarvest


function GetEToFile() result(str)
    !! Getter for the "EToFile" global variable.
    character(len=len(EToFile)) :: str

    str = EToFile
end function GetEToFile


subroutine SetEToFile(str)
    !! Setter for the "EToFile" global variable.
    character(len=*), intent(in) :: str

    EToFile = str
end subroutine SetEToFile


function GetEToFileFull() result(str)
    !! Getter for the "EToFileFull" global variable.
    character(len=len(EToFileFull)) :: str

    str = EToFileFull
end function GetEToFileFull


subroutine SetEToFileFull(str)
    !! Setter for the "EToFileFull" global variable.
    character(len=*), intent(in) :: str

    EToFileFull = str
end subroutine SetEToFileFull


function GetEToDescription() result(str)
    !! Getter for the "EToDescription" global variable.
    character(len=len(EToDescription)) :: str

    str = EToDescription
end function GetEToDescription


subroutine SetEToDescription(str)
    !! Setter for the "EToDescription" global variable.
    character(len=*), intent(in) :: str

    EToDescription = str
end subroutine SetEToDescription


function GetRainFile() result(str)
    !! Getter for the "RainFile" global variable.
    character(len=len(RainFile)) :: str

    str = RainFile
end function GetRainFile


subroutine SetRainFile(str)
    !! Setter for the "RainFile" global variable.
    character(len=*), intent(in) :: str

    RainFile = str
end subroutine SetRainFile


function GetRainFileFull() result(str)
    !! Getter for the "RainFileFull" global variable.
    character(len=len(RainFileFull)) :: str

    str = RainFileFull
end function GetRainFileFull


subroutine SetRainFileFull(str)
    !! Setter for the "RainFileFull" global variable.
    character(len=*), intent(in) :: str

    RainFileFull = str
end subroutine SetRainFileFull


function GetRainDescription() result(str)
    !! Getter for the "RainDescription" global variable.
    character(len=len(RainDescription)) :: str

    str = RainDescription
end function GetRainDescription


subroutine SetRainDescription(str)
    !! Setter for the "RainDescription" global variable.
    character(len=*), intent(in) :: str

    RainDescription = str
end subroutine SetRainDescription


integer(int8) function GetManagement_Mulch()
    !! Getter for the "Management" global variable.

    GetManagement_Mulch = Management%Mulch
end function GetManagement_Mulch

integer(int8) function GetManagement_SoilCoverBefore()
    !! Getter for the "Management" global variable.

    GetManagement_SoilCoverBefore = Management%SoilCoverBefore
end function GetManagement_SoilCoverBefore

integer(int8) function GetManagement_SoilCoverAfter()
    !! Getter for the "Management" global variable.

    GetManagement_SoilCoverAfter = Management%SoilCoverAfter
end function GetManagement_SoilCoverAfter

integer(int8) function GetManagement_EffectMulchOffS()
    !! Getter for the "Management" global variable.

    GetManagement_EffectMulchOffS = Management%EffectMulchOffS
end function GetManagement_EffectMulchOffS

integer(int8) function GetManagement_EffectMulchInS()
    !! Getter for the "Management" global variable.

    GetManagement_EffectMulchInS = Management%EffectMulchInS
end function GetManagement_EffectMulchInS

integer(int8) function GetManagement_FertilityStress()
    !! Getter for the "Management" global variable.

    GetManagement_FertilityStress = Management%FertilityStress
end function GetManagement_FertilityStress

real(dp) function GetManagement_BundHeight()
    !! Getter for the "Management" global variable.

    GetManagement_BundHeight = Management%BundHeight
end function GetManagement_BundHeight

logical function GetManagement_RunoffOn()
    !! Getter for the "Management" global variable.

    GetManagement_RunoffOn = Management%RunoffOn
end function GetManagement_RunoffOn

integer(int32) function GetManagement_CNcorrection()
    !! Getter for the "Management" global variable.

    GetManagement_CNcorrection = Management%CNcorrection
end function GetManagement_CNcorrection

integer(int8) function GetManagement_WeedRC()
    !! Getter for the "Management" global variable.

    GetManagement_WeedRC = Management%WeedRC
end function GetManagement_WeedRC

integer(int32) function GetManagement_WeedDeltaRC()
    !! Getter for the "Management" global variable.

    GetManagement_WeedDeltaRC = Management%WeedDeltaRC
end function GetManagement_WeedDeltaRC

real(dp) function GetManagement_WeedShape()
    !! Getter for the "Management" global variable.

    GetManagement_WeedShape = Management%WeedShape
end function GetManagement_WeedShape

integer(int8) function GetManagement_WeedAdj()
    !! Getter for the "Management" global variable.

    GetManagement_WeedAdj = Management%WeedAdj
end function GetManagement_WeedAdj

type(rep_Cuttings) function GetManagement_Cuttings()
    !! Setter for the "Management" global variable.

    GetManagement_Cuttings = Management%Cuttings
end function GetManagement_Cuttings

subroutine SetManagement_Mulch(Mulch)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: Mulch

    Management%Mulch = Mulch
end subroutine SetManagement_Mulch

subroutine SetManagement_SoilCoverBefore(SoilCoverBefore)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: SoilCoverBefore

    Management%SoilCoverBefore = SoilCoverBefore
end subroutine SetManagement_SoilCoverBefore

subroutine SetManagement_SoilCoverAfter(SoilCoverAfter)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: SoilCoverAfter

    Management%SoilCoverAfter = SoilCoverAfter
end subroutine SetManagement_SoilCoverAfter

subroutine SetManagement_EffectMulchOffS(EffectMulchOffS)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: EffectMulchOffS

    Management%EffectMulchOffS = EffectMulchOffS
end subroutine SetManagement_EffectMulchOffS

subroutine SetManagement_EffectMulchInS(EffectMulchInS)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: EffectMulchInS

    Management%EffectMulchInS = EffectMulchInS
end subroutine SetManagement_EffectMulchInS

subroutine SetManagement_FertilityStress(FertilityStress)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: FertilityStress

    Management%FertilityStress = FertilityStress
end subroutine SetManagement_FertilityStress

subroutine SetManagement_BundHeight(BundHeight)
    !! Setter for the "Management" global variable.
    real(dp), intent(in) :: BundHeight

    Management%BundHeight = BundHeight
end subroutine SetManagement_BundHeight

subroutine SetManagement_RunoffOn(RunoffOn)
    !! Setter for the "Management" global variable.
    logical, intent(in) :: RunoffOn

    Management%RunoffOn = RunoffOn
end subroutine SetManagement_RunoffOn

subroutine SetManagement_CNcorrection(CNcorrection)
    !! Setter for the "Management" global variable.
    integer(int32), intent(in) :: CNcorrection

    Management%CNcorrection = CNcorrection
end subroutine SetManagement_CNcorrection

subroutine SetManagement_WeedRC(WeedRC)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: WeedRC

    Management%WeedRC = WeedRC
end subroutine SetManagement_WeedRC

subroutine SetManagement_WeedDeltaRC(WeedDeltaRC)
    !! Setter for the "Management" global variable.
    integer(int32), intent(in) :: WeedDeltaRC

    Management%WeedDeltaRC = WeedDeltaRC
end subroutine SetManagement_WeedDeltaRC

subroutine SetManagement_WeedShape(WeedShape)
    !! Setter for the "Management" global variable.
    real(dp), intent(in) :: WeedShape

    Management%WeedShape = WeedShape
end subroutine SetManagement_WeedShape

subroutine SetManagement_WeedAdj(WeedAdj)
    !! Setter for the "Management" global variable.
    integer(int8), intent(in) :: WeedAdj

    Management%WeedAdj = WeedAdj
end subroutine SetManagement_WeedAdj

subroutine SetManagement_Cuttings(Cuttings)
    !! Setter for the "Management" global variable.
    type(rep_Cuttings), intent(in) :: Cuttings

    Management%Cuttings = Cuttings
end subroutine SetManagement_Cuttings

logical function GetManagement_Cuttings_Considered()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Considered = Cuttings%Considered
end function GetManagement_Cuttings_Considered

integer(int32) function GetManagement_Cuttings_CCcut()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_CCcut = Cuttings%CCcut
end function GetManagement_Cuttings_CCcut

integer(int32) function GetManagement_Cuttings_CGCPlus()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_CGCPlus = Cuttings%CGCPlus
end function GetManagement_Cuttings_CGCPlus

integer(int32) function GetManagement_Cuttings_Day1()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Day1 = Cuttings%Day1
end function GetManagement_Cuttings_Day1

integer(int32) function GetManagement_Cuttings_NrDays()
    !! Setter for the "Cuttings" global variable.

    GetManagement_Cuttings_NrDays = Cuttings%NrDays
end function GetManagement_Cuttings_NrDays

logical function GetManagement_Cuttings_Generate()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Generate = Cuttings%Generate
end function GetManagement_Cuttings_Generate

integer(intEnum) function GetManagement_Cuttings_Criterion()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_Criterion = Cuttings%Criterion
end function GetManagement_Cuttings_Criterion

logical function GetManagement_Cuttings_HarvestEnd()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_HarvestEnd = Cuttings%HarvestEnd
end function GetManagement_Cuttings_HarvestEnd

integer(int32) function GetManagement_Cuttings_FirstDayNr()
    !! Getter for the "Cuttings" global variable.

    GetManagement_Cuttings_FirstDayNr = Cuttings%FirstDayNr
end function GetManagement_Cuttings_FirstDayNr

subroutine SetManagement_Cuttings_Considered(Considered)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: Considered

    Cuttings%Considered = Considered
end subroutine SetManagement_Cuttings_Considered

subroutine SetManagement_Cuttings_CCcut(CCcut)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: CCcut

    Cuttings%CCcut = CCcut
end subroutine SetManagement_Cuttings_CCcut

subroutine SetManagement_Cuttings_CGCPlus(CGCPlus)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: CGCPlus

    Cuttings%CGCPlus = CGCPlus
end subroutine SetManagement_Cuttings_CGCPlus

subroutine SetManagement_Cuttings_Day1(Day1)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: Day1

    Cuttings%Day1 = Day1
end subroutine SetManagement_Cuttings_Day1

subroutine SetManagement_Cuttings_NrDays(NrDays)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: NrDays

    Cuttings%NrDays = NrDays
end subroutine SetManagement_Cuttings_NrDays

subroutine SetManagement_Cuttings_Generate(Generate)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: Generate

    Cuttings%Generate = Generate
end subroutine SetManagement_Cuttings_Generate

subroutine SetManagement_Cuttings_Criterion(Criterion)
    !! Setter for the "Cuttings" global variable.
    integer(intEnum), intent(in) :: Criterion

    Cuttings%Criterion = Criterion
end subroutine SetManagement_Cuttings_Criterion

subroutine SetManagement_Cuttings_HarvestEnd(HarvestEnd)
    !! Setter for the "Cuttings" global variable.
    logical, intent(in) :: HarvestEnd

    Cuttings%HarvestEnd = HarvestEnd
end subroutine SetManagement_Cuttings_HarvestEnd

subroutine SetManagement_Cuttings_FirstDayNr(FirstDayNr)
    !! Setter for the "Cuttings" global variable.
    integer(int32), intent(in) :: FirstDayNr

    Cuttings%FirstDayNr = FirstDayNr
end subroutine SetManagement_Cuttings_FirstDayNr

function Geteffectiverain() result(effectiverain_out)
    !! Getter for the "effectiverain" global variable.
    type(rep_EffectiveRain) :: effectiverain_out

    effectiverain_out = effectiverain
end function Geteffectiverain

function Geteffectiverain_Method() result(Method)
    !! Getter for the "Method" attribute of the "effectiverain" global variable.
    integer(intEnum) :: Method

    Method = effectiverain%Method
end function Geteffectiverain_Method

function Geteffectiverain_PercentEffRain() result(PercentEffRain)
    !! Getter for the "PercentEffRain" attribute of the "effectiverain" global variable.
    integer(int8) :: PercentEffRain

    PercentEffRain = effectiverain%PercentEffRain
end function Geteffectiverain_PercentEffRain

function Geteffectiverain_ShowersInDecade() result(ShowersInDecade)
    !! Getter for the "ShowersInDecade" attribute of the "effectiverain" global variable.
    integer(int8) :: ShowersInDecade

    ShowersInDecade = effectiverain%ShowersInDecade
end function Geteffectiverain_ShowersInDecade

function Geteffectiverain_RootNrEvap() result(RootNrEvap)
    !! Getter for the "RootNrEvap" attribute of the "effectiverain" global variable.
    integer(int8) :: RootNrEvap

    RootNrEvap = effectiverain%RootNrEvap
end function Geteffectiverain_RootNrEvap

subroutine Seteffectiverain(effectiverain_in)
    !! Setter for the "effectiverain" global variable.
    type(rep_EffectiveRain), intent(in) :: effectiverain_in

    effectiverain = effectiverain_in
end subroutine Seteffectiverain

subroutine Seteffectiverain_Method(Method)
    !! Setter for the "Method" attribute of the "effectiverain" global variable.
    integer(intEnum), intent(in) :: Method

    effectiverain%Method = Method
end subroutine Seteffectiverain_Method

subroutine Seteffectiverain_PercentEffRain(PercentEffRain)
    !! Setter for the "PercentEffRain" attribute of the "effectiverain" global variable.
    integer(int8), intent(in) :: PercentEffRain

    effectiverain%PercentEffRain = PercentEffRain
end subroutine Seteffectiverain_PercentEffRain

subroutine Seteffectiverain_ShowersInDecade(ShowersInDecade)
    !! Setter for the "ShowersInDecade" attribute of the "effectiverain" global variable.
    integer(int8), intent(in) :: ShowersInDecade

    effectiverain%ShowersInDecade = ShowersInDecade
end subroutine Seteffectiverain_ShowersInDecade

subroutine Seteffectiverain_RootNrEvap(RootNrEvap)
    !! Setter for the "RootNrEvap" attribute of the "effectiverain" global variable.
    integer(int8), intent(in) :: RootNrEvap

    effectiverain%RootNrEvap = RootNrEvap
end subroutine Seteffectiverain_RootNrEvap

real(dp) function GetSumWaBal_Epot()
    !! Getter for the "SumWaBal" global variable.

     GetSumWaBal_Epot = SumWaBal%Epot
end function GetSumWaBal_Epot

real(dp) function GetSumWaBal_Tpot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Tpot = SumWaBal%Tpot
end function GetSumWaBal_Tpot

real(dp) function GetSumWaBal_Rain()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Rain = SumWaBal%Rain
end function GetSumWaBal_Rain

real(dp) function GetSumWaBal_Irrigation()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Irrigation = SumWaBal%Irrigation
end function GetSumWaBal_Irrigation

real(dp) function GetSumWaBal_Infiltrated()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Infiltrated = SumWaBal%Infiltrated
end function GetSumWaBal_Infiltrated

real(dp) function GetSumWaBal_Runoff()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Runoff = SumWaBal%Runoff
end function GetSumWaBal_Runoff

real(dp) function GetSumWaBal_Drain()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Drain = SumWaBal%Drain
end function GetSumWaBal_Drain

real(dp) function GetSumWaBal_Eact()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Eact = SumWaBal%Eact
end function GetSumWaBal_Eact

real(dp) function GetSumWaBal_Tact()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Tact = SumWaBal%Tact
end function GetSumWaBal_Tact

real(dp) function GetSumWaBal_TrW()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_TrW = SumWaBal%TrW
end function GetSumWaBal_TrW

real(dp) function GetSumWaBal_ECropCycle()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_ECropCycle = SumWaBal%ECropCycle
end function GetSumWaBal_ECropCycle

real(dp) function GetSumWaBal_CRwater()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_CRwater = SumWaBal%CRwater
end function GetSumWaBal_CRwater

real(dp) function GetSumWaBal_Biomass()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_Biomass = SumWaBal%Biomass
end function GetSumWaBal_Biomass

real(dp) function GetSumWaBal_YieldPart()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_YieldPart = SumWaBal%YieldPart
end function GetSumWaBal_YieldPart

real(dp) function GetSumWaBal_BiomassPot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassPot = SumWaBal%BiomassPot
end function GetSumWaBal_BiomassPot

real(dp) function GetSumWaBal_BiomassUnlim()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassUnlim = SumWaBal%BiomassUnlim
end function GetSumWaBal_BiomassUnlim

real(dp) function GetSumWaBal_BiomassTot()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_BiomassTot = SumWaBal%BiomassTot
end function GetSumWaBal_BiomassTot

real(dp) function GetSumWaBal_SaltIn()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_SaltIn = SumWaBal%SaltIn
end function GetSumWaBal_SaltIn

real(dp) function GetSumWaBal_SaltOut()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_SaltOut = SumWaBal%SaltOut
end function GetSumWaBal_SaltOut

real(dp) function GetSumWaBal_CRSalt()
    !! Getter for the "SumWaBal" global variable.

    GetSumWaBal_CRSalt = SumWaBal%CRSalt
end function GetSumWaBal_CRSalt

subroutine SetSumWaBal_Epot(Epot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Epot

    SumWaBal%Epot = Epot
end subroutine SetSumWaBal_Epot

subroutine SetSumWaBal_Tpot(Tpot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Tpot

    SumWaBal%Tpot = Tpot
end subroutine SetSumWaBal_Tpot

subroutine SetSumWaBal_Rain(Rain)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Rain

    SumWaBal%Rain = Rain
end subroutine SetSumWaBal_Rain

subroutine SetSumWaBal_Irrigation(Irrigation)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Irrigation

    SumWaBal%Irrigation = Irrigation
end subroutine SetSumWaBal_Irrigation

subroutine SetSumWaBal_Infiltrated(Infiltrated)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Infiltrated

    SumWaBal%Infiltrated = Infiltrated
end subroutine SetSumWaBal_Infiltrated

subroutine SetSumWaBal_Runoff(Runoff)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Runoff

    SumWaBal%Runoff = Runoff
end subroutine SetSumWaBal_Runoff

subroutine SetSumWaBal_Drain(Drain)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Drain

    SumWaBal%Drain = Drain
end subroutine SetSumWaBal_Drain

subroutine SetSumWaBal_Eact(Eact)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Eact

    SumWaBal%Eact = Eact
end subroutine SetSumWaBal_Eact

subroutine SetSumWaBal_Tact(Tact)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Tact

    SumWaBal%Tact = Tact
end subroutine SetSumWaBal_Tact

subroutine SetSumWaBal_TrW(TrW)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: TrW

    SumWaBal%TrW = TrW
end subroutine SetSumWaBal_TrW

subroutine SetSumWaBal_ECropCycle(ECropCycle)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: ECropCycle

    SumWaBal%ECropCycle = ECropCycle
end subroutine SetSumWaBal_ECropCycle

subroutine SetSumWaBal_CRwater(CRwater)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: CRwater

    SumWaBal%CRwater = CRwater
end subroutine SetSumWaBal_CRwater

subroutine SetSumWaBal_Biomass(Biomass)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: Biomass

    SumWaBal%Biomass = Biomass
end subroutine SetSumWaBal_Biomass

subroutine SetSumWaBal_YieldPart(YieldPart)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: YieldPart

    SumWaBal%YieldPart = YieldPart
end subroutine SetSumWaBal_YieldPart

subroutine SetSumWaBal_BiomassPot(BiomassPot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassPot

    SumWaBal%BiomassPot = BiomassPot
end subroutine SetSumWaBal_BiomassPot

subroutine SetSumWaBal_BiomassUnlim(BiomassUnlim)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassUnlim

    SumWaBal%BiomassUnlim = BiomassUnlim
end subroutine SetSumWaBal_BiomassUnlim

subroutine SetSumWaBal_BiomassTot(BiomassTot)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: BiomassTot

    SumWaBal%BiomassTot = BiomassTot
end subroutine SetSumWaBal_BiomassTot

subroutine SetSumWaBal_SaltIn(SaltIn)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: SaltIn

    SumWaBal%SaltIn = SaltIn
end subroutine SetSumWaBal_SaltIn

subroutine SetSumWaBal_SaltOut(SaltOut)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: SaltOut

    SumWaBal%SaltOut = SaltOut
end subroutine SetSumWaBal_SaltOut

subroutine SetSumWaBal_CRSalt(CRSalt)
    !! Setter for the "SumWaBal" global variable.
    real(dp), intent(in) :: CRSalt

    SumWaBal%CRSalt = CRSalt
end subroutine SetSumWaBal_CRSalt

type(rep_soil) function GetSoil()
    !! Getter for the "Soil" global variable.

    GetSoil = Soil
end function GetSoil

subroutine SetSoil_REW(REW)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: REW

    Soil%REW = REW
end subroutine SetSoil_REW

subroutine SetSoil_NrSoilLayers(NrSoilLayers)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: NrSoilLayers

    Soil%NrSoilLayers = NrSoilLayers
end subroutine SetSoil_NrSoilLayers

subroutine SetSoil_CNvalue(CNvalue)
    !! Setter for the "Soil" global variable.
    integer(int8), intent(in) :: CNvalue

    Soil%CNvalue = CNvalue
end subroutine SetSoil_CNvalue

subroutine SetSoil_RootMax(RootMax)
    !! Setter for the "Soil" global variable.
    real(sp), intent(in) :: RootMax

    Soil%RootMax = RootMax
end subroutine SetSoil_RootMax

function GetCrop_StressResponse() result(StressResponse)
    !! Getter for the "StressResponse" attribute of the "crop" global variable.
    type(rep_Shapes) :: StressResponse

    StressResponse = crop%StressResponse
end function GetCrop_StressResponse

function GetCrop_StressResponse_Stress() result(Stress)
    !! Getter for the "Stress" attribute of the "StressResponse" attribute of the "crop" global variable.
    integer(int8) :: Stress

    Stress = crop%StressResponse%Stress
end function GetCrop_StressResponse_Stress

function GetCrop_StressResponse_ShapeCGC() result(ShapeCGC)
    !! Getter for the "ShapeCGC" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp) :: ShapeCGC

    ShapeCGC = crop%StressResponse%ShapeCGC
end function GetCrop_StressResponse_ShapeCGC

function GetCrop_StressResponse_ShapeCCX() result(ShapeCCX)
    !! Getter for the "ShapeCCX" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp) :: ShapeCCX

    ShapeCCX = crop%StressResponse%ShapeCCX
end function GetCrop_StressResponse_ShapeCCX

function GetCrop_StressResponse_ShapeWP() result(ShapeWP)
    !! Getter for the "ShapeWP" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp) :: ShapeWP

    ShapeWP = crop%StressResponse%ShapeWP
end function GetCrop_StressResponse_ShapeWP

function GetCrop_StressResponse_ShapeCDecline() result(ShapeCDecline)
    !! Getter for the "ShapeCDecline" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp) :: ShapeCDecline

    ShapeCDecline = crop%StressResponse%ShapeCDecline
end function GetCrop_StressResponse_ShapeCDecline

function GetCrop_StressResponse_Calibrated() result(Calibrated)
    !! Getter for the "Calibrated" attribute of the "StressResponse" attribute of the "crop" global variable.
    logical :: Calibrated

    Calibrated = crop%StressResponse%Calibrated
end function GetCrop_StressResponse_Calibrated

subroutine SetCrop_StressResponse(StressResponse)
    !! Setter for the "StressResponse" attribute of the "crop" global variable.
    type(rep_Shapes), intent(in) :: StressResponse

    crop%StressResponse = StressResponse
end subroutine SetCrop_StressResponse

subroutine SetCrop_StressResponse_Stress(Stress)
    !! Setter for the "Stress" attribute of the "StressResponse" attribute of the "crop" global variable.
    integer(int8), intent(in) :: Stress

    crop%StressResponse%Stress = Stress
end subroutine SetCrop_StressResponse_Stress

subroutine SetCrop_StressResponse_ShapeCGC(ShapeCGC)
    !! Setter for the "ShapeCGC" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp), intent(in) :: ShapeCGC

    crop%StressResponse%ShapeCGC = ShapeCGC
end subroutine SetCrop_StressResponse_ShapeCGC

subroutine SetCrop_StressResponse_ShapeCCX(ShapeCCX)
    !! Setter for the "ShapeCCX" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp), intent(in) :: ShapeCCX

    crop%StressResponse%ShapeCCX = ShapeCCX
end subroutine SetCrop_StressResponse_ShapeCCX

subroutine SetCrop_StressResponse_ShapeWP(ShapeWP)
    !! Setter for the "ShapeWP" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp), intent(in) :: ShapeWP

    crop%StressResponse%ShapeWP = ShapeWP
end subroutine SetCrop_StressResponse_ShapeWP

subroutine SetCrop_StressResponse_ShapeCDecline(ShapeCDecline)
    !! Setter for the "ShapeCDecline" attribute of the "StressResponse" attribute of the "crop" global variable.
    real(dp), intent(in) :: ShapeCDecline

    crop%StressResponse%ShapeCDecline = ShapeCDecline
end subroutine SetCrop_StressResponse_ShapeCDecline

subroutine SetCrop_StressResponse_Calibrated(Calibrated)
    !! Setter for the "Calibrated" attribute of the "StressResponse" attribute of the "crop" global variable.
    logical, intent(in) :: Calibrated

    crop%StressResponse%Calibrated = Calibrated
end subroutine SetCrop_StressResponse_Calibrated

function GetCrop() result(Crop_out)
    !! Getter for the "crop" global variable.
    type(rep_Crop) :: Crop_out

    Crop_out = crop
end function GetCrop

function GetCrop_subkind() result(subkind)
    !! Getter for the "subkind" attribute of the "crop" global variable.
    integer(intEnum) :: subkind

    subkind = crop%subkind
end function GetCrop_subkind

function GetCrop_ModeCycle() result(ModeCycle)
    !! Getter for the "ModeCycle" attribute of the "crop" global variable.
    integer(intEnum) :: ModeCycle

    ModeCycle = crop%ModeCycle
end function GetCrop_ModeCycle

function GetCrop_Planting() result(Planting)
    !! Getter for the "Planting" attribute of the "crop" global variable.
    integer(intEnum) :: Planting

    Planting = crop%Planting
end function GetCrop_Planting

function GetCrop_pMethod() result(pMethod)
    !! Getter for the "pMethod" attribute of the "crop" global variable.
    integer(intEnum) :: pMethod

    pMethod = crop%pMethod
end function GetCrop_pMethod

function GetCrop_pdef() result(pdef)
    !! Getter for the "pdef" attribute of the "crop" global variable.
    real(dp) :: pdef

    pdef = crop%pdef
end function GetCrop_pdef

function GetCrop_pActStom() result(pActStom)
    !! Getter for the "pActStom" attribute of the "crop" global variable.
    real(dp) :: pActStom

    pActStom = crop%pActStom
end function GetCrop_pActStom

function GetCrop_KsShapeFactorLeaf() result(KsShapeFactorLeaf)
    !! Getter for the "KsShapeFactorLeaf" attribute of the "crop" global variable.
    real(dp) :: KsShapeFactorLeaf

    KsShapeFactorLeaf = crop%KsShapeFactorLeaf
end function GetCrop_KsShapeFactorLeaf

function GetCrop_KsShapeFactorStomata() result(KsShapeFactorStomata)
    !! Getter for the "KsShapeFactorStomata" attribute of the "crop" global variable.
    real(dp) :: KsShapeFactorStomata

    KsShapeFactorStomata = crop%KsShapeFactorStomata
end function GetCrop_KsShapeFactorStomata

function GetCrop_KsShapeFactorSenescence() result(KsShapeFactorSenescence)
    !! Getter for the "KsShapeFactorSenescence" attribute of the "crop" global variable.
    real(dp) :: KsShapeFactorSenescence

    KsShapeFactorSenescence = crop%KsShapeFactorSenescence
end function GetCrop_KsShapeFactorSenescence

function GetCrop_pLeafDefUL() result(pLeafDefUL)
    !! Getter for the "pLeafDefUL" attribute of the "crop" global variable.
    real(dp) :: pLeafDefUL

    pLeafDefUL = crop%pLeafDefUL
end function GetCrop_pLeafDefUL

function GetCrop_pLeafDefLL() result(pLeafDefLL)
    !! Getter for the "pLeafDefLL" attribute of the "crop" global variable.
    real(dp) :: pLeafDefLL

    pLeafDefLL = crop%pLeafDefLL
end function GetCrop_pLeafDefLL

function GetCrop_pLeafAct() result(pLeafAct)
    !! Getter for the "pLeafAct" attribute of the "crop" global variable.
    real(dp) :: pLeafAct

    pLeafAct = crop%pLeafAct
end function GetCrop_pLeafAct

function GetCrop_pSenescence() result(pSenescence)
    !! Getter for the "pSenescence" attribute of the "crop" global variable.
    real(dp) :: pSenescence

    pSenescence = crop%pSenescence
end function GetCrop_pSenescence

function GetCrop_pSenAct() result(pSenAct)
    !! Getter for the "pSenAct" attribute of the "crop" global variable.
    real(dp) :: pSenAct

    pSenAct = crop%pSenAct
end function GetCrop_pSenAct

function GetCrop_pPollination() result(pPollination)
    !! Getter for the "pPollination" attribute of the "crop" global variable.
    real(dp) :: pPollination

    pPollination = crop%pPollination
end function GetCrop_pPollination

function GetCrop_SumEToDelaySenescence() result(SumEToDelaySenescence)
    !! Getter for the "SumEToDelaySenescence" attribute of the "crop" global variable.
    integer(int32) :: SumEToDelaySenescence

    SumEToDelaySenescence = crop%SumEToDelaySenescence
end function GetCrop_SumEToDelaySenescence

function GetCrop_AnaeroPoint() result(AnaeroPoint)
    !! Getter for the "AnaeroPoint" attribute of the "crop" global variable.
    integer(int32) :: AnaeroPoint

    AnaeroPoint = crop%AnaeroPoint
end function GetCrop_AnaeroPoint

function GetCrop_ECemin() result(ECemin)
    !! Getter for the "ECemin" attribute of the "crop" global variable.
    integer(int8) :: ECemin

    ECemin = crop%ECemin
end function GetCrop_ECemin

function GetCrop_ECemax() result(ECemax)
    !! Getter for the "ECemax" attribute of the "crop" global variable.
    integer(int8) :: ECemax

    ECemax = crop%ECemax
end function GetCrop_ECemax

function GetCrop_CCsaltDistortion() result(CCsaltDistortion)
    !! Getter for the "CCsaltDistortion" attribute of the "crop" global variable.
    integer(int8) :: CCsaltDistortion

    CCsaltDistortion = crop%CCsaltDistortion
end function GetCrop_CCsaltDistortion

function GetCrop_ResponseECsw() result(ResponseECsw)
    !! Getter for the "ResponseECsw" attribute of the "crop" global variable.
    integer(int32) :: ResponseECsw

    ResponseECsw = crop%ResponseECsw
end function GetCrop_ResponseECsw

function GetCrop_SmaxTopQuarter() result(SmaxTopQuarter)
    !! Getter for the "SmaxTopQuarter" attribute of the "crop" global variable.
    real(dp) :: SmaxTopQuarter

    SmaxTopQuarter = crop%SmaxTopQuarter
end function GetCrop_SmaxTopQuarter

function GetCrop_SmaxBotQuarter() result(SmaxBotQuarter)
    !! Getter for the "SmaxBotQuarter" attribute of the "crop" global variable.
    real(dp) :: SmaxBotQuarter

    SmaxBotQuarter = crop%SmaxBotQuarter
end function GetCrop_SmaxBotQuarter

function GetCrop_SmaxTop() result(SmaxTop)
    !! Getter for the "SmaxTop" attribute of the "crop" global variable.
    real(dp) :: SmaxTop

    SmaxTop = crop%SmaxTop
end function GetCrop_SmaxTop

function GetCrop_SmaxBot() result(SmaxBot)
    !! Getter for the "SmaxBot" attribute of the "crop" global variable.
    real(dp) :: SmaxBot

    SmaxBot = crop%SmaxBot
end function GetCrop_SmaxBot

function GetCrop_KcTop() result(KcTop)
    !! Getter for the "KcTop" attribute of the "crop" global variable.
    real(dp) :: KcTop

    KcTop = crop%KcTop
end function GetCrop_KcTop

function GetCrop_KcDecline() result(KcDecline)
    !! Getter for the "KcDecline" attribute of the "crop" global variable.
    real(dp) :: KcDecline

    KcDecline = crop%KcDecline
end function GetCrop_KcDecline

function GetCrop_CCEffectEvapLate() result(CCEffectEvapLate)
    !! Getter for the "CCEffectEvapLate" attribute of the "crop" global variable.
    integer(int32) :: CCEffectEvapLate

    CCEffectEvapLate = crop%CCEffectEvapLate
end function GetCrop_CCEffectEvapLate

function GetCrop_Day1() result(Day1)
    !! Getter for the "Day1" attribute of the "crop" global variable.
    integer(int32) :: Day1

    Day1 = crop%Day1
end function GetCrop_Day1

function GetCrop_DayN() result(DayN)
    !! Getter for the "DayN" attribute of the "crop" global variable.
    integer(int32) :: DayN

    DayN = crop%DayN
end function GetCrop_DayN

function GetCrop_RootMin() result(RootMin)
    !! Getter for the "RootMin" attribute of the "crop" global variable.
    real(dp) :: RootMin

    RootMin = crop%RootMin
end function GetCrop_RootMin

function GetCrop_RootMax() result(RootMax)
    !! Getter for the "RootMax" attribute of the "crop" global variable.
    real(dp) :: RootMax

    RootMax = crop%RootMax
end function GetCrop_RootMax

function GetCrop_RootShape() result(RootShape)
    !! Getter for the "RootShape" attribute of the "crop" global variable.
    integer(int8) :: RootShape

    RootShape = crop%RootShape
end function GetCrop_RootShape

function GetCrop_Tbase() result(Tbase)
    !! Getter for the "Tbase" attribute of the "crop" global variable.
    real(dp) :: Tbase

    Tbase = crop%Tbase
end function GetCrop_Tbase

function GetCrop_Tupper() result(Tupper)
    !! Getter for the "Tupper" attribute of the "crop" global variable.
    real(dp) :: Tupper

    Tupper = crop%Tupper
end function GetCrop_Tupper

function GetCrop_Tcold() result(Tcold)
    !! Getter for the "Tcold" attribute of the "crop" global variable.
    integer(int8) :: Tcold

    Tcold = crop%Tcold
end function GetCrop_Tcold

function GetCrop_Theat() result(Theat)
    !! Getter for the "Theat" attribute of the "crop" global variable.
    integer(int8) :: Theat

    Theat = crop%Theat
end function GetCrop_Theat

function GetCrop_GDtranspLow() result(GDtranspLow)
    !! Getter for the "GDtranspLow" attribute of the "crop" global variable.
    real(dp) :: GDtranspLow

    GDtranspLow = crop%GDtranspLow
end function GetCrop_GDtranspLow

function GetCrop_SizeSeedling() result(SizeSeedling)
    !! Getter for the "SizeSeedling" attribute of the "crop" global variable.
    real(dp) :: SizeSeedling

    SizeSeedling = crop%SizeSeedling
end function GetCrop_SizeSeedling

function GetCrop_SizePlant() result(SizePlant)
    !! Getter for the "SizePlant" attribute of the "crop" global variable.
    real(dp) :: SizePlant

    SizePlant = crop%SizePlant
end function GetCrop_SizePlant

function GetCrop_PlantingDens() result(PlantingDens)
    !! Getter for the "PlantingDens" attribute of the "crop" global variable.
    integer(int32) :: PlantingDens

    PlantingDens = crop%PlantingDens
end function GetCrop_PlantingDens

function GetCrop_CCo() result(CCo)
    !! Getter for the "CCo" attribute of the "crop" global variable.
    real(dp) :: CCo

    CCo = crop%CCo
end function GetCrop_CCo

function GetCrop_CCini() result(CCini)
    !! Getter for the "CCini" attribute of the "crop" global variable.
    real(dp) :: CCini

    CCini = crop%CCini
end function GetCrop_CCini

function GetCrop_CGC() result(CGC)
    !! Getter for the "CGC" attribute of the "crop" global variable.
    real(dp) :: CGC

    CGC = crop%CGC
end function GetCrop_CGC

function GetCrop_GDDCGC() result(GDDCGC)
    !! Getter for the "GDDCGC" attribute of the "crop" global variable.
    real(dp) :: GDDCGC

    GDDCGC = crop%GDDCGC
end function GetCrop_GDDCGC

function GetCrop_CCx() result(CCx)
    !! Getter for the "CCx" attribute of the "crop" global variable.
    real(dp) :: CCx

    CCx = crop%CCx
end function GetCrop_CCx

function GetCrop_CDC() result(CDC)
    !! Getter for the "CDC" attribute of the "crop" global variable.
    real(dp) :: CDC

    CDC = crop%CDC
end function GetCrop_CDC

function GetCrop_GDDCDC() result(GDDCDC)
    !! Getter for the "GDDCDC" attribute of the "crop" global variable.
    real(dp) :: GDDCDC

    GDDCDC = crop%GDDCDC
end function GetCrop_GDDCDC

function GetCrop_CCxAdjusted() result(CCxAdjusted)
    !! Getter for the "CCxAdjusted" attribute of the "crop" global variable.
    real(dp) :: CCxAdjusted

    CCxAdjusted = crop%CCxAdjusted
end function GetCrop_CCxAdjusted

function GetCrop_CCxWithered() result(CCxWithered)
    !! Getter for the "CCxWithered" attribute of the "crop" global variable.
    real(dp) :: CCxWithered

    CCxWithered = crop%CCxWithered
end function GetCrop_CCxWithered

function GetCrop_CCoAdjusted() result(CCoAdjusted)
    !! Getter for the "CCoAdjusted" attribute of the "crop" global variable.
    real(dp) :: CCoAdjusted

    CCoAdjusted = crop%CCoAdjusted
end function GetCrop_CCoAdjusted

function GetCrop_DaysToCCini() result(DaysToCCini)
    !! Getter for the "DaysToCCini" attribute of the "crop" global variable.
    integer(int32) :: DaysToCCini

    DaysToCCini = crop%DaysToCCini
end function GetCrop_DaysToCCini

function GetCrop_DaysToGermination() result(DaysToGermination)
    !! Getter for the "DaysToGermination" attribute of the "crop" global variable.
    integer(int32) :: DaysToGermination

    DaysToGermination = crop%DaysToGermination
end function GetCrop_DaysToGermination

function GetCrop_DaysToFullCanopy() result(DaysToFullCanopy)
    !! Getter for the "DaysToFullCanopy" attribute of the "crop" global variable.
    integer(int32) :: DaysToFullCanopy

    DaysToFullCanopy = crop%DaysToFullCanopy
end function GetCrop_DaysToFullCanopy

function GetCrop_DaysToFullCanopySF() result(DaysToFullCanopySF)
    !! Getter for the "DaysToFullCanopySF" attribute of the "crop" global variable.
    integer(int32) :: DaysToFullCanopySF

    DaysToFullCanopySF = crop%DaysToFullCanopySF
end function GetCrop_DaysToFullCanopySF

function GetCrop_DaysToFlowering() result(DaysToFlowering)
    !! Getter for the "DaysToFlowering" attribute of the "crop" global variable.
    integer(int32) :: DaysToFlowering

    DaysToFlowering = crop%DaysToFlowering
end function GetCrop_DaysToFlowering

function GetCrop_LengthFlowering() result(LengthFlowering)
    !! Getter for the "LengthFlowering" attribute of the "crop" global variable.
    integer(int32) :: LengthFlowering

    LengthFlowering = crop%LengthFlowering
end function GetCrop_LengthFlowering

function GetCrop_DaysToSenescence() result(DaysToSenescence)
    !! Getter for the "DaysToSenescence" attribute of the "crop" global variable.
    integer(int32) :: DaysToSenescence

    DaysToSenescence = crop%DaysToSenescence
end function GetCrop_DaysToSenescence

function GetCrop_DaysToHarvest() result(DaysToHarvest)
    !! Getter for the "DaysToHarvest" attribute of the "crop" global variable.
    integer(int32) :: DaysToHarvest

    DaysToHarvest = crop%DaysToHarvest
end function GetCrop_DaysToHarvest

function GetCrop_DaysToMaxRooting() result(DaysToMaxRooting)
    !! Getter for the "DaysToMaxRooting" attribute of the "crop" global variable.
    integer(int32) :: DaysToMaxRooting

    DaysToMaxRooting = crop%DaysToMaxRooting
end function GetCrop_DaysToMaxRooting

function GetCrop_DaysToHIo() result(DaysToHIo)
    !! Getter for the "DaysToHIo" attribute of the "crop" global variable.
    integer(int32) :: DaysToHIo

    DaysToHIo = crop%DaysToHIo
end function GetCrop_DaysToHIo

function GetCrop_GDDaysToCCini() result(GDDaysToCCini)
    !! Getter for the "GDDaysToCCini" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToCCini

    GDDaysToCCini = crop%GDDaysToCCini
end function GetCrop_GDDaysToCCini

function GetCrop_GDDaysToGermination() result(GDDaysToGermination)
    !! Getter for the "GDDaysToGermination" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToGermination

    GDDaysToGermination = crop%GDDaysToGermination
end function GetCrop_GDDaysToGermination

function GetCrop_GDDaysToFullCanopy() result(GDDaysToFullCanopy)
    !! Getter for the "GDDaysToFullCanopy" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToFullCanopy

    GDDaysToFullCanopy = crop%GDDaysToFullCanopy
end function GetCrop_GDDaysToFullCanopy

function GetCrop_GDDaysToFullCanopySF() result(GDDaysToFullCanopySF)
    !! Getter for the "GDDaysToFullCanopySF" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToFullCanopySF

    GDDaysToFullCanopySF = crop%GDDaysToFullCanopySF
end function GetCrop_GDDaysToFullCanopySF

function GetCrop_GDDaysToFlowering() result(GDDaysToFlowering)
    !! Getter for the "GDDaysToFlowering" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToFlowering

    GDDaysToFlowering = crop%GDDaysToFlowering
end function GetCrop_GDDaysToFlowering

function GetCrop_GDDLengthFlowering() result(GDDLengthFlowering)
    !! Getter for the "GDDLengthFlowering" attribute of the "crop" global variable.
    integer(int32) :: GDDLengthFlowering

    GDDLengthFlowering = crop%GDDLengthFlowering
end function GetCrop_GDDLengthFlowering

function GetCrop_GDDaysToSenescence() result(GDDaysToSenescence)
    !! Getter for the "GDDaysToSenescence" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToSenescence

    GDDaysToSenescence = crop%GDDaysToSenescence
end function GetCrop_GDDaysToSenescence

function GetCrop_GDDaysToHarvest() result(GDDaysToHarvest)
    !! Getter for the "GDDaysToHarvest" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToHarvest

    GDDaysToHarvest = crop%GDDaysToHarvest
end function GetCrop_GDDaysToHarvest

function GetCrop_GDDaysToMaxRooting() result(GDDaysToMaxRooting)
    !! Getter for the "GDDaysToMaxRooting" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToMaxRooting

    GDDaysToMaxRooting = crop%GDDaysToMaxRooting
end function GetCrop_GDDaysToMaxRooting

function GetCrop_GDDaysToHIo() result(GDDaysToHIo)
    !! Getter for the "GDDaysToHIo" attribute of the "crop" global variable.
    integer(int32) :: GDDaysToHIo

    GDDaysToHIo = crop%GDDaysToHIo
end function GetCrop_GDDaysToHIo

function GetCrop_WP() result(WP)
    !! Getter for the "WP" attribute of the "crop" global variable.
    real(dp) :: WP

    WP = crop%WP
end function GetCrop_WP

function GetCrop_WPy() result(WPy)
    !! Getter for the "WPy" attribute of the "crop" global variable.
    integer(int32) :: WPy

    WPy = crop%WPy
end function GetCrop_WPy

function GetCrop_AdaptedToCO2() result(AdaptedToCO2)
    !! Getter for the "AdaptedToCO2" attribute of the "crop" global variable.
    integer(int8) :: AdaptedToCO2

    AdaptedToCO2 = crop%AdaptedToCO2
end function GetCrop_AdaptedToCO2

function GetCrop_HI() result(HI)
    !! Getter for the "HI" attribute of the "crop" global variable.
    integer(int32) :: HI

    HI = crop%HI
end function GetCrop_HI

function GetCrop_dHIdt() result(dHIdt)
    !! Getter for the "dHIdt" attribute of the "crop" global variable.
    real(dp) :: dHIdt

    dHIdt = crop%dHIdt
end function GetCrop_dHIdt

function GetCrop_HIincrease() result(HIincrease)
    !! Getter for the "HIincrease" attribute of the "crop" global variable.
    integer(int8) :: HIincrease

    HIincrease = crop%HIincrease
end function GetCrop_HIincrease

function GetCrop_aCoeff() result(aCoeff)
    !! Getter for the "aCoeff" attribute of the "crop" global variable.
    real(dp) :: aCoeff

    aCoeff = crop%aCoeff
end function GetCrop_aCoeff

function GetCrop_bCoeff() result(bCoeff)
    !! Getter for the "bCoeff" attribute of the "crop" global variable.
    real(dp) :: bCoeff

    bCoeff = crop%bCoeff
end function GetCrop_bCoeff

function GetCrop_DHImax() result(DHImax)
    !! Getter for the "DHImax" attribute of the "crop" global variable.
    integer(int8) :: DHImax

    DHImax = crop%DHImax
end function GetCrop_DHImax

function GetCrop_DeterminancyLinked() result(DeterminancyLinked)
    !! Getter for the "DeterminancyLinked" attribute of the "crop" global variable.
    logical :: DeterminancyLinked

    DeterminancyLinked = crop%DeterminancyLinked
end function GetCrop_DeterminancyLinked

function GetCrop_fExcess() result(fExcess)
    !! Getter for the "fExcess" attribute of the "crop" global variable.
    integer(int16) :: fExcess

    fExcess = crop%fExcess
end function GetCrop_fExcess

function GetCrop_DryMatter() result(DryMatter)
    !! Getter for the "DryMatter" attribute of the "crop" global variable.
    integer(int8) :: DryMatter

    DryMatter = crop%DryMatter
end function GetCrop_DryMatter

function GetCrop_RootMinYear1() result(RootMinYear1)
    !! Getter for the "RootMinYear1" attribute of the "crop" global variable.
    real(dp) :: RootMinYear1

    RootMinYear1 = crop%RootMinYear1
end function GetCrop_RootMinYear1

function GetCrop_SownYear1() result(SownYear1)
    !! Getter for the "SownYear1" attribute of the "crop" global variable.
    logical :: SownYear1

    SownYear1 = crop%SownYear1
end function GetCrop_SownYear1

function GetCrop_YearCCx() result(YearCCx)
    !! Getter for the "YearCCx" attribute of the "crop" global variable.
    integer(int8) :: YearCCx

    YearCCx = crop%YearCCx
end function GetCrop_YearCCx

function GetCrop_CCxRoot() result(CCxRoot)
    !! Getter for the "CCxRoot" attribute of the "crop" global variable.
    real(dp) :: CCxRoot

    CCxRoot = crop%CCxRoot
end function GetCrop_CCxRoot

subroutine SetCrop(Crop_in)
    !! Setter for the "crop" global variable.
    type(rep_Crop), intent(in) :: Crop_in

    crop = Crop_in
end subroutine SetCrop

subroutine SetCrop_subkind(subkind)
    !! Setter for the "subkind" attribute of the "crop" global variable.
    integer(intEnum), intent(in) :: subkind

    crop%subkind = subkind
end subroutine SetCrop_subkind

subroutine SetCrop_ModeCycle(ModeCycle)
    !! Setter for the "ModeCycle" attribute of the "crop" global variable.
    integer(intEnum), intent(in) :: ModeCycle

    crop%ModeCycle = ModeCycle
end subroutine SetCrop_ModeCycle

subroutine SetCrop_Planting(Planting)
    !! Setter for the "Planting" attribute of the "crop" global variable.
    integer(intEnum), intent(in) :: Planting

    crop%Planting = Planting
end subroutine SetCrop_Planting

subroutine SetCrop_pMethod(pMethod)
    !! Setter for the "pMethod" attribute of the "crop" global variable.
    integer(intEnum), intent(in) :: pMethod

    crop%pMethod = pMethod
end subroutine SetCrop_pMethod

subroutine SetCrop_pdef(pdef)
    !! Setter for the "pdef" attribute of the "crop" global variable.
    real(dp), intent(in) :: pdef

    crop%pdef = pdef
end subroutine SetCrop_pdef

subroutine SetCrop_pActStom(pActStom)
    !! Setter for the "pActStom" attribute of the "crop" global variable.
    real(dp), intent(in) :: pActStom

    crop%pActStom = pActStom
end subroutine SetCrop_pActStom

subroutine SetCrop_KsShapeFactorLeaf(KsShapeFactorLeaf)
    !! Setter for the "KsShapeFactorLeaf" attribute of the "crop" global variable.
    real(dp), intent(in) :: KsShapeFactorLeaf

    crop%KsShapeFactorLeaf = KsShapeFactorLeaf
end subroutine SetCrop_KsShapeFactorLeaf

subroutine SetCrop_KsShapeFactorStomata(KsShapeFactorStomata)
    !! Setter for the "KsShapeFactorStomata" attribute of the "crop" global variable.
    real(dp), intent(in) :: KsShapeFactorStomata

    crop%KsShapeFactorStomata = KsShapeFactorStomata
end subroutine SetCrop_KsShapeFactorStomata

subroutine SetCrop_KsShapeFactorSenescence(KsShapeFactorSenescence)
    !! Setter for the "KsShapeFactorSenescence" attribute of the "crop" global variable.
    real(dp), intent(in) :: KsShapeFactorSenescence

    crop%KsShapeFactorSenescence = KsShapeFactorSenescence
end subroutine SetCrop_KsShapeFactorSenescence

subroutine SetCrop_pLeafDefUL(pLeafDefUL)
    !! Setter for the "pLeafDefUL" attribute of the "crop" global variable.
    real(dp), intent(in) :: pLeafDefUL

    crop%pLeafDefUL = pLeafDefUL
end subroutine SetCrop_pLeafDefUL

subroutine SetCrop_pLeafDefLL(pLeafDefLL)
    !! Setter for the "pLeafDefLL" attribute of the "crop" global variable.
    real(dp), intent(in) :: pLeafDefLL

    crop%pLeafDefLL = pLeafDefLL
end subroutine SetCrop_pLeafDefLL

subroutine SetCrop_pLeafAct(pLeafAct)
    !! Setter for the "pLeafAct" attribute of the "crop" global variable.
    real(dp), intent(in) :: pLeafAct

    crop%pLeafAct = pLeafAct
end subroutine SetCrop_pLeafAct

subroutine SetCrop_pSenescence(pSenescence)
    !! Setter for the "pSenescence" attribute of the "crop" global variable.
    real(dp), intent(in) :: pSenescence

    crop%pSenescence = pSenescence
end subroutine SetCrop_pSenescence

subroutine SetCrop_pSenAct(pSenAct)
    !! Setter for the "pSenAct" attribute of the "crop" global variable.
    real(dp), intent(in) :: pSenAct

    crop%pSenAct = pSenAct
end subroutine SetCrop_pSenAct

subroutine SetCrop_pPollination(pPollination)
    !! Setter for the "pPollination" attribute of the "crop" global variable.
    real(dp), intent(in) :: pPollination

    crop%pPollination = pPollination
end subroutine SetCrop_pPollination

subroutine SetCrop_SumEToDelaySenescence(SumEToDelaySenescence)
    !! Setter for the "SumEToDelaySenescence" attribute of the "crop" global variable.
    integer(int32), intent(in) :: SumEToDelaySenescence

    crop%SumEToDelaySenescence = SumEToDelaySenescence
end subroutine SetCrop_SumEToDelaySenescence

subroutine SetCrop_AnaeroPoint(AnaeroPoint)
    !! Setter for the "AnaeroPoint" attribute of the "crop" global variable.
    integer(int32), intent(in) :: AnaeroPoint

    crop%AnaeroPoint = AnaeroPoint
end subroutine SetCrop_AnaeroPoint

subroutine SetCrop_ECemin(ECemin)
    !! Setter for the "ECemin" attribute of the "crop" global variable.
    integer(int8), intent(in) :: ECemin

    crop%ECemin = ECemin
end subroutine SetCrop_ECemin

subroutine SetCrop_ECemax(ECemax)
    !! Setter for the "ECemax" attribute of the "crop" global variable.
    integer(int8), intent(in) :: ECemax

    crop%ECemax = ECemax
end subroutine SetCrop_ECemax

subroutine SetCrop_CCsaltDistortion(CCsaltDistortion)
    !! Setter for the "CCsaltDistortion" attribute of the "crop" global variable.
    integer(int8), intent(in) :: CCsaltDistortion

    crop%CCsaltDistortion = CCsaltDistortion
end subroutine SetCrop_CCsaltDistortion

subroutine SetCrop_ResponseECsw(ResponseECsw)
    !! Setter for the "ResponseECsw" attribute of the "crop" global variable.
    integer(int32), intent(in) :: ResponseECsw

    crop%ResponseECsw = ResponseECsw
end subroutine SetCrop_ResponseECsw

subroutine SetCrop_SmaxTopQuarter(SmaxTopQuarter)
    !! Setter for the "SmaxTopQuarter" attribute of the "crop" global variable.
    real(dp), intent(in) :: SmaxTopQuarter

    crop%SmaxTopQuarter = SmaxTopQuarter
end subroutine SetCrop_SmaxTopQuarter

subroutine SetCrop_SmaxBotQuarter(SmaxBotQuarter)
    !! Setter for the "SmaxBotQuarter" attribute of the "crop" global variable.
    real(dp), intent(in) :: SmaxBotQuarter

    crop%SmaxBotQuarter = SmaxBotQuarter
end subroutine SetCrop_SmaxBotQuarter

subroutine SetCrop_SmaxTop(SmaxTop)
    !! Setter for the "SmaxTop" attribute of the "crop" global variable.
    real(dp), intent(in) :: SmaxTop

    crop%SmaxTop = SmaxTop
end subroutine SetCrop_SmaxTop

subroutine SetCrop_SmaxBot(SmaxBot)
    !! Setter for the "SmaxBot" attribute of the "crop" global variable.
    real(dp), intent(in) :: SmaxBot

    crop%SmaxBot = SmaxBot
end subroutine SetCrop_SmaxBot

subroutine SetCrop_KcTop(KcTop)
    !! Setter for the "KcTop" attribute of the "crop" global variable.
    real(dp), intent(in) :: KcTop

    crop%KcTop = KcTop
end subroutine SetCrop_KcTop

subroutine SetCrop_KcDecline(KcDecline)
    !! Setter for the "KcDecline" attribute of the "crop" global variable.
    real(dp), intent(in) :: KcDecline

    crop%KcDecline = KcDecline
end subroutine SetCrop_KcDecline

subroutine SetCrop_CCEffectEvapLate(CCEffectEvapLate)
    !! Setter for the "CCEffectEvapLate" attribute of the "crop" global variable.
    integer(int32), intent(in) :: CCEffectEvapLate

    crop%CCEffectEvapLate = CCEffectEvapLate
end subroutine SetCrop_CCEffectEvapLate

subroutine SetCrop_Day1(Day1)
    !! Setter for the "Day1" attribute of the "crop" global variable.
    integer(int32), intent(in) :: Day1

    crop%Day1 = Day1
end subroutine SetCrop_Day1

subroutine SetCrop_DayN(DayN)
    !! Setter for the "DayN" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DayN

    crop%DayN = DayN
end subroutine SetCrop_DayN

subroutine SetCrop_RootMin(RootMin)
    !! Setter for the "RootMin" attribute of the "crop" global variable.
    real(dp), intent(in) :: RootMin

    crop%RootMin = RootMin
end subroutine SetCrop_RootMin

subroutine SetCrop_RootMax(RootMax)
    !! Setter for the "RootMax" attribute of the "crop" global variable.
    real(dp), intent(in) :: RootMax

    crop%RootMax = RootMax
end subroutine SetCrop_RootMax

subroutine SetCrop_RootShape(RootShape)
    !! Setter for the "RootShape" attribute of the "crop" global variable.
    integer(int8), intent(in) :: RootShape

    crop%RootShape = RootShape
end subroutine SetCrop_RootShape

subroutine SetCrop_Tbase(Tbase)
    !! Setter for the "Tbase" attribute of the "crop" global variable.
    real(dp), intent(in) :: Tbase

    crop%Tbase = Tbase
end subroutine SetCrop_Tbase

subroutine SetCrop_Tupper(Tupper)
    !! Setter for the "Tupper" attribute of the "crop" global variable.
    real(dp), intent(in) :: Tupper

    crop%Tupper = Tupper
end subroutine SetCrop_Tupper

subroutine SetCrop_Tcold(Tcold)
    !! Setter for the "Tcold" attribute of the "crop" global variable.
    integer(int8), intent(in) :: Tcold

    crop%Tcold = Tcold
end subroutine SetCrop_Tcold

subroutine SetCrop_Theat(Theat)
    !! Setter for the "Theat" attribute of the "crop" global variable.
    integer(int8), intent(in) :: Theat

    crop%Theat = Theat
end subroutine SetCrop_Theat

subroutine SetCrop_GDtranspLow(GDtranspLow)
    !! Setter for the "GDtranspLow" attribute of the "crop" global variable.
    real(dp), intent(in) :: GDtranspLow

    crop%GDtranspLow = GDtranspLow
end subroutine SetCrop_GDtranspLow

subroutine SetCrop_SizeSeedling(SizeSeedling)
    !! Setter for the "SizeSeedling" attribute of the "crop" global variable.
    real(dp), intent(in) :: SizeSeedling

    crop%SizeSeedling = SizeSeedling
end subroutine SetCrop_SizeSeedling

subroutine SetCrop_SizePlant(SizePlant)
    !! Setter for the "SizePlant" attribute of the "crop" global variable.
    real(dp), intent(in) :: SizePlant

    crop%SizePlant = SizePlant
end subroutine SetCrop_SizePlant

subroutine SetCrop_PlantingDens(PlantingDens)
    !! Setter for the "PlantingDens" attribute of the "crop" global variable.
    integer(int32), intent(in) :: PlantingDens

    crop%PlantingDens = PlantingDens
end subroutine SetCrop_PlantingDens

subroutine SetCrop_CCo(CCo)
    !! Setter for the "CCo" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCo

    crop%CCo = CCo
end subroutine SetCrop_CCo

subroutine SetCrop_CCini(CCini)
    !! Setter for the "CCini" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCini

    crop%CCini = CCini
end subroutine SetCrop_CCini

subroutine SetCrop_CGC(CGC)
    !! Setter for the "CGC" attribute of the "crop" global variable.
    real(dp), intent(in) :: CGC

    crop%CGC = CGC
end subroutine SetCrop_CGC

subroutine SetCrop_GDDCGC(GDDCGC)
    !! Setter for the "GDDCGC" attribute of the "crop" global variable.
    real(dp), intent(in) :: GDDCGC

    crop%GDDCGC = GDDCGC
end subroutine SetCrop_GDDCGC

subroutine SetCrop_CCx(CCx)
    !! Setter for the "CCx" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCx

    crop%CCx = CCx
end subroutine SetCrop_CCx

subroutine SetCrop_CDC(CDC)
    !! Setter for the "CDC" attribute of the "crop" global variable.
    real(dp), intent(in) :: CDC

    crop%CDC = CDC
end subroutine SetCrop_CDC

subroutine SetCrop_GDDCDC(GDDCDC)
    !! Setter for the "GDDCDC" attribute of the "crop" global variable.
    real(dp), intent(in) :: GDDCDC

    crop%GDDCDC = GDDCDC
end subroutine SetCrop_GDDCDC

subroutine SetCrop_CCxAdjusted(CCxAdjusted)
    !! Setter for the "CCxAdjusted" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCxAdjusted

    crop%CCxAdjusted = CCxAdjusted
end subroutine SetCrop_CCxAdjusted

subroutine SetCrop_CCxWithered(CCxWithered)
    !! Setter for the "CCxWithered" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCxWithered

    crop%CCxWithered = CCxWithered
end subroutine SetCrop_CCxWithered

subroutine SetCrop_CCoAdjusted(CCoAdjusted)
    !! Setter for the "CCoAdjusted" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCoAdjusted

    crop%CCoAdjusted = CCoAdjusted
end subroutine SetCrop_CCoAdjusted

subroutine SetCrop_DaysToCCini(DaysToCCini)
    !! Setter for the "DaysToCCini" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToCCini

    crop%DaysToCCini = DaysToCCini
end subroutine SetCrop_DaysToCCini

subroutine SetCrop_DaysToGermination(DaysToGermination)
    !! Setter for the "DaysToGermination" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToGermination

    crop%DaysToGermination = DaysToGermination
end subroutine SetCrop_DaysToGermination

subroutine SetCrop_DaysToFullCanopy(DaysToFullCanopy)
    !! Setter for the "DaysToFullCanopy" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToFullCanopy

    crop%DaysToFullCanopy = DaysToFullCanopy
end subroutine SetCrop_DaysToFullCanopy

subroutine SetCrop_DaysToFullCanopySF(DaysToFullCanopySF)
    !! Setter for the "DaysToFullCanopySF" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToFullCanopySF

    crop%DaysToFullCanopySF = DaysToFullCanopySF
end subroutine SetCrop_DaysToFullCanopySF

subroutine SetCrop_DaysToFlowering(DaysToFlowering)
    !! Setter for the "DaysToFlowering" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToFlowering

    crop%DaysToFlowering = DaysToFlowering
end subroutine SetCrop_DaysToFlowering

subroutine SetCrop_LengthFlowering(LengthFlowering)
    !! Setter for the "LengthFlowering" attribute of the "crop" global variable.
    integer(int32), intent(in) :: LengthFlowering

    crop%LengthFlowering = LengthFlowering
end subroutine SetCrop_LengthFlowering

subroutine SetCrop_DaysToSenescence(DaysToSenescence)
    !! Setter for the "DaysToSenescence" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToSenescence

    crop%DaysToSenescence = DaysToSenescence
end subroutine SetCrop_DaysToSenescence

subroutine SetCrop_DaysToHarvest(DaysToHarvest)
    !! Setter for the "DaysToHarvest" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToHarvest

    crop%DaysToHarvest = DaysToHarvest
end subroutine SetCrop_DaysToHarvest

subroutine SetCrop_DaysToMaxRooting(DaysToMaxRooting)
    !! Setter for the "DaysToMaxRooting" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToMaxRooting

    crop%DaysToMaxRooting = DaysToMaxRooting
end subroutine SetCrop_DaysToMaxRooting

subroutine SetCrop_DaysToHIo(DaysToHIo)
    !! Setter for the "DaysToHIo" attribute of the "crop" global variable.
    integer(int32), intent(in) :: DaysToHIo

    crop%DaysToHIo = DaysToHIo
end subroutine SetCrop_DaysToHIo

subroutine SetCrop_GDDaysToCCini(GDDaysToCCini)
    !! Setter for the "GDDaysToCCini" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToCCini

    crop%GDDaysToCCini = GDDaysToCCini
end subroutine SetCrop_GDDaysToCCini

subroutine SetCrop_GDDaysToGermination(GDDaysToGermination)
    !! Setter for the "GDDaysToGermination" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToGermination

    crop%GDDaysToGermination = GDDaysToGermination
end subroutine SetCrop_GDDaysToGermination

subroutine SetCrop_GDDaysToFullCanopy(GDDaysToFullCanopy)
    !! Setter for the "GDDaysToFullCanopy" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToFullCanopy

    crop%GDDaysToFullCanopy = GDDaysToFullCanopy
end subroutine SetCrop_GDDaysToFullCanopy

subroutine SetCrop_GDDaysToFullCanopySF(GDDaysToFullCanopySF)
    !! Setter for the "GDDaysToFullCanopySF" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToFullCanopySF

    crop%GDDaysToFullCanopySF = GDDaysToFullCanopySF
end subroutine SetCrop_GDDaysToFullCanopySF

subroutine SetCrop_GDDaysToFlowering(GDDaysToFlowering)
    !! Setter for the "GDDaysToFlowering" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToFlowering

    crop%GDDaysToFlowering = GDDaysToFlowering
end subroutine SetCrop_GDDaysToFlowering

subroutine SetCrop_GDDLengthFlowering(GDDLengthFlowering)
    !! Setter for the "GDDLengthFlowering" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDLengthFlowering

    crop%GDDLengthFlowering = GDDLengthFlowering
end subroutine SetCrop_GDDLengthFlowering

subroutine SetCrop_GDDaysToSenescence(GDDaysToSenescence)
    !! Setter for the "GDDaysToSenescence" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToSenescence

    crop%GDDaysToSenescence = GDDaysToSenescence
end subroutine SetCrop_GDDaysToSenescence

subroutine SetCrop_GDDaysToHarvest(GDDaysToHarvest)
    !! Setter for the "GDDaysToHarvest" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToHarvest

    crop%GDDaysToHarvest = GDDaysToHarvest
end subroutine SetCrop_GDDaysToHarvest

subroutine SetCrop_GDDaysToMaxRooting(GDDaysToMaxRooting)
    !! Setter for the "GDDaysToMaxRooting" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToMaxRooting

    crop%GDDaysToMaxRooting = GDDaysToMaxRooting
end subroutine SetCrop_GDDaysToMaxRooting

subroutine SetCrop_GDDaysToHIo(GDDaysToHIo)
    !! Setter for the "GDDaysToHIo" attribute of the "crop" global variable.
    integer(int32), intent(in) :: GDDaysToHIo

    crop%GDDaysToHIo = GDDaysToHIo
end subroutine SetCrop_GDDaysToHIo

subroutine SetCrop_WP(WP)
    !! Setter for the "WP" attribute of the "crop" global variable.
    real(dp), intent(in) :: WP

    crop%WP = WP
end subroutine SetCrop_WP

subroutine SetCrop_WPy(WPy)
    !! Setter for the "WPy" attribute of the "crop" global variable.
    integer(int32), intent(in) :: WPy

    crop%WPy = WPy
end subroutine SetCrop_WPy

subroutine SetCrop_AdaptedToCO2(AdaptedToCO2)
    !! Setter for the "AdaptedToCO2" attribute of the "crop" global variable.
    integer(int8), intent(in) :: AdaptedToCO2

    crop%AdaptedToCO2 = AdaptedToCO2
end subroutine SetCrop_AdaptedToCO2

subroutine SetCrop_HI(HI)
    !! Setter for the "HI" attribute of the "crop" global variable.
    integer(int32), intent(in) :: HI

    crop%HI = HI
end subroutine SetCrop_HI

subroutine SetCrop_dHIdt(dHIdt)
    !! Setter for the "dHIdt" attribute of the "crop" global variable.
    real(dp), intent(in) :: dHIdt

    crop%dHIdt = dHIdt
end subroutine SetCrop_dHIdt

subroutine SetCrop_HIincrease(HIincrease)
    !! Setter for the "HIincrease" attribute of the "crop" global variable.
    integer(int8), intent(in) :: HIincrease

    crop%HIincrease = HIincrease
end subroutine SetCrop_HIincrease

subroutine SetCrop_aCoeff(aCoeff)
    !! Setter for the "aCoeff" attribute of the "crop" global variable.
    real(dp), intent(in) :: aCoeff

    crop%aCoeff = aCoeff
end subroutine SetCrop_aCoeff

subroutine SetCrop_bCoeff(bCoeff)
    !! Setter for the "bCoeff" attribute of the "crop" global variable.
    real(dp), intent(in) :: bCoeff

    crop%bCoeff = bCoeff
end subroutine SetCrop_bCoeff

subroutine SetCrop_DHImax(DHImax)
    !! Setter for the "DHImax" attribute of the "crop" global variable.
    integer(int8), intent(in) :: DHImax

    crop%DHImax = DHImax
end subroutine SetCrop_DHImax

subroutine SetCrop_DeterminancyLinked(DeterminancyLinked)
    !! Setter for the "DeterminancyLinked" attribute of the "crop" global variable.
    logical, intent(in) :: DeterminancyLinked

    crop%DeterminancyLinked = DeterminancyLinked
end subroutine SetCrop_DeterminancyLinked

subroutine SetCrop_fExcess(fExcess)
    !! Setter for the "fExcess" attribute of the "crop" global variable.
    integer(int16), intent(in) :: fExcess

    crop%fExcess = fExcess
end subroutine SetCrop_fExcess

subroutine SetCrop_DryMatter(DryMatter)
    !! Setter for the "DryMatter" attribute of the "crop" global variable.
    integer(int8), intent(in) :: DryMatter

    crop%DryMatter = DryMatter
end subroutine SetCrop_DryMatter

subroutine SetCrop_RootMinYear1(RootMinYear1)
    !! Setter for the "RootMinYear1" attribute of the "crop" global variable.
    real(dp), intent(in) :: RootMinYear1

    crop%RootMinYear1 = RootMinYear1
end subroutine SetCrop_RootMinYear1

subroutine SetCrop_SownYear1(SownYear1)
    !! Setter for the "SownYear1" attribute of the "crop" global variable.
    logical, intent(in) :: SownYear1

    crop%SownYear1 = SownYear1
end subroutine SetCrop_SownYear1

subroutine SetCrop_YearCCx(YearCCx)
    !! Setter for the "YearCCx" attribute of the "crop" global variable.
    integer(int8), intent(in) :: YearCCx

    crop%YearCCx = YearCCx
end subroutine SetCrop_YearCCx

subroutine SetCrop_CCxRoot(CCxRoot)
    !! Setter for the "CCxRoot" attribute of the "crop" global variable.
    real(dp), intent(in) :: CCxRoot

    crop%CCxRoot = CCxRoot
end subroutine SetCrop_CCxRoot

function GetCrop_Assimilates() result(Assimilates)
    !! Getter for the "Assimilates" attribute of the "crop" global variable.
    type(rep_Assimilates) :: Assimilates

    Assimilates = crop%Assimilates
end function GetCrop_Assimilates

function GetCrop_Assimilates_On() result(On)
    !! Getter for the "On" attribute of the "Assimilates" attribute of the "crop" global variable.
    logical :: On

    On = crop%Assimilates%On
end function GetCrop_Assimilates_On

function GetCrop_Assimilates_Period() result(Period)
    !! Getter for the "Period" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int32) :: Period

    Period = crop%Assimilates%Period
end function GetCrop_Assimilates_Period

function GetCrop_Assimilates_Stored() result(Stored)
    !! Getter for the "Stored" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int8) :: Stored

    Stored = crop%Assimilates%Stored
end function GetCrop_Assimilates_Stored

function GetCrop_Assimilates_Mobilized() result(Mobilized)
    !! Getter for the "Mobilized" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int8) :: Mobilized

    Mobilized = crop%Assimilates%Mobilized
end function GetCrop_Assimilates_Mobilized

subroutine SetCrop_Assimilates(Assimilates)
    !! Setter for the "Assimilates" attribute of the "crop" global variable.
    type(rep_Assimilates), intent(in) :: Assimilates

    crop%Assimilates = Assimilates
end subroutine SetCrop_Assimilates

subroutine SetCrop_Assimilates_On(On)
    !! Setter for the "On" attribute of the "Assimilates" attribute of the "crop" global variable.
    logical, intent(in) :: On

    crop%Assimilates%On = On
end subroutine SetCrop_Assimilates_On

subroutine SetCrop_Assimilates_Period(Period)
    !! Setter for the "Period" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int32), intent(in) :: Period

    crop%Assimilates%Period = Period
end subroutine SetCrop_Assimilates_Period

subroutine SetCrop_Assimilates_Stored(Stored)
    !! Setter for the "Stored" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int8), intent(in) :: Stored

    crop%Assimilates%Stored = Stored
end subroutine SetCrop_Assimilates_Stored

subroutine SetCrop_Assimilates_Mobilized(Mobilized)
    !! Setter for the "Mobilized" attribute of the "Assimilates" attribute of the "crop" global variable.
    integer(int8), intent(in) :: Mobilized

    crop%Assimilates%Mobilized = Mobilized
end subroutine SetCrop_Assimilates_Mobilized

function GetOnset() result(Onset_out)
    !! Getter for the "onset" global variable.
    type(rep_Onset) :: Onset_out

    Onset_out = onset
end function GetOnset

function GetOnset_GenerateOn() result(GenerateOn)
    !! Getter for the "GenerateOn" attribute of the "onset" global variable.
    logical :: GenerateOn

    GenerateOn = onset%GenerateOn
end function GetOnset_GenerateOn

function GetOnset_GenerateTempOn() result(GenerateTempOn)
    !! Getter for the "GenerateTempOn" attribute of the "onset" global variable.
    logical :: GenerateTempOn

    GenerateTempOn = onset%GenerateTempOn
end function GetOnset_GenerateTempOn

function GetOnset_Criterion() result(Criterion)
    !! Getter for the "Criterion" attribute of the "onset" global variable.
    integer(intEnum) :: Criterion

    Criterion = onset%Criterion
end function GetOnset_Criterion

function GetOnset_AirTCriterion() result(AirTCriterion)
    !! Getter for the "AirTCriterion" attribute of the "onset" global variable.
    integer(intEnum) :: AirTCriterion

    AirTCriterion = onset%AirTCriterion
end function GetOnset_AirTCriterion

function GetOnset_StartSearchDayNr() result(StartSearchDayNr)
    !! Getter for the "StartSearchDayNr" attribute of the "onset" global variable.
    integer(int32) :: StartSearchDayNr

    StartSearchDayNr = onset%StartSearchDayNr
end function GetOnset_StartSearchDayNr

function GetOnset_StopSearchDayNr() result(StopSearchDayNr)
    !! Getter for the "StopSearchDayNr" attribute of the "onset" global variable.
    integer(int32) :: StopSearchDayNr

    StopSearchDayNr = onset%StopSearchDayNr
end function GetOnset_StopSearchDayNr

function GetOnset_LengthSearchPeriod() result(LengthSearchPeriod)
    !! Getter for the "LengthSearchPeriod" attribute of the "onset" global variable.
    integer(int32) :: LengthSearchPeriod

    LengthSearchPeriod = onset%LengthSearchPeriod
end function GetOnset_LengthSearchPeriod

subroutine SetOnset(Onset_in)
    !! Setter for the "onset" global variable.
    type(rep_Onset), intent(in) :: Onset_in

    onset = Onset_in
end subroutine SetOnset

subroutine SetOnset_GenerateOn(GenerateOn)
    !! Setter for the "GenerateOn" attribute of the "onset" global variable.
    logical, intent(in) :: GenerateOn

    onset%GenerateOn = GenerateOn
end subroutine SetOnset_GenerateOn

subroutine SetOnset_GenerateTempOn(GenerateTempOn)
    !! Setter for the "GenerateTempOn" attribute of the "onset" global variable.
    logical, intent(in) :: GenerateTempOn

    onset%GenerateTempOn = GenerateTempOn
end subroutine SetOnset_GenerateTempOn

subroutine SetOnset_Criterion(Criterion)
    !! Setter for the "Criterion" attribute of the "onset" global variable.
    integer(intEnum), intent(in) :: Criterion

    onset%Criterion = Criterion
end subroutine SetOnset_Criterion

subroutine SetOnset_AirTCriterion(AirTCriterion)
    !! Setter for the "AirTCriterion" attribute of the "onset" global variable.
    integer(intEnum), intent(in) :: AirTCriterion

    onset%AirTCriterion = AirTCriterion
end subroutine SetOnset_AirTCriterion

subroutine SetOnset_StartSearchDayNr(StartSearchDayNr)
    !! Setter for the "StartSearchDayNr" attribute of the "onset" global variable.
    integer(int32), intent(in) :: StartSearchDayNr

    onset%StartSearchDayNr = StartSearchDayNr
end subroutine SetOnset_StartSearchDayNr

subroutine SetOnset_StopSearchDayNr(StopSearchDayNr)
    !! Setter for the "StopSearchDayNr" attribute of the "onset" global variable.
    integer(int32), intent(in) :: StopSearchDayNr

    onset%StopSearchDayNr = StopSearchDayNr
end subroutine SetOnset_StopSearchDayNr

subroutine SetOnset_LengthSearchPeriod(LengthSearchPeriod)
    !! Setter for the "LengthSearchPeriod" attribute of the "onset" global variable.
    integer(int32), intent(in) :: LengthSearchPeriod

    onset%LengthSearchPeriod = LengthSearchPeriod
end subroutine SetOnset_LengthSearchPeriod

type(rep_Content) function GetTotalSaltContent()
    !! Getter for the "TotalSaltContent" global variable.

    GetTotalSaltContent = TotalSaltContent
end function GetTotalSaltContent

subroutine SetTotalSaltContent_BeginDay(BeginDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: BeginDay

    TotalSaltContent%BeginDay = BeginDay
end subroutine SetTotalSaltContent_BeginDay

subroutine SetTotalSaltContent_EndDay(EndDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: EndDay

    TotalSaltContent%EndDay = EndDay
end subroutine SetTotalSaltContent_EndDay

subroutine SetTotalSaltContent_ErrorDay(ErrorDay)
    !! Setter for the "TotalSaltContent" global variable.
    real(dp), intent(in) :: ErrorDay

    TotalSaltContent%ErrorDay = ErrorDay
end subroutine SetTotalSaltContent_ErrorDay

type(rep_Content) function GetTotalWaterContent()
    !! Getter for the "TotalWaterContent" global variable.

    GetTotalWaterContent = TotalWaterContent
end function GetTotalWaterContent

subroutine SetTotalWaterContent_BeginDay(BeginDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: BeginDay

    TotalWaterContent%BeginDay = BeginDay
end subroutine SetTotalWaterContent_BeginDay

subroutine SetTotalWaterContent_EndDay(EndDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: EndDay

    TotalWaterContent%EndDay = EndDay
end subroutine SetTotalWaterContent_EndDay

subroutine SetTotalWaterContent_ErrorDay(ErrorDay)
    !! Setter for the "TotalWaterContent" global variable.
    real(dp), intent(in) :: ErrorDay

    TotalWaterContent%ErrorDay = ErrorDay
end subroutine SetTotalWaterContent_ErrorDay


type(rep_RootZoneSalt) function GetRootZoneSalt()
    !! Getter for the "RootZoneSalt" global variable.

    GetRootZoneSalt = RootZoneSalt
end function GetRootZoneSalt

subroutine SetRootZoneSalt_ECe(ECe)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECe

    RootZoneSalt%ECe = ECe
end subroutine SetRootZoneSalt_ECe

subroutine SetRootZoneSalt_ECsw(ECsw)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECsw

    RootZoneSalt%ECsw = ECsw
end subroutine SetRootZoneSalt_ECsw

subroutine SetRootZoneSalt_ECswFC(ECswFC)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: ECswFC

    RootZoneSalt%ECswFC = ECswFC
end subroutine SetRootZoneSalt_ECswFC

subroutine SetRootZoneSalt_KsSalt(KsSalt)
    !! Setter for the "RootZoneSalt" global variable.
    real(dp), intent(in) :: KsSalt

    RootZoneSalt%KsSalt = KsSalt
end subroutine SetRootZoneSalt_KsSalt

integer(intEnum) function GetGenerateTimeMode()
    !! Getter for the "GenerateTimeMode" global variable.

    GetGenerateTimeMode = GenerateTimeMode
end function GetGenerateTimeMode

integer(intEnum) function GetGenerateDepthMode()
    !! Getter for the "GenerateDepthMode" global variable.

    GetGenerateDepthMode = GenerateDepthMode
end function GetGenerateDepthMode

subroutine SetGenerateTimeMode(int_in)
    !! Setter for the "GenerateTimeMode" global variable.
    integer(intEnum), intent(in) :: int_in

    GenerateTimeMode = int_in
end subroutine SetGenerateTimeMode

subroutine SetGenerateDepthMode(int_in)
    !! Setter for the "GenerateDepthMode" global variable.
    integer(intEnum), intent(in) :: int_in

    GenerateDepthMode = int_in
end subroutine SetGenerateDepthMode

integer(intEnum) function GetIrriMode()
    !! Getter for the "IrriMode" global variable.

    GetIrriMode = IrriMode
end function GetIrriMode

integer(intEnum) function GetIrriMethod()
    !! Getter for the "IrriMethod" global variable.

    GetIrriMethod = IrriMethod
end function GetIrriMethod

subroutine SetIrriMode(int_in)
    !! Setter for the "IrriMode" global variable.
    integer(intEnum), intent(in) :: int_in

    IrriMode = int_in
end subroutine SetIrriMode

subroutine SetIrriMethod(int_in)
    !! Setter for the "IrriMethod" global variable.
    integer(intEnum), intent(in) :: int_in

    IrriMethod = int_in
end subroutine SetIrriMethod

function GetTemperatureFile() result(str)
    !! Getter for the "TemperatureFile" global variable.
    character(len=len(TemperatureFile)) :: str

    str = TemperatureFile
end function GetTemperatureFile

subroutine SetTemperatureFile(str)
    !! Setter for the "TemperatureFile" global variable.
    character(len=*), intent(in) :: str

    TemperatureFile = str
end subroutine SetTemperatureFile

function GetTemperatureFilefull() result(str)
    !! Getter for the "TemperatureFilefull" global variable.
    character(len=len(TemperatureFilefull)) :: str

    str = TemperatureFilefull
end function GetTemperatureFilefull

subroutine SetTemperatureFilefull(str)
    !! Setter for the "TemperatureFilefull" global variable.
    character(len=*), intent(in) :: str

    TemperatureFilefull = str
end subroutine SetTemperatureFilefull

function GetCrop_Length_i(i) result(Length_i)
    !! Getter for the "Length" attribute of "Crop" global variable.
    integer(int32), intent(in) :: i
    integer(int32) :: Length_i

    Length_i = Crop%Length(i)
end function GetCrop_Length_i

subroutine SetCrop_Length_i(i, Length_i)
    !! Setter for the "Length" attribute of "Crop" global variable.
    integer(int32), intent(in) :: i
    integer(int32), intent(in) :: Length_i

    Crop%Length(i) = Length_i
end subroutine SetCrop_Length_i

type(rep_clim) function GetTemperatureRecord()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord = TemperatureRecord
end function GetTemperatureRecord

integer(intEnum) function GetTemperatureRecord_DataType()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_DataType = TemperatureRecord%DataType
end function GetTemperatureRecord_DataType

integer(int32) function GetTemperatureRecord_FromD()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromD = TemperatureRecord%FromD
end function GetTemperatureRecord_FromD

integer(int32) function GetTemperatureRecord_FromM()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromM = TemperatureRecord%FromM
end function GetTemperatureRecord_FromM

integer(int32) function GetTemperatureRecord_FromY()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromY = TemperatureRecord%FromY
end function GetTemperatureRecord_FromY

integer(int32) function GetTemperatureRecord_ToD()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToD = TemperatureRecord%ToD
end function GetTemperatureRecord_ToD

integer(int32) function GetTemperatureRecord_ToM()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToM = TemperatureRecord%ToM
end function GetTemperatureRecord_ToM

integer(int32) function GetTemperatureRecord_ToY()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToY = TemperatureRecord%ToY
end function GetTemperatureRecord_ToY

integer(int32) function GetTemperatureRecord_FromDayNr()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_FromDayNr = TemperatureRecord%FromDayNr
end function GetTemperatureRecord_FromDayNr

integer(int32) function GetTemperatureRecord_ToDayNr()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_ToDayNr = TemperatureRecord%ToDayNr
end function GetTemperatureRecord_ToDayNr

function GetTemperatureRecord_FromString() result(str)
    !! Getter for the "TemperatureRecord" global variable.
    character(len=len(TemperatureRecord%FromString)) :: str

    str = TemperatureRecord%FromString
end function GetTemperatureRecord_FromString

function GetTemperatureRecord_ToString() result(str)
    !! Getter for the "TemperatureRecord" global variable.
    character(len=len(TemperatureRecord%ToString)) :: str

    str = TemperatureRecord%ToString
end function GetTemperatureRecord_ToString

integer(int32) function GetTemperatureRecord_NrObs()
    !! Getter for the "TemperatureRecord" global variable.

    GetTemperatureRecord_NrObs = TemperatureRecord%NrObs
end function GetTemperatureRecord_NrObs

subroutine SetTemperatureRecord_DataType(DataType)
    !! Setter for the "TemperatureRecord" global variable.
    integer(intEnum), intent(in) :: DataType

    TemperatureRecord%DataType = DataType
end subroutine SetTemperatureRecord_DataType

subroutine SetTemperatureRecord_FromD(FromD)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromD

    TemperatureRecord%FromD = FromD
end subroutine SetTemperatureRecord_FromD

subroutine SetTemperatureRecord_FromM(FromM)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromM

    TemperatureRecord%FromM = FromM
end subroutine SetTemperatureRecord_FromM

subroutine SetTemperatureRecord_FromY(FromY)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromY

    TemperatureRecord%FromY = FromY
end subroutine SetTemperatureRecord_FromY

subroutine SetTemperatureRecord_ToD(ToD)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToD

    TemperatureRecord%ToD = ToD
end subroutine SetTemperatureRecord_ToD

subroutine SetTemperatureRecord_ToM(ToM)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToM

    TemperatureRecord%ToM = ToM
end subroutine SetTemperatureRecord_ToM

subroutine SetTemperatureRecord_TOY(ToY)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToY

    TemperatureRecord%ToY = ToY
end subroutine SetTemperatureRecord_ToY

subroutine SetTemperatureRecord_ToDayNr(ToDayNr)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: ToDayNr

    TemperatureRecord%ToDayNr = ToDayNr
end subroutine SetTemperatureRecord_ToDayNr

subroutine SetTemperatureRecord_FromDayNr(FromDayNr)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: FromDayNr

    TemperatureRecord%FromDayNr = FromDayNr
end subroutine SetTemperatureRecord_FromDayNr

subroutine SetTemperatureRecord_NrObs(NrObs)
    !! Setter for the "TemperatureRecord" global variable.
    integer(int32), intent(in) :: NrObs

    TemperatureRecord%NrObs = NrObs
end subroutine SetTemperatureRecord_NrObs

subroutine SetTemperatureRecord_ToString(ToString)
    !! Setter for the "TemperatureRecord" global variable.
    character(len=*), intent(in) :: ToString

    TemperatureRecord%ToString = ToString
end subroutine SetTemperatureRecord_ToString

subroutine SetTemperatureRecord_FromString(FromString)
    !! Setter for the "TemperatureRecord" global variable.
    character(len=*), intent(in) :: FromString

    TemperatureRecord%FromString = FromString
end subroutine SetTemperatureRecord_FromString

end module ac_global
