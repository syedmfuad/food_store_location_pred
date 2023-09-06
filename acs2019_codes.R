library(tidycensus)
library(tidyverse)
library(maps)
library(sf)
library(data.table)
data(state.fips)

#variables in batch 1
#B19013_001 = median household income
#DP02_0001 = total households acs5/profile
#B17017_002 = household poverty
#B22010_002 = households with snap
#B19083_001 = tract gini inequality
#B01003_001 = tract population

#variables in batch 2 #education
#B06009_002 = 25+ age population with less than high school education
#B15002_012 + B15002_013 = 25+ age population with some college less than 1 year, no degree + some college more than 1 year, no degree (male)
#B15002_029 + B15002_030 = 25+ age population with some college less than 1 year, no degree + some college more than 1 year, no degree (female)
#B06009_004 = 25+ age population with associate's degree or some college
#B06009_005 + B06009_006 = 25+ age population with bachelor's degree + population with grad and above degree

#variables in batch 3 #unemployment
#B08101_001 = population in tract above 16
#   male        female
#B23001_008 + B23001_094 = unemployed civilian population between ages 16-19 #1
#B23001_015 + B23001_101 = unemployed civilian population between ages 20-21 #2
#B23001_022 + B23001_108 = unemployed civilian population between ages 22-24 #3
#B23001_029 + B23001_115 = unemployed civilian population between ages 25-29 #4
#B23001_036 + B23001_122 = unemployed civilian population between ages 30-34 #5
#B23001_043 + B23001_129 = unemployed civilian population between ages 35-44 #6
#B23001_050 + B23001_136 = unemployed civilian population between ages 45-54 #7
#B23001_057 + B23001_143 = unemployed civilian population between ages 55-59 #8
#B23001_064 + B23001_150 = unemployed civilian population between ages 60-61 #9
#B23001_071 + B23001_157 = unemployed civilian population between ages 62-64 #10
#B23001_076 + B23001_162 = unemployed civilian population between ages 65-69 #11
#B23001_081 + B23001_167 = unemployed civilian population between ages 70-74 #12
#B23001_086 + B23001_172 = unemployed civilian population between ages 75 and over #13

#civilian labor force population
#   male        female
#B23001_006 + B23001_092 = civilian labor force population between ages 16-19 #1
#B23001_013 + B23001_099 = civilian labor force population between ages 20-21 #2
#B23001_020 + B23001_106 = civilian labor force population between ages 22-24 #3
#B23001_027 + B23001_113 = civilian labor force population between ages 25-29 #4
#B23001_034 + B23001_120 = civilian labor force population between ages 30-34 #5
#B23001_041 + B23001_127 = civilian labor force population between ages 35-44 #6
#B23001_048 + B23001_134 = civilian labor force population between ages 45-54 #7
#B23001_055 + B23001_141 = civilian labor force population between ages 55-59 #8
#B23001_062 + B23001_148 = civilian labor force population between ages 60-61 #9
#B23001_069 + B23001_155 = civilian labor force population between ages 62-64 #10
#B23001_074 + B23001_160 = civilian labor force population between ages 65-69 #11
#B23001_079 + B23001_165 = civilian labor force population between ages 70-74 #12
#B23001_084 + B23001_170 = civilian labor force population between ages 75 and over #13

#variables in batch 4
#B01003_001 = tract population
#B25097_001 = median value of house
#B08006_008 = population using public transport excluding taxi
#B08201_002 = households with no vehicle
#B02001_003 = black population in tract
#B02001_005 = asian population in tract
#B02001_004 = american indian population in tract
#B02001_006 = native hawaiian population in tract
#B01001I_001 = hispanic population in tract

#variables in batch 5
#DP02_0001 = total households
#B28002_002 = households with internet

#batch 1

vt1 <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001", hh_medianincome = "B19013_001", 
                             hh_poverty="B17017_002", hh_snap="B22010_002", 
                             tract_gini="B19083_001", tract_pop="B01003_001", 
                             hh_medianval="B25097_001", public_trans="B08006_008", 
                             hh_novehicle="B08201_002"), 
               state=force(state.fips)$abb,
               year = 2019)

vt1_dt <- as.data.table(vt1)
data_wide_vt1 <- dcast(vt1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt1$pov_rate <- (data_wide_vt1$hh_poverty/data_wide_vt1$hh_number)*100
data_wide_vt1$pub_transport <- (data_wide_vt1$public_trans/data_wide_vt1$tract_pop)*100
data_wide_vt1$hh_medianincome000 <- data_wide_vt1$hh_medianincome/1000
data_wide_vt1$hh_medianval000 <- data_wide_vt1$hh_medianval/1000

data_wide_vt1 <- select(data_wide_vt1, GEOID, hh_medianincome000, pov_rate, hh_snap,
                        tract_gini, hh_medianval000, pub_transport, 
                        hh_novehicle)

#batch 2 #education

vt2 <- get_acs(geography = "tract", 
               variables = c(tract_pop="B06009_001", b_hs="B06009_002", 
                             b_25_1="B15002_012", b_25_2="B15002_013", 
                             b_25_3="B15002_029", b_25_4="B15002_030",
                             associate="B06009_004", bachelor="B06009_005", 
                             a_bachelor="B06009_006"), 
               state=force(state.fips)$abb,
               year = 2019)

vt2_dt <- as.data.table(vt2)
data_wide_vt2 <- dcast(vt2_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt2$below_hs <- (data_wide_vt2$b_hs/data_wide_vt2$tract_pop)*100
data_wide_vt2$no_degree <- ((data_wide_vt2$b_25_1 + data_wide_vt2$b_25_2 +
                               data_wide_vt2$b_25_3 + data_wide_vt2$b_25_4)/data_wide_vt2$tract_pop)*100
data_wide_vt2$some_college <- (data_wide_vt2$associate/data_wide_vt2$tract_pop)*100
data_wide_vt2$grad <- ((data_wide_vt2$bachelor + data_wide_vt2$a_bachelor)/data_wide_vt2$tract_pop)*100

data_wide_vt2 <- select(data_wide_vt2, GEOID, below_hs, no_degree, some_college,
                        grad)

#batch 3 #unemployment

vt3 <- get_acs(geography = "tract", 
               variables = c(tract_pop="B08101_001", um1="B23001_008", 
                             uf1="B23001_094", um2="B23001_015", 
                             uf2="B23001_101", um3="B23001_022",
                             uf3="B23001_108", um4="B23001_029", 
                             uf4="B23001_115", um5="B23001_036",
                             uf5="B23001_122", um6="B23001_043",
                             uf6="B23001_129", um7="B23001_050",
                             uf7="B23001_136", um8="B23001_057",
                             uf8="B23001_143", um9="B23001_064",
                             uf9="B23001_150", um10="B23001_071",
                             uf10="B23001_157", um11="B23001_076",
                             uf11="B23001_162", um12="B23001_081",
                             uf12="B23001_167", um13="B23001_086",
                             uf13="B23001_172", popm1="B23001_006", 
                             popf1="B23001_092", popm2="B23001_013", 
                             popf2="B23001_099", popm3="B23001_020",
                             popf3="B23001_106", popm4="B23001_027", 
                             popf4="B23001_113", popm5="B23001_034",
                             popf5="B23001_120", popm6="B23001_041",
                             popf6="B23001_127", popm7="B23001_048",
                             popf7="B23001_134", popm8="B23001_055",
                             popf8="B23001_141", popm9="B23001_062",
                             popf9="B23001_148", popm10="B23001_069",
                             popf10="B23001_155", popm11="B23001_074",
                             popf11="B23001_160", popm12="B23001_079",
                             popf12="B23001_165", popm13="B23001_084",
                             popf13="B23001_170"), 
               state=force(state.fips)$abb,
               year = 2019)

vt3_dt <- as.data.table(vt3)
data_wide_vt3 <- dcast(vt3_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt3$unemp <- ((data_wide_vt3$um1 + data_wide_vt3$uf1 + 
                           data_wide_vt3$um2 + data_wide_vt3$uf2 + 
                           data_wide_vt3$um3 + data_wide_vt3$uf3 +
                           data_wide_vt3$um4 + data_wide_vt3$uf4 + 
                           data_wide_vt3$um5 + data_wide_vt3$uf5 + 
                           data_wide_vt3$um6 + data_wide_vt3$uf6 +
                           data_wide_vt3$um7 + data_wide_vt3$uf7 + 
                           data_wide_vt3$um8 + data_wide_vt3$uf8 + 
                           data_wide_vt3$um9 + data_wide_vt3$uf9 +
                           data_wide_vt3$um10 + data_wide_vt3$uf10 + 
                           data_wide_vt3$um11 + data_wide_vt3$uf11 + 
                           data_wide_vt3$um12 + data_wide_vt3$uf12 +
                           data_wide_vt3$um13 + data_wide_vt3$uf13)/(data_wide_vt3$popm1 + data_wide_vt3$popf1 + 
                                                                       data_wide_vt3$popm2 + data_wide_vt3$popf2 + 
                                                                       data_wide_vt3$popm3 + data_wide_vt3$popf3 +
                                                                       data_wide_vt3$popm4 + data_wide_vt3$popf4 + 
                                                                       data_wide_vt3$popm5 + data_wide_vt3$popf5 + 
                                                                       data_wide_vt3$popm6 + data_wide_vt3$popf6 +
                                                                       data_wide_vt3$popm7 + data_wide_vt3$popf7 + 
                                                                       data_wide_vt3$popm8 + data_wide_vt3$popf8 + 
                                                                       data_wide_vt3$popm9 + data_wide_vt3$popf9 +
                                                                       data_wide_vt3$popm10 + data_wide_vt3$popf10 + 
                                                                       data_wide_vt3$popm11 + data_wide_vt3$popf11 + 
                                                                       data_wide_vt3$popm12 + data_wide_vt3$popf12 +
                                                                       data_wide_vt3$popm13 + data_wide_vt3$popf13))*100

data_wide_vt3 <- select(data_wide_vt3, GEOID, unemp)

#batch 4

vt4 <- get_acs(geography = "tract", 
               variables = c(tract_pop="B01003_001", black="B02001_003", 
                             asian="B02001_005", am_ind="B02001_004", 
                             nat_hawaiian="B02001_006", hispanic="B01001I_001"), 
               geometry=TRUE, state=force(state.fips)$abb, year = 2019)

vt4_dt <- as.data.table(vt4)
data_wide_vt4 <- dcast(vt4_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt4$black_pop <- (data_wide_vt4$black/data_wide_vt4$tract_pop)*100
data_wide_vt4$asian_pop <- (data_wide_vt4$asian/data_wide_vt4$tract_pop)*100
data_wide_vt4$native_pop <- (data_wide_vt4$am_ind/data_wide_vt4$tract_pop)*100
data_wide_vt4$pacific_pop <- (data_wide_vt4$nat_hawaiian/data_wide_vt4$tract_pop)*100
data_wide_vt4$hispanic_pop <- (data_wide_vt4$hispanic/data_wide_vt4$tract_pop)*100

data_wide_vt4 <- select(data_wide_vt4, GEOID, tract_pop, black_pop, asian_pop,
                        native_pop, pacific_pop, hispanic_pop)

data_wide_total <- merge(data_wide_vt1, data_wide_vt2, by="GEOID")
data_wide_total <- merge(data_wide_total, data_wide_vt3, by="GEOID")
data_wide_total <- merge(data_wide_total, data_wide_vt4, by="GEOID")

#batch 5

geom <- subset(vt4_dt, variable=="tract_pop")
geom$area <- (st_area(geom$geometry)/1000000)*0.386102 #converting sq km to sq miles
geom$variable <- NULL
geom$estimate <- NULL
geom$moe <- NULL

geom_unique <- distinct(geom, GEOID, .keep_all = TRUE)

data_wide_total <- merge(data_wide_total, geom_unique, by="GEOID")

data_wide_total$pop_dens <- (data_wide_total$tract_pop/1000)/data_wide_total$area

data_total <- data_wide_total[complete.cases(data_wide_total[, -21]), ]
data_total %>% select(GEOID, NAME, everything()) -> data_total

data_curate <- select(data_total, GEOID, NAME, hh_medianincome000, pov_rate, hh_snap, 
                      tract_gini, unemp, below_hs, no_degree, some_college, grad, hh_medianval000, 
                      pub_transport, hh_novehicle, area, pop_dens, black_pop, hispanic_pop, asian_pop, 
                      native_pop, pacific_pop)

write.csv(data_curate, file="data_curate.csv")

####################### NEW ADDITION ############################

####################### ADDED INTERNET ACCESS, WHITE POP, WFH ############################

vt5 <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001", hh_int_wo_sub = "B28002_012", 
                             tract_pop="B01003_001", white_pop="B02001_002",
                             hh_internet="B28002_002", hh_nointernet="B28002_013", 
                             hh_broadband="B28002_004", hh_cell="B28002_005", 
                             hh_satellite="B28002_009", wfh="B08006_017"),
               state=force(state.fips)$abb,
               year = 2019)

vt5_dt <- as.data.table(vt5)
data_wide_vt5 <- dcast(vt5_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt5$hh_internet <- ((data_wide_vt5$hh_internet+data_wide_vt5$hh_int_wo_sub)/data_wide_vt5$hh_number)*100
data_wide_vt5$hh_nointernet <- (data_wide_vt5$hh_nointernet/data_wide_vt5$hh_number)*100
data_wide_vt5$hh_broadband <- (data_wide_vt5$hh_broadband/data_wide_vt5$hh_number)*100
data_wide_vt5$hh_cell <- (data_wide_vt5$hh_cell/data_wide_vt5$hh_number)*100
data_wide_vt5$hh_satellite <- (data_wide_vt5$hh_satellite/data_wide_vt5$hh_number)*100
data_wide_vt5$wfh <- (data_wide_vt5$wfh/data_wide_vt5$tract_pop)*100
data_wide_vt5$white_pop <- (data_wide_vt5$white_pop/data_wide_vt5$tract_pop)*100
#data_wide_vt5$wfh1 <- (data_wide_vt5$wfh1/data_wide_vt5$hh_number)*100

data_wide_vt5 <- select(data_wide_vt5, GEOID, hh_internet, hh_nointernet, hh_broadband,
                        hh_cell, hh_satellite, wfh, white_pop)
data_wide_vt5$GEOID2 <- paste0("x", data_wide_vt5$GEOID)

data_old <- read.csv("data_curate.csv")

data_new <- merge(data_old, data_wide_vt5, by.x=c("GEOID2"), by.y=c("GEOID2"))

###################### CRIME DATA ########################

#data source: https://www.icpsr.umich.edu/web/NACJD/studies/37059/summary

library(haven)

crime_data <- load(file="37059-0004-Data.rda")
crime_data <- da37059.0004
crime_data <- crime_data[crime_data$COVIND>0,] #removing counties with zero coverage

nDigits <- function(x) nchar(trunc(abs(x)))
crime_data$cdigit <- nDigits(crime_data$FIPS_CTY)
crime_data$sdigit <- nDigits(crime_data$FIPS_ST)
crime_data$lead_county <- ifelse(crime_data$cdigit==1, as.character("00"), ifelse(crime_data$cdigit==2, as.character("0"), ""))
crime_data$countyfips <- paste0(crime_data$lead_county, crime_data$FIPS_CTY)
crime_data$lead_state <- ifelse(crime_data$sdigit==1, as.character("x0"), as.character("x"))
crime_data$statefips <- paste0(crime_data$lead_state, crime_data$FIPS_ST)

crime_data$FIPS <- paste0(crime_data$statefips, crime_data$countyfips)

crime_data$vcrime <- (crime_data$VIOL/crime_data$CPOPCRIM)*1000
crime_data$pcrime <- (crime_data$PROPERTY/crime_data$CPOPCRIM)*1000

is.na(crime_data) <- sapply(crime_data, is.infinite)
crime_data$pcrime[is.nan(crime_data$pcrime)]<-NA
crime_data$vcrime[is.nan(crime_data$vcrime)]<-NA

crime_data <- select(crime_data, FIPS, vcrime, pcrime)

data_new <- merge(data_new, crime_data, by.x=c("LeftGEOID"), by.y=c("FIPS"))

##################### PARKING SPACE DATA ########################

#data source: https://www.sciencebase.gov/catalog/item/5c0ea593e4b0c53ecb2af59f

park <- read.csv("parking.csv")

data_new <- merge(data_new, park, by.x=c("LeftGEOID"), by.y=c("GEOID"))

##################### WALKABILITY SCORE DATA #####################

#data source: https://catalog.data.gov/dataset/walkability-index

walk <- read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
nDigits <- function(x) nchar(trunc(abs(x)))
walk$cdigit <- nDigits(walk$COUNTYFP)
walk$sdigit <- nDigits(walk$STATEFP)
walk$tdigit <- nDigits(walk$TRACTCE)
walk$lead_county <- ifelse(walk$cdigit==1, as.character("00"), ifelse(walk$cdigit==2, as.character("0"), ""))
walk$countyfips <- paste0(walk$lead_county, walk$COUNTYFP)
walk$lead_state <- ifelse(walk$sdigit==1, as.character("x0"), as.character("x"))
walk$statefips <- paste0(walk$lead_state, walk$STATEFP)
walk$lead_tract <- ifelse(walk$tdigit==3, as.character("000"), ifelse(walk$tdigit==4, as.character("00"), 
                          ifelse(walk$tdigit==5, as.character("0"), "")))
walk$tract <- paste0(walk$lead_tract, walk$TRACTCE)

walk$geoid <- paste0(walk$statefips, walk$countyfips, walk$tract)

walk$D4A[walk$D4A==-99999]<-1609
walk[walk==-99999]<-0

data_tract = walk %>% group_by(geoid)  %>%
  summarise(walkability = mean(NatWalkInd, na.rm=TRUE),
            transit_sqml = mean(D4D, na.rm=TRUE),
            transit_dist = mean(D4A, na.rm=TRUE),
            network_density_auto = mean(D3AAO, na.rm=TRUE),
            network_density_ped = mean(D3APO, na.rm=TRUE),
            road_density = mean(D3A, na.rm=TRUE),
            resident_hh_density = mean(D1A, na.rm=TRUE),
            pop_density = mean(D1B, na.rm=TRUE),
            emp_density = mean(D1C, na.rm=TRUE))

data_new <- merge(data_new, data_tract, by.x=c("GEOID2"), by.y=c("geoid"))

############################# LATCH DATA ##############################

#data source: https://www.bts.gov/latch-2017-methodology-appendix-d

latch <- read.csv("latch.csv")
latch$geodigit <- nDigits(latch$geocode)
latch$lead_geocode <- ifelse(latch$geodigit==10, as.character("x0"), as.character("x"))
latch$GEOCODE <- paste0(latch$lead_geocode, latch$geocode)

latch_sub <- select(latch, GEOCODE, est_pmiles, est_vmiles)

data_new <- merge(data_new, latch_sub, by.x=c("GEOID2"), by.y=c("GEOCODE"))

############################ LOW ACCESS TO STORES DATA ############################

#data source: https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/#Current%20Version

atlas <- read.csv("atlas_data.csv")
data_new <- merge(data_new, atlas, by.x=c("LeftGEOID"), by.y=c("fips"))

data_new <- subset(data_new, select = -c(X, GEOID.x, GEOID.y, FIPS, GEOID10, park1974, park1982, park1992,
                                         park2002, proc_code, State, County))

data_new <- data_new %>%
  rename(FIPS = LeftGEOID,
         GEOID = GEOID2,
         NAME = NAME.x,
         COUNTY = NAME.y)

write.csv(data_new, file="data_curate_new.csv")


