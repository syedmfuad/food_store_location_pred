library(tidycensus)
library(tidyverse)
library(maps)
library(sf)
library(data.table)
data(state.fips)

var_dec10 <- load_variables(2010, "sf1", cache = TRUE) #variables in decennial 2010
var_acs10 <- load_variables(2010, "acs5", cache = TRUE) #variables in acs 2010

#sequencing data download because downloading so many variables slows down office PC!!!

#batch 1

#variables in batch 1
#DP02_0001 = total households acs5/profile
#B19013_001 = median household income
#B17017_002 = total households in poverty
#B22001_002 = total households with snap
#B19083_001 = tract gini inequality
#B01003_001 = total tract population
#B25097_001 = median property value
#B08006_008 = total population taking public transportation to work (except taxicab)
#B08201_002 = total households without vehicle
#B08006_017 = total population working from home
#B28002_002 = total households with internet

vt1 <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001", hh_medianincome = "B19013_001",
                             hh_poverty="B17017_002", hh_snap="B22002_002", #B22001_002
                             tract_gini="B19083_001", tract_pop="B01003_001", 
                             hh_medianval="B25097_001", public_trans="B08006_008", 
                             hh_novehicle="B08201_002", wfh = "B08006_017"), 
               state=force(state.fips)$abb,
               year = 2010)

vt1_dt <- as.data.table(vt1)
data_wide_vt1 <- dcast(vt1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt1$pov_rate <- (data_wide_vt1$hh_poverty/data_wide_vt1$hh_number)*100
data_wide_vt1$pub_transport <- (data_wide_vt1$public_trans/data_wide_vt1$tract_pop)*100
data_wide_vt1$hh_medianincome000 <- data_wide_vt1$hh_medianincome/1000
data_wide_vt1$hh_medianval000 <- data_wide_vt1$hh_medianval/1000
data_wide_vt1$wfh <- (data_wide_vt1$wfh/data_wide_vt1$tract_pop)*100

data_wide_vt1 <- select(data_wide_vt1, GEOID, hh_medianincome000, pov_rate, hh_snap,
                        tract_gini, hh_medianval000, pub_transport, 
                        hh_novehicle, wfh)

internet <- get_acs(geography = "tract", 
                    variables = c(hh_internet="B28002_002", hh_number="DP02_0001"), 
                    state=force(state.fips)$abb,
                    year = 2017)

internet <- as.data.table(internet)
internet <- dcast(internet, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

internet$hh_internet <- (internet$hh_internet/internet$hh_number)*100

data_wide_vt1 <- merge(data_wide_vt1, internet, by="GEOID")

#batch 2 #education

#variables in batch 2 #education
#B06009_002 = 25+ age population with less than high school education
#B15002_012 + B15002_013 = 25+ age population with some college less than 1 year, no degree + some college more than 1 year, no degree (male)
#B15002_029 + B15002_030 = 25+ age population with some college less than 1 year, no degree + some college more than 1 year, no degree (female)
#B06009_004 = 25+ age population with associate's degree or some college
#B06009_005 + B06009_006 = 25+ age population with bachelor's degree + population with grad and above degree

vt2 <- get_acs(geography = "tract", 
               variables = c(tract_pop="B06009_001", b_hs="B06009_002", 
                             b_25_1="B15002_012", b_25_2="B15002_013", 
                             b_25_3="B15002_029", b_25_4="B15002_030",
                             associate="B06009_004", bachelor="B06009_005", 
                             a_bachelor="B06009_006"), 
               state=force(state.fips)$abb,
               year = 2010)

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
               year = 2010)

vt3_dt <- as.data.table(vt3)
data_wide_vt3 <- dcast(vt3_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt3$unemp <- (rowSums(data_wide_vt3[,29:54])/rowSums(data_wide_vt3[,2:27]))*100

data_wide_vt3 <- select(data_wide_vt3, GEOID, unemp)

#batch 4

#variables in batch 4
#B01003_001 = tract population
#B02001_002 = white population in tract
#B02001_003 = black population in tract
#B02001_005 = asian population in tract
#B02001_004 = american indian population in tract
#B02001_006 = native hawaiian population in tract
#B01001I_001 = hispanic population in tract

vt4 <- get_acs(geography = "tract", 
               variables = c(tract_pop="B01003_001", white="B02001_002", black="B02001_003", 
                             asian="B02001_005", am_ind="B02001_004", 
                             nat_hawaiian="B02001_006", hispanic="B01001I_001"), 
               geometry=TRUE, state=force(state.fips)$abb, year = 2010)

vt4_dt <- as.data.table(vt4)
data_wide_vt4 <- dcast(vt4_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

data_wide_vt4$white_pop <- (data_wide_vt4$white/data_wide_vt4$tract_pop)*100
data_wide_vt4$black_pop <- (data_wide_vt4$black/data_wide_vt4$tract_pop)*100
data_wide_vt4$asian_pop <- (data_wide_vt4$asian/data_wide_vt4$tract_pop)*100
data_wide_vt4$native_pop <- (data_wide_vt4$am_ind/data_wide_vt4$tract_pop)*100
data_wide_vt4$pacific_pop <- (data_wide_vt4$nat_hawaiian/data_wide_vt4$tract_pop)*100
data_wide_vt4$hispanic_pop <- (data_wide_vt4$hispanic/data_wide_vt4$tract_pop)*100

data_wide_vt4 <- select(data_wide_vt4, GEOID, tract_pop, white_pop, black_pop, asian_pop,
                        native_pop, pacific_pop, hispanic_pop)

#batch 5 #rural and urban population percentage

#P002005 = rural population in tract
#P002002 = urban population in tract
#P002001 = tract population

dc1 <- get_decennial(geography = "tract", 
                     variables = c(rural_pop= "P002005", urban_pop = "P002002",
                                   tract_pop="P002001"), 
                     state=force(state.fips)$abb,
                     year = 2010)

dc1_dt <- as.data.table(dc1)
data_wide_dc1 <- dcast(dc1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("value"))

data_wide_dc1$rural_perc <- (data_wide_dc1$rural_pop/data_wide_dc1$tract_pop)*100

data_wide_dc1 <- select(data_wide_dc1, GEOID, rural_perc)

#merge

data_wide_total <- merge(data_wide_vt1, data_wide_vt2, by="GEOID")
data_wide_total <- merge(data_wide_total, data_wide_vt3, by="GEOID")
data_wide_total <- merge(data_wide_total, data_wide_vt4, by="GEOID")
data_wide_total <- merge(data_wide_total, data_wide_dc1, by="GEOID")

#batch 6

geom <- subset(vt4_dt, variable=="tract_pop")
geom$area <- (st_area(geom$geometry)/1000000)*0.386102 #converting sq km to sq miles
geom$variable <- NULL
geom$estimate <- NULL
geom$moe <- NULL

geom_unique <- distinct(geom, GEOID, .keep_all = TRUE)

data_wide_total <- merge(data_wide_total, geom_unique, by="GEOID")

data_wide_total$pop_dens <- (data_wide_total$tract_pop/1000)/data_wide_total$area

data_wide_total$geometry <- NULL
data_total <- data_wide_total[complete.cases(data_wide_total), ]

data_total %>% select(GEOID, NAME, everything()) -> data_total

data_curate <- select(data_total, GEOID, NAME, hh_medianincome000, pov_rate, hh_snap, 
                      tract_gini, unemp, below_hs, no_degree, some_college, grad, hh_medianval000, 
                      pub_transport, hh_novehicle, wfh, hh_internet, area, pop_dens, white_pop, black_pop, hispanic_pop, asian_pop, 
                      native_pop, pacific_pop, rural_perc)

write.csv(data_curate, file="data_curate.csv")

###################### CRIME DATA ########################

#data source: https://www.icpsr.umich.edu/web/NACJD/studies/33523

library(haven)

crime_data <- load(file="33523-0004-Data.rda")
crime_data <- da33523.0004
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

crime_data$crime <- crime_data$pcrime+crime_data$vcrime

crime_data <- select(crime_data, FIPS, vcrime, pcrime, crime)

data_new <- read.csv("data_curate.csv")
data_new$digit <- nDigits(data_new$GEOID)
data_new$lead_GEOID <- ifelse(data_new$digit==10, as.character("x0"), as.character("x"))
data_new$GEOID <- paste0(data_new$lead_GEOID, data_new$GEOID)
data_new$LeftGEOID <- substr(data_new$GEOID, 1, 6)

data_new <- merge(data_new, crime_data, by.x=c("LeftGEOID"), by.y=c("FIPS"))

##################### PARKING SPACE DATA ########################

#data source: https://www.sciencebase.gov/catalog/item/5c0ea593e4b0c53ecb2af59f

park <- read.csv("parking.csv")

data_new <- merge(data_new, park, by.x=c("LeftGEOID"), by.y=c("GEOID"))

##################### WALKABILITY SCORE DATA #####################

#data source: https://edg.epa.gov/data/
#Public -> EPA Office of Policy -> Smart_Location_DB_v02b.zip -> SLDv02.csv

walk <- read.csv("SLDv02.csv")

walk$D4a[walk$D4a==-99999]<-1609
walk[walk==-99999]<-0

data_tract = walk %>% group_by(geoid)  %>%
  summarise(walkability = mean(WalkIndex, na.rm=TRUE),
            transit_sqml = mean(D4d, na.rm=TRUE),
            transit_dist = mean(D4a, na.rm=TRUE),
            network_density_auto = mean(D3aao, na.rm=TRUE),
            network_density_ped = mean(D3apo, na.rm=TRUE),
            road_density = mean(D3a, na.rm=TRUE),
            resident_hh_density = mean(D1A, na.rm=TRUE),
            pop_density = mean(D1B, na.rm=TRUE),
            emp_density = mean(D1C, na.rm=TRUE),
            retail_density = mean(D1C5_Ret10, na.rm=TRUE),
            office_density = mean(D1C5_Off10, na.rm=TRUE),
            industrial_density = mean(D1C5_Ind10, na.rm=TRUE),
            service_density = mean(D1C5_Svc10, na.rm=TRUE))

data_tract$bc_density <- data_tract$retail_density+data_tract$industrial_density
data_tract$wc_density <- data_tract$office_density+data_tract$service_density

data_tract$sdigit <- nDigits(data_tract$geoid)
data_tract$lead_GEOID <- ifelse(data_tract$sdigit==10, as.character("x0"), as.character("x"))
data_tract$GEOID <- paste0(data_tract$lead_GEOID, data_tract$geoid)
data_new <- merge(data_new, data_tract, by=c("GEOID"))

############################ LOW ACCESS TO STORES DATA ############################

#data source: https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/#Current%20Version

atlas <- read.csv("atlas_Nov2012.csv")
atlas$sdigit <- nDigits(atlas$FIPS)
atlas$lead_state <- ifelse(atlas$sdigit==4, as.character("x0"), as.character("x"))
atlas$LeftGEOID <- paste0(atlas$lead_state, atlas$FIPS)

data_new <- merge(data_new, atlas, by=c("LeftGEOID"))

data_new <- subset(data_new, select = -c(X.x, digit, lead_GEOID.x, GEOID10, park1974, park1982, park1992,
                                         park2002, NAME.y, lead_state, sdigit.y, FIPS2, FIPS, X.y, lead_GEOID.y, sdigit.x, 
                                         geoid, proc_code, State))

data_new <- data_new %>%
  rename(FIPS = LeftGEOID,
         NAME = NAME.x,
         pop_laccess = PCT_LACCESS_POP10,
         hhnv_laccess = PCT_LACCESS_HHNV10,
         hhli_laccess = PCT_LACCESS_LOWI10,
         food_tax = FOOD_TAX11,
         pct_diabetes = PCT_DIABETES_ADULTS09,
         pct_obese = PCT_OBESE_ADULTS09,
         rec_facility = RECFACPTH09)


data_new %>% select(FIPS, GEOID, NAME, STATE, County, everything()) -> data_new

write.csv(data_new, file="data_curate_new.csv")


