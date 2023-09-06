library(dplyr)

data_new <- read.csv("data_curate_new.csv")

########################### Y VARIABLE #########################

########################### 2018 ##########################

y_var_18 <- read.table("store_visits2018June.txt", header=TRUE, sep = "@")

y_var_18 <- as.data.table(y_var_18)
y_var_18 <- dcast(y_var_18, fips ~ type, fun.aggregate = mean,
                  value.var=c("visitors", "store_count"))

#making all NAs/NaNs 0
y_var_18[is.na(y_var_18)] <- 0

colnames(y_var_18)
col <- ncol(y_var_18)

y_var_18 %>% mutate(largeretail_visit_18 = `visitors_Supermarkets and Other Grocery (except Convenience) Stores`,
                    largeretail_count_18 = `store_count_Supermarkets and Other Grocery (except Convenience) Stores`,
                    smallretail_visit_18 = `visitors_Gasoline Stations with Convenience Stores` + 
                      `visitors_All Other General Merchandise Stores` + 
                      `visitors_Pharmacies and Drug Stores` + 
                      `visitors_Department Stores` + 
                      `visitors_Convenience Stores`,
                    smallretail_count_18 = `store_count_Gasoline Stations with Convenience Stores` + 
                      `store_count_All Other General Merchandise Stores` + 
                      `store_count_Pharmacies and Drug Stores` + 
                      `store_count_Department Stores` + 
                      `store_count_Convenience Stores`,
                    fullservice_visit_18 = `visitors_Full-Service Restaurants`,
                    fullservice_count_18 = `store_count_Full-Service Restaurants`,
                    fastfood_visit_18 = `visitors_Limited-Service Restaurants` + 
                      `visitors_Snack and Nonalcoholic Beverage Bars`,
                    fastfood_count_18 = `store_count_Limited-Service Restaurants` + 
                      `store_count_Snack and Nonalcoholic Beverage Bars`) ->  y_var_18

y_var_18 <- subset(y_var_18, select = -c(2:col))

####################### 2019 #########################

y_var_19 <- read.table("store_visits2019June.txt", header=TRUE, sep = "@")

y_var_19 <- as.data.table(y_var_19)
y_var_19 <- dcast(y_var_19, fips ~ type, fun.aggregate = mean,
                  value.var=c("visitors", "store_count"))

y_var_19[is.na(y_var_19)] <- 0

colnames(y_var_19)
col <- ncol(y_var_19)

y_var_19 %>% mutate(largeretail_visit_19 = `visitors_Supermarkets and Other Grocery (except Convenience) Stores`,
                    largeretail_count_19 = `store_count_Supermarkets and Other Grocery (except Convenience) Stores`,
                    smallretail_visit_19 = `visitors_Gasoline Stations with Convenience Stores` + 
                      `visitors_All Other General Merchandise Stores` + 
                      `visitors_Pharmacies and Drug Stores` + 
                      `visitors_Department Stores` + 
                      `visitors_Convenience Stores`,
                    smallretail_count_19 = `store_count_Gasoline Stations with Convenience Stores` + 
                      `store_count_All Other General Merchandise Stores` + 
                      `store_count_Pharmacies and Drug Stores` + 
                      `store_count_Department Stores` + 
                      `store_count_Convenience Stores`,
                    fullservice_visit_19 = `visitors_Full-Service Restaurants`,
                    fullservice_count_19 = `store_count_Full-Service Restaurants`,
                    fastfood_visit_19 = `visitors_Limited-Service Restaurants` + 
                      `visitors_Snack and Nonalcoholic Beverage Bars`,
                    fastfood_count_19 = `store_count_Limited-Service Restaurants` + 
                      `store_count_Snack and Nonalcoholic Beverage Bars`) ->  y_var_19

y_var_19 <- subset(y_var_19, select = -c(2:col))

####################### MERGE #########################

y_var <- merge(y_var_19, y_var_18, by.x=c("fips"), by.y=c("fips"), all.x=TRUE)

y_var %>% mutate(largeretail_count_growth = case_when(largeretail_count_18==0 & largeretail_count_19==0 ~ 0, 
                                                      TRUE ~ ((largeretail_count_19-largeretail_count_18)/(largeretail_count_18))*100),
                 smallretail_count_growth = case_when(smallretail_count_18==0 & smallretail_count_19==0 ~ 0, 
                                                      TRUE ~ ((smallretail_count_19-smallretail_count_18)/(smallretail_count_18))*100),
                 fullservice_count_growth = case_when(fullservice_count_18==0 & fullservice_count_19==0 ~ 0, 
                                                      TRUE ~ ((fullservice_count_19-fullservice_count_18)/(fullservice_count_18))*100),
                 fastfood_count_growth = case_when(fastfood_count_18==0 & fastfood_count_19==0 ~ 0, 
                                                   TRUE ~ ((fastfood_count_19-fastfood_count_18)/(fastfood_count_18))*100),
                 
                 largeretail_visit_growth = case_when(largeretail_visit_18==0 & largeretail_visit_19==0 ~ 0, 
                                                      TRUE ~ ((largeretail_visit_19-largeretail_visit_18)/(largeretail_visit_18))*100),
                 smallretail_visit_growth = case_when(smallretail_visit_18==0 & smallretail_visit_19==0 ~ 0, 
                                                      TRUE ~ ((smallretail_visit_19-smallretail_visit_18)/(smallretail_visit_18))*100),
                 fullservice_visit_growth = case_when(fullservice_visit_18==0 & fullservice_visit_19==0 ~ 0, 
                                                      TRUE ~ ((fullservice_visit_19-fullservice_visit_18)/(fullservice_visit_18))*100),
                 fastfood_visit_growth = case_when(fastfood_visit_18==0 & fastfood_visit_19==0 ~ 0, 
                                                   TRUE ~ ((fastfood_visit_19-fastfood_visit_18)/(fastfood_visit_18))*100)) -> y_var

y_var <- do.call(data.frame,lapply(y_var, function(x) replace(x, is.infinite(x),NA)))

data_df <- merge(data_new, y_var, by.x=c("GEOID"), by.y=c("fips"), all.x=TRUE)

####################### BINARY VARIABLES FOR LOGIT #########################

data_df$largeretail <- ifelse(data_df$largeretail_count_19 > 0, 1, 0)
data_df$smallretail <- ifelse(data_df$smallretail_count_19 > 0, 1, 0)
data_df$fullservice <- ifelse(data_df$fullservice_count_19 > 0, 1, 0)
data_df$fastfood <- ifelse(data_df$fastfood_count_19 > 0, 1, 0)

write.csv(data_df, file="data_curate_withy.csv")

#Y-variables data available for 56,369 census tracts and X-variables data available for 69,496 tracts
#After merging, the Y-variables in the extra tracts (~13,000 tracts) are assigned NA but I'm not converting these to zero 
#B/c I'm assuming that data were not collected from these census tracts (and NOT that there were no stores in these tracts)

#data_df[,c(53:80)][is.na(data_df[,c(53:80)])] <- 0

