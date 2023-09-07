# food_store_location_pred
This paper uses census tract-level data to determine the presence, density, and popularity of U.S. food retailers. We merge census tract-level demographic data, neighborhood amenities, and Point of Interest (POI) data from anonymized cellphone GPS ‘pings’ to identify food retailer location and foot traffic information. 

## acs2010_codes.R 
Downloads ACS 2010 5-year average census tract level demographic data + 2010 dicennial census data. Merges census tract-level data on walkability scores, job density, transit availability, road network, local sales tax and county-level crime statistics. 

## acs2019_codes.R 
Same as above but with 2019 data. 

## merge.R 
Merges with Safegraph point-of-interest data (store count) and store visit frequency data. 

## regressions.R

* Correlation plot
* t-test
* Logit
* Gradient Boosted Logit
* Gradient Boosted Logit w/ balancing
* OLS
* Gradient Boosted OLS 

## nbreg.do

* Negative binomial regression
* Zero-truncated negative binomial regression 
