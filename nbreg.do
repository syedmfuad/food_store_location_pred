import delimited "C:\Users\Syed Fuad\Desktop\Poisson\ACS_2010\data_curate_withy.csv"

gen r_crime = real(crime)
gen r_bc_density = real(bc_density) 

gen r_largeretail_visit_19 = real(largeretail_visit_19)
gen r_smallretail_visit_19 = real(smallretail_visit_19)
gen r_fastfood_visit_19 = real(fastfood_visit_19)
gen r_fullservice_visit_19 = real(fullservice_visit_19)

gen r_largeretail_count_19 = real(largeretail_count_19)
gen r_smallretail_count_19 = real(smallretail_count_19)
gen r_fastfood_count_19 = real(fastfood_count_19)
gen r_fullservice_count_19 = real(fullservice_count_19)

gen r_largeretail = real(largeretail)
gen r_smallretail = real(smallretail)
gen r_fastfood = real(fastfood)
gen r_fullservice = real(fullservice)

// store count regressions start

// negative binomial on large retailer count

nbreg r_largeretail_count_19 r_smallretail_count_19 r_fastfood_count_19 r_fullservice_count_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on small retailer count

nbreg r_smallretail_count_19 r_largeretail_count_19 r_fastfood_count_19 r_fullservice_count_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on fast food restaurant count

nbreg r_fastfood_count_19 r_largeretail_count_19 r_smallretail_count_19 r_fullservice_count_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on full service restaurant count

nbreg r_fullservice_count_19 r_largeretail_count_19 r_smallretail_count_19 r_fastfood_count_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// store count regressions end



// store visits regressions start

// negative binomial on large retailers visit

nbreg r_largeretail_visit_19 r_smallretail_visit_19 r_fastfood_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on small retailers visit

nbreg r_smallretail_visit_19 r_largeretail_visit_19 r_fastfood_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on fast food visit

nbreg r_fastfood_visit_19 r_largeretail_visit_19 r_smallretail_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// negative binomial on full service visit

nbreg r_fullservice_visit_19 r_largeretail_visit_19 r_smallretail_visit_19 r_fastfood_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime

nbreg, irr

// store visits regressions end



// extra stuff 

// zero truncated negative binomial on large retailers visit

tnbreg r_largeretail_visit_19 r_smallretail_visit_19 r_fastfood_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime if r_largeretail==1

tnbreg, irr

// zero truncated negative binomial on small retailers visit

tnbreg r_smallretail_visit_19 r_largeretail_visit_19 r_fastfood_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime if r_smallretail==1

tnbreg, irr

// zero truncated negative binomial on fast food visit

tnbreg r_fastfood_visit_19 r_largeretail_visit_19 r_smallretail_visit_19 r_fullservice_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime if r_fastfood==1

tnbreg, irr

// zero truncated negative binomial on full service visit

tnbreg r_fullservice_visit_19 r_largeretail_visit_19 r_smallretail_visit_19 r_fastfood_visit_19 hh_medianincome000 pov_rate hh_snap tract_gini unemp below_hs no_degree grad hh_medianval000 pub_transport hh_novehicle pop_dens area black_pop hispanic_pop asian_pop native_pop pacific_pop wfh rural_perc walkability transit_sqml network_density_auto food_tax r_bc_density wc_density r_crime if r_fullservice==1

tnbreg, irr
