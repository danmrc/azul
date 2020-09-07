*** TABLE 1: DESCRIPTIVE STATISTICS ***

quietly use "analysis.dta", replace

collapse (mean) peerage civilservant military politician eton oxford cambridge age_entry  (max) tenure_aid no_colonies if full==1, by(aid)
drop aid
sum

quietly use "analysis.dta", replace
sum peerage civilservant military politician eton oxford cambridge age_entry tenure no_colonies if year==1860 & full==1
sum peerage civilservant military politician eton oxford cambridge age_entry tenure no_colonies if year==1900 & full==1
sum peerage civilservant military politician eton oxford cambridge age_entry tenure no_colonies if year==1930 & full==1
sum peerage civilservant military politician eton oxford cambridge age_entry tenure no_colonies if year==1960 & full==1

gen share_customs=exp(log_rev_customs_gbp)/exp(log_rev_total_gbp)

sum log_rev_total_gbp share_customs log_exp_total_gbp log_population log_salary_governor_gbp area_tropics log_dist_london if full==1

sum log_rev_total_gbp share_customs log_exp_total_gbp log_population log_salary_governor_gbp area_tropics log_dist_london if year==1860 & full==1
sum log_rev_total_gbp share_customs log_exp_total_gbp log_population log_salary_governor_gbp area_tropics log_dist_london if year==1900 & full==1
sum log_rev_total_gbp share_customs log_exp_total_gbp log_population log_salary_governor_gbp area_tropics log_dist_london if year==1930 & full==1
sum log_rev_total_gbp share_customs log_exp_total_gbp log_population log_salary_governor_gbp area_tropics log_dist_london if year==1960 & full==1

*** TABLE 2: GOVERNOR SALARY AND CONNECTEDNESS TO SECRETARY OF STATE ***

quietly use "analysis.dta", replace

reghdfe log_salary_governor_gbp no_colonies shared_ancestry if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies both_arist if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies both_eton if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies both_oxbridge if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies shared_ancestry both_arist both_eton both_oxbridge if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)

*** TABLE 3: TRANSFERS AND CONNECTEDNESS TO SECRETARY OF STATE ***

quietly use "analysis.dta", replace
bysort sid: egen min_rev_amount_gbp=min(log_rev_total_gbp) if full==1

reghdfe log_salary_governor_gbp  no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp no_colonies connected if full==1, absorb(sid aid year duration) vce(cluster bilateral)
reghdfe min_rev_amount no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe area_tropics no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_dist_london no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)

*** TABLE 4: WARREN FISHER 1930 - REMOVAL OF PATRONAGE ***

quietly use "analysis.dta", replace

foreach var of varlist civilservant politician military peerage eton {
replace `var'=0 if `var'==.
}
foreach var of varlist civilservant politician military peerage no_colonies  min_revenue landlocked min_year log_dist_london area_tropics {
center `var' 
}
foreach var of varlist c_civilservant c_politician c_military c_peerage c_no_colonies  c_min_revenue c_log_dist_london c_area_tropics  c_landlocked c_min_year  {
gen connected_`var'=`var'*connected
}

reghdfe log_salary_governor_gbp  no_colonies connected if full==1, absorb(aid year duration) vce(cluster bilateral)
reghdfe log_salary_governor_gbp  no_colonies connected post1930_connected if full==1, absorb(aid year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_salary_governor_gbp  no_colonies connected post1930_connected connected_year1930 if full==1, absorb(aid year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_salary_governor_gbp  no_colonies connected post1930_connected connected_c_no_colonies connected_c_civilservant connected_c_military connected_c_politician if full==1, absorb(aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_salary_governor_gbp  no_colonies connected post1930_connected   connected_c_min_revenue connected_c_log_dist_london connected_c_area_tropics connected_c_min_year connected_c_landlocked if full==1, absorb(aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_salary_governor_gbp post1930_connected connected no_colonies if sum_d_pre_post==2 & full==1, absorb(aid  year duration) vce(cluster bilateral) 
lincom connected+post1930_connected

*** TABLE 5: FISCAL PERFORMANCE AND CONNECTEDNESS TO SECRETARY OF STATE ***

quietly use "analysis.dta", replace

*** PANEL A: REVENUE
reghdfe log_rev_total_gbp connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
reghdfe log_rev_total_gbp connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_rev_customs_gbp connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
reghdfe log_rev_internal_gbp connected  no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)

*** PANEL B: EXPENDITURES
reghdfe log_exp_total_gbp connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
reghdfe log_exp_total_gbp connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe log_exp_tax_gbp connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
reghdfe log_exp_pubworks_gbp connected  no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)

*** TABLE 6: TAX ORDINANCES, EXEMPTIONS AND CONNECTEDNESS TO SECRETARY OF STATE ***

quietly use "analysis.dta", replace

gen ordinance_social= ordinance_health+ ordinance_education+ ordinance_welfare

reghdfe ordinance_total connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe ordinance_tax connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe ordinance_trade connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe customs_exemptions connected post1930_connected  no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe ordinance_social connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe ordinance_pubworks connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected

*** TABLE 7: ALTERNATIVE PERFORMANCE MEASURES AND CONNECTEDNESS ***

quietly use "analysis.dta", replace

reghdfe social_unrest connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral) 
lincom connected+post1930_connected
reghdfe hansard_mention connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral) 
lincom connected+post1930_connected
reghdfe polarity connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral)
lincom connected+post1930_connected
reghdfe award_highest connected post1930_connected no_colonies if full==1, absorb(state_aid  year duration) vce(cluster bilateral) 
lincom connected+post1930_connected


