*** FIGURE 1: TERRITORIES ADMINISTERED BY THE COLONIAL OFFICE 1905 ***

*** FIGURE 2: SHARE OF GOVERNORS CONNECTED TO THE SECRETARY OF STATE ***

quietly use "analysis.dta", replace

egen connected_excl_school=rowtotal(shared_ancestry both_arist)
replace connected_excl_school=1 if connected_excl_school==2

collapse (mean)  connected_excl_school connected, by(quinquennial)

twoway (connected connected quinquennial)  (connected connected_excl_school quinquennial), scheme(s2mono) graphregion(color(white)) xtitle("Year") ytitle("Share connected")  legend(label(1 "Connected") label(2 "Both peerage & Shared ancestry")) ylabel(0(0.2)0.8) xlabel(1860(10)1960)

*** FIGURE 3: AVERAGE SALARY CONNECTED VS. UNCONNECTED OVER TIME ***

quietly use "analysis.dta", replace

replace decade=1850 if year>=1850 & year<=1859
gen salary_governor_gbp=exp(log_salary_governor_gbp)
collapse (mean) salary_governor_gbp , by(decade connected)

drop if decade==.
twoway  (connected salary_governor_gbp decade if connected==1) (connected salary_governor_gbp decade if connected==0), scheme(s2mono) graphregion(color(white)) xtitle("Decade") legend(label(1 "Connected") label(2 "Unconnected")) ytitle("Average salary (GBP)") xlabel(1850(10)1960) ylabel(2000(1000)7000)

*** FIGURE 4: SALARY GAP AND THE REMOVAL OF PATRONAGE (WARREN FISHER REFORM 1930) ***

quietly use "analysis.dta", replace

tab quinquennial, gen(d_quinquennial)
forvalues i=1/22 {
gen d_quin_connected`i'=connected*d_quinquennial`i'
}

reghdfe log_salary_governor_gbp no_colonies connected_year1930 d_quin_connected1 d_quin_connected2 d_quin_connected3 d_quin_connected4 d_quin_connected5 d_quin_connected6 d_quin_connected7 d_quin_connected8 d_quin_connected9 d_quin_connected10 d_quin_connected11 d_quin_connected12 d_quin_connected13 d_quin_connected14 d_quin_connected15 d_quin_connected16 d_quin_connected17 d_quin_connected18 d_quin_connected19 d_quin_connected20 d_quin_connected21 if year>=1854, absorb(aid year duration) vce(cluster bilateral)

regsave, ci level(90)

drop if var=="no_colonies" | var=="connected_year1930"

gen n=[_n]
gen year=1854 if n==1
replace year=1860 if n==2
replace year=1865 if n==3
replace year=1870 if n==4
replace year=1875 if n==5
replace year=1880 if n==6
replace year=1885 if n==7
replace year=1890 if n==8
replace year=1895 if n==9
replace year=1900 if n==10
replace year=1905 if n==11
replace year=1910 if n==12
replace year=1915 if n==13
replace year=1920 if n==14
replace year=1925 if n==15
replace year=1930 if n==16
replace year=1935 if n==17
replace year=1940 if n==18
replace year=1945 if n==19
replace year=1950 if n==20
replace year=1955 if n==21

twoway (scatter coef year if year>=1905 & year<=1955) (rcap ci_lower ci_upper year  if year>=1905 & year<=1955), xline(1930) ylabel(-0.4(0.2)0.6) scheme(s2mono) graphregion(color(white)) legend(off) xtitle("Year") ytitle("Log salary gap connected vs. unconnected") xlabel(1905(5)1955)

*** FIGURE 5: PERFORMANCE GAP AND CONNECTEDNESS - EVENT STUDY ***

quietly use "analysis.dta", replace
xtset state_aid year
reghdfe log_rev_total_gbp F2.connected F.connected connected L.connected L2.connected no_colonies, absorb(state_aid year duration) vce(cluster bilateral)
regsave, ci level(90)
gen t=[_n]-3
drop if t==3
gen type="rev"
drop if var=="no_colonies"
save "event_rev.dta", replace

quietly use "analysis.dta", replace
xtset state_aid year
reghdfe log_exp_total_gbp F2.connected F.connected connected L.connected L2.connected no_colonies, absorb(state_aid year duration) vce(cluster bilateral)
regsave, ci level(90)
gen t=[_n]-2.9
drop if var=="no_colonies"
gen type="exp"
save "event_exp.dta", replace

append using "event_rev.dta"

twoway (scatter coef t if type=="rev")  (scatter coef t if type=="exp")  (rcap ci_lower ci_upper t), scheme(s2mono) graphregion(color(white)) xtitle("Leads and lags") ytitle("log difference connected vs. unconnected") legend(label(1 "Revenue") label(2 "Expenditure") label(3 "") label(4 "")) ylabel(-0.2(0.1)0.2)

erase "event_rev.dta" 
erase "event_exp.dta" 
