set more off
set mem 5000m
set matsize 800
cap log close

cd "/Users/ariel/Dropbox/Lshare_USbanking/LeblebWein-Finaldraft/3 replication package"

use state_industry_replication.dta, clear


gen indgroup=1 if industryid==3 /*agriculture*/
replace indgroup=2 if industryid>6 & industryid<11 /*mining*/
replace indgroup=3 if industryid==11 /*construction*/
replace indgroup=4 if (industryid>13 & industryid<36)|industryid==76 /*manuf*/
replace indgroup=5 if industryid>37 & industryid<47 /*transport, etc*/
replace indgroup=6 if industryid==47 | industryid==48 /*wholesale, retail*/
replace indgroup=8 if (industryid>48 & industryid<57)|industryid==77 /*finance (NOTICE IT IS GROUP 8!!) */
replace indgroup=7 if (industryid>56 & industryid<72)|industryid==78 /*services*/

/*
"laborsh_private" is laborshare using only private industries (no govt)
"laborsh_3" is private industries minus ALL finance (**State level data is all labor compensation and GDP excluding government)
*/
gen laborsh_private = comp_private/gsp_private
** Everything but government and finance (including real estate, finance and insurance).
bys state_name year: egen gsp3 = total(indgsp) if industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71
bys state_name year: egen comp3 = total(indempcomp) if industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71
bys state_name year: egen gropsurplus3 = total(indgropsurplus) if industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71
gen laborsh_3 = comp3/gsp3  
gen laborsh_selfemp_scaled = (1 + (selfempl/empl))*laborsh_priv 
** Assumes the same labor share (or wage) for self-employed as payroll employed (headline measure in Elsby et al, which is less extreme because lots of propr. income is capital

**Use industries to create manufacturing dummy
bys state_name indgroup year: egen empstateindgroup = total(indempl)
bys state_name year: egen empstate = total(indempl)
gen indgroupshare_state = empstateindgroup/empstate
gen manufshare_fixed_t = indgroupshare_state if indgroup==4 & year==1977
bys state_name: egen manufshare_fixed = mean(manufshare_fixed_t)
drop manufshare_fixed_t empstateindgroup empstate indgroupshare_state


******************************************************************************************
********** State Level Regressions in this File ******************
******************************************************************************************

ds state_name state_abbrev year industryid industryclassification description indgropsurplus indempcomp indgsp indlshare indgroup, not
collapse (mean) `r(varlist)' (first) state_name, by(state_abbrev year)

drop if state_abbrev=="AK" | state_abbrev=="HI" | state_abbrev=="SD" | state_abbrev=="DE" | state_abbrev=="DC" 
keep if year > 1969 & year < 1997


************** Generate Miscellaneous ****************** 
gen loan2gsp=allloans_fdic/gsp/1000 /*loans data in thousands, gsp data in millions*/
gen loan2gsp_perc = loan2gsp*100
gen lnavgloanyield=ln(avgloanyield)
gen lnherf_deposits=ln(1+herf_deposits)
gen lnherf_assets=ln(1+herf_assets)
gen lnassetreturns=ln(assetreturns)
gen lnunemp=ln(unemp)
gen lnhpi = ln(HPI)
gen kint = kpwall //Paper by Tamura uses "real capital per worker
gen lnkint = ln(kint)
egen stateid = group(state_abbrev)
gen avgloanyield_perc = avgloanyield*100  //This is the loan yield in percent now.
*************************************


************** Table 1: Summary Stats *********
replace gsp_private = gsp_private/1000
replace comp_private = comp_private/1000
label var laborsh_private "Labor share"
label var gsp_private "State GDP (billions \textdollar)"
label var comp_private "Total Compensation (billions \textdollar)"
label var avgloanyield "Avg. loan yield rate"
label var loan2gsp "Credit/GDP"
label var herf_deposits "HHI of deposits"
label var grgsp "GDP growth rate"
label var popgrowth "Pop'l growth rate"
label var avgsalary "Avg salary (\textdollar)"
label var corptax "Corporate tax rate"
label var unionmem "Union memb. (\% of workers)"
//label var lfp "Labor Force Part. (\%)"
label var unemp "Unemployment (\%)"
label var HPI "House Price Index"
gen groupyear = (year > 1982)
label values groupyear era
eststo clear
lab def era 0 "\textbf{1970-1982}" 1 "\textbf{1983-1996}", modify
eststo: estpost tabstat laborsh_private gsp_private comp_private avgloanyield loan2gsp herf_deposits grgsp popgrowth avgsalary corptax unionmem unemp HPI, statistics(mean sd p50) columns(statistics) by(groupyear)



************** Figure 1: Agg Labor Share *********

preserve
collapse (mean) intbanking intbranching, by(state_abbrev year)
egen stateid = group(state_abbrev)
bys year: egen num_states = count(stateid)
bys year: egen adopt_banking = total(intbanking)
bys year: egen adopt_branching = total(intbranching)
gen cum_banking = adopt_banking/num_states
gen cum_branching = adopt_branching/num_states
collapse (mean) cum_banking cum_branching, by(year)
tempfile cdf
save `cdf'
restore
merge m:1 year using `cdf'
drop _merge
preserve
collapse (mean) agg_lshare cum_banking cum_branching, by(year)
label var agg_lshare "Aggregate L-share"
label var cum_banking "% States - Banking"
label var cum_branching "% States - Branching"
graph twoway (line cum_banking cum_branching year, clpattern(solid) yaxis(1)) (connected agg_lshare year, yaxis(2)), xtitle("Year") ytitle("% of States with Policy", axis(1)) ytitle("Aggregate L-share", axis(2)) title("")
restore


************** Figure 2: Dynamic Effects *********

**** Banking Laws (taken from literature) and lags/leads are produced in accompanying files ******
merge 1:1 state_abbrev year using banking_law_indicators.dta
drop if _merge==2
drop _merge
gen zero = 0
gen zero2 = 0
	xi: reghdfe laborsh_private intbanking_yb89 intbanking_yb67 intbanking_yb45 intbanking_yb23 zero intbanking_y0 intbanking_ya1 intbanking_ya23 intbanking_ya45 intbanking_ya67 intbanking_ya89  /*
*/ intbranching_yb89 intbranching_yb67 intbranching_yb45 intbranching_yb23 zero2 intbranching_y0 intbranching_ya1 intbranching_ya23 intbranching_ya45 intbranching_ya67 intbranching_ya89 /*
	*/ lnunemp popgrowth corptax unionmem lnhpi, absorb(year statenum) vce(cluster statenum)
	foreach i in intbanking intbranching {
	label var `i'_yb89 "-8, -9"
	label var `i'_yb67 "-6, -7"
	label var `i'_yb45 "-4, -5"
	label var `i'_yb23 "-2, -3"
	label var `i'_y0 "0"
	label var `i'_ya1 "1"
	label var `i'_ya23 "2, 3"
	label var `i'_ya45 "4, 5"
	label var `i'_ya67 "6, 7"
	label var `i'_ya89 "8, 9"
	}
	label var zero "-1"
	label var zero2 "-1"
	**PANEL A:
	coefplot, vertical keep(zero intbanking_*) yline(0) omitted label ciopts(recast(rcap) lpattern(dash) lwidth(thin)) title("(a) Inter-state Banking") ytitle("Coefficients") xtitle("Years before and after Reform (Drop -1)")
	**PANEL B:
	coefplot, vertical keep(zero2 intbranching_*) yline(0) omitted label ciopts(recast(rcap) lpattern(dash) lwidth(thin)) title("(b) Intra-state Branching") ytitle("Coefficients") xtitle("Years before and after Reform (Drop -1)")
	drop zero*

	

************** Table 2 Regressions *********

*After each regression, the mean of the corresponding dependent variable is displayed
qui xi: reg laborsh_private intbanking intbranching corptax unionmem grgsp popgrowth i.year i.state_abbrev, cluster(state_abbrev)
est table, keep( intbanking intbranching corptax unionmem grgsp popgrowth) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s = e(sample)
qui summ laborsh_private if s==1
display "Average Labor Share (Private) = " r(mean)
qui xi: reg laborsh_private intbanking intbranching corptax unionmem grgsp popgrowth lnunemp lnhpi i.year i.state_abbrev, cluster(state_abbrev)
est table, keep(intbanking intbranching corptax unionmem grgsp popgrowth lnunemp lnhpi) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s2 = e(sample)
qui summ laborsh_private if s2==1
display "Average Labor Share (Private - shortened sample) = " r(mean)
qui xi: reg laborsh_private intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi i.year i.state_abbrev, cluster(state_abbrev)
est table, keep(intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s3 = e(sample)
qui summ laborsh_private if s3==1
display "Average Labor Share (Private - shortened sample) = " r(mean)
qui xi: reg laborsh_3 intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi i.year i.state_abbrev, cluster(state_abbrev)
est table, keep(intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s4 = e(sample)
qui summ laborsh_3 if s4==1
display "Average Labor Share (Ex Finance) = " r(mean)
qui xi: reg laborsh_selfemp_scaled intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi i.year i.state_abbrev, cluster(state_abbrev)
est table, keep(intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s5 = e(sample)
qui summ laborsh_selfemp_scaled if s5==1
display "Average Labor Share (Self Employed) = " r(mean)
qui xi: reg laborsh_private intbanking_gr intbranching_gr corptax unionmem lnunemp popgrowth lnhpi i.year i.state_abbrev, cluster(state_abbrev)
est table, keep(intbanking intbanking_gr intbranching_gr corptax unionmem lnunemp popgrowth lnhpi) b se 
display "R-squared =" e(r2)
display "Observations =" e(N)
gen s6 = e(sample)
qui summ laborsh_private if s6==1
display "Average Labor Share (Private - shortened sample) = " r(mean)

************** Table 3 (IV) Regressions *********

xi: ivreg2 laborsh_private (avgloanyield_perc=intbanking) corptax unionmem lnunemp popgrowth lnhpi i.year i.state_abbrev ,ffirst savefirst cluster(state_abbrev)
xi: ivreg2 laborsh_private (loan2gsp_perc=intbanking) corptax unionmem lnunemp popgrowth lnhpi i.year i.state_abbrev ,ffirst savefirst cluster(state_abbrev)
xi: ivreg2 laborsh_private (lnherf_deposits=intbanking) corptax unionmem lnunemp popgrowth lnhpi i.year i.state_abbrev ,ffirst savefirst cluster(state_abbrev)


************** Table 4 (Interactions/Charact) Regressions *********
gen largefirms = firms6+ firms7 + firms8 +firms9  //number of firms with 100+ employees
gen emplargefirms =  emp6+ emp7+ emp8+ emp9  //number of employees in firms with 100+ employees
gen perclarge = largefirms/totfirms
gen percemplarge = emplargefirms/totemp
gen perclarge77 = largefirms/totfirm if year==1977
gen percemplarge77 = emplargefirms/totemp if year==1977
bys stateid: egen perclarge_fixed = mean(perclarge77) //% of number of firms that have over 50 employees in each state in 1977
bys stateid: egen percemplarge_fixed = mean(percemplarge77) //% of employment in firms that have over 50 employees in each state in 1977

summ manufshare_fixed percemplarge_fixed perclarge_fixed, d
gen manufstate = manufshare_fixed > .21
gen emplargestate = percemplarge_fixed > 0.45
gen firmlargestate = perclarge_fixed > 0.0175
foreach y in manufstate emplargestate firmlargestate {
gen intbank_`y' = intbanking*`y'
}

foreach y in manufstate emplargestate firmlargestate {
xi: reghdfe laborsh_private intbanking intbranching intbank_`y' corptax unionmem popgrowth lnunemp lnhpi ,absorb(year stateid) vce(cluster stateid)
}
xi: reghdfe lnkint intbanking intbranching corptax unionmem popgrowth lnunemp lnhpi ,absorb(year stateid) vce(cluster stateid)
xi: reghdfe lnkint intbanking intbranching intbank_manufstate corptax unionmem popgrowth lnunemp lnhpi ,absorb(year stateid) vce(cluster stateid)




************** Figure 3: CounterFactuals*********

preserve
tsset stateid year
bys year: egen us_gsp = total(gsp_private)
gen gsp_weight = gsp_private/us_gsp
gen gsp_weight_77 = gsp_weight if year==1977
bys state_abbrev: egen gsp_weight_base = max(gsp_weight_77)
**Create counterfactual for growing treatment:
gen intbanking_0 = intbanking_gr
gen intbranching_0 = intbranching_gr
qui xi: reg laborsh_private intbanking_0 intbranching_0 corptax unionmem lnunemp popgrowth lnhpi i.year i.state_fips, cluster(state_abbrev)
predict laborshhat_gr
replace intbanking_0 = 0
replace intbranching_0 = 0
predict laborshcf_gr
drop intbanking_0 intbranching_0
drop if year < 1976 //predict can only start in 1976 due to unemp data
bys year: egen laborshhat_basew_gr = total(laborshhat_gr*gsp_weight_base)
bys year: egen laborshcf_basew_gr = total(laborshcf_gr*gsp_weight_base)
collapse (mean) laborshhat_basew_gr laborshcf_basew_gr , by(year)
label var laborshhat_basew_gr "Lshare Hat"
label var laborshcf_basew_gr "Lshare No Policy"
graph twoway (line laborshhat_basew_gr year) (line laborshcf_basew_gr year, clpattern(dash)), ytitle("Labor Share (Predicted and Counterfactual)") xtitle("Year")
restore

