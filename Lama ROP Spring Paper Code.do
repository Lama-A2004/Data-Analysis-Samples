************************ ROP FINAL PAPER CODE ************************
// NAME: LAMA AMMANE
// DATE: Friday, April 17, 2026
// TOPIC 1: Does the ethnic makeup of an electoral district affect the vote share of minority candidates?
// TOPIC 2: Does the existence of minority candidates lead to higher numbers of
// same-group minority candidates in the future?
// DV: minority candidate vote share.
// IV: minority population share.

// Install necessary programs.
sysdir set PLUS "C:\MyStataAdo\newado"
cap mkdir "C:\MyStataAdo\newado"
ssc install cleanplots, replace
set scheme cleanplots

sysdir set PLUS "C:\MyStataAdo\newado"
cap mkdir "C:\MyStataAdo\newado"
ssc install reghdfe, replace
which reghdfe

ssc install ftools, replace
ssc install require, replace

sysdir set PLUS "C:\MyStataAdo\newado"
cap mkdir "C:\MyStataAdo\newado"
ssc install coefplot, replace
which coefplot

************************ Part 1: Understand census data and election years ************************
use "\\tsclient\Drives\Dropbox\Friendly Fire Replication\Candidate data\Edited candidate datasets\2004-2025 Candidates combined 2"
tab year census_year

// 2006 census --> 2004, 2006, 2008 elections
// 2011 census --> (2013 rep order) 2011 election
// 2016 census --> 2015 election
// 2021 census --> 2019, 2021 elections

// Drop observations for 2025 since there is no recent census data with the same districts to merge with.
drop if year==2025
drop if missing(firstname)& missing(lastname)

replace minority_candidate = 1 if ethnicity_recoded != 11 & ethnicity_recoded != 13
replace minority_candidate = 0 if ethnicity_recoded == 11
// ensure that minority_candidate is accurate.

egen minpop_num = rowtotal(chinese south_asian black filipino ///
    latin_american southeast_asian arab west_asian korean japanese ///
    Total_Aboriginal_identity_population)
	// Generate a variable to count the total minority population in a district  

gen minpop_perc = minpop_num / population
// Generate the percentage of the population of a riding that is a minority.
	
duplicates report minpop_perc edanum year firstname lastname // did not duplicate across years.

save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\working 2004-2025 dataset - new vars generated.dta", replace
use "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\working 2004-2025 dataset - new vars generated.dta", clear

************************ Part 2: Visualizations ************************

// Recreate the Ie et al. bar graph of number of minority candidates per population share:

bysort edanum year: gen tag_riding = _n == 1
bysort total_mincand: egen ridings_per_mincands = total(tag_riding)
tab total_mincand if tag_riding

// collapse dataset to get minority population averages in 2021 (most recent year to analyze).
preserve
keep if tag_riding & year == 2021

collapse (mean) minpop_perc (count) N=edanum, by(total_mincand)

gen numcan = string(total_mincand) + " [N = " + string(N) + "]"

graph bar minpop_perc, over(numcan, sort(1)) ///
    blabel(bar, format(%9.2f)) ///
	b1title("Number of Minority Candidates in District [N=Number of Districts]") ///
	title("Minority Population and Number of Candidates (2021)", size(large)) ///
	ytitle("Average Minority Population Share")

restore

graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\MinCanBar.png", replace

// Graph minority vs nonminority share of winners over time

gen minority_winner = (won_final == 1 & minority_candidate == 1)
gen nonminority_winner = (won_final == 1 & minority_candidate == 0)

bysort year: egen total_minority_winners = total(minority_winner)
bysort year: egen total_winners = total(won_final == 1)
bysort year: egen total_nonminority_winners = total(nonminority_winner)
// generate variables for the total count of minority and non-minority winners.

bysort year: egen total_mincands = total(minority_candidate == 1)
bysort year: egen total_nonmincands = total(minority_candidate == 0)
// generate variables for the total number of minority and non-minority candidates overall.

bysort year: gen total_candidates_peryear = _N
bysort year: gen perc_min = total_mincands / total_candidates_peryear
bysort year: gen perc_nonmin = total_nonmincands/ total_candidates_peryear
// generate the percentage of minority and non-minority candidates by dividing by the total candidate count (_N).

graph bar perc_nonmin  perc_min, over(year) ///
    blabel(bar, format(%9.2f)) ///
    bar(1, color(blue)) ///
    title("Minority and Non-Minority Win Shares Over Time", size(large)) ///
    ytitle("Percentage of Candidates") ///
	b1title("Year") ///
	legend(label(1 "% of Non-Minority Candidates") label(2 "% of Minority Candidates"))

graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\WinSharesBar.png", replace
	
// Graph minority vs non-minority win rates over time

gen minority_win_rate2 = total_minority_winners / total_mincands
gen nonminority_win_rate2 = total_nonminority_winners / total_nonmincands
// calculate the win rate of minority candidates as minority winners out of minority canidates.
// Repeat for non-minority candidates.

graph bar nonminority_win_rate2 minority_win_rate2, over(year) ///
    blabel(bar, format(%9.2f)) ///
    bar(1, color(blue)) ///
    title("Minority and Nonminority Win Rates", size(large)) ///
    ytitle("Percentage of Winning Candidates") ///
	b1title("Year") ///
	legend(label(1 "% of Non-Minority Wins") label(2 "% of Minority Wins"))
	
graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\WinRateBar.png", replace

// Graph a scatter plot of the independent and dependent variables, with a regression line.
twoway ///
    (scatter votes_over_validb minpop_perc if minority_candidate == 1) ///
    (lfitci votes_over_validb minpop_perc if minority_candidate == 1), ///
    title("Scatter Plot of Minority Population and Vote Share", size(large)) ///
    xtitle("Minority Population Share") ///
    ytitle("Minority Candidate Votes out of Valid Ballots")

graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\Scatter.png", replace

************************ Part 3: Descriptive Statistics ************************

// Calculate the minority populations of each ethnic group, and their representation in Parliament:

label list ethnicity_label

****** Chinese  ******
egen chinese_pop_total = total (chinese) if year == 2021
egen total_overall_population = total(population) if year == 2021
gen chinese_pop_share_total = chinese_pop_total / total_overall_population
tab chinese_pop_share_total
// Chinese-Canadians are 4.5% of the national population.

egen num_chinese_candidates = count(firstname) if ethnicity_recoded == 2 & won_final == 1 & year == 2021
gen chinese_representation = num_chinese_candidates / 338
tab chinese_representation // 1.77% of MPs were Chinese in 2021.

****** Black  ******
egen black_pop_total = total (black) if year == 2021
gen black_pop_share_total = black_pop_total / total_overall_population
tab black_pop_share_total
// Black Canadians are 4.2% of the national population.

egen num_black_mps = count(firstname) if ethnicity_recoded == 0 & year == 2021 & won_final ==1
gen black_representation = num_black_mps / 338
tab black_totmps_2021
tab black_representation // 2.36% of the house of commons is black mps.

****** Arab  ******
egen arab_pop_total = total (arab) if year == 2021
gen arab_pop_share_total = arab_pop_total / total_overall_population
tab arab_pop_share_total
// Arab Canadians are 1.93% of the national population.

egen num_arab_mps = count(firstname) if ethnicity_recoded == 1 & won_final == 1 & year == 2021
gen arab_representation = num_arab_mps / 338
tab arab_representation // 1.77% of MPs were Arab in 2021.%

****** Filipino  ******
egen filipino_pop_total = total (filipino) if year == 2021
gen filipino_pop_share_total = filipino_pop_total / total_overall_population
tab filipino_pop_share_total
// Filipino Canadians are 2.5% of the national population.

replace ethnicity_recoded = 3 if firstname == "Rechie" & lastname == "Valdez" & edanum == 35063 & year == 2021
egen num_filipino_mps = count(firstname) if ethnicity_recoded == 3 & won_final == 1 & year == 2021
gen filipino_representation = num_filipino_mps / 338
tab filipino_representation // 0.29% of MPs were Filipino in 2021.%

****** Indigenous  ******
egen indigenous_pop_total = total (indigenous) if year == 2021
gen indigenous_pop_share_total = indigenous_pop_total / total_overall_population
tab indigenous_pop_share_total
// Indigenous Canadians are 4.79% of the national population.

egen num_indigenous_mps = count(firstname) if ethnicity_recoded == 4 & won_final == 1 & year == 2021
gen indigenous_representation = num_indigenous_mps / 338
tab indigenous_representation // 2.9% of MPs were Indigenous in 2021.%

****** Japanese  ******
egen japanese_pop_total = total (japanese) if year == 2021
gen japanese_pop_share_total = japanese_pop_total / total_overall_population
tab japanese_pop_share_total
// Japanese Canadians are 0.026% of the national population.

egen num_japanese_mps = count(firstname) if ethnicity_recoded == 5 & won_final == 1 & year == 2021
gen japanese_representation = num_japanese_mps / 338
tab japanese_representation // 0%.

****** Korean  ******
egen korean_pop_total = total (korean) if year == 2021
gen korean_pop_share_total = korean_pop_total / total_overall_population
tab korean_pop_share_total
// Korean Canadians are 0.057% of the national population.

egen num_korean_mps = count(firstname) if ethnicity_recoded == 6 & won_final == 1 & year == 2021
gen korean_representation = num_korean_mps / 338
tab korean_representation // 0%.

****** Latin American ******
egen latin_pop_total = total (latin_american) if year == 2021
gen latin_pop_share_total = latin_pop_total / total_overall_population
tab latin_pop_share_total
// Latin American Canadians are 1.58% of the national population.

egen num_latin_mps = count(firstname) if ethnicity_recoded == 7 & won_final == 1 & year == 2021
gen latin_representation = num_latin_mps / 338
tab latin_representation // 0.59%

****** South Asian ******
egen sa_pop_total = total (south_asian) if year == 2021
gen sa_pop_share_total = sa_pop_total / total_overall_population
tab sa_pop_share_total
// South Asian Canadians are 6.5% of the national population.

egen num_sa_mps = count(firstname) if ethnicity_recoded == 9 & won_final == 1 & year == 2021
gen sa_representation = num_sa_mps / 338
tab sa_representation // 6.8%

****** South-East Asian ******
egen sea_pop_total = total (southeast_asian) if year == 2021
gen sea_pop_share_total = sea_pop_total / total_overall_population
tab sea_pop_share_total
// Southeast Asian Canadians are 1% of the national population.

egen num_sea_mps = count(firstname) if ethnicity_recoded == 10 & won_final == 1 & year == 2021
gen sea_representation = num_sea_mps / 338
tab sea_representation // 0.29%

****** West Asian ******
egen we_pop_total = total (west_asian) if year == 2021
gen we_pop_share_total = we_pop_total / total_overall_population
tab we_pop_share_total
// West Asian Canadians are 0.94% of the national population.

egen num_we_mps = count(firstname) if ethnicity_recoded == 12 & won_final == 1 & year == 2021
gen we_representation = num_we_mps / 338
tab we_representation // 0.59%

****** White ******
egen num_white_mps = count(firstname) if ethnicity_recoded == 11 & won_final == 1 & year == 2021
gen white_representation = num_white_mps / 338
tab white_representation // 81%

// Generate a lagged variable for the share of same-ethnicity candidates last election for a given candidate.
bysort edanum year ethnicity_recoded: gen eth_count = _N
bysort edanum year: gen total_cand = _N 
bysort edanum year ethnicity_recoded: gen tag_eth = _n == 1 

preserve
keep if tag_eth

keep edanum year ethnicity_recoded eth_count total_cand

rename year prev_year // create a previous year variable in a new collapsed dataset.
tempfile prevdata
save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\prevdata.dta",replace
restore

use "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\prevdata.dta", clear

gen year = .
replace year = 2006 if prev_year == 2004
replace year = 2008 if prev_year == 2006
replace year = 2011 if prev_year == 2008
replace year = 2015 if prev_year == 2011
replace year = 2019 if prev_year == 2015
replace year = 2021 if prev_year == 2019 // generate a year variable that is an election year ahead.

drop prev_year
tempfile shifted
save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\shifted.dta", replace

use "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\working 2004-2025 dataset - new vars generated.dta", clear

merge m:1 edanum year ethnicity_recoded using "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\shifted.dta", gen(merge1) ///
    keepusing(eth_count total_cand)
	// merge the old dataset with the shifted year and candidate dataset. 

gen prev_ethnicity_match = eth_count / total_cand
replace prev_ethnicity_match = 0 if missing(prev_ethnicity_match) & year != 2004
replace prev_ethnicity_match = . if year == 2004 // drop prev_ethnicity_match for 2004 since no data before that.

// after merging, there are values at the bottom with merge 2: using only.
// drop these values because these exist in the shifted.dta file, which do not represent
// candidates in the master dataset (2004-2025 dataset).

tab merge1
drop if merge1 == 2
drop merge1

save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\merged with ethinicity lag var 2004-2021.dta"
// verify that prev_ethnicity_match is indeed a lagged variable for ethnicity.

list prev_ethnicity_match if edanum == 35019 & year == 2019 & party == 2
list ethnicity_recoded gender firstname lastname party if edanum == 35019 & year == 2015
list ethnicity_recoded gender firstname lastname party if edanum == 35019 & year == 2019
// From the perspective of the black 2019 candidate in riding 35019,
// there was 1 previous black candidate in 2015. This is a 1/3 previous ethnicity match,
// and since prev_ethnicity_match = 0.333, it is correct.  


************************ Part 4: 2 Regression Models ************************

****** Regression Model 1: minority population and minority candidate votes ******
use "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\merged with ethinicity lag var 2004-2021.dta", clear

gen minority_vote_share = votes_over_validb if minority_candidate == 1 // generate a minority votes share variable

// gen a number of candidates control since more candidates = lower vote share automatically.
bysort year edanum: gen num_candidates = _N

// Run Regression Model 1
reg minority_vote_share minpop_perc ///
    wonlastyear num_candidates margin_of_win i.party##i.year i.province

putdocx begin
putdocx paragraph
putdocx text ("Regression Model 1 Results")
putdocx table regtbl = etable
putdocx save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\Regression1.docx", replace

// Regression Model 1 Plot
reg minority_vote_share minpop_perc  ///
wonlastyear num_candidates margin_of_win i.party##i.year i.province
coefplot, ci(95) drop(_cons) xline(0) ///
title("Regression Model 1 Coefficient Plot") ///
coeflabels(total_visible_minority = "Total visible minority poplation" ///
votes_over_electors = "Turnout" ///
margin_of_win = "District Competitiveness" ///
incumbent_ethnicity_change = "Incumbent Ethnicity Change") 

graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\Regression1.png", replace

// Regression Model 2: whether a minority candidate runs after role model effects //
reg minority_candidate prev_ethnicity_match minpop_perc ///
wonlastyear num_candidates margin_of_win i.party##i.year i.province

putdocx begin
putdocx paragraph
putdocx text ("Regression Model 2 Results")
putdocx table regtbl2 = etable
putdocx save "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\Regression2.docx", replace

// Coefficient Plot for regression model 2
reg minority_candidate prev_ethnicity_match minpop_perc ///
wonlastyear num_candidates margin_of_win i.party##i.year i.province
coefplot, ci(95) drop(_cons) xline(0) ///
title("Regression Model 2 Coefficient Plot") ///
subtitle("Is a match on prior minority candidate ethnicity" ///
"associated with more minority candidates in the next election?") ///
coeflabels(total_visible_minority = "Total visible minority poplation" ///
votes_over_electors = "turnout" ///
margin_of_win = "District Competitiveness" ///
incumbent_ethnicity_change = "Incumbent Ethnicity Change") 

graph export "\\tsclient\Drives\Dropbox\ROP 2025-2026\Census data\Lama working folder- merging districts and candidate data\Regression2.png", replace
