*Code for all graphs and tables in final ROP paper due Dec 5th, 2025*

*Create a table of frequencies of minority (not white or unknown) vs non-minority candidates by year*
*This will show how minorities have fared in party nominations over time*
gen min_status_candidate = .
replace min_status_candidate = 0 if ethnicity_recoded == 11
replace min_status_candidate = 1 if ethnicity_recoded != 11 & ethnicity_recoded != 13

tab min_status_candidate year, row

*Create a table of frequencies of minority vs non-minority winners by year*
gen min_status_incumbent = .
replace min_status_incumbent = 0 if ethnicity_recoded == 11 & won_final == 1
replace min_status_incumbent = 1 if ethnicity_recoded != 11 & ethnicity_recoded != 13 & won_final == 1

tab min_status_incumbent year, row

*Create a bar graph of the total percentage of every ethnicity across the 2004-2025 elections*
graph hbar, over(ethnicity_recoded, sort(1) descending ///
        relabel(1 "Black" 2 "Arab" 3 "Chinese" 4 "Filipino" 5 "Indigenous" 6 "Japanese" 7 "Korean" 8 "Latin American" 9 "Other Visible Minorities" 10 "South Asian" 11 "Southeast Asian"  12 "White" 13 "West Asian" 14 "Unknown" 15 "Other MENA")) ///
    bar(1, fcolor(cranberry) lcolor(cranberry)) ///
    graphregion(color(eggshell)) ///
    plotregion(color(white)) ///
    title("Ethnicity of Candidates From the 2004-2025 Elections", size(medium)) ///
    ytitle("Percentage") ///
    blabel(bar, format(%4.1f))

*Regression*
regress won_final ib11. ethnicity_recoded
regress winner_vote_perc ib11. ethnicity_recoded

*Create a dummy variable for minority vs nonminority (all non-white ethnicities)*
gen minority_status = .
replace minority_status = 0 if ethnicity_recoded == 11
replace minority_status = 1 if ethnicity_recoded != 11

graph twoway connected minority_status year

scatter winner_vote_perc year

sort year

gen minority_win_status = .
replace minority_win_status = 1 if ethnicity_recoded != 11 & won_final == 1
replace minority_win_status = 0 if ethnicity_recoded != 11 & won_final == 0

gen nonminority_win_status = .
replace nonminority_win_status = 1 if ethnicity_recoded != 11 & won_final == 1
replace nonminority_win_status = 0 if ethnicity_recoded != 11 & won_final == 0

graph hbar, over(minority_win_status, sort(1) descending) ///
title("Win Status of Minorities in the 2004-2025 Elections")  ///
    ytitle("Win Status") ///
    blabel(bar, format(%4.1f))
	
gen minority_win_rate = .
replace minority_win_rate = divide if ethnicity_recoded != 11 / by if ethnicity != 11 and won_final == 1

gen minority_win_rate = ethnicity_recoded != 11 / ethnicity_recoded != 11 & won_final == 1
replace minority_win_rate = sum(ethnicity_recoded != 11) / sum(ethnicity_recoded != 11 & won_final == 1)

gen nonminority_win_rate = .
replace nonminority_win_rate = sum(ethnicity_recoded == 11) / sum(ethnicity_recoded == 11 & won_final == 1)

preserve
collapse (mean) minority_win_status nonminority_win_status, by(year)


twoway ///
    (line minority_win_rate year, lwidth(medthick) lcolor(blue)) ///
    (line nonminority_win_rate year, lwidth(medthick) lcolor(red)) ///
    , ///
    ylabel(0(.1).6, angle(horizontal)) ///
    xtitle("Election Year") ///
    ytitle("Win Rate") ///
    legend(order(1 "Minority candidates" 2 "Non-minority candidates")) 
	
line minority_win_status year

line ethnicity_recoded year if ethnicity_recoded == 0, sort lcolor(blue)

*another graph idea: line graph showing how 


*make a cross tab of minority_status and won_final*
gen minority_status = .
replace minority_status = 1 if ethnicity_recoded != 11
replace minority_status = 0 if ethnicity_recoded == 11

tab minority_status won_final
*only sums. use something else!*

tabstat won_final, by(minority_status) stats(mean)

*Generate minority/ nonminority minoritystatus variable = % of total candidates that are minority, by year)

gen count_candidates = _N
replace count_candidates = count(lastname)

egen count_candidates = count(lastname) if year == 2004 || count(lastname) if year == 2006 || count(lastname) if year == 2008 

sort year
by year: egen count_candidates = count(lastname)

egen count_candidates_2004 = count(lastname) if year == 2004 
egen count_candidates_2006 = count(lastname) if year == 2006
egen count_candidates_2008 = count(lastname) if year == 2008 
egen count_candidates_2011 = count(lastname) if year == 2011 
egen count_candidates_2015 = count(lastname) if year == 2015 
egen count_candidates_2019 = count(lastname) if year == 2019 
egen count_candidates_2021 = count(lastname) if year == 2021 
egen count_candidates_2025 = count(lastname) if year == 2025 

gen minoritystatus = .

replace minoritystatus = count_candidates_2004 if ethnicity_recoded != 11 / count_candidates_2004
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2006
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2008
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2011
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2015
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2019
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2021
replace minoritystatus = divide if ethnicity_recoded != 11 / by if ethnicity != 11 & year = 2025

egen count_minorities_2004 = count(lastname) if year == 2004 & ethnicity_recoded != 11
egen count_nonminority_2004 = count(lastname) if year == 2004 & ethnicity_recoded == 11
egen count_candidates_2004 = count(lastname) if year == 2004

gen minority_perc_2004 = count_minorities_2004 / count_candidates_2004

egen count_minorities_2006 = count(lastname) if year == 2006 & ethnicity_recoded != 11
egen count_nonminority_2006 = count(lastname) if year == 2006 & ethnicity_recoded == 11
egen count_candidates_2006 = count(lastname) if year == 2006

gen minority_perc_2006 = count_minorities_2006 / count_candidates_2006

egen count_minorities_2008 = count(lastname) if year == 2008 & ethnicity_recoded != 11
egen count_nonminority_2008 = count(lastname) if year == 2008 & ethnicity_recoded == 11
egen count_candidates_2008 = count(lastname) if year == 2008

gen minority_perc_2008 = count_minorities_2008 / count_candidates_2008

twoway (line minority_perc_2004 year) || (line minority_perc_2006 year) || (line minority_perc_2008 year), legend(label(1 "Line 1") label(2 "Line 2") label(3 "Line 3"))
*Not working because each variable is one single point!)

use "\\tsclient\Drives\Dropbox\Friendly Fire Replication\Candidate data\Edited candidate datasets\2004-2025 Candidates combined 2.dta"

egen count_candidates = count(lastname)


gen minority_count = count_candidates if ethnicity_recoded != 11 / count_candidates


egen count_candidates_2004 = count(lastname) if year == 2004

gen minority_perc_2004 = count_candidates_2004 if ethnicity_recoded != 11 / count_candidates_2004

gen minority_perc = count_candidates_2004 if ethnicity_recoded != 11 / count_candidates_2004

*chatgpt code*
* 1. Indicator for minority (ethnicity_recoded != 11)
gen is_minority = ethnicity_recoded != 11 & ethnicity_recoded != 13 if !missing(ethnicity_recoded)

* 2. Count minorities by year
sort year
by year: egen minorities_year = total(is_minority)

* 3. Count total candidates by year
by year: egen total_year = count(ethnicity_recoded)

* 4. Compute year-specific percentage
gen minority_perc = minorities_year / total_year

*same for nonminority*
gen not_minority = ethnicity_recoded == 11 if !missing(ethnicity_recoded)
by year: egen nonminorities_year = total(not_minority)
gen nonminority_perc = nonminorities_year / total_year


*Graph the percentage of total candidates that are minorities vs nonminorities by year*
twoway (line minority_perc year, lcolor(red)) || (line nonminority_perc year, lcolor(blue)), legend(label(1 "Minority Percentage") label(2 "Nonminority Percentage")) ///
			 xlabel(2004 2006 2008 2011 2015 2019 2021 2025) ///
			 graphregion(color(eggshell)) ///
			plotregion(color(white)) ///
			title("Percentage of Minority vs. Non-Minority Candidates") ///
			ytitle("Percentage of Candidates") ///
			xtitle("Year")


tab minority_perc nonminority_perc
tab is_minority year, row
tab is_minority year, col

tab is_minority year, col matcell(freq) matrow(rowlbl) matcol(collbl)

putexcel set "minority_vs_nonminority_table.xlsx", replace

* Write column labels
putexcel A1 = "is_minority"
putexcel B1 = collbl

* Write row labels  
putexcel A2 = rowlbl

* Write the frequency matrix  
putexcel B2 = matrix(freq)

ssc install tabout
tabout is_minority year using table_output.xlsx, ///
    cells(freq colpct) ///
    replace


*Now see the win rates of elections*
*minority_winner is 1 if the individual won and is not white, or unknown ethnicity*
gen minority_winner = ethnicity_recoded != 11 & ethnicity_recoded != 13 if !missing(ethnicity_recoded) & won_final ==1 

*Total up the number of minority winners*
by year: egen minority_winners_by_year = total(minority_winner)

*Generate total_winners to be total minority candidates running that year* 
gen total_min = ethnicity_recoded != 11 & ethnicity_recoded != 13 if !missing(ethnicity_recoded)
by year: egen total_min_year = total(total_min)

*Generate minority win rate as the number of minority winners / by number of minority candidates that year*
gen minority_winrate = minority_winners_by_year / total_min_year

*Generate number of non-minority winners*
gen nonminority_winner = ethnicity_recoded == 11 if !missing(ethnicity_recoded) & won_final ==1 
by year: egen nonminority_winners_by_year = total(nonminority_winner)

*Generate total_nonmin as number of non-minorities running by year, and add them up* 
gen total_nonmin = ethnicity_recoded == 11 if !missing(ethnicity_recoded)
by year: egen total_nonmin_year = total(total_nonmin)

*Generate nonminority win rate as the number of nonminority winners / by number of nonminority candidates that year*
gen nonminority_winrate = nonminority_winners_by_year / total_nonmin_year


twoway (line minority_winrate year, lcolor(red)) || (line nonminority_winrate year, lcolor(blue)), legend(label(1 "Minority Win Rate") label(2 "Nonminority Win Rate")) ///
			 xlabel(2004 2006 2008 2011 2015 2019 2021 2025) ///
			graphregion(color(eggshell)) ///
			plotregion(color(white)) ///
			title("Win Rates of Minority vs. Non-Minority Candidates") ///
			ytitle("Win Rate") ///
			xtitle("Year")

*Generate a line graph for all 14 ethnicities' win rates over time*
sort year
gen black_winners = ethnicity_recoded == 0 if !missing(ethnicity_recoded) & won_final == 1
by year: egen black_win_count = total(black_winners)
gen black_candidates = ethnicity_recoded == 0
by year: egen black_candidate_total = total(black_candidates)
gen black_win_rate = black_win_count / black_candidate_total

gen arab_winners = ethnicity_recoded == 1 if !missing(ethnicity_recoded) & won_final == 1
by year: egen arab_win_count = total(arab_winners)
gen arab_candidates = ethnicity_recoded == 1
by year: egen arab_candidate_total = total(arab_candidates)
gen arab_win_rate = arab_win_count / arab_candidate_total

gen chinese_winners = ethnicity_recoded == 2 if !missing(ethnicity_recoded) & won_final == 1
by year: egen chinese_win_count = total(chinese_winners)
gen chinese_candidates = ethnicity_recoded == 2
by year: egen chinese_candidate_total = total(chinese_candidates)
gen chinese_win_rate = chinese_win_count / chinese_candidate_total

gen filipino_winners = ethnicity_recoded == 3 if !missing(ethnicity_recoded) & won_final == 1
by year: egen filipino_win_count = total(filipino_winners)
gen filipino_candidates = ethnicity_recoded == 3
by year: egen filipino_candidate_total = total(filipino_candidates)
gen filipino_win_rate = filipino_win_count / filipino_candidate_total

gen indigenous_winners = ethnicity_recoded == 4 if !missing(ethnicity_recoded) & won_final == 1
by year: egen indigenous_win_count = total(indigenous_winners)
gen indigenous_candidates = ethnicity_recoded == 4
by year: egen indigenous_candidate_total = total(indigenous_candidates)
gen indigenous_win_rate = indigenous_win_count / indigenous_candidate_total

gen japanese_winners = ethnicity_recoded == 5 if !missing(ethnicity_recoded) & won_final == 1
by year: egen japanese_win_count = total(japanese_winners)
gen japanese_candidates = ethnicity_recoded == 5
by year: egen japanese_candidate_total = total(japanese_candidates)
gen japanese_win_rate = japanese_win_count / japanese_candidate_total

gen korean_winners = ethnicity_recoded == 6 if !missing(ethnicity_recoded) & won_final == 1
by year: egen korean_win_count = total(korean_winners)
gen korean_candidates = ethnicity_recoded == 6
by year: egen korean_candidate_total = total(korean_candidates)
gen korean_win_rate = korean_win_count / korean_candidate_total

gen latinamerican_winners = ethnicity_recoded == 7 if !missing(ethnicity_recoded) & won_final == 1
by year: egen latinamerican_win_count = total(latinamerican_winners)
gen latinamerican_candidates = ethnicity_recoded == 7
by year: egen latinamerican_candidate_total = total(latinamerican_candidates)
gen latinamerican_win_rate = latinamerican_win_count / latinamerican_candidate_total

gen othervismin_winners = ethnicity_recoded == 8 if !missing(ethnicity_recoded) & won_final == 1
by year: egen othervismin_win_count = total(othervismin_winners)
gen othervismin_candidates = ethnicity_recoded == 8
by year: egen othervismin_candidate_total = total(othervismin_candidates)
gen othervismin_win_rate = othervismin_win_count / othervismin_candidate_total

gen southasian_winners = ethnicity_recoded == 9 if !missing(ethnicity_recoded) & won_final == 1
by year: egen southasian_win_count = total(southasian_winners)
gen southasian_candidates = ethnicity_recoded == 9
by year: egen southasian_candidate_total = total(southasian_candidates)
gen southasian_win_rate = southasian_win_count / southasian_candidate_total

gen southeastasian_winners = ethnicity_recoded == 10 if !missing(ethnicity_recoded) & won_final == 1
by year: egen southeastasian_win_count = total(southeastasian_winners)
gen southeastasian_candidates = ethnicity_recoded == 10
by year: egen southeastasian_candidate_total = total(southeastasian_candidates)
gen southeastasian_win_rate = southeastasian_win_count / southeastasian_candidate_total

gen white_winners = ethnicity_recoded == 11 if !missing(ethnicity_recoded) & won_final == 1
by year: egen white_win_count = total(white_winners)
gen white_candidates = ethnicity_recoded == 11
by year: egen white_candidate_total = total(white_candidates)
gen white_win_rate = white_win_count / white_candidate_total

gen westasian_winners = ethnicity_recoded == 12 if !missing(ethnicity_recoded) & won_final == 1
by year: egen westasian_win_count = total(westasian_winners)
gen westasian_candidates = ethnicity_recoded == 12
by year: egen westasian_candidate_total = total(westasian_candidates)
gen westasian_win_rate = westasian_win_count / westasian_candidate_total

twoway (line black_win_rate year) || (line arab_win_rate year) || (line chinese_win_rate year) || (line filipio_win_rate year) || (line indigenous_win_rate year), legend(label(1 "Black Win Rate") label(2 "Arab Win Rate")) lcolour(red blue green orange purple)

			 
*Code year-specific percentage of winning minorities belonging to each party*
gen is_minority = ethnicity_recoded != 11 & ethnicity_recoded != 13 if !missing(ethnicity_recoded)

gen min_cpc_winner = is_minority == 1 & party == 0 & won_final == 1
bysort year: egen total_min_cpc_winner = total(min_cpc_winner)
gen cpc_min = is_minority == 1 & party == 0
bysort year: egen total_cpc_min = total(cpc_min)
gen min_winrate_cpc = 100 *  total_min_cpc_winner/ total_cpc_min

gen min_lpc_winner = is_minority == 1 & party == 1 & won_final == 1
bysort year: egen total_min_lpc_winner = total(min_lpc_winner)
gen lpc_min = is_minority == 1 & party == 0
bysort year: egen total_lpc_min = total(lpc_min)
gen min_winrate_lpc = 100 *  total_min_lpc_winner/ total_lpc_min

gen min_ndp_winner = is_minority == 1 & party == 2 & won_final == 1
bysort year: egen total_min_ndp_winner = total(min_ndp_winner)
gen ndp_min = is_minority == 1 & party == 0
bysort year: egen total_ndp_min = total(lpc_min)
gen min_winrate_ndp = 100 *  total_min_ndp_winner/ total_ndp_min

gen min_bq_winner = is_minority == 1 & party == 3 & won_final == 1
bysort year: egen total_min_bq_winner = total(min_bq_winner)
gen bq_min = is_minority == 1 & party == 0
bysort year: egen total_bq_min = total(bq_min)
gen min_winrate_bq = 100 *  total_min_bq_winner/ total_bq_min

gen min_gpc_winner = is_minority == 1 & party == 4 & won_final == 1
bysort year: egen total_min_gpc_winner = total(min_gpc_winner)
gen gpc_min = is_minority == 1 & party == 0
bysort year: egen total_gpc_min = total(gpc_min)
gen min_winrate_gpc = 100 *  total_min_gpc_winner/ total_gpc_min

gen min_ppc_winner = is_minority == 1 & party == 5 & won_final == 1
bysort year: egen total_min_ppc_winner = total(min_ppc_winner)
gen ppc_min = is_minority == 1 & party == 0
bysort year: egen total_ppc_min = total(ppc_min)
gen min_winrate_ppc = 100 *  total_min_ppc_winner/ total_ppc_min

*verifying that the rates add up to 100%. They do.*
gen minority_sum = min_winrate_cpc + min_winrate_lpc + min_winrate_ndp + min_winrate_bq + min_winrate_gpc + min_winrate_ppc

graph twoway (line min_winrate_cpc year, lcolor(navy) lwidth(medthick)) ///
             (line min_winrate_lpc year, lcolor(red) lwidth(medthick)) ///
             (line min_winrate_ndp year, lcolor(orange) lwidth(medthick)) ///
             (line min_winrate_bq year, lcolor(ebblue) lwidth(medthick)) ///
			 (line min_winrate_gpc year, lcolor(green) lwidth(medthick)) ///
			 (line min_winrate_ppc year, lcolor(purple) lwidth(medthick)) ///
             , legend(order(1 "Conservative" 2 "Liberal" 3 "NDP" 4 "BQ" 5 "Green" 6 "PPC")) ///
			 graphregion(color(eggshell)) ///
			plotregion(color(white)) ///
			 title("Minority Win Rate by Party") ///
			 ytitle("Win Rate") ///
			 xtitle("Year") ///
			 xlabel(2004 2006 2008 2011 2015 2019 2021 2025)
			 
*instead make a bar graph for minority win rate by party*


graph hbar, over(ethnicity_recoded, sort(1) descending ///
        relabel(1 "Black" 2 "Arab" 3 "Chinese" 4 "Filipino" 5 "Indigenous" 6 "Japanese" 7 "Korean" 8 "Latin American" 9 "Other Visible Minorities" 10 "South Asian" 11 "Southeast Asian"  12 "White" 13 "West Asia" 14 "Unknown" 15 "Other MENA")) ///
    bar(1, fcolor(cranberry) lcolor(cranberry)) ///
    graphregion(color(eggshell)) ///
    plotregion(color(white)) ///
    title("Ethnicity of Candidates From the 2004-2025 Elections", size(medium)) ///
    ytitle("Percentage") ///
    blabel(bar, format(%4.1f))


gen minority_count = ethnicity_recoded != 11 if !missing(ethnicity_recoded)
by year: egen minority_by_year = total(minority_winner)
by year: egen total_winners = count(ethnicity_recoded) if won_final == 1
gen minority_winner_perc = minority_winners_by_year / total_winners


sort ethnicity_recoded

by year: egen minority_count = total(minority_candidates)
gen black_win_rate = black_count / total_year
			 
graph twoway (line black_win_rate year, lcolor(red)) ///
             (line arab_win_rate year, lcolor(blue)) ///
             (line chinese_win_rate year, lcolor(green)) ///
             (line filipino_win_rate year, lcolor(purple)) ///
			 (line indigenous_win_rate year, lcolor(orange)) ///
			 (line japanese_win_rate year, lcolor(navy)) ///
			 (line korean_win_rate year, lcolor (emerald)) ///
			 (line latinamerican_win_rate year, lcolor (lavender)) ///
			 (line othervismin_win_rate year, lcolor (cyan)) ///
			 (line southasian_win_rate year, lcolor (cranberry)) ///
			 (line southeastasian_win_rate year, lcolor (pink)) ///
			 (line westasian_win_rate year, lcolor (brown)) ///
			 (line white_win_rate year, lcolor (yellow)) ///
             , legend(label(1 "Black Win Rate") label (2 "Arab Win Rate") label(3 "Chinese Win Rate") label(4 "Filipino Win Rate") label(5 "Indigenous Win Rate") label(6 "Japanese Win Rate") label(7 "Korean Win Rate") label(8 "Latin American Win Rate") label(9 "Other Visible Minorities Win Rate") label(10 "South Asian Win Rate") label(11 "Southeast Asian Win Rate") label (12 "West Asian Win Rate") label(13 "White Win Rate")) ///
			graphregion(color(eggshell)) ///
			plotregion(color(white)) ///
			title("Canadian Election Win Rates by Ethnicity") ///
			 ytitle("Win Rate") ///
			 xlabel(2004 2006 2008 2011 2015 2019 2021 2025) ///
			 ylabel(0(0.1)1)
			 
			 
*Minority vs Nonminority Table*

tab minority_perc nonminority_perc

tab minority_status won_final

gen count_candidates = _N
sort year
by year: egen count_candidates = count(ethnicity_recoded)

tab is_minority year, row

        gen dummy = 1
        collapse (sum) freq = dummy, by(is_minority year)


	*generate a dummy variable for minority candidates. 0 if white, 1 if minority*
gen minority = (ethnicity_recoded != 11)
label define minority_label 0 "Non-minority" 1 "Minority"
label values minority minority_label

* Total number of candidates by group
table minority won_final, c(count ethnicity_recoded)

* Collapse data for easy math
collapse (count) total=ethnicity_recoded, by(minority won_final)

* Create summary statistics
bys won_final: egen total_by_won = total(total)
gen percent = 100 * total / total_by_won

putexcel set "\\tsclient\Drives\Dropbox\Friendly Fire Replication\Candidate data\Edited candidate datasets\minority_winstatus_crosstab.xlsx", replace


		 

*Calculate the percentage of minority candidates (out of all minority candidates) by party over time*
gen is_min_cpc = party == 0 & is_minority == 1
by year: egen count_min_cpc = total(is_min_cpc)
by year: gen min_perc_cpc = count_min_cpc/ minorities_year

gen is_min_lpc = party == 1 & is_minority == 1
by year: egen count_min_lpc = total(is_min_lpc)
by year: gen min_perc_lpc = count_min_lpc/ minorities_year

gen is_min_ndp = party == 2 & is_minority == 1
by year: egen count_min_ndp = total(is_min_ndp)
by year: gen min_perc_ndp = count_min_ndp/ minorities_year

gen is_min_bq = party == 3 & is_minority == 1
by year: egen count_min_bq = total(is_min_bq)
by year: gen min_perc_bq = count_min_bq/ minorities_year

gen is_min_gpc = party == 4 & is_minority == 1
by year: egen count_min_gpc = total(is_min_gpc)
by year: gen min_perc_gpc = count_min_gpc/ minorities_year

gen is_min_ppc = party == 5 & is_minority == 1
by year: egen count_min_ppc = total(is_min_ppc)
by year: gen min_perc_ppc = count_min_ppc/ minorities_year

*Make sure percentages for each party all add up to 100%. They do.*
gen total_perc_parties = min_perc_cpc + min_perc_lpc + min_perc_ndp + min_perc_bq + min_perc_gpc + min_perc_ppc

*Graph the percentage of minority candidates by party over time*

graph twoway (line min_perc_cpc year, lcolor(navy) lwidth(medthick)) ///
             (line min_perc_lpc year, lcolor(red) lwidth(medthick)) ///
             (line min_perc_ndp year, lcolor(orange) lwidth(medthick)) ///
             (line min_perc_bq year, lcolor(ebblue) lwidth(medthick)) ///
			 (line min_perc_gpc year, lcolor(green) lwidth(medthick)) ///
			 (line min_perc_ppc year, lcolor(purple) lwidth(medthick)) ///
             , legend(order(1 "Conservative" 2 "Liberal" 3 "NDP" 4 "BQ" 5 "Green" 6 "PPC")) ///
			 graphregion(color(eggshell)) ///
			plotregion(color(white)) ///
			 title("Share of Total Minority Candidates by Party") ///
			 ytitle("Percentage") ///
			 xlabel(2004 2006 2008 2011 2015 2019 2021 2025)
			
*Instead make a bar graph of average number of minorities per party from 2004-2025*
graph bar is_minority, over(party, sort(1) descending gap(*0.5)) ///
    bar(1, color(orange)) ///
    bar(2, color(red)) ///
    bar(3, color(blue)) ///
    bar(4, color(green)) ///
    bar(5, color(purple)) ///
    bar(6, color(cyan)) ///
    graphregion(color(eggshell)) ///
    plotregion(color(white)) ///
    title("Average Share of Minority Candidates by Party, 2004–2025") ///
    ytitle("Share of Minority Candidates") ///
    b1title("Party") ///
    blabel(bar, format(%5.2f))


margins black_candidates arab_candidates chinese_candidates filipino_candidates indigenous_candidates japanese_candidates korean_candidates latin_american_candidates south_asian_candidates southeast_asian_candidates westasian_candidates
marginsplot ///
	lcolor(red) ///
	graphregion(color(eggshell)) ///
	plotregion(color(white))

logit won_final i.black_candidates i.arab_candidates i. chinese_candidates i. filipino_candidates i. indigenous_candidates i. japanese_candidates i. korean_candidates i. latin_american_candidates i.south_asian_candidates i. southeast_asian_candidates i. westasian_candidates ///
      gender incumbent_change party i.province
	  
*Linear regression and graph*

reg won_final ib11.ethnicity_recoded
margins ethnicity_recoded
marginsplot, ///
	plotopts(mcolor(black) lcolor(red)) ///
	ciopts(color(red)) ///
    graphregion(color(eggshell)) ///
    plotregion(color(white)) ///
	title("Predictions of Ethnicity with 95% CIs") ///
	xtitle("ethnicity_recoded") ///
	xlabel(1 "Black" 2 "Arab" 3 "Chinese" 4 "Filipino" 5 "Indigenous" 6 "Japanese" 7 "Korean" 8 "Latin American" 9 "Other Visible Minorities" 10 "South Asian" 11 "Southeast Asian"  12 "White" 13 "West Asian" 14 "Unknown", angle(45))

	
tab ethnicity_recoded

tab1 (ethnicity_recoded) (won_final)

collapse (mean) win_final, by(ethnicity_recoded)
graph bar won_final, over(ethnicity_recoded) ytitle("Win rate")


 graph dot won_final, over(ethnicity_recoded) ///
    ytitle("Win Probability") marker(1, msymbol(circle))

 ssc install estout			 
