*Open the first dataset*
use "\\tsclient\Drives\Downloads\media_state_1950.dta"

* (A) - Summarize pct_tvhh*
su pct_tvhh, detail

* (B) - Sort percentage of families owning TVs from highest to lowest*
gsort -pct_tvhh // Sort TV ownership percentage in descending order
count // See the total number of observations
list statenam in 1 // Show the highest TV ownership state
list statenam in 49 // Show the lowest TV ownership state

* (C) - Merge statenam variable with state_socioeconomic_1950 dataset*
merge 1:1 statenam using "\\tsclient\Drives\Downloads\state_socioeconomic_1950.dta"

* (D) - Use a for loop to divide urb950, mtot, black_pop and col_edu by totpop*
foreach var in urb950 mtot black_pop col_edu{
	 generate pct_`var' = `var'/ totpop *100
}

* (E) - Scatter plot
label variable pct_urb950 "Urban population percentage"
twoway scatter pct_tvhh pct_urb950, mlabel(state_abbrev) mlabsize(vsmall)
graph export "\\tsclient\Drives\Downloads\ECO334 A2 Twoway Scatter.png", as(png) replace

* (F) - Regress circ_per1000 on pct_tvhh*
regress circ_per1000 pct_tvhh

* (G) - Regress circ_per1000 on pct_tvhh, but with pct_urb950 as a control *
regress circ_per1000 pct_tvhh pct_urb950

* (H) - Plot the regression coefficients*
	*First set up a new location to install coefplot*
sysdir set PLUS "C:\MyStataAdo\newado"
cap mkdir "C:\MyStataAdo\newado"
ssc install coefplot, replace
which coefplot
	
	*Run regression, plot*
regress circ_per1000 pct_tvhh pct_urb950
coefplot, drop(_cons) ///
title("Regression Coefficients")

	*Export the graph*
graph export "\\tsclient\Drives\Downloads\ECO334 Coefplot.png", as(png) replace


*(I) - Merge with election_state.dta based on statenam*
drop _merges
merge 1:1 statenam using "\\tsclient\Drives\Downloads\election_state.dta"

ssc install outreg2
reg rep52 theater_per1000 circ_per1000 pct_tvhh pct_urb950 pct_mtot pct_black_pop pct_col_edu i.region, robust
outreg2 using "\\tsclient\Drives\Downloads\ECO334outreg.xls"

* (J) - Interpret regression coefficients of theater_per1000 and pct_col_edu*
*See variable descriptions*
describe

