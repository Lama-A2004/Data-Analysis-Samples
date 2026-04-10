***ECO344H5S Homework Assignment 1***
***Name: Lama Ammane***
***Date: Wednesday, Feb 25, 2026***

log using "\\tsclient\Drives\Downloads\eco344_homework1_log.log", replace

*Open the dataset*
use "\\tsclient\Drives\Downloads\eco344hw1data2026_AMLA.dta"

****Question 1 and 2 - what are the 2 years in the data?****
tab year
*ANS= 2016 and 2018*

****Question 3 - What is the mean of labor income?****
mean incwage
*ANS = 49,126.91*

****Question 4 - How much did the mean of labor income increase between the two years?****
mean incwage if year==2016
mean incwage if year==2018
display 52507.92 - 45822.31
*or*
ttest incwage, by(year)

*ANS= 6,685.61*

****Question 5****
reg incwage age

*ANS= 705.9*

****Question 6 - How much higher is labor income, on average, for those in manufacturing industries than for those in non-manufacturing industries?****
ttest incwage, by(manuf)
*but you have to interpret the sign the opposite way since it takes 0 groups - 1 group*

*ANS is 15,519.17*

****Question 7 - What is the median of non-labor income?****
gen nonlabour_income = inctot - incwage
summarize nonlabour_income, detail
*ANS= 98.5*

****Question 8 - What is the 90th percentile of non-labor income?****
*ANS= 13,455*

****Question 9****
*what percentage of people have a incwage > than nonlabour_income???*

count if incwage > nonlabour_income & !missing(incwage, nonlabour_income)
display 100 * (r(N) / _N)
*or*
gen labourincome_larger = (incwage > nonlabour_income)
egen proportion_labinc_larger = mean(labourincome_larger)
gen percentage_labinc_larger = proportion_labinc_larger *100
summarize percentage_labinc_larger

*ANS= 93.78571*

****Question 10****
*How much higher is labor income, on average, for those who receive any EITC income, over those who receive any welfare income?

summ incwage if eitcred > 0
scalar mean_eitcred = r(mean)

summ incwage if incwelfr > 0
scalar mean_welfr = r(mean)

di "Difference in EITC average income and welfare average income:" mean_eitcred - mean_welfr

*ANS=-980.4417*

****Question 11 - In the final year in your data, how many more hours per week do people with college degrees work than people without college degrees, on average?
mean hrsperwk if collplus == 0
mean hrsperwk if collplus == 1
di 42.4498 - 37.35809

*or*
ttest hrsperwk, by(collplus)

*ANS = 5.0917*

*Written Question 4)*
ttest nonlabour_income, by(sex)

log close
