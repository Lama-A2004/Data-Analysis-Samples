***ECO334 Data Assignment 3***

use "\\tsclient\Drives\Downloads\china_city_data_2011_2016.dta"

describe
gen pct_nonag_pop = nonag_pop / totpop *100


*a)*
reg lib_books_pc internet_users mobile_users totpop pct_nonag_pop gdp_pc

*b)*
reg lib_books_pc internet_users mobile_users totpop pct_nonag_pop gdp_pc i.citycode 

*or*

*Install reghdfe*
sysdir set PLUS "C:\MyStataAdo\newado"
cap mkdir "C:\MyStataAdo\newado"
ssc install reghdfe, replace
which reghdfe

ssc install ftools, replace
ssc install require, replace

reghdfe lib_books_pc internet_users mobile_users totpop pct_nonag_pop gdp_pc, abs(citycode)

*c)*
reghdfe lib_books_pc internet_users mobile_users totpop pct_nonag_pop gdp_pc, abs(citycode year)

*d)*
collapse(sum) totpop nonag_pop gdp mobile_users internet_users unemp_pop, by(province year)

*e)* Merge dataset with labor_protest_data_province_2011_2016.dta
merge 1:1 province year using "\\tsclient\Drives\Downloads\labor_protest_data_province_2011_2016.dta"

*f)*
gen pct_nonag_pop = nonag_pop / totpop *100
gen pct_unemp_pop = unemp_pop/totpop *100
gen gdp_pc = gdp/ totpop

reghdfe total_protest internet_users mobile_users totpop pct_nonag_pop pct_unemp_pop gdp_pc, abs(province year)


*or*
egenprovince_id = group(province)
reg total_protest internet_users mobile_users totpop pct_nonag_pop pct_unemp_pop gdp_pc i.province_id i.year
*coefficients are same*


*h)*
reghdfe mass_protest internet_users mobile_users totpop pct_nonag_pop pct_unemp_pop gdp_pc, abs(province year)



