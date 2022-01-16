*Hprod do file*

* Clear memory and set parameters
	clear all
	set more off
	*set trace on

* Clear memory and set parameters

	global input = "C:\Users\arthu\Desktop\STATA"		

* Set directory path, where "$input" refers to the path of the main folder 
	cd "$input/Paper_replication_2022"	

************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Data/hprod.dta", clear

*we define globals

global tables "C:\Users\arthu\Desktop\STATA\Paper_replication_2022\Tables"

global cleaning "persnr year h_job h_housework h_edu h_repair h_act child revb adult h_child h_errand age lrevb ret b60 b63 b65 woman"

sum persnr
codebook persnr
*Initial dataset: 178 435 observations, 26 361 individuals

foreach c of varlist $cleaning {
drop if `c' == .
}

sum persnr
codebook persnr
*After cleaning: 163 870 observations, 25 142 individuals

sort persnr year 

*Creating the residual time use category where the person is likely resting or other uses in a day
egen h_total = rowtotal(h_*)
gen h_other = 24 - h_total

*Dropping values measured with error, with h_total>19
drop if h_total>19
*2,494 observations deleted
sum persnr
codebook persnr
*now 161,376 observations and 24,917 individuals

*Counting the average amount of observation periods for the individuals
bysort persnr: gen frindiv = _N
quietly by persnr:  gen dup = cond(_N==1,0,_n)
preserve
drop if dup>1
sum frindiv
*We observe on average an individual 6.5 times in the period 1991 to 2016
restore

rename ret retired

gen other=0 if h_job!=0 | ret ==1
replace other = 1 if h_job==0 & ret==0
*no observations for other in the dataset, so either working or retired
drop other 

tab woman
*51.51% of individuals in our sample are men (83,131 individuals) and 48.49% are female (78,245 individuals)

tab woman retired, row
*63.19 percent of men are working and 36.81 percent of them are retired in our sample.
*58.36 percent of women are working and 41.64 percent of them are retired in our sample. 
*In our total sample, 60.85 percent of individuals are workign and 39.15 percent are retired.

gen other=0 if h_job!=0 | ret ==1
replace other = 1 if h_job==0 & ret==0
*no observations for other in the dataset, so either working or retired
drop other 

*Creating the home production category
gen h_production = h_housework + h_repair + h_errand

*Check for the relevancy of using a linear model, frequency of individuals with 0 home production by gender
tab woman h_production, row
*10.54 percent of men have no home production and 1.43 percent of women have 0 home production in our sample

*Transforming the weekday time spent categories into weekly categories
qui foreach x of varlist h_*{
    replace `x' = `x' * 5
}



label val woman

*Label new variables created/modified
label variable h_total "Total hours of time use categories per week"
label variable h_other "Other time use per week"
label variable h_production "Home production per week"
label variable h_job "Working hours per week"
label variable h_housework "Hours of housework per week"
label variable h_edu "Hours of education/training per week"
label variable h_repair "Hours of repairing/gardening per week"
label variable h_act "Hours of leisure activities per week"
label variable h_child "Hours of child care per week"
label variable h_errand "Hours of errand per week"
label variable frindiv "Number of times individual observed in data"
label variable dup "Numbering the duplicate observations of an indiviudal"

*Saving cleaned dataset in a tempfile in order to recover it for male analysis
tempfile cldata
save `cldata'
************************* Descriptive Statistics *************************
 
*Some descriptive statistics
global dstat "h_job h_production h_housework h_edu h_repair h_act h_child h_errand age adult child revb"

eststo clear 
eststo:estpost sum $dstat if retired ==1 & woman==0,d
eststo:estpost sum $dstat if retired ==1 & woman==1,d
eststo:estpost sum $dstat if retired ==0 & woman==0,d
eststo:estpost sum $dstat if retired ==0 & woman==1,d
esttab,varwidth(20) cell((mean(fmt(2)))) title("Descriptive statistics")

esttab using "Tables/table_1.tex", label varwidth(20) main(mean) nonumber nostar unstack nonote title("Descriptive statistics") ///
stat(N, labels("Number of individuals")) ///
mtitles("Retired men" "Retired women" "Working men" "Working women") compress nogaps ///
replace

****************************Analysis for Women*****************************
************************* Discontinuity Evidence *************************
*Discontinuity in the probability of retirement at age thresholds
keep if woman==1

gen aged=(1/2)*floor(2*age)
xi:reg retired i.aged
predict ret_hat,xb
gen a=age-60
gen a2=((a^2)/100)*(1-b60)
gen a3=((a^3)/1000)*(1-b60)

gen ca1=a*b60
gen ca2=(a-3)*b63
gen ca3=(a-5)*b65

gen ca12=ca1^2
gen ca22=ca2^2
gen ca32=ca3^2

gen ca13=ca1^3
gen ca23=ca2^3
gen ca33=ca3^3

gen mage=floor(age)
forvalues i=45(1)74{
gen z`i'=mage==`i'
}
drop z45 z60 z63 z65
preserve

xi:reg retired b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add first degree polynomials.

xi:reg retired a ca1 ca2 ca3 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add second degree polynomials.

xi: reg retired a a2 ca1 ca2 ca3 ca12 ca22 ca32 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (95% confidence level) of bin dummies equal to 0 for this specification, so we add third degree polynomials to ages before 60 and after 65.

xi:reg retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 60% confidence level) of bin dummies equal to 0 for this specification, so we use this specification as the flexible age function for the women of our sample

xi:reg retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
predict ret_hat_f, xb

collapse ret_hat ret_hat_f aged, by(age)
replace ret_hat=. if (age-aged)>0.1 | (age-aged)<0.08
twoway line ret_hat_f age if age<60, lcolor(gs2) || line ret_hat_f age if age>=60 & age<63, lcolor(gs2) || line ret_hat_f age if age>=63 & age<65, lcolor(gs2) ||line ret_hat_f age if age>=65, lcolor(gs2) || ///
scatter ret_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("porportion retired") xtitle("Age") legend(off) graphregion(color(white)) title("Women: Retirement by age")

*Discontinuity in the hours of home production per week at age thresholds
restore

xi:reg h_production i.aged
predict home_hat,xb

xi:reg h_production b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add first degree polynomials.

xi: reg h_production a ca1 ca2 ca3 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (95% confidence level) of bin dummies equal to 0 for this specification, so we add second degree polynomials.

xi: reg h_production a a2 ca1 ca2 ca3 ca12 ca22 ca32 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 10% confidence level) of bin dummies equal to 0 for this specification, but we need that the age functions differ at the age thresholds.

xi: reg h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 10% confidence level) of bin dummies equal to 0 for this specification, so we use this specification as the flexible age function the women of our sample.

xi:reg h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
predict home_hat_f, xb

preserve

collapse home_hat home_hat_f aged, by(age)
replace home_hat=. if (age-aged)>0.1 | (age-aged)<0.08
twoway line home_hat_f age if age<60, lcolor(gs2) || line home_hat_f age if age>=60 & age<63, lcolor(gs2) || line home_hat_f age if age>=63 & age<65, lcolor(gs2) ||line home_hat_f age if age>=65, lcolor(gs2) || ///
scatter home_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("hours of home production per week") xtitle("Age") legend(off) graphregion(color(white)) title("Women: Hours of home production by age")

*Check if there are no other discontinuities on other covariates
restore
foreach x of varlist adult child revb lrevb {
   preserve
   xi:reg `x' i.aged
   predict `x'_hat,xb
   
   xi:reg `x' a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
   predict `x'_hat_f, xb
   

   collapse `x'_hat `x'_hat_f aged, by(age)
   replace `x'_hat=. if (age-aged)>0.1 | (age-aged)<0.08
   twoway line `x'_hat_f age if age<60, lcolor(gs2) || line `x'_hat_f age if age>=60 & age<63, lcolor(gs2) || line `x'_hat_f age if age>=63 & age<65, lcolor(gs2) ||line `x'_hat_f age if age>=65, lcolor(gs2) || ///
   scatter `x'_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("Covariate `x'  ") xtitle("Age") legend(off) graphregion(color(white)) title("Identifying assumption check")
   restore
}

*For women, there seems to be no discontinuities for other covariates, thus adding support to our identifying assumption.
************************* Regressions *************************
ssc install ranktest
ssc install ivreg2

*First stage FE regression
xtset persnr year
xtreg retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo retired_FSW

*Reduced Form
xtset persnr year
xtreg h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo h_production_RFW

*FE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo FE_W

*RE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, re vce(cluster persnr)

*Get similar results with RE estimation than found with FE. 

*FE-IV estimation
ivreg2 h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo FE_IV_W

*FD-IV estimation
xtset persnr year
xtreg D.(h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65), vce(cluster persnr)

*Results go in the same direction and are significant but are lower

*FE-IV and FE estimation for specific time uses
foreach x of varlist h_*{
ivreg2 `x' child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo W_`x'
xtset persnr year
xtreg `x' retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
}

*Similar results from FE model than FE-IV

*****************Robustness tests*****************************

*Robustness test without control variables adult and child

*FE estimation
xtset persnr year
xtreg h_production retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*IV estimation
ivreg2 h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)

*We find similar results as to the baseline specification

*Robustness test with individuals aged from 50 to 70 years old

drop if age<50 | age>70

*First stage FE regression
xtset persnr year
xtreg retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*Reduced Form
xtset persnr year
xtreg h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*FE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*IV estimation
ivreg2 h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo FE_IV_robW

*We find similar results as to the baseline specification

****************************Analysis for Men*****************************
************************* Discontinuity Evidence *************************

*Discontinuity in the probability of retirement at age thresholds

*Load the tempfile 
use `cldata',clear
keep if woman==0

gen aged=(1/2)*floor(2*age)
xi:reg retired i.aged
predict ret_hat,xb
gen a=age-60
gen a2=((a^2)/100)*(1-b60)
gen a3=((a^3)/1000)*(1-b60)

gen ca1=a*b60
gen ca2=(a-3)*b63
gen ca3=(a-5)*b65

gen ca12=ca1^2
gen ca22=ca2^2
gen ca32=ca3^2

gen ca13=ca1^3
gen ca23=ca2^3
gen ca33=ca3^3

gen mage=floor(age)
forvalues i=45(1)74{
gen z`i'=mage==`i'
}
drop z45 z60 z63 z65
preserve

xi:reg retired b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add first degree polynomials.

xi:reg retired a ca1 ca2 ca3 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add second degree polynomials.

xi: reg retired a a2 ca1 ca2 ca3 ca12 ca22 ca32 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 30% confidence level) of bin dummies equal to 0 for this specification, but we need that the age functions differ at the age thresholds.

xi:reg retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 5% confidence level) of bin dummies equal to 0 for this specification, so we use this specification as the flexible age function for the women of our sample

xi:reg retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
predict ret_hat_f, xb

collapse ret_hat ret_hat_f aged, by(age)
replace ret_hat=. if (age-aged)>0.1 | (age-aged)<0.08
twoway line ret_hat_f age if age<60, lcolor(gs2) || line ret_hat_f age if age>=60 & age<63, lcolor(gs2) || line ret_hat_f age if age>=63 & age<65, lcolor(gs2) ||line ret_hat_f age if age>=65, lcolor(gs2) || ///
scatter ret_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("porportion retired") xtitle("Age") legend(off) graphregion(color(white)) title("Men: Retirement by age")

*Discontinuity in the hours of home production per week at age thresholds
restore
xi:reg h_production i.aged
predict home_hat,xb

xi:reg h_production b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (99% confidence level) of bin dummies equal to 0 for this specification, so we add first degree polynomials.

xi: reg h_production a ca1 ca2 ca3 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (95% confidence level) of bin dummies equal to 0 for this specification, so we add second degree polynomials.

xi: reg h_production a a2 ca1 ca2 ca3 ca12 ca22 ca32 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We reject the null hypothesis (95% confidence level) of bin dummies equal to 0 for this specification, so we add third degree polynomials

xi: reg h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65 z*
test z46 z47 z48 z49 z50 z51 z52 z53 z54 z55 z56 z57 z58 z59 z61 z62 z64 z66 z67 z68 z69 z70 z71 z72 z73 z74
*We do not reject the null hypothesis (less than 95% confidence level) of bin dummies equal to 0 for this specification, so we use this specification as the flexible age function the women of our sample.

xi:reg h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
predict home_hat_f, xb

preserve

collapse home_hat home_hat_f aged, by(age)
replace home_hat=. if (age-aged)>0.1 | (age-aged)<0.08
twoway line home_hat_f age if age<60, lcolor(gs2) || line home_hat_f age if age>=60 & age<63, lcolor(gs2) || line home_hat_f age if age>=63 & age<65, lcolor(gs2) ||line home_hat_f age if age>=65, lcolor(gs2) || ///
scatter home_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("hours of home production per week") xtitle("Age") legend(off) graphregion(color(white)) title("Men: Hours of home production by age")

*Check if there are no other discontinuities on other covariates
restore
foreach x of varlist adult child revb lrevb {
   preserve
   xi:reg `x' i.aged
   predict `x'_hat,xb
   
   xi:reg `x' a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65
   predict `x'_hat_f, xb
   

   collapse `x'_hat `x'_hat_f aged, by(age)
   replace `x'_hat=. if (age-aged)>0.1 | (age-aged)<0.08
   twoway line `x'_hat_f age if age<60, lcolor(gs2) || line `x'_hat_f age if age>=60 & age<63, lcolor(gs2) || line `x'_hat_f age if age>=63 & age<65, lcolor(gs2) ||line `x'_hat_f age if age>=65, lcolor(gs2) || ///
   scatter `x'_hat age, msize(small) mcolor(black) xlabel(45(1)75) xline(60 63 65) ytitle("Covariate `x'  ") xtitle("Age") legend(off) graphregion(color(white)) title("Identifying assumption check")
   restore
}

*For men, there seems to be no discontinuities for other covariates, thus adding support to our identifying assumption. However, there is a discontinuity at age 60 in terms of revenue.

************************* Regressions *************************
ssc install ranktest
ssc install ivreg2

*First stage FE regression
xtset persnr year
xtreg retired child adult lrevb a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo retired_FSM

*Reduced Form
xtset persnr year
xtreg h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo h_production_RFM

*FE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
eststo FE_M

*RE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, re vce(cluster persnr)

*Get similar results with RE estimation than found with FE. 

*IV estimation
ivreg2 h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo FE_IV_M

*FD-IV estimation
xtset persnr year
xtreg D.(h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65), vce(cluster persnr)

*Results go in the same direction and are significant but are lower

*FE-IV and FE estimation for specific time uses
foreach x of varlist h_*{
ivreg2 `x' child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo M_`x'
xtset persnr year
xtreg `x' retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)
}

*Similar results from FE model than FE-IV

*****************Robustness tests*****************************

*Robustness test without control variables adult and child

*FE estimation
xtset persnr year
xtreg h_production retired a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*IV estimation
ivreg2 h_production a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)

*We find similar results as to the baseline specification

*Robustness test with individuals aged from 50 to 70 years old

drop if age<50 | age>70

*First stage FE regression
xtset persnr year
xtreg retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*Reduced Form
xtset persnr year
xtreg h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*FE estimation
xtset persnr year
xtreg h_production retired child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 b60 b63 b65, fe vce(cluster persnr)

*IV estimation
ivreg2 h_production child adult a a2 a3 ca1 ca2 ca3 ca12 ca22 ca32 ca33 (retired = b60 b63 b65), robust cluster(persnr) endog(retired)
eststo FE_IV_robM

*We find similar results as to the baseline specification

******************Exporting Regression Tables******************

*First stage and Reduced Form estimates by gender

esttab retired_FSM retired_FSW h_production_RFM h_production_RFW using "Tables/table_2.tex", replace ///
keep(b60 b63 b65) se se(3) b(3)  star (* .10 ** .05 *** .01) compress nonum nonotes noconstant ///
varlabels(b60 "being 60 year-old or more" b63 "being 63 year-old or more" b65 "being 65 year-old or more") ///
addnote(Note: *** p<0.01, ** p<0.05, * p<0.1.) ///
stat(N, labels("Number of individuals")) ///
mtitles("Man" "Woman" "Man" "Woman") ///
mgroups("Retired" "Home Production", pattern(1 0 1 0)) ///
title("First-stage and reduced form estimates using age thresholds as instruments. FE models.")

*Retirement and hours of home production per week by gender

esttab FE_M FE_IV_M FE_IV_robM FE_W FE_IV_W FE_IV_robW using "Tables/table_3.tex", replace ///
keep(retired) se se(3) b(3)  star (* .10 ** .05 *** .01) compress nonotes noconstant ///
varlabels(retired "Own retirement") ///
addnote(Note: *** p < 0.01, ** p < 0.05, * p < 0.1.) ///
stat(N, labels("Number of individuals")) ///
mtitles("FE-Model 45-75" "FE-IV 45-75" "FE-IV 50-70" "FE-Model 45-75" "FE-IV 45-75" "FE-IV 50-70") ///
mgroups("Men" "Women", pattern(1 0 0 1 0 0)) ///
title("Retirement and hours of home production per week by gender.")

*Retirement and hours spent per week on the different components of home production and other time use. FE-IV models using age thresholds as instruments.

*For Men

esttab M_h_housework M_h_errand M_h_repair M_h_child M_h_act M_h_other using "Tables/table_4.tex", replace ///
keep(retired) se se(3) b(3)  star (* .10 ** .05 *** .01) compress nonotes noconstant ///
varlabels(retired "Own retirement") ///
addnote(Note: *** p < 0.01, ** p < 0.05, * p < 0.1. ) ///
stat(N, labels("Number of individuals")) ///
mtitles("Housework" "Errands" "Repairs/Gardening" "Child care" "Leisure" "Other") ///
mgroups("Components of home production" "Other time use", pattern(1 0 0 1 0 0 0)) ///
title("Men: Retirement and hours spent per week on the different components of home production and other time use. FE-IV models using age thresholds as instruments.")

*For Women 

esttab W_h_housework W_h_errand W_h_repair W_h_child W_h_act W_h_other using "Tables/table_5.tex", replace ///
keep(retired) se se(3) b(3)  star (* .10 ** .05 *** .01) compress nonotes noconstant ///
varlabels(retired "Own retirement") ///
addnote(Note: *** p < 0.01, ** p < 0.05, * p < 0.1.) ///
stat(N, labels("Number of individuals")) ///
mtitles("Housework" "Errands" "Repairs/Gardening" "Child care" "Leisure" "Other") ///
mgroups("Components of home production" "Other time use", pattern(1 0 0 1 0 0 0)) ///
title("Women: Retirement and hours spent per week on the different components of home production and other time use. FE-IV models using age thresholds as instruments.")

