************************************************************************************
*	Project:	How does Household Saving Behaviour Respond to Financial Uncertainty?
*	Course:		Applied Data Science for Business and Economics					    	   
*	Author:		Philip Dimitrov	   
*	Institution:University of Würzburg
*	Notes:		This code was part of a group project. The current file is prepared
*				for an ECB internship application. Only the code written by 
*				Philip Dimitrov is shown. 
************************************************************************************

*--- Step 1: Prepare working environment
 
* Set
clear all
set more off
set varabbrev off
set maxvar 10000


* Paths
global RD "L:\12-WiWi\VWL\VWL8\SOEPCampus\SOEP_T" 	          		// Reading directory
global IN "$RD"														// Data

global WD "J:\Applied Data Science\File_Structure"					// Working directory

* copy "MyWD" structure into WD
global OUT "$WD/Out"
global TEMP "$WD/Temp"
global DO "$WD/Do"
global FIG "$WD/Fig"
global TAB "$WD/Table"
ssc install outreg2

*--- Step 2: Load and merge datasets

/* 	Merge the main (household) dataset with the personal dataset, pgen, and ppathl.
	Merge the datasets by household-id, personal-id, and survey year. Keep only the
	matched observations.
*/

use "$RD/hl.dta"
merge 1:m hid syear using "$RD/pl.dta"  
drop _merge
merge 1:m hid pid syear using "$RD/ppathl.dta"
drop _merge
merge 1:m hid pid syear using "$RD/pgen.dta"
keep if _merge == 3


*--- Step 3: Construct derived variables for subsequent regression analysis
 
/* 	Replace some variable names with more intuitive labels, drop implausible
	and/or irrelevant observations, trim outliers and generate new variables.
*/

gen Income = plc0013_h
gen Savings = hlc0120_h
gen Risk_Tolerance = plh0204_h

sort hid syear

drop if syear < 2002
drop if Income < 0
drop if Savings < 0 
drop if gebjahr < 0 
drop if sampreg < 0 
drop if Risk_Tolerance < 0 

sum Income, d 

egen p5 = pctile(Income), p(5)
egen p95 = pctile(Income), p(95)

drop if Income <= p5 | Income >= p95

sum Savings, d

egen sp95 = pctile(Savings), p(95)
drop if Savings >= sp95

gen Worries_About_Economy = (plh0032 < 3)
gen Age = syear - gebjahr
gen West_Germany_Dummy = (sampreg == 1)

/* 	Since some respondents report zero savings, add a small constant to each
	observation so as not to take the logarithm of zero. 
*/

gen Log_Savings = log(Savings + 1)
gen Log_Income = log(Income)

* Crisis dummy captures the Global Financial Crisis, the Eurocrisis, and the COVID crisis.

gen Crisis_Dummy = (syear == 2008 | syear == 2009 | syear == 2014 | syear == 2015 | syear == 2020 | syear == 2021)
gen Education_Dummy = (pgpbbil02 > 0)
gen HasKids_Dummy = (hlc0043 > 0)

* Lag savings by one period to generate a savings differential. *

gen lag_savings = .
bysort hid (syear): replace lag_savings = hlc0120_h[_n-1]
gen savings_diff = Savings - lag_savings

/* 	However, a decrease in savngs will result in a negative savings differential.
	To prevent taking logarithms of negative numbers, the variable is transformed,
	but the real signs are preserved.
*/
gen Log_Savings_Diff = .
replace Log_Savings_Diff = log(savings_diff + 1) if savings_diff >= 0
replace Log_Savings_Diff = -log(abs(savings_diff)) if savings_diff < 0
drop if missing(savings_diff)

/* 	Flag special cases where households consistently report zero savings.
	Construct a dummy variable for subsequent examination. 
*/
gen zero_to_zero = (lag_savings == 0 & hlc0120_h == 0)

/* 	The following dummy captures all respondents with an income no more than
	the 50th percentile, i.e. 2500 Euros.
	Takes between-household differences into account.
*/	
gen Below_Median_Dummy = (Income <= 2500)


*--- Step 4: Regression models

* Always cluster standard errors at the household level to account for within-household correlations.

* Simple OLS regression without controls. Used to purposefully show biased results.
reg Log_Savings Log_Income, cluster(hid)
 

* Regressing Log_Savings (OLS) 
reg Log_Savings Log_Income HasKids_Dummy Crisis_Dummy Age Risk_Tolerance Worries_About_Economy Education_Dummy Below_Median_Dummy, cluster(hid)


* Regressing Log_Savings_Diff (OLS)
reg Log_Savings_Diff Log_Income HasKids_Dummy Crisis_Dummy Age Risk_Tolerance Worries_About_Economy Education_Dummy Below_Median_Dummy, cluster(hid)


* IV regression using West Germany Dummy as an instrument. 
ivregress 2sls Log_Savings (Log_Income =  West_Germany_Dummy) HasKids_Dummy Crisis_Dummy Age Risk_Tolerance Worries_About_Economy Education_Dummy Below_Median_Dummy, robust cluster(hid)


* First stage IV regression to test the relevance assumption.
regress Log_Income West_Germany_Dummy HasKids_Dummy Crisis_Dummy Age Risk_Tolerance Worries_About_Economy Education_Dummy, cluster(hid)


* Logit regression testing the impact of each factor on the probability of reporting zero savings in two consecutive periods.
logit zero_to_zero Log_Income Crisis_Dummy Risk_Tolerance Worries_About_Economy Education_Dummy Below_Median_Dummy, cluster(hid)


* Generate a graph to illustrate how a relative income increase influences the probability of staying at zero savings.
margins, at(Log_Income=(0 0.0953 0.1823 0.4055 0.6931)) atmeans
marginsplot, title("Effect of Income Increase on Probability of Staying at Zero Savings") xtitle("Percentage Increase in Income") xlabel(0 "0%" 0.0953 "10%" 0.1823 "25%" 0.4055 "50%" 0.6931 "100%") ytitle("Predicted Probability of Staying at Zero Savings")

