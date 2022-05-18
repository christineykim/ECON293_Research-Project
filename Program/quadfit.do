********************************************************************************
* Description: 	Estimate an RDD with local quadratic polynomial. 
*				Compute curvature B for use in optrdd.
* Authors: 		Team (ML)overs
********************************************************************************

**#1. Paths

clear all
set more off

if "`c(username)'" == "51989"{
	global myGithub "C:/Users/51989/Projects_Alessandra/ECON293_Research-Project"
	global myDrop 	"C:/Users/51989/OneDrive/Escritorio/Dropbox/Default Tips Project"
}
global out "$myDrop/out"

**#2. Cleaning

import delimited using "$myDrop/Data/Intermediate/fare_1020_recoded.csv"

gen x = fare - 15
gen x2 = x^2
gen w = (x > 0)
gen wx = w*x
gen wx2 = w*x2
gen y = tip_zero

**#3. Results

**#3.1. Distribution of fare
tab x
* Note from this tabulation that this is clearly a discrete variable!

**#3.2. Regression specification
reg y x x2 w wx wx2, vce(robust)

* Saving the estimate of B before and after the cutoff 
* Note we take the negative of 
lincom 2*x2
lincom 2*x2-2*wx2 // wx2 was originally negative
local B = `r(estimate)'
local Bu= `r(estimate)' + 1.96*r(se)
local Bl= `r(estimate)' - 1.96*r(se)

disp "Curvature B = `r(estimate)'"
disp "95% CI is (`Bl', `Bu')"


