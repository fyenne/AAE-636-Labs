********************************************************************************
//References: https://www.princeton.edu/~otorres/DID101.pdf
********************************************************************************
cd "/Users/SunnyJieFeng/AAE636/Session7/"
log using Session7

* Getting sample data.
use "/Users/SunnyJieFeng/AAE636/Session7/Panel101.dta", clear

* Create a dummy variable to indicate the time when the treatment started. 
gen time = (year>=1994) & !missing(year)

* Create a dummy variable to identify the group exposed to the treatment. 
gen treated = (country>4) & !missing(country)

* Create an interaction between time and treated. 
gen did = time*treated

* Estimating the DID estimator
* Method 1:
reg y time treated did

* Method 2:
reg y time##treated

* Method 3:
ssc install diff
diff y, t(treated) p(time)

* Estimating the DID estimator with Robust SE
reg y time treated did,r

log close
