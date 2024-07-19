*********************************************************
** ELSA
*********************************************************
* This file to prepare a IVFE Table for to study the effect of subjective well-being level on total savings
* Written by Safaa Basabreen June, 2024
**********************************************************************
*global data_in= ....
cd  "C:\Users\safaa\OneDrive - The University of Liverpool\Desktop\SB\Do file\"
clear

use "data\ELSA.dta", clear

*ssc install outreg2 
*ssc install xtivreg2 
*ssc install reghdfe


***Define variables
***Variables: Dep: total savings
**Indp: Subjective well-being  age female married retired self_employed unemployed unemployed_spouse education has_child household size health 
**IV1: sleep quality: pscedc & pscedh
** Controls: Lincome age has_child female married higher_ed unemployed unemployed_spouse health household_size i.location_f
** For addational analysis and robustness check: Total wealth
*********************************************************
*** Prepare Data
*********************************************************
drop Lincome
*gen mincome= totinc_bu_s*4 // * if you want to transform income from weekly to monthly
*gen Lincome=ln(mincome+1)
gen Lincome=ln(totinc_bu_s+1)
lab var Lincome "Ln total net income_summary"
gen has_savings = (savings_bu_s > 0)
gen Lns=ln(savings_bu_s+1)
lab var Lns "Ln total savings_summary"
egen avg_savings = mean(savings_bu_s)
gen Lns2=ln(invests_bu_s+1) 
lab var Lns2 "Ln total investment_summary"
gen Ln_wealth=ln(netfw_bu_s+1)
lab var Ln_wealth "Ln net total financial wealth"

gen Ln_debt=ln(debt_bu_s+1)
lab var Ln_debt "Ln total financial debt"

*tab wave, gen(wave)
gen wave2 = (wave == 2)
gen wave3 = (wave == 3)
gen wave4 = (wave == 4)
gen wave5 = (wave == 5)
gen wave6 = (wave == 6)
gen wave7 = (wave == 7)
gen wave8 = (wave == 8)
gen wave9 = (wave == 9)

g male=indsex==1
g female=indsex==2
g married = dimar ==2&3
g higher_ed = edqual== 1&2 
g employed=wselfd==1
g self_employed=wselfd==2
g retired=wselfd==3
g unemployed=wselfd==4
g unemployed_spouse=wselfd_p==4
*tab wselfd, gen(wselfd)
*tab edqual, gen(edqual)
g has_child=chinhh ==1
g takingrisks = scgrisk > 5
g restless=pscedc ==1
g Notgoing=pscedh ==1
tab location, gen(location)
encode location, generate(location_f)
rename hehelf health 
rename hhtot household_size
lab var household_size "Number of people in household"

* Generate dummy variables
*Remove missing data
drop if SWB == .
gen high_swb = (SWB > 24)
gen low_swb = (SWB < 20)

**Declare data as panel data
xtset idauniq wave
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**Controls
local cons  Lincome age squr_age has_child female married higher_ed unemployed unemployed_spouse health household_size i.location_f
local cons1  Lincome age squr_age has_child married higher_ed unemployed unemployed_spouse health household_size i.location_f
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
**Tables
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*Tab.1: Descriptive Statistics of the Study Sample Between 2004 and 2019
tabstat savings_bu_s SWB age totinc_bu_s debt_bu_s household_size health age female married has_child higher_ed self_employed unemployed retired unemployed_spouse, c(stat) stat(n mean sd)
***********************************************
***********************************************
*Tab.2: OLS and 2SLS Regressions of Subjective Well-being on Ln(Savings) with Sleep Disruption Instruments:*Non-retired vs Retired
***********************************************
***********************************************
* First Stage Instruments Regression 
eststo clear
qui eststo reg1: reg SWB pscedc pscedh `cons' i.wave if retired == 0, cluster(idauniq)
test pscedc pscedh
qui eststo reg2: reg SWB pscedc pscedh `cons' i.wave if retired == 1, cluster(idauniq)
test pscedc pscedh
esttab reg1 reg2, se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) indicate( "Year FE=*wave" ) order( SWB)

eststo clear
qui eststo reg1:  reg  Lns  SWB `cons'  if retired == 0, cluster(idauniq)
qui eststo reg2:  ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 0, cluster(idauniq)
qui eststo reg1a: reg  Lns  SWB `cons'  if retired == 1, cluster(idauniq)
qui eststo reg2a: ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 1, cluster(idauniq)
***Produce Latex Table 
esttab reg1 reg2 reg1a reg2a,  se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) indicate( "Year FE=*wave" ) order( SWB)
  estimates table reg1 reg2 reg1a reg2a
  cd $output
* Save regression table as LaTeX file
esttab using main_regression1.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("OLS and 2SLS Regressions of Subjective Well-being on Ln(Savings) with sleep disruption instruments") indicate("Year FE = *.wave") drop(_cons) addnote("")
***Note: *, **, and *** denote significance level respectively at 10%, 5%, and 1%.
***********************************************
***********************************************
*Tab.3: FE and IVFE
***********************************************
***********************************************
eststo clear
qui eststo reg3: reghdfe  Lns SWB `cons'  i.wave if retired == 0, absorb(idauniq wave) vce(cluster idauniq)
qui eststo reg4: xtivreg  Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 0, fe vce(cluster idauniq)
qui eststo reg3a: reghdfe Lns SWB `cons'  i.wave if retired == 1, absorb(idauniq wave) vce(cluster idauniq)
qui eststo reg4a: xtivreg Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 1, fe vce(cluster idauniq)  
***Produce Latex Table 
esttab reg3 reg4 reg3a reg4a,  se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) indicate( "Year FE=*wave" ) order(SWB)
  estimates table reg3 reg4 reg3a reg4a
  cd $output
* Save regression table as LaTeX file
esttab using main_regression2.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("FE and IVFE Regressions of Subjective Well-being on Ln(Savings) with sleep disruption instruments") indicate("Year FE = *.wave") drop(_cons) addnote("")
*Summary Results for First-Stage Regressions
xtivreg2 Lns Lincome has_child female married higher_ed unemployed unemployed_spouse health household_size location2 location3 location4 location5 location6 location7 wave3 wave4 wave5 wave6 wave7 wave8 wave9 (SWB = pscedc pscedh) if retired == 0, fe first endog(SWB)
xtivreg2 Lns Lincome has_child female married higher_ed unemployed unemployed_spouse health household_size location2 location3 location4 location5 location6 location7 wave3 wave4 wave5 wave6 wave7 wave8 wave9 (SWB = pscedc pscedh) if retired == 1, fe first endog(SWB)
***********************************************
***********************************************
*Tab.4: IVFE by Gender
***********************************************
***********************************************
eststo clear
qui eststo reg1: xtivreg  Lns  (SWB =pscedc pscedh) `cons1' i.wave if retired == 0 & male==1, fe vce(cluster idauniq)
qui eststo reg2: xtivreg  Lns  (SWB =pscedc pscedh) `cons1' i.wave if retired == 0 & female==1, fe vce(cluster idauniq)
qui eststo reg3: xtivreg  Lns  (SWB =pscedc pscedh) `cons1' i.wave if retired == 1 & male==1, fe vce(cluster idauniq)
qui eststo reg4: xtivreg  Lns  (SWB =pscedc pscedh) `cons1' i.wave if retired == 1 & female==1, fe vce(cluster idauniq)
esttab reg1 reg2 reg3 reg4, se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) order(SWB)
  estimates table reg1 reg2 reg3 reg4
  cd $output
esttab using main_regression3.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("IVFE Regressions of Subjective Well-being on Ln(Savings) by Gender") drop(_cons)
***********************************************
***********************************************
*Tab.5: IVFE by by Gender & Income Levels for Non-retired
***********************************************
***********************************************
egen income_bracket = cut(totinc_bu_s), group(3)
    eststo clear
qui eststo reg1: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & income_bracket == 1 & male==1, fe vce(cluster idauniq)
qui eststo reg2: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & income_bracket == 1 & female==1, fe vce(cluster idauniq)
qui eststo reg3: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & income_bracket == 2 & male==1, fe vce(cluster idauniq)
qui eststo reg4: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & income_bracket == 2 & female==1, fe vce(cluster idauniq)
esttab reg1 reg2 reg3 reg4, se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) order(SWB)
  estimates table reg1 reg2 reg3 reg4
  cd $output
esttab using main_regression4.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("IVFE Regressions of Subjective Well-being on Ln(Savings) by Income Levels") drop(_cons)

***********************************************
***********************************************
*Tab.6: Robustness Check: Ln(Wealth)
***********************************************
***********************************************
eststo clear
qui eststo reg3: reghdfe  Ln_wealth SWB `cons'  i.wave if retired == 0, absorb(idauniq wave) vce(cluster idauniq)
qui eststo reg4: xtivreg  Ln_wealth (SWB =pscedc pscedh) `cons'  i.wave if retired == 0, fe vce(cluster idauniq)
qui eststo reg3a: reghdfe Ln_wealth SWB `cons'  i.wave if retired == 1, absorb(idauniq wave) vce(cluster idauniq)
qui eststo reg4a: xtivreg Ln_wealth (SWB =pscedc pscedh) `cons'  i.wave if retired == 1, fe vce(cluster idauniq)  
esttab reg3 reg4 reg3a reg4a,  se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) indicate( "Year FE=*wave" ) order(SWB)
  estimates table reg3 reg4 reg3a reg4a
  cd $output
esttab using main_regression5.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("FE and IVFE Regressions of Subjective Well-being on Ln(Wealth) with sleep disruption instruments") indicate("Year FE = *.wave") drop(_cons) addnote("")

***********************************************
***********************************************
*Tab.7: Robustness Check: Ln(Wealth): by Gender
***********************************************
***********************************************
eststo clear
qui eststo reg1: xtivreg  Ln_wealth  (SWB =pscedc pscedh) `cons1' i.wave if retired == 0 & male==1, fe vce(cluster idauniq)
qui eststo reg2: xtivreg  Ln_wealth  (SWB =pscedc pscedh) `cons1' i.wave if retired == 0 & female==1, fe vce(cluster idauniq)
qui eststo reg3: xtivreg  Ln_wealth  (SWB =pscedc pscedh) `cons1' i.wave if retired == 1 & male==1, fe vce(cluster idauniq)
qui eststo reg4: xtivreg  Ln_wealth  (SWB =pscedc pscedh) `cons1' i.wave if retired == 1 & female==1, fe vce(cluster idauniq)
esttab reg1 reg2 reg3 reg4, se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) order(SWB)
  estimates table reg1 reg2 reg3 reg4
  cd $output
esttab using main_regression6.tex, replace se r2  star(* 0.1 ** 0.05 *** 0.01) label title("IVFE Regressions of Subjective Well-being on Ln(Savings) by Gender") drop(_cons)

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*Additional Analysis
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*Tab.1: OLS and 2SLS Regressions of Subjective Well-being on Ln(Savings) by Gender
qui eststo reg1:  reg  Lns  SWB `cons'  if retired == 0 & male==1, cluster(idauniq)
qui eststo reg2:  ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 0 & male==1, cluster(idauniq)
qui eststo reg3:  reg  Lns  SWB `cons'  if retired == 0 & female==1, cluster(idauniq)
qui eststo reg4: ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 0 & female==1, cluster(idauniq)
qui eststo reg5:  reg  Lns  SWB `cons'  if retired == 1 & male==1, cluster(idauniq)
qui eststo reg6:  ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 1 & male==1, cluster(idauniq)
qui eststo reg7:  reg  Lns  SWB `cons'  if retired == 1 & female==1, cluster(idauniq)
qui eststo reg8: ivregress 2sls Lns (SWB =pscedc pscedh) `cons'  i.wave if retired == 1 & female==1, cluster(idauniq)
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8,  se r2 star(* 0.1 ** 0.05 *** 0.01)
 
*Tab.2: IVFE Regressions of Subjective Well-being on Ln(Savings) by Gender and Risk Aversion
    eststo clear
qui eststo reg1: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & takingrisks == 0 & male==1, fe vce(cluster idauniq)
qui eststo reg2: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & takingrisks == 0 & female==1, fe vce(cluster idauniq)
qui eststo reg3: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & takingrisks == 1 & male==1, fe vce(cluster idauniq)
qui eststo reg4: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 0 & takingrisks == 1 & female==1, fe vce(cluster idauniq)
qui eststo reg5: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 1 & takingrisks == 0 & male==1, fe vce(cluster idauniq)
qui eststo reg6: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 1 & takingrisks == 0 & female==1, fe vce(cluster idauniq)
qui eststo reg7: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 1 & takingrisks == 1 & male==1, fe vce(cluster idauniq)
qui eststo reg8: xtivreg  Lns  (SWB =pscedc pscedh) `cons1'  i.wave if retired == 1 & takingrisks == 1 & female==1, fe vce(cluster idauniq)
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8, se r2 star(* 0.1 ** 0.05 *** 0.01) label drop(_cons) order(SWB)

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*Figures
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
* Fig.3: Perentage of Non-retired and Retired Respondents by Gender
graph bar, over(indsex) by(retired)
* Fig.1: Average values for the five statements constituting subjective well-being
*** Calculate the average value for each variable by wave
collapse (mean) SWB1 SWB2 SWB3 SWB4 SWB5, by(wave)
*** Plot the trend line
twoway (line SWB1 wave) (line SWB2 wave) (line SWB3 wave) (line SWB4 wave) (line SWB5 wave), scheme(s2mono) legend(order(1 "I am satisfied with my life" 2 "My life is close to my ideal" 3 "The conditions of my life are excellent" 4 "I have gotten the important things I want in life" 5 "If I could live my life over, then I would change almost nothing")) xtitle("Wave") ytitle("Average Value")
* Fig.2: Average subjective well-being by age
use "C:\Users\safaa\OneDrive - The University of Liverpool\Desktop\SB\Do file\data\ELSA.dta", clear
collapse (mean) SWB, by (age)
twoway (line SWB age if age >=50),
legend (order (1 "SWB")) xtitle("Age") ytitle("Average Value")
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
