
cd "~/Dropbox/projects/corrupt_audit"
use "data/wrangle/exportStata.dta", clear
gen corruption = cTot
drop if start >= yearAudit // keep only people that were there before audit


local controls "nItems logEmp gini illiteracy urban medianWage age gender experience i.edu_cat tenuredContract i.corruption i.year i.state i.start"

replace corruption = cTot
mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
estimates store m1

replace corruption = cHigh
mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
estimates store m2

replace corruption = cTotPerAmount
mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
estimates store m3

replace corruption = cHighPerAmount
mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
estimates store m4

esttab m1 m2 m3 m4 using "writing/draft9/tables/multinom.tex", /// 
booktabs noomitted aic label nofloat replace ///
keep(1.treat 2.corruption 3.corruption 1.treat#2.corruption 1.treat#3.corruption) /// 
order(1.treat 2.corruption 3.corruption 1.treat#2.corruption 1.treat#3.corruption) ///
mtitles("All faults" "Serious faults" "All faults normalized" "Serious faults normalized")

cap file close myfile
local name="writing/draft9/tables/multinomTests.tex"
file open myfile using "`name'", write text replace
file write myfile "\begin{table}[ht]" _n
file write myfile "\centering" _n
file write myfile "\begin{tabular}{l cc  cc }" _n
file write myfile "\toprule" _n
file write myfile "& \multicolumn{1}{c}{All} & \multicolumn{1}{c}{$1^{st}$ half} & \multicolumn{1}{c}{$2^{nd}$ half} & \multicolumn{1}{c}{after $70^{th}$ minute} \\" _n
	file write myfile "\midrule" _n
	local line1="$\beta_{11} + \beta_{21}$"
	local line2="$\beta_{11} + \beta_{31}$"
	local line3="$\beta_{12} + \beta_{22}$"
	local line4="$\beta_{12} + \beta_{32}$"
		foreach model in 1 2 3 4 {

			*cap{
				local controls "nItems logEmp gini illiteracy urban medianWage age gender experience i.edu_cat tenuredContract i.corruption i.year i.state i.start"
				
				estimates restore m`model'
				
				matrix B=e(b)
				
				local beta1=B[1,75]+B[1,80]
				local beta1:di%4.3f `beta1'
				
				local beta2=B[1,75]+B[1,81]
				local beta2:di%4.3f `beta2'
				
				local beta3=B[1,148]+B[1,153]
				local beta3:di%4.3f `beta3'
				
				local beta4=B[1,148]+B[1,154]
				local beta4:di%4.3f `beta4'
				
				
				test [1]1.treat + [1]1.treat#2.corruption = 0
				local pv1=r(p)
				local pv1:di%4.3f `pv1'
				local star1=""
				if `pv1'<0.1{
					local star1="^{*}"
				}
				if `pv1'<0.05{
					local star1="^{**}"
				}
				if `pv1'<0.01{
					local star1="^{***}"
				}
				if `pv1'<0.001{
					local star1="^{***}"
				}
				
				test [1]1.treat + [1]1.treat#3.corruption = 0
				local pv2=r(p)
				local pv2:di%4.3f `pv2'
				local star2=""
				if `pv2'<0.1{
					local star2="^{*}"
				}
				if `pv2'<0.05{
					local star2="^{**}"
				}
				if `pv2'<0.01{
					local star2="^{***}"
				}
				if `pv2'<0.001{
					local star2="^{***}"
				}
				
				test [1]1.treat + [1]1.treat#2.corruption = 0
				local pv3=r(p)
				local pv3:di%4.3f `pv3'
				local star3=""
				if `pv3'<0.1{
					local star3="^{*}"
				}
				if `pv3'<0.05{
					local star3="^{**}"
				}
				if `pv3'<0.01{
					local star3="^{***}"
				}
				if `pv3'<0.001{
					local star3="^{***}"
				}
				
				test [1]1.treat + [1]1.treat#3.corruption = 0
				local pv4=r(p)
				local pv4:di%4.3f `pv4'
				local star4=""
				if `pv4'<0.1{
					local star4="^{*}"
				}
				if `pv4'<0.05{
					local star4="^{**}"
				}
				if `pv4'<0.01{
					local star4="^{***}"
				}
				if `pv4'<0.001{
					local star4="^{***}"
				}
				
				
			
			local line1="`line1'"+"& $"+"`beta1'"+"`star1'"+" $ "
			local line2="`line2'"+"& $"+"`beta2'"+"`star2'"+" $ "
			local line3="`line3'"+"& $"+"`beta3'"+"`star3'"+" $ "
			local line4="`line4'"+"& $"+"`beta4'"+"`star4'"+" $ "
			
		}
		file write myfile "`line1'" "\\"  _n 
		file write myfile "`line2'" "\\"  _n 
		file write myfile "`line3'" "\\"  _n 
		file write myfile "`line4'" "\\"  _n 

file close myfile
