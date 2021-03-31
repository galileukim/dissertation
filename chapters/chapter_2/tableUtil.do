cd "~/Dropbox/projects/corrupt_audit"
cd "~/Dropbox/projects/corrupt_audit"
use "data/wrangle/exportStata.dta", clear
gen corruption = cTot
drop if start >= yearAudit // keep only people that were there before audit

		cap file close myfile
		local name="table.tex"
		file open myfile using "`name'", write text replace
		file write myfile "\begin{table}[ht]" _n
		file write myfile "\centering" _n
		file write myfile "\begin{tabular}{l cc  cc }" _n
		file write myfile "\toprule" _n
		file write myfile "& \multicolumn{1}{c}{All} & \multicolumn{1}{c}{$1^{st}$ half} & \multicolumn{1}{c}{$2^{nd}$ half} & \multicolumn{1}{c}{after $70^{th}$ minute} \\" _n
			file write myfile "\midrule" _n
			local line1="estimates "
			local line2="pv"
			local line3="AIC"
				foreach model in 1 2 /*3 4*/{
					local b1=.
					local pv=.
					local N=.
					local star=""
					*cap{
						local controls "nItems logEmp gini illiteracy urban medianWage age gender experience i.edu_cat tenuredContract i.corruption i.year i.state i.start"
						
						if `model'==1{
							replace corruption = cTot
							mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
						}
						
						if `model'==2{
							replace corruption = cHigh
							mlogit outcome i.treat i.treat#i.corruption `controls', vce(cluster idMun)
							
							*estimates use m2
						}
						
						matrix B=e(b)
						matrix V=e(V)
						local estimates=B[1,75]+B[1,80]
						local b1:di%4.3f `estimates'
						
						local N=e(N)
						local N: di%6.0fc `N'
						
						local AIC=-2*e(ll)
						local AIC:di%4.0f `AIC'
						
						test [1]1.treat + [1]1.treat#2.corruption = 0
						local pv=r(p)
						
						local pv:di%4.3f `pv'
						local star1=""
						if `pv'<0.1{
							local star="^{\dag}"
						}
						if `pv'<0.05{
							local star="^{*}"
						}
						if `pv'<0.01{
							local star="^{**}"
						}
						if `pv'<0.001{
							local star="^{***}"
							local pv="<0.001"
						}
					
					local line1="`line1'"+"& $"+"`b1'"+"`star'"+" $ "
					local line2="`line2'"+" & \footnotesize{$("+"`pv'"+")$} "
					local line3="`line3'"+" & \footnotesize{$"+"`AIC'"+"$} "
					
				}
				file write myfile "`line1'" "\\"  _n 
				file write myfile "`line2'" "\\"  _n 
				file write myfile "`line3'" "\\"  _n 
	
		file write myfile "\bottomrule" _n
		file write myfile "\end{tabular}" _n
		file write myfile "`caption'" _n 
		file write myfile "\end{table}" 
		file close myfile

