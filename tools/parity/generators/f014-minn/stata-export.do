version 14.2

args input_csv output_dir ado_root

if "`input_csv'" == "" {
    display as error "usage: stata -b do stata-export.do <input_csv> <output_dir> [ado_root]"
    exit 198
}
if "`output_dir'" == "" {
    display as error "usage: stata -b do stata-export.do <input_csv> <output_dir> [ado_root]"
    exit 198
}
if "`ado_root'" == "" {
    local ado_root "${STATA_ADO_ROOT}"
}

cap mkdir "`output_dir'"
set more off
set type double
sysdir set PLUS "`ado_root'/ado/plus"
sysdir set PERSONAL "`ado_root'/ado/personal"
adopath ++ "`ado_root'/ado/personal"
adopath ++ "`ado_root'/ado/plus"

log using "`output_dir'/run.log", text replace

display "F014_STATA_INPUT=`input_csv'"
display "F014_STATA_OUTPUT=`output_dir'"
display "F014_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

did_imputation Y unit t Ei, horizons(0/1) minn(30) cluster(unit) saveweights

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local bcols : colnames b
local vrows : rownames V
local vcols : colnames V
local term_count = colsof(b)
local droplist "`e(droplist)'"

gen byte sample = e(sample)

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated,suppressed,algebraic_target" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local horizon = substr("`term'", 4, .)
    local suppressed = strpos(" `droplist' ", " `term' ") > 0
    quietly summarize tau if sample == 1 & D == 1 & event_time == `horizon', meanonly
    local algebraic_target = r(mean)
    local estimate = el(b, 1, `idx')
    local std_error = sqrt(el(V, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) "," %21.17g (`suppressed') "," %21.17g (`algebraic_target') _n
}
file close estimates

file open cov using "`output_dir'/covariance.csv", write replace
file write cov "row_term,col_term,value" _n
forvalues r = 1/`=rowsof(V)' {
    local row_term : word `r' of `vrows'
    forvalues c = 1/`=colsof(V)' {
        local col_term : word `c' of `vcols'
        file write cov "`row_term',`col_term'," %21.17g (el(V, `r', `c')) _n
    }
}
file close cov

file open mask using "`output_dir'/sample-mask.csv", write replace
file write mask "row_id,sample" _n
forvalues r = 1/`=_N' {
    file write mask "`=row_id[`r']'," %21.17g (sample[`r']) _n
}
file close mask

file open weights using "`output_dir'/imputation-weights.csv", write replace
file write weights "row_id,term,weight" _n
forvalues r = 1/`=_N' {
    file write weights "`=row_id[`r']',tau0," %21.17g (__w_tau0[`r']) _n
    file write weights "`=row_id[`r']',tau1," %21.17g (__w_tau1[`r']) _n
}
file close weights

local tau0_estimate = el(b, 1, 1)
local tau1_estimate = el(b, 1, 2)
local tau0_se = sqrt(el(V, 1, 1))
local tau1_se = sqrt(el(V, 2, 2))

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "command": "did_imputation Y unit t Ei, horizons(0/1) minn(30) cluster(unit) saveweights","' _n
file write diag `"  "terms": ["tau0", "tau1"],"' _n
file write diag `"  "minn": 30,"' _n
file write diag `"  "droplist": "`droplist'","' _n
file write diag `"  "estimates": {"' _n
file write diag `"    "tau0": "' %21.17f (`tau0_estimate') "," _n
file write diag `"    "tau1": "' %21.17f (`tau1_estimate') _n
file write diag `"  }, "' _n
file write diag `"  "std_errors": {"' _n
file write diag `"    "tau0": "' %21.17f (`tau0_se') "," _n
file write diag `"    "tau1": "' %21.17f (`tau1_se') _n
file write diag `"  }, "' _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated": ["' %21.17g (el(Nt, 1, 1)) ", " %21.17g (el(Nt, 1, 2)) "]" _n
file write diag "}" _n
file close diag

display "F014_STATA_EXPORT_OK=1"
log close
exit, clear
