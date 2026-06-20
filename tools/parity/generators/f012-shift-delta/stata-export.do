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

display "F012_STATA_INPUT=`input_csv'"
display "F012_STATA_OUTPUT=`output_dir'"
display "F012_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

did_imputation Y unit t Ei, horizons(0/2) shift(2) delta(2) minn(0) cluster(unit) saveweights

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local bcols : colnames b
local vrows : rownames V
local vcols : colnames V
local term_count = colsof(b)

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated,algebraic_target" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local horizon = substr("`term'", 4, .)
    quietly summarize tau if event_time == `horizon' & D == 1, meanonly
    local algebraic_target = r(mean)
    local estimate = el(b, 1, `idx')
    local std_error = sqrt(el(V, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) "," %21.17g (`algebraic_target') _n
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

gen byte sample = e(sample)
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
    file write weights "`=row_id[`r']',tau2," %21.17g (__w_tau2[`r']) _n
}
file close weights

preserve
capture noisily did_imputation Y unit t Ei, horizons(0/2) shift(2) delta(3) minn(0) cluster(unit)
local invalid_delta_rc = _rc
restore

file open invalid using "`output_dir'/invalid-delta.json", write replace
file write invalid "{" _n
if (`invalid_delta_rc' == 0) {
    file write invalid `"  "status": "unexpected_success","' _n
}
else {
    file write invalid `"  "status": "error","' _n
}
file write invalid `"  "return_code": "' %21.17g (`invalid_delta_rc') _n
file write invalid "}" _n
file close invalid

local tau0_estimate = el(b, 1, 1)
local tau1_estimate = el(b, 1, 2)
local tau2_estimate = el(b, 1, 3)
local tau0_se = sqrt(el(V, 1, 1))
local tau1_se = sqrt(el(V, 2, 2))
local tau2_se = sqrt(el(V, 3, 3))

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "command": "did_imputation Y unit t Ei, horizons(0/2) shift(2) delta(2) minn(0) cluster(unit) saveweights","' _n
file write diag `"  "terms": ["tau0", "tau1", "tau2"],"' _n
file write diag `"  "shift": 2,"' _n
file write diag `"  "delta": 2,"' _n
file write diag `"  "estimates": {"' _n
file write diag `"    "tau0": "' %21.17f (`tau0_estimate') "," _n
file write diag `"    "tau1": "' %21.17f (`tau1_estimate') "," _n
file write diag `"    "tau2": "' %21.17f (`tau2_estimate') _n
file write diag `"  }, "' _n
file write diag `"  "std_errors": {"' _n
file write diag `"    "tau0": "' %21.17f (`tau0_se') "," _n
file write diag `"    "tau1": "' %21.17f (`tau1_se') "," _n
file write diag `"    "tau2": "' %21.17f (`tau2_se') _n
file write diag `"  }, "' _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated": ["' %21.17g (el(Nt, 1, 1)) ", " %21.17g (el(Nt, 1, 2)) ", " %21.17g (el(Nt, 1, 3)) "], " _n
file write diag `"  "invalid_delta_return_code": "' %21.17g (`invalid_delta_rc') _n
file write diag "}" _n
file close diag

display "F012_STATA_EXPORT_OK=1"
log close
exit, clear
