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

display "F018_STATA_INPUT=`input_csv'"
display "F018_STATA_OUTPUT=`output_dir'"
display "F018_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

did_imputation Y unit t Ei, minn(0) cluster(unit) saveweights

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local bcols : colnames b
local vrows : rownames V
local vcols : colnames V
local term_count = colsof(b)
local y_estimate = el(b, 1, 1)
local y_se = sqrt(el(V, 1, 1))

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local estimate = el(b, 1, `idx')
    local std_error = sqrt(el(V, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) _n
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
    file write weights "`=row_id[`r']',tau," %21.17g (__w_tau[`r']) _n
}
file close weights

gen double weighted_y = __w_tau * Y
quietly summarize weighted_y, meanonly
local weighted_y_estimate = r(sum)
quietly summarize __w_tau if D == 1, meanonly
local treated_weight_sum = r(sum)
egen double unit_weight_sum = total(__w_tau), by(unit)
egen double time_weight_sum = total(__w_tau), by(t)
gen double abs_unit_weight_sum = abs(unit_weight_sum)
gen double abs_time_weight_sum = abs(time_weight_sum)
quietly summarize abs_unit_weight_sum, meanonly
local max_abs_unit_sum = r(max)
quietly summarize abs_time_weight_sum, meanonly
local max_abs_time_sum = r(max)
drop weighted_y unit_weight_sum time_weight_sum abs_unit_weight_sum abs_time_weight_sum

file open checks using "`output_dir'/weight-checks.csv", write replace
file write checks "check,value" _n
file write checks "weighted_y_estimate," %21.17g (`weighted_y_estimate') _n
file write checks "treated_weight_sum," %21.17g (`treated_weight_sum') _n
file write checks "max_abs_unit_sum," %21.17g (`max_abs_unit_sum') _n
file write checks "max_abs_time_sum," %21.17g (`max_abs_time_sum') _n
file close checks

rename __w_tau saved_tau

did_imputation Y2 unit t Ei, minn(0) cluster(unit) loadweights(saved_tau)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local bcols : colnames b
local vrows : rownames V
local vcols : colnames V
local term_count = colsof(b)
local y2_load_estimate = el(b, 1, 1)
local y2_load_se = sqrt(el(V, 1, 1))

file open estimates using "`output_dir'/load-estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local estimate = el(b, 1, `idx')
    local std_error = sqrt(el(V, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) _n
}
file close estimates

file open cov using "`output_dir'/load-covariance.csv", write replace
file write cov "row_term,col_term,value" _n
forvalues r = 1/`=rowsof(V)' {
    local row_term : word `r' of `vrows'
    forvalues c = 1/`=colsof(V)' {
        local col_term : word `c' of `vcols'
        file write cov "`row_term',`col_term'," %21.17g (el(V, `r', `c')) _n
    }
}
file close cov

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "save_command": "did_imputation Y unit t Ei, minn(0) cluster(unit) saveweights","' _n
file write diag `"  "load_command": "did_imputation Y2 unit t Ei, minn(0) cluster(unit) loadweights(saved_tau)","' _n
file write diag `"  "schema_version": "stata.__w_tau","' _n
file write diag `"  "y_estimate": "' %21.17g (`y_estimate') "," _n
file write diag `"  "y_std_error": "' %22.17f (`y_se') "," _n
file write diag `"  "weighted_y_estimate": "' %21.17g (`weighted_y_estimate') "," _n
file write diag `"  "y2_load_estimate": "' %21.17g (`y2_load_estimate') "," _n
file write diag `"  "y2_load_std_error": "' %22.17f (`y2_load_se') "," _n
file write diag `"  "treated_weight_sum": "' %21.17g (`treated_weight_sum') "," _n
file write diag `"  "max_abs_unit_sum": "' %21.17g (`max_abs_unit_sum') "," _n
file write diag `"  "max_abs_time_sum": "' %21.17g (`max_abs_time_sum') "," _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated": "' %21.17g (el(Nt, 1, 1)) _n
file write diag "}" _n
file close diag

display "F018_STATA_EXPORT_OK=1"
log close
exit, clear
