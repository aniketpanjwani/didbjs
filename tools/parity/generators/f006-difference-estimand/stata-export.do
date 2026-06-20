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

display "F006_STATA_INPUT=`input_csv'"
display "F006_STATA_OUTPUT=`output_dir'"
display "F006_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

gen byte treated_obs = Ei < . & t >= Ei
gen double raw_wtr_diff = treated_obs * w * wtr_diff
gen double raw_wtr_diff_abs = abs(raw_wtr_diff)
egen double algebraic_diff = total(raw_wtr_diff * tau)
egen double raw_wtr_diff_sum = total(raw_wtr_diff)
egen double raw_wtr_diff_abs_sum = total(raw_wtr_diff_abs)
egen double raw_wtr_diff_negative = total(cond(raw_wtr_diff < 0, raw_wtr_diff, 0))
egen double raw_wtr_diff_positive = total(cond(raw_wtr_diff > 0, raw_wtr_diff, 0))
summarize algebraic_diff, meanonly
local target_diff = r(mean)
summarize raw_wtr_diff_sum, meanonly
local raw_sum = r(mean)
summarize raw_wtr_diff_abs_sum, meanonly
local raw_abs_sum = r(mean)
summarize raw_wtr_diff_negative, meanonly
local raw_negative = r(mean)
summarize raw_wtr_diff_positive, meanonly
local raw_positive = r(mean)

did_imputation Y unit t Ei [aw=w], wtr(wtr_diff) sum minn(0) cluster(unit)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)

local bcols : colnames b
local estimate = el(b, 1, 1)
if abs(`estimate' - `target_diff') > 1e-10 {
    display as error "F006 difference assertion failed: " %21.17g `estimate' " target " %21.17g `target_diff'
    exit 459
}

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues j = 1/`=colsof(b)' {
    local term : word `j' of `bcols'
    local estimate = el(b, 1, `j')
    local variance = el(V, `j', `j')
    local std_error = sqrt(`variance')
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `j')) _n
}
file close estimates

local vrows : rownames V
local vcols : colnames V
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

file open weights using "`output_dir'/difference-weights.csv", write replace
file write weights "term,row_id,raw_weight" _n
forvalues r = 1/`=_N' {
    file write weights "tau,`=row_id[`r']'," %21.17g (raw_wtr_diff[`r']) _n
}
file close weights

gen byte sample = e(sample)
keep row_id sample
export delimited using "`output_dir'/sample-mask.csv", replace

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w], wtr(wtr_diff) sum minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "terms": ["tau"],"' _n
file write diag `"  "raw_weight_sum": "' %21.17g (`raw_sum') "," _n
file write diag `"  "raw_abs_weight_sum": "' %21.17g (`raw_abs_sum') "," _n
file write diag `"  "raw_negative_weight_sum": "' %21.17g (`raw_negative') "," _n
file write diag `"  "raw_positive_weight_sum": "' %21.17g (`raw_positive') "," _n
file write diag `"  "algebraic_difference": "' %21.17g (`target_diff') "," _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated_tau": "' %21.17g (el(Nt, 1, 1)) _n
file write diag "}" _n
file close diag

display "F006_STATA_EXPORT_OK=1"
log close
exit, clear
