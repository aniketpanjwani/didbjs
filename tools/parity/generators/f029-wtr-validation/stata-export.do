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

display "F029_STATA_INPUT=`input_csv'"
display "F029_STATA_OUTPUT=`output_dir'"
display "F029_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_base) minn(0) cluster(unit)
matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local base_estimate = el(b, 1, 1)
local base_variance = el(V, 1, 1)
local base_se = sqrt(`base_variance')
file write estimates "base,tau," %24.17f (`base_estimate') "," %24.17f (`base_se') "," %24.17f (`base_estimate' - 1.959963984540054 * `base_se') "," %24.17f (`base_estimate' + 1.959963984540054 * `base_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, 1)) _n
file write cov "base,tau,tau," %24.17f (`base_variance') _n
gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-base.csv", replace
restore

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_scaled) minn(0) cluster(unit)
matrix bs = e(b)
matrix Vs = e(V)
matrix Nts = e(Nt)
local scaled_estimate = el(bs, 1, 1)
local scaled_variance = el(Vs, 1, 1)
local scaled_se = sqrt(`scaled_variance')
file write estimates "scaled,tau," %24.17f (`scaled_estimate') "," %24.17f (`scaled_se') "," %24.17f (`scaled_estimate' - 1.959963984540054 * `scaled_se') "," %24.17f (`scaled_estimate' + 1.959963984540054 * `scaled_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nts, 1, 1)) _n
file write cov "scaled,tau,tau," %24.17f (`scaled_variance') _n

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_untreated) minn(0) cluster(unit)
matrix bu = e(b)
matrix Vu = e(V)
matrix Ntu = e(Nt)
local untreated_support_estimate = el(bu, 1, 1)
local untreated_support_variance = el(Vu, 1, 1)
local untreated_support_se = sqrt(`untreated_support_variance')
file write estimates "untreated_support,tau," %24.17f (`untreated_support_estimate') "," %24.17f (`untreated_support_se') "," %24.17f (`untreated_support_estimate' - 1.959963984540054 * `untreated_support_se') "," %24.17f (`untreated_support_estimate' + 1.959963984540054 * `untreated_support_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Ntu, 1, 1)) _n
file write cov "untreated_support,tau,tau," %24.17f (`untreated_support_variance') _n
gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-untreated-support.csv", replace
restore

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_missing) minn(0) cluster(unit)
matrix bm = e(b)
matrix Vm = e(V)
matrix Ntm = e(Nt)
local missing_estimate = el(bm, 1, 1)
local missing_variance = el(Vm, 1, 1)
local missing_se = sqrt(`missing_variance')
file write estimates "missing,tau," %24.17f (`missing_estimate') "," %24.17f (`missing_se') "," %24.17f (`missing_estimate' - 1.959963984540054 * `missing_se') "," %24.17f (`missing_estimate' + 1.959963984540054 * `missing_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Ntm, 1, 1)) _n
file write cov "missing,tau,tau," %24.17f (`missing_variance') _n
gen byte sample = e(sample)
count if row_id == "2_3" & sample == 0
local missing_row_excluded = r(N)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-missing.csv", replace
restore

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_base wtr_alt) minn(0) cluster(unit)
matrix bmulti = e(b)
matrix Vmulti = e(V)
matrix Ntmulti = e(Nt)
local multi_terms : colnames bmulti
forvalues j = 1/`=colsof(bmulti)' {
    local term : word `j' of `multi_terms'
    local estimate = el(bmulti, 1, `j')
    local variance = el(Vmulti, `j', `j')
    local std_error = sqrt(`variance')
    file write estimates "multiple,`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Ntmulti, 1, `j')) _n
}
local multi_rows : rownames Vmulti
local multi_cols : colnames Vmulti
forvalues r = 1/`=rowsof(Vmulti)' {
    local row_term : word `r' of `multi_rows'
    forvalues c = 1/`=colsof(Vmulti)' {
        local col_term : word `c' of `multi_cols'
        file write cov "multiple,`row_term',`col_term'," %24.17f (el(Vmulti, `r', `c')) _n
    }
}
local multiple_estimate_1 = el(bmulti, 1, 1)
local multiple_estimate_2 = el(bmulti, 1, 2)

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_sum_zero) sum minn(0) cluster(unit)
matrix bz = e(b)
matrix Vz = e(V)
matrix Ntz = e(Nt)
local sum_zero_estimate = el(bz, 1, 1)
local sum_zero_variance = el(Vz, 1, 1)
local sum_zero_se = sqrt(`sum_zero_variance')
file write estimates "sum_zero,tau," %24.17f (`sum_zero_estimate') "," %24.17f (`sum_zero_se') "," %24.17f (`sum_zero_estimate' - 1.959963984540054 * `sum_zero_se') "," %24.17f (`sum_zero_estimate' + 1.959963984540054 * `sum_zero_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Ntz, 1, 1)) _n
file write cov "sum_zero,tau,tau," %24.17f (`sum_zero_variance') _n

use `base', clear
did_imputation Y unit t Ei [aw=w], wtr(wtr_zero) minn(0) cluster(unit)
matrix bzero = e(b)
matrix Vzero = e(V)
matrix Ntzero = e(Nt)
local zero_estimate = el(bzero, 1, 1)
local zero_variance = el(Vzero, 1, 1)
local zero_se = sqrt(`zero_variance')
file write estimates "zero,tau," %24.17f (`zero_estimate') "," %24.17f (`zero_se') "," %24.17f (`zero_estimate' - 1.959963984540054 * `zero_se') "," %24.17f (`zero_estimate' + 1.959963984540054 * `zero_se') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Ntzero, 1, 1)) _n
file write cov "zero,tau,tau," %24.17f (`zero_variance') _n
gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-zero.csv", replace
restore

file close estimates
file close cov

use `base', clear
gen byte treated_obs = Ei < . & t >= Ei
gen double raw_wtr_sum_zero = treated_obs * w * wtr_sum_zero
gen double raw_wtr_sum_zero_abs = abs(raw_wtr_sum_zero)
egen double raw_wtr_sum_zero_sum = total(raw_wtr_sum_zero)
egen double raw_wtr_sum_zero_abs_sum = total(raw_wtr_sum_zero_abs)
summarize raw_wtr_sum_zero_sum, meanonly
local raw_sum_zero = r(mean)
summarize raw_wtr_sum_zero_abs_sum, meanonly
local raw_abs_sum_zero = r(mean)

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], wtr(wtr_base wtr_base) minn(0) cluster(unit)
local duplicate_rc = _rc
local duplicate_status = cond(`duplicate_rc' == 0, "reference_success", "reference_error")

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], wtr(wtr_zero) minn(0) cluster(unit)
local zero_rc = _rc
local zero_status = cond(`zero_rc' == 0, "reference_success", "reference_error")

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], wtr(wtr_negative) minn(0) cluster(unit)
local negative_rc = _rc
local negative_status = cond(`negative_rc' == 0, "reference_success", "reference_error")

file open invalid using "`output_dir'/invalid-probes.json", write replace
file write invalid "{" _n
file write invalid `"  "duplicate_names": {"status": "`duplicate_status'", "return_code": "' %21.17g (`duplicate_rc') "}," _n
file write invalid `"  "zero_treated_weight": {"status": "`zero_status'", "return_code": "' %21.17g (`zero_rc') "}," _n
file write invalid `"  "negative_without_sum": {"status": "`negative_status'", "return_code": "' %21.17g (`negative_rc') "}" _n
file write invalid "}" _n
file close invalid

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "base_command": "did_imputation Y unit t Ei [aw=w], wtr(wtr_base) minn(0) cluster(unit)","' _n
file write diag `"  "scaled_command": "did_imputation Y unit t Ei [aw=w], wtr(wtr_scaled) minn(0) cluster(unit)","' _n
file write diag `"  "base_estimate": "' %24.17f (`base_estimate') "," _n
file write diag `"  "scaled_estimate": "' %24.17f (`scaled_estimate') "," _n
file write diag `"  "untreated_support_estimate": "' %24.17f (`untreated_support_estimate') "," _n
file write diag `"  "missing_estimate": "' %24.17f (`missing_estimate') "," _n
file write diag `"  "multiple_estimates": ["' %24.17f (`multiple_estimate_1') "," %24.17f (`multiple_estimate_2') "]," _n
file write diag `"  "sum_zero_estimate": "' %24.17f (`sum_zero_estimate') "," _n
file write diag `"  "zero_estimate": "' %24.17f (`zero_estimate') "," _n
file write diag `"  "estimate_scale_abs_diff": "' %24.17f (abs(`base_estimate' - `scaled_estimate')) "," _n
file write diag `"  "untreated_support_abs_diff": "' %24.17f (abs(`base_estimate' - `untreated_support_estimate')) "," _n
file write diag `"  "missing_row_id": "2_3","' _n
file write diag `"  "missing_row_excluded": "' %21.17g (`missing_row_excluded') "," _n
file write diag `"  "sum_zero_raw_weight_sum": "' %24.17f (`raw_sum_zero') "," _n
file write diag `"  "sum_zero_raw_abs_weight_sum": "' %24.17f (`raw_abs_sum_zero') _n
file write diag "}" _n
file close diag

display "F029_STATA_EXPORT_OK=1"
log close
exit, clear
