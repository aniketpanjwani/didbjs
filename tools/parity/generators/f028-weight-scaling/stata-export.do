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

display "F028_STATA_INPUT=`input_csv'"
display "F028_STATA_OUTPUT=`output_dir'"
display "F028_STATA_ADO_ROOT=`ado_root'"
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
did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local base_estimate = el(b, 1, 1)
local base_variance = el(V, 1, 1)
local base_se = sqrt(`base_variance')
local base_n = e(N)
local base_nc = e(Nc)
local base_nt = el(Nt, 1, 1)
file write estimates "base,tau," %21.17g (`base_estimate') "," %21.17g (`base_se') "," %21.17g (`base_estimate' - 1.959963984540054 * `base_se') "," %21.17g (`base_estimate' + 1.959963984540054 * `base_se') "," %21.17g (`base_n') "," %21.17g (`base_nc') "," %21.17g (`base_nt') _n
file write cov "base,tau,tau," %21.17g (`base_variance') _n
gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-base.csv", replace
restore

use `base', clear
did_imputation Y unit t Ei [aw=w_scaled], minn(0) cluster(unit)
matrix bs = e(b)
matrix Vs = e(V)
matrix Nts = e(Nt)
local scaled_estimate = el(bs, 1, 1)
local scaled_variance = el(Vs, 1, 1)
local scaled_se = sqrt(`scaled_variance')
local scaled_n = e(N)
local scaled_nc = e(Nc)
local scaled_nt = el(Nts, 1, 1)
file write estimates "scaled,tau," %21.17g (`scaled_estimate') "," %21.17g (`scaled_se') "," %21.17g (`scaled_estimate' - 1.959963984540054 * `scaled_se') "," %21.17g (`scaled_estimate' + 1.959963984540054 * `scaled_se') "," %21.17g (`scaled_n') "," %21.17g (`scaled_nc') "," %21.17g (`scaled_nt') _n
file write cov "scaled,tau,tau," %21.17g (`scaled_variance') _n

file close estimates
file close cov

use `base', clear
replace w = . if row_id == "2_3"
did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
matrix bm = e(b)
gen byte sample = e(sample)
local missing_estimate = el(bm, 1, 1)
count if row_id == "2_3" & sample == 0
local missing_weight_row_excluded = r(N)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask-missing.csv", replace
restore

use `base', clear
replace w = 0 if row_id == "1_3"
capture noisily did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
local zero_rc = _rc
local zero_status = cond(`zero_rc' == 0, "reference_success", "reference_error")

use `base', clear
replace w = -1 if row_id == "1_3"
capture noisily did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
local negative_rc = _rc
local negative_status = cond(`negative_rc' == 0, "reference_success", "reference_error")

use `base', clear
replace w = 0
capture noisily did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
local all_zero_rc = _rc
local all_zero_status = cond(`all_zero_rc' == 0, "reference_success", "reference_error")

use `base', clear
capture noisily did_imputation Y unit t Ei [iw=w], minn(0) cluster(unit)
local iweight_rc = _rc
local iweight_status = cond(`iweight_rc' == 0, "reference_success", "reference_error")

use `base', clear
capture noisily did_imputation Y unit t Ei [fw=w], minn(0) cluster(unit)
local fweight_rc = _rc
local fweight_status = cond(`fweight_rc' == 0, "reference_success", "reference_error")

file open invalid using "`output_dir'/invalid-probes.json", write replace
file write invalid "{" _n
file write invalid `"  "zero_weight": {"status": "`zero_status'", "return_code": "' %21.17g (`zero_rc') "}," _n
file write invalid `"  "negative_weight": {"status": "`negative_status'", "return_code": "' %21.17g (`negative_rc') "}," _n
file write invalid `"  "all_zero_weight": {"status": "`all_zero_status'", "return_code": "' %21.17g (`all_zero_rc') "}," _n
file write invalid `"  "iweight": {"status": "`iweight_status'", "return_code": "' %21.17g (`iweight_rc') "}," _n
file write invalid `"  "fweight": {"status": "`fweight_status'", "return_code": "' %21.17g (`fweight_rc') "}" _n
file write invalid "}" _n
file close invalid

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "base_command": "did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)","' _n
file write diag `"  "scaled_command": "did_imputation Y unit t Ei [aw=w_scaled], minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "base_estimate": "' %24.17f (`base_estimate') "," _n
file write diag `"  "scaled_estimate": "' %24.17f (`scaled_estimate') "," _n
file write diag `"  "base_variance": "' %24.17f (`base_variance') "," _n
file write diag `"  "scaled_variance": "' %24.17f (`scaled_variance') "," _n
file write diag `"  "estimate_scale_abs_diff": "' %24.17f (abs(`base_estimate' - `scaled_estimate')) "," _n
file write diag `"  "variance_scale_abs_diff": "' %24.17f (abs(`base_variance' - `scaled_variance')) "," _n
file write diag `"  "missing_weight_row_id": "2_3","' _n
file write diag `"  "missing_weight_row_excluded": "' %21.17g (`missing_weight_row_excluded') "," _n
file write diag `"  "missing_weight_estimate": "' %24.17f (`missing_estimate') _n
file write diag "}" _n
file close diag

display "F028_STATA_EXPORT_OK=1"
log close
exit, clear
