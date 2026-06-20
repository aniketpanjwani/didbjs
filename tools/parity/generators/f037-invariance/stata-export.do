version 14.2

args input_dir output_dir ado_root

if "`input_dir'" == "" {
    display as error "usage: stata -b do stata-export.do <input_dir> <output_dir> [ado_root]"
    exit 198
}
if "`output_dir'" == "" {
    display as error "usage: stata -b do stata-export.do <input_dir> <output_dir> [ado_root]"
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

capture program drop f037_run
program define f037_run
    args scenario input_path output_dir
    import delimited using "`input_path'", clear varnames(1) stringcols(1) case(preserve)
    did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local estimate = el(b, 1, 1)
    local variance = el(V, 1, 1)
    local std_error = sqrt(`variance')
    file open estimates using "`output_dir'/estimates.csv", write append
    file write estimates "`scenario',tau," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`variance') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, 1)) _n
    file close estimates
    gen byte sample = e(sample)
    preserve
    keep row_id sample
    export delimited using "`output_dir'/sample-`scenario'.csv", replace
    restore
end

log using "`output_dir'/run.log", text replace

display "F037_STATA_INPUT_DIR=`input_dir'"
display "F037_STATA_OUTPUT=`output_dir'"
display "F037_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)
which did_imputation

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,variance,n_obs,n_control,n_treated" _n
file close estimates

local scenarios "base row_permuted unit_relabel time_shift outcome_scaled constant_shift weight_scaled"
foreach scenario of local scenarios {
    f037_run `scenario' "`input_dir'/`scenario'.csv" "`output_dir'"
}

preserve
import delimited using "`output_dir'/estimates.csv", clear varnames(1) case(preserve)
summarize estimate if scenario == "base", meanonly
local base_estimate = r(mean)
summarize std_error if scenario == "base", meanonly
local base_se = r(mean)
summarize variance if scenario == "base", meanonly
local base_variance = r(mean)
summarize estimate if scenario == "outcome_scaled", meanonly
local scaled_estimate = r(mean)
summarize std_error if scenario == "outcome_scaled", meanonly
local scaled_se = r(mean)
summarize variance if scenario == "outcome_scaled", meanonly
local scaled_variance = r(mean)
summarize estimate if scenario == "row_permuted", meanonly
local row_permuted_estimate = r(mean)
summarize estimate if scenario == "unit_relabel", meanonly
local unit_relabel_estimate = r(mean)
summarize estimate if scenario == "time_shift", meanonly
local time_shift_estimate = r(mean)
summarize estimate if scenario == "constant_shift", meanonly
local constant_shift_estimate = r(mean)
summarize estimate if scenario == "weight_scaled", meanonly
local weight_scaled_estimate = r(mean)
restore

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "base_estimate": "' %24.17f (`base_estimate') "," _n
file write diag `"  "row_permutation_abs_diff": "' %24.17f (abs(`row_permuted_estimate' - `base_estimate')) "," _n
file write diag `"  "unit_relabel_abs_diff": "' %24.17f (abs(`unit_relabel_estimate' - `base_estimate')) "," _n
file write diag `"  "time_shift_abs_diff": "' %24.17f (abs(`time_shift_estimate' - `base_estimate')) "," _n
file write diag `"  "constant_shift_abs_diff": "' %24.17f (abs(`constant_shift_estimate' - `base_estimate')) "," _n
file write diag `"  "weight_scale_abs_diff": "' %24.17f (abs(`weight_scaled_estimate' - `base_estimate')) "," _n
file write diag `"  "outcome_scale": 3.5,"' _n
file write diag `"  "outcome_scaled_estimate_ratio": "' %24.17f (`scaled_estimate' / `base_estimate') "," _n
file write diag `"  "outcome_scaled_se_ratio": "' %24.17f (`scaled_se' / `base_se') "," _n
file write diag `"  "outcome_scaled_variance_ratio": "' %24.17f (`scaled_variance' / `base_variance') _n
file write diag "}" _n
file close diag

display "F037_STATA_EXPORT_OK=1"
log close
exit, clear
