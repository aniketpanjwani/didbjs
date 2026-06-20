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

capture program drop f031_export_current
program define f031_export_current
    args scenario output_dir
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    local vrows : rownames V
    local vcols : colnames V

    file open estimates using "`output_dir'/estimates.csv", write append
    forvalues j = 1/`=colsof(b)' {
        local term : word `j' of `bcols'
        local estimate = el(b, 1, `j')
        local variance = el(V, `j', `j')
        local std_error = sqrt(`variance')
        if "`term'" == "tau" {
            file write estimates "`scenario',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, 1)) _n
        }
        else {
            file write estimates "`scenario',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," _n
        }
    }
    file close estimates

    file open cov using "`output_dir'/covariance.csv", write append
    forvalues r = 1/`=rowsof(V)' {
        local row_term : word `r' of `vrows'
        forvalues c = 1/`=colsof(V)' {
            local col_term : word `c' of `vcols'
            file write cov "`scenario',`row_term',`col_term'," %24.17f (el(V, `r', `c')) _n
        }
    }
    file close cov

    gen byte sample = e(sample)
    preserve
    keep row_id sample
    export delimited using "`output_dir'/sample-mask-`scenario'.csv", replace
    restore
    drop sample
end

log using "`output_dir'/run.log", text replace

display "F031_STATA_INPUT=`input_csv'"
display "F031_STATA_OUTPUT=`output_dir'"
display "F031_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
file close estimates
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

use `base', clear
capture noisily did_imputation Y unit t Ei, fe(group unit t) minn(0) cluster(unit)
local nested_rc = _rc
local nested_status = cond(`nested_rc' == 0, "reference_success", "reference_error")
if `nested_rc' == 0 {
    f031_export_current nested_fe "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei, fe(group group_dup t) minn(0) cluster(unit)
local duplicate_fe_rc = _rc
local duplicate_fe_status = cond(`duplicate_fe_rc' == 0, "reference_success", "reference_error")
if `duplicate_fe_rc' == 0 {
    f031_export_current duplicate_fe "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei, fe(singleton_fe t) minn(0) cluster(unit)
local singleton_rc = _rc
local singleton_status = cond(`singleton_rc' == 0, "reference_success", "reference_error")
if `singleton_rc' == 0 {
    f031_export_current singleton_fe "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei if disc_keep == 1, fe(unit disc_time) minn(0) cluster(unit)
local disconnected_rc = _rc
local disconnected_status = cond(`disconnected_rc' == 0, "reference_success", "reference_error")
if `disconnected_rc' == 0 {
    f031_export_current disconnected_fe "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei, controls(x_absorbed) fe(group t) minn(0) cluster(unit) maxit(1000)
local absorbed_rc = _rc
local absorbed_status = cond(`absorbed_rc' == 0, "reference_success", "reference_error")
if `absorbed_rc' == 0 {
    f031_export_current absorbed_control "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei, controls(x_treated_only) fe(unit t) minn(0) cluster(unit) maxit(1000)
local untreated_rank_rc = _rc
local untreated_rank_status = cond(`untreated_rank_rc' == 0, "reference_success", "reference_error")
if `untreated_rank_rc' == 0 {
    f031_export_current untreated_rank "`output_dir'"
}

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "nested_fe": {"status": "`nested_status'", "return_code": "' %21.17g (`nested_rc') "}," _n
file write probes `"  "duplicate_fe": {"status": "`duplicate_fe_status'", "return_code": "' %21.17g (`duplicate_fe_rc') "}," _n
file write probes `"  "singleton_fe": {"status": "`singleton_status'", "return_code": "' %21.17g (`singleton_rc') "}," _n
file write probes `"  "disconnected_fe": {"status": "`disconnected_status'", "return_code": "' %21.17g (`disconnected_rc') "}," _n
file write probes `"  "absorbed_control": {"status": "`absorbed_status'", "return_code": "' %21.17g (`absorbed_rc') "}," _n
file write probes `"  "untreated_rank": {"status": "`untreated_rank_status'", "return_code": "' %21.17g (`untreated_rank_rc') "}" _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata did_imputation FE rank probes","' _n
file write diag `"  "success_scenarios": "nested_fe,duplicate_fe,singleton_fe,disconnected_fe,absorbed_control","' _n
file write diag `"  "failure_scenarios": "untreated_rank","' _n
local scenario_count = 6
file write diag `"  "scenario_count": "' %21.17g (`scenario_count') _n
file write diag "}" _n
file close diag

display "F031_STATA_EXPORT_OK=1"
log close
exit, clear
