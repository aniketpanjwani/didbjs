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

capture program drop f033_export_success
program define f033_export_success
    args scenario cluster output_dir
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    local vrows : rownames V
    local vcols : colnames V

    quietly levelsof `cluster' if e(sample), local(cluster_levels)
    local cluster_count : word count `cluster_levels'

    file open estimates using "`output_dir'/estimates.csv", write append
    forvalues j = 1/`=colsof(b)' {
        local term : word `j' of `bcols'
        local estimate = el(b, 1, `j')
        local variance = el(V, `j', `j')
        local std_error = sqrt(`variance')
        file write estimates "`scenario',`cluster',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `j')) "," %21.17g (`cluster_count') _n
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
    export delimited using "`output_dir'/sample-`scenario'.csv", replace
    restore
    drop sample
end

log using "`output_dir'/run.log", text replace

display "F033_STATA_INPUT=`input_csv'"
display "F033_STATA_OUTPUT=`output_dir'"
display "F033_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,cluster,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated,n_clusters" _n
file close estimates
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

local scenarios "two alt singleton nested one missing"

foreach sc of local scenarios {
    use `base', clear
    local cluster "cluster_two"
    if "`sc'" == "alt" local cluster "cluster_alt"
    if "`sc'" == "singleton" local cluster "cluster_singleton"
    if "`sc'" == "nested" local cluster "cluster_nested"
    if "`sc'" == "one" local cluster "cluster_one"
    if "`sc'" == "missing" local cluster "cluster_missing"

    capture noisily did_imputation Y unit t Ei, minn(0) cluster(`cluster')
    local `sc'_rc = _rc
    local `sc'_status = cond(_rc == 0, "reference_success", "reference_error")
    if _rc == 0 {
        f033_export_success `sc' `cluster' "`output_dir'"
    }
}

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "two": {"status": "`two_status'", "return_code": "' %21.17g (`two_rc') `", "cluster": "cluster_two"},"' _n
file write probes `"  "alt": {"status": "`alt_status'", "return_code": "' %21.17g (`alt_rc') `", "cluster": "cluster_alt"},"' _n
file write probes `"  "singleton": {"status": "`singleton_status'", "return_code": "' %21.17g (`singleton_rc') `", "cluster": "cluster_singleton"},"' _n
file write probes `"  "nested": {"status": "`nested_status'", "return_code": "' %21.17g (`nested_rc') `", "cluster": "cluster_nested"},"' _n
file write probes `"  "one": {"status": "`one_status'", "return_code": "' %21.17g (`one_rc') `", "cluster": "cluster_one"},"' _n
file write probes `"  "missing": {"status": "`missing_status'", "return_code": "' %21.17g (`missing_rc') `", "cluster": "cluster_missing"}"' _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata did_imputation cluster and finite-sample probes","' _n
file write diag `"  "success_scenarios": "two,alt,singleton,nested,missing","' _n
file write diag `"  "failure_scenarios": "one","' _n
file write diag `"  "scenario_count": 6"' _n
file write diag "}" _n
file close diag

display "F033_STATA_EXPORT_OK=1"
log close
exit, clear
