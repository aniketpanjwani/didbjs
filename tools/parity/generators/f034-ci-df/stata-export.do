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

capture program drop f034_export_success
program define f034_export_success
    args scenario alpha cluster output_dir
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    local vrows : rownames V
    local vcols : colnames V

    quietly levelsof `cluster' if e(sample), local(cluster_levels)
    local cluster_count : word count `cluster_levels'
    local df = `cluster_count' - 1
    local normal_critical = invnormal(1 - `alpha' / 2)
    local t_critical = invttail(`df', `alpha' / 2)
    local confidence_level = 100 * (1 - `alpha')

    file open estimates using "`output_dir'/estimates.csv", write append
    forvalues j = 1/`=colsof(b)' {
        local term : word `j' of `bcols'
        local estimate = el(b, 1, `j')
        local variance = el(V, `j', `j')
        local std_error = sqrt(`variance')
        local normal_low = `estimate' - `normal_critical' * `std_error'
        local normal_high = `estimate' + `normal_critical' * `std_error'
        file write estimates "`scenario'," %21.17g (`alpha') "," %21.17g (`confidence_level') ",`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`variance') "," %24.17f (`normal_low') "," %24.17f (`normal_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `j')) "," %21.17g (`cluster_count') "," %21.17g (`df') ",normal," %24.17f (`normal_critical') _n

        file open grid using "`output_dir'/ci-grid.csv", write append
        local t_low = `estimate' - `t_critical' * `std_error'
        local t_high = `estimate' + `t_critical' * `std_error'
        file write grid "`scenario'," %21.17g (`alpha') ",`term',normal,Inf," %24.17f (`normal_critical') "," %24.17f (`normal_low') "," %24.17f (`normal_high') ",1" _n
        file write grid "`scenario'," %21.17g (`alpha') ",`term',t_n_clusters_minus_1," %21.17g (`df') "," %24.17f (`t_critical') "," %24.17f (`t_low') "," %24.17f (`t_high') ",0" _n
        file close grid
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

display "F034_STATA_INPUT=`input_csv'"
display "F034_STATA_OUTPUT=`output_dir'"
display "F034_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,alpha,confidence_level,term,estimate,std_error,variance,conf_low,conf_high,n_obs,n_control,n_treated,n_clusters,df,critical_type,critical_value" _n
file close estimates
file open grid using "`output_dir'/ci-grid.csv", write replace
file write grid "scenario,alpha,term,critical_type,df,critical_value,conf_low,conf_high,used_by_reference" _n
file close grid
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei, minn(0) cluster(cluster_two)
local default_rc = _rc
local default_status = cond(_rc == 0, "reference_success", "reference_error")
if _rc == 0 {
    f034_export_success default 0.05 cluster_two "`output_dir'"
}

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei, minn(0) cluster(cluster_two) alpha(0.10)
local alpha10_rc = _rc
local alpha10_status = cond(_rc == 0, "reference_success", "reference_error")
if _rc == 0 {
    f034_export_success alpha10 0.10 cluster_two "`output_dir'"
}

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei, minn(0) cluster(cluster_two) alpha(0)
local alpha_zero_rc = _rc
local alpha_zero_status = cond(_rc == 0, "reference_success", "reference_error")

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei, minn(0) cluster(cluster_two) alpha(1)
local alpha_one_rc = _rc
local alpha_one_status = cond(_rc == 0, "reference_success", "reference_error")

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei, minn(0) cluster(cluster_two) alpha(-0.10)
local alpha_negative_rc = _rc
local alpha_negative_status = cond(_rc == 0, "reference_success", "reference_error")

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "default": {"status": "`default_status'", "return_code": "' %21.17g (`default_rc') `", "alpha": 0.05},"' _n
file write probes `"  "alpha10": {"status": "`alpha10_status'", "return_code": "' %21.17g (`alpha10_rc') `", "alpha": 0.10},"' _n
file write probes `"  "alpha_zero": {"status": "`alpha_zero_status'", "return_code": "' %21.17g (`alpha_zero_rc') `", "alpha": 0},"' _n
file write probes `"  "alpha_one": {"status": "`alpha_one_status'", "return_code": "' %21.17g (`alpha_one_rc') `", "alpha": 1},"' _n
file write probes `"  "alpha_negative": {"status": "`alpha_negative_status'", "return_code": "' %21.17g (`alpha_negative_rc') `", "alpha": -0.10}"' _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata did_imputation alpha option and ereturn display semantics","' _n
file write diag `"  "command_default": "did_imputation Y unit t Ei, minn(0) cluster(cluster_two)","' _n
file write diag `"  "command_alpha10": "did_imputation Y unit t Ei, minn(0) cluster(cluster_two) alpha(0.10)","' _n
file write diag `"  "alpha_option": "alpha(real 0.05)","' _n
file write diag `"  "confidence_level_formula": "100 * (1 - alpha)","' _n
file write diag `"  "ci_distribution": "normal","' _n
file write diag `"  "ci_degrees_of_freedom": "Inf","' _n
file write diag `"  "normal_critical_95": "' %21.17g (invnormal(0.975)) "," _n
file write diag `"  "normal_critical_90": "' %21.17g (invnormal(0.95)) "," _n
file write diag `"  "scenario_count": 2"' _n
file write diag "}" _n
file close diag

display "F034_STATA_EXPORT_OK=1"
log close
exit, clear
