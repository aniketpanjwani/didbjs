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

capture program drop f035_export_success
program define f035_export_success
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
        local n_treated = .
        if `j' <= colsof(Nt) local n_treated = el(Nt, 1, `j')
        file write estimates "`scenario',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`variance') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (`n_treated') _n
    }
    file close estimates

    file open order using "`output_dir'/matrix-order.csv", write append
    forvalues j = 1/`=colsof(b)' {
        local bterm : word `j' of `bcols'
        local vrow : word `j' of `vrows'
        local vcol : word `j' of `vcols'
        file write order "`scenario'," %21.17g (`j') ",`bterm',`vrow',`vcol'" _n
    }
    file close order

    file open cov using "`output_dir'/covariance.csv", write append
    forvalues r = 1/`=rowsof(V)' {
        local row_term : word `r' of `vrows'
        forvalues c = 1/`=colsof(V)' {
            local col_term : word `c' of `vcols'
            file write cov "`scenario'," %21.17g (`r') "," %21.17g (`c') ",`row_term',`col_term'," %24.17f (el(V, `r', `c')) _n
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

display "F035_STATA_INPUT=`input_csv'"
display "F035_STATA_OUTPUT=`output_dir'"
display "F035_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,variance,conf_low,conf_high,n_obs,n_control,n_treated" _n
file close estimates
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_index,col_index,row_term,col_term,value" _n
file close cov
file open order using "`output_dir'/matrix-order.csv", write replace
file write order "scenario,position,b_term,v_row_term,v_col_term" _n
file close order

local full_pre_F = .
local full_pre_p = .
local full_pre_df = .
local singular_pre_F = .
local singular_pre_p = .
local singular_pre_df = .

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei [aw=w], controls(x1 x2) horizons(0/1) pretrends(2) minn(0) cluster(unit) maxit(1000)
local full_rc = _rc
local full_status = cond(`full_rc' == 0, "reference_success", "reference_error")
if `full_rc' == 0 {
    local full_pre_F = e(pre_F)
    local full_pre_p = e(pre_p)
    local full_pre_df = e(pre_df)
    f035_export_success full_order "`output_dir'"
}

use `base', clear
set seed 123456789
capture noisily did_imputation Y unit t Ei [aw=w], controls(x1 x2) horizons(0/1) pretrends(2) minn(0) cluster(cluster_pair) maxit(1000)
local singular_rc = _rc
local singular_status = cond(`singular_rc' == 0, "reference_success", "reference_error")
if `singular_rc' == 0 {
    local singular_pre_F = e(pre_F)
    local singular_pre_p = e(pre_p)
    local singular_pre_df = e(pre_df)
    f035_export_success singular_pretrend "`output_dir'"
}

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "full_order": {"status": "`full_status'", "return_code": "' %21.17g (`full_rc') `", "cluster": "unit"},"' _n
file write probes `"  "singular_pretrend": {"status": "`singular_status'", "return_code": "' %21.17g (`singular_rc') `", "cluster": "cluster_pair"}"' _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata covariance row/column stripe and singular pretrend probes","' _n
file write diag `"  "full_order_command": "did_imputation Y unit t Ei [aw=w], controls(x1 x2) horizons(0/1) pretrends(2) minn(0) cluster(unit) maxit(1000)","' _n
file write diag `"  "singular_pretrend_command": "did_imputation Y unit t Ei [aw=w], controls(x1 x2) horizons(0/1) pretrends(2) minn(0) cluster(cluster_pair) maxit(1000)","' _n
file write diag `"  "expected_order": ["tau0", "tau1", "pre1", "pre2", "x1", "x2"],"' _n
file write diag `"  "full_pre_F": "' %24.17f (`full_pre_F') "," _n
file write diag `"  "full_pre_p": "' %24.17f (`full_pre_p') "," _n
file write diag `"  "full_pre_df": "' %21.17g (`full_pre_df') "," _n
file write diag `"  "singular_pre_F": "' %24.17f (`singular_pre_F') "," _n
file write diag `"  "singular_pre_p": "' %24.17f (`singular_pre_p') "," _n
file write diag `"  "singular_pre_df": "' %21.17g (`singular_pre_df') _n
file write diag "}" _n
file close diag

display "F035_STATA_EXPORT_OK=1"
log close
exit, clear
