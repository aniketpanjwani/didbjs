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

capture program drop f039_write_results
program define f039_write_results
    args scenario input_path output_dir
    import delimited using "`input_path'", clear varnames(1) stringcols(1) case(preserve)
    did_imputation Y unit t Ei [aw=w], horizons(0/2) minn(0) cluster(unit) saveweights

    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    local vrows : rownames V
    local vcols : colnames V
    local term_count = colsof(b)

    file open estimates using "`output_dir'/estimates.csv", write append
    forvalues idx = 1/`term_count' {
        local term : word `idx' of `bcols'
        local estimate = el(b, 1, `idx')
        local std_error = sqrt(el(V, `idx', `idx'))
        local conf_low = `estimate' - 1.959963984540054 * `std_error'
        local conf_high = `estimate' + 1.959963984540054 * `std_error'
        file write estimates "`scenario',`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) _n
    }
    file close estimates

    file open cov using "`output_dir'/covariance.csv", write append
    forvalues r = 1/`=rowsof(V)' {
        local row_term : word `r' of `vrows'
        forvalues c = 1/`=colsof(V)' {
            local col_term : word `c' of `vcols'
            file write cov "`scenario',`row_term',`col_term'," %21.17g (el(V, `r', `c')) _n
        }
    }
    file close cov

    gen byte sample = e(sample)
    file open mask using "`output_dir'/sample-`scenario'.csv", write replace
    file write mask "row_id,sample" _n
    forvalues r = 1/`=_N' {
        file write mask "`=row_id[`r']'," %21.17g (sample[`r']) _n
    }
    file close mask

    file open weights using "`output_dir'/weights-dense.csv", write append
    file open sparse using "`output_dir'/weights-sparse.csv", write append
    foreach term in tau0 tau1 tau2 {
        forvalues r = 1/`=_N' {
            local w = __w_`term'[`r']
            file write weights "`scenario',`=row_id[`r']',`term'," %21.17g (`w') _n
            if abs(`w') > 1e-12 {
                file write sparse "`scenario',`=row_id[`r']',`term'," %21.17g (`w') _n
            }
        }
    }
    file close weights
    file close sparse

    rename __w_tau0 saved_tau0
    rename __w_tau1 saved_tau1
    rename __w_tau2 saved_tau2
    did_imputation Y2 unit t Ei [aw=w], horizons(0/2) minn(0) cluster(unit) loadweights(saved_tau0 saved_tau1 saved_tau2)
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    file open load_estimates using "`output_dir'/load-estimates.csv", write append
    forvalues idx = 1/`=colsof(b)' {
        local term : word `idx' of `bcols'
        local estimate = el(b, 1, `idx')
        local std_error = sqrt(el(V, `idx', `idx'))
        file write load_estimates "`scenario',`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) _n
    }
    file close load_estimates
end

log using "`output_dir'/run.log", text replace

display "F039_STATA_INPUT_DIR=`input_dir'"
display "F039_STATA_OUTPUT=`output_dir'"
display "F039_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)
which did_imputation

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
file close estimates

file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

file open weights using "`output_dir'/weights-dense.csv", write replace
file write weights "scenario,row_id,term,weight" _n
file close weights

file open sparse using "`output_dir'/weights-sparse.csv", write replace
file write sparse "scenario,row_id,term,weight" _n
file close sparse

file open load_estimates using "`output_dir'/load-estimates.csv", write replace
file write load_estimates "scenario,term,estimate,std_error,n_obs,n_control,n_treated" _n
file close load_estimates

f039_write_results base "`input_dir'/base.csv" "`output_dir'"
f039_write_results reordered "`input_dir'/reordered.csv" "`output_dir'"

preserve
import delimited using "`output_dir'/weights-dense.csv", clear varnames(1) case(preserve)
count
local dense_count = r(N)
count if abs(weight) > 1e-12
local dense_nonzero = r(N)
restore

preserve
import delimited using "`output_dir'/weights-sparse.csv", clear varnames(1) case(preserve)
count
local sparse_count = r(N)
restore

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "save_command": "did_imputation Y unit t Ei [aw=w], horizons(0/2) minn(0) cluster(unit) saveweights","' _n
file write diag `"  "load_command": "did_imputation Y2 unit t Ei [aw=w], horizons(0/2) minn(0) cluster(unit) loadweights(saved_tau0 saved_tau1 saved_tau2)","' _n
file write diag `"  "dense_row_count": "' %21.17g (`dense_count') "," _n
file write diag `"  "dense_nonzero_count": "' %21.17g (`dense_nonzero') "," _n
file write diag `"  "sparse_row_count": "' %21.17g (`sparse_count') _n
file write diag "}" _n
file close diag

display "F039_STATA_EXPORT_OK=1"
log close
exit, clear
