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

display "F005_STATA_INPUT=`input_csv'"
display "F005_STATA_OUTPUT=`output_dir'"
display "F005_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

gen byte treated_obs = Ei < . & t >= Ei
gen double raw_wtr_uniform = treated_obs * w * wtr_uniform
gen double raw_wtr_late = treated_obs * w * wtr_late
egen double denom_wtr_uniform = total(raw_wtr_uniform)
egen double denom_wtr_late = total(raw_wtr_late)
gen double norm_wtr_uniform = raw_wtr_uniform / denom_wtr_uniform
gen double norm_wtr_late = raw_wtr_late / denom_wtr_late
egen double algebraic_wtr_uniform = total(norm_wtr_uniform * tau)
egen double algebraic_wtr_late = total(norm_wtr_late * tau)
summarize algebraic_wtr_uniform, meanonly
local target_wtr_uniform = r(mean)
summarize algebraic_wtr_late, meanonly
local target_wtr_late = r(mean)
summarize norm_wtr_uniform, meanonly
local normalized_sum_wtr_uniform = r(sum)
summarize norm_wtr_late, meanonly
local normalized_sum_wtr_late = r(sum)
summarize denom_wtr_uniform, meanonly
local denom_wtr_uniform = r(mean)
summarize denom_wtr_late, meanonly
local denom_wtr_late = r(mean)

did_imputation Y unit t Ei [aw=w], wtr(wtr_uniform wtr_late) minn(0) cluster(unit)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)

local bcols : colnames b
forvalues j = 1/`=colsof(b)' {
    local term : word `j' of `bcols'
    local target = .
    if "`term'" == "tau_wtr_uniform" local target = `target_wtr_uniform'
    if "`term'" == "tau_wtr_late" local target = `target_wtr_late'
    local estimate = el(b, 1, `j')
    if abs(`estimate' - `target') > 1e-10 {
        display as error "F005 custom wtr assertion failed for `term': " %21.17g `estimate' " target " %21.17g `target'
        exit 459
    }
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

file open weights using "`output_dir'/normalized-weights.csv", write replace
file write weights "term,row_id,normalized_weight" _n
forvalues r = 1/`=_N' {
    file write weights "tau_wtr_uniform,`=row_id[`r']'," %21.17g (norm_wtr_uniform[`r']) _n
    file write weights "tau_wtr_late,`=row_id[`r']'," %21.17g (norm_wtr_late[`r']) _n
}
file close weights

gen byte sample = e(sample)
keep row_id sample
export delimited using "`output_dir'/sample-mask.csv", replace

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w], wtr(wtr_uniform wtr_late) minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "terms": ["tau_wtr_uniform", "tau_wtr_late"],"' _n
file write diag `"  "denom_wtr_uniform": "' %21.17g (`denom_wtr_uniform') "," _n
file write diag `"  "denom_wtr_late": "' %21.17g (`denom_wtr_late') "," _n
file write diag `"  "normalized_sum_wtr_uniform": "' %21.17g (`normalized_sum_wtr_uniform') "," _n
file write diag `"  "normalized_sum_wtr_late": "' %21.17g (`normalized_sum_wtr_late') "," _n
file write diag `"  "algebraic_wtr_uniform": "' %21.17g (`target_wtr_uniform') "," _n
file write diag `"  "algebraic_wtr_late": "' %21.17g (`target_wtr_late') "," _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated_tau_wtr_uniform": "' %21.17g (el(Nt, 1, 1)) "," _n
file write diag `"  "n_treated_tau_wtr_late": "' %21.17g (el(Nt, 1, 2)) _n
file write diag "}" _n
file close diag

display "F005_STATA_EXPORT_OK=1"
log close
exit, clear
