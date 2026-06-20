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

display "F007_STATA_INPUT=`input_csv'"
display "F007_STATA_OUTPUT=`output_dir'"
display "F007_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

gen byte treated_obs = Ei < . & t >= Ei
gen double raw_att_weight = treated_obs * w
egen double denom_att_weight = total(raw_att_weight)
gen double normalized_att_weight = raw_att_weight / denom_att_weight
summarize normalized_att_weight, meanonly
local normalized_sum = r(sum)
summarize denom_att_weight, meanonly
local treated_weight_sum = r(mean)

did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local weighted_estimate = el(b, 1, 1)

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
local bcols : colnames b
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
    file write weights "tau,`=row_id[`r']'," %21.17g (normalized_att_weight[`r']) _n
}
file close weights

gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask.csv", replace
restore

did_imputation Y unit t Ei, minn(0) cluster(unit)
matrix b_unweighted = e(b)
local unweighted_estimate = el(b_unweighted, 1, 1)
local weighted_unweighted_abs_diff = abs(`weighted_estimate' - `unweighted_estimate')
if `weighted_unweighted_abs_diff' < 1e-6 {
    display as error "F007 analytic weight probe did not change the estimate enough"
    exit 459
}

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w], minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "terms": ["tau"],"' _n
file write diag `"  "treated_weight_sum": "' %21.17g (`treated_weight_sum') "," _n
file write diag `"  "normalized_weight_sum": "' %21.17g (`normalized_sum') "," _n
file write diag `"  "weighted_estimate": "' %21.17g (`weighted_estimate') "," _n
file write diag `"  "unweighted_estimate": "' %21.17g (`unweighted_estimate') "," _n
file write diag `"  "weighted_unweighted_abs_diff": "' %21.17f (`weighted_unweighted_abs_diff') "," _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated": "' %21.17g (el(Nt, 1, 1)) _n
file write diag "}" _n
file close diag

display "F007_STATA_EXPORT_OK=1"
log close
exit, clear
