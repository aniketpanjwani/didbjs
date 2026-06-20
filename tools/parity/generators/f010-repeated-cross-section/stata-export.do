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

display "F010_STATA_INPUT=`input_csv'"
display "F010_STATA_OUTPUT=`output_dir'"
display "F010_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

egen byte person_tag = tag(person)
quietly count if person_tag
local unique_persons = r(N)
drop person_tag
egen byte region_tag = tag(region)
quietly count if region_tag
local unique_regions = r(N)
drop region_tag
quietly count
local total_rows = r(N)
quietly summarize tau if D == 1, meanonly
local algebraic_att = r(mean)

did_imputation Y person t Ei, fe(region t) minn(0) cluster(region) saveweights

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)
local bcols : colnames b
local vrows : rownames V
local vcols : colnames V
local term_count = colsof(b)

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local estimate = el(b, 1, `idx')
    local std_error = sqrt(el(V, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `idx')) _n
}
file close estimates

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

gen byte sample = e(sample)
file open mask using "`output_dir'/sample-mask.csv", write replace
file write mask "row_id,sample" _n
forvalues r = 1/`=_N' {
    file write mask "`=row_id[`r']'," %21.17g (sample[`r']) _n
}
file close mask

file open weights using "`output_dir'/imputation-weights.csv", write replace
file write weights "row_id,weight" _n
forvalues r = 1/`=_N' {
    file write weights "`=row_id[`r']'," %21.17g (__w_tau[`r']) _n
}
file close weights

local tau_estimate = el(b, 1, 1)
local tau_se = sqrt(el(V, 1, 1))
local n_obs = e(N)
local n_control = e(Nc)
local n_treated = el(Nt, 1, 1)
local algebraic_gap = `tau_estimate' - `algebraic_att'

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "command": "did_imputation Y person t Ei, fe(region t) minn(0) cluster(region) saveweights","' _n
file write diag `"  "unit_var": "person","' _n
file write diag `"  "fe": ["region", "t"],"' _n
file write diag `"  "cluster": "region","' _n
file write diag `"  "total_rows": "' %21.17g (`total_rows') "," _n
file write diag `"  "unique_persons": "' %21.17g (`unique_persons') "," _n
file write diag `"  "unique_regions": "' %21.17g (`unique_regions') "," _n
file write diag `"  "algebraic_att": "' %21.17f (`algebraic_att') "," _n
file write diag `"  "tau_estimate": "' %21.17f (`tau_estimate') "," _n
file write diag `"  "tau_std_error": "' %21.17f (`tau_se') "," _n
file write diag `"  "algebraic_gap": "' %21.17f (`algebraic_gap') "," _n
file write diag `"  "n_obs": "' %21.17g (`n_obs') "," _n
file write diag `"  "n_control": "' %21.17g (`n_control') "," _n
file write diag `"  "n_treated": "' %21.17g (`n_treated') _n
file write diag "}" _n
file close diag

display "F010_STATA_EXPORT_OK=1"
log close
exit, clear
