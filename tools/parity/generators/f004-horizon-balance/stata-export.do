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

display "F004_STATA_INPUT=`input_csv'"
display "F004_STATA_OUTPUT=`output_dir'"
display "F004_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

did_imputation Y unit t Ei [aw=w], horizons(0/2) hbalance minn(0) cluster(unit)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)

local bcols : colnames b
forvalues j = 1/`=colsof(b)' {
    local term : word `j' of `bcols'
    local target = .
    if "`term'" == "tau0" local target = 1
    if "`term'" == "tau1" local target = 2
    if "`term'" == "tau2" local target = 3
    local estimate = el(b, 1, `j')
    if abs(`estimate' - `target') > 1e-10 {
        display as error "F004 hbalance ATT assertion failed for `term': " %21.17g `estimate'
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

gen K = t - Ei
gen byte treated_unit = Ei < .
gen byte in_requested_horizon = treated_unit & inlist(K, 0, 1, 2)
bys unit: egen requested_horizon_count = total(in_requested_horizon)
gen byte hbalance_included = treated_unit & requested_horizon_count == 3
preserve
keep unit treated_unit requested_horizon_count hbalance_included
duplicates drop
sort unit
export delimited using "`output_dir'/hbalance-units.csv", replace
restore

gen byte sample = e(sample)
keep row_id sample
export delimited using "`output_dir'/sample-mask.csv", replace

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w], horizons(0/2) hbalance minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "horizons": [0, 1, 2],"' _n
file write diag `"  "hbalance_included_units": [1, 2, 3],"' _n
file write diag `"  "hbalance_excluded_units": [4],"' _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated_tau0": "' %21.17g (el(Nt, 1, 1)) "," _n
file write diag `"  "n_treated_tau1": "' %21.17g (el(Nt, 1, 2)) "," _n
file write diag `"  "n_treated_tau2": "' %21.17g (el(Nt, 1, 3)) _n
file write diag "}" _n
file close diag

display "F004_STATA_EXPORT_OK=1"
log close
exit, clear
