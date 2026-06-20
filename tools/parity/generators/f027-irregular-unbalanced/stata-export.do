version 14.2

args input_csv duplicate_csv output_dir ado_root

if "`input_csv'" == "" {
    display as error "usage: stata -b do stata-export.do <input_csv> <duplicate_csv> <output_dir> [ado_root]"
    exit 198
}
if "`duplicate_csv'" == "" {
    display as error "usage: stata -b do stata-export.do <input_csv> <duplicate_csv> <output_dir> [ado_root]"
    exit 198
}
if "`output_dir'" == "" {
    display as error "usage: stata -b do stata-export.do <input_csv> <duplicate_csv> <output_dir> [ado_root]"
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

display "F027_STATA_INPUT=`input_csv'"
display "F027_STATA_DUPLICATE_INPUT=`duplicate_csv'"
display "F027_STATA_OUTPUT=`output_dir'"
display "F027_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

did_imputation Y unit t Ei, minn(0) cluster(unit) maxit(1000)

matrix b = e(b)
matrix V = e(V)
matrix Nt = e(Nt)

local bcols : colnames b
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

gen byte sample = e(sample)
preserve
keep row_id sample
export delimited using "`output_dir'/sample-mask.csv", replace
restore

preserve
sort unit t
by unit: gen n_rows = _N
by unit: egen min_t = min(t)
by unit: egen max_t = max(t)
gen byte has_gap = n_rows < (max_t - min_t + 1)
by unit: keep if _n == 1
keep unit n_rows min_t max_t has_gap
export delimited using "`output_dir'/panel-structure.csv", replace
restore

local estimate = el(b, 1, 1)
local std_error = sqrt(el(V, 1, 1))
local n_obs = e(N)
local n_control = e(Nc)
local n_treated = el(Nt, 1, 1)

import delimited using "`duplicate_csv'", clear varnames(1) stringcols(1) case(preserve)
duplicates tag unit t, gen(duplicate_unit_time)
count if duplicate_unit_time > 0
local duplicate_rows = r(N)
capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit) maxit(1000)
local duplicate_rc = _rc
local duplicate_status = cond(`duplicate_rc' == 0, "reference_success_with_duplicates", "reference_error")
local duplicate_estimate = .
if `duplicate_rc' == 0 {
    matrix dup_b = e(b)
    local duplicate_estimate = el(dup_b, 1, 1)
}

file open dup using "`output_dir'/duplicate-probe.json", write replace
file write dup "{" _n
file write dup `"  "status": "`duplicate_status'","' _n
file write dup `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) maxit(1000)", "' _n
file write dup `"  "return_code": "' %21.17g (`duplicate_rc') "," _n
file write dup `"  "duplicate_unit_time_rows": "' %21.17g (`duplicate_rows') "," _n
file write dup `"  "estimate": "' %21.17g (`duplicate_estimate') _n
file write dup "}" _n
file close dup

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) maxit(1000)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "n_obs": "' %21.17g (`n_obs') "," _n
file write diag `"  "n_control": "' %21.17g (`n_control') "," _n
file write diag `"  "n_treated": "' %21.17g (`n_treated') "," _n
file write diag `"  "algebraic_att": "' %21.17g (2) "," _n
file write diag `"  "estimate": "' %21.17g (`estimate') "," _n
file write diag `"  "std_error": "' %21.17g (`std_error') _n
file write diag "}" _n
file close diag

display "F027_STATA_EXPORT_OK=1"
log close
exit, clear
