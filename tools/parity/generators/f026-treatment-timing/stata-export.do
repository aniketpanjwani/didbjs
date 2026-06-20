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

display "F026_STATA_INPUT=`input_csv'"
display "F026_STATA_OUTPUT=`output_dir'"
display "F026_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(_all) case(preserve)
describe

destring unit t Y w, replace
gen double Ei_num = .
replace Ei_num = real(Ei) if Ei != "" & Ei != "Inf" & Ei != "."
drop Ei
rename Ei_num Ei

gen double expected_event_time = .
replace expected_event_time = t - Ei if Ei < .
gen byte expected_treated = Ei < . & t >= Ei
gen byte expected_untreated = !expected_treated
gen str18 timing_class = "in_sample"
replace timing_class = "missing_never" if Ei >= .
replace timing_class = "after_sample" if Ei < . & Ei > 5
replace timing_class = "before_sample_negative" if Ei < 1

did_imputation Y unit t Ei [aw=w], fe(.) minn(0) cluster(unit) maxit(1000)

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
keep row_id unit t Ei expected_event_time expected_treated expected_untreated timing_class
export delimited using "`output_dir'/timing-classification.csv", replace
restore

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w], fe(.) minn(0) cluster(unit) maxit(1000)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) "," _n
file write diag `"  "n_treated": "' %21.17g (el(Nt, 1, 1)) "," _n
count if timing_class == "missing_never"
file write diag `"  "missing_never_rows": "' %21.17g (r(N)) "," _n
count if timing_class == "after_sample"
file write diag `"  "after_sample_rows": "' %21.17g (r(N)) "," _n
count if timing_class == "before_sample_negative"
file write diag `"  "before_sample_negative_rows": "' %21.17g (r(N)) _n
file write diag "}" _n
file close diag

display "F026_STATA_EXPORT_OK=1"
log close
exit, clear
