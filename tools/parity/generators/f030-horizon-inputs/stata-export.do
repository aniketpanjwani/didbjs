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

capture program drop f030_export_current
program define f030_export_current
    args scenario output_dir
    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    file open estimates using "`output_dir'/estimates.csv", write append
    forvalues j = 1/`=colsof(b)' {
        local term : word `j' of `bcols'
        local estimate = el(b, 1, `j')
        local variance = el(V, `j', `j')
        local std_error = sqrt(`variance')
        file write estimates "`scenario',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `j')) _n
    }
    file close estimates

    local vrows : rownames V
    local vcols : colnames V
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
    export delimited using "`output_dir'/sample-mask-`scenario'.csv", replace
    restore
    drop sample
end

log using "`output_dir'/run.log", text replace

display "F030_STATA_INPUT=`input_csv'"
display "F030_STATA_OUTPUT=`output_dir'"
display "F030_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe
tempfile base
save `base', replace

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
file close estimates
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(2 0) minn(0) cluster(unit)
local unsorted_rc = _rc
local unsorted_status = cond(`unsorted_rc' == 0, "reference_success", "reference_error")
if `unsorted_rc' == 0 {
    f030_export_current unsorted "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(0 2) minn(0) cluster(unit)
local sparse_rc = _rc
local sparse_status = cond(`sparse_rc' == 0, "reference_success", "reference_error")
if `sparse_rc' == 0 {
    f030_export_current sparse "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(0 3) minn(0) cluster(unit)
local absent_rc = _rc
local absent_status = cond(`absent_rc' == 0, "reference_success", "reference_error")
if `absent_rc' == 0 {
    f030_export_current absent "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(0 0) minn(0) cluster(unit)
local duplicate_rc = _rc
local duplicate_status = cond(`duplicate_rc' == 0, "reference_success", "reference_error")
if `duplicate_rc' == 0 {
    f030_export_current duplicate "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(-1) minn(0) cluster(unit)
local negative_rc = _rc
local negative_status = cond(`negative_rc' == 0, "reference_success", "reference_error")
if `negative_rc' == 0 {
    f030_export_current negative "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons() minn(0) cluster(unit)
local empty_rc = _rc
local empty_status = cond(`empty_rc' == 0, "reference_success", "reference_error")
if `empty_rc' == 0 {
    f030_export_current empty "`output_dir'"
}

use `base', clear
capture noisily did_imputation Y unit t Ei [aw=w], horizons(0 1) allhorizons minn(0) cluster(unit)
local combined_rc = _rc
local combined_status = cond(`combined_rc' == 0, "reference_success", "reference_error")
if `combined_rc' == 0 {
    f030_export_current horizons_allhorizons "`output_dir'"
}

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "unsorted": {"status": "`unsorted_status'", "return_code": "' %21.17g (`unsorted_rc') "}," _n
file write probes `"  "sparse": {"status": "`sparse_status'", "return_code": "' %21.17g (`sparse_rc') "}," _n
file write probes `"  "absent": {"status": "`absent_status'", "return_code": "' %21.17g (`absent_rc') "}," _n
file write probes `"  "duplicate": {"status": "`duplicate_status'", "return_code": "' %21.17g (`duplicate_rc') "}," _n
file write probes `"  "negative": {"status": "`negative_status'", "return_code": "' %21.17g (`negative_rc') "}," _n
file write probes `"  "empty": {"status": "`empty_status'", "return_code": "' %21.17g (`empty_rc') "}," _n
file write probes `"  "horizons_allhorizons": {"status": "`combined_status'", "return_code": "' %21.17g (`combined_rc') "}" _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata did_imputation horizon boundary probes","' _n
file write diag `"  "input_file": "`input_csv'","' _n
file write diag `"  "success_scenarios": "unsorted,sparse,absent,duplicate,empty","' _n
file write diag `"  "failure_scenarios": "negative,horizons_allhorizons","' _n
local scenario_count = 7
file write diag `"  "scenario_count": "' %21.17g (`scenario_count') _n
file write diag "}" _n
file close diag

display "F030_STATA_EXPORT_OK=1"
log close
exit, clear
