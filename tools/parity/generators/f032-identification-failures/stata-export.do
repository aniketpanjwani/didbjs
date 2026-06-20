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

capture program drop f032_export_success
program define f032_export_success
    args scenario output_dir
    local sample_stem "`scenario'"
    if "`scenario'" == "no_treated" {
        local sample_stem "no-treated"
    }
    if "`scenario'" == "no_supported_horizon" {
        local sample_stem "no-h1"
    }
    matrix b = e(b)
    matrix V = e(V)
    capture matrix Nt = e(Nt)
    local has_nt = (_rc == 0)
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
        if `has_nt' {
            local n_treated = el(Nt, 1, `j')
        }
        file write estimates "`scenario',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`estimate' - 1.959963984540054 * `std_error') "," %24.17f (`estimate' + 1.959963984540054 * `std_error') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (`n_treated') _n
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
    export delimited using "`output_dir'/sample-`sample_stem'.csv", replace
    restore
    drop sample
end

capture program drop f032_export_cannot
program define f032_export_cannot
    args scenario output_dir
    local cannot_stem "`scenario'"
    if "`scenario'" == "one_cohort" {
        local cannot_stem "one"
    }
    if "`scenario'" == "all_post_treated" {
        local cannot_stem "all-post"
    }
    capture confirm variable cannot_impute
    if (_rc == 0) {
        preserve
        keep row_id cannot_impute
        export delimited using "`output_dir'/cannot-`cannot_stem'.csv", replace
        restore
    }
end

log using "`output_dir'/run.log", text replace

display "F032_STATA_INPUT=`input_csv'"
display "F032_STATA_OUTPUT=`output_dir'"
display "F032_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1 2) case(preserve)
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
keep if scenario == "no_untreated"
capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit)
local no_untreated_rc = _rc
local no_untreated_status = cond(`no_untreated_rc' == 0, "reference_success", "reference_error")
local no_untreated_cannot = .
capture confirm variable cannot_impute
if (_rc == 0) {
    quietly count if cannot_impute == 1
    local no_untreated_cannot = r(N)
    f032_export_cannot no_untreated "`output_dir'"
}
if `no_untreated_rc' == 0 {
    f032_export_success no_untreated "`output_dir'"
}

use `base', clear
keep if scenario == "no_treated"
capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit)
local no_treated_rc = _rc
local no_treated_status = cond(`no_treated_rc' == 0, "reference_success", "reference_error")
local no_treated_cannot = .
capture confirm variable cannot_impute
if (_rc == 0) {
    quietly count if cannot_impute == 1
    local no_treated_cannot = r(N)
    f032_export_cannot no_treated "`output_dir'"
}
if `no_treated_rc' == 0 {
    f032_export_success no_treated "`output_dir'"
}

use `base', clear
keep if scenario == "one_cohort"
capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit)
local one_cohort_rc = _rc
local one_cohort_status = cond(`one_cohort_rc' == 0, "reference_success", "reference_error")
local one_cohort_cannot = .
capture confirm variable cannot_impute
if (_rc == 0) {
    quietly count if cannot_impute == 1
    local one_cohort_cannot = r(N)
    f032_export_cannot one_cohort "`output_dir'"
}
if `one_cohort_rc' == 0 {
    f032_export_success one_cohort "`output_dir'"
}

use `base', clear
keep if scenario == "all_post_treated"
capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit)
local all_post_treated_rc = _rc
local all_post_treated_status = cond(`all_post_treated_rc' == 0, "reference_success", "reference_error")
local all_post_treated_cannot = .
capture confirm variable cannot_impute
if (_rc == 0) {
    quietly count if cannot_impute == 1
    local all_post_treated_cannot = r(N)
    f032_export_cannot all_post_treated "`output_dir'"
}
if `all_post_treated_rc' == 0 {
    f032_export_success all_post_treated "`output_dir'"
}

use `base', clear
keep if scenario == "no_supported_horizon"
capture noisily did_imputation Y unit t Ei, horizons(1) minn(0) cluster(unit)
local no_supported_horizon_rc = _rc
local no_supported_horizon_status = cond(`no_supported_horizon_rc' == 0, "reference_success", "reference_error")
local no_supported_horizon_cannot = .
capture confirm variable cannot_impute
if (_rc == 0) {
    quietly count if cannot_impute == 1
    local no_supported_horizon_cannot = r(N)
    f032_export_cannot no_supported_horizon "`output_dir'"
}
if `no_supported_horizon_rc' == 0 {
    f032_export_success no_supported_horizon "`output_dir'"
}

local nunt_rcj : display %21.17g (`no_untreated_rc')
local ntr_rcj : display %21.17g (`no_treated_rc')
local one_rcj : display %21.17g (`one_cohort_rc')
local post_rcj : display %21.17g (`all_post_treated_rc')
local nhor_rcj : display %21.17g (`no_supported_horizon_rc')
local nunt_cj "null"
local ntr_cj "null"
local one_cj "null"
local post_cj "null"
local nhor_cj "null"
if `no_untreated_cannot' < . {
    local nunt_cj : display %21.17g (`no_untreated_cannot')
}
if `no_treated_cannot' < . {
    local ntr_cj : display %21.17g (`no_treated_cannot')
}
if `one_cohort_cannot' < . {
    local one_cj : display %21.17g (`one_cohort_cannot')
}
if `all_post_treated_cannot' < . {
    local post_cj : display %21.17g (`all_post_treated_cannot')
}
if `no_supported_horizon_cannot' < . {
    local nhor_cj : display %21.17g (`no_supported_horizon_cannot')
}

file open probes using "`output_dir'/probes.json", write replace
file write probes "{" _n
file write probes `"  "no_untreated": {"status": "`no_untreated_status'", "return_code": `nunt_rcj', "cannot_impute_count": `nunt_cj', "command": "did_imputation Y unit t Ei, minn(0) cluster(unit)"},"' _n
file write probes `"  "no_treated": {"status": "`no_treated_status'", "return_code": `ntr_rcj', "cannot_impute_count": `ntr_cj', "command": "did_imputation Y unit t Ei, minn(0) cluster(unit)"},"' _n
file write probes `"  "one_cohort": {"status": "`one_cohort_status'", "return_code": `one_rcj', "cannot_impute_count": `one_cj', "command": "did_imputation Y unit t Ei, minn(0) cluster(unit)"},"' _n
file write probes `"  "all_post_treated": {"status": "`all_post_treated_status'", "return_code": `post_rcj', "cannot_impute_count": `post_cj', "command": "did_imputation Y unit t Ei, minn(0) cluster(unit)"},"' _n
file write probes `"  "no_supported_horizon": {"status": "`no_supported_horizon_status'", "return_code": `nhor_rcj', "cannot_impute_count": `nhor_cj', "command": "did_imputation Y unit t Ei, horizons(1) minn(0) cluster(unit)"}"' _n
file write probes "}" _n
file close probes

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source": "pinned Stata did_imputation identification-failure probes","' _n
file write diag `"  "success_scenarios": "no_treated,no_supported_horizon","' _n
file write diag `"  "failure_scenarios": "no_untreated,one_cohort,all_post_treated","' _n
file write diag `"  "omitted_zero_scenarios": "no_treated,no_supported_horizon","' _n
local scenario_count = 5
file write diag `"  "scenario_count": "' %21.17g (`scenario_count') _n
file write diag "}" _n
file close diag

display "F032_STATA_EXPORT_OK=1"
log close
exit, clear
