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
set seed 20260620
sysdir set PLUS "`ado_root'/ado/plus"
sysdir set PERSONAL "`ado_root'/ado/personal"
adopath ++ "`ado_root'/ado/personal"
adopath ++ "`ado_root'/ado/plus"

log using "`output_dir'/run.log", text replace

display "F020_STATA_INPUT=`input_csv'"
display "F020_STATA_OUTPUT=`output_dir'"
display "F020_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit) autosample project(x)
local autosample_project_rc = _rc
file open invalid using "`output_dir'/autosample-proj-error.json", write replace
file write invalid "{" _n
file write invalid `"  "status": "error","' _n
file write invalid `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) autosample project(x)","' _n
file write invalid `"  "return_code": "' %9.0g (`autosample_project_rc') "," _n
file write invalid `"  "error_message": "Autosample cannot be combined with project. Please specify the sample explicitly","' _n
file write invalid `"  "stata_return_checked": true"' _n
file write invalid "}" _n
file close invalid
if `autosample_project_rc' != 184 {
    display as error "Expected autosample/project to fail with rc 184"
    exit 498
}

capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(group) project(x)
local hetby_project_rc = _rc
file open invalid using "`output_dir'/hetby-project-error.json", write replace
file write invalid "{" _n
file write invalid `"  "status": "error","' _n
file write invalid `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(group) project(x)","' _n
file write invalid `"  "return_code": "' %9.0g (`hetby_project_rc') "," _n
file write invalid `"  "error_message": "Options project and hetby cannot be combined.","' _n
file write invalid `"  "stata_return_checked": true"' _n
file write invalid "}" _n
file close invalid
if `hetby_project_rc' != 184 {
    display as error "Expected hetby/project to fail with rc 184"
    exit 498
}

capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(bad_group)
local bad_hetby_rc = _rc
file open invalid using "`output_dir'/bad-hetby-error.json", write replace
file write invalid "{" _n
file write invalid `"  "status": "error","' _n
file write invalid `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(bad_group)","' _n
file write invalid `"  "return_code": "' %9.0g (`bad_hetby_rc') "," _n
file write invalid `"  "error_message": "The hetby variable cannot take negative values.","' _n
file write invalid `"  "stata_return_checked": true"' _n
file write invalid "}" _n
file close invalid
if `bad_hetby_rc' != 411 {
    display as error "Expected negative hetby to fail with rc 411"
    exit 498
}

capture noisily did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(group)
local direct_hetby_rc = _rc
file open invalid using "`output_dir'/direct-hetby-error.json", write replace
file write invalid "{" _n
file write invalid `"  "status": "expected_divergence","' _n
file write invalid `"  "command": "did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(group)","' _n
file write invalid `"  "return_code": "' %9.0g (`direct_hetby_rc') "," _n
file write invalid `"  "error_message": "The hetby variable takes too many (over 30) values","' _n
file write invalid `"  "root_cause": "Stata 14.2 levelsof returns r(levels) but not r(r), while the pinned ado checks r(r)>30.","' _n
file write invalid `"  "oracle_command": "did_imputation Y unit t Ei, minn(0) cluster(unit) wtr(g0 g1)","' _n
file write invalid `"  "stata_return_checked": true"' _n
file write invalid "}" _n
file close invalid
if `direct_hetby_rc' != 149 {
    display as error "Expected direct hetby to fail with rc 149 on Stata 14.2"
    exit 498
}

gen double g0 = (group == 0)
gen double g1 = (group == 1)
did_imputation Y unit t Ei, minn(0) cluster(unit) wtr(g0 g1)

matrix f020_bmat = e(b)
matrix f020_Vmat = e(V)
matrix f020_Ntmat = e(Nt)
local bcols : colnames f020_bmat
local vrows : rownames f020_Vmat
local vcols : colnames f020_Vmat
local term_count = colsof(f020_bmat)

file open estimates using "`output_dir'/hetby-estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    if "`term'" == "tau_g0" local term "tau_0"
    if "`term'" == "tau_g1" local term "tau_1"
    local estimate = el(f020_bmat, 1, `idx')
    local std_error = sqrt(el(f020_Vmat, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(f020_Ntmat, 1, `idx')) _n
}
file close estimates

file open cov using "`output_dir'/hetby-covariance.csv", write replace
file write cov "row_term,col_term,value" _n
forvalues r = 1/`=rowsof(f020_Vmat)' {
    local row_term : word `r' of `vrows'
    if "`row_term'" == "tau_g0" local row_term "tau_0"
    if "`row_term'" == "tau_g1" local row_term "tau_1"
    forvalues c = 1/`=colsof(f020_Vmat)' {
        local col_term : word `c' of `vcols'
        if "`col_term'" == "tau_g0" local col_term "tau_0"
        if "`col_term'" == "tau_g1" local col_term "tau_1"
        file write cov "`row_term',`col_term'," %21.17g (el(f020_Vmat, `r', `c')) _n
    }
}
file close cov

gen byte sample = e(sample)
file open mask using "`output_dir'/hetby-sample-mask.csv", write replace
file write mask "row_id,sample" _n
forvalues r = 1/`=_N' {
    file write mask "`=row_id[`r']'," %21.17g (sample[`r']) _n
}
file close mask
drop sample

did_imputation Y unit t Ei, minn(0) cluster(unit) project(x)

matrix f020_bmat = e(b)
matrix f020_Vmat = e(V)
matrix f020_Ntmat = e(Nt)
local bcols : colnames f020_bmat
local vrows : rownames f020_Vmat
local vcols : colnames f020_Vmat
local term_count = colsof(f020_bmat)

file open estimates using "`output_dir'/project-estimates.csv", write replace
file write estimates "term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
forvalues idx = 1/`term_count' {
    local term : word `idx' of `bcols'
    local estimate = el(f020_bmat, 1, `idx')
    local std_error = sqrt(el(f020_Vmat, `idx', `idx'))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    file write estimates "`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(f020_Ntmat, 1, `idx')) _n
}
file close estimates

file open cov using "`output_dir'/project-covariance.csv", write replace
file write cov "row_term,col_term,value" _n
forvalues r = 1/`=rowsof(f020_Vmat)' {
    local row_term : word `r' of `vrows'
    forvalues c = 1/`=colsof(f020_Vmat)' {
        local col_term : word `c' of `vcols'
        file write cov "`row_term',`col_term'," %21.17g (el(f020_Vmat, `r', `c')) _n
    }
}
file close cov

gen byte sample = e(sample)
file open mask using "`output_dir'/project-sample-mask.csv", write replace
file write mask "row_id,sample" _n
forvalues r = 1/`=_N' {
    file write mask "`=row_id[`r']'," %21.17g (sample[`r']) _n
}
file close mask

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "hetby_command": "did_imputation Y unit t Ei, minn(0) cluster(unit) hetby(group)","' _n
file write diag `"  "hetby_oracle_command": "did_imputation Y unit t Ei, minn(0) cluster(unit) wtr(g0 g1)","' _n
file write diag `"  "direct_hetby_return_code": "' %9.0g (`direct_hetby_rc') "," _n
file write diag `"  "project_command": "did_imputation Y unit t Ei, minn(0) cluster(unit) project(x)","' _n
file write diag `"  "hetby_terms": ["tau_0", "tau_1"],"' _n
file write diag `"  "project_terms": ["tau_cons", "tau_x"],"' _n
file write diag `"  "n_obs": "' %21.17g (e(N)) "," _n
file write diag `"  "n_control": "' %21.17g (e(Nc)) _n
file write diag "}" _n
file close diag

display "F020_STATA_EXPORT_OK=1"
log close
exit, clear
