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

log using "`output_dir'/run.log", text replace

display "F046_STATA_INPUT_DIR=`input_dir'"
display "F046_STATA_OUTPUT=`output_dir'"
display "F046_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)
which did_imputation

import delimited using "`input_dir'/panels.csv", clear varnames(1) stringcols(1 2 3) case(preserve)
tempfile allpanel
save `allpanel', replace

levelsof scenario, local(scenarios)

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "scenario,estimand,term,estimate,std_error,variance,n_obs,n_control,n_treated" _n
file close estimates

file open cov using "`output_dir'/covariance.csv", write replace
file write cov "scenario,row_term,col_term,value" _n
file close cov

file open samples using "`output_dir'/sample-mask.csv", write replace
file write samples "scenario,row_id,sample" _n
file close samples

file open failures using "`output_dir'/failures.csv", write replace
file write failures "scenario,reference,failure_class,failure_message,retained_fixture_path" _n
file close failures

local scenario_count = 0
local static_count = 0
local dynamic_count = 0
local weighted_count = 0
local estimate_rows = 0
local failure_count = 0

foreach scenario of local scenarios {
    use `allpanel', clear
    keep if scenario == "`scenario'"
    local estimand = estimand[1]
    local weighted = weighted[1]
    local weight_spec ""
    if `weighted' == 1 {
        local weight_spec "[aw=w]"
        local weighted_count = `weighted_count' + 1
    }
    local options "minn(0) cluster(unit)"
    if "`estimand'" == "dynamic" {
        local options "horizons(0/2) `options'"
        local dynamic_count = `dynamic_count' + 1
    }
    else {
        local static_count = `static_count' + 1
    }
    local scenario_count = `scenario_count' + 1
    keep row_id unit t Ei Y w
    matrix drop _all
    discard

    capture noisily did_imputation Y unit t Ei `weight_spec', `options'
    if _rc {
        local failure_count = `failure_count' + 1
        local rc = _rc
        file open failures using "`output_dir'/failures.csv", write append
        file write failures "`scenario',stata,stata_rc_`rc',did_imputation returned rc `rc',tests/fixtures/parity/f046-differential/inputs/panels.csv" _n
        file close failures
        exit `rc'
    }

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
        file write estimates "`scenario',`estimand',`term'," %24.17f (`estimate') "," %24.17f (`std_error') "," %24.17f (`variance') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, `j')) _n
        local estimate_rows = `estimate_rows' + 1
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
    file open samples using "`output_dir'/sample-mask.csv", write append
    forvalues r = 1/`=_N' {
        file write samples "`scenario'," (row_id[`r']) "," %1.0f (sample[`r']) _n
    }
    file close samples
}

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "command": "did_imputation Y unit t Ei [aw=w optional], horizons(0/2 optional) minn(0) cluster(unit)","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "scenario_count": `scenario_count',"' _n
file write diag `"  "static_count": `static_count',"' _n
file write diag `"  "dynamic_count": `dynamic_count',"' _n
file write diag `"  "weighted_count": `weighted_count',"' _n
file write diag `"  "estimate_rows": `estimate_rows',"' _n
file write diag `"  "failure_count": `failure_count'"' _n
file write diag "}" _n
file close diag

display "F046_STATA_EXPORT_OK=1"
log close
exit, clear
