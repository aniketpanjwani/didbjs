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

display "F008_STATA_INPUT=`input_csv'"
display "F008_STATA_OUTPUT=`output_dir'"
display "F008_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which did_imputation
import delimited using "`input_csv'", clear varnames(1) stringcols(1) case(preserve)
describe

file open estimates using "`output_dir'/estimates.csv", write replace
file write estimates "spec,term,estimate,std_error,conf_low,conf_high,n_obs,n_control,n_treated" _n
file open cov using "`output_dir'/covariance.csv", write replace
file write cov "spec,row_term,col_term,value" _n
file open mask using "`output_dir'/sample-mask.csv", write replace
file write mask "spec,row_id,sample" _n

local spec_names "constant_only unit_only time_only arbitrary"

local estimate_constant_only = .
local estimate_unit_only = .
local estimate_time_only = .
local estimate_arbitrary = .
local se_constant_only = .
local se_unit_only = .
local se_time_only = .
local se_arbitrary = .
local n_constant_only = .
local n_unit_only = .
local n_time_only = .
local n_arbitrary = .
local nc_constant_only = .
local nc_unit_only = .
local nc_time_only = .
local nc_arbitrary = .
local nt_constant_only = .
local nt_unit_only = .
local nt_time_only = .
local nt_arbitrary = .

forvalues s = 1/4 {
    local spec : word `s' of `spec_names'
    if "`spec'" == "constant_only" local fe_command "fe(.)"
    if "`spec'" == "unit_only" local fe_command "fe(unit)"
    if "`spec'" == "time_only" local fe_command "fe(t)"
    if "`spec'" == "arbitrary" local fe_command "fe(group t)"

    preserve
    did_imputation Y unit t Ei, `fe_command' minn(0) cluster(unit)

    matrix b = e(b)
    matrix V = e(V)
    matrix Nt = e(Nt)
    local bcols : colnames b
    local vrows : rownames V
    local vcols : colnames V
    local estimate = el(b, 1, 1)
    local std_error = sqrt(el(V, 1, 1))
    local conf_low = `estimate' - 1.959963984540054 * `std_error'
    local conf_high = `estimate' + 1.959963984540054 * `std_error'
    local term : word 1 of `bcols'
    file write estimates "`spec',`term'," %21.17g (`estimate') "," %21.17g (`std_error') "," %21.17g (`conf_low') "," %21.17g (`conf_high') "," %21.17g (e(N)) "," %21.17g (e(Nc)) "," %21.17g (el(Nt, 1, 1)) _n

    forvalues r = 1/`=rowsof(V)' {
        local row_term : word `r' of `vrows'
        forvalues c = 1/`=colsof(V)' {
            local col_term : word `c' of `vcols'
            file write cov "`spec',`row_term',`col_term'," %21.17g (el(V, `r', `c')) _n
        }
    }

    gen byte sample = e(sample)
    forvalues r = 1/`=_N' {
        file write mask "`spec',`=row_id[`r']'," %21.17g (sample[`r']) _n
    }

    local estimate_`spec' = `estimate'
    local se_`spec' = `std_error'
    local n_`spec' = e(N)
    local nc_`spec' = e(Nc)
    local nt_`spec' = el(Nt, 1, 1)
    restore
}

file close estimates
file close cov
file close mask

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "specs": ["constant_only", "unit_only", "time_only", "arbitrary"],"' _n
file write diag `"  "commands": {"' _n
file write diag `"    "constant_only": "did_imputation Y unit t Ei, fe(.) minn(0) cluster(unit)", "' _n
file write diag `"    "unit_only": "did_imputation Y unit t Ei, fe(unit) minn(0) cluster(unit)", "' _n
file write diag `"    "time_only": "did_imputation Y unit t Ei, fe(t) minn(0) cluster(unit)", "' _n
file write diag `"    "arbitrary": "did_imputation Y unit t Ei, fe(group t) minn(0) cluster(unit)""' _n
file write diag `"  }, "' _n
file write diag `"  "estimates": {"' _n
file write diag `"    "constant_only": "' %21.17f (`estimate_constant_only') "," _n
file write diag `"    "unit_only": "' %21.17f (`estimate_unit_only') "," _n
file write diag `"    "time_only": "' %21.17f (`estimate_time_only') "," _n
file write diag `"    "arbitrary": "' %21.17f (`estimate_arbitrary') _n
file write diag `"  }, "' _n
file write diag `"  "std_errors": {"' _n
file write diag `"    "constant_only": "' %21.17f (`se_constant_only') "," _n
file write diag `"    "unit_only": "' %21.17f (`se_unit_only') "," _n
file write diag `"    "time_only": "' %21.17f (`se_time_only') "," _n
file write diag `"    "arbitrary": "' %21.17f (`se_arbitrary') _n
file write diag `"  }, "' _n
file write diag `"  "n_obs": {"' _n
file write diag `"    "constant_only": "' %21.17g (`n_constant_only') "," _n
file write diag `"    "unit_only": "' %21.17g (`n_unit_only') "," _n
file write diag `"    "time_only": "' %21.17g (`n_time_only') "," _n
file write diag `"    "arbitrary": "' %21.17g (`n_arbitrary') _n
file write diag "  }" _n
file write diag "}" _n
file close diag

display "F008_STATA_EXPORT_OK=1"
log close
exit, clear
