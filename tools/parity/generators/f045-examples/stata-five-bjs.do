version 14.2

args output_dir ado_root

if "`output_dir'" == "" {
    display as error "usage: stata -b do stata-five-bjs.do <output_dir> [ado_root]"
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

display "F045_STATA_OUTPUT=`output_dir'"
display "F045_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)
which did_imputation
which event_plot

clear all
timer clear
set seed 10
global T = 15
global I = 300

set obs `=$I*$T'
gen i = int((_n - 1) / $T) + 1
gen t = mod((_n - 1), $T) + 1
tsset i t

gen Ei = ceil(runiform() * 7) + $T - 6 if t == 1
bys i (t): replace Ei = Ei[1]
gen K = t - Ei
gen D = K >= 0 & Ei != .
gen tau = cond(D == 1, (t - 12.5), 0)
gen eps = rnormal()
gen Y = i + 3 * t + tau * D + eps

export delimited i t Ei K D tau Y using "`output_dir'/stata-five-bjs-panel.csv", replace

did_imputation Y i t Ei, allhorizons pretrend(5)
matrix b = e(b)
matrix V = e(V)
local terms : colnames b
local nterms : word count `terms'

file open est using "`output_dir'/estimates.csv", write replace
file write est "term,estimate,std_error,conf_low,conf_high" _n
forvalues j = 1/`nterms' {
    local term : word `j' of `terms'
    local estimate = b[1, `j']
    local se = sqrt(V[`j', `j'])
    file write est "`term'," %21.17g (`estimate') "," %21.17g (`se') "," %21.17g (`estimate' - invnormal(.975) * `se') "," %21.17g (`estimate' + invnormal(.975) * `se') _n
}
file close est

file open cov using "`output_dir'/covariance.csv", write replace
file write cov "row_term,col_term,value" _n
forvalues r = 1/`nterms' {
    local rterm : word `r' of `terms'
    forvalues c = 1/`nterms' {
        local cterm : word `c' of `terms'
        file write cov "`rterm',`cterm'," %21.17g (V[`r', `c']) _n
    }
}
file close cov

event_plot, savecoef noplot alpha(0.05)
file open plot using "`output_dir'/plot-data.csv", write replace
file write plot "model,event_time,position,estimate,ci_low,ci_high" _n
local saved_rows = 0
forvalues r = 1/`=_N' {
    if !missing(__event_coef1[`r']) {
        file write plot "1," %21.17g (__event_H1[`r']) "," %21.17g (__event_pos1[`r']) "," %21.17g (__event_coef1[`r']) "," %21.17g (__event_lo1[`r']) "," %21.17g (__event_hi1[`r']) _n
        local ++saved_rows
    }
}
file close plot

matrix btrue = J(1, 6, .)
matrix colnames btrue = tau0 tau1 tau2 tau3 tau4 tau5
qui forvalues h = 0/5 {
    sum tau if K == `h'
    matrix btrue[1, `h' + 1] = r(mean)
}
file open true using "`output_dir'/true-effects.csv", write replace
file write true "term,true_effect" _n
forvalues h = 0/5 {
    file write true "tau`h'," %21.17g (btrue[1, `h' + 1]) _n
}
file close true

count
local n_obs = r(N)
count if D == 1
local n_treated_rows = r(N)
count if D == 0
local n_control_rows = r(N)

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "source_example": "five_estimators_example.do BJS did_imputation/event_plot excerpt","' _n
file write diag `"  "command": "did_imputation Y i t Ei, allhorizons pretrend(5); event_plot, savecoef noplot alpha(0.05)","' _n
file write diag `"  "seed": 10,"' _n
file write diag `"  "units": 300,"' _n
file write diag `"  "periods": 15,"' _n
file write diag `"  "observations": `n_obs',"' _n
file write diag `"  "treated_rows": `n_treated_rows',"' _n
file write diag `"  "control_rows": `n_control_rows',"' _n
file write diag `"  "terms": `nterms',"' _n
file write diag `"  "plot_rows": `saved_rows',"' _n
file write diag `"  "third_party_estimators_in_source": "did_multiplegt,csdid,eventstudyinteract,reghdfe""' _n
file write diag "}" _n
file close diag

display "F045_STATA_EXPORT_OK=1"
log close
exit, clear
