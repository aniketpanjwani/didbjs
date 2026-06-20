version 14.2

args output_dir ado_root

if "`output_dir'" == "" {
    display as error "usage: stata -b do stata-export.do <output_dir> [ado_root]"
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

display "F022_STATA_OUTPUT=`output_dir'"
display "F022_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which event_plot

clear
set obs 5
gen coef = 1
regress coef

matrix f022_b = (0.2, -0.1, 1, 2, 3)
matrix colnames f022_b = pre2 pre1 tau0 tau1 tau2
matrix f022_V = J(5, 5, 0)
matrix f022_V[1, 1] = 0.07^2
matrix f022_V[2, 2] = 0.05^2
matrix f022_V[3, 3] = 0.1^2
matrix f022_V[4, 4] = 0.2^2
matrix f022_V[5, 5] = 0.3^2
matrix rownames f022_V = pre2 pre1 tau0 tau1 tau2
matrix colnames f022_V = pre2 pre1 tau0 tau1 tau2

event_plot f022_b#f022_V, stub_lag(tau#) stub_lead(pre#) savecoef noplot alpha(0.05)

file open plot using "`output_dir'/plot-data.csv", write replace
file write plot "model,event_time,position,estimate,ci_low,ci_high" _n
forvalues r = 1/`=_N' {
    file write plot "1," %21.17g (__event_H1[`r']) "," %21.17g (__event_pos1[`r']) "," %21.17g (__event_coef1[`r']) "," %21.17g (__event_lo1[`r']) "," %21.17g (__event_hi1[`r']) _n
}
file close plot

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "command": "regress coef; event_plot f022_b#f022_V, stub_lag(tau#) stub_lead(pre#) savecoef noplot alpha(0.05)","' _n
file write diag `"  "eclass_seed": "regress coef","' _n
file write diag `"  "coef_sentinel": true,"' _n
file write diag `"  "savecoef": true,"' _n
file write diag `"  "noplot": true,"' _n
file write diag `"  "row_count": "' %21.17g (_N) _n
file write diag "}" _n
file close diag

display "F022_STATA_EXPORT_OK=1"
log close
exit, clear
