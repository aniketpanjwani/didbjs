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

display "F023_STATA_OUTPUT=`output_dir'"
display "F023_STATA_ADO_ROOT=`ado_root'"
display "STATA_VERSION=" c(stata_version)

which event_plot

clear
set obs 6
gen coef = 1
regress coef

matrix m1_b = (-0.3, -0.2, -0.1, 1, 1.5, 2)
matrix colnames m1_b = m1pre3 m1pre2 m1pre1 m1tau0 m1tau1 m1tau2
matrix m1_V = J(6, 6, 0)
matrix m1_V[1, 1] = 0.03^2
matrix m1_V[2, 2] = 0.04^2
matrix m1_V[3, 3] = 0.05^2
matrix m1_V[4, 4] = 0.1^2
matrix m1_V[5, 5] = 0.15^2
matrix m1_V[6, 6] = 0.2^2
matrix rownames m1_V = m1pre3 m1pre2 m1pre1 m1tau0 m1tau1 m1tau2
matrix colnames m1_V = m1pre3 m1pre2 m1pre1 m1tau0 m1tau1 m1tau2

matrix m2_b = (-0.6, -0.4, -0.2, 2, 2.5, 3)
matrix colnames m2_b = lead_3 lead_2 lead_1 lag_0 lag_1 lag_2
matrix m2_V = J(6, 6, 0)
matrix m2_V[1, 1] = 0.06^2
matrix m2_V[2, 2] = 0.08^2
matrix m2_V[3, 3] = 0.1^2
matrix m2_V[4, 4] = 0.2^2
matrix m2_V[5, 5] = 0.25^2
matrix m2_V[6, 6] = 0.3^2
matrix rownames m2_V = lead_3 lead_2 lead_1 lag_0 lag_1 lag_2
matrix colnames m2_V = lead_3 lead_2 lead_1 lag_0 lag_1 lag_2

event_plot m1_b#m1_V m2_b#m2_V, stub_lag(m1tau# lag_#) stub_lead(m1pre# lead_#) trimlag(1 1) trimlead(2 2) shift(0 1) perturb(0 .25) savecoef noplot alpha(0.05)

file open plot using "`output_dir'/plot-data.csv", write replace
file write plot "model,event_time,position,estimate,ci_low,ci_high" _n
local saved_rows = 0
forvalues m = 1/2 {
    forvalues r = 1/`=_N' {
        if !missing(__event_coef`m'[`r']) {
            file write plot "`m'," %21.17g (__event_H`m'[`r']) "," %21.17g (__event_pos`m'[`r']) "," %21.17g (__event_coef`m'[`r']) "," %21.17g (__event_lo`m'[`r']) "," %21.17g (__event_hi`m'[`r']) _n
            local ++saved_rows
        }
    }
}
file close plot

file open diag using "`output_dir'/diagnostics.json", write replace
file write diag "{" _n
file write diag `"  "status": "success","' _n
file write diag `"  "stata_version": "`=c(stata_version)'","' _n
file write diag `"  "command": "regress coef; event_plot m1_b#m1_V m2_b#m2_V, stub_lag(m1tau# lag_#) stub_lead(m1pre# lead_#) trimlag(1 1) trimlead(2 2) shift(0 1) perturb(0 .25) savecoef noplot alpha(0.05)","' _n
file write diag `"  "eclass_seed": "regress coef","' _n
file write diag `"  "coef_sentinel": true,"' _n
file write diag `"  "savecoef": true,"' _n
file write diag `"  "noplot": true,"' _n
file write diag `"  "models": 2,"' _n
file write diag `"  "saved_rows": "' %21.17g (`saved_rows') _n
file write diag "}" _n
file close diag

display "F023_STATA_EXPORT_OK=1"
log close
exit, clear
