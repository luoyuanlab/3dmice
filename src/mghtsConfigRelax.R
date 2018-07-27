server = Sys.info()['nodename']
# configure in case you have multiple sites
if (server == '[your server 1]') {
    dnroot = '[your folder 1]'
}else if (grepl('[your cluster name]', server)) {
    dnroot = '[your folder 2]'
}else {
    quit(sprintf('unrecognized server %s\n', server))
}

duration = 60*24*7
step = 60*24
n = duration / step
dnraw = sprintf('%s/perPt', dnroot)
dnaln = sprintf('%s/perPtHourly', dnroot)
dnval = sprintf('%s/validation/gpml_raw_sample', dnroot)
tests = c('PCL', 'PK', 'PLCO2', 'PNA', 'HCT', 'HGB', 'MCV', 'PLT', 'WBC', 'RDW', 'PBUN', 'PCRE', 'PGLU') #
dnstrict = sprintf('%s/pt_strict_criteria', dnroot)
dnrelax = sprintf('%s/pt_relax_criteria', dnroot)
dndata = dnrelax
fnptads = sprintf('%s/ptads.csv', dndata)
fnwptads = sprintf('%s/stgp_warning_ptads.csv', dndata)
fnptads.val = sprintf('%s/ptads_val.csv', dndata)

fntr.tmp = sprintf('%s/ptads_val_tr_%%s.csv', dndata)
fnte.tmp = sprintf('%s/ptads_val_te_%%s.csv', dndata)

fnlog.tmp = sprintf('%s/micegp_log_large_cohort/%%s_output_iter%%s.RData', dndata)
fnres.tmp = sprintf('%s/micegp_log_large_cohort/%%s_res_iter%%s.RData', dndata)
fnkm.tmp = sprintf('%s/micegp_log_large_cohort/%%s_km_iter%%s.RData', dndata)

timeidx = 'CollectDateTime'
rtrte = 0.5
param = F # T
nimp = 100
