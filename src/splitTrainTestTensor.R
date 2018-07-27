splitTrainTestTensor <- function(t, fncf='mghtsConfig.R') {
    source(fncf)
    fntr = sprintf(fntr.tmp, rtrte)
    fnte = sprintf(fnte.tmp, rtrte)
    pt.tr = read.csv(fntr, header=F); pt.tr = as.vector(pt.tr[,1])
    pt.te = read.csv(fnte, header=F); pt.te = as.vector(pt.te[,1])
    ttr = t[pt.tr]
    tte = t[pt.te]
    return (list(tr=ttr, te=tte))
}
