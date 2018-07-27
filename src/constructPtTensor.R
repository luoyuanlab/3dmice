constructPtTensor <- function(fncf='mghtsConfig.R') {
    ## for raw sampling, return a list of matrices
    source(fncf)
    pts = read.csv(fnptads, header=F)
    pts = pts[,1]
    
    wptads = read.csv(fnwptads, header=F)
    wptads = wptads[,1]
    pts = setdiff(pts, wptads)
    npt = length(pts); ntest = length(tests)
    ptt = vector('list', npt)
    # mcnt = rep(0, npt*ntest) # missing variable count for each time point
    names(ptt) = pts
    for (pt in pts) {
        fnpt = sprintf('%s/%s.csv', dnraw, pt)
        ts = read.table(fnpt, row.names=NULL, header=T, sep=',')
        if (exists('cns')) {
            colnames(ts) = cns
        }
        ptt[[pt]] = t(ts)
    }
    write.table(pts[!pts %in% (wptads)], file=fnptads.val, row.names=F, col.names=F, quote=F)
    # print(table(mcnt))
    save(ptt, file=sprintf('%s/ptt.RData', dndata))
    return (ptt);
}
