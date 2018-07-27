selfNorm <- function(Y, hmax, hmin, center=F, logt=F, fncf='mghtsConfig.R') {
    library(hash)
    source(fncf)
    V = Y; n = length(V)
    
    cat(sprintf('self normalizing\n'))
    for (ipt in names(V)) {
        for (iv in tests) {
            k = sprintf('%s|%s', ipt, iv)
            pv = V[[ipt]][iv,]
            kmax = max(pv, na.rm=T); hmax[[k]] = kmax
            kmin = min(pv, na.rm=T); hmin[[k]] = kmin
            if (center) {
                offset = (kmin+kmax)/2
            }else {
                offset = kmin
            }
            V[[ipt]][iv,] = (pv - offset) / (kmax - kmin)
        }
    }
    if (logt) {
        for (ipt in names(V)) {
            V[[ipt]][tests,] = log(V[[ipt]][tests,]+1)
        }
    }
    cat(sprintf('done\n'))
    return(V)
}
