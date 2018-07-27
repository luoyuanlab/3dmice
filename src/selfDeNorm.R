selfDeNorm <- function(V, hmax, hmin, center=F, logt=F, fncf='mghtsConfig.R') {
    source(fncf)
    V.raw = V; n = length(V)
    cat(sprintf('self denormalizing\n'))
    for (ipt in names(V)) {
        for (iv in tests) {
            if (logt) {
                V.raw[[ipt]][iv,] = exp(V[[ipt]][iv,]) - 1
            }
            k = sprintf('%s|%s', ipt, iv)
            kmax = hmax[[k]]
            kmin = hmin[[k]]
            if (center) {
                offset = (kmin+kmax)/2
            }else {
                offset = kmin
            }
            V.raw[[ipt]][iv,] = V.raw[[ipt]][iv,] * (kmax - kmin) + offset
            
        }
    }
    cat(sprintf('done\n'))
    return (V.raw)
}
