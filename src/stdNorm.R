stdNorm <- function(Y, hstd, fncf='mghtsConfig.R') {
    source(fncf)
    library(hash)
    cat(sprintf('self normalizing\n'))
    V = Y
    for (test in tests) {
        vt = c()
        for (pt in names(V)) {
            vt = c(vt, V[[pt]][test,])
        }
        hstd[[test]] = sd(vt, na.rm=T)

    }
    for (pt in names(V)) {
        for (test in tests) {
            V[[pt]][test,] = V[[pt]][test,] / hstd[[test]]
        }
    }
    cat(sprintf('done\n'))
    return(V)
}
