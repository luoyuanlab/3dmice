stdDeNorm <- function(V, hstd, fncf='mghtsConfig.R') {
    source(fncf)
    V.raw = V
    cat(sprintf('self denormalizing\n'))
    for (pt in names(V)) {
        for (test in tests) {
            V.raw[[pt]][test,] = V[[pt]][test,] * hstd[[test]]
        }
    }
    cat(sprintf('done\n'))
    return (V.raw)
}
