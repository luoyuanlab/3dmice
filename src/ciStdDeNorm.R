ciStdDeNorm <- function(ci, hstd) {
    ci.raw = ci
    cat(sprintf('CI self denormalizing\n'))
    for (nci in names(ci)) {
        naidx = strsplit(nci, "_")[[1]]
        test = naidx[2]
        ci.raw[nci] = ci[nci] * hstd[[test]]

    }
    cat(sprintf('done\n'))
    return (ci.raw)
}
