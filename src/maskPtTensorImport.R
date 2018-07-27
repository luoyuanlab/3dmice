maskPtTensorImport <- function(t, fncf='mghtsConfig.R') {
    source(fncf)
    wptads = read.csv(fnwptads, header=F)
    wptads = wptads[,1]
    fns = list.files(path=dnval, pattern='gpTensorImpValidation\\d+\\.tsv')
    res = c()
    for (fn in fns) {
        print(fn)
        res = rbind(res, read.csv(sprintf('%s/%s', dnval, fn), sep="\t", stringsAsFactors=FALSE))
    }
    ## cna - count of NAs to be added for each patient
    library(hash)
    h = hash()
    t.na = t; pts = names(t)
    ## assume that the first dimension is pt, 2nd variable, 3rd time
    set.seed(1019)
    npt = dim(t)[1]; nv = dim(t)[2]; nt = dim(t)[3]
    naidx = res[,c('pt', 'test', 'i')] # only index the added missingness where we have ground truth
    naidx = naidx[naidx$pt %in% pts,]
    naidx = naidx[naidx$test %in% tests,]
    for (i in 1:dim(naidx)[1]) {
        ipt = naidx[i, 1]
        iv = naidx[i, 2]
        it = naidx[i, 3]
        if (!(ipt %in% wptads)) {
            nnas = which(!is.na(t[[ipt]][iv,]))
            it = nnas[it] # the original it indicates masking the it'th not-na element
            naidx[i,3] = it
            ## cat(sprintf('i %d, ipt %s iv %s, it %s\n', i, ipt, iv, it))
            t.na[[ipt]][iv, it] = NA
        }
    }
    rownames(naidx) = paste(naidx$pt, naidx$test, naidx$i, sep='_')
    cna.all = sum(unlist(lapply(t.na, function(x) sum(is.na(x)))))
    naidx.all = data.frame(matrix(NA, cna.all, 3))
    j = 0
    for (ipt in pts) {
        for (iv in tests) {
            selna = is.na(t.na[[ipt]][iv,])
            for (it in which(selna)) {
                j = j+1
                naidx.all[j,] = c(ipt, iv, it)
            }
        }
    }
    colnames(naidx.all) = c('pt', 'test', 'i')
    rownames(naidx.all) = paste(naidx.all$pt, naidx.all$test, naidx.all$i, sep='_')
    naidx.all[,'i'] = as.integer(naidx.all$i)
    h[['tna']] = t.na
    h[['t']] = t
    h[['naidx']] = naidx
    h[['naidx.all']] = naidx.all
    return (h);
}
