miceCrossSectionInit <- function(Y, sd=11242015, nimp=30, maxit=20, mincor=0.5, cl=NULL, ncores=10, fncf='mghtsConfig.R') {
    library(foreach)
    library(doParallel)
    source(fncf)
    cl.init=F
    if (is.null(cl)) {
        cl=makeCluster(ncores, type="FORK", outfile="") #
        registerDoParallel(cl)
        cl.init=T
        clusterSetRNGStream(cl, sd)
    }
    library(mice)
    V = Y    
    cat(sprintf('MICE cross sectional initialization\n'))
    clusterSetRNGStream(cl,sd)
    
    x = c()
    for (xsub in V) {
        x = rbind(x, t(xsub[tests,]))
    }
    cat(sprintf('#na in x (%d x %d) %d\n', dim(x)[1], dim(x)[2], sum(is.na(x))))
    
    imp <- foreach(no = 1:ncores, .combine=ibind, .packages="mice") %dopar% {
        mice(x, m=nimp/ncores, maxit=maxit, printFlag=F) # may want to increase the maxit and monitors the convergence, which statistics? may even consider parallelizing mincor=mincor, 
    }
    print(imp)
    xt = array(NA, dim=c(dim(x)[1], dim(x)[2], nimp))
    for (i in 1:nimp) {
        xt[,,i] = as.matrix(mice::complete(imp,i))
    }
    xm = apply(xt, c(1,2), mean)
    sdm = apply(xt, c(1,2), sd)
    std = V; cursor = 0
    for (pt in names(V)) {
        batch = dim(V[[pt]])[2]
        ## rnames = rownames(V[[pt]]); cnames = colnames(V[[pt]])
        V[[pt]][tests,] = t(xm[cursor+1:batch,]); # rownames(V[[pt]]) = rnames; colnames(V[[pt]]) = cnames
        std[[pt]][tests,] = t(sdm[cursor+1:batch,]); # rownames(std[[pt]]) = rnames; colnames(std[[pt]]) = cnames
        cursor = cursor + batch
    }
    
    if (cl.init) {
        stopCluster(cl)
    }
    return (list(V=V, std=std, imp=imp))
}
