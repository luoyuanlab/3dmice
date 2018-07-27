temporalMICEGP <- function(h, m=10, ci.thr=0.2, norm="self", trte="tr", sd=11242015, nimp=30, maxit=5, mincor=0.5, cl=NULL, ncores=10, param=T, fncf='mghtsConfig.R', nug_thres=20) { 
    library(mice)
    library(GPfit)
    library(hash)
    library(doParallel)
    library(foreach)
    source('evalPtTensorImpImportTrTe.R')
    source('miceCrossSectionInit.R')
    source('selfNorm.R')
    source('selfDeNorm.R')
    source('ciSelfDeNorm.R')
    source('stdNorm.R')
    source('stdDeNorm.R')
    source('ciStdDeNorm.R')
    source(fncf)
    print(timeidx)
    cl.init = F;
    if (is.null(cl)) {
        cl=makeCluster(ncores, type="FORK", outfile="") 
        registerDoParallel(cl)
        cl.init = T
        clusterSetRNGStream(cl, sd)
    }

    qtl = c(0, 0.25, 0.5, 0.75, 1)
    V = h[['tna']]; Y = V; naidx = h[['naidx']];
    ci = V; pts = names(V)
    cat(sprintf('na tensor len %d\n', length(V)))

    hmax = hash(); hmin = hash(); hstd = hash()
    Jl = dim(V)[2]; n = length(V)
    if (norm=='self') {
        V = selfNorm(V, hmax, hmin, fncf=fncf)
    }else if (norm=='std') {
        V = stdNorm(V, hstd, fncf=fncf)
    }
    r = miceCrossSectionInit(V, nimp=nimp, sd=sd, maxit=maxit, mincor=mincor, cl=cl, ncores=ncores, fncf=fncf)
    V = r$V; V.sd = r$std
    for (pt in pts) {
        ci[[pt]] = 1.96*V.sd[[pt]]
    }
    if (norm=="self") {
        V.raw = selfDeNorm(V, hmax, hmin, fncf=fncf)
        ci.raw = ciSelfDeNorm(ci, hmax, hmin, fncf=fncf)
    }else if (norm=="std") {
        V.raw = stdDeNorm(V, hstd, fncf=fncf)
        ci.raw = ciStdDeNorm(ci, hstd)
    }else {
        V.raw = V
        ci.raw = ci
    }
    nrmse = evalPtTensorImpImportTrTe(V.raw, h[['t']], ci.raw, naidx, trte=trte, iter='init', fncf=fncf)
    cat(sprintf('MICE init %s nRMSE\n', trte))
    print(nrmse)

    for (c in 1:m) {# for m iters
        nleft = 0; ci.v = c()
        for (ipt in pts) {
            for (iv in tests) {
                selna = is.na(Y[[ipt]][iv,])
                sel = selna & (ci[[ipt]][iv,] > ci.thr)
                V[[ipt]][iv,sel] = NA
                nleft = nleft + sum(sel)
                ci.v = c(ci.v, ci[[ipt]][iv, selna])
            }
        }
        cat(sprintf('iter=%d, conf int quantile\n', c))
        print(quantile(ci.v, probs=qtl), digits=4)
        ## cross sectional impute for all variables
        ## concatenate all time slices
        x = c() # columns are variables
        for (xsub in V) {
            x = rbind(x, t(xsub[tests,]))
        }
        cat(sprintf('iter=%d, #na in x (%d x %d) %d\n', c, dim(x)[1], dim(x)[2], nleft))

        imp <- foreach(no = 1:ncores, .combine=ibind, .packages="mice") %dopar% {
            mice(x, m=nimp/ncores, mincor=mincor, maxit=maxit, printFlag=F) 
        }
        print(imp)
        xt = array(NA, dim=c(dim(x)[1], dim(x)[2], nimp))
        for (i in 1:nimp) {
            xt[,,i] = as.matrix(complete(imp, i))
        }
        ## reconstruct V1
        V1 = V; V1.sd = V; cursor = 0
        if (param) {
            xm = apply(xt, c(1,2), mean)
            sdm = apply(xt, c(1,2), sd)
            for (pt in names(V)) {
                batch = dim(V1[[pt]])[2]
                V1[[pt]][tests,] = t(xm[cursor+1:batch,]); 
                V1.sd[[pt]][tests,] = t(sdm[cursor+1:batch,]); 
                cursor = cursor + batch
            }
        }else {
            ## reconstruct V1m, with multiple imputations
            V1m = list()
            for (pt in names(V)) {
                vm = V[[pt]]
                batch = dim(vm)[2]
                V1m[[pt]] = array(NA, dim=c(dim(vm)[1], dim(vm)[2], nimp))
                dimnames(V1m[[pt]])[[1]] = rownames(vm)
                dimnames(V1m[[pt]])[[2]] = colnames(vm)
                for (i in 1:nimp) {
                    V1m[[pt]][tests,,i] = t(xt[cursor+1:batch,,i]);
                }
                V1[[pt]][tests,] = apply(V1m[[pt]][tests,,], c(1,2), mean)
                V1.sd[[pt]][tests,] = apply(V1m[[pt]][tests,,], c(1,2), sd)
                cursor = cursor + batch
            }
        }

        cat(sprintf('iter=%d, gp\n', c))
        ## parallel computation of V2 and V
        vci <- foreach (i=1:length(V), .packages='GPfit') %dopar% {
            pt = pts[i]
            ev = V[[pt]]
            ev2 = V[[pt]]
            x = ev[timeidx,]
            x = (x - min(x))/(max(x)-min(x)) # map x to (0,1)
            eci = ci[[pt]] 
            esd2 = V1.sd[[pt]]
            ediff = list()
            for (test in tests) {
                ediff[[test]] = c()
                y = ev[test,]; itr = !is.na(y); ite = is.na(y)
                ytr = y[itr]; xtr = x[itr]
                xte = x[ite]
                if (length(xte)>0) {
                    gpmod = GP_fit(xtr, ytr, nug_thres=nug_thres)
                    gppred = predict(gpmod, xte)
                    mu2 = gppred$Y_hat; ev2[test, ite] = mu2
                    sigma2 = sqrt(gppred$MSE); esd2[test, ite] = sigma2

                    if (param) {
                        mu1 = V1[[pt]][test, ite]
                        sigma1 = V1.sd[[pt]][test, ite]
                        w1 = sigma2/ (sigma1 + sigma2)
                        w2 = 1-w1
                        mu = w1*mu1 + w2*mu2
                        ev[test, ite] = mu
                        ediff[[test]] = abs(mu1 - mu2)
                        eci[test, ite] = 1.96*sqrt(((mu1-mu)^2+(mu2-mu)^2)/2) 
                    }else { # non-parametric
                        j = 0
                        for (i in which(ite)) { # this is likely a bug regarding i, revisit later
                            j = j+1
                            V1.v = V1m[[pt]][test,i,]
                            if (sigma2[j]==0) {
                                nimpw = nimp
                                cat(sprintf('zero gp sd for %s, %s, %s\n', pt, test, i))
                            }else {
                                nimpw = ceiling(nimp*sd(V1.v)/sigma2[j])
                            }
                            V2.v = rnorm(nimpw, mean=mu2[j], sd=sigma2[j]) # *sd(V1.v)/sigma2[j] - upsample or downsample inv prop to std
                            if (sigma2[j]==0) {
                                V12.v = V2.v
                            }else {
                                V12.v = c(V1.v, V2.v)
                            }
                            ev[test, i] = mean(V12.v)
                            eci[test, i] = 1.96*sd(V12.v) 
                            if (eci[test,i]>1.2) {
                                cat(sprintf('big ci %.3f at %s, %s, %s\n', eci[test, i], pt, test, i))
                                print(V1.v)
                                print(V2.v)
                            }
                            ediff[[test]] = abs(mean(V1.v) - mean(V2.v))
                        }
                    }
                }
            }
            er = list(v=ev, ci=eci, diff=ediff, v2=ev2, sd2=esd2)
            er
        }
        V = lapply(vci, function(a) {a$v}); names(V) = pts
        ci = lapply(vci, function(a) {a$ci}); names(ci) = pts

        V2 = lapply(vci, function(a) {a$v2}); names(V2) = pts
        V2.sd = lapply(vci, function(a) {a$sd2}); names(V2.sd) = pts

        ptdiff = lapply(vci, function(a) {a$diff}); names(ptdiff) = pts
        vdiff = list()
        for (test in tests) {
            vdiff[[test]] = c()
            for (pt in pts) {
                vdiff[[test]] = c(vdiff[[test]], ptdiff[[pt]][[test]])
            }
            cat(sprintf('iter=%d, %s diff\n', c, test))
            print(quantile(vdiff[[test]], qtl), digits=4)
        }

        if (norm=='self') {
            V.raw = selfDeNorm(V, hmax, hmin, fncf=fncf)
            V1.raw = selfDeNorm(V1, hmax, hmin, fncf=fncf)
            V2.raw = selfDeNorm(V2, hmax, hmin, fncf=fncf)
            ci.raw = ciSelfDeNorm(ci, hmax, hmin, fncf=fncf)
        }else if (norm=='std') {
            V.raw = stdDeNorm(V, hstd, fncf=fncf)
            ci.raw = ciStdDeNorm(ci, hstd)
        }else {
            V.raw = V
            ci.raw = ci
        }
        nrmse = evalPtTensorImpImportTrTe(V.raw, h[['t']], ci.raw, naidx, V1.raw=V1.raw, V2.raw=V2.raw, V1.sd=V1.sd, V2.sd=V2.sd, trte=trte, iter=c, fncf=fncf)
        cat(sprintf('iter %d %s nRMSE\n', c, trte))
        print(nrmse)
        res = list(t.imp=V.raw, tn.imp=V, ci.imp=ci.raw, cin.imp=ci, V1.raw=V1.raw, V2.raw=V2.raw, V1.sd=V1.sd, V2.sd=V2.sd, h=h)
        fnres = sprintf(fnres.tmp, trte, c)
        save(res, file=fnres)
    }
    if (cl.init) {
        stopCluster(cl)
    }
    return (res)
}
