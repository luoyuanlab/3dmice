#' ---
#' title: "Temporal MICE-GP Train Evaluation"
#' author: "Yuan Luo"
#' always_allow_html: yes
#' ---
library(hash)
source('constructPtTensor.R')
source('maskPtTensorImport.R')
source('evalPtTensorImpImportTrTe.R')
source('temporalMICEGP.R')
source('splitTrainTestTensor.R')

fncf='mghtsConfigRelax.R'
## ptt = constructPtTensor(fncf=fncf)
source(fncf)
load(sprintf('%s/ptt.RData', dndata))
t.trte = splitTrainTestTensor(ptt, fncf=fncf)
ptt = t.trte[['tr']]
h = maskPtTensorImport(ptt, fncf=fncf)
rmicegp.tr = temporalMICEGP(h, m=1, trte='tr', nimp=nimp, ncores=20, param=param, fncf=fncf, nug_thres=25)



