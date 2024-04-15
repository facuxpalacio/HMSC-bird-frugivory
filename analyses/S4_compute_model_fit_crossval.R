#Note: This is very slow to run!

setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo/models")

library(Hmsc)

#load fitted model
load("models_thin_1000_samples_250_chains_4.Rdata")

nChains = 4
samples = 250
thin = 1000

MF = list()
MFCV = list()
WAIC = list()

for(n in 1:2){
  m = models[[n]]
  preds = computePredictedValues(m)
  MF[[n]] = evaluateModelFit(hM=m, predY=preds)
  partition = createPartition(m, nfolds = 10)
  preds = computePredictedValues(m, partition=partition, nParallel = nChains)
  MFCV[[n]] = evaluateModelFit(hM=m, predY=preds)
  WAIC[[n]] = computeWAIC(m)       
}

filename_out = paste("MF_models_thin_", as.character(thin),
                     "_samples_", as.character(samples),
                     "_chains_",as.character(nChains),
                     ".Rdata",sep = "")

save(MF, MFCV, WAIC, modelnames, file = filename_out)

