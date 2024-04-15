setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")

library(Hmsc)

#Fitted models
load("C:/Users/Usuario/Documents/models_thin_1000_samples_250_chains_4.Rdata")

# Cross-validation results
load("C:/Users/Usuario/Documents/MF_models_thin_1000_samples_250_chains_4.Rdata")

for(j in 1:2){
  
m = models[[j]]

preds = computePredictedValues(m)
VP = computeVariancePartitioning(m)
MF = evaluateModelFit(hM=m, predY=preds)
if(j == 1)
{R2 = MF$TjurR2
AUC = MF$AUC}
if(j == 2)
{R2 = MF$R2}
for(k in 1:m$ns){
VP$vals[,k] = R2[k]*VP$vals[,k]
}


if(j == 1)
  {
  mfcv = MFCV[[j]]
  PAresults = round(data.frame(R2, AUC, mfcv$TjurR2, mfcv$AUC, t(VP$vals)*100), 3)
  names(PAresults) = c("Tjur R2", "AUC", "cv Tjur R2", "cv AUC", "Effort", "log.Fruit_crop", "Fruit_diameter", "Sugar_concentration", "Seed_weight", "rL.Tree", "rL.Year")
  write.csv(PAresults, "PAmodel_R2_AUC_cvR2_cvAUC_VarPart_fractions_20240323.csv")
}

if(j == 2)
{
  mfcv = MFCV[[j]]
  ABUresults = round(data.frame(R2, mfcv$R2, t(VP$vals)*100), 3)
  names(ABUresults) = c("R2", "cv R2", "Effort", "log.Fruit_crop", "Fruit_diameter", "Sugar_concentration", "Seed_weight", "rL.Tree", "rL.Year")
  write.csv(ABUresults, "ABUmodel_R2_cvR2_VarPart_fractions_20240323.csv")
}
}


