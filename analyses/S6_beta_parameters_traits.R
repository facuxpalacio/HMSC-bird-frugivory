setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")

library(Hmsc)

setwd("C:/Users/Usuario/Documents")
load("models_thin_1000_samples_250_chains_4.Rdata")

library(Hmsc)
library(writexl)

for(j in 1:2)
{
  m = models[[j]]
  postBeta = getPostEstimate(m, parName="Beta")
  me = as.data.frame(t(postBeta$mean))
  me = cbind(m$spNames,me)
  colnames(me) = c("Species",m$covNames)
  po = as.data.frame(t(postBeta$support))
  po = cbind(m$spNames,po)
  colnames(po) = c("Species",m$covNames)
  ne = as.data.frame(t(postBeta$supportNeg))
  ne = cbind(m$spNames,ne)
  colnames(ne) = c("Species",m$covNames)
  vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
  
  Betameans = t(postBeta$mean[-1,])
  colnames(Betameans) = m$covNames[-1]
  plotting_data = cbind(m$TrData[,3:4], Betameans)
  colnames(plotting_data)[1:2] = c("Handling", "Gape_width")
  colnames(plotting_data)[-c(1:2)] = paste0("Beta_", colnames(plotting_data)[-c(1:2)])
  filename2 = paste0("BirdTraits_Betameans_",modelnames[j],".csv")
  write.csv(plotting_data, filename2)
}
