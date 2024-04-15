setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")

library(Hmsc)
library(abind)

load("models/models_thin_1000_samples_250_chains_4.Rdata")
m = models[[1]]

colour = "darkgoldenrod2"
cicol = adjustcolor("darkgoldenrod2", alpha.f = 0.2)

covariates = all.vars(m$XFormula)[-1]
#Predictions for the four continuous covariates only
covariatenames = c("log(Fruit crop)", "Fruit diameter", "Sugar concentration", "Seed weight")
panels = c("a", "b", "c", "d")

maxmin = function(x){max(x)>x[1]}

# Species richness vs model covariates

pdf("TotalRemovalEvents_per_covariate.pdf", width = 8, height = 6)         
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

for(i in 1:4)
{
  covariate = covariates[i]
  covariatename = covariatenames[i]
  panelID = panels[i]
    
    Gradient = constructGradient(m, focalVariable = covariate, non.focalVariables = 1)
    
    predY = predict(m, Gradient=Gradient, expected = TRUE) 
    EpredY = apply(abind(predY,along = 3), c(1,2), mean)
    
    predS = abind(lapply(predY, rowSums),along=2)
    qpredS = apply(predS, c(1), quantile, probs = c(0.025, 0.5, 0.975), na.rm=TRUE)
    
    lo = qpredS[1, ]
    hi = qpredS[3, ]
    me = qpredS[2, ]
    
    Pr = mean(apply(predS, 2, maxmin))
    linetype = ifelse(Pr>0.95|Pr<0.05, 1, 2)
    
    focalgradient = Gradient$XDataNew[,1]
    
    plot(focalgradient, me, xlab = covariatename, ylab = "Total removal events", ylim = c(0, 5), type = "l", lty = linetype, col = colour, lwd = 2)
    polygon(c(focalgradient, rev(focalgradient)), c(lo, rev(hi)), col =  cicol, border = FALSE)
    legend("topleft", panelID, cex = 1.2, bty = "n")        
  }
dev.off()


#####################################################################


load("models/models_thin_1000_samples_250_chains_4.Rdata")
m = models[[2]]

colour = "darkgoldenrod2"
cicol = adjustcolor("darkgoldenrod2", alpha.f = 0.2)

covariates = all.vars(m$XFormula)[-1]
#Predictions for the four continuous covariates only
covariatenames = c("log(Fruit crop)", "Fruit diameter", "Sugar concentration", "Seed weight")
panels = c("a", "b", "c", "d")

maxmin = function(x){max(x)>x[1]}

# Total individual abundance (log-transformed) vs model covariates

pdf("SumRemoval_per_covariate.pdf", width = 8, height = 6)         
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

for(i in 1:4)
{
  covariate = covariates[i]
  covariatename = covariatenames[i]
  panelID = panels[i]
  
  Gradient = constructGradient(m, focalVariable = covariate, non.focalVariables = 1)
  
  predY = predict(m, Gradient=Gradient, expected = TRUE) 
  EpredY = apply(abind(predY,along = 3), c(1,2), mean)
  
  predAB = abind(lapply(predY, rowSums),along=2)
  qpredAB = apply(predAB, c(1), quantile, probs = c(0.025, 0.5, 0.975), na.rm=TRUE)
  
  lo = qpredAB[1, ]
  hi = qpredAB[3, ]
  me = qpredAB[2, ]
  
  Pr = mean(apply(predAB, 2, maxmin))
  linetype = ifelse(Pr>0.95|Pr<0.05, 1, 2)
  
  focalgradient = Gradient$XDataNew[,1]
  
  plot(focalgradient, me, xlab = covariatename, ylab = "log(abundance)", ylim = c(0, 30), type = "l", lty = linetype, col = colour, lwd = 2)
  polygon(c(focalgradient, rev(focalgradient)), c(lo, rev(hi)), col =  cicol, border = FALSE)
  legend("topleft", panelID, cex = 1.2, bty = "n")        
}
dev.off()