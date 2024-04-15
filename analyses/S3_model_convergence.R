setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")

library(Hmsc)
library(vioplot)
library(colorspace)

load("./models/models_thin_1000_samples_250_chains_4.Rdata")

thin = 1000

maxOmega = 100

psrfbeta_summary = list()
psrfgamma_summary = list()
psrfomega_summary_space = list()
psrfomega_summary_year = list()

for(n in 1:2)
{
  m = models[[n]]
  
  nrows = length(m$covNames)*length(m$trNames)
  
  mpost = convertToCodaObject(m, spNamesNumbers = c(T,F), covNamesNumbers = c(T,F))
    
    psrf.beta = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
    psrfbeta_summary[[n]] = data.frame(psrf.beta[,1])
    
    psrf.gamma = gelman.diag(mpost$Gamma,multivariate=FALSE)$psrf
    psrfgamma_summary[[n]] = data.frame(psrf.gamma[,1])
    
    #RL 1 - space
    tmp = mpost$Omega[[1]]
    z = dim(tmp[[1]])[2]
    if(z > maxOmega){
      sel = sample(1:z, size = maxOmega)
      for(i in 1:length(tmp)){
        tmp[[i]] = tmp[[i]][,sel]
      }
    }
    psrf.omega = gelman.diag(tmp, multivariate = FALSE)$psrf
    psrfomega_summary_space[[n]] = data.frame(psrf.omega[,1])
    
    #RL 2 - year
    tmp = mpost$Omega[[2]]
    z = dim(tmp[[1]])[2]
    if(z > maxOmega){
      sel = sample(1:z, size = maxOmega)
      for(i in 1:length(tmp)){
        tmp[[i]] = tmp[[i]][,sel]
      }
    }
    psrf.omega = gelman.diag(tmp, multivariate = FALSE)$psrf
    psrfomega_summary_year[[n]] = data.frame(psrf.omega[,1])
}

max(psrfbeta_summary[[1]])
max(psrfbeta_summary[[2]])
max(psrfgamma_summary[[1]])
max(psrfgamma_summary[[2]])
max(psrfomega_summary_space[[1]])
max(psrfomega_summary_space[[2]])
max(psrfomega_summary_year[[1]])
max(psrfomega_summary_year[[2]])

mean(c(psrfbeta_summary[[1]][,1], psrfbeta_summary[[2]][,1], 
     psrfgamma_summary[[1]][,1],psrfgamma_summary[[2]][,1], 
     psrfomega_summary_space[[1]][,1], psrfomega_summary_space[[2]][,1],
  psrfomega_summary_year[[1]][,1], psrfomega_summary_year[[2]][,1]))

pdf("psrfs.pdf", height = 4, width = 8)
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
{
psrfbeta_summary2 = data.frame(unlist(psrfbeta_summary), c(sort(rep(1, nrow(psrfbeta_summary[[1]]))), sort(rep(2, nrow(psrfbeta_summary[[2]])))))
colnames(psrfbeta_summary2) = c("psrf.beta", "modelN")   
vioplot(psrf.beta~modelN,data=psrfbeta_summary2,col=rainbow_hcl(2),ylim = c(0.95,1.5), xlab = "Model")

psrfgamma_summary2 = data.frame(unlist(psrfgamma_summary), c(sort(rep(1, nrow(psrfgamma_summary[[1]]))), sort(rep(2, nrow(psrfgamma_summary[[2]])))))
colnames(psrfgamma_summary2) = c("psrf.gamma", "modelN")   
vioplot(psrf.gamma~modelN,data=psrfgamma_summary2,col=rainbow_hcl(2),ylim = c(0.95,1.5), xlab = "Model")

psrfomega_summary_space2 = data.frame(unlist(psrfomega_summary_space), sort(rep(1:2, nrow(psrfomega_summary_space[[1]]))))
colnames(psrfomega_summary_space2) = c("psrf.omega", "modelN")                        
vioplot(psrf.omega~modelN,data=psrfomega_summary_space2,col=rainbow_hcl(2),ylim = c(0.95,1.5), ylab = "psrf.omega space", xlab = "Model")

psrfomega_summary_year2 = data.frame(unlist(psrfomega_summary_year), sort(rep(1:2, nrow(psrfomega_summary_year[[1]]))))
colnames(psrfomega_summary_year2) = c("psrf.omega", "modelN")                        
vioplot(psrf.omega~modelN,data=psrfomega_summary_year2,col=rainbow_hcl(2),ylim = c(0.95,1.5), ylab = "psrf.omega year", xlab = "Model")
}
dev.off()