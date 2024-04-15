#Note: Extremely slow to run!

library(Hmsc)

load(file = "unfitted_models.Rdata")

nChains = 4
samples = 250
thin = 1000

for(n in 1:2)
{
  m = models[[n]]
  m = sampleMcmc(m, samples = samples, thin=thin,
                 adaptNf=rep(ceiling(0.4*samples*thin),m$nr),
                 transient = ceiling(0.5*samples*thin),
                 nChains = nChains, nParallel = nChains)
  models[[n]] = m
}
filename_out = paste("models_thin_", as.character(thin),
                     "_samples_", as.character(samples),
                     "_chains_",as.character(nChains),
                     ".Rdata",sep = "")
save(models, modelnames, file=filename_out) 

