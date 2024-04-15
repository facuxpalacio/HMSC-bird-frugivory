setwd("/Users/mirkkajones/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")

library(Hmsc)
library(ggplot2)
library(abind)

load("C:/Users/Usuario/Documents/models_thin_1000_samples_250_chains_4.Rdata")

legendtext = c("A. badi", "E. parv", "M. satu", "M. mona", "P. coro", "P. sulp", "T. rufi", "Z. cape")

library(khroma)
discrete_rainbow <- colour("discrete rainbow")
colours = discrete_rainbow(ncol(models[[1]]$Y))

###################
          
          
          pdf("PRED_FruitRemoval_per_covariate.pdf", width = 8, height = 10)    
          #Plot includes the four continuous fruit traits only (predictions for the 2-level factor 'effort' are not plotted).
          par(mfrow = c(4, 2))
          covariates = c("log.Fruit_crop", "xFruit_diameter", "xSugar_concentration", "xSeed_weight")
          
          for(i in 1:4)
          {
            covariate = covariates[i]
            xlabel = switch(i, "log(Fruit crop)", "Fruit diameter", "Sugar concentration", "Seed weight")
            for(j in 1:2)
            {
            m = switch(j, models[[1]], models[[2]])
            
            plottitle = switch(j, "Fruit removal (binary)", "Fruit removal (log N fruits)")
            
            ylimits = switch(j, c(0,1), c(0,10))
            ylabel = switch(j, "Predicted probability of fruit removal", "Predicted log. N fruits removed")
            
            
            Gradient = constructGradient(m,focalVariable = covariate, non.focalVariables = 1)
            
            if(covariate == "Fruit_crop")
            {Gradient$XDataNew[, 1] = log(Gradient$XDataNew[, 1])}
            
            predY = abind(predict(m, Gradient=Gradient, expected = TRUE), along = 3)
            
            
            EpredY = apply(predY, c(1,2), mean)
            
            plotpredY = EpredY
            
            selgradient = Gradient$XDataNew[,1]
            
            Pr = vector()
            
            for(n in 1:ncol(EpredY))
              {
              Pr[n] = mean(apply(predY[2:20, n, ], 2, max)>predY[1,n, ]) 
              linetype = ifelse(Pr>0.95|Pr<0.05, 1, 2)
              linewidth = ifelse(Pr>0.95|Pr<0.05, 2, 0)
                if(n == 1)
                  plot(selgradient, plotpredY[,n], xlab = xlabel, main = plottitle, ylab = ylabel, ylim = ylimits, col = colours[n], type = "l", lwd = linewidth[n], lty = linetype[n])
                if(n > 1)
                  points(selgradient, plotpredY[,n], type = "l", col = colours[n], lwd = linewidth[n], lty = linetype[n])
                if(n == 1)
                  legend("topright", legend = legendtext, ncol = 3, fill = colours, cex=0.8)
              }
            }
          }
          dev.off()
          
          # 5x4.5 in
          colour = "darkgoldenrod2"
          cicol = adjustcolor("darkgoldenrod2", alpha.f = 0.2)
          
          for (i in 1:2){
            m = models [[i]]
            Gradient = constructGradient(m, focalVariable = "xSugar_concentration")
            predY = predict(m, Gradient = Gradient, expected = TRUE)
            q = c(0.25,0.5,0.75)
            #plotGradient(m, Gradient, pred = predY, measure = "S",
            #             showData = TRUE, q = q)
            plotGradient(m, Gradient, pred = predY, measure = "T", index = 2,
                         showData = TRUE, q = q, cicol = cicol)
          }
          
          colour = "darkgoldenrod2"
          cicol = adjustcolor("darkgoldenrod2", alpha.f = 0.2)
          
          for (i in 1:2){
            m = models [[i]]
            Gradient = constructGradient(m, focalVariable = "xFruit_diameter")
            predY = predict(m, Gradient = Gradient, expected = TRUE)
            q = c(0.25,0.5,0.75)
            #plotGradient(m, Gradient, pred = predY, measure = "S",
            #             showData = TRUE, q = q)
            plotGradient(m, Gradient, pred = predY, measure = "T", index = 2,
                         showData = TRUE, q = q, cicol = cicol)
          }
          
          for (i in 1:2){
            m = models [[i]]
            Gradient = constructGradient(m, focalVariable = "xSeed_weight")
            predY = predict(m, Gradient = Gradient, expected = TRUE)
            q = c(0.25,0.5,0.75)
            #plotGradient(m, Gradient, pred = predY, measure = "S",
            #             showData = TRUE, q = q)
            plotGradient(m, Gradient, pred = predY, measure = "T", index = 2,
                         showData = TRUE, q = q, cicol = cicol)
          }
