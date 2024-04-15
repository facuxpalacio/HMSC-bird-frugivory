setwd("/Users/marijone/Documents/Mirkka Documents BAU/BAU projects/Hmsc course November 2020/Participant datasets/palacio_facundo")
localDir = "."
ModelDir = file.path(localDir, "models")
DataDir = file.path(localDir, "data")
library(Hmsc)

load(file=file.path(DataDir,"allData_corr.R")) #S,X,Y,Tr,P

# Y = number of fruit removed from Spiny Hackberry Celtis tala trees by each of 14 bird species.
# Samples are observations of individual trees in three different years (2013-2015).

# Check for always absent (always 0) or ubiquitous (1) Y data.
range(colMeans(Y>0))
#0.006329114 0.151898734

min(colSums(Y>0))
# =1.

sparsedata = which(colSums(Y>0)<5)
length(sparsedata)
# 6 of the original 14 species have less than 5 observations of fruit removal. 
# These are removed from the model.

Y = Y[,-sparsedata]
# Excluding rare taxa leaves 8 bird species in the analysis.

hist(colMeans(Y>0),main="prevalence")
# typical prevalence distribution (many species are rare), need zero-inflated model

hist(as.matrix(log(Y[Y>0])),main="log abundance conditional on presence")

# log-normal model will be applied to log(abundance) conditional on presence

table(rowSums(Y>0))
# bird species numbers removing fruit per tree/observation time (range from 0-4)

X[, 2:7] = apply(X[, 2:7], 2, as.numeric)
plot(X)
summary(X)
X$log.Fruit_crop = log(X$Fruit_crop)
X$Effort = as.factor(X$Effort)


XFormula = ~Effort + log.Fruit_crop + xFruit_diameter + xSugar_concentration + xSeed_weight

# Max. correlation between any two covariates = 0.47, between Seed weight and Fruit diameter.
#cor(X[,c(2, 3, 4, 8, 9)])

Tr = droplevels(Tr[-sparsedata,])
Tr$log.Mass = log(Tr$Mass)
Tr$log.Gape_width = log(Tr$Gape_width)
plot(Tr)

# Handling = 2 categories (gulper vs pulp_consumer)
TrFormula = ~Handling + log.Gape_width

head(S)
S[, 3:4] = apply(S[, 3:4], 2, as.numeric)
plot(S$Tree_x,S$Tree_y)
sum(duplicated(S[,3:4]))

# All coordinates are unique.


studyDesign = data.frame(tree = as.factor(S$Tree), year = as.factor(S$Year))

xy = data.frame(x=S$Tree_x,y=S$Tree_y)
row.names(xy) = studyDesign$tree
rL.tree = HmscRandomLevel(sData = xy, longlat = TRUE)

Yr = studyDesign$year
rL.year = HmscRandomLevel(units = levels(Yr))

Ypa = 1*(Y>0)
Yabu = Y
Yabu[Y==0] = NA
Yabu=log(Yabu)


m1 = Hmsc(Y=Ypa, XData = X,  XFormula = XFormula,
          TrData = Tr,TrFormula = TrFormula,
          distr="probit",
          studyDesign=studyDesign,
          ranLevels={list("tree"=rL.tree, "year" = rL.year)})

m2 = Hmsc(Y=Yabu, YScale = TRUE,
          XData = X,  XFormula = XFormula,
          TrData = Tr,TrFormula = TrFormula,
          distr="normal",
          studyDesign=studyDesign,
          ranLevels={list("tree"=rL.tree, "year" = rL.year)})

models = list(m1, m2)
modelnames = c("presence_absence","abundance_COP")

save(models,modelnames,file = file.path(ModelDir, "unfitted_models.Rdata"))
