library(readr)
library(psych)
d4_all <- read.csv("dweckRawData.csv")
d4_all <- d4_all[,2:ncol(d4_all)]

nfactors <- 3
noRotation <-fa(r = d4_all, nfactors = nfactors, rotate = "none", fm = "minres")
rotation <- fa(r = d4_all, nfactors = nfactors, fm = "minres", rotate = "varimax")
rotation$loadings
