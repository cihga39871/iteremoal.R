
library(iteremoval)

g1 = SWR1[,5:ncol(SWR1)]
g0 = SWR0[,5:ncol(SWR0)]

row.names(g1) <- SWR1[,4]
row.names(g0) <- SWR0[,4]


removal.stat <- feature.removal(g1, g0, cutoff1=0.95, cutoff0=0.95)

removal.stat2 <- feature.removal(g1, g0, cutoff1=0.95, cutoff0=0.95, lt=">",
								 offset=c(0.5, 1, 2),
								 weight.method="reciprocal.colSums",
								 scoreStandardization.method="min_max",
								 scoreCombine.method="linear.combine")

ggiteration_trace(removal.stat2) + theme_bw()


features <- feature.prevalence(removal.stat2, index=0.5, hist.plot=TRUE)
features2 <- feature.prevalence(removal.stat2, index=255, hist.plot=TRUE)

screened.features <- feature.screen(features2, prevalence=3)
