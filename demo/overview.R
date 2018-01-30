
library(iteremoval)

removal.stat <- feature_removal(SWRG1, SWRG0, cutoff1=0.95, cutoff0=0.95,
								 offset=c(0.25, 0.5, 2, 4))

removal.stat <- feature_removal(SummarizedData, SummarizedData$Group==0,
								 cutoff1=0.95, cutoff0=0.95,
								 offset=c(0.25, 0.5, 2, 4))

ggiteration_trace(removal.stat) + theme_bw()

features <- feature_prevalence(removal.stat, index=255, hist.plot=TRUE)

feature_screen(features, prevalence=4)
