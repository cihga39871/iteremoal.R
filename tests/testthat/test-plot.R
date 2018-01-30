library(iteremoval)

result.complex <- feature_removal(SWRG1, SWRG0,
    cutoff1=0.95, cutoff0=0.925,
    offset=c(0.5, 1, 2))

# it is a ggplot2 object, so plus sign is available


test_that("ggiteration_trace", {
	ggit = ggiteration_trace(result.complex) + theme_bw()
})

test_that("prevalence", {
	# index is a proportion in 0-1
	prevalence.result1 <- feature_prevalence(result.complex, 0.5)

	# index is a positive integer
	prevalence.result2 <- feature_prevalence(result.complex, 233)

	feature.list <- feature_screen(prevalence.result1, 3)
	expect_equal(tail(feature.list),
				 c("Feature8","Feature82","Feature85",
				   "Feature9","Feature93","Feature94"))
})
