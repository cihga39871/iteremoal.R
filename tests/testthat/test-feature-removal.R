library(iteremoval)

test_that("feature-removal, single offset", {
	result.simple.A <- feature_removal(SWRG1, SWRG0, cutoff1=0.95, cutoff0=0.95)
	expect_equal(result.simple.A$offset, 1)
	expect_equal(dim(result.simple.A$removed.scores), c(299,2))
	expect_equal(result.simple.A$max.scores[,2] %>% sum(na.rm = TRUE), 588,
				 tolerance = 0.1)
})


test_that("feature-removal, multiple offset", {
	result.simple.B <- feature_removal(SWRG1, SWRG0, cutoff1=0.95, cutoff0=0.95,
									   offset=c(0.5, 2))
	result.complex <- feature_removal(SWRG1, SWRG0,
	    cutoff1=0.95, cutoff0=0.925, lt=">",
	    offset=c(0.5, 2),
	    weight.method="reciprocal_colSums",
	    scoreStandardization.method="min_max",
	    scoreCombine.method="linear_combine")
})

test_that("feature-removal, SE data", {
	result.simple.C <- feature_removal(SummarizedData, SummarizedData$Group==0,
									   cutoff1=0.95, cutoff0=0.95)
	expect_equal(result.simple.C$offset, 1)
	expect_equal(dim(result.simple.C$removed.scores), c(299,2))
	expect_equal(result.simple.C$max.scores[,2] %>% sum(na.rm = TRUE), 588,
				 tolerance = 0.1)
})
