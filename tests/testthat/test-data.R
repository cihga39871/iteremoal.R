library(iteremoval)

test_that("SWRG0 signiture", {
	expect_equal(length(SWRG0), 22)
	expect_equal(nrow(SWRG0), 299)
	expect_equal(rowSums(SWRG0) %>% log10 %>% sum(na.rm=T),
				 297.0726, tolerance = .0001)
})

test_that("SWRG1 signiture", {
	expect_equal(length(SWRG1), 22)
	expect_equal(nrow(SWRG1), 299)
	expect_equal(rowSums(SWRG1) %>% log10 %>% sum(na.rm=T),
				 310.207, tolerance = .0001)
})

test_that("SummarizedData signiture", {
	expect_equal(assay(SummarizedData[,SummarizedData$Group==0]) %>%
				 	rowSums %>% log10 %>% sum(na.rm=T),
				 297.0726, tolerance = 0.001)
	expect_equal(assay(SummarizedData[,SummarizedData$Group==1]) %>%
				 	rowSums %>% log10 %>% sum(na.rm=T),
				 310.207, tolerance = 0.001)
})
