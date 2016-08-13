library('testthat')
library(reshape2) # melt
source('../general/correlations.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_correlations.R")

test_that("general: correlations", {
	a = seq(from=1, to=10, by=1)
	b = seq(from=10, to=1, by=-1)
	c = c(1,1,2,2,3,3,4,4,5,5)
	d = c(1,4,6,2,9,4,6,7,9,3)
	e = c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b')

	df = data.frame(a, b, c, d, e)
	# test with tolerance below minimum number to make sure all cells are accurate
	correlations = get_correlations(df, 0.4)
	expect_that(length(colnames(correlations)), equals(4))
	expect_true(any(colnames(correlations) == 'a'))
	expect_true(any(colnames(correlations) == 'b'))
	expect_true(any(colnames(correlations) == 'c'))
	expect_true(any(colnames(correlations) == 'd'))
	expect_true(!any(colnames(correlations) == 'e'))
	
	melted_correlations = melt(correlations)
	correct_correlations = c(1.0000000, -1.0000000, 0.9847319, 0.4310690, -1.0000000, 1.0000000, -0.9847319, -0.4310690, 0.9847319, -0.9847319, 1.0000000, 0.5118338, 0.4310690,  -0.4310690, 0.5118338, 1.0000000)
	expect_that(melted_correlations$value, equals(correct_correlations, tolerance=0.001))

	# test with tolerance above minimum number to make sure it works
	correlations = get_correlations(df, 0.5)
	expect_that(length(colnames(correlations)), equals(4))
	expect_true(any(colnames(correlations) == 'a'))
	expect_true(any(colnames(correlations) == 'b'))
	expect_true(any(colnames(correlations) == 'c'))
	expect_true(any(colnames(correlations) == 'd'))
	expect_true(!any(colnames(correlations) == 'e'))
	
	melted_correlations = melt(correlations)
	correct_correlations = c(1.0000000, -1.0000000, 0.9847319, NA, -1.0000000, 1.0000000, -0.9847319, NA, 0.9847319, -0.9847319, 1.0000000, 0.5118338, NA,  NA, 0.5118338, 1.0000000)
	expect_that(melted_correlations$value, equals(correct_correlations, tolerance=0.001))
})
