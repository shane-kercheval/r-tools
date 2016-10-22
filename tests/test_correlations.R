source('../tools.R', chdir=TRUE)
library('testthat')
library(reshape2) # melt

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
	correlations = get_correlations(df, corr_threshold=0.4, p_value_threshold=1)
	expect_that(length(colnames(correlations)), equals(4))
	expect_true(any(colnames(correlations) == 'a'))
	expect_true(any(colnames(correlations) == 'b'))
	expect_true(any(colnames(correlations) == 'c'))
	expect_true(any(colnames(correlations) == 'd'))
	expect_true(!any(colnames(correlations) == 'e'))

	melted_correlations = melt(correlations)
	correct_correlations = c(1.00, -1.00, 0.98, 0.43, -1.00, 1.00, -0.98, -0.43, 0.98, -0.98, 1.00, 0.51, 0.43,  -0.43, 0.51, 1.00)
	expect_that(melted_correlations$value, equals(correct_correlations, tolerance=0.001))

	correlations = get_correlations(df, corr_threshold=0.4, p_value_threshold=0.1)
	melted_correlations = melt(correlations)
	correct_correlations = c(1.00, -1.00, 0.98, NA, -1.00, 1.00, -0.98, NA, 0.98, -0.98, 1.00, NA, NA,  NA, NA, 1.00)
	expect_that(melted_correlations$value, equals(correct_correlations, tolerance=0.001))


	# test with tolerance above minimum number to make sure it works
	correlations_same = get_correlations(df, corr_threshold=0.5, p_value_threshold=0.1)
	# this should be the same as before because even though we have a higher tolerance, the low-pvalue still filters out the 0.51 numbers
	all(correlations == correlations_same, na.rm = TRUE)
	all_equal = all(map2_lgl(as.list(melt(correlations)$value), as.list(melt(correlations_same)$value), .f=function(x,y){
		if(is.na(x) || is.na(y))
		{
			if(!is.na(x) || !is.na(y)) # if either x or y are NA (above) AND x is not NA or y is not NA, then they aren't equal, so return false, otherwise return true.
			{
				return (FALSE)
			}
			return (TRUE)
		}
		return (x == y)
	}))
	expect_true(all_equal)

	correlations = get_correlations(df, corr_threshold=0.5, p_value_threshold=0.15)
	expect_that(length(colnames(correlations)), equals(4))
	expect_true(any(colnames(correlations) == 'a'))
	expect_true(any(colnames(correlations) == 'b'))
	expect_true(any(colnames(correlations) == 'c'))
	expect_true(any(colnames(correlations) == 'd'))
	expect_true(!any(colnames(correlations) == 'e'))

	melted_correlations = melt(correlations)
	correct_correlations = c(1.00, -1.00, 0.98, NA, -1.00, 1.00, -0.98, NA, 0.98, -0.98, 1.00, 0.51, NA,  NA, 0.51, 1.00)
	expect_that(melted_correlations$value, equals(correct_correlations, tolerance=0.001))

	file = '../general/example_plot_correlations.png'
	file.remove(file)
	png(filename=file)
	plot_correlations(data_frame=df)
	dev.off()
	
})
