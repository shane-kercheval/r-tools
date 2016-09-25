library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_basic_stats.R")

test_that("general: basic_stats", {
	vector1 = seq(from=0, to=100, by=1)
	vector2 = seq(from=0, to=-100, by=-1)

	expected_quantiles = c(0, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 1)*100
	expected_outlier_thresholds = c(-50, 150)
	expected_perc_outside = c(0,0)

	percentile_matrix = create_percentile_matrix(list_of_datasets=list(vector1, vector2), row_names=c('test1', 'test2'))
	
	t1 = list(x=1, y=1:11, expected=expected_quantiles) # t stands for test
	t2 = list(x=1, y=12:13, expected=expected_outlier_thresholds)
	t3 = list(x=1, y=14:15, expected=expected_perc_outside)
	t4 = list(x=2, y=1:11, expected=rev(expected_quantiles)*-1)
	t5 = list(x=2, y=12:13, expected=rev(expected_outlier_thresholds)*-1)
	t6 = list(x=2, y=14:15, expected=rev(expected_perc_outside)*-1)
	test_cases = transpose(list(t1, t2, t3, t4, t5, t6)) # convert to single list, with 3 sublists representing `x`, `y`, & `expected` parameters which matches anonymous function's parameters
	results = pmap(test_cases, function(x,y,expected) expect_true(all(percentile_matrix[x,y] == expected))) # ensures that the matrix[x,y] equals the `expected` values where `y` can be a range and `expected` can be a vector of values matching the length of `y`
})
