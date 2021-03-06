library('testthat')
library(purrr)
source('../general/basic_stats.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_basic_stats.R")

test_that("general: basic_stats", {
	vector1 = seq(from=0, to=100, by=1)
	vector2 = seq(from=0, to=-100, by=-1)
	vector3 = seq(from=0, to=100, by=1)
	vector3 = c(-1000, vector3, 1000) # put outliers in

	expected_quantiles = c(0, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 1)*100
	expected_outlier_thresholds = c(-50, 150)
	expected_perc_outside = c(0,0)

	percentile_matrix = create_percentile_matrix(list_of_datasets=list(vector1, vector2, vector3), row_names=c('test1', 'test2', 'test3-outliers'))
	
	t1 = list(x=1, y=1:11, expected=expected_quantiles) # t stands for test
	t2 = list(x=1, y=12:13, expected=expected_outlier_thresholds)
	t3 = list(x=1, y=14:15, expected=expected_perc_outside)
	t4 = list(x=2, y=1:11, expected=rev(expected_quantiles)*-1)
	t5 = list(x=2, y=12:13, expected=rev(expected_outlier_thresholds)*-1)
	t6 = list(x=2, y=14:15, expected=rev(expected_perc_outside)*-1)
	#t7 = list(x=3, y=1:11, expected=rev(expected_quantiles)*-1)
	t8 = list(x=3, y=12:13, expected=c(-52, 152))
	t9 = list(x=3, y=14:15, expected=c(0.01, 0.01))
	test_cases = transpose(list(t1, t2, t3, t4, t5, t6, t8, t9)) # convert to single list, with 3 sublists representing `x`, `y`, & `expected` parameters which matches anonymous function's parameters
	results = pmap(test_cases, function(x,y,expected) expect_true(all(percentile_matrix[x,y] == expected))) # ensures that the matrix[x,y] equals the `expected` values where `y` can be a range and `expected` can be a vector of values matching the length of `y`
})

# test_that("general: basic_stats, explore_numeric", {
# 	set.seed(1)
# 	random_sample = sample(1:1000, size = 20)
# 	
# 	explore_numeric(random_sample)
# })
