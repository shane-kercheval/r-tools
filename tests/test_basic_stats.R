library('testthat')
library(reshape2) # melt
source('../general/basic_stats.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_basic_stats.R")

test_that("general: basic_stats", {
	vector = seq(from=0, to=100, by=1)

	percentile_matrix = create_percentile_matrix(list_of_datasets=list(vector), 'test')
	expect_true(all(percentile_matrix[1,1:13] == c(0, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 1, -0.5, 1.5)*100))
})
