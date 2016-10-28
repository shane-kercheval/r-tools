library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_decision_analysis.R")

test_that("general: decision_analysis", {
	observations = c(56, 42, 7, 5)
	probabilities = observations / sum(observations)
	benefits = c(99, 0, -1, 0)
	
	expect_error(expected_value(benefits=benefits))
	expect_equal(expected_value(n_occur=observations, benefits=benefits), 50.33636, tolerance=0.00001)
	expect_equal(expected_value(probs=probabilities, benefits=benefits), 50.33636, tolerance=0.00001)
})
