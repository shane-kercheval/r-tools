library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_ab_tests.R")

test_that("general: ab_tests", {
	successes = c(50, 70)
	non_successes = c(950, 930)
	observations = c(1000,1000)

	test_results = ab_test.prop(s=successes, ns=non_successes)
	expect_that(test_results$p.value, equals(0.07362231))
	expect_that(as.numeric(test_results$estimate[1]), equals(successes[1]/observations[1]))
	expect_that(as.numeric(test_results$estimate[2]), equals(successes[2]/observations[2]))

	test_results = ab_test.prop(s=successes, o=observations)
	expect_that(test_results$p.value, equals(0.07362231))
	expect_that(as.numeric(test_results$estimate[1]), equals(successes[1]/observations[1]))
	expect_that(as.numeric(test_results$estimate[2]), equals(successes[2]/observations[2]))

	#chisq.test(matrix(c(successes, non_successes), byrow = TRUE, nrow = 2)) # chi-square
	pt_p1 = prop.test(x=successes, n=observations)$p.value # chi-square via prop.test.. http://www.marketingdistillery.com/2014/08/03/ab-tests-in-marketing-sample-size-and-significance-using-r/   https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
	pt_p2 = ab_test.prop(s=successes, o=observations)$p.value
	pt_p3 = ab_test.prop(s=successes, ns=non_successes)$p.value
	expect_equal(pt_p1, pt_p2, pt_p3)

	ptp_p1 = prop.test(70,1000, p=50/1000)$p.value # z/t test for proportions? http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-proportion
	ptp_p2 = ab_test.propp(s=successes, o=observations)$p.value
	ptp_p3 = ab_test.propp(s=successes, ns=non_successes)$p.value
	expect_equal(ptp_p1, ptp_p2, ptp_p3)

	fisher_p1 = fisher.test(rbind(c(50, 70), c(950, 930)))$p.value
	fisher_p2 = ab_test.fisher(s=successes, o=observations)$p.value
	fisher_p3 = ab_test.fisher(s=successes, ns=non_successes)$p.value
	expect_equal(fisher_p1, fisher_p2, fisher_p3)

# 	fisher.test(rbind(c(188, 220), c(1881, 1606)))$p.value
# 	prop.test(x=c(188, 220), n=c(2069, 1826)) # chi-square via prop.test.. http://www.marketingdistillery.com/2014/08/03/ab-tests-in-marketing-sample-size-and-significance-using-r/   https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
# 	prop.test(220,1826, p=188/2069) # z-test?? http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-proportion
# 	prop.test(x=c(30,65), n=c(74,103))
# 	ab_test(s=c(30,65), o=c(74,103))
# 	prop.test(65, 103, p=10/74)
# 	prop.test(x=c(30,65), n=c(74,103))
	#https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
})
