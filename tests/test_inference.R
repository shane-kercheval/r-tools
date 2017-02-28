library('testthat')
source('../probability/inference.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_inference.R")

test_that("general: original", {
	successes = c(50, 70)
	non_successes = c(950, 930)
	observations = c(1000,1000)

	test_results = chi.square.independence(s=successes, ns=non_successes)
	expect_that(test_results$p.value, equals(0.07362231))
	expect_that(as.numeric(test_results$estimate[1]), equals(successes[1]/observations[1]))
	expect_that(as.numeric(test_results$estimate[2]), equals(successes[2]/observations[2]))

	test_results = chi.square.independence(s=successes, o=observations)
	expect_that(test_results$p.value, equals(0.07362231))
	expect_that(as.numeric(test_results$estimate[1]), equals(successes[1]/observations[1]))
	expect_that(as.numeric(test_results$estimate[2]), equals(successes[2]/observations[2]))

	# prop.test like this is basically Chi-square of independene.
	pt_p1 = prop.test(x=successes, n=observations)$p.value # chi-square via prop.test.. http://www.marketingdistillery.com/2014/08/03/ab-tests-in-marketing-sample-size-and-significance-using-r/   https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
	pt_p2 = chi.square.independence(s=successes, o=observations)$p.value
	pt_p3 = chi.square.independence(s=successes, ns=non_successes)$p.value
	expect_equal(pt_p1, pt_p2, pt_p3, 0.07362231)
	ch_p = chisq.test(matrix(c(successes, non_successes), byrow = TRUE, nrow = 2))$p.value # with chisq.test you need to pass in non_successes, but
	expect_equal(pt_p1, ch_p)

	# correct=FALSE is like calculating by hand, confirmed in excel spreadsheet:
	pt_c = chi.square.independence(s=successes, o=observations, correct=FALSE)$p.value
	ch_c = chisq.test(matrix(c(successes, non_successes), byrow = TRUE, nrow = 2), correct = FALSE)$p.value
	expect_equal(pt_c, ch_c)

	bin.test = binom.test(x=8, n=3562, p = 3/3520, alternative = c("two.sided"), conf.level = 0.95)
	ab.bin = ab_test.binomial(3, 3520, 8, 3562)
	expect_equal(ab.bin$p.value, bin.test$p.value, 0.0126582)

	ptp_p1 = prop.test(70,1000, p=50/1000)$p.value # chi-square goodness of fit? http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-proportion
	ptp_p2 = chi.square.goodness_of_fit(s=successes, o=observations)$p.value
	ptp_p3 = chi.square.goodness_of_fit(s=successes, ns=non_successes)$p.value
	expect_equal(ptp_p1, ptp_p2, ptp_p3, 0.004664158)

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


test_that("general: inference", {
	mammogram_yes <- 500
	mammogram_no <- 44425
	control_yes <- 505
	control_no <- 44405
	z_prop_results = z.prop(mammogram_yes, control_yes, (mammogram_yes + mammogram_no), (control_yes + control_no))
	expect_equal(z_prop_results, -0.163933, tolerance=0.00001) # OpenIntro Statistics Mammogram example page 286-284
	expect_equal(convert.z.score(z_prop_results), 0.8697839, tolerance=0.000001)

	expect_equal(z.prop(30, 65, 74, 103), -2.969695, tolerance=0.00001) # https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
	
	# x = 30
	# x_total = 65
	# y = 74
	# y_total = 103
	# fisher.test(matrix(c(x, x_total - x, y, y_total-y), ncol=2))
	# 
	# tt = prop.test(x = c(x, x_total), n = c(y, y_total), correct = FALSE)$p.value
})

test_that("general: inference: bayes.t.test", {
	data = c(0.015, 0.028, 0.177, 0.121, 0.102, 0.107, 0.019, 0.066, 0.058, 0.111)
	bayes_t_test_results = bayes.t.test(numeric_vector=data)
	expect_equal(bayes_t_test_results$bayes_factor.H1.H2, 0.01539321, tolerance=0.00000001)
	expect_equal(bayes_t_test_results$bayes_factor.H2.H1, 64.96373, tolerance=0.00001)
	expect_equal(bayes_t_test_results$post.prob.H1, 0.01515985, tolerance=0.00000001)
	expect_equal(bayes_t_test_results$post.prob.H2, 0.9848402, tolerance=0.0000001)
	expect_equal(bayes_t_test_results$t.statistic, 4.863813, tolerance=0.0000001)
	expect_equal(bayes_t_test_results$p.value, 0.0008911155, tolerance=0.00000001)
	expect_equal(bayes_t_test_results$degrees.of.freedom, 9)
})
