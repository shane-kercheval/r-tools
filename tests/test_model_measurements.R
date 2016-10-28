library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_model_measurements.R")

test_that("general: model_measurements", {

	# example from https://en.wikipedia.org/wiki/Sensitivity_and_specificity (beware that they put the truth as columns and predictions as rows, whereas my tables are reverse, so the False Negative / False Positive will be swapped)
	model_true_neg = 1820
	model_false_neg = 10
	model_false_pos = 180
	model_true_pos = 20
	model_total_actual_pos = model_true_pos + model_false_neg
	model_total_actual_neg = model_true_neg + model_false_pos
	model_total_observations = model_total_actual_pos + model_total_actual_neg
	expect_that(model_total_actual_pos, equals(30))
	expect_that(model_total_actual_neg, equals(2000))
	expect_that(model_total_observations, equals(2030))
	model_sensitivity = sensitivity(true_pos=model_true_pos, total_actual_pos=model_total_actual_pos)
	model_specificity = specificity(true_neg=model_true_neg, total_actual_neg=model_total_actual_neg)
	model_false_negative_rate = false_negative_rate(false_neg=model_false_neg, total_actual_pos=model_total_actual_pos)
	model_false_positive_rate = false_positive_rate(false_pos=model_false_pos, total_actual_neg=model_total_actual_neg)
	model_accuracy = accuracy(true_neg=model_true_neg, true_pos=model_true_pos, total_observations=model_total_observations)
	model_error_rate = error_rate(false_pos=model_false_pos, false_neg=model_false_neg, total_observations=model_total_observations)
	model_positive_predictive_value = positive_predictive_value(true_pos=model_true_pos, false_pos=model_false_pos)
	model_negative_predictive_value = negative_predictive_value(true_neg=model_true_neg, false_neg=model_false_neg)
	expect_that(model_sensitivity, equals(20 / (20+10), tolerance=0.001))
	expect_that(model_specificity, equals(1820 / (180 + 1820), tolerance=0.001))
	expect_that(model_false_negative_rate, equals(10 / (20 + 10), tolerance=0.001))
	expect_that(model_false_positive_rate, equals(180 / (180 + 1820), tolerance=0.001))
	expect_that(model_accuracy, equals((1820 + 20)/(1820+10+180+20), tolerance=0.001))
	expect_that(model_error_rate, equals((10 + 180)/(1820+10+180+20), tolerance=0.001))
	expect_that(model_positive_predictive_value, equals(20 / (20 + 180), tolerance=0.001))
	expect_that(model_negative_predictive_value, equals(1820 / (10 + 1820), tolerance=0.001))

	# use same tp/tn/fp/fn as above
	model_quality = quality_of_model(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg)
	expect_that(model_quality$sensitivity, equals(20 / (20+10), tolerance=0.001))
	expect_that(model_quality$specificity, equals(1820 / (180 + 1820), tolerance=0.001))
	expect_that(model_quality$false_negative_rate, equals(10 / (20 + 10), tolerance=0.001))
	expect_that(model_quality$false_positive_rate, equals(180 / (180 + 1820), tolerance=0.001))
	expect_that(model_quality$accuracy, equals((1820 + 20)/(1820+10+180+20), tolerance=0.001))
	expect_that(model_quality$error_rate, equals((10 + 180)/(1820+10+180+20), tolerance=0.001))
	expect_that(model_quality$positive_predictive_value, equals(20 / (20 + 180), tolerance=0.001))
	expect_that(model_quality$negative_predictive_value, equals(1820 / (10 + 1820), tolerance=0.001))
	expect_that(model_quality$total_observations, equals(2030))

	expect_that(logistic_response_function(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), equals(0.2689414, tolerance=0.001))
	expect_that(logit(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), equals(-1, tolerance=0.001))
	expect_that(odds(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), equals(0.3678794, tolerance=0.001))

	# EXPECTED VALUE
	ex_value = expected_value_confusion(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg,
							  tp_cost_benefit=1, tn_cost_benefit=1, fp_cost_benefit=1, fn_cost_benefit=1)
	expect_equal(ex_value, 1) # this is because if all costs/benefits are equal to 1, it will just add up the probabilities, which should equal 1

	# often times we expect fp vs. fn to have different weights or concequences (e.g. false positive of court trial is sending innocent man to jail)
	model_true_pos = 5
	model_true_neg = 90
	model_false_pos = 4
	model_false_neg = 1
	# these are just 'ratios', but you could use actual costs and actual benefits (i.e. profit) in a business scenario if you have the numbers
	tp_cost_benefit = 10 # a true positive (e.g. cancer) is very benefitial
	tn_cost_benefit = 0 # a true negative doesn't cost or benefit anything
	fp_cost_benefit = -1 # a false positive (e.g. predicting cancer, when there actually was not) is expensive in that it costs money for additional tests, additional stress, etc., but it is not life-threatoning (i.e. we'd rather have false positives than false negatives)
	fn_cost_benefit = -5 # a false negative (e.g. predicting no cancer when there actually is cancer) is a matter of life and death... A quick guess at the ratio would be that is that it is worth half of a true positive (if for every life you save via detection, you also miss a detection and someone dies, then you are saving only about half the people who have cancer, which is still better than no one, so it's not a zero-sum (i.e. ratios shouldn't be the same between true positive and false negative))
	ex_value = expected_value_confusion(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg,
							  tp_cost_benefit=tp_cost_benefit, tn_cost_benefit=tn_cost_benefit, fp_cost_benefit=fp_cost_benefit, fn_cost_benefit=fn_cost_benefit)
	expect_equal(ex_value, 0.41)

	expect_equal(expected_value_confusion(56, 42, 7, 5, 99, 0, -1, 0), 50.33636, tolerance=0.00001)
})
