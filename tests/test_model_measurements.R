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
	expect_equal(model_total_actual_pos, 30)
	expect_equal(model_total_actual_neg, 2000)
	expect_equal(model_total_observations, 2030)
	model_sensitivity = sensitivity(true_pos=model_true_pos, total_actual_pos=model_total_actual_pos)
	model_specificity = specificity(true_neg=model_true_neg, total_actual_neg=model_total_actual_neg)
	model_false_negative_rate = false_negative_rate(false_neg=model_false_neg, total_actual_pos=model_total_actual_pos)
	model_false_positive_rate = false_positive_rate(false_pos=model_false_pos, total_actual_neg=model_total_actual_neg)
	model_accuracy = accuracy(true_neg=model_true_neg, true_pos=model_true_pos, total_observations=model_total_observations)
	model_error_rate = error_rate(false_pos=model_false_pos, false_neg=model_false_neg, total_observations=model_total_observations)
	model_positive_predictive_value = positive_predictive_value(true_pos=model_true_pos, false_pos=model_false_pos)
	model_negative_predictive_value = negative_predictive_value(true_neg=model_true_neg, false_neg=model_false_neg)
	expect_equal(model_sensitivity, model_true_pos / (model_true_pos+model_false_neg))
	expect_equal(model_specificity, model_true_neg / (model_false_pos + model_true_neg))
	expect_equal(model_false_negative_rate, model_false_neg / (model_true_pos + model_false_neg))
	expect_equal(model_false_positive_rate, model_false_pos / (model_false_pos + model_true_neg))
	expect_equal(model_accuracy, (model_true_neg + model_true_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_error_rate, (model_false_neg + model_false_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_positive_predictive_value, model_true_pos / (model_true_pos + model_false_pos))
	expect_equal(model_negative_predictive_value, model_true_neg / (model_false_neg + model_true_neg))

	# use same tp/tn/fp/fn as above
	model_quality = quality_of_model(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg)
	expect_equal(model_quality$sensitivity, model_true_pos / (model_true_pos+model_false_neg))
	expect_equal(model_quality$specificity, model_true_neg / (model_false_pos + model_true_neg))
	expect_equal(model_quality$false_negative_rate, model_false_neg / (model_true_pos + model_false_neg))
	expect_equal(model_quality$false_positive_rate, model_false_pos / (model_false_pos + model_true_neg))
	expect_equal(model_quality$accuracy, (model_true_neg + model_true_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_quality$error_rate, (model_false_neg + model_false_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_quality$positive_predictive_value, model_true_pos / (model_true_pos + model_false_pos))
	expect_equal(model_quality$negative_predictive_value, model_true_neg / (model_false_neg + model_true_neg))

	expect_equal(model_quality$actual_pos_prob, (model_true_pos + model_false_neg) / model_total_observations)
	expect_equal(model_quality$actual_neg_prob, (model_false_pos  + model_true_neg) / model_total_observations)
	expect_equal(model_quality$total_observations, 2030)

	expect_equal(logistic_response_function(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), 0.2689414, tolerance=0.0001)
	expect_equal(logit(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), -1)
	expect_equal(odds(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), 0.3678794, tolerance=0.0001)
})
