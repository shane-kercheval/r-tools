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

	conf_list = confusion_list(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg)
	expect_equal(conf_list$true_pos, model_true_pos)
	expect_equal(conf_list$true_neg, model_true_neg)
	expect_equal(conf_list$false_pos, model_false_pos)
	expect_equal(conf_list$false_neg, model_false_neg)
	expect_equal(conf_list$actual_pos, model_total_actual_pos)
	expect_equal(conf_list$actual_neg, model_total_actual_neg)
	expect_equal(conf_list$total, sum(model_true_pos, model_true_neg, model_false_pos, model_false_neg))

	# |                  | Predicted Negative | Predicted Positive |
	# | ---------------- | ------------------ | ------------------ |
	# | Actual Negative  | True Negative      | False Positive     |
	# | Actual Positive  | False Negative     | True Positive      |
	confusion_matrix = matrix(c(model_true_neg, model_false_pos, model_false_neg, model_true_pos), nrow=2, ncol=2, byrow = TRUE)
	conf_list = confusion_list_from_confusion(confusion_matrix)
	expect_equal(conf_list$true_pos, model_true_pos)
	expect_equal(conf_list$true_neg, model_true_neg)
	expect_equal(conf_list$false_pos, model_false_pos)
	expect_equal(conf_list$false_neg, model_false_neg)
	expect_equal(conf_list$actual_pos, model_total_actual_pos)
	expect_equal(conf_list$actual_neg, model_total_actual_neg)
	expect_equal(conf_list$total, sum(model_true_pos, model_true_neg, model_false_pos, model_false_neg))

	model_total_observations = model_total_actual_pos + model_total_actual_neg
	expect_equal(model_total_actual_pos, 30)
	expect_equal(model_total_actual_neg, 2000)
	expect_equal(model_total_observations, 2030)
	model_sensitivity = sensitivity(conf_list)
	model_specificity = specificity(conf_list)
	model_false_negative_rate = false_negative_rate(conf_list)
	model_false_positive_rate = false_positive_rate(conf_list)
	model_accuracy = accuracy(conf_list)
	model_error_rate = error_rate(conf_list)
	model_positive_predictive_value = positive_predictive_value(conf_list)
	model_negative_predictive_value = negative_predictive_value(conf_list)
	model_prevalence = prevalence(conf_list)
	expect_equal(model_sensitivity, model_true_pos / (model_true_pos+model_false_neg))
	expect_equal(model_specificity, model_true_neg / (model_false_pos + model_true_neg))
	expect_equal(model_false_negative_rate, model_false_neg / (model_true_pos + model_false_neg))
	expect_equal(model_false_positive_rate, model_false_pos / (model_false_pos + model_true_neg))
	expect_equal(model_accuracy, (model_true_neg + model_true_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_error_rate, (model_false_neg + model_false_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_positive_predictive_value, model_true_pos / (model_true_pos + model_false_pos))
	expect_equal(model_negative_predictive_value, model_true_neg / (model_false_neg + model_true_neg))
	expect_equal(model_prevalence, (model_true_pos + model_false_neg) / model_total_observations)

	# use same tp/tn/fp/fn as above
	model_quality = quality_of_model(conf_list)
	expect_equal(model_quality$sensitivity, model_true_pos / (model_true_pos+model_false_neg))
	expect_equal(model_quality$specificity, model_true_neg / (model_false_pos + model_true_neg))
	expect_equal(model_quality$false_negative_rate, model_false_neg / (model_true_pos + model_false_neg))
	expect_equal(model_quality$false_positive_rate, model_false_pos / (model_false_pos + model_true_neg))
	expect_equal(model_quality$accuracy, (model_true_neg + model_true_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_quality$error_rate, (model_false_neg + model_false_pos)/(model_true_neg+model_false_neg+model_false_pos+model_true_pos))
	expect_equal(model_quality$positive_predictive_value, model_true_pos / (model_true_pos + model_false_pos))
	expect_equal(model_quality$negative_predictive_value, model_true_neg / (model_false_neg + model_true_neg))
	expect_equal(model_quality$prevalence, (model_true_pos + model_false_neg) / model_total_observations)

	expect_equal(model_quality$actual_pos_prob, (model_true_pos + model_false_neg) / model_total_observations)
	expect_equal(model_quality$actual_neg_prob, (model_false_pos  + model_true_neg) / model_total_observations)
	expect_equal(model_quality$total_observations, 2030)

	expect_equal(logistic_response_function(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), 0.2689414, tolerance=0.0001)
	expect_equal(logit(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), -1)
	expect_equal(odds(b=-1.5, b1=3, b2=-0.5, x1=1, x2=5), 0.3678794, tolerance=0.0001)

	#specificity is 1-false_positive_rate
	expect_equal(specificity(conf_list), 1 - false_positive_rate(conf_list))
})
