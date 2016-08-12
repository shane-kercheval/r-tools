library('testthat')
source('../general/model_measurements.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_model_measurements.R")


test_that("analytics edge: Unit 3: Logistic Regression - healthcare quality", {

	# example from https://en.wikipedia.org/wiki/Sensitivity_and_specificity (beware that they put the truth as columns and predictions as rows, whereas my tables are reverse, so the False Negative / False Positive will be swapped)
	model_true_negatives = 1820
	model_false_negatives = 10
	model_false_positives = 180
	model_true_positives = 20
	model_total_actual_positives = model_true_positives + model_false_negatives
	model_total_actual_negatives = model_true_negatives + model_false_positives
	model_total_observations = model_total_actual_positives + model_total_actual_negatives
	expect_that(model_total_actual_positives, equals(30))
	expect_that(model_total_actual_negatives, equals(2000))
	expect_that(model_total_observations, equals(2030))
	model_sensitivity = sensitivity(true_positives=model_true_positives, total_actual_positives=model_total_actual_positives)
	model_specificity = specificity(true_negatives=model_true_negatives, total_actual_negatives=model_total_actual_negatives)
	model_false_negative_rate = false_negative_rate(false_negatives=model_false_negatives, total_actual_positives=model_total_actual_positives)
	model_false_positive_rate = false_positive_rate(false_positives=model_false_positives, total_actual_negatives=model_total_actual_negatives)
	model_accuracy = accuracy(true_negatives=model_true_negatives, true_positives=model_true_positives, total_observations=model_total_observations)
	model_error_rate = error_rate(false_positives=model_false_positives, false_negatives=model_false_negatives, total_observations=model_total_observations)
	model_positive_predictive_value = positive_predictive_value(true_positives=model_true_positives, false_positives=model_false_positives)
	model_negative_predictive_value = negative_predictive_value(true_negatives=model_true_negatives, false_negatives=model_false_negatives)
	expect_that(model_sensitivity, equals(20 / (20+10), tolerance=0.001))
	expect_that(model_specificity, equals(1820 / (180 + 1820), tolerance=0.001))  
	expect_that(model_false_negative_rate, equals(10 / (20 + 10), tolerance=0.001))  
	expect_that(model_false_positive_rate, equals(180 / (180 + 1820), tolerance=0.001))  
	expect_that(model_accuracy, equals((1820 + 20)/(1820+10+180+20), tolerance=0.001))  
	expect_that(model_error_rate, equals((10 + 180)/(1820+10+180+20), tolerance=0.001))  
	expect_that(model_positive_predictive_value, equals(20 / (20 + 180), tolerance=0.001))  
	expect_that(model_negative_predictive_value, equals(1820 / (10 + 1820), tolerance=0.001))  

	# use same tp/tn/fp/fn as above
	model_quality = quality_of_model(true_positives=model_true_positives, true_negatives=model_true_negatives, false_positives=model_false_positives, false_negatives=model_false_negatives)
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
})
