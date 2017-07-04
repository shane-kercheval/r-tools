library('testthat')
library('caret')
library(purrr)
source('../general/model_measurements.R', chdir=TRUE)
source('../probability/bayes.R', chdir=TRUE)

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

	##### Compare with confusionMatrix from caret library
	lvs <- c("neg", "pos")
	truth <- factor(rep(lvs, times = c(model_true_neg+model_false_pos, model_true_pos + model_false_neg)),
					levels = rev(lvs))
	pred <- factor(
		c(
			rep(lvs, times = c(model_true_neg, model_false_pos)),
			rep(lvs, times = c(model_false_neg, model_true_pos))),
		levels = rev(lvs))

	#xtab <- table(pred, truth)
	#confusionMatrix(xtab)
	confusion_matrix_caret = confusionMatrix(pred, truth, positive = 'pos')
	model_quality = quality_of_model(conf_list)

	expect_equal(confusion_matrix_caret$overall[['Accuracy']], model_quality$accuracy)
	expect_equal(confusion_matrix_caret$overall[['Kappa']], model_quality$kappa)
	expect_equal(confusion_matrix_caret$byClass[['Sensitivity']], model_quality$sensitivity)
	expect_equal(confusion_matrix_caret$byClass[['Specificity']], model_quality$specificity)
	expect_equal(confusion_matrix_caret$byClass[['Pos Pred Value']], model_quality$positive_predictive_value)
	#expect_equal(confusion_matrix_caret$byClass[['Precision']], model_quality$positive_predictive_value)
	expect_equal(confusion_matrix_caret$byClass[['Neg Pred Value']], model_quality$negative_predictive_value)
	expect_equal(confusion_matrix_caret$byClass[['Prevalence']], model_quality$prevalence)
	#expect_equal(confusion_matrix_caret$byClass[['Recall']], model_quality$recall)
	#expect_equal(confusion_matrix_caret$byClass[['Recall']], model_quality$sensitivity) # recall same as sensitivity

	# positive predictive value i.e. precision is same as bayes rule
	bayes_c = bayes_confusion(conf_list)
	expect_equal(bayes_c, model_quality$positive_predictive_value)

	# test visualization
	file_name = '../readme/quality_of_model_plot.png'
	file.remove(file_name)
	quality_plot = visualize_quality_of_model(conf_list)
	ggsave(plot=quality_plot, filename=file_name)
	expect_true(file.exists(file_name))

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

test_that("gain_lift_table: model_measurements", {
	require(purrr)
	actual_observations <- NULL
	events <- c(2179, 1753, 396, 111, 110, 85, 67, 69, 49, 55)
	walk(events, ~ {
		temp_events <- rep('yes', .)
		temp_events_no <- rep('no', 2500 - .)
		actual_observations <<- c(actual_observations, temp_events, temp_events_no)
	})

	gl_table <- gain_lift_table(actual_observations = actual_observations, predicted_probabilities = NULL, number_of_bins = 10, target_positive_class = 'yes')
	#saveRDS(gl_table, file = './data/model_measurements_gain_lift_table.RDS')
	expected_gain_lift_table <- readRDS(file = './data/model_measurements_gain_lift_table.RDS')
	expect_true(all.equal(gl_table, expected_gain_lift_table))
	
	expect_equal(sum(gl_table$percentage_of_events), 1) # table should account for 100% of events
	
	file.remove('../general/example_gain_chart.png')
	file.remove('../general/example_lift_chart.png')

	gl_charts <- gain_lift_charts(gl_table = gl_table)
	ggsave(filename = '../general/example_gain_chart.png', plot = gl_charts[[1]])
	ggsave(filename = '../general/example_lift_chart.png', plot = gl_charts[[2]])

	expect_true(file.exists('../general/example_gain_chart.png'))
	expect_true(file.exists('../general/example_lift_chart.png'))
})

test_that("calibration_table: model_measurements", {
	predicted_probabilities <- readRDS('./data/calibration_predicted_probabilities.RDS')
	actual_observations <- readRDS('./data/calibration_actual_observations.RDS')

	cal_table <- calibration_table(actual_observations = actual_observations, predicted_probabilities = predicted_probabilities, target_positive_class = 'yes')
	#saveRDS(cal_table, file = './data/model_measurements_calibration_table.RDS')
	expected_cal_table <- readRDS(file = './data/model_measurements_calibration_table.RDS')
	expect_true(all.equal(cal_table, expected_cal_table))
	
	expect_equal(sum(cal_table$number_of_observations), 100)
	expect_equal(sum(cal_table$number_of_events), 30)
	
	file.remove('../general/example_calibration_chart.png')
	
	cal_chart <- calibration_chart(cal_table = cal_table)
	ggsave(filename = '../general/example_calibration_chart.png', plot = cal_chart)

	expect_true(file.exists('../general/example_calibration_chart.png'))
	
})

test_that('expected_value: expected_value_confusion_matrix, expected_value_with_priors_confusion_matrix', {
	# example from pg 196 of Data Science for Business. NOTE!!! because they round the TP/TN/FP/FN rates before they use them in the calculations, I get a different (i.e. the correct) answer, but I varified if I use the rounded values then I get the same answer they did.
	average_profit <- 200 - 100
	average_cost <- 1
	gain_true_positive <- average_profit - average_cost # positive is gain
	gain_true_negative <- 0
	gain_false_positive <- -average_cost # negative is cost
	gain_false_negative <- 0
	gain_cost_matrix <- matrix(c(gain_true_negative, gain_false_positive, gain_false_negative, gain_true_positive), nrow = 2, byrow = TRUE, dimnames = list(c('Actual Negative', 'Actual Positive'), c('Predicted Negative', 'Predicted Positive')))
	confusion_matrix <- matrix(c(42, 7, 5, 56), nrow = 2, byrow = TRUE, dimnames = list(c('Actual Negative', 'Actual Positive'), c('Predicted Negative', 'Predicted Positive')))

	expected_profit <- expected_value_confusion_matrix(confusion_matrix = confusion_matrix, gain_cost_matrix = gain_cost_matrix)
	expected_profit_with_priors <- expected_value_with_priors_confusion_matrix(confusion_matrix = confusion_matrix, gain_cost_matrix = gain_cost_matrix, class_prior_positive_rate = 0.55)
	expect_equal(round(expected_profit, 5), 50.33636)
	expect_equal(expected_profit_with_priors, 49.9226)
})

test_that("expected_value_chart: model_measurements", {
	list_predictions_actuals <- readRDS('./data/predictions_actuals.RDS')
	predicted_probabilities_positive <- list_predictions_actuals[[1]] # predicted probabilities (of positive outcome)
	actual_outcomes <- list_predictions_actuals[[2]]
	
	threshold <- 0.5
	predictions_outcomes <- predicted_probabilities_positive > 0.5
	#conf_list <- confusion_list_from_confusion(table(actual_outcomes, predictions_outcomes))
	
	average_profit <- 5
	average_cost <- 2
	gain_true_positive <- average_profit - average_cost # positive is gain
	gain_true_negative <- 0
	gain_false_positive <- -average_cost # negative is cost
	gain_false_negative <- -(average_profit - average_cost) # more than likely, opportunity cost, but still should be counted (opportunity cost of not making the profit, but had we labed as a positive, we would have also incurred the average_cost (e.g. of mailing the letter, etc.))
	gain_cost_matrix <- matrix(c(gain_true_negative, gain_false_positive, gain_false_negative, gain_true_positive), nrow = 2, byrow = TRUE, dimnames = list(c('Actual Negative', 'Actual Positive'), c('Predicted Negative', 'Predicted Positive')))
	
	# table(actual_outcomes, predictions_outcomes)
	# gain_cost_matrix
	
	expected_value <- expected_value_confusion_matrix(confusion_matrix = table(actual_outcomes, predictions_outcomes), gain_cost_matrix = gain_cost_matrix)
	expect_equal(expected_value, -0.08)

	expected_value_plot <- expected_value_chart(predicted_probabilities_positive = predicted_probabilities_positive, actual_outcomes = actual_outcomes, gain_cost_matrix = gain_cost_matrix)
	expected_value_plot
	file.remove('../general/example_expected_value_chart.png')
	ggsave(filename = '../general/example_expected_value_chart.png', plot = expected_value_plot)
	expect_true(file.exists('../general/example_expected_value_chart.png'))
})


test_that("model_measurements: check_data", {

	number_of_predictors <- 10
	number_of_rows <- 100
	dataset <- expand.grid(paste0('var', seq(from = 1, to = number_of_predictors, by = 1)),
					seq(from = 1, to = number_of_rows/number_of_predictors, by =1),
					stringsAsFactors = FALSE) %>%
			mutate(target = as.character(seq(from = 1, to = number_of_rows, by =1))) %>%
			spread(Var1, Var2)

	# no error
	check_data(dataset = dataset, sample_to_predictor_ratio_threshold = 10, class_to_predictor_ratio_threshold = NULL, class_names = NULL)

	# adding an extra predictor, so that the sample to predictor ratio decreases
	expected_error = tryCatch(
		check_data(dataset = dataset %>% mutate(extra_predictor = 1:nrow(dataset)), sample_to_predictor_ratio_threshold = 10, class_to_predictor_ratio_threshold = NULL, class_names = NULL),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('sample_to_predictor_ratio >= sample_to_predictor_ratio_threshold is not TRUE'))
	
	# removing a sample, so that the sample to predictor ratio decreases
	expected_error = tryCatch(
		check_data(dataset = dataset[-1,], sample_to_predictor_ratio_threshold = 10, class_to_predictor_ratio_threshold = NULL, class_names = NULL),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('sample_to_predictor_ratio >= sample_to_predictor_ratio_threshold is not TRUE'))
	
	# no error - even though we removed a sample, we lowered the threshold
	check_data(dataset = dataset[-1,], sample_to_predictor_ratio_threshold = 9, class_to_predictor_ratio_threshold = NULL, class_names = NULL)

	# check ratio of samples for each class
	target_yes <- 'yes'
	target_no <- 'no'
	classification_dataset <- dataset %>% mutate(target = c(as.character(rep(target_yes, times = 20)), as.character(rep(target_no, times = 80))))
	
	check_data(dataset = classification_dataset, sample_to_predictor_ratio_threshold = 10, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no))
	check_data(dataset = classification_dataset, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no))
	
	# increased the sample to predictor threshold, make sure it still works
	expected_error = tryCatch(
		check_data(dataset = classification_dataset, sample_to_predictor_ratio_threshold = 11, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no)),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('sample_to_predictor_ratio >= sample_to_predictor_ratio_threshold is not TRUE'))

	# added a predictor, lower the class sample to prediction ratio
	expected_error = tryCatch(
		check_data(dataset = classification_dataset %>% mutate(extra_predictor = 1:nrow(dataset)), sample_to_predictor_ratio_threshold = NULL, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no)),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('class_samples_to_predictor_ratio >= class_to_predictor_ratio_threshold is not TRUE'))

	# removed a sample from the smaller class, lower the class sample to prediction ratio
	expected_error = tryCatch(
		check_data(dataset = classification_dataset[-1, ], sample_to_predictor_ratio_threshold = NULL, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no)),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('class_samples_to_predictor_ratio >= class_to_predictor_ratio_threshold is not TRUE'))

	# but now remove a sample from the class that already has enough samples, so no error expected
	check_data(dataset = classification_dataset[-100, ], sample_to_predictor_ratio_threshold = NULL, class_to_predictor_ratio_threshold = 2, class_names = c(target_yes, target_no))

	# remove one of the required parameters
	expected_error = tryCatch(
		check_data(dataset = classification_dataset[-1, ], sample_to_predictor_ratio_threshold = NULL, class_to_predictor_ratio_threshold = NULL, class_names = c(target_yes, target_no)),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('!is.null(class_to_predictor_ratio_threshold) && !is.null(class_names) is not TRUE'))
	
	# remove one of the required parameters
	expected_error = tryCatch(
		check_data(dataset = classification_dataset[-1, ], sample_to_predictor_ratio_threshold = NULL, class_to_predictor_ratio_threshold = 2, class_names = NULL),
		error=function(e) e)
	expect_true(inherits(expected_error, 'error'))
	expect_that(expected_error$message, equals('!is.null(class_to_predictor_ratio_threshold) && !is.null(class_names) is not TRUE'))
})
