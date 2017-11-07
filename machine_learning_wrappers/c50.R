library(purrr)
library(C50)
library(caret)
c5.0_cost_tuning <- function(	training_data,
								target_positive_class,
								target_negative_class,
								param_grid,
								k_folds = 5,
								min_branches = 4,
								max_branches = 80,
								seed = 1,
								criteria_weights = c(	kappa = 2,
														sensitivity = 1,
														positive_predictive_value = 1),
								tree_file_name = NULL) {
	

	
	training_data <- training_data %>%
		dplyr::mutate(target_variable = factor(target_variable,
											   levels = c(target_negative_class, target_positive_class))) # expect the `negative` is the first level

	target_outcomes <- levels(training_data$target_variable)
	matrix_dimensions <- list(target_outcomes, target_outcomes)
	names(matrix_dimensions) <- c('predicted', 'actual')

	# for each combination of parameters, do k-fold cross validation, get mean of performance, and return all means
	param_grid <- param_grid %>% mutate(results = pmap(param_grid, function(cost, min_cases) {

		fold_error_cost <- matrix(c(0, 1, cost, 0), nrow = 2, dimnames = matrix_dimensions)
		fold_c50_control <- C5.0Control(minCases = min_cases)

		set.seed(seed)
		custom_folds <- createFolds(training_data$target_variable, k = k_folds) # maintains the `churn` ratio, returnTrain = TRUE means the training indexes will be returned, which is required by `index` below
		stopifnot(all(as.numeric(sort(unlist(custom_folds))) == 1:nrow(training_data))) # make sure there are no duplicates or missing indexes (i.e. each fold is the testing set)

		# custom cross-validation
		cv_results <- map(custom_folds, function(fold_indexes) { # for each fold, train on all other folds, test on current fold, then average the results.

			fold_train <- training_data[-fold_indexes, ] # training data for this loop gets all the data except for this loop's fold, which will be assigned to the test set 
			fold_test <- training_data[fold_indexes, ]
			fold_actual <- fold_test$target_variable

			set.seed(seed)
			fold_model <- C5.0(	fold_train %>% dplyr::select(-target_variable),
								fold_train$target_variable,
								costs = fold_error_cost,
								trials = 1,
								control=fold_c50_control)

			fold_pred_class <- predict(fold_model, fold_test, type = 'class')
			fold_confusion_matrix <- table(actual=fold_actual, predictions=fold_pred_class)
			fold_confusion_ls <- confusion_list_from_confusion(fold_confusion_matrix)
			fold_model_quality <- quality_of_model(fold_confusion_ls)
			#summary(fold_model)
			fold_model_quality$num_branches <- fold_model$size
			fold_model_quality <- fold_model_quality %>% dplyr::select(num_branches, everything())

			return (fold_model_quality)
		})

		model_quality_means <- data.frame( # get the mean of the stats for all the folds
			min_cases = min_cases,
			cost = cost,
			num_branches = mean(map_dbl(cv_results, ~ { .$num_branches })),
			kappa = mean(map_dbl(cv_results, ~ { .$kappa })),
			accuracy = mean(map_dbl(cv_results, ~ { .$accuracy })),
			sensitivity = mean(map_dbl(cv_results, ~ { .$sensitivity })),
			specificity = mean(map_dbl(cv_results, ~ { .$specificity })),
			positive_predictive_value = mean(map_dbl(cv_results, ~ { .$positive_predictive_value })))

		return (model_quality_means)
	}))

	c50_tune_results <- param_grid %>% 
		mutate(
			params = paste('min_cases:', min_cases, 'cost:', cost),
			num_branches = map_dbl(results, ~ .$num_branches),
			kappa = map_dbl(results, ~ .$kappa),
			accuracy = map_dbl(results, ~ .$accuracy),
			sensitivity = map_dbl(results, ~ .$sensitivity), 
			specificity = map_dbl(results, ~ .$specificity), 
			positive_predictive_value = map_dbl(results, ~ .$positive_predictive_value))
	
	kappa_percentile_dist <- ecdf(c50_tune_results$kappa)
	accuracy_percentile_dist <- ecdf(c50_tune_results$accuracy)
	sensitivity_percentile_dist <- ecdf(c50_tune_results$sensitivity)
	pos_pred_val_percentile_dist <- ecdf(c50_tune_results$positive_predictive_value)

	c50_tune_results <- c50_tune_results %>%
		mutate(mix =(kappa_percentile_dist(kappa) * as.numeric(criteria_weights['kappa'])) +
			   		(accuracy_percentile_dist(accuracy) * as.numeric(criteria_weights['accuracy'])) +
					(sensitivity_percentile_dist(sensitivity) * as.numeric(criteria_weights['sensitivity'])) +
					(pos_pred_val_percentile_dist(positive_predictive_value) * as.numeric(criteria_weights['positive_predictive_value']))) %>%
		arrange(desc(mix))

	c50_ideal_params <- c50_tune_results %>%
		filter(num_branches >= min_branches, 
			   num_branches <= max_branches) %>%
		arrange(desc(mix))

	# c50_ideal_params <- c50_tune_results %>%
	# 	arrange(desc(accuracy))

	if(nrow(c50_ideal_params) == 0) {
		print(c50_tune_results)
		stop('No c50 params found within tree-size limits')
	}

	# in case there are multiple selected (e.g. same branch size in final selection)
	if(nrow(c50_ideal_params) > 1) {

	  c50_ideal_params <- c50_ideal_params[1, ] # choose first row if more than 1 row exists (or regardless)	
	}

	df_c50_tune_results_long <- c50_tune_results %>%
		dplyr::select(params, kappa, accuracy, sensitivity, positive_predictive_value, mix, num_branches) %>%
		gather(measurement, value, -params)
	df_c50_tune_results_long$measurement_type <- ifelse(df_c50_tune_results_long$measurement == 'mix',
														'mix',
														'measurement')
	df_c50_tune_results_long$measurement_type <- ifelse(df_c50_tune_results_long$measurement == 'num_branches',
														'tree_size',
														df_c50_tune_results_long$measurement_type)

	tune_parameters_plot <- ggplot(df_c50_tune_results_long, aes(x = value, y = factor(params, levels=rev(c50_tune_results$params)), col = measurement)) +
		geom_point(size = 2, alpha = 0.7) +
		geom_hline(yintercept=which(rev(c50_tune_results$params) == c50_ideal_params$params), color='red') +
		#geom_vline(data = dummy2, aes(xintercept = Z), color='red') +
		facet_grid(. ~ measurement_type, scales = 'free') +
		ggtitle(paste('c5.0', '- ideal params'))
	force(tune_parameters_plot) # force the evaluation here since we have all the parameters in memory

	c50_ideal_min_cases <- c50_ideal_params$min_cases
	c50_ideal_cost <- c50_ideal_params$cost

	c50_error_cost <- matrix(c(0, 1, c50_ideal_cost, 0), nrow = 2, dimnames = matrix_dimensions)
	c50_control <- C5.0Control(minCases = c50_ideal_min_cases)

	# now let's redo the model with the best parameters, on full training_data
	set.seed(seed)
	c50_model <- C5.0(	training_data %>% dplyr::select(-target_variable),
						training_data$target_variable,
						costs = c50_error_cost,
						control=c50_control)

	if(!is.null(tree_file_name)) {

		# https://stackoverflow.com/questions/31738045/in-r-error-in-is-data-framedata-object-not-found-c5-0-plot/37197335#37197335
		c50_model$call$x <- training_data %>% dplyr::select(-target_variable)
		c50_model$call$y <- training_data$target_variable

		png(filename = tree_file_name, width = 2000, height = 2000)
		plot(c50_model)
		dev.off()
	}

	c50_cost_used <- c50_model$costMatrix[target_negative_class, target_positive_class]

	c50_model_info <- list(	final_model = c50_model,
							tune_parameters_plot = tune_parameters_plot,
							selected_cost = c50_ideal_cost,
							seleted_min_cases = c50_ideal_min_cases)
	return(c50_model_info)
}
