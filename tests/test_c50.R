library('testthat')
source('../machine_learning_wrappers/c50.R', chdir=TRUE)
source('~/r-tools/general/model_measurements.R', chdir=TRUE)
library(dplyr)

#to run from command line, use:
#test_file("test_clustering.R")

test_that("clustering: methods", {
	
	# credit_data = read.csv("./data/credit.csv")
	# seed <- 123
	# set.seed(seed)
	# training_indexes <- createDataPartition(dataset$target_variable, p = training_perc, list = FALSE)
	# training_data <- dataset[ training_indexes,] %>% rename(target_variable = default)
	# testing_data  <- dataset[-training_indexes,] %>% rename(target_variable = default)
	# c5.0_cost_tuning(training_data = training_data,
	# 				target_positive_class = 'yes',
	# 				target_negative_class = 'no',
	# 				param_grid = expand.grid(	cost = c(0.1, 0.5, seq(from=1, to=3, by=1)),
	# 										  	min_cases = seq(20, 100, by=10)),
	# 				k_folds = 5,
	# 				min_branches = 4,
	# 				max_branches = 40,
	# 				seed = seed,
	# 				criteria_weights = c(	kappa = 2,
	# 									  	accuracy = 2,
	# 										sensitivity = 1,
	# 										positive_predictive_value = 1))

	# if(!is.null(c50_cost_used) && c50_cost_used == 1){
	# 	# cut and measure
	# 	model_roc <- roc(	classification_validation$recently_active,
	# 					test_probabilities_active,
	# 					levels = rev(c(target_positive_class, target_negative_class)))
	# 	cutoff_closest_topleft <- coords(model_roc, x = 'best', best.method = 'closest.topleft')
	# 	topleft_treshold <- cutoff_closest_topleft[['threshold']]

	# 	p_class <- ifelse(test_probabilities_active > topleft_treshold, 
	# 						target_positive_class,
	# 						target_negative_class) # from ROC curve
	# }else{
	# 	p_class <- predict(c50_model, newdata = classification_validation) 
	# }

	# prop.table(table(p_class))

	# C50_confusion_matrix <- table(actual = classification_validation$recently_active, predictions = p_class)
	# C50_confusion_ls <- confusion_list_from_confusion(C50_confusion_matrix)
	# visualize_quality_of_model(C50_confusion_ls)

	# confusionMatrix(data = p_class, classification_validation$recently_active, positive = target_positive_class)
})
