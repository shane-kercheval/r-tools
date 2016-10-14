sensitivity <- function(true_positives, total_actual_positives)
{
	return(true_positives / total_actual_positives)
}
specificity <- function(true_negatives, total_actual_negatives)
{
	return(true_negatives / total_actual_negatives)
}
false_negative_rate <- function(false_negatives, total_actual_positives)
{
	return(false_negatives / total_actual_positives)
}
false_positive_rate <- function(false_positives, total_actual_negatives)
{
	return(false_positives / total_actual_negatives)
}
accuracy <- function(true_negatives, true_positives, total_observations)
{
	return((true_negatives + true_positives) / total_observations)
}
error_rate <- function(false_positives, false_negatives, total_observations)
{
	return((false_positives + false_negatives) / total_observations)
}
positive_predictive_value <- function(true_positives, false_positives)
{
	return(true_positives / (true_positives + false_positives))
}
negative_predictive_value <- function(true_negatives, false_negatives)
{
	return(true_negatives / (true_negatives + false_negatives))
}

# confusion matrix with predictions as columns and actuals as rows (negatives first, positives second)
quality_of_model_from_confusion <- function(confusion_matrix)
{
	true_positives = confusion_matrix[2, 2] # actual, predicted
	true_negatives = confusion_matrix[1, 1]
	false_positives = confusion_matrix[1, 2] # column first; predicted true, but actually was false
	false_negatives = confusion_matrix[2, 1] # column first; predicted false, but actually was true
	return(quality_of_model(true_positives=true_positives, true_negatives=true_negatives, false_positives=false_positives, false_negatives=false_negatives))
}

quality_of_model <- function(true_positives, true_negatives, false_positives, false_negatives)
{
	return(list(
		"accuracy" = accuracy(true_negatives=true_negatives, true_positives=true_positives, total_observations=true_positives+true_negatives+false_positives+false_negatives),
		"error_rate" = error_rate(false_positives=false_positives, false_negatives=false_negatives, total_observations=true_positives+true_negatives+false_positives+false_negatives),
		"positive_predictive_value" = positive_predictive_value(true_positives=true_positives, false_positives=false_positives),
		"negative_predictive_value" = negative_predictive_value(true_negatives=true_negatives, false_negatives=false_negatives),
		"false_positive_rate" = false_positive_rate(false_positives=false_positives, total_actual_negatives=true_negatives + false_positives),
		"false_negative_rate" = false_negative_rate(false_negatives=false_negatives, total_actual_positives=true_positives + false_negatives),
		"sensitivity" = sensitivity(true_positives=true_positives, total_actual_positives=true_positives + false_negatives),
		"specificity" = specificity(true_negatives=true_negatives, total_actual_negatives=true_negatives + false_positives),
		"total_observations" = true_positives + true_negatives + false_positives + false_negatives
		))
}

logistics_regression_coefficients <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	return(b + (b1*x1) + (b2*x2) + (b3*x3) + (b4*x4) + (b5*x5))
}
logistic_response_function <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	#e == exp(). This works because exp is the exponentiation function with base e. (http://stackoverflow.com/questions/9458536/r-programming-how-do-i-get-eulers-number)
	return( 1 / (1 + exp(-1* logistics_regression_coefficients(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5))))
}
odds <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	#e == exp(). This works because exp is the exponentiation function with base e. (http://stackoverflow.com/questions/9458536/r-programming-how-do-i-get-eulers-number)
	return(exp(logistics_regression_coefficients(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5)))
}
logit <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	return(log(odds(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5)))
}
