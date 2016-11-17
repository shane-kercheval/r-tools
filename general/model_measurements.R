confusion_list <- function(true_pos, true_neg, false_pos, false_neg)
{
	return (list(true_pos=true_pos, true_neg=true_neg, false_pos=false_pos, false_neg=false_neg, actual_pos = true_pos + false_neg, actual_neg = true_neg + false_pos, total=sum(true_pos, true_neg, false_pos, false_neg)))
}
confusion_list_from_confusion <- function(confusion_matrix)
{
	# |                  | Predicted Negative | Predicted Positive |
	# | ---------------- | ------------------ | ------------------ |
	# | Actual Negative  | True Negative      | False Positive     |
	# | Actual Positive  | False Negative     | True Positive      |
	true_pos = confusion_matrix[2, 2] # actual, predicted
	true_neg = confusion_matrix[1, 1]
	false_pos = confusion_matrix[1, 2] # column first; predicted true, but actually was false
	false_neg = confusion_matrix[2, 1] # column first; predicted false, but actually was true

	return (confusion_list(true_pos=true_pos, true_neg=true_neg, false_pos=false_pos, false_neg=false_neg))
}
sensitivity <- function(conf_list)
{
	return (conf_list$true_pos / conf_list$actual_pos)
}
specificity <- function(conf_list)
{
	return (conf_list$true_neg / conf_list$actual_neg)
}
false_negative_rate <- function(conf_list)
{
	return (conf_list$false_neg / conf_list$actual_pos)
}
false_positive_rate <- function(conf_list)
{
	return (conf_list$false_pos / conf_list$actual_neg)
}
accuracy <- function(conf_list)
{
	return ((conf_list$true_neg + conf_list$true_pos) / conf_list$total)
}
error_rate <- function(conf_list)
{
	return ((conf_list$false_pos + conf_list$false_neg) / conf_list$total)
}
positive_predictive_value <- function(conf_list)
{
	return (conf_list$true_pos / (conf_list$true_pos + conf_list$false_pos))
}
negative_predictive_value <- function(conf_list)
{
	return (conf_list$true_neg / (conf_list$true_neg + conf_list$false_neg))
}

prevalence <- function(conf_list)
{
	return ((conf_list$true_pos + conf_list$false_neg) / conf_list$total)
}

expected_value_confusion <- function(true_pos, true_neg, false_pos, false_neg, tp_cost_benefit, tn_cost_benefit, fp_cost_benefit, fn_cost_benefit)
{
	return (expected_value(	n_occur=c(true_pos, true_neg, false_pos, false_neg),
							benefits=c(tp_cost_benefit, tn_cost_benefit, fp_cost_benefit, fn_cost_benefit)))
}

quality_of_model <- function(conf_list)
{
	return (list(
		"accuracy" = accuracy(conf_list),
		"error_rate" = error_rate(conf_list),
		"positive_predictive_value" = positive_predictive_value(conf_list),
		"negative_predictive_value" = negative_predictive_value(conf_list),
		"false_positive_rate" = false_positive_rate(conf_list),
		"false_negative_rate" = false_negative_rate(conf_list),
		"sensitivity" = sensitivity(conf_list),
		"specificity" = specificity(conf_list),
		"prevalence" = prevalence(conf_list),
		'actual_pos_prob' = conf_list$actual_pos / conf_list$total,
		'actual_neg_prob' = conf_list$actual_neg / conf_list$total,
		"total_observations" = conf_list$total
		))
}

logistics_regression_coefficients <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	return (b + (b1*x1) + (b2*x2) + (b3*x3) + (b4*x4) + (b5*x5))
}
logistic_response_function <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	#e == exp(). This works because exp is the exponentiation function with base e. (http://stackoverflow.com/questions/9458536/r-programming-how-do-i-get-eulers-number)
	return ( 1 / (1 + exp(-1* logistics_regression_coefficients(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5))))
}
odds <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	#e == exp(). This works because exp is the exponentiation function with base e. (http://stackoverflow.com/questions/9458536/r-programming-how-do-i-get-eulers-number)
	return (exp(logistics_regression_coefficients(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5)))
}
logit <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
{
	return (log(odds(b=b, b1=b1, x1=x1, b2=b2, x2=x2, b3=b3, x3=x3, b4=b4, x4=x4, b5=b5, x5=x5)))
}
