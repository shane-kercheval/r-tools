source('../probability/decision_analysis.R', chdir=TRUE)
library(dplyr)
library(tidyr)
library(reshape2)

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
recall <- function(conf_list)
{
	return (conf_list$true_pos / (conf_list$true_pos + conf_list$false_neg))
}
prevalence <- function(conf_list)
{
	return ((conf_list$true_pos + conf_list$false_neg) / conf_list$total)
}
kappa <- function(conf_list)
{
	# proportion of the actual agreements
	pr_a = (conf_list$true_neg +  conf_list$true_pos) / conf_list$total # add the proportion of all instances where the predicted type and actual type agree
	# probability of both predicted and actual being negative
	p_negative_prediction_and_actual = ((conf_list$true_neg + conf_list$false_pos) / conf_list$total) * ((conf_list$true_neg + conf_list$false_neg) / conf_list$total)
	# probability of both predicted and actual being positive
	p_positive_prediction_and_actual = prevalence(conf_list) * ((conf_list$false_pos + conf_list$true_pos) / conf_list$total)
	# probability that chance alone would lead the predicted and actual values to match, under the assumptiont that both are selected randomly (i.e. implies independence) according to the observed proportions (probility of independent events = P(A & B) == P(A) * P(B)
	pr_e = p_negative_prediction_and_actual + p_positive_prediction_and_actual
	return ((pr_a - pr_e) / (1 - pr_e))
}

expected_value_confusion <- function(true_pos, true_neg, false_pos, false_neg, tp_cost_benefit, tn_cost_benefit, fp_cost_benefit, fn_cost_benefit)
{
	return (expected_value(	n_occur=c(true_pos, true_neg, false_pos, false_neg),
							benefits=c(tp_cost_benefit, tn_cost_benefit, fp_cost_benefit, fn_cost_benefit)))
}

expected_value_confusion_matrix <- function(confusion_matrix, gain_cost_matrix)
{
	total_observations <- sum(confusion_matrix)
	confusion_matrix_percentages <- confusion_matrix / total_observations
	expected_value_matrix <- confusion_matrix_percentages * gain_cost_matrix
	expected_profit <- sum(expected_value_matrix)

	return (expected_profit)
}

expected_value_with_priors_confusion_matrix <- function(confusion_matrix, gain_cost_matrix, class_prior_positive_rate)
{
	conf_list <- confusion_list_from_confusion(confusion_matrix)

	sens <- sensitivity(conf_list)
	spec <- specificity(conf_list)
	fp_rate <- false_positive_rate(conf_list)
	fn_rate <- false_negative_rate(conf_list)

	expected_profit_with_priors <- (class_prior_positive_rate * ((sens * gain_true_positive) + (fn_rate * gain_false_negative))) + 
									((1 - class_prior_positive_rate) * ((spec * gain_true_negative) + (fp_rate * gain_false_positive)))
	return (expected_profit_with_priors)
}


quality_of_model <- function(conf_list)
{
	return (data.frame(
		'kappa' = kappa(conf_list),
		'accuracy' = accuracy(conf_list),
		'error_rate' = error_rate(conf_list),
		'sensitivity' = sensitivity(conf_list),
		'specificity' = specificity(conf_list),
		'false_positive_rate' = false_positive_rate(conf_list),
		'false_negative_rate' = false_negative_rate(conf_list),
		'positive_predictive_value' = positive_predictive_value(conf_list),
		'negative_predictive_value' = negative_predictive_value(conf_list),
		'recall' = recall(conf_list),
		'prevalence' = prevalence(conf_list),
		'actual_pos_prob' = conf_list$actual_pos / conf_list$total,
		'actual_neg_prob' = conf_list$actual_neg / conf_list$total,
		'total_observations' = conf_list$total
		))
}

visualize_quality_of_model <- function(conf_list)
{
	model_quality = quality_of_model(conf_list)

	df_quality = melt(model_quality %>% dplyr::select(-recall, -actual_pos_prob, -total_observations), id.vars=NULL)
	df_quality$value = round(df_quality$value, 3)
	df_quality$category = factor(NA,levels=c('data','overall','pos correct', 'neg correct', 'incorrect'),ordered=TRUE)

	df_quality$category[df_quality$variable == 'prevalence'] = 'data'
	df_quality$category[df_quality$variable == 'actual_neg_prob'] = 'data'

	df_quality$category[df_quality$variable == 'kappa'] = 'overall'
	df_quality$category[df_quality$variable == 'accuracy'] = 'overall'
	df_quality$category[df_quality$variable == 'error_rate'] = 'overall'

	df_quality$category[df_quality$variable == 'sensitivity'] = 'pos correct'
	df_quality$category[df_quality$variable == 'positive_predictive_value'] = 'pos correct'

	df_quality$category[df_quality$variable == 'specificity'] = 'neg correct'
	df_quality$category[df_quality$variable == 'negative_predictive_value'] = 'neg correct'

	df_quality$category[df_quality$variable == 'false_positive_rate'] = 'incorrect'
	df_quality$category[df_quality$variable == 'false_negative_rate'] = 'incorrect'

	p = ggplot(data = df_quality, aes(x=variable, y=value)) + geom_boxplot() + coord_cartesian(ylim = c(0, 1)) +
			facet_grid( ~ category, scales='free') +
			geom_text(aes(label = value), size = 3, vjust=-1) +
			theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

	return (p)
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

# if predicted_probabilities is not passed in, then actual_observations needs to be ordered by the corresponding predicted probabilities in desc order
gain_lift_table <- function(actual_observations, predicted_probabilities = NULL, number_of_bins = 10, target_positive_class = 'positive')
{
	total_observations <- length(actual_observations)
	total_events <- sum(actual_observations == target_positive_class)

	df_gain_lift <- data.frame(actual_observations = actual_observations)
	if(!is.null(predicted_probabilities))
	{
		df_gain_lift <- df_gain_lift %>%
			mutate(predicted_probabilities = predicted_probabilities) %>%
			arrange(desc(predicted_probabilities))
	}

	# it is assumed at this point the data is arranged descended by the corresponiding predicted probabilities
	df_gain_lift <- df_gain_lift %>%
		mutate(bin_number = floor((row_number() - 1) / ceiling(total_observations/number_of_bins) + 1))

	final_gain_lift_table <- df_gain_lift %>%
		group_by(bin_number) %>%
		summarise(number_of_observations = n(),
				  number_of_events = sum(actual_observations == target_positive_class),
				  percentage_of_events = number_of_events / total_events) %>%
		mutate(cumulative_observations = cumsum(number_of_observations),
			   cumulative_events = cumsum(number_of_events),
			   gain_score = percentage_of_events * 100,
			   gain = cumsum(gain_score),
			   lift = (gain / 100) / (cumulative_observations / total_observations)) %>%
		dplyr::select(bin_number, number_of_observations, number_of_events, cumulative_events, percentage_of_events, gain, lift)

	return (final_gain_lift_table)
}

# assumes number_of_bins is 10
gain_lift_charts <- function(gl_table, round_by = 2)
{
	if(nrow(gl_table) != 10)
	{
		stop('`gain_lift_charts` only designed for 10 bins')
	}
	
	axis_sequence <- seq(from = 0, to = 100, by = 10)
	gain_20th_percentile <- round(gl_table[2, ]$gain, round_by)
	lift_20th_percentile <- round(gl_table[2, ]$lift, round_by)
	
	prevalence <- sum(gl_table$number_of_events) / sum(gl_table$number_of_observations)
	ideal_gain_line <- map_dbl(seq(from = 0.10, to =1.0, by = 0.10), ~ {
		# how far are we from 0 to the prevalence?
		return (min(. / prevalence, 1) * 100)
	})
	
	zero_row = data.frame(bin_number = 0, number_of_observations = 0, number_of_events = 0, cumulative_events = 0, percentage_of_events = 0, gain = 0, lift = 0)
	gain_data <- rbind(zero_row, gl_table)
	gain_data_long <- gather(gain_data %>% 
							dplyr::select(bin_number, gain) %>% 
							dplyr::mutate(bin_number = round(bin_number * 10), gain_random = axis_sequence, gain_perfect = c(0, ideal_gain_line)) %>%
							dplyr::rename(percentile = bin_number) %>%
							dplyr::rename(gain_model = gain),
						key = gain_type, percent_of_events, -percentile)
	gain_data_long <- rbind(data.frame(percentile = prevalence * 100, gain_type = 'gain_perfect', percent_of_events = 100), gain_data_long)
	gain_data_long$gain_type = factor(gain_data_long$gain_type, levels = rev(unique(gain_data_long$gain_type)))
	gain_chart <- ggplot(data = gain_data_long, mapping = aes(x = percentile, y = percent_of_events, col = gain_type)) + 
		geom_line() +
		geom_point() +
		geom_text(aes(label=ifelse(percent_of_events > 1 & gain_type != 'gain_perfect', as.character(round(percent_of_events, round_by)), '')), hjust = 0.5, vjust = -1) +
		scale_x_continuous(breaks = axis_sequence) +
		scale_y_continuous(breaks = axis_sequence) +
		ggtitle('Gain Chart') + ylab('% of Actual Events in Percentile') + xlab('Percentile (lower percentiles contain higher predicted probabilities)') +
		labs(caption = paste0("\ne.g., ", gain_20th_percentile, "% of the events are covered in the top 20% of data based on the model. \nIn the case of propensity to buy, we can say we can\nidentify and target ", gain_20th_percentile, "% of customers who are\nlikely to buy the product by just sending email to\n20% of total customers."))

	lift_data_long <- gather(gl_table %>% 
							dplyr::select(bin_number, lift) %>% 
							dplyr::mutate(bin_number = round(bin_number * 10), lift_random = rep(1, 10)) %>% 
							dplyr::rename(percentile = bin_number) %>%
							dplyr::rename(lift_model = lift),
						key = lift_type, lift, -percentile)
	lift_data_long$lift_type = factor(lift_data_long$lift_type, levels = rev(unique(lift_data_long$lift_type)))
	lift_chart <- ggplot(data = lift_data_long, mapping = aes(x = percentile, y = lift, col = lift_type)) + 
		geom_line() +
		geom_point() +
		geom_text(aes(label=ifelse(lift > 1, as.character(round(lift, round_by)), '')), hjust = 0, vjust = -0.5) +
		scale_x_continuous(breaks = axis_sequence) +
		ggtitle('Lift Chart') + ylab('Lift') + xlab('Percentile (lower percentiles contain higher predicted probabilities)') +
		labs(caption = paste("(e.g.) The Cumulative Lift of", lift_20th_percentile, "for top 20% of model predictions, means that when\nselecting 20% of the records based on the model, one can expect", lift_20th_percentile, "times the\ntotal number of targets (events) found than by randomly selecting 20% without a model."))

	return (list(gain_chart, lift_chart))
}

calibration_table <- function(actual_observations, predicted_probabilities, target_positive_class = 'positive')
{
	df_calibration <- data.frame(actual_observations = actual_observations, predicted_probabilities = predicted_probabilities) %>%
		dplyr::mutate(bins = cut(predicted_probabilities, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE)) %>%
		dplyr::arrange(desc(predicted_probabilities))

	cal_table <- df_calibration %>%
		dplyr::group_by(bins) %>%
		dplyr::summarise(   number_of_observations = n(),
							number_of_events = sum(actual_observations == target_positive_class),
							percent_events_in_percentile = number_of_events / number_of_observations)

	# make sure each percentile exists, levels will give all breaks, even if non-existed in actual data
	bin_levels <- levels(cal_table$bins)
	walk(bin_levels, ~ {
		level <- .
		if(!(level %in% cal_table$bins))
		{
			cal_table <<- rbind(cal_table, data.frame(  bins = factor(level, levels = levels(cal_table$bins), ordered = TRUE),
														number_of_observations = 0,
														number_of_events = 0,
														percent_events_in_percentile = 0))	
		}
	})
	cal_table <- cal_table %>% arrange(bins)
	return(cal_table)
}

calibration_chart <- function(cal_table, round_by = 2)
{
	require(scales)
	second_bin_observations <- cal_table[2, ]$number_of_observations
	second_bin_events <- cal_table[2, ]$number_of_events

	calibration_midpoints <- seq(from = 0.05, to = 0.95, by = 0.1)
	cal_table_long <- cal_table %>%
		dplyr::mutate(ideal_calibration = calibration_midpoints) %>%
		dplyr::rename(model_calibration = percent_events_in_percentile) %>%
		gather(calibration_type, calibration_value, -bins, -number_of_observations, -number_of_events) %>%
		dplyr::mutate(calibration_type = factor(calibration_type))

	cal_chart <- ggplot(data = cal_table_long, aes(x = bins, y = calibration_value, col = calibration_type, group = calibration_type)) +
		geom_line(size = 0.8, alpha = 0.7) +
		geom_point() +
		scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
		geom_text(aes(label=ifelse(calibration_type == 'model_calibration', paste0(number_of_events, ' / ', number_of_observations), '')), hjust = 0.5, vjust = -0.7) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		ggtitle('Calibration Chart') + ylab('Percent of Events Found in Corresponding Bin') + xlab('Predited Probabilities') +
		labs(caption = paste("\nShows the % of actual events found for each group of binned predicted probabilities.\nFor example, in the second bin (predicted probabilities range from > 0.1 to <= 0.2),\n out of the", second_bin_observations, "observations in that bin,", second_bin_events, "`actual` events are found.\nIf the points fall along the 45 degree line, the model has produced well-calibrated\nprobabilities."))

	return (cal_chart)
}

# same value as expected-value
expected_value_conditional <- function(o_tp, o_tn, o_fp, o_fn, b_tp, b_tn, b_fp, b_fn)
{
	conf_list = confusion_list(true_pos=o_tp, true_neg=o_tn, false_pos=o_fp, false_neg=o_fn)
	qom = quality_of_model(conf_list)

	# this equation corresponds to equation in Data Science for Business (Provost, Fawcett, kindle loc 4424)
	# sensitivity == TRUE POSITIVE RATE
	# specificity == TRUE NEGATIVE RATE
	return ((qom$actual_pos_prob * (qom$sensitivity * b_tp + qom$false_negative_rate * b_fn)) +
			(qom$actual_neg_prob * (qom$specificity * b_tn + qom$false_positive_rate * b_fp)))

}

