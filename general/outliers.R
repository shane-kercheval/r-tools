calculate_outlier_thresholds <- function(vect)
{
	quantile_1 = quantile(vect, na.rm = TRUE)[2]# gets the '1st' quartile, i.e. 25th percentile
	quantile_3 = quantile(vect, na.rm = TRUE)[4] # gets the '3rd' quartile, i.e. 75th percentile
	threshold = IQR(vect, na.rm = TRUE) * 1.5
	lower_threshold = quantile_1 - threshold
	upper_threshold = quantile_3 + threshold

	thresholds = c(lower_threshold, upper_threshold)
	names(thresholds) = c('lower', 'upper')
	return (thresholds)
}

remove_outliers <- function(vect)
{
	outlier_thresholds = calculate_outlier_thresholds(vect)
	no_outliers = vect
	no_outliers[no_outliers < outlier_thresholds[1]] <- NA # set NA to anything lower than lower threshold
	no_outliers[no_outliers > outlier_thresholds[2]] <- NA # set NA to anything higher than upper threshold
	return (no_outliers)
}

which_outliers <- function(vect)
{
	outlier_thresholds = calculate_outlier_thresholds(vect)
	indexes_above = which(vect > outlier_thresholds['upper'])
	indexes_below = which(vect < outlier_thresholds['lower'])
	return (c(indexes_below, indexes_above))
}
