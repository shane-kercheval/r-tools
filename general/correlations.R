#######################################################################################################################################
# takes a data-frame, subsets the numeric columns, and returns a matrix of correlations with NAs above/below
#######################################################################################################################################
get_correlations <- function(data_frame, threshold)
{
	numeric_columns = sapply(data_frame, is.numeric)
	data_numeric = data_frame[numeric_columns]
	
	correlations = cor(data_numeric, use='complete') # Tell the correlation to ignore the NAs with use="complete"
	# show correlations above threshold
	correlations[which(correlations <= threshold & correlations >= -threshold, arr.ind=TRUE)] <- NA 
	return (correlations)
}
