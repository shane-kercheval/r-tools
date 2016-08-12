get_correlations <- function(data_frame, threshold)
{
	correlations = cor(data_frame, use='complete') # Tell the correlation to ignore the NAs with use="complete"
	# show correlations above threshold
	correlations[which(correlations < threshold & correlations > -threshold, arr.ind=TRUE)] <- NA 
	return (correlations)
}
