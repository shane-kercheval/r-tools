library(Hmisc)

get_correlations <- function(data_frame, corr_threshold=0.7, p_value_threshold=0.1, type='pearson')
{
	numeric_columns = map_lgl(data_frame, is.numeric)
	data_numeric = data_frame[numeric_columns]
	df_complete = data_numeric[complete.cases(data_numeric), ]
	
	rcorr_results = rcorr(as.matrix(df_complete), type=type)
	correlations = rcorr_results$r
	p_values = rcorr_results$P 
	
	correlations[which(correlations <= corr_threshold & correlations >= -corr_threshold, arr.ind=TRUE)] = NA # set correlations that are 'lower' than corr_threshold to NA
	correlations[which(p_values > p_value_threshold, arr.ind=TRUE)] = NA # set correlations that have p_value > `p_value_threshold` to NA (i.e. we only want values who have low p_value (i.e. statistically significant), lower than pvalue threshold)
	
	return (as.matrix(round(correlations, 2)))
}
