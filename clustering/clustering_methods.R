source('../general/modification.R', chdir=TRUE)
library(fpc)
library(NbClust)
library('purrr')

hierarchical_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
	# make sure there are no duplicate rows for analysis
	if(any(duplicated(data_frame)))
	{
		stop('duplicated values found in dataframe')
	}
	if(any(is.na(data_frame)))
	{
		stop('na values found in dataframe, which will lead lead to bugs in cluster anlaysis')
	}
	cluster_data = get_numeric_logical_dataset(data_frame=data_frame, named_column=merge_column)
	dataset_na_omited = na.omit(cluster_data)
	dataset_scaled = get_scaled_dataset(data_frame=dataset_na_omited, named_column=merge_column)
	
	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)

	hierarchical_results = lapply(clusters_to_analyze, FUN=function(x) {
	
		distances = dist(dataset_scaled, method = "euclidean")
		clusters = hclust(distances, method = "ward.D")
	
		clusterGroups = cutree(clusters, k = x)
		cluster_splits = split(dataset_scaled, clusterGroups)
	
		return (cluster_splits)
	})
	return (hierarchical_results)
}

hierarchical_merge_cluster_data <- function(original_data_frame, merge_column, num_clusters=5, plus_minus=3)
{
		# make sure there are no duplicate rows for analysis
	if(any(duplicated(original_data_frame)))
	{
		stop('duplicated values found in dataframe')
	}
	if(any(is.na(original_data_frame)))
	{
		stop('na values found in dataframe, which will lead lead to bugs in cluster anlaysis')
	}
	cluster_data = get_numeric_logical_dataset(data_frame=original_data_frame, named_column=merge_column)
	dataset_na_omited = na.omit(cluster_data)
	dataset_scaled = get_scaled_dataset(data_frame=dataset_na_omited, named_column=merge_column)
	
	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)

	hierarchical_results = lapply(clusters_to_analyze, FUN=function(x) {
	
		distances = dist(dataset_scaled, method = "euclidean")
		clusters = hclust(distances, method = "ward.D") 
	
		clusterGroups = cutree(clusters, k = x)
		#cluster_splits = split(dataset_scaled, clusterGroups)
	
		return (clusterGroups)
	})

	cluster_data_frame = as.data.frame(sapply(hierarchical_results, FUN=function(x) {return (x)}))
	cluster_column_names = sapply(clusters_to_analyze, FUN=function(x) {return (sprintf('cluster_%s', x))})
	colnames(cluster_data_frame) = cluster_column_names
	dataset_na_omited = cbind(dataset_na_omited, cluster_data_frame)
	final = merge(original_data_frame, dataset_na_omited[c(merge_column, cluster_column_names)], by=c(merge_column), all.x=TRUE)

	return (final)
}

kmeans_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
	# make sure there are no duplicate rows for analysis
	if(any(duplicated(data_frame)))
	{
		stop('duplicated values found in dataframe')
	}
	if(any(is.na(data_frame)))
	{
		stop('na values found in dataframe, which will lead lead to bugs in cluster anlaysis')
	}
	cluster_data = get_numeric_logical_dataset(data_frame, merge_column)
	dataset_na_omited = na.omit(cluster_data)
	dataset_scaled = as.data.frame(lapply(dataset_na_omited[, -grep(merge_column, colnames(dataset_na_omited))], scale))
	
	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)
	
	kmeans_results = lapply(clusters_to_analyze, FUN=function(x) {
		set.seed(seed_num)
		cluster_results = kmeans(dataset_scaled, x)
		dataset_na_omited[sprintf('cluster_%s', x)] = cluster_results$cluster
		return (cluster_results)
	})

	return (kmeans_results)
}

kmeans_merge_cluster_data <- function(kmeans_results, original_data_frame, merge_column, num_clusters=5, plus_minus=3)
{
	cluster_data = get_numeric_logical_dataset(original_data_frame, merge_column)
	dataset_na_omited = na.omit(cluster_data)
	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)
	
	cluster_data_frame = as.data.frame(sapply(kmeans_results, FUN=function(x) {return (x$cluster)}))
	cluster_column_names = sapply(clusters_to_analyze, FUN=function(x) {return (sprintf('cluster_%s', x))})
	colnames(cluster_data_frame) = cluster_column_names
	dataset_na_omited = cbind(dataset_na_omited, cluster_data_frame)
	final = merge(original_data_frame, dataset_na_omited[c(merge_column, cluster_column_names)], by=c(merge_column), all.x=TRUE)

	return (final)
}

kmeans_BSS_TSS <- function(kmeans_results)
{
	bss_tss_ratios = sapply(kmeans_results, FUN=function(x) {
		return(x$betweenss / x$totss)
	})
	return (bss_tss_ratios)
}

get_ideal_number_of_clusters <- function(data_frame, named_column)
{
	data_frame = na.omit(data_frame)
	scaled_data = get_scaled_dataset(data_frame=data_frame, named_column=named_column)
	clusters = pamk(scaled_data)
	return (clusters$nc)
}

hierarchical_get_clusters_mean_st_dev <- function(hierarchical_results)
{
	# each top level list in hierarchical_results is 1 cluster analysis (e.g. [[1]] is analysis with 2 clusters, [[2]] is analysis with 3 clusters, etc.)
	# next level is particular cluster for cluster analysis, so [[1]][[1]] might be the first cluster (cluster 1) of the analysis done with 2 clusters. Each row in [[1]][[1]] will represent a row of data in the dataset (standarded with z-scores) that has been assigned to that particular cluster.
	cluster_mean_standard_deviations = hierarchical_results %>% map(~ map(., ~ map_dbl(., sd))) %>% # This will measure the variance (standard deviation) for each column's data for each cluster for each cluster analysis. So [[1]] represents the first cluster analysis and [[1]][[1]] represents the first cluster of the first cluster analysis. It will have the columns from the dataset with the standard deviation among all the data in that cluster for each column
			map(~ transpose(.)) %>% # transposes the data such that for each cluster analysis, there is a list of columns (from the original data) and for each column, a number (standard deviation) for each cluster
			map(~map(., ~unlist(.))) %>% # this shows (for each cluster anlaysis), the standard deviation for each column (e.g. signups) for each cluster number
			map(~map_dbl(.,~mean(.))) %>% # now (for each cluster anlaysis (i.e. [[i]])) we want to get the mean of the standard deviations for each column across all clusters. The lower the mean, the less variation there is for that column, among the clusters
			map_dbl(~mean(.)) # this final iteration gives us the mean of all the means from the columns for each cluster analysis.
			# note: you can't tell this from the heatmap, because the heatmap just shows the mean for each cluster/column, you can't see the standard deviation for each cluster/column

	#then you would use `which(cluster_mean_standard_deviations == min(cluster_mean_standard_deviations))` to get the cluster with the least variability within each cluster
	return (cluster_mean_standard_deviations)
}

hierarchical_get_clusters_means <- function(hierarchical_results)
{
	# each top level list in hierarchical_results is 1 cluster analysis (e.g. [[1]] is analysis with 2 clusters, [[2]] is analysis with 3 clusters, etc.)
	# next level is particular cluster for cluster analysis, so [[1]][[1]] might be the first cluster (cluster 1) of the analysis done with 2 clusters. Each row in [[1]][[1]] will represent a row of data in the dataset (standarded with z-scores) that has been assigned to that particular cluster.
	cluster_means = hierarchical_results %>% map(~ map(., ~ map_dbl(., mean))) # This will measure the variance (standard deviation) for each column's data for each cluster for each cluster analysis. So [[1]] represents the first cluster analysis and [[1]][[1]] represents the first cluster of the first cluster analysis. It will have the columns from the dataset with the standard deviation among all the data in that cluster for each column
	return (cluster_means)
}

hierarchical_nrow <- function(hierarchical_results)
{
	rows = hierarchical_results %>% map(~ map(., ~ map_dbl(., length))) %>%
		map(~ unname(unlist(map_dbl(.,  ~.[[1]]))))
	return (rows)
}

get_ideal_number_of_clusters_nb <- function(data_frame, named_column)
{
	data_frame = na.omit(data_frame)
	scaled_data = get_scaled_dataset(data_frame=data_frame, named_column=named_column)
	return (NbClust(data = scaled_data, distance = "euclidean", min.nc = 2, max.nc = 20, method = "average", index = "all", alphaBeale = 0.1))
}
