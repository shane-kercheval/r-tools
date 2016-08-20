source('../general/modification.R', chdir=TRUE)
library(fpc)
library(NbClust)

#######################################################################################################################################
# takes a dataframe and runs hierarchical cluster analysis using all numeric and logical (TRUE/FALSE) columns
# runs a hierarchical analysis for cluster numbers [num_clusters - plus_minus, num_clusters + plus_minus] default: (5-3,5+3) == (2,8) 
#######################################################################################################################################
hierarchical_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
	cluster_data = get_numeric_logical_dataset(data_frame, merge_column)
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

#######################################################################################################################################
# takes hierarchical_results (list returned by `hierarchical_cluster_analysis` fucntion) and merges clusters with `original_data_frame`
# merge_column is the column that represents the unique row identifier
# NOTE: NA values are omitted, so be conscious about cleaning your data; for example, pre-clustering your data such (e.g. could split up data between paid v. non-paying customers if you have a lot of data with NAs for non-paying customers), or converting columns with NAs (such as a time column `time_to_paid`) to a TRUE/FASLE column (e.g. `is_paying`)
#######################################################################################################################################
hierarchical_merge_cluster_data <- function(original_data_frame, merge_column, num_clusters=5, plus_minus=3)
{
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

#######################################################################################################################################
# takes a dataframe and runs kmeans cluster analysis using all numeric and logical (TRUE/FALSE) columns
# runs a k-means analysis for cluster numbers [num_clusters - plus_minus, num_clusters + plus_minus] default: (5-3,5+3) == (2,8) 
# NOTE: NA values are omitted, so be conscious about cleaning your data; for example, pre-clustering your data such (e.g. could split up data between paid v. non-paying customers if you have a lot of data with NAs for non-paying customers), or converting columns with NAs (such as a time column `time_to_paid`) to a TRUE/FASLE column (e.g. `is_paying`)
#######################################################################################################################################
kmeans_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
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

#######################################################################################################################################
# takes kmeans_results (list returned by `kmeans_cluster_analysis` fucntion) and merges clusters with `original_data_frame`
# merge_column is the column that represents the unique row identifier
#######################################################################################################################################
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

#######################################################################################################################################
# returns a vector with the BSS/TSS ratio for each item in kmeans_results
# Ideally you want a clustering that has the properties of internal cohesion and external separation, i.e. the BSS/TSS ratio should approach 1. (http://stats.stackexchange.com/questions/82776/what-does-total-ss-and-between-ss-mean-in-k-means-clustering)
#######################################################################################################################################
kmeans_BSS_TSS <- function(kmeans_results)
{
	bss_tss_ratios = sapply(kmeans_results, FUN=function(x) {
		return(x$betweenss / x$totss)
	})
	return (bss_tss_ratios)
}

#######################################################################################################################################
# calculates the idea number of clusters
#######################################################################################################################################
get_ideal_number_of_clusters <- function(data_frame, named_column)
{
	data_frame = na.omit(data_frame)
	scaled_data = get_scaled_dataset(data_frame=data_frame, named_column=named_column)
	clusters = pamk(scaled_data)
	return (clusters$nc)
}

#######################################################################################################################################
# calculates the idea number of clusters
#######################################################################################################################################
get_ideal_number_of_clusters_nb <- function(data_frame, named_column)
{
	data_frame = na.omit(data_frame)
	scaled_data = get_scaled_dataset(data_frame=data_frame, named_column=named_column)
	return (NbClust(data = scaled_data, distance = "euclidean", min.nc = 2, max.nc = 20, method = "average", index = "all", alphaBeale = 0.1))
}
