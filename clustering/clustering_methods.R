source('../general/modification.R', chdir=TRUE)
library(fpc)
library(NbClust)
library('purrr')

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

get_ideal_number_of_clusters_nb <- function(data_frame, named_column)
{
	data_frame = na.omit(data_frame)
	scaled_data = get_scaled_dataset(data_frame=data_frame, named_column=named_column)
	return (NbClust(data = scaled_data, distance = "euclidean", min.nc = 2, max.nc = 20, method = "average", index = "all", alphaBeale = 0.1))
}
