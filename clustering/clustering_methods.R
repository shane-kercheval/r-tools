# requires a numeric dataset with a column that has a unique identifier for rows (e.g. tenant id)
run_cluster_analysis <- function(dataset, num_clusters, merge_column)
{
	# summary(dataset)
	
	# dataset_na_omited = na.omit(dataset)
	# dataset_scaled = as.data.frame(lapply(dataset_na_omited[, -grep(merge_column, colnames(dataset_na_omited))], scale))
	# summary(dataset_scaled)
	
	# #cluster_info = pamk(dataset_scaled)
	# #n = cluster_info$nc
	# #library(NbClust)
	# #NbClust(data = dataset_scaled, distance = "euclidean", min.nc = 2, max.nc = 20, method = "average", index = "all", alphaBeale = 0.1)
	
	# ### Hierarchical clustering
	# distances = dist(dataset_scaled, method = "euclidean")
	# clusters = hclust(distances, method = "ward.D") 
	# plot(clusters)
	# rect.hclust(clusters, k = num_clusters, border = "red")
	# dev.copy(png,filename=sprintf('./results/cluster_analaysis_%s.png', Sys.time()), width=500, height=500);
	# dev.off();

	# clusterGroups = cutree(clusters, k = num_clusters)
	# cluster_splits = split(dataset_scaled, clusterGroups)
	# note('euclidean hclust means (z-score scaled)')
	# print(lapply(cluster_splits, nrow))
	# print(lapply(cluster_splits, function(x){ percent(nrow(x)/nrow(dataset_na_omited)) }))
	# print(lapply(cluster_splits, colMeans))
	# results_df = as.data.frame(lapply(cluster_splits, colMeans))
	# results_df = as.data.frame(t(results_df)) # t (transpose) results in a matrix, convert back to df
	# results_df$cluster_name = 1:nrow(results_df)
	# # refactor heatmat code to function so both clustering techniques can create heatmap and output to file
	
	
	# lapply(cluster_splits, FUN = function(x){
	#  	print(ifelse(colMeans(x) < 0.06 & colMeans(x) > -0.06, '=', ifelse(colMeans(x) < 0, strrep('-', abs(round(colMeans(x), 1)*10)), strrep('+', abs(round(colMeans(x), 1)*10))))) #0.6 because of the way round() works
	# })
	#lapply(cluster_splits, summary)
	



	#	note('kmeans:')
#	note('kmeans sizes:')
#	print(kmeans_results$size)
#	print(percent(kmeans_results$size/length(kmeans_results$cluster)))
#	note('kmeans centers (z-score scaled):')
#	print(kmeans_results$centers)
	results_df = as.data.frame(kmeans_results$centers)
	heatmap = cluster_heatmap(results_df)
	ggsave(filename=sprintf("kmeans_%s_clusters_%s.png", num_clusters, Sys.date()), plot=heatmap)

	dataset_na_omited$cluster = kmeans_results$cluster
	final = merge(data_frame, dataset_na_omited[c(merge_column, 'cluster')], by=c(merge_column), all.x=TRUE)
	print(describe(final))
}

#######################################################################################################################################
# takes a dataframe and runs hierarchical cluster analysis using all numeric and logical (TRUE/FALSE) columns
# runs a hierarchical analysis for cluster numbers [num_clusters - plus_minus, num_clusters + plus_minus] default: (5-3,5+3) == (2,8) 
#######################################################################################################################################
hierarchical_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
	numeric_logical_columns = sapply(data_frame, is.numeric | is.logical)
	data_numeric_logical = data_frame[ c(merge_column, numeric_logical_columns) ]

	dataset_na_omited = na.omit(data_numeric_logical)
	dataset_scaled = as.data.frame(lapply(dataset_na_omited[, -grep(merge_column, colnames(dataset_na_omited))], scale))
	#summary(dataset_scaled)

	set.seed(seed_num)

	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)

	hierarchical_results = lapply(clusters_to_analyze, FUN=function(x) {
		distances = dist(dataset_scaled, method = "euclidean")
		clusters = hclust(distances, method = "ward.D") 
	
		clusterGroups = cutree(clusters, k = num_clusters)
		cluster_splits = split(dataset_scaled, clusterGroups)
	
		return (cluster_splits)
	})
	return (hierarchical_results)
}

#######################################################################################################################################
# takes a dataframe and runs kmeans cluster analysis using all numeric and logical (TRUE/FALSE) columns
# runs a k-means analysis for cluster numbers [num_clusters - plus_minus, num_clusters + plus_minus] default: (5-3,5+3) == (2,8) 
#######################################################################################################################################
kmeans_cluster_analysis <- function(data_frame, merge_column, num_clusters=5, plus_minus=3, seed_num=123)
{
	numeric_logical_columns = sapply(data_frame, is.numeric | is.logical)
	data_numeric_logical = data_frame[ c(merge_column, numeric_logical_columns) ]

	dataset_na_omited = na.omit(data_numeric_logical)
	dataset_scaled = as.data.frame(lapply(dataset_na_omited[, -grep(merge_column, colnames(dataset_na_omited))], scale))
	#summary(dataset_scaled)

	set.seed(seed_num)

	clusters_to_analyze = seq(from=num_clusters-plus_minus, to=num_clusters+plus_minus, by=1)

	kmeans_results = lapply(clusters_to_analyze, FUN=function(x) {
		return (kmeans(data_numeric_logical, x))
	})
	return (kmeans_results)
}