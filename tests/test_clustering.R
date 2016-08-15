library('testthat')
source('../clustering/clustering_methods.R', chdir=TRUE)
source('../clustering/cluster_visualization.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_clustering.R")

test_that("general: clustering", {
	
	worlddata = read.csv("./data/worlddata.csv")

	ideal_clusters = get_ideal_number_of_clusters(worlddata, named_column = 'country')
	expect_that(ideal_clusters, equals(5))
	
	wdata <- na.omit(worlddata)
	wdata <-  wdata[ , -which(names(wdata) %in% c("country"))]
	wdata_scaled <- data.matrix(wdata)
	wdata_scaled <- scale(wdata_scaled)
	set.seed(1)
	fit <- kmeans(wdata_scaled, ideal_clusters)
	bss_tss = fit$betweenss / fit$totss
	
	kmeans_results = kmeans_cluster_analysis(data_frame=worlddata, merge_column='country', seed_num=1)
	bss_tss_vector = kmeans_BSS_TSS(kmeans_results)
	expect_that(bss_tss_vector[4], equals(bss_tss))
	final_kmeans = kmeans_merge_cluster_data(kmeans_results=kmeans_results, original_data_frame=worlddata, merge_column='country')
	save_kmeans_heatmaps(kmeans_results, folder='.')
	kmeans_vector = c(2,3,4,5,6,7,8)
	temp = sapply(kmeans_vector, FUN=function(x){
		filename = sprintf('kmeans_%s_clusters_%s.png', x, Sys.Date())
		expect_true(file.exists(filename))
		file.remove(filename)
	})

	hierarchical_results = hierarchical_cluster_analysis(data_frame=worlddata, merge_column='country')
	final_hierarchical = hierarchical_merge_cluster_data(original_data_frame=worlddata, merge_column='country')
	save_hierarchical_heatmaps(hierarchical_results = hierarchical_results, folder='.')
	temp = sapply(kmeans_vector, FUN=function(x){
		filename = sprintf('hierarchical_%s_clusters_%s.png', x, Sys.Date())
		expect_true(file.exists(filename))
		file.remove(filename)
	})

	save_hierarchical_dendogram(data_frame=worlddata, named_column='country', num_clusters=5)
	expect_true(file.exists('./dendogram.png'))
	file.remove('./dendogram.png')
})
