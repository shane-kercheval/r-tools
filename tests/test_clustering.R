library('testthat')
library(purrr)
source('../machine_learning_wrappers/clustering_methods.R', chdir=TRUE)

#to run from command line, use:
# test_file("test_clustering.R")

test_that("clustering: methods", {
	
	worlddata = read.csv("./data/worlddata.csv")
	
	#test error conditions
	worlddata_na = worlddata
	possibleError = tryCatch(hierarchical_cluster_analysis(data_frame=worlddata_na, merge_column='country')
							, error=function(e) e)
	expect_true(inherits(possibleError, 'error'))
	possibleError = tryCatch(kmeans_cluster_analysis(data_frame=worlddata_na, merge_column='country')
							 , error=function(e) e)
	expect_true(inherits(possibleError, 'error'))

	worlddata_no_na = na.omit(worlddata_na)
	possibleError = tryCatch(hierarchical_cluster_analysis(data_frame=worlddata_no_na, merge_column='country')
							 , error=function(e) e)
	expect_false(inherits(possibleError, 'error'))
	possibleError = tryCatch(kmeans_cluster_analysis(data_frame=worlddata_no_na, merge_column='country')
							 , error=function(e) e)
	expect_false(inherits(possibleError, 'error'))
	
	worlddata_dup = rbind(worlddata_no_na, worlddata_no_na[1,])
	possibleError = tryCatch(hierarchical_cluster_analysis(data_frame=worlddata_dup, merge_column='country')
							 , error=function(e) e)
	expect_true(inherits(possibleError, 'error'))
	possibleError = tryCatch(kmeans_cluster_analysis(data_frame=worlddata_dup, merge_column='country')
							 , error=function(e) e)
	expect_true(inherits(possibleError, 'error'))
	
	
	ideal_clusters = get_ideal_number_of_clusters(worlddata, named_column = 'country')
	expect_that(ideal_clusters, equals(5))
	
	wdata = na.omit(worlddata)
	wdata =  wdata[ , -which(names(wdata) %in% c("country"))]
	wdata_scaled = data.matrix(wdata)
	wdata_scaled = scale(wdata_scaled)
	set.seed(1)
	fit = kmeans(wdata_scaled, ideal_clusters)
	bss_tss = fit$betweenss / fit$totss
	
	kmeans_results = kmeans_cluster_analysis(data_frame=worlddata_no_na, merge_column='country', seed_num=1)
	bss_tss_vector = kmeans_BSS_TSS(kmeans_results)
	expect_that(bss_tss_vector[4], equals(bss_tss))
	final_kmeans = kmeans_merge_cluster_data(kmeans_results=kmeans_results, original_data_frame=worlddata_no_na, merge_column='country')
	final_kmeans2 = kmeans_merge_cluster_data2(kmeans_results=kmeans_results, any_dataframe=worlddata_no_na, merge_column='country', num_clusters = 5, plus_minus = 3)
	stopifnot(all(final_kmeans == final_kmeans2))

	expect_that(nrow(worlddata_no_na), equals(nrow(final_kmeans)))
	expect_false(any(is.na(final_kmeans)))
	
	save_kmeans_heatmaps(kmeans_results, folder='.')
	kmeans_vector = c(2,3,4,5,6,7,8)
	invisible(walk(kmeans_vector, ~{
		filename = sprintf('kmeans_%s_clusters_%s.png', ., Sys.Date())
		expect_true(file.exists(filename))
		if(. == 5) {
			file.copy(from = filename, to = '../readme/kmeans_5_clusters.png', overwrite = TRUE)
		}
		file.remove(filename)
	}))

	save_kmeans_heatmaps(kmeans_results, merged_data=final_kmeans2 %>% select(-country), folder='.', means_size = 2)
	kmeans_vector = c(2,3,4,5,6,7,8)
	invisible(walk(kmeans_vector, ~{
		filename = sprintf('kmeans_%s_clusters_%s.png', ., Sys.Date())
		expect_true(file.exists(filename))
		if(. == 5) {
			file.copy(from = filename, to = '../readme/kmeans_5_clusters_means.png', overwrite = TRUE)
		}
		file.remove(filename)
	}))

	hierarchical_results = hierarchical_cluster_analysis(data_frame=worlddata_no_na, merge_column='country')
	final_hierarchical = hierarchical_merge_cluster_data(original_data_frame=worlddata_no_na, merge_column='country')
	expect_that(nrow(worlddata_no_na), equals(nrow(final_hierarchical)))
	expect_false(any(is.na(final_hierarchical)))
	save_hierarchical_heatmaps(hierarchical_results = hierarchical_results, folder='.', save_file=TRUE)
	invisible(walk(kmeans_vector, ~{
		filename = sprintf('hierarchical_%s_clusters_%s.png', ., Sys.Date())
		expect_true(file.exists(filename))
		file.remove(filename)
	}))

	hierarchical_dendogram(data_frame=worlddata_no_na, named_column='country')
	expect_true(file.exists('./dendogram.png'))
	file.remove('./dendogram.png')
	
	hierarchical_dendogram(data_frame=worlddata_no_na, named_column='country', ideal_cluster_size = 10)
	expect_true(file.exists('./dendogram.png'))
	file.remove('./dendogram.png')

	mstd = hierarchical_get_clusters_mean_st_dev(hierarchical_results=hierarchical_results)
	expect_that(length(mstd), equals(7))
	expect_that(mstd, equals(c(1.0261817, 0.7128722, 0.6452611, 0.5238504, 0.5511163, 0.5354918, 0.5113339), tolerance=0.001))
	best_cluster_index = which(mstd == min(mstd))
	expect_that(best_cluster_index, equals(7))

	cluster_means = hierarchical_get_clusters_means(hierarchical_results=hierarchical_results)
	expect_that(length(cluster_means), equals(7))
	#save(cluster_means_original, file='./data/test_data_hierarchical_get_clusters_means.Rda')
	load('./data/test_data_hierarchical_get_clusters_means.Rda')
	expect_that(nrow(cluster_means), equals(nrow(cluster_means_original)))
	invisible(map2(cluster_means, cluster_means_original, ~ expect_that(.x, equals(.y))))

	total_expected_rows = nrow(final_hierarchical)
	expected_totals_rows = map(list(2, 3, 4, 5, 6, 7, 8), .f=function(y){ map_dbl(as.list(1:y), function(z){sum(final_hierarchical[,paste0('cluster_', y)]==z)})}) #~final_hierarchical[,paste0('cluster_', .)]))paste0('cluster_', y)
	cluster_rows = hierarchical_nrow(hierarchical_results=hierarchical_results)
	invisible(map2(expected_totals_rows, cluster_rows, function(x,y){expect_that(x, equals(y))}))
	invisible(map(cluster_rows, ~ expect_that(sum(.), equals(total_expected_rows))))
})


test_that("clustering: methods_correlation", {
  
	worlddata = read.csv("./data/worlddata.csv")
	worlddata_no_na = na.omit(worlddata)

	hierarchical_results = hierarchical_cluster_analysis(data_frame=worlddata_no_na, merge_column='country', use_correlation_distince = TRUE)
	final_hierarchical = hierarchical_merge_cluster_data(original_data_frame=worlddata_no_na, merge_column='country', use_correlation_distince = TRUE)
	expect_that(nrow(worlddata_no_na), equals(nrow(final_hierarchical)))
	expect_false(any(is.na(final_hierarchical)))
	save_hierarchical_heatmaps(hierarchical_results = hierarchical_results, folder='.', save_file=TRUE)
	invisible(walk(c(2,3,4,5,6,7,8), ~{
		filename = sprintf('hierarchical_%s_clusters_%s.png', ., Sys.Date())
		expect_true(file.exists(filename))
		file.remove(filename)
	}))

	hierarchical_dendogram(data_frame=worlddata_no_na, named_column='country')
	expect_true(file.exists('./dendogram.png'))
	file.remove('./dendogram.png')

	hierarchical_dendogram(data_frame=worlddata_no_na, named_column='country', ideal_cluster_size = 10)
	expect_true(file.exists('./dendogram.png'))
	file.remove('./dendogram.png')

	mstd = hierarchical_get_clusters_mean_st_dev(hierarchical_results=hierarchical_results)
	expect_that(length(mstd), equals(7))
	#expect_that(mstd, equals(c(1.0261817, 0.7128722, 0.6452611, 0.5238504, 0.5511163, 0.5354918, 0.5113339), tolerance=0.001))
	#best_cluster_index = which(mstd == min(mstd))
	#expect_that(best_cluster_index, equals(7))

	cluster_means = hierarchical_get_clusters_means(hierarchical_results=hierarchical_results)
	expect_that(length(cluster_means), equals(7))
	#save(cluster_means_original, file='./data/test_data_hierarchical_get_clusters_means_corr.Rda')
	load('./data/test_data_hierarchical_get_clusters_means_corr.Rda')
	expect_that(nrow(cluster_means), equals(nrow(cluster_means_original)))
	invisible(map2(cluster_means, cluster_means_original, ~ expect_that(.x, equals(.y))))

	total_expected_rows = nrow(final_hierarchical)
	expected_totals_rows = map(list(2, 3, 4, 5, 6, 7, 8), .f=function(y){ map_dbl(as.list(1:y), function(z){sum(final_hierarchical[,paste0('cluster_', y)]==z)})}) #~final_hierarchical[,paste0('cluster_', .)]))paste0('cluster_', y)
	cluster_rows = hierarchical_nrow(hierarchical_results=hierarchical_results)
	invisible(map2(expected_totals_rows, cluster_rows, function(x,y){expect_that(x, equals(y))}))
	invisible(map(cluster_rows, ~ expect_that(sum(.), equals(total_expected_rows))))
})

