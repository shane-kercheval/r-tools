library(fpc)
library(RColorBrewer)
library(reshape2)
library(tidyr)
library(ggplot2)


cluster_heatmap <- function(results_df, start_stop=1)
{
	heatmap_breaks <- c(-Inf, seq(from = (-1 * start_stop), to = start_stop, by = (start_stop/5)), Inf)
 	heatmap_colors <- c(rev(brewer.pal(n = 9,name = 'Blues')[2:7]), brewer.pal(n = 9,name = 'Reds')[2:7])

	results_df$cluster_name <- 1:nrow(results_df)
	results_df_long <- results_df %>% gather(variable, value, -cluster_name, - cluster_size)
	results_df_long$means <- cut(results_df_long$value, breaks = heatmap_breaks)

	heatmap <- ggplot(data=results_df_long, aes(x = factor(variable, levels = unique(variable)), y = factor(cluster_size, levels = sort(unique(cluster_size))))) +
		geom_tile(aes(fill = means), colour = "white") +
		theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
		scale_fill_manual(values = heatmap_colors, drop = FALSE) + # drop=FALSE so that scale shows every value, even if not in dataset
		labs(title = 'Segmentation Visualization',
			 subtitle = paste(max(results_df$cluster_name), 'Segments'),
			 y = 'Cluster Size (i.e. number of instances/samples)',
			 x = 'Variable',
			 caption = paste0(
			 "\nEach row represents a segment. Each column represents a variable to segment by.",
			 "\nEach variable is scaled/centered so that the mean is zero, and standard deviation is 1.",
			 "\nEach cell's color represents the average value found for that variable in a paricular segment.",
			 "\nTherefore, a deep blue color `(-Inf, -", start_stop,"]` represents a segment who's mean value for that variable",
			 "\n(i.e. the average value of all the samples that were classified into the paricular segment)",
			 "\nis outside ", start_stop," standard deviations (on the low side) compared with all values for the entire variable.",
			 "\nA deep red color `(", start_stop,", Inf]` means the average value is outside ", start_stop," standard deviations on the high-side."))

	return (heatmap)
}

save_kmeans_heatmaps <- function(kmeans_results, folder, subscript='', height = 7, width = 10, units = c('in'))
{
	if(subscript != '')
	{
		subscript <- sprintf('_%s', subscript)
	}

	temp <- lapply(kmeans_results, FUN=function(kmeans_result){
		results_df <- as.data.frame(kmeans_result$centers)
		results_df$cluster_size <- kmeans_result$size
		heatmap_plot <- cluster_heatmap(results_df = results_df)
		ggsave(	filename = sprintf("./%s/kmeans%s_%s_clusters_%s.png", folder, subscript, length(kmeans_result$size), Sys.Date()),
				plot = heatmap_plot,
				height = height,
				width = width,
				units = units)
	})
}

save_hierarchical_heatmaps <- function(hierarchical_results, folder='', subscript='', save_file=FALSE,
										height = 7, width = 10, units = c('in'))
{
	if(subscript != '')
	{
		subscript <- sprintf('_%s', subscript)
	}

	heatmaps <- lapply(hierarchical_results, FUN=function(hierarchical_result){
		
		results_df <- as.data.frame(lapply(hierarchical_result, colMeans))
		results_df <- as.data.frame(t(results_df)) # t (transpose) results in a matrix, convert back to df
		results_df$cluster_name <- 1:nrow(results_df)
		results_df$cluster_size <- map_dbl(hierarchical_result, ~ nrow(.))
		heatmap_plot <- cluster_heatmap(results_df = results_df)
		if(save_file)
		{
			ggsave(	filename = sprintf("./%s/hierarchical%s_%s_clusters_%s.png", folder, subscript, length(hierarchical_result), Sys.Date()),
					plot = heatmap_plot,
					height = height,
					width = width,
					units = units)
		}

		return(heatmap_plot)
	})

	return (heatmaps)
}

hierarchical_dendogram <-function(data_frame, named_column, ideal_cluster_size = NULL, path = './dendogram.png')
{
	cluster_data <- get_numeric_logical_dataset(data_frame, named_column)
	dataset_na_omited <- na.omit(cluster_data)
	dataset_scaled <- get_scaled_dataset(data_frame = dataset_na_omited, named_column = named_column)
	
	if(is.null(ideal_cluster_size))
	{
		clusters <- pamk(dataset_scaled)
		ideal_cluster_size <- clusters$nc
	}

	distances <- dist(dataset_scaled, method = "euclidean")
	clusters <- hclust(distances, method = "ward.D") 
	plot(clusters, cex = 0.5, cex.lab = 1, cex.axis = 1, cex.main = 1, cex.sub = 1) # display dendogram
	rect.hclust(clusters, k = ideal_cluster_size, border = "red")
	if(!is.null(path))
	{
		dev.copy(png,filename = path, width = 500, height = 500);
		dev.off();
	}
}