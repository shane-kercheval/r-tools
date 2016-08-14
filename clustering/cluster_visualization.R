#######################################################################################################################################
# takes a dataframe with each cluster as row and all cluster variables as columns, cells representing cluster/variable means (or medians,etc.)
# start_stop is absolute number of min/max scale value
#######################################################################################################################################
cluster_heatmap <- function(results_df, start_stop=1)
{
	heatmap_breaks = c(-Inf, seq(from=(-1 * start_stop), to=start_stop, by=(start_stop/5)), Inf)
 	heatmap_colors = c(rev(brewer.pal(n=9,name='Blues')[2:7]), brewer.pal(n=9,name='Reds')[2:7])

	results_df$cluster_name = 1:nrow(results_df)
	results_df_melted = melt(results_df, id.vars='cluster_name', measure.vars=colnames(results_df[-c(ncol(results_df))]))
	results_df_melted$means = cut(results_df_melted$value, breaks = heatmap_breaks)
	
	heatmap = ggplot(data=results_df_melted, aes(x = variable, y = cluster_name)) + 
	  geom_tile(aes(fill = means), colour = "white") +
	  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
	  scale_fill_manual(values = heatmap_colors, drop = FALSE) # drop=FALSE so that scale shows every value, even if not in dataset

	return (heatmap)
}
