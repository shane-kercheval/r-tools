library(tidyr)

line_plot_wide_data <- function(df_wide, title='line plot', x_label='x', y_label='count', x_factor_order=NULL, y_factor_order=NULL, stack=FALSE, save_file=NULL)
{
	df_long = gather_data(df_wide)
	return (line_plot_long_data(df_long=df_long, title=title, x_label=x_label, y_label=y_label, x_factor_order=x_factor_order, y_factor_order=y_factor_order, stack=stack, save_file=save_file))
}

line_plot_long_data <- function(df_long, title='line plot', x_label='x', y_label='count', x_factor_order=NULL, y_factor_order=NULL, stack=FALSE, save_file=NULL)
{
	df_long = convert_to_factors(df_long=df_long, x_factor_order=x_factor_order, y_factor_order=y_factor_order)
	line_plot = NULL
	if(stack)
	{
		line_plot = ggplot(df_long, aes(x = x, y = count, group=y)) + 
					geom_line(aes(color = y)) + 
					facet_grid(y ~ ., scales = "free_y") + 
					theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
	}
	else
	{
		line_plot = ggplot(df_long, aes(x=x, y=count, group=y, colour=y) ) + 
					geom_line(size=1, alpha=.75) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1))
	}

	line_plot = line_plot + ggtitle(title) + labs(x=x_label,y=y_label)

	if(!is.null(save_file))
	{
		ggsave(plot=line_plot, filename=save_file)
	}

	return (line_plot)
}

heat_map_wide_data <- function(df_wide, title='heat map', x_label='x', y_label='count', scale_label='scale', x_factor_order=NULL, y_factor_order=NULL, save_file=NULL)
{
	df_long = gather_data(df_wide)
	return (heat_map_long_data(df_long=df_long, title=title, x_label=x_label, y_label=y_label, scale_label=scale_label, x_factor_order=x_factor_order, y_factor_order=y_factor_order, save_file=save_file))
}

heat_map_long_data <- function(df_long, title='heat map', x_label='x', y_label='count', scale_label='scale', x_factor_order=NULL, y_factor_order=NULL, save_file=NULL)
{
	df_long = convert_to_factors(df_long=df_long, x_factor_order=x_factor_order, y_factor_order=y_factor_order)
	heat_map = 	ggplot(df_long, aes(x = x, y = y)) + 
				ggtitle(title) + 
				geom_tile(aes(fill = count)) +
				scale_fill_gradient(name=scale_label, low="white", high="red") +
				labs(x=x_label,y=y_label)

	if(!is.null(save_file))
	{
		ggsave(plot=heat_map, filename=save_file)
	}

	return (heat_map)
}

gather_data <- function(df_wide)
{
	df_long = gather(df_wide, y, count, -x)
	return (df_long)
}

convert_to_factors <- function(df_long, x_factor_order=NULL, y_factor_order=NULL)
{
	if(!is.null(x_factor_order))
	{
		df_long$x = factor(df_long$x, levels=x_factor_order, ordered=TRUE)
	}
	if(!is.null(y_factor_order))
	{
		df_long$y = factor(df_long$y, levels=y_factor_order, ordered=TRUE)
	}
	return (df_long)
}
