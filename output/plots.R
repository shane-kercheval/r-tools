library(tidyr)

plot_wide_data <- function(df_wide, title='line plot', x_label='x', y_label='count')
{
	df_long = gather(df_wide, group, count, -x)
	return (ggplot(df_long, aes(x=x, y=count, group=group, colour=group) ) + 
			geom_line(size=1) + 
			ggtitle(title) +
			labs(x=x_label,y=y_label)+
			theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}
