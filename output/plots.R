library(dplyr)

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

tree_diagram <- function (p_a, p_b_given_a, p_b_given_not_a, file_name = NULL)
{
	# code from http://www.harrysurden.com/wordpress/archives/292
	# R Conditional Probability Tree Diagram

	# The Rgraphviz graphing package must be installed to do this
	#install
		#source("http://bioconductor.org/biocLite.R")
		#biocLite("Rgraphviz")
	suppressWarnings(require("Rgraphviz"))

	# # Probability of a
	# p_a = 0.01
	# # Probability (b | a)
	# p_b_given_a = 0.99
	# # Probability (b | ¬a)
	# p_b_given_not_a = 0.10

	# Calculate the rest of the values based upon the 3 variables above
	p_not_b_given_a = 1 - p_b_given_a
	p_not_a = 1 - p_a
	p_not_b_given_not_a = 1 - p_b_given_not_a

	#Joint Probabilities of a and B, a and not_b, p_not_a and b, p_not_a and not_b
	p_a_and_b = p_a * p_b_given_a
	p_a_and_not_b = p_a * p_not_b_given_a
	p_not_a_and_b = p_not_a * p_b_given_not_a
	p_not_a_and_not_b = p_not_a * p_not_b_given_not_a

	# Probability of B
	p_b = p_a_and_b + p_not_a_and_b
	p_not_b = 1 - p_b

	# Bayes theorum - probabiliyt of A | B
	# (a | b) = Prob (a AND b) / prob (b)
	p_a_given_b = p_a_and_b / p_b

	# These are the labels of the nodes on the graph
	# To signify "Not A" - we use A' or A prime

	node1 = "P"
	node2 = "A"
	node3 = "A'"
	node4 = "A&B"
	node5 = "A&B'"
	node6 = "A'&B"
	node7 = "A'&B'"
	nodeNames=c(node1,node2,node3,node4, node5,node6, node7)

	rEG = new("graphNEL", nodes=nodeNames, edgemode="directed")

	# Draw the "lines" or "branches" of the probability Tree
	rEG = addEdge(nodeNames[1], nodeNames[2], rEG, 1)
	rEG = addEdge(nodeNames[1], nodeNames[3], rEG, 1)
	rEG = addEdge(nodeNames[2], nodeNames[4], rEG, 1)
	rEG = addEdge(nodeNames[2], nodeNames[5], rEG, 1)
	rEG = addEdge(nodeNames[3], nodeNames[6], rEG, 1)
	rEG = addEdge(nodeNames[3], nodeNames[7], rEG, 10)

	eAttrs = list()

	q = edgeNames(rEG)

	# Add the probability values to the the branch lines

	eAttrs$label = c(toString(round(p_a, 4)),toString(round(p_not_a, 4)), toString(round(p_b_given_a, 4)), toString(round(p_not_b_given_a, 4)), toString(round(p_b_given_not_a, 4)), toString(round(p_not_b_given_not_a, 4)))
	names(eAttrs$label) = c(q[1],q[2], q[3], q[4], q[5], q[6])
	edgeAttrs=eAttrs

	# Set the color, etc, of the tree
	attributes=list(node=list(label="foo", fillcolor="lightgreen", fontsize="15"), edge=list(color="red"),graph=list(rankdir="LR"))

	result = tryCatch({ dev.off()}, error = function(e) {})

	#Plot the probability tree using Rgraphvis
	if(!is.null(file_name))
	{
		png(file_name, width = 500, height = 500)
	}

	plot(rEG, edgeAttrs=eAttrs, attrs=attributes)
	nodes(rEG)
	edges(rEG)

	#Add the probability values to the leaves of A&B, A&B', A'&B, A'&B'
	text(500, 420, round(p_a_and_b, 4), cex=.8)
	text(500, 280, round(p_a_and_not_b, 4), cex=.8)
	text(500, 160, round(p_not_a_and_b, 4), cex=.8)
	text(500, 30, round(p_not_a_and_not_b, 4), cex=.8)
	text(340, 440,"(B | A)", cex=.8)
	text(340, 230,"(B | A')", cex=.8)

	#Write a table in the lower left of the probablites of A and B
	text(80, 50, paste("P(A):", round(p_a, 4)), cex=.9, col="darkgreen")
	text(80, 20, paste("P(A'):", round(p_not_a, 4)), cex=.9, col="darkgreen")

	text(160, 50, paste("P(B):", round(p_b, digits=4)), cex=.9)
	text(160, 20, paste("P(B'):", round(p_not_b, digits=4)), cex=.9)
	text(80, 420, paste("P(A|B): ", round(p_a_given_b, digits=4)), cex=.9, col="blue")

	if(!is.null(file_name))
	{
		result = tryCatch({ dev.off()}, error = function(e) {})
	}
}

gg_qq_plot <- function(data_vector)
{
	# QQ Plot (examine fit of normal distribution)
	slope = diff(quantile(data_vector, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
	int = quantile(data_vector, 0.25) - slope * qnorm(0.25)
	return (ggplot(NULL, aes(sample=data_vector)) +
				geom_qq(alpha=0.3) +
				geom_abline(aes(slope=slope, intercept = int), col='red'))
}
