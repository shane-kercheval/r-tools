library('testthat')
source('../output/plots.R', chdir=TRUE)
source('../probability/bayes.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file('test_plots.R')

test_that('output: plot: line_plot_wide_data', {
	# NOTE THIS TESTS line_plot_wide_data & line_plot_long_data
	set.seed(1)
	normal_distribution = rnorm(n=20)
	df_wide = data.frame(x=1:20, rnorm=normal_distribution, flipped=normal_distribution * -1, zero=rep(0,20))

	file_name = '../output/example_line_plot_wide_data.png'
	file.remove(file_name) # remove file to ensure no file is created for first test
	line_plot = line_plot_wide_data(df_wide)
	expect_false(is.null(line_plot))
	expect_false(file.exists(file_name))

	line_plot = line_plot_wide_data(df_wide, save_file=file_name)
	expect_false(is.null(line_plot))
	expect_true(file.exists(file_name))

	file_name = '../output/example_line_plot_wide_data_stack.png'
	file.remove(file_name) # remove file to ensure no file is created for first test
	line_plot = line_plot_wide_data(df_wide, y_factor_order=colnames(df_wide)[2:4], stack=TRUE, save_file=file_name)
	expect_false(is.null(line_plot))
	expect_true(file.exists(file_name))

	# BUG: was getting geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
	# this appears to be caused when x is a character, fix was to add group=y in plots.R
	df_wide_x = mutate(df_wide,x=as.character(x))
	line_plot = line_plot_wide_data(df_wide_x, y_factor_order=colnames(df_wide)[2:4], stack=TRUE)
	expect_false(is.null(line_plot))
	# we could order x using x_factor_order as follows
	x_factor_order = df_wide_x[order(as.numeric(df_wide_x$x)), ]$x # could just pass in df_wide_x$x, since it is ordered, but documenting how to order if not already ordered
	line_plot = line_plot_wide_data(df_wide_x, x_factor_order=x_factor_order, y_factor_order=colnames(df_wide)[2:4], stack=TRUE)
	expect_false(is.null(line_plot))
})

test_that('output: plot: heat_map', {
	# for example, counts of visitors to website
	mock_website_visitors_mo = round(dnorm(seq(from=2, to=-1, length.out=24))*850)
	mock_website_visitors_tu = round(dnorm(seq(from=2, to=-1, length.out=24))*700)
	mock_website_visitors_we = round(dnorm(seq(from=2, to=-1, length.out=24))*800)
	mock_website_visitors_th = round(dnorm(seq(from=2, to=-1, length.out=24))*900)
	mock_website_visitors_fr = round(dnorm(seq(from=2, to=-1, length.out=24))*1000)
	mock_website_visitors_sa = round(dnorm(seq(from=1, to=-1, length.out=24))*500)
	mock_website_visitors_su = round(dnorm(seq(from=1, to=-1, length.out=24))*400)
	df_wide = data.frame(hour=0:23, monday=mock_website_visitors_mo, tuesday=mock_website_visitors_tu,wednesday=mock_website_visitors_we,thursday=mock_website_visitors_th,friday=mock_website_visitors_fr,saturday=mock_website_visitors_sa,sunday=mock_website_visitors_su)
	colnames(df_wide)[1] = 'x'

	ordered_factor = colnames(df_wide)[2:ncol(df_wide)]
	# NOTE THIS TESTS line_plot_wide_data & line_plot_long_data
	file_name = '../output/example_heat_map_wide_data.png'
	file.remove(file_name) # remove file to ensure no file is created for first test
	heat_map = heat_map_wide_data(df_wide)
	expect_false(is.null(heat_map))
	expect_false(file.exists(file_name))

	heat_map = heat_map_wide_data(df_wide, y_factor_order=ordered_factor)
	expect_false(is.null(heat_map))
	expect_false(file.exists(file_name))

	heat_map = heat_map_wide_data(df_wide, , y_factor_order=ordered_factor, save_file=file_name)
	expect_false(is.null(heat_map))
	expect_true(file.exists(file_name))
})

test_that('output: plot: tree_diagram', {

	# example from OpenIntro Statistics pg 98
	# Probability of a
	p_a = 0.0035
	# Probability (b | a)
	p_b_given_a = 0.89
	# Probability (b | ¬a)
	p_b_given_not_a = 0.07
	
	file_name = '../output/example_tree_diagram.png'
	file.remove(file_name)
	expect_false(file.exists(file_name))
	tree_diagram(p_a=p_a, p_b_given_a=p_b_given_a, p_b_given_not_a=p_b_given_not_a)
	expect_false(file.exists(file_name))
	tree_diagram(p_a=p_a, p_b_given_a=p_b_given_a, p_b_given_not_a=p_b_given_not_a, file_name = file_name)
	expect_true(file.exists(file_name))
	
	bayes_result = bayes_prevalence(prevalence = p_a, sensitivity = p_b_given_a, false_positive_rate = p_b_given_not_a)
	expect_equal(bayes_result, 0.04274736) # this is what should be displayed on graph for P(A|B)
})

test_that('output: plot: gg_qq_plot', {
	file_name = '../output/example_gg_qq_plot.png'
	if(file.exists(file_name))
	{
		file.remove(file_name)
	}

	data = c(0.015, 0.028, 0.177, 0.121, 0.102, 0.107, 0.019, 0.066, 0.058, 0.111)
	ggsave(filename= file_name, gg_qq_plot(data = data))

	expect_true(file.exists(file_name))
})
