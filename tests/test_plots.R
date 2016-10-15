library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file('test_plots.R')

test_that('output: plot: plot_wide_data', {
	set.seed(1)
	normal_distribution = rnorm(n=20)
	df_wide = data.frame(x=1:20, rnorm=normal_distribution, flipped=normal_distribution * -1, zero=rep(0,20))

	line_plot = plot_wide_data(df_wide)
	expect_false(is.null(line_plot))
	ggsave(plot=line_plot, filename='../output/example_plot_wide_data.png')
})
