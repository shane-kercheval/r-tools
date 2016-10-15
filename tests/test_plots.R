library('testthat')
source('../tools.R', chdir=TRUE)

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
