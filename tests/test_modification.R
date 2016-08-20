library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#test_file("test_modification.R")

test_that("modification: get_x_dataset", {
	# test that there is no need to convert boolean to 
	named_column = c('1','2','3','4','5','6','7','8','9','10')
	a = c(1,2,3,4,5,6,7,8,9,10)
	b_logical = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
	b_numeric = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

	data_frame = data.frame(named_column, a, b_logical, b_numeric)

	dataset = get_numeric_dataset(data_frame=data_frame)
	expect_true(all(colnames(dataset) == c('a', 'b_numeric')))
	expect_that(nrow(dataset), equals(10))
	expect_true(all(dataset$a == a))
	expect_true(all(dataset$b_numeric == b_numeric))
	
	dataset = get_numeric_dataset(data_frame=data_frame, named_column='named_column')
	expect_true(all(colnames(dataset) == c('named_column', 'a', 'b_numeric')))
	expect_that(nrow(dataset), equals(10))
	expect_true(all(dataset$named_column == named_column))
	expect_true(all(dataset$a == a))
	expect_true(all(dataset$b_numeric == b_numeric))
	
	dataset = get_numeric_logical_dataset(data_frame=data_frame)
	expect_true(all(colnames(dataset) == c('a', 'b_numeric', 'b_logical')))
	expect_that(nrow(dataset), equals(10))
	expect_true(all(dataset$a == a))
	expect_true(all(dataset$b_logical == b_logical))
	expect_true(all(dataset$b_numeric == b_numeric))

	dataset = get_numeric_logical_dataset(data_frame=data_frame, named_column='named_column')
	expect_true(all(colnames(dataset) == c('named_column', 'a', 'b_numeric', 'b_logical')))
	expect_that(nrow(dataset), equals(10))
	expect_true(all(dataset$named_column == named_column))
	expect_true(all(dataset$a == a))
	expect_true(all(dataset$b_logical == b_logical))
	expect_true(all(dataset$b_numeric == b_numeric))
})

test_that("modification: get_scaled_dataset", {
	# test that there is no need to convert boolean to 
	named_column = c('1','2','3','4','5','6','7','8','9','10')
	a = c(1,2,3,4,5,6,7,8,9,10)
	b_logical = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
	b_numeric = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

	data_frame_logical = data.frame(named_column, a, b_logical)
	scaled_logical = get_scaled_dataset(data_frame=data_frame_logical, named_column='named_column')

	data_frame_numeric = data.frame(named_column, a, b_numeric)
	scaled_numeric = get_scaled_dataset(data_frame=data_frame_numeric, named_column='named_column')

	expect_true(all(scaled_logical == scaled_numeric))
})
