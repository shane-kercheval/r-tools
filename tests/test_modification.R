library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
# test_file("test_modification.R")

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

test_that("modification: match_regex", {

	websites = read.csv(file='./data/test_website_strings.csv')
	
	pattern_to_find = '\\b\\/\\w{20,}\\b'
	pattern_to_extract = 'manage.auth0.com\\/.*'
	substitue_find_with = ''
	results = vector_match_regex(the_vector=websites$name, pattern_to_find=pattern_to_find, pattern_to_extract=pattern_to_extract, 
									substitue_find_with=substitue_find_with)
	#websites$expects_1 = rep(NA, nrow(websites))
	#websites$expects_1[results[[1]]] = results[[2]]
	#write.csv(websites,'./data/test_website_strings.csv', row.names = FALSE)
	expect_that(results[[1]], equals(which(!is.na(websites$expects_1))))	
	expect_true(all(results[[2]] == websites$expects_1[results[[1]]]))	

	pattern_to_find = 'manage.auth0.com\\/#\\/\\w+\\/.*'
	results = vector_match_regex(the_vector=websites$name, pattern_to_find=pattern_to_find)
	#websites$expects_2 = rep(NA, nrow(websites))
	#websites$expects_2[results[[1]]] = results[[2]]
	#write.csv(websites,'./data/test_website_strings.csv', row.names = FALSE)
	expect_that(results[[1]], equals(which(!is.na(websites$expects_2))))	
	expect_true(all(results[[2]] == websites$expects_2[results[[1]]]))	

	pattern_to_find = 'manage.auth0.com\\/#\\/\\w*'
	results = vector_match_regex(the_vector=websites$name, pattern_to_find=pattern_to_find)
	#websites$expects_3 = rep(NA, nrow(websites))
	#websites$expects_3[results[[1]]] = results[[2]]
	#write.csv(websites,'./data/test_website_strings.csv', row.names = FALSE)
	expect_that(results[[1]], equals(which(!is.na(websites$expects_3))))	
	expect_true(all(results[[2]] == websites$expects_3[results[[1]]]))	

	pattern_to_find = 'manage.auth0.com\\/\\w+\\/*'
	results = vector_match_regex(the_vector=websites$name, pattern_to_find=pattern_to_find)
	#websites$expects_4 = rep(NA, nrow(websites))
	#websites$expects_4[results[[1]]] = results[[2]]
	#write.csv(websites,'./data/test_website_strings.csv', row.names = FALSE)
	expect_that(results[[1]], equals(which(!is.na(websites$expects_4))))	
	expect_true(all(results[[2]] == websites$expects_4[results[[1]]]))	

	pattern_to_find = 'manage.auth0.com\\/#'
	pattern_to_extract = 'manage.auth0.com\\/#.*'
	substitue_find_with = '/#'
	results = vector_match_regex(the_vector=websites$name, pattern_to_find=pattern_to_find, pattern_to_extract=pattern_to_extract, 
									substitue_find_with=substitue_find_with)
	#websites$expects_5 = rep(NA, nrow(websites))
	#websites$expects_5[results[[1]]] = results[[2]]
	#write.csv(websites,'./data/test_website_strings.csv', row.names = FALSE)
	expect_that(results[[1]], equals(which(!is.na(websites$expects_5))))	
	expect_true(all(results[[2]] == websites$expects_5[results[[1]]]))	
})
