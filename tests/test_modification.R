library(tidyverse)
library('testthat')
source('../general/modification.R', chdir=TRUE)

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

	expected_scaled = tribble(
				~a, ~b,
		-1.4863011, -0.9486833,
		-1.1560120,  0.9486833,
		-0.8257228, -0.9486833,
		-0.4954337,  0.9486833,
		-0.1651446, -0.9486833,
		 0.1651446,  0.9486833,
		 0.4954337, -0.9486833,
		 0.8257228,  0.9486833,
		 1.1560120, -0.9486833,
		 1.4863011,  0.9486833)
	
	data_frame_logical = data.frame(named_column, a, b_logical)
	scaled_logical = get_scaled_dataset(data_frame=data_frame_logical, named_column='named_column')
	expect_equal(scaled_logical$a, expected_scaled$a, tolerance=0.0000001)
	expect_equal(scaled_logical$b_logical, expected_scaled$b, tolerance=0.0000001)
	
	data_frame_numeric = data.frame(named_column, a, b_numeric)
	scaled_numeric = get_scaled_dataset(data_frame=data_frame_numeric, named_column='named_column')
	expect_equal(scaled_numeric$a, expected_scaled$a, tolerance=0.0000001)
	expect_equal(scaled_numeric$b_numeric, expected_scaled$b, tolerance=0.0000001)
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

test_that("modification: normalize", {
	expect_equal(normalize(c(1, 2, 3, 4, 5)), c(0.00, 0.25, 0.50, 0.75, 1.00))
	expect_equal(normalize(c(10, 20, 30, 40, 50)), c(0.00, 0.25, 0.50, 0.75, 1.00))
})

test_that("modification: normalize", {
	contingency_table = matrix(c(60, 255, 86, 426, 58, 450, 21, 382), nrow = 2, dimnames = list(c('yes', 'no'), c('18-29', '30-49', '50-64', '65+')))
	contingency_table = add_matrix_totals(contingency_table)

	# check column totals
	expect_equal(as.vector(contingency_table[3, ]), c(315, 512, 508, 403, 1738))
	# check row totals
	expect_equal(as.vector(contingency_table[, 5]), c(225, 1513, 1738))
	# check labels
	expect_equal(colnames(contingency_table), c("18-29", "30-49", "50-64", "65+", "TOTALS"))
	expect_equal(rownames(contingency_table), c("yes", "no", "TOTALS"))
})

test_that("modification: add_dummy_columns", {
	# default
	df_test <- data.frame(strcol = c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
	df_actual <- add_dummy_columns(data_frame = df_test, column_name = 'strcol')
	df_expected <- readRDS('./data/add_dummy_columns_sort_false.Rds')
	expect_true(all(df_actual == df_expected))
	# sort levels
	df_test <- data.frame(strcol = c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
	df_actual <- add_dummy_columns(data_frame = df_test, column_name = 'strcol', sort_levels = TRUE)
	df_expected <- readRDS('./data/add_dummy_columns_sort_true.Rds')
	expect_true(all(df_actual == df_expected))
	# use factor levels in variable
	df_test <- data.frame(strcol = factor(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"), levels = c('G', 'F', 'E', 'D', 'C', 'B', 'A')))
	df_actual <- add_dummy_columns(data_frame = df_test, column_name = 'strcol', use_levels = TRUE)
	df_expected <- readRDS('./data/add_dummy_columns_use_levels.Rds')
	expect_true(all(df_actual == df_expected))
	# use custom levels in variable
	df_test <- data.frame(strcol = factor(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"), levels = c('G', 'F', 'E', 'D', 'C', 'B', 'A')))
	df_actual <- add_dummy_columns(data_frame = df_test, column_name = 'strcol', custom_levels = c('F', 'G', 'E', 'D', 'C', 'B', 'A'))
	df_expected <- readRDS('./data/add_dummy_columns_custom_levels.Rds')
	expect_true(all(df_actual == df_expected))
})

test_that("modification: create_many_to_many_group_id", {

	relationship_df <- tribble(
		~colA, ~colB,
		'a1',   'b1',
		'a1',   'b4',
		'a2',   'b2',
		'a2',   'b3',
		'a3',   'b3',
		'a4',   'b1',
		'a4',   'b5',
		'a5',   'b6',
		'a6',   'b6',
		'a7',   'b7')
	
	relationship_with_group_df <- create_many_to_many_group_id(m_2_m_relationship_df = relationship_df[sample(1:nrow(relationship_df)),], column_A = colA, column_B = colB)
	
	expect_true(all(map_lgl(unique(relationship_df$colA), ~ {
		column_a_value <- .
		return (length(unique((relationship_with_group_df %>% filter(colA == column_a_value))$group_id)) == 1)
	})))
	
	expect_true(all(map_lgl(unique(relationship_df$colB), ~ {
		column_b_value <- .
		return (length(unique((relationship_with_group_df %>% filter(colB == column_b_value))$group_id)) == 1)
	})))
	
	expect_true(all(map2_lgl(c('a1', 'a2', 'a5'), c('a4', 'a3', 'a6'), ~ unique((relationship_with_group_df %>% filter(colA == .x))$group_id) == unique((relationship_with_group_df %>% filter(colA == .y))$group_id))))
	expect_true(all(map2_lgl(c('b1', 'b2', 'b1'), c('b4', 'b3', 'b5'), ~ unique((relationship_with_group_df %>% filter(colB == .x))$group_id) == unique((relationship_with_group_df %>% filter(colB == .y))$group_id))))
	
	expect_true(sum(unique((relationship_with_group_df %>% filter(colA == 'a7'))$group_id) == relationship_with_group_df$group_id) == 1) # make sure it is only found once
	expect_true(sum(unique((relationship_with_group_df %>% filter(colB == 'b7'))$group_id) == relationship_with_group_df$group_id) == 1) # make sure it is only found once
	
	expect_true(length(unique(relationship_with_group_df$group_id)) == 4) # 4 distinct groups
})
