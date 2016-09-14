library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file('test_output.R')

test_that('output: print_c', {
	# TEST ON MATRIX
	# matrix from DataCamp
	box_office = c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)
	star_wars_matrix <- matrix(	box_office, nrow = 3, byrow = TRUE,
								dimnames = list(c('A New Hope', 'The Empire Strikes Back', 'Return of the Jedi'), 
												c('US', 'non-US')))
	printed_string = print_c(star_wars_matrix) #default
	expect_that(printed_string, equals('                             US non-US\nA New Hope              460.998  314.4\nThe Empire Strikes Back 290.475  247.9\nReturn of the Jedi      309.306  165.8'))

	printed_string = print_c(star_wars_matrix, print_row_names=TRUE)
	expect_that(printed_string, equals('                             US non-US\nA New Hope              460.998  314.4\nThe Empire Strikes Back 290.475  247.9\nReturn of the Jedi      309.306  165.8')) # no change

	printed_string = print_c(star_wars_matrix, collapse=', ', trim=TRUE)
	expect_that(printed_string, equals('US non-US, A New Hope              460.998  314.4, The Empire Strikes Back 290.475  247.9, Return of the Jedi      309.306  165.8'))

	# TEST ON DATAFRAME
	temp_dataframe = data.frame(col1 = c('a', 'b', 'c'), col2 = c(1,2,3))
	printed_string = print_c(temp_dataframe) #default
	expect_that(printed_string, equals(' col1 col2\n    a    1\n    b    2\n    c    3'))
	
	printed_string = print_c(temp_dataframe, collapse = ', ')
	expect_that(printed_string, equals(' col1 col2,     a    1,     b    2,     c    3'))

	printed_vector = print_c(temp_dataframe, collapse = NULL) # not collapsing the vector, so it outputs a vector
	expect_that(length(printed_vector), equals(4)) # 3 rows + 1 row for col.names 
	expect_that(printed_vector[1], equals(' col1 col2'))
	expect_that(printed_vector[2], equals('    a    1'))
	expect_that(printed_vector[3], equals('    b    2'))
	expect_that(printed_vector[4], equals('    c    3'))

	printed_vector = print_c(temp_dataframe, collapse = NULL, trim=TRUE) # not collapsing the vector, so it outputs a vector
	expect_that(length(printed_vector), equals(4)) # 3 rows + 1 row for col.names 
	expect_that(printed_vector[1], equals('col1 col2'))
	expect_that(printed_vector[2], equals('a    1'))
	expect_that(printed_vector[3], equals('b    2'))
	expect_that(printed_vector[4], equals('c    3'))

	printed_string = print_c(temp_dataframe, trim=TRUE)
	expect_that(printed_string, equals('col1 col2\n    a    1\n    b    2\n    c    3'))

	printed_string = print_c(temp_dataframe, print_row_names=TRUE)
	expect_that(printed_string, equals('  col1 col2\n1    a    1\n2    b    2\n3    c    3'))

	printed_string = print_c(temp_dataframe, print_row_names=TRUE, trim=TRUE)
	expect_that(printed_string, equals('col1 col2\n1    a    1\n2    b    2\n3    c    3'))
})
