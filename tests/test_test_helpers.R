library('testthat')
library('readr')
source('../testing/test_helpers.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_test_helpers.R")

temp_function <- function(output_file, text)
{
	sink(output_file)
	cat(text)
	sink()
}

test_that("test_helpers: test_output_file", {
	original_file = './data/test_file_test_helpers.txt'
	original_content = read_file(original_file)
	new_file = 'temp.txt'

	# this (below) should fail, but can't wrap in trycatch, so not sure how to test a test failing?
	#test_output_file(output_file=new_file, expected_chars=1000, delete_output_file=FALSE, 
	#				 FUN=function(){ temp_function(output_file=new_file, text=original_content) })
	
	test_output_file(output_file=new_file, expected_chars=79, delete_output_file=FALSE, 
					 FUN=function(){ temp_function(output_file=new_file, text=original_content) })
	expect_true(file.exists(new_file))
	
	test_output_file(output_file=new_file, expected_chars=79, expected_file_output=original_content, 
						FUN=function(){ temp_function(output_file=new_file, text=original_content) })
	expect_false(file.exists(new_file))
})
