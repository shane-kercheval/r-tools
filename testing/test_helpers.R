library('testthat')

test_output_file <- function(output_file, FUN, expected_chars, expected_file_output=NULL, delete_output_file=TRUE)
{
	if(file.exists(output_file))
	{
		file.remove(output_file)
	}
	expect_false(file.exists(output_file))
	FUN()
	expect_true(file.exists(output_file))	
	test_file_text = read_file(output_file)
	expect_equal(nchar(test_file_text), expected_chars)
	
	if(!is.null(expected_file_output))
	{
		expect_equal(expected_file_output, test_file_text)
	}

	if(delete_output_file)
	{
		file.remove(output_file)
	}
}
