library('testthat')
library('readr')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_logger.R")

test_that("output: logging", {
	# Test OUTPUT FILE
	expect_that(logger.output_file, equals('./output.txt'))
	
	logger.set_output(NULL)
	expect_that(logger.output_file, equals(NULL))
	logger.reset_log_file() # make sure this doesn't crash with NULL output file name
	
	output_file = '../output/example_logger.txt'
	expect_true(file.exists(output_file))
	logger.set_output(output_file = output_file)
	logger.reset_log_file() # should delete file if it exists (which it should because we keep it for an example in git repo)
	expect_that(logger.output_file, equals(output_file))
	expect_false(file.exists(output_file))
	
	# Test THRESHOLD
	expect_that(logger.threshold, equals(logger.WARNING))
	logger.set_threshold()
	expect_that(logger.threshold, equals(logger.DEBUG))
	logger.set_threshold(logger.ERROR)
	expect_that(logger.threshold, equals(logger.ERROR))

	logger.set_output(output_file)
	expect_that(logger.output_file, equals(output_file))
	expect_false(file.exists(output_file))
	
	logger.set_threshold(logger.INFO) # set logger to INFO
	expect_that(logger.threshold, equals(logger.INFO))	# verify INFO level
	log.DEBUG('test') # test DEBUG (which shouldn't print anything because it is lower level than INFO)
	expect_false(file.exists(output_file))

	logger.set_threshold(logger.DEBUG) # set logger to DEBUG
	expect_that(logger.threshold, equals(logger.DEBUG))	# verify DEBUG level
	log.DEBUG('test') # test DEBUG 
	expect_true(file.exists(output_file))
	
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test\n\n')+1,nchar(test_file_text)), equals('test\n\n'))
	expect_that(nchar(test_file_text), equals(35))

	log.DEBUG('test2') # test DEBUG 
	log.DEBUG('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test2\n\n')+1,nchar(test_file_text)), equals('test2\n\n'))
	expect_that(nchar(test_file_text), equals(71))

	log.INFO('test3') # test INFO 
	log.INFO('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test3\n\n')+1,nchar(test_file_text)), equals('test3\n\n'))
	expect_that(nchar(test_file_text), equals(106))

	log.WARNING('test4') # test WARNING 
	log.WARNING('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test4\n\n')+1,nchar(test_file_text)), equals('test4\n\n'))
	expect_that(nchar(test_file_text), equals(144))

	log.ERROR('test5') # test ERROR 
	log.ERROR('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test5\n\n')+1,nchar(test_file_text)), equals('test5\n\n'))
	expect_that(nchar(test_file_text), equals(180))

	log.NOTE('test note') # test NOTE 
	log.NOTE('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test note\n\n')+1,nchar(test_file_text)), equals('test note\n\n'))
	expect_that(nchar(test_file_text), equals(191))
})

test_that("output: logging-markdown", {
	logger.set_use_markdown(TRUE)
	expect_true(logger.use_markdown)
})
