library('testthat')
library('readr')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_logger.R")

test_that("general: logging", {
	# Test OUTPUT FILE
	expect_that(logger.output_file, equals('./output.txt'))
	logger.set_output('./tests/output.txt')
	expect_that(logger.output_file, equals('./tests/output.txt'))
	
	logger.set_output(NULL)
	expect_that(logger.output_file, equals(NULL))
	logger.reset_log_file() # make sure this doesn't crash with NULL output file name
	
	# Test THRESHOLD
	expect_that(logger.threshold, equals(logger.WARNING))
	logger.set_threshold()
	expect_that(logger.threshold, equals(logger.DEBUG))
	logger.set_threshold(logger.ERROR)
	expect_that(logger.threshold, equals(logger.ERROR))

	output_file = './output.txt'
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
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test\n')+1,nchar(test_file_text)), equals('test\n'))
	expect_that(nchar(test_file_text), equals(34))

	log.DEBUG('test2') # test DEBUG 
	log.DEBUG('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test2\n')+1,nchar(test_file_text)), equals('test2\n'))
	expect_that(nchar(test_file_text), equals(69))

	log.INFO('test4') # test INFO 
	log.INFO('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test4\n')+1,nchar(test_file_text)), equals('test4\n'))
	expect_that(nchar(test_file_text), equals(103))

	log.WARNING('test5') # test WARNING 
	log.WARNING('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test5\n')+1,nchar(test_file_text)), equals('test5\n'))
	expect_that(nchar(test_file_text), equals(140))

	log.NOTE('test6') # test NOTE 
	log.NOTE('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test6\n')+1,nchar(test_file_text)), equals('test6\n'))
	expect_that(nchar(test_file_text), equals(174))

	log.ERROR('test7') # test ERROR 
	log.ERROR('LOGGING ERROR', should_log=FALSE)
	test_file_text = read_file(output_file)
	expect_that(substr(test_file_text, nchar(test_file_text)-nchar('test7\n')+1,nchar(test_file_text)), equals('test7\n'))
	expect_that(nchar(test_file_text), equals(209))

	logger.reset_log_file()
	expect_false(file.exists(output_file))
})
