library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#test_file("test_utilities.R")

test_that("utilities: stopif", {

	possibleError = tryCatch(stopif(TRUE, message='test'), error=function(e) e)
	expect_true(inherits(possibleError, 'error'))
	expect_that(possibleError$message, equals('test'))
	
	possibleError = tryCatch(stopif(FALSE, message='test1'), error=function(e) e)
	expect_false(inherits(possibleError, 'error'))
})

test_that("utilities: is.nullNaEmpty", {
	# individual items as parameters (i.e. non-vector)
	expect_true(is.nullNaEmpty(NULL,'asdf','', NA))
	expect_true(is.nullNaEmpty('asdf','', NA))
	expect_true(is.nullNaEmpty('asdf',''))
	expect_true(is.nullNaEmpty(''))
	expect_true(is.nullNaEmpty(NA))
	expect_true(is.nullNaEmpty(NULL))

	expect_false(is.nullNaEmpty('asdf'))
	expect_false(is.nullNaEmpty('asdf', 'a'))
	expect_false(is.nullNaEmpty('NULL', 'NA'))

	# vectors
	expect_true(is.nullNaEmpty(c(NULL,'asdf','', NA)))
	expect_true(is.nullNaEmpty(c(NULL,'asdf','', NA), c('')))
	expect_true(is.nullNaEmpty(c('asdf','', NA, NULL), c('asfd')))
	expect_true(is.nullNaEmpty(c('asdf','', NA), c('asfd')))
	expect_true(is.nullNaEmpty(c('asdf',''), c('asfd')))
	expect_true(is.nullNaEmpty(c(''),c('asdf','', NA, NULL)))
	expect_true(is.nullNaEmpty(c(''),c('asdf')))
	expect_true(is.nullNaEmpty(c(''),c('asdf','')))
	expect_true(is.nullNaEmpty(c('asdf', 'asdf'), c('asfd', 'asdf'), ''))
	expect_true(is.nullNaEmpty(c('asdf', 'asdf'), c('asfd', NA), 'asdfas'))
	expect_true(is.nullNaEmpty(c('asdf', 'asdf'), c('asfd', 'asdf'), NULL))
	
	expect_false(is.nullNaEmpty(c('asdf', 'asdf'), c('asfd', 'asdf'), 'adsf'))
	expect_false(is.nullNaEmpty(c('asdf'), c('asfd'), 'adsf'))
})
