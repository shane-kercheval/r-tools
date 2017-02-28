library('testthat')
source('../general/utilities.R', chdir=TRUE)

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
	expect_true(is.nullNaEmpty('asdf','',2))
	expect_true(is.nullNaEmpty(''))
	expect_true(is.nullNaEmpty(' ', 2))
	expect_true(is.nullNaEmpty('   '))
	expect_true(is.nullNaEmpty(NA))
	expect_true(is.nullNaEmpty(NULL))

	expect_false(is.nullNaEmpty('asdf'))
	expect_false(is.nullNaEmpty('asdf', 'a'))
	expect_false(is.nullNaEmpty('NULL', 'NA'))
	expect_false(is.nullNaEmpty('   1'))
	expect_false(is.nullNaEmpty('1', 1))
	expect_false(is.nullNaEmpty(1))

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
	expect_false(is.nullNaEmpty(c('asdf'), c('asfd',2), 'adsf',2))
})

test_that("utilities: lazy_load", {
	
	# need to first test create_object==FALSE when object doesnt exist in memory to test corner case.
	path = './data/lazy_load.Rda'
	a = c(1,2,3)
	b = c(4,5,6)
	
	expect_false(exists('df_lazy_load'))
	expect_true(file.exists(path))
	
	# this should load the data from .Rda, because it doesn't exist in memory, and create_data==FALSE
	df_lazy_load = lazy_load(path=path, object=df_lazy_load, create_data=FALSE, dataset_function=function(){stop("THIS SHOULDN'T BE CALLED :(")})
	expect_equal(df_lazy_load$a, a)
	expect_equal(df_lazy_load$b, b)
	
	# doing it again with null data should force .Rda load
	df_lazy_load = NULL
	df_lazy_load = lazy_load(path=path, object=df_lazy_load, create_data=FALSE, dataset_function=function(){stop("THIS SHOULDN'T BE CALLED :(")})
	expect_equal(df_lazy_load$a, a)
	expect_equal(df_lazy_load$b, b)
	
	#test building from scratch
	file.remove(path)
	expect_false(file.exists(path))
	df_lazy_load = lazy_load(path=path, object=df_lazy_load, create_data=TRUE, dataset_function=function(){data.frame(a=a, b=b)})
	expect_true(file.exists(path))
	expect_equal(df_lazy_load$a, a)
	expect_equal(df_lazy_load$b, b)
	df_lazy_load = NULL
	
	df_lazy_load = lazy_load(path=path, object=df_lazy_load, create_data=FALSE, dataset_function=function(){stop("THIS SHOULDN'T BE CALLED :(")})
	expect_equal(df_lazy_load$a, a)
	expect_equal(df_lazy_load$b, b)
})
