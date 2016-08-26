library('testthat')
library('readr')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_markdown.R")

test_that("output: markdown", {
	markdown_file = 'test_markdown.md'
	
	sink(file=markdown_file, append=FALSE)
	cat(h1('header 1'))
	cat('text\n')
	cat(h2('header 2'))
	cat('text\n')
	cat(h3('header 3'))
	cat('text\n')

	cat(sprintf('%s %s %s %s %s %s %s %s\n\n', 'this is some', bold('bold text'), 'and this is some', ital('italicized text'), 'and this is some', strike('strike-through text'), 'and this some', code('inline code')))
	cat(codeb('and here is some\nblock code'))
	cat(codeb('document.getElementById(\'demo\').style.fontSize=\'35px\'', syntax='javascript'))
	cat(bqoute('this is a blockquote'))
	sink() # remove sink

	test_file_text = read_file(markdown_file)
	expect_that(nchar(test_file_text), equals(313))
	
	file.remove(markdown_file)
})
