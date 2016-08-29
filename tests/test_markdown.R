library('testthat')
library('readr')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_markdown.R")

test_that("output: markdown", {
	markdown_file = '../output/example_markdown.md'
	
	sink(file=markdown_file, append=FALSE) # should automatically delete file if it exists (which it should because we keep it for an example)
	cat(h1('header 1', postfix='\n'))
	cat('text\n')
	cat(h2('header 2', postfix='\n'))
	cat('text\n')
	cat(h3('header 3', postfix='\n'))
	cat('text\n')

	cat(sprintf('%s %s %s %s %s %s %s %s\n\n', 'this is some', bold('bold text'), 'and this is some', ital('italicized text'), 'and this is some', strike('strike-through text'), 'and this some', code('inline code')))
	cat(codeb('and here is some\nblock code',postfix='\n\n'))
	cat(codeb('document.getElementById(\'demo\').style.fontSize=\'35px\'', syntax='javascript',postfix='\n\n'))
	cat(blockq('this is a blockquote', postfix='\n\n'))

	vector1 = seq(from=0, to=100, by=1)
	vector2 = seq(from=100, to=0, by=-1)
	percentile_matrix = create_percentile_matrix(list_of_datasets=list(vector1, vector2), c('test1', 'test2'))
	cat(table_matrix(a_matrix=percentile_matrix, title='example table from matrix', title_format=h3, postfix='\n'))
	cat('and blow is a code block using `codebc` which calls `print_c` in [output.R](./output.R) to preserve `print()`-like formatting:\n') # add this to test formatting (something after matrix)
	cat(codebc(summary(percentile_matrix), postfix = '\n\n'))
	cat('and here is an image:\n\n')
	cat(image(text='the image', url='./data/kmeans_non-try_5_clusters_2016-08-29.png', postfix='\n\n'))
	cat('the end :)\n') # add this to test formatting (something after matrix)
	sink() # remove sink

	test_file_text = read_file(markdown_file)
	expect_that(nchar(test_file_text), equals(2614))
})
