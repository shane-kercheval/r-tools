# Output

## markdown.R
- helper functions that format text as markdown
- these functions are tailored towards GitHub markdown (https://guides.github.com/features/mastering-markdown/) so that the output of various analysis can be read directly via GitHub
- the defualt `postfix` value for most functions is *not* to add newlines. This is primarily used in `logger.R` which automatically formats trailing spaces or newlines
- example markdown file (generated from [test_markdown.R](../tests/test_markdown.R)): [example_markdown.md](./example_markdown.md)

## logger. R
- setting `logger.output_file` to NULL allows logging to be printed to console. Setting it to file path allows outputing to file.
	- e.g. you can do `logger.set_output(NULL)` and then manually use sink e.g. `sink(file='./results/non_try_analysis_results.txt', append=FALSE)`
