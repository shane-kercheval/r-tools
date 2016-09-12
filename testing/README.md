# testing
- helper functions for unit tests (using the [testthat](https://github.com/hadley/testthat) framework)

## 
test_helpers.R

```R
test_output_file <- function(output_file, FUN, expected_file_output=NULL)
```
- helper method to ensure that a function (which writes to a file) is writing the expected output 
- takes `FUN` argument (no parameters) that should be a function which outputs to the output file `output_file`
- this helper funciton deletes `output_file` if it already exists, runs the `FUN`, and ensures that newly created `output_file` contains expected number of characters (`epxected_chars`)
- if `expected_file_output` is passed in, the helper function additionally ensures that the text of `output_file` matches that of `expected_file_output`
- by default, this function deletes the `output_file` after finishing, but can be overriddent with parameter `delete_output_file=FALSE` 
