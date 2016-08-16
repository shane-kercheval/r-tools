# r-tools
Collection of helper methods in R.

# Using

- download repo to `~` directory
- Include `tools.R` in your R files: `source('~/r-tools/tools.R', chdir=TRUE)`

# Testing
- Run unit tests with working directory set to root folder `~/r-tools/` and command:
```
library('testthat')
test_dir('tests')
```
