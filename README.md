# r-tools
Collection of helper functions/libraries in R.

# Documentation
Each subdirectory has a README.md file which contains information about functions contained within that package
- [clustering/README.md](./clustering/README.md)
- [general/README.md](./general/README.md)
- [output/README.md](./output/README.md)
- [puttern_recognition/README.md](./puttern_recognition/README.md)

# Using

- download repo to `~` directory
- install the following packages with these commands
```
R
install.packages('fpc')
install.packages('RColorBrewer')
install.packages('ggplot2')
install.packages('scales')
install.packages('reshape2')
install.packages('NbClust')
install.packages('stringr')
install.packages('arules')
install.packages('arulesSequences')
install.packages('testthat')
```
- Include `tools.R` in your R files: `source('~/r-tools/tools.R', chdir=TRUE)`

# Testing
- Run unit tests with working directory set to root folder `~/r-tools/` and command:
```
library('testthat')
test_dir('tests')
```
