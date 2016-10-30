# r-tools
Collection of helper functions/libraries in R.

> NOTE: this collection of functions is meant to be a) a personal learning tool while I discover and work with R, b) a personal learning tool as I  learn/implement marketing and data science formulas/techniques, and c) a way to abstract/wrap non-trivial R package calls and implementation techniques.
>
> For these reasons, I have not packaged these functions, and they are not indented to consumable by others. That said, this is clearly open source code and you are free to download, use, and offer updates via pull requests.
>
> I will maintain this repository for only as long as it remains aligned with my interests and career path. While maintaining, I plan to unit test everything and keep well-documented README's in each folder.

# Documentation
Each subdirectory has a README.md file which contains information about functions contained within that package
- [clustering/README.md](./clustering/README.md)
- [general/README.md](./general/README.md)
- [output/README.md](./output/README.md)
- [puttern_recognition/README.md](./puttern_recognition/README.md)
- [probability/README.md](./probability/README.md)

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
install.packages('dplyr')
install.packages('tidyr')
install.packages('purrr')
install.packages('Hmisc')
install.packages('corrr')
install.packages('arules')
install.packages('arulesSequences')
install.packages('FinCal')
```
- Include `tools.R` in your R files: `source('~/r-tools/tools.R', chdir=TRUE)`

# Testing
- Run unit tests with working directory set to root folder `~/r-tools/` and command:
```
R
library('testthat')
test_dir('tests')
```
