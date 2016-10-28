# probability

- probability related functions

## decision_analysis.R

- functions for aid in decisions

```R
expected_value <- function(probs=NULL, n_occur=NULL, benefits)
```
- calculates the expected value of benefits given the probability of each benefit occurring (`probs`) or the number of expected occurrences for each benefit (`n_occur`)
- `benefits`: vector containing the values of expected benefits
	- values can be positive or negative to represent benefits or costs, respectively
- `probabilities`: vector of same length as `benefits` and represents respective probabilities that each benefit (or cost if negative value) will occur.
	- vector should sum to 1
- `n_occur`: vector of same length as `benefits` and represents the respective number of expected occurrences of each benefit
