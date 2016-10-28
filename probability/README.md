# probability

- probability related functions

## decision_analysis.R

- functions for aid in decisions

```R
expected_value <- function(probs=NULL, n_occur=NULL, benefits)
```
- calculates the expected value of benefits given the probability of each benefit occurring (`probs`) or the number of expected occurrences for each benefit (`n_occur`)
	- for example, we can use expected_value to calculate *expected profit* for, say, marketing initiatives given certain conversion rates and expected benefits (e.g. customer lifetime value) and expected costs (e.g. CPC)
- `benefits`: vector containing the values of expected benefits
	- values can be positive or negative to represent benefits or costs, respectively
- `probabilities`: vector of same length as `benefits` and represents respective probabilities that each benefit (or cost if negative value) will occur.
	- vector should sum to 1
- `n_occur`: vector of same length as `benefits` and represents the respective number of expected occurrences of each benefit

```R
bayes <- function(p_b, p_a_given_b, p_a_given_nb)
```
- Bayes' theorem: `P(B|A) = (P(B) * P(A|B)) / (P(B) * P(A|B)) + P(B') * P(A|B')))`, where:
	- `P(B|A)` (probability of B given A has occurred) is what the function returns
	- `P(A|B)` is the probability of A given B has occurred.
	- `P(B')` is the probability of the complement of Event B
	- `P(A|B')` is the probability of Event A, given that the complement to Event B has occurred.
