# probability

- probability related functions

## bayes.R
- helper functions for `bayes rule`

```R
bayes_simple <- function(p_e, p_h, p_e_given_h)
bayes_explicit <- function(p_h, p_e_given_h, p_e_given_nh)
```
- equation reference: http://homepages.wmich.edu/~mcgrew/Bayes8.pdf
- `H` is your hypothesis and `E` is your evidence
	- `H` is also known as the posterior
- `P(H|E)` (probability of B given A has occurred) is what the function returns
- `p_e` is `P(E)` and is the probability of the evidence in general. Also known as the `prior`
	- a.k.a base rate, the prevalence of E in the population as a whole.
- `p_h` is `P(H)` is the probability of Event H (overall)
- `p_nh` is `P(H')` is the probability of the complement of Event H
- `p_e_given_h` is `P(E|H)` is the probability of A given B has occurred.
	- this is also known as the `likelihood` of seeing the evidence when H is present.
	- in prediction, this is the `sensitivity` of the test (a.k.a `true positive rate`)
- `p_e_given_nh` is `P(E|H')` is the probability of Event E, given that the complement to Event H has occurred.
	- in prediction, this would be the `false positive rate` (number of false_positives / total actual negatives)
		e.g. the probability that you test positive even though you don't have the disease.
- additional resources
	- http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Probability/BS704_Probability6.html
	- http://www.statsdirect.com/help/clinical_epidemiology/screening_test.htm

```R
bayes_prevalence <- function(prevalence, sensitivity, specificity=NULL, false_positive_rate=NULL)
```
- used for terminology common with medical tests
	- `D` == disease
	- `T` == positive test
	- simple Bayes: `P(D|T) == P(T|D) * P(D) / P(T)`
- Probability of disease given a positive test == `P(D|T)` == `(prevalence * sensitivity) / ((prevalence * sensitivity) + ((1-prevalence) * (1-specificity)))`
	- _([source](http://ebp.uga.edu/courses/Chapter%204%20-%20Diagnosis%20I/4%20-%20Sensitivity%20and%20specificity.html))_
- where
	- `prevalence` == `P(disease in population)` == `P(D)` == probability of hypothesis in general (e.g disease in the relevant population) == `true_pos + false_neg / total_observations`
	- `sensitivity` == `P(positive test | disease)` == `P(T|D)` == among patients with disease, the probability of a positive test == `true_pos / actual_pos`
	- `specificity` == `true negative rate` == `P(not positive test | no disease)` == `P(not E | not H)` == among patients without disease (i.e. healthy patients), the probability of a negative test == `true_neg / actual_neg`
		- `specificity` is `1 - false positive rate`
	- `false_positive_rate` == `P(E | not H)` == among patients without disease, the probability of a positive test
		- `false_positive_rate` is `1 - specificity`
	- if `specificity` is provided it is simply converted to `false positive rate` == `[ P(not E | not H) == 1 - P(E | not H)]`
- provide either `specificity` or `false_positive_rate`
- resources
	- https://www.math.hmc.edu/funfacts/ffiles/30002.6.shtml
	- http://ebp.uga.edu/courses/Chapter%204%20-%20Diagnosis%20I/4%20-%20Sensitivity%20and%20specificity.html

```R
bayes_confusion <- function(conf_list)
```
- takes a confusion matrix in the form of a list, returned by `confusion_list` ([/general/README.md](../general/README.md))

```R
prob_e <- function(p_h, p_e_given_h, p_e_given_nh)
```
- simple Bayes rule is `P(H|E)= P(E|H) * P(H) / P(E)`, but if P(E) is unknown, you can use this method which does `P(E)=P(E|H)P(H)+P(E|not H)P(not H)`

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
customer_lifetime_value <- function(retention_rate_monthly=NULL, retention_rate_annual=NULL, avg_monthly_customer_revenue=NULL, avg_annual_customer_revenue=NULL, contribution_margin_ratio, acquisition_conversion_rates_per_stage=NULL, acquisition_conversion_rate=NULL, cost_per_event=NULL, cost_of_customer_acquisition=NULL, discount_rate=.10)
```
- calculates customer lifetime value (CLV) and cost of customer acquisition (CAC) type metrics
- Paid user acquisition works for you when the following proves true: `LTV > CAC`

parameters:

- `retention_rate_annual` is the annual retention rate (percent of customers that remain 1 year after signing up (or percent of customers that repurchase after a year, etc.)).
	- retention rate = 1 - churn rate
	- `retention_rate_monthly` can be used for monthly rates, and will be converted to the annual equivalent (`retention_rate_monthly`^(12))
- `avg_annual_customer_revenue` is the revenue a single customer generates in a single year
	- `avg_monthly_customer_revenue` can be used for monthly revenue
- `contribution_margin_ratio` is the gross margin on VARIABLE costs
	- i.e. the *fraction* of revenue that *contributes* to the offset of fixed costs.
- `acquisition_conversion_rate` is the conversion rate from 'event' (e.g. click) to 'acquisitions' (e.g. paying customer)
	- `acquisition_conversion_rates_per_stage` can be used if there are multiple 'stages' from the event (e.g. 'click') to 'acquisition'
		- for example, pay per click and stages are click->signup->pay. If the click->signup conversion rate is 5% and the signup->pay conversion rate is 10% then you can pass int the rates like `c(0.05, 0.10)`
- `cost_per_event`
	- e.g. cost per 'click', 'signup', etc.
- `cost_of_customer_acquisition`
	- rather than passing in `acquisition_conversion_rate` and `cost_per_event`, which are used to calculate the cost of customer acquisition (CAC), you can directly pass in CAC
- `discount_rate` is the interest rate at which the amount will be compounded each period

returns:

- `acquisition_conversion_rate`
- `cost_per_event`
- `cost_of_customer_acquisition`
- `retention_rate_annual`
- `average_customer_life_span`: calculated from retention rate, average number of years a customer will stay
- `average_annual_customer_revenue`
- `contribution_margin_ratio`
- `contribution_margin_annual`: the amount that each customers *contributes* on a yearly basis to offset fixed costs.
- `customer_lifetime_profit`: `average_customer_life_span` * `contribution_margin_annual`
- `customer_lifetime_value`: `customer_lifetime_profit` - `cost_of_customer_acquisition`
- `clv_to_cac`: ratio of customer lifetime value to cost of acquisition
- `payback_period_years`: amount of years it takes to payoff the cost of acquisition from the customer annual contribution margin.
- `return_on_investment`: ROI based on lifetime value and cost of acquisition
- `annualized_return`: annualized return based on lifetime value and lifespan
- `customer_lifetime_value_npv`: lifetime value based on Net Present Value which is a discounted cashflow based on `discount_rate`
- `clv_to_cac_pv`: lifetime value to cost of acquisition ratio based on discounted cash flows
- `return_on_investment_pv`: return on investment based on discounted cash flows
- `annualized_return_pv`: annualized return based on discounted cash flows


acquisition_conversion_rates_per_stage

- examples:
	- pay per click
		- cost per click is $2.50 ()
	- pay per signup w/ proxy


	- so if it costs you $1 per click and, on average, and it takes 100 clicks to convert someone to acquisition (1/100) your acquisition_conversion_rate will be 1% (0.01) and your expected cost of acquisition will be $100 per customer
		- `cost_per_event` = 1.00
		- `acquisition_conversion_rate` = 0.01
	- or if you have a long/complex sales cycle, perhaps you have a model of prediction (via logistic regression, for example) or some other 'proxy' as your end goal (i.e. acquisition)
		- so let's say we are paying for signups ($4.50 per signup), we have a score proxy (X), and from that score proxy, the majority are 'acquired' (.e. pay), because X is a good predictor.
			- so, in this case, our click->signup CR doesn't matter because we aren't paying for clicks
			- signup->proxy CR let's say is 5%, and historical proxy->acquistion CR is 90% (because it is a good model, estimator, or prox)
			- let's then say our retention rate is 75% annual, on average each *acquired* customer brings in $300 per year, and 1/3 of that is profit (based on variable costs)
			- so:
				- `annual_retention_rate` = .75
				- `average_annual_revenue` = 300
				- `contribution_margin_ratio` = 1/3
				- `cost_per_event` = 4.50
				- `acquisition_conversion_rates_per_stage` = c(0.05, 0.90)
			- results:
				- $acquisition_conversion_rate: 0.045
				- $cost_per_event: 4.5
				- $cost_of_customer_acquisition: 100
				- $retention_rate_annual: 0.75
				- $average_customer_life_span: 4
				- $average_annual_customer_revenue: 300
				- $contribution_margin_ratio: 0.3333333
				- $contribution_margin_annual: 100
				- $customer_lifetime_profit: 400
				- $customer_lifetime_value: 300
				- $clv_to_cac: 3
				- $payback_period_years: 1
				- $return_on_investment: 2
				- $annualized_return: 0.316074
				- $customer_lifetime_value_npv: 216.9865
				- $clv_to_cac_pv: 2.169865
				- $return_on_investment_pv: 1.169865
				- $annualized_return_pv: 0.213691
		- _NOTE: you might use a proxy if you have a long sales cycle, in order to have a quicker feedback cycle. So while technically you wouldn't have to include the proxy into this equation, theoretically it could help you identify the CR assumptions between signup->acquisition, to check, for example, that the conversion rate between signup->proxy remains the same. If it doesn't, (e.g. if there are lower 'scores' or less proxy classifications (e.g. you classify high scores as X)) then that might be a sign that you have lower quality. Or if the CR is lower between the proxy and the acquisition, that might be a sign that your scoring model isn't as accurate for this particular channel. Or, the opposite could be true. Either way breaking those down a) makes you think through the process and b) gives you more data points to make decisions._
- **NOTE: CLV & CAC calculations and ratios don't take into consideration, for example, the probability that the customer would have signed up anyway (e.g. they would have found the company via Google search, and signed up, in which case you are paying for a signup that you didn't have to)**

###references:
http://andrewchen.co/when-does-paid-acquisition-work-for-saas-startups/

## inference.R

```R
ab_test.indep <- function(s, o=NULL, ns=NULL, correct=TRUE)
```
- uses R's `test.prop` i.e. chi-square test of `independence` to determine success of a/b tests
	- according to [stats.stackexchange](http://stats.stackexchange.com/questions/96835/chisq-test-in-r-doesnt-produce-same-answer-as-by-hand)
- `s` is `successes`
- `o` is total number of `observations` (i.e. count of trials)
- `ns` is number of `non-successes` (i.e. 'failure') (e.g. if s is clicks, ns, is non-clicks, or if s is female, ns is male)
- `correct`: a logical indicating whether Yates' continuity correction should be applied where possible. (default is `TRUE`)
	- when `correct` is `FALSE`, then it is same as calculating 'by hand' ([source](http://stats.stackexchange.com/questions/96835/chisq-test-in-r-doesnt-produce-same-answer-as-by-hand))

```R
ab_test.gof <- function(s, o=NULL, ns=NULL, correct=TRUE)
```
- same inputs as `ab_test.indep`
- uses R's `test.prop` i.e. chi-square test of `goodness of fit` to determine success of a/b tests
	- according to [stats.stackexchange](http://stats.stackexchange.com/questions/96835/chisq-test-in-r-doesnt-produce-same-answer-as-by-hand)
- uses `prop.test` function, but uses first element of `s` and `o` (or `ns`) as the target proportion. (e.g. first experiment is original and subsequent are variants.)
- `correct`: a logical indicating whether Yates' continuity correction should be applied where possible. (default is `TRUE`)
	- when `correct` is `FALSE`, then it is same as calculating 'by hand' ([source](http://stats.stackexchange.com/questions/96835/chisq-test-in-r-doesnt-produce-same-answer-as-by-hand))

```R
ab_test.binomial <- function(original_successes, original_total, variation_successes, variation_total)
```
- uses R's `binom.test` ([docs](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/binom.test.html))

```R
ab_test.fisher <- function(s, o=NULL, ns=NULL)
```
- same inputs as `ab_test.indep`
- uses R's `fisher.test`
	- `fisher.test` will perform a test analogous to the chi-square test of independence but not the chi-square goodness of fit test. ([source](http://stats.stackexchange.com/questions/96835/chisq-test-in-r-doesnt-produce-same-answer-as-by-hand))

```R
bayes.t.test = function(numeric_vector, n0=1, mu0 = 0,  prior_h1=.5)
```

**Original Source**: Dr. Merlise Clyde: https://github.com/StatsWithR/figures/blob/master/04_bayesian_statistics/week_03/4.3.2_comparing_two_paired_means/Docs/4-3-2-pair-notes.pdf

- `bayes.t.test` is used for comparing two paired means using Bayes Factors.
- the hypotheses of interest in terms of the original parameters and the mean of the differences:
	- no difference: `H1 : μa =μb -> μdiff = 0`
	- means are different: `H2 : μa != μb -> μdiff != 0`
	- mean from sample 1 is larger than mean of sample 2: `H3 : μa >μb -> μdiff > 0`
	- mean from sample 2 is larger than mean of sample 1: `H4 : μa <μb -> μdiff < 0`
- It should be clear that H3 and H4 are included in H2, so that we first need to find the probability of H1 and H2.

```R
z.prop <- function(x1, x2, n1, n2)
```

- `x`'s are 'success'
- `n`'s are total observations (so if you have a contingency table, add both the 'success' and 'failure' cells together)

```R
convert.z.score <- function(z, one.sided=NULL)
```

- converts a `z score` from `z.prop` to a `p-value`.