# General Helper Functions

## ab_tests.R

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

## basic_stats.R
- general stats helper functions

```R
create_percentile_matrix <- function(list_of_datasets, row_names, percentiles=c(0, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 1), round_by=1)
```
- note that if some data has subsets (e.g. comparing all users to a subset of users) for the subset data, you only want to pass in the
- subset (as opposed to relying on NA data). The reason is that, for example, the '% data above/below outlier threshold' needs to know
- the total lenght of only the subset, in order to accurately calculate

```R
tabulate_logical <- function(logical_data_frame)
```
- takes a dataframe that contains logical columns and returns a dataframe that contains percent information for each

```R
explore_numeric <- function(numeric_vector)
```
- takes a numeric vector and prints out basic/exploratory stats

## correlations.R

```R
get_correlations <- function(data_frame, corr_threshold=0.7, p_value_threshold=0.1, type='pearson')
```
- takes a data-frame, subsets the numeric columns, and returns a matrix of correlations with `NA`s where
	- absolute value of correlation value is <= `corr_threshold`
	- corresponding p_value is <= `p_value_threshold`
- can pass in correalation `type`
	- specifies the type of correlations to compute. `Spearman` correlations are the `Pearson` (default) linear correlations computed on the ranks of non-missing elements, using midranks for ties.
	- If you want to explore your data it is best to compute both, since the relation between the `Spearman (S)` and `Pearson (P)` correlations will give some information. Briefly, `S` is computed on ranks and so depicts monotonic relationships while `P` is on true values and depicts linear relationships. [stats.stackexchange.com](http://stats.stackexchange.com/questions/8071/how-to-choose-between-pearson-and-spearman-correlation)
- so if `threshold` is 0.9, the resulting matrix will only show correlations >=0.9 and <= -0.9. All other values will show `NA`

```R
plot_correlations <- function(data_frame)
```
- plots correlations on x/y axis using colors/sizes to visualize
- example:

![example_plot_correlations](../readme/example_plot_correlations.png)

## dates.R

```R
add_date_columns <- function(data_frame, date_column)
```
- takes a dataframe with date column and adds a column for `year`, `month`, `week number`, `weekday`, `day of year`, and `day of month`
- `date_column` is string name of date column

## model_measurements.R
- provides a list of functions that help assess the quality of various models
	- e.g. https://en.wikipedia.org/wiki/Sensitivity_and_specificity

```R
sensitivity <- function(true_positives, total_actual_positives)
```
- a.k.a `true positive rate`
- number of positives predicted correctly (true positives) out of total number of positives

```R
specificity <- function(true_negatives, total_actual_negatives)
```
- a.k.a `true negative rate`
- number of negatives predicted correctly (true negatives) out of total number of negatives

```R
false_negative_rate <- function(false_negatives, total_actual_positives)
```
- number of negatives predicted that were actually positive, out of total number of positives

```R
false_positive_rate <- function(false_positives, total_actual_negatives)
```
- number of positives predicted that were actually negative out of total number of negatives

```R
accuracy <- function(true_negatives, true_positives, total_observations)
```
- number of correct predictions out of total number of observations

```R
error_rate <- function(false_positives, false_negatives, total_observations)
```
- number of incorrect predictions out of total number of observations
- however, because not all errors are treated the same (e.g. false negative for cancer detection is worse than false positive), this number shouldn't be looked at alone.

```R
positive_predictive_value <- function(true_positives, false_positives)
```
- number of positives predicted correctly out of total number positive predictions

```R
negative_predictive_value <- function(true_negatives, false_negatives)
```
- number of negatives predicted correctly out of total number of negative predictions

```R
quality_of_model_from_confusion <- function(confusion_matrix)
```
- takes a confusion matrix with predictions as columns and actuals as rows (negatives first, positives second) and returns a `quality_of_model` list
- confusion matrix in following format

|                  | Predicted Negative | Predicted Positive |
| ---------------- | ------------------ | ------------------ |
| Actual Negative  | True Negative      | False Positive     |
| Actual Positive  | False Negative     | True Positive      |

- a `positive` can be thought of as a `detection`, while a `negative` is a `non-detection`
	- e.g. logistic regression predicting (detecting) fraud. A positive would be a predicted or actual fraud occurance. So a `false positive` would be a case when we *predict* a positive (i.e. fraud), but the *actual* case was not fraud.

```R
quality_of_model <- function(true_positives, true_negatives, false_positives, false_negatives)
```
- returns a dataframe that contains the following columns
	- `accuracy`
	- `error_rate`
	- `positive_predictive_value`
	- `negative_predictive_value`
	- `false_positive_rate`
	- `false_negative_rate`
	- `sensitivity`
	- `specificity`
	- `actual_pos_prob` (total actual positives / total observations)
	- `actual_neg_prob` (total actual negatives / total observations)
	- `total_observations`

```R
logistics_regression_coefficients <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
```

```R
logistic_response_function <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
```

```R
odds <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
```

```R
logit <- function(b, b1, x1, b2=0, x2=0, b3=0, x3=0, b4=0, x4=0, b5=0, x5=0)
```

## modification.R

```R
get_scaled_dataset <- function(data_frame, named_column)
```
- scales data to `z-scores` so they can be compared against each other (e.g. in clustering)
- `named_column` is name of column that is row_identifier that tells the function to ignore that column for scaling

```R
get_numeric_dataset <- function(data_frame, named_column=NULL)
```
- returns the numeric columns, as well as the (optional) column `named_column` (most likely a row unique identifier)

```R
get_logical_dataset <- function(data_frame, named_column=NULL)
```
- returns the logical columns, as well as the (optional) column `named_column` (most likely a row unique identifier)

```R
get_numeric_logical_dataset <- function(data_frame, named_column=NULL)
```
- returns the numeric and logical columns, as well as the column `named_column` (most likely a row unique identifier)

```R
vector_match_regex <- function(the_vector, pattern_to_find, pattern_to_extract=NULL, substitue_find_with=NULL)
```
- returns a list of matched items by regex as well as the corresponding indexes that match
- can pass in a substitute string if the matched items from `pattern_to_find` should be substituted with a string
- sometimes the `pattern_to_extract` will be different from the `pattern_to_find`, so set variables accordingly; if `pattern_to_extract` is not set, it will default to `pattern_to_find`
- returns a list of two items 1) `indexes_of_match` and `matches`

## outliers.R

```R
calculate_outlier_thresholds <- function(vect)
```
- calculates outlier thresholds based on Quantil 1,3 +- 1.5 IQR
- returns a named vector with `lower` `upper` labels

```R
remove_outliers <- function(vect)
```
- changes any outliers in vector to NA

## utlities

```R
stopif <- function(condition, message, call=FALSE)
```
- calls base `stop` function if `condition` is true with `message`
- `call` is passed into `call.` variable of base `stop` function

```R
is.nullNaEmpty <- function(...)
```
- tests each element in `...` (which can be vector, list, single element, or combination) for NULL, NA, or Empty (i.e. ==\'\')

```R
lazy_load <- function(path, object, create_data, dataset_function)
```
- this method is used for creating/caching large datasets via manipulation methods (e.g. dplyr), that take a substantial amount of time to run
- the method either creates the dataset from scratch (via `dataset_function` function), loads the stored dataset from an .Rda file, or returns the existing dataset if it is already populated.
- `path` is path to Rda file
- `object` is the object you want to populate
- `create_data` is a flag to specify whether or not to create the dataset specified by `dataset_function`
- `dataset_function` should return a dataset
- if `create_data` is `TRUE`, then the `dataset_function` will be executed
- otherwise, if the `object` exists and is not null, it will simply be returned.
- if neither condition is true, then the dataset will be loaded from the Rda file specified from `path`
