# pattern recognition

## apriori

### sequential analysis
- `apriori_sequence_analysis` returns rules object (`[sequencerules] (attr(,"package") "arulesSequences")` object) based on sequential data
  - link to [arulesSequences pdf](https://cran.r-project.org/web/packages/arulesSequences/arulesSequences.pdf)
- [This](https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Sequence_Mining/SPADE) is an good example of the type of problem these functions can help analyze.
    - these functions use the same algorithms and R functions in the example.


```R
apriori_sequence_analysis <- function(apriori_dataset, support=0.5, confidence=0.5)
```
- conducts an apriori sequence analysis
- returns `sequencerules (attr(,"package") "arulesSequences")` object
- can do: as(rules)

```R
single_event_sequence_dataset <- function(dataset, id_column_name, order_by=NULL)
```
- transforms a dataframe that contains one event per row into a apriori (sequence) dataset
- takes a dataframe that has A) an id column (e.g. customer id) B) an event column (e.g. page url/path, signup event, paid event)
- and C) if data needs to be ordered, an order column
- order_by should be a string of the column name that needs to be (secondarily) ordered (e.g. datetime of event).
- 	if order_by is not null, the data will be ordered by 1) `id_column_name` then 2) `order_by`
- **IF** `order_by` IS NOT SET, THE DATASET SHOULD ALREADY BE ORDERED BY ID COLUMN AND ORDER OF EVENT SEQUENCE (E.G. datetime)

```R
as_dataframe <- function(rules_sequential, sort=TRUE, sort_by='lift')
```
- converts `sequencerules` object to dataframe, adding `left hand side` (`lhs`) and `right hand side` (`rhs`) columns

```R
subset_sequence <- function(rules_sequential_df, lhs_regex=NULL, rhs_regex=NULL)
```
- takes a dataframe returned by `as_dataframe` function and returns subset of dataframe that matches regex expressions for lhs and/or rhs

```R
helper_add_sequenced_data <- function(dataset, id_column_name, order_by)
```
- helper method to add sequence data to dataset. Expects an ordered dataset

```R
helper_order <- function(dataset, id_column_name, order_by)
```
- helper method to order data based on id column (e.g. customer id) and sequence column (e.g. datetime)


The following types of commands can be used with the `rules` object returned from `apriori_sequence_analysis` (for example):

```R
# summary of sequential rules
summary(rules)

# look at rules
inspect(rules)

# look at the first three rules
inspect(rules[1:3])

# sorting grocery rules by lift
inspect(sort(rules, by = "lift", decreasing=FALSE)[1:3])

# inspect start of data
inspect(head(rules, n=2)) # If only the top n associations are needed then head using by performs this faster than calling sort and then head since it does it without copying and rearranging all the data. tail works in the same way.

# inspect end of data
inspect(tail(rules, n=2))

# converting the rule set to a data frame
rules_df = as(rules, 'data.frame')
str(rules_df)

# finding subsets of rules containing any `b` items (e.g. can replace `b` with e.g. `signup_event` if that is an item in your dataset)
b_rules = rules_df[grep("b", rules_df$rule),]

# writing the rules to a CSV file
write(rules, file = 'rules.csv', sep = ';', quote = TRUE, row.names = FALSE)
```
