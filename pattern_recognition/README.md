# pattern recognition

## apriori

### sequential analysis
- `apriori_sequence_analysis` returns rules object (`[sequencerules] (attr(,"package") "arulesSequences")` object) based on sequential data
  - link to [arulesSequences pdf](https://cran.r-project.org/web/packages/arulesSequences/arulesSequences.pdf)
- [This](https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Sequence_Mining/SPADE) is an good example of the type of problem these functions can help analyze.
    - these functions use the same algorithms and R functions in the example.

The following types of commands can be used with the `rules` object returned from `apriori_sequence_analysis` (for example):

```c
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
