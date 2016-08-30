# pattern recognition

## apriori

### sequential
- `apriori_sequence_analysis` returns rules based on sequential data
- this is an good [example](https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Sequence_Mining/SPADE) of the type of problem these functions can help analyze.
    - these functions use the same algorithms and R functions in the example.

can do the following types of commands (for example):

```
# summary of sequential rules
summary(rules)

# look at the first three rules
inspect(rules[1:3])

# sorting grocery rules by lift
inspect(sort(rules, by = "lift")[1:5])

# finding subsets of rules containing any 'signup_event' items
signup_rules <- subset(rules, items %in% 'signup_event')
inspect(signup_rules)

# writing the rules to a CSV file
write(rules, file = 'rules.csv', sep = ',', quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
rules_df <- as(rules, 'data.frame')
str(rules_df)
```
