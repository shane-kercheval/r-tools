library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_apriori.R")

test_that("pattern_recognition: apriori-helper_order", {
	# test that we are ordering data correctly
	unordered_data = read.csv('./data/apriori_sequence_dataset_start.csv')
	ordered_data = helper_order(dataset=unordered_data, id_column_name='customer_id', order_by='event_sequence')
	comparison_data = read.csv('./data/apriori_sequence_dataset_end.csv')
	expect_true(all(ordered_data == comparison_data))
})

test_that("pattern_recognition: single_event_sequence_dataset", {
	# test that we are ordering data correctly
	unordered_data = read.csv('./data/apriori_sequence_dataset_start.csv')
	ordered_data = helper_order(dataset=unordered_data, id_column_name='customer_id', order_by='event_sequence')
	sequenced_dataset = helper_add_sequenced_data(dataset=ordered_data, id_column_name='customer_id', order_by='event_sequence')

	#check that sequenced_dataset matches columns from ordered_data for shared/old columns
	expect_true(all(sequenced_dataset$customer_id == ordered_data$customer_id))
	expect_true(all(sequenced_dataset$items == ordered_data$event))
	expect_true(all(sequenced_dataset$event_sequence == ordered_data$event_sequence))

	#because numbers are used for customer_id and event_sequence, these columns should match sequenceID and eventID respectively
	expect_true(all(sequenced_dataset$sequenceID == ordered_data$customer_id))
	expect_true(all(sequenced_dataset$eventID == ordered_data$event_sequence))
	expect_true(all(sequenced_dataset$SIZE == 1))

	# main test
	apriori_dataset = single_event_sequence_dataset(dataset=unordered_data, id_column_name='customer_id', order_by='event_sequence')
	expect_true(all(colnames(apriori_dataset) == c('sequenceID', 'eventID', 'SIZE', 'items')))

	expect_true(all(apriori_dataset$sequenceID == ordered_data$customer_id))
	expect_true(all(apriori_dataset$eventID == ordered_data$event_sequence))
	expect_true(all(apriori_dataset$SIZE == 1))
	expect_true(all(apriori_dataset$items == ordered_data$event))
})

test_that("pattern_recognition: apriori_sequence_analysis", {
	sink(file='./data/apriori_results.txt', append=FALSE)

	unordered_data = read.csv('./data/apriori_sequence_dataset_start.csv')

	# apriori functions
	apriori_dataset = single_event_sequence_dataset(dataset=unordered_data, id_column_name='customer_id', order_by='event_sequence')
	rules = apriori_sequence_analysis(apriori_dataset=apriori_dataset, support=0.55)

	# summary of sequential rules
	summary(rules)

	expect_that(length(rules), equals(4))

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
	rules_df = as_dataframe(rules) # custom method
	str(rules_df)
	expect_that(nrow(rules_df), equals(4))
	
	# finding subsets of rules containing any `b` items (e.g. can replace `b` with e.g. `signup_event` if that is an item in your dataset)
	b_rules = rules_df[grep("b", rules_df$rule),]
	expect_that(nrow(b_rules), equals(3))
	
	# writing the rules to a CSV file
	write(rules, file = './data/rules.csv', sep = ';', quote = TRUE, row.names = FALSE)
	file.remove('./data/rules.csv')
	
	comparison_data = read.csv('./data/rules_original.csv')
	expect_that(colnames(rules_df), equals(c('rule', 'lhs', 'rhs', 'support', 'confidence', 'lift')))
	expect_true(all(rules_df$rule == comparison_data$rule))
	expect_true(all(rules_df$lhs == comparison_data$lhs))
	expect_true(all(rules_df$rhs == comparison_data$rhs))
	expect_true(all(rules_df$support == comparison_data$support))
	expect_true(all(rules_df$confidence == comparison_data$confidence))
	expect_true(all(rules_df$lift == comparison_data$lift))

	sink()
})

test_that("pattern_recognition: apriori_sequence_analysis-extract_rules", {
	original_data = read.csv('./data/rules_original.csv')
	
	lhs_regex='a'
	rhs_regex='c'
	rules_subset = subset_sequence(rules_sequential_df=original_data, lhs_regex=lhs_regex)
	#write.csv(rules_subset, './data/test_data_rules_subset_a.csv' ,row.names = FALSE)
	subset_a = read.csv('./data/test_data_rules_subset_a.csv', stringsAsFactors = FALSE)
	expect_true(all(subset_a == rules_subset))
	
	rules_subset = subset_sequence(rules_sequential_df=original_data, rhs_regex=rhs_regex)
	#write.csv(rules_subset, './data/test_data_rules_subset_c.csv' ,row.names = FALSE)
	subset_c = read.csv('./data/test_data_rules_subset_c.csv', stringsAsFactors = FALSE)
	expect_true(all(subset_c == rules_subset))
	
	rules_subset = subset_sequence(rules_sequential_df=original_data, lhs_regex=lhs_regex, rhs_regex=rhs_regex)
	expect_true(all(original_data == rules_subset)) # this should return all rows, in order, so should equal original_data
})

