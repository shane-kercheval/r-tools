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
	unordered_data = read.csv('./data/apriori_sequence_dataset_start.csv')
	apriori_dataset = single_event_sequence_dataset(dataset=unordered_data, id_column_name='customer_id', order_by='event_sequence')

	rules = apriori_sequence_analysis(apriori_dataset=apriori_dataset)

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
})
