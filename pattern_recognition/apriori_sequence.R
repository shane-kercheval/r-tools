library(arules)
library(arulesSequences)
library(stringr)
source('./apriori.R', chdir=TRUE)

#######################################################################################################################################
# conducts an apriori sequence analysis
# returns `sequencerules (attr(,"package") "arulesSequences")` object
# can do: as(rules)
#######################################################################################################################################
apriori_sequence_analysis <- function(apriori_dataset, support=0.5, confidence=0.5)
{
	# need to write out to a csv so we can use `read_baskets` function
	write.table(apriori_dataset, file='input_file.csv', sep=",", quote=FALSE, col.names=FALSE, row.names=FALSE, append=FALSE)
	basket_dataset <- read_baskets(con = 'input_file.csv', sep = ',', info = c('sequenceID','eventID','SIZE'))
	file.remove('input_file.csv')

	c_rules<- cspade(basket_dataset, parameter = list(support = support), control = list(verbose = FALSE))
	rules_sequential <- ruleInduction(c_rules, confidence = confidence)#, control = list(verbose = TRUE))

	return (rules_sequential)
}
#######################################################################################################################################
# transforms a dataframe that contains one event per row into a apriori (sequence) dataset
# takes a dataframe that has A) an id column (e.g. customer id) B) an event column (e.g. page url/path, signup event, paid event)
# and C) if data needs to be ordered, an order column
# order_by should be a string of the column name that needs to be (secondarily) ordered (e.g. datetime of event).
# 	if order_by is not null, the data will be ordered by 1) `id_column_name` then 2) `order_by`
# *IF `order_by` IS NOT SET, THE DATASET SHOULD ALREADY BE ORDERED BY ID COLUMN AND ORDER OF EVENT SEQUENCE (E.G. datetime)
#######################################################################################################################################
single_event_sequence_dataset <- function(dataset, id_column_name, order_by=NULL)
{
	dataset = helper_order(dataset=dataset, id_column_name=id_column_name, order_by=order_by)
	apriori_dataset = helper_add_sequenced_data(dataset=dataset, id_column_name=id_column_name, order_by=order_by)
	apriori_dataset = as.data.frame(sapply(apriori_dataset[c('sequenceID', 'eventID', 'SIZE', 'items')],as.factor))

	return (apriori_dataset)
}

#######################################################################################################################################
# converts `rules_sequential` object to dataframe, adding `left hand side` (`lhs`) and `right hand side` (`rhs`) columns
#######################################################################################################################################
as_dataframe <- function(rules_sequential, sort=TRUE, sort_by='lift')
{
	if(sort)
	{
		rules_sequential = sort(rules_sequential, by = sort_by)
	}

	rules_sequential_df = as(rules_sequential, 'data.frame')

	rule_groups = as.data.frame(str_match(rules_sequential_df$rule, '(.*) => (.*)'))

	rules_sequential_df$lhs = rule_groups$V2
	rules_sequential_df$rhs = rule_groups$V3


	return (rules_sequential_df[c('rule', 'lhs', 'rhs', 'support', 'confidence', 'lift')]) # return custom order of dataframe
}

#######################################################################################################################################
# takes a dataframe returned by `as_dataframe` function and returns subset of dataframe that matches regex expressions for lhs and/or rhs
#######################################################################################################################################
subset_sequence <- function(rules_sequential_df, lhs_regex=NULL, rhs_regex=NULL)
{
	rules_subset = NULL

	if (!is.null(lhs_regex) && !is.null(rhs_regex)) # if lhs/rhs both not null (otherwise only one of them should be)
	{
		rules_subset = subset(rules_sequential_df, grepl(lhs_regex, rules_sequential_df$lhs) | grepl(rhs_regex, rules_sequential_df$rhs))
	}
	else if (!is.null(lhs_regex))
	{
		rules_subset = subset(rules_sequential_df, grepl(lhs_regex, rules_sequential_df$lhs))
	}
	else if (!is.null(rhs_regex))
	{
		rules_subset = subset(rules_sequential_df, grepl(rhs_regex, rules_sequential_df$rhs))
	}
	# else return NULL

	return (rules_subset)
}

#######################################################################################################################################
# helper method to add sequence data to dataset. Expects an ordered dataset
#######################################################################################################################################
helper_add_sequenced_data <- function(dataset, id_column_name, order_by)
{
	apriori_dataset = dataset
	apriori_dataset$sequenceID = as.numeric(factor(apriori_dataset[, id_column_name]))
	apriori_dataset$eventID = ave(apriori_dataset$sequenceID, apriori_dataset$sequenceID, FUN = seq_along)
	apriori_dataset$SIZE = 1
	names(apriori_dataset)[names(apriori_dataset) == 'event'] <- 'items'
	#	apriori_dataset = helper_order(dataset=apriori_dataset, id_column_name=id_column_name, order_by=order_by)

	return (apriori_dataset)
}
