library(dplyr)
library(arules)
library(arulesSequences)
library(stringr)
source('./apriori.R', chdir=TRUE)

#######################################################################################################################################
# conducts an apriori sequence analysis
# returns `sequencerules (attr(,"package") "arulesSequences")` object
# can do: as(rules)
#######################################################################################################################################
apriori_sequence_analysis <- function(apriori_dataset, support=0.5, confidence=0.5, memsize = 2000, verbose = FALSE)
{
	# need to write out to a csv so we can use `read_baskets` function
	write.table(apriori_dataset, file = 'input_file.csv', sep = ",", quote=FALSE, col.names=FALSE, row.names=FALSE, append = FALSE)
	basket_dataset <- read_baskets(con = 'input_file.csv', sep = ',', info = c('sequenceID','eventID','SIZE'))
	file.remove('input_file.csv')

	c_rules<- cspade(basket_dataset, parameter = list(support = support), control = list(memsize = memsize, verbose = verbose))
	rules_sequential <- ruleInduction(c_rules, confidence = confidence)#, control = list(verbose = TRUE))

	return (rules_sequential)
}
#######################################################################################################################################
# transforms a dataframe that contains one event per row into a apriori (sequence) dataset
# takes a dataframe that has A) an id column (e.g. customer id) B) an event column (e.g. page url/path, signup event, paid event)
# and C) if data needs to be ordered, an order column
# column_to_order_by should be a string of the column name that needs to be (secondarily) ordered (e.g. datetime of event).
# 	if column_to_order_by is not null, the data will be ordered by 1) `id_column_name` then 2) `column_to_order_by`
# *IF `column_to_order_by` IS NOT SET, THE DATASET SHOULD ALREADY BE ORDERED BY ID COLUMN AND ORDER OF EVENT SEQUENCE (E.G. datetime)
#######################################################################################################################################
single_event_sequence_dataset <- function(dataset, id_column_name, column_to_order_by = NULL)
{
	dataset <- data.frame(dataset) # need to do this so that subsetting by column name is consistent (i.e. gives vector, not data.frame/list)
	dataset <- helper_order(dataset = dataset, id_column_name = id_column_name, column_to_order_by = column_to_order_by)
	apriori_dataset <- helper_add_sequenced_data(dataset = dataset, id_column_name = id_column_name, column_to_order_by = column_to_order_by)
	apriori_dataset <- as.data.frame(sapply(apriori_dataset[c('sequenceID', 'eventID', 'SIZE', 'items')],as.factor))

	return (apriori_dataset)
}

#######################################################################################################################################
# converts `rules_sequential` object to dataframe, adding `antecedent` and `consequent` columns
#######################################################################################################################################
as_dataframe <- function(rules_sequential, sort = TRUE, sort_by = 'lift', number_of_unique_ids = NULL, is_rules_non_sequential = FALSE, round_to = NULL)
{
	rules_sequential_df <- as(rules_sequential, 'data.frame')
	
	if(is_rules_non_sequential) {

		rules_sequential_df <- rules_sequential_df %>% dplyr::rename(rule = rules)
	}	
	
	rules_sequential_df <- rules_sequential_df %>% mutate(rule = as.character(rule))
	
	rule_groups <- as.data.frame(str_match(rules_sequential_df$rule, '(.*) => (.*)'), stringsAsFactors = FALSE)
	names(rule_groups) <- c('rule', 'antecedent', 'consequent')

	rules_sequential_df <- inner_join(rules_sequential_df, rule_groups, by = 'rule') %>%
		dplyr::mutate(number_of_terms = str_count(string = antecedent, pattern = ',') + 1) %>%
		dplyr::select(rule, antecedent, consequent, support, confidence, lift, number_of_terms)

	if(sort) {

		rules_sequential_df <- rules_sequential_df %>% dplyr::arrange_(paste0("desc(", sort_by, ")"))
	}
	
	if(!is.null(number_of_unique_ids)) {

		rules_sequential_df <- rules_sequential_df %>% dplyr::mutate(number_of_ids_having_rule = number_of_unique_ids * support)
	}

	if(!is.null(round_to)) {
		rules_sequential_df <- rules_sequential_df %>%
									dplyr::mutate(	support = round(support, round_to),
													lift = round(lift, round_to),
													confidence = round(confidence, round_to))
	}

	return (rules_sequential_df)
}

#######################################################################################################################################
# takes a dataframe returned by `as_dataframe` function and returns subset of dataframe that matches regex expressions for antecedent and/or consequent
#######################################################################################################################################
subset_sequence <- function(rules_sequential_df, antecedent_regex=NULL, consequent_regex=NULL)
{
	rules_subset <- NULL

	if (!is.null(antecedent_regex) && !is.null(consequent_regex)) # if antecedent/consequent both not null (otherwise only one of them should be)
	{
		rules_subset <- subset(rules_sequential_df, grepl(antecedent_regex, rules_sequential_df$antecedent) | grepl(consequent_regex, rules_sequential_df$consequent))
	}
	else if (!is.null(antecedent_regex))
	{
		rules_subset <- subset(rules_sequential_df, grepl(antecedent_regex, rules_sequential_df$antecedent))
	}
	else if (!is.null(consequent_regex))
	{
		rules_subset <- subset(rules_sequential_df, grepl(consequent_regex, rules_sequential_df$consequent))
	}
	# else return NULL

	return (rules_subset)
}

#######################################################################################################################################
# helper method to add sequence data to dataset. Expects an ordered dataset
#######################################################################################################################################
helper_add_sequenced_data <- function(dataset, id_column_name, column_to_order_by)
{
	apriori_dataset <- dataset
	apriori_dataset$sequenceID <- as.numeric(factor(apriori_dataset[, id_column_name]))
	apriori_dataset$eventID <- ave(apriori_dataset$sequenceID, apriori_dataset$sequenceID, FUN = seq_along)
	apriori_dataset$SIZE <- 1
	names(apriori_dataset)[names(apriori_dataset) == 'event'] <- 'items'
	#	apriori_dataset = helper_order(dataset=apriori_dataset, id_column_name=id_column_name, column_to_order_by=column_to_order_by)

	return (apriori_dataset)
}
