#######################################################################################################################################
# conducts an apriori sequence analysis
# returns `sequencerules (attr(,"package") "arulesSequences")` object
# can do: as(rules)
#######################################################################################################################################
apriori_sequence_analysis <- function(apriori_dataset)
{
	# need to write out to a csv so we can use `read_baskets` function
	write.table(apriori_dataset, file='input_file.csv', sep=",", quote=FALSE, col.names=FALSE, row.names=FALSE)
	library(arulesSequences)
	fuck <- read_baskets(con = 'input_file.csv', sep = ',', info = c('sequenceID','eventID','SIZE'))
	c_rules<- cspade(fuck, parameter = list(support = 0.55), control = list(verbose = TRUE))
	
	rules <- ruleInduction(c_rules, confidence = .5)#, control = list(verbose = TRUE))

	return (rules)
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
	dataset = order_helper(dataset=dataset, id_column_name=id_column_name, order_by=order_by)
	apriori_dataset = helper_add_sequenced_data(dataset=dataset, id_column_name=id_column_name, order_by=order_by)
	apriori_dataset = as.data.frame(sapply(apriori_dataset[c('sequenceID', 'eventID', 'SIZE', 'items')],as.factor))

	return (apriori_dataset)
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
	#	apriori_dataset = order_helper(dataset=apriori_dataset, id_column_name=id_column_name, order_by=order_by)

	return (apriori_dataset)
}

#######################################################################################################################################
# helper method to order data based on id column (e.g. customer id) and sequence column (e.g. datetime)
#######################################################################################################################################
helper_order <- function(dataset, id_column_name, order_by)
{
	if(!is.null(order_by))
	{
		dataset = dataset[order(dataset[, id_column_name], dataset[, order_by]), ]
	}

	return (dataset)
}
