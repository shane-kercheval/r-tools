library(arules)

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
