#######################################################################################################################################
# helper method to order data based on id column (e.g. customer id) and sequence column (e.g. datetime)
#######################################################################################################################################
helper_order <- function(dataset, id_column_name, column_to_order_by)
{
	if(!is.null(column_to_order_by))
	{
		dataset = dataset[order(dataset[, id_column_name], dataset[, column_to_order_by]), ]
	}

	return (dataset)
}
