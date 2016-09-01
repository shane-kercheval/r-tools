#######################################################################################################################################
# scales data
#######################################################################################################################################
get_scaled_dataset <- function(data_frame, named_column)
{
	return (as.data.frame(lapply(data_frame[, -grep(named_column, colnames(data_frame))], scale)))
}

#######################################################################################################################################
# returns the numeric columns, as well as the (optional) column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_numeric_dataset <- function(data_frame, named_column=NULL)
{
	numeric_columns = sapply(data_frame, is.numeric)
	new_dataset = data_frame[numeric_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

#######################################################################################################################################
# returns the logical columns, as well as the (optional) column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = sapply(data_frame, is.logical)
	new_dataset = data_frame[logical_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

#######################################################################################################################################
# returns the numeric and logical columns, as well as the column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_numeric_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = sapply(data_frame, is.logical)
	new_dataset = get_numeric_dataset(data_frame=data_frame, named_column=named_column)
	new_dataset = cbind(new_dataset, data_frame[logical_columns])
	return (new_dataset)
}

vector_match_regex <- function(the_vector, pattern_to_find, pattern_to_extract=NULL, substitue_find_with=NULL)
{
	if(is.null(pattern_to_extract))
	{
		pattern_to_extract = pattern_to_find
	}
	indexes_of_match = grep(pattern = pattern_to_find, the_vector)

	matches = sapply(the_vector[indexes_of_match], function(x){regmatches(x, regexpr(pattern_to_extract, x)) })
	if(!is.null(substitue_find_with))
	{
		matches = sub(pattern = pattern_to_find, substitue_find_with, matches)
	}

	return (list(indexes_of_match, matches))
}

#######################################################################################################################################
# returns a list of matched items by regex as well as the corresponding indexes that match 
# can pass in a substitute string if the matched items should be substituted with a string 
# returns a list of two items 1) `indexes_of
#######################################################################################################################################


#######################################################################################################################################
# returns a lsit of  
# returns a list of two items 1) `indexes_of
#######################################################################################################################################
