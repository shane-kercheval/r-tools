library('purrr')

get_scaled_dataset <- function(data_frame, named_column)
{
	return (as.data.frame(lapply(data_frame[, -grep(named_column, colnames(data_frame))], scale)))
}

get_numeric_dataset <- function(data_frame, named_column=NULL)
{
	numeric_columns = map_lgl(data_frame, is.numeric)
	new_dataset = data_frame[numeric_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

get_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = map_lgl(data_frame, is.logical)
	new_dataset = data_frame[logical_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

get_numeric_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = map_lgl(data_frame, is.logical)
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

	matches = map_chr(the_vector[indexes_of_match], function(x){regmatches(x, regexpr(pattern_to_extract, x)) })
	if(!is.null(substitue_find_with))
	{
		matches = sub(pattern = pattern_to_find, substitue_find_with, matches)
	}

	return (list(indexes_of_match, matches))
}

normalize <- function(x)
{
	return ((x - min(x)) / (max(x) - min(x)))
}
