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

add_matrix_totals <- function(contingency_table)
{
	# add row totals
	contingency_table = cbind(contingency_table, rowSums(contingency_table))
	colnames(contingency_table) = c(colnames(contingency_table)[1:(length(colnames(contingency_table))-1)], 'TOTALS')
	# add column totals
	contingency_table = rbind(contingency_table, colSums(contingency_table))
	rownames(contingency_table) = c(rownames(contingency_table)[1:(length(rownames(contingency_table))-1)], 'TOTALS')

	return (contingency_table)
}

add_dummy_columns <- function(data_frame, column_name, sort_levels=FALSE, use_levels=FALSE, custom_levels = NULL)
{
	data_frame <- as.data.frame(data_frame) # if it is a tibble, at times columns are class matrix
	levels <- unique(as.character(data_frame[, column_name]))
	if(sort_levels)
	{
		levels <- sort(levels)
	}
	if(use_levels)
	{
		levels <- levels(data_frame[, column_name])
	}
	if(!is.null(custom_levels))
	{
		levels <- custom_levels
	}
	for(level in levels[1:(length(levels)-1)])
	{
		data_frame[paste0(column_name, "_dum_", level)] <- ifelse(data_frame[,column_name] == level, 1, 0)
	}

	return(data_frame)
}