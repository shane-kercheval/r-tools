stopif <- function(condition, message, call=FALSE)
{
	if(condition)
	{
		stop(message, call.=call)
	}
}

is.nullNaEmpty <- function(...)
{
	# for each item in `...`, if the item is a vector/list (i.e. length<1), do recursive call for each item, otherwise, test to see if the individual item is NULL, NA, or empty.
	return (any(map_lgl(list(...), ~
		{
			if(length(.) > 1)
			{
				return (any(map_lgl(., ~is.nullNaEmpty(.))))
			}
			# else
			return (is.null(.) || is.na(.) || as.character(str_trim(.)) == '')
		})))
}

lazy_load <- function(path, object, create_data, dataset_function)
{
	if(create_data)
	{
		object = dataset_function()
		save(object, file=path)
		return (object)
	}
	# otherwise we don't want to create data (i.e. from the function), so we either just want to return it if it exists, or we want to pull it from the file.
	object_name = deparse(substitute(object))
	if(exists(object_name) && !is.null(object))
	{
		return (object)
	}
	else
	{
		env = new.env()
		object = load(path, env)[1]
		return (env[[object]])
	}
}
