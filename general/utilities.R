library('purrr')

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
			return (is.null(.) || is.na(.) || . == '')
        })))
}
