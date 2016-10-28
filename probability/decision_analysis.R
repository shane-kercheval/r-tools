expected_value <- function(probs=NULL, n_occur=NULL, benefits)
{
	if(is.null(probs))
	{
		if(is.null(n_occur))
		{
			stop('either obs or probs must be supplied')
		}
		probs = n_occur / sum(n_occur)
	}
	return (sum(probs * benefits))
}
