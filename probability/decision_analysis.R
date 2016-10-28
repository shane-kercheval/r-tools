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

bayes <- function(p_b, p_a_given_b, p_a_given_nb)
{
	p_b_given_a = (p_b * p_a_given_b) / ((p_b * p_a_given_b) + ((1 - p_b) * p_a_given_nb))
	return (p_b_given_a)
}
