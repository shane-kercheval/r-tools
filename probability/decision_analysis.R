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

# same value as expected-value
expected_value_conditional <- function(o_tp, o_tn, o_fp, o_fn, b_tp, b_tn, b_fp, b_fn)
{
	qom = quality_of_model(true_pos=o_tp, true_neg=o_tn, false_pos=o_fp, false_neg=o_fn)

	# this equation corresponds to equation in Data Science for Business (Provost, Fawcett, kindle loc 4424)
	# sensitivity == TRUE POSITIVE RATE
	# specificity == TRUE NEGATIVE RATE
	return ((qom$actual_pos_prob * (qom$sensitivity * b_tp + qom$false_negative_rate * b_fn)) +
			(qom$actual_neg_prob * (qom$specificity * b_tn + qom$false_positive_rate * b_fp)))

}

bayes <- function(p_b, p_a_given_b, p_a_given_nb)
{
	p_b_given_a = (p_b * p_a_given_b) / ((p_b * p_a_given_b) + ((1 - p_b) * p_a_given_nb))
	return (p_b_given_a)
}
