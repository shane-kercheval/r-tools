# estimate the probability of hypothesis (h) given evidence (e) p_h_given_e
# P(E)=P(E|H)*P(H)  +  P(E|not H)*P(not H)|
bayes_simple <- function(p_e, p_h, p_e_given_h)
{
	p_h_given_e = (p_h * p_e_given_h) / p_e
	return (p_h_given_e)
}

bayes_explicit <- function(p_h, p_e_given_h, p_e_given_nh)
{
	p_h_given_e = (p_h * p_e_given_h) / prob_e(p_h=p_h, p_e_given_h=p_e_given_h, p_e_given_nh=p_e_given_nh)
	return (p_h_given_e)
}

bayes_confusion <- function(conf_list)
{
	return (bayes_prevalence(prevalence=prevalence(conf_list), sensitivity=sensitivity(conf_list), specificity=specificity(conf_list)))
}

bayes_prevalence <- function(prevalence, sensitivity, specificity=NULL, false_positive_rate=NULL)
{
	if(!is.null(specificity))
	{
		false_positive_rate = 1 - specificity
	}
	else if(is.null(false_positive_rate))
	{
		stop("`specificity` and `false_positive_rate` can't both be NULL")
	}
	return( (prevalence * sensitivity) /  ((prevalence * sensitivity) + ((1-prevalence) * false_positive_rate)) )
}

prob_e <- function(p_h, p_e_given_h, p_e_given_nh)
{	# P(B)=P(B|A)P(A)+P(B|not A)P(not A)
	p_e = (p_e_given_h * p_h) + (p_e_given_nh * (1 - p_h))
	return (p_e)
}
