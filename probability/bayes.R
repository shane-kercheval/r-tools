bayes_explicit <- function(p_h, p_e_given_h, p_e_given_nh)
{
	p_h_given_e = (p_h * p_e_given_h) / prob_e(p_h=p_h, p_e_given_h=p_e_given_h, p_e_given_nh=p_e_given_nh)
	return (p_h_given_e)
}

prob_e <- function(p_h, p_e_given_h, p_e_given_nh)
{	# P(B)=P(B|A)P(A)+P(B|not A)P(not A)
	p_e = (p_e_given_h * p_h) + (p_e_given_nh * (1 - p_h))
	return (p_e)
}

#P(H|E)= P(E|H) * P(H) / P(E)
#P(E)=P(E|H)P(H)+P(E|not H)P(not H)

bayes_confusion <- function(conf_list)
{
	return( (prevalence(conf_list) * sensitivity(conf_list)) /
			((prevalence(conf_list) * sensitivity(conf_list)) + ((1-prevalence(conf_list)) * (1-specificity(conf_list)))) )
}

# D==disease
# T==positive test
# P(D|T) == P(T|D) * P(D) / P(T) 
# P(D|T) == sensitivity * prevalence / (tru)

# prevalence == P(disease in population) == P(H) == probability of hypothesis in general (e.g disease in the relevant population) == true_pos + false_neg / total_observations
# sensitivity == P(positive test | disease) == P(E|H) among patients with disease, the probability of a positive test == true_pos / actual_pos
# specificity == true negative rate == P(not positive test | no disease) == P(not E | not H) == among patients without disease (i.e. healthy patients), the probability of a negative test == true_neg / actual_neg
# if specificity is provided it is converted to false positive rate == [ P(not E | not H) == 1 - P(E | not H)]
#1-specificity == p_e_given_nh == false pos rate   Thus P(B|not A) is the probability of a "false positive": that you test positive even though you don't have the disease. (https://www.math.hmc.edu/funfacts/ffiles/30002.6.shtml)
# post-test probability = probability of disease given a positive or negative test. Also called the "conditional probability", as in "Probability of disease conditional on the patient having a positive test."
# http://ebp.uga.edu/courses/Chapter%204%20-%20Diagnosis%20I/4%20-%20Sensitivity%20and%20specificity.html
# can either provide specificity (true negiative right == P(E | not H)
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

#P(E) == specificity == true negative rate

#prevalence = true_pos + false_neg / total_observations


#Probability of disease given a positive test = (prevalence x sensitivity) / ((prevalence x sensitivity) + ((1-prevalence) x (1-specificity)))
# http://ebp.uga.edu/courses/Chapter%204%20-%20Diagnosis%20I/4%20-%20Sensitivity%20and%20specificity.html


# estimate the probability of hypothesis (h) given evidence (e) p_h_given_e
# P(E)=P(E|H)*P(H)  +  P(E|not H)*P(not H)|
bayes_simple <- function(p_e, p_h, p_e_given_h)
{
	p_h_given_e = (p_h * p_e_given_h) / p_e
	return (p_h_given_e)
}
