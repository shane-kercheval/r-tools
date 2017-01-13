z.prop <- function(x1, x2, n1, n2)
{ # ORIGINAL: https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
	numerator = (x1/n1) - (x2/n2)
	p_common = (x1+x2) / (n1+n2)
	denominator = sqrt(p_common * (1-p_common) * (1/n1 + 1/n2))
	z_prop_ris = numerator / denominator
	return(z_prop_ris)
}

convert.z.score <- function(z, one.sided=NULL)
{
	if(is.null(one.sided)) 
	{
		pval = pnorm(-abs(z))
		pval = 2 * pval
	} 
	else if(one.sided=="-") 
	{
		pval = pnorm(z)
	} 
	else 
	{
		pval = pnorm(-z)
	}
	return(pval)
}

bayes.t.test = function(numeric_vector, n0=1, mu0 = 0,  prior_h1=.5)
{
	# source: Dr. Merlise Clyde: https://github.com/StatsWithR/figures/blob/master/04_bayesian_statistics/week_03/4.3.2_comparing_two_paired_means/Docs/4-3-2-pair-notes.pdf
	t_test_results = t.test(numeric_vector - mu0)
	t_statistic = as.numeric(abs(t_test_results$statistic)) 
	data_length = length(numeric_vector)
	degrees_of_freedom = data_length - 1
	#bayes_factor is Bayes Factor of Hypothesis 1 to Hypothesis 2
	bayes_factor = exp(0.5 * (log(data_length + n0) - log(n0) + (degrees_of_freedom + 1) * (log(t_statistic^ 2 * n0 / (data_length + n0) + degrees_of_freedom) - log(t_statistic^2 + degrees_of_freedom))))
	prior_odds = bayes_factor * prior_h1 / (1 - prior_h1)
	post_prob_H1 = 1 / (1 + 1/prior_odds)

	return (list(bayes_factor.H1.H2=bayes_factor, bayes_factor.H2.H1=1/bayes_factor, post.prob.H1=post_prob_H1, post.prob.H2= 1 - post_prob_H1, t.statistic=t_statistic, p.value=t_test_results$p.value, degrees.of.freedom=data_length-1))
}

chi.square.independence <- function(s, o=NULL, ns=NULL, correct=TRUE)
{
	o = get_observations(s=s, ns=ns, o=o)
	return (prop.test(x=s, n=o, correct=correct))
	# trial_matrix = matrix(c(s, ns), byrow = TRUE, nrow = 2, dimnames = list(c('successes', 'non_successes'), c(letters[1:length(s)])))
	# return (chisq.test(trial_matrix, simulate.p.value = simulate.p.value))
}

chi.square.goodness_of_fit <- function(s, o=NULL, ns=NULL, correct=TRUE)
{	# with this test, we want to get the proportion of the first trial, use that as p, and pass in the rest of successes/observations
	o = get_observations(s=s, ns=ns, o=o)
	first_success = s[1]
	first_num_obs = o[1]

	s = s[2:length(s)]
	o = o[2:length(o)]

	return (prop.test(x=s, n=o, p=first_success/first_num_obs, correct=correct))
}

ab_test.fisher <- function(s, o=NULL, ns=NULL)
{
	ns = get_non_successes(s, ns=ns, o=o)
	return (fisher.test(rbind(s, ns)))
}

ab_test.binomial <- function(original_successes, original_total, variation_successes, variation_total)
{
	return (binom.test(x=variation_successes, n=variation_total, p = original_successes/original_total, alternative = c("two.sided"), conf.level = 0.95))
}

get_observations <- function(s, ns=NULL, o=NULL)
{
	if(is.null(o) && is.null(ns))
	{
		stop("`o` and `ns` can't both be NULL")
	}
	if(!is.null(ns)) # if ns is not NULL, then calculate non_successes (`o`), otherwise `NS` is NULL and therefore `o` won't be NULL (since that is the first check), and we don't have to do anything
	{
		o = s + ns
	}
	return (o)
}

get_non_successes <- function(s, ns=NULL, o=NULL)
{
	if(is.null(ns) && is.null(o))
	{
		stop("`ns` and `o` can't both be NULL")
	}
	if(!is.null(o)) # if o is not NULL, then calculate non_successes (`o`), otherwise `NS` is NULL and therefore `o` won't be NULL (since that is the first check), and we don't have to do anything
	{
		ns = o - s
	}
	return (ns)
}
