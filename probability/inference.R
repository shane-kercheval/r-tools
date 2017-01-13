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

bayes.t.test = function(x, n0=1, mu0 = 0,  prior.H1=.5)
{
	# source: Dr. Merlise Clyde: https://github.com/StatsWithR/figures/blob/master/04_bayesian_statistics/week_03/4.3.2_comparing_two_paired_means/Docs/4-3-2-pair-notes.pdf
	out = t.test(x - mu0)
	t = as.numeric(abs(out$statistic)) 
	n = length(x)
	df=n-1
	#BF is BF of H1 to H2 
	BF=exp(.5*(log(n + n0) - log(n0) + (df + 1)*(log(t^2*n0/(n + n0) + df) - log(t^2 + df))))
	PO= BF*prior.H1/(1 - prior.H1)
	post.prob = 1/(1 + 1/PO)

	return (list(BF.H1.H2=BF, post.prob.H1 = post.prob, post.prob.H2= 1 - post.prob, t=t, p.value=out$p.value, df=n-1))
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
