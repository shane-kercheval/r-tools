ab_test.prop <- function(s, o=NULL, ns=NULL, correct=TRUE)
{
	o = get_observations(s=s, ns=ns, o=o)
	return (prop.test(x=s, n=o, correct=correct))
	# trial_matrix = matrix(c(s, ns), byrow = TRUE, nrow = 2, dimnames = list(c('successes', 'non_successes'), c(letters[1:length(s)])))
	# return (chisq.test(trial_matrix, simulate.p.value = simulate.p.value))
}

ab_test.propp <- function(s, o=NULL, ns=NULL, correct=TRUE)
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
