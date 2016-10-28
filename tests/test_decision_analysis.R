library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_decision_analysis.R")

test_that("probability: decision_analysis", {
	observations = c(56, 42, 7, 5)
	probabilities = observations / sum(observations)
	benefits = c(99, 0, -1, 0)

	expect_error(expected_value(benefits=benefits))
	expect_equal(expected_value(n_occur=observations, benefits=benefits), 50.33636, tolerance=0.00001)
	expect_equal(expected_value(probs=probabilities, benefits=benefits), 50.33636, tolerance=0.00001)
})

test_that("probability: bayes", {
	# example from Signal v. Noise (Nate Silver) http://www.businessinsider.com/bayess-theorem-nate-silver-2012-9

	# Suppose you are living with a partner and come home from a business trip to discover a strange pair of underwear in your dresser drawer.
	# You will probably ask yourself: what is the probability that your partner is cheating on you?
	# probability of being cheated on (p_b a.k.a P(B)) given there exists strange panties (p_a P(A))
	# P(A) (probability of panties) is clearly 1 (i.e. you see them) and irrelevant

	p_b_given_a = NULL # need to figure this out!!

	# P(A) is the probability that A is happening, even if you didn't know about B. (The general probability you're being cheated on)
	# This is like a 'base rate'... Studies have found, for instance, that around 4 percent of married partners cheat on their spouses in any given year, so we'll set that as our prior.
	p_b = 0.04

	# P(A|B) is the probability that the underwear appeared because you are in fact being cheated on.
	# If he's cheating on you, it's certainly easy enough to imagine how the panties got there. Then again, even (and perhaps especially) if he is cheating on you, might expect him to be more careful.
	# Let's say the probability of the panties appearing, conditional on him cheating on you, is 50 percent
	p_a_given_b = 0.5

	# P(A|B`) is the probability that you found the panties and he's not cheating on you.
	# Next, you want to estimate the probability that the underwear appears but he's not cheating on you - maybe they're a gift, or a platonic female friend stayed over and left them, or there was a luggage mix up.
	# Silver says that you could put the probability at 5%
	p_a_given_nb = 0.05

	p_b_given_a = bayes(p_b, p_a_given_b, p_a_given_nb)
	expect_equal(p_b_given_a, 0.2941176, tolerance=0.0000001) #29% chance of being cheated on.

})
