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

test_that("probability: expected_value", {
	ex_value = expected_value_confusion(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg,
										tp_cost_benefit=1, tn_cost_benefit=1, fp_cost_benefit=1, fn_cost_benefit=1)
	expect_equal(ex_value, 1) # this is because if all costs/benefits are equal to 1, it will just add up the probabilities, which should equal 1

	# often times we expect fp vs. fn to have different weights or concequences (e.g. false positive of court trial is sending innocent man to jail)
	model_true_pos = 5
	model_true_neg = 90
	model_false_pos = 4
	model_false_neg = 1
	# these are just 'ratios', but you could use actual costs and actual benefits (i.e. profit) in a business scenario if you have the numbers
	tp_cost_benefit = 10 # a true positive (e.g. cancer) is very benefitial
	tn_cost_benefit = 0 # a true negative doesn't cost or benefit anything
	fp_cost_benefit = -1 # a false positive (e.g. predicting cancer, when there actually was not) is expensive in that it costs money for additional tests, additional stress, etc., but it is not life-threatoning (i.e. we'd rather have false positives than false negatives)
	fn_cost_benefit = -5 # a false negative (e.g. predicting no cancer when there actually is cancer) is a matter of life and death... A quick guess at the ratio would be that is that it is worth half of a true positive (if for every life you save via detection, you also miss a detection and someone dies, then you are saving only about half the people who have cancer, which is still better than no one, so it's not a zero-sum (i.e. ratios shouldn't be the same between true positive and false negative))
	ex_value = expected_value_confusion(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg,
										tp_cost_benefit=tp_cost_benefit, tn_cost_benefit=tn_cost_benefit, fp_cost_benefit=fp_cost_benefit, fn_cost_benefit=fn_cost_benefit)
	expect_equal(ex_value, 0.41)

	expect_equal(expected_value_confusion(56, 42, 7, 5, 99, 0, -1, 0), 50.33636, tolerance=0.00001)

	# from Data Science for Business (Provost, Fawcett, kindle loc 4424)
	model_true_pos = 56
	model_true_neg = 42
	model_false_pos = 7
	model_false_neg = 5
	tp_cost_benefit = 99 # profit from customer
	tn_cost_benefit = 0 # predicted correctly they wouldn't respond, no cost
	fp_cost_benefit = -1 # we predicted they would convert, so we send them mail, but they don't convert. Mail cost $1
	fn_cost_benefit = 0 # predicted incorrectly they wouldn't responde, so we didn't send them anything (IGNORES OPPORTUNITY COST OF GAINING PROFIT. NOT QUITE AS LARGE AS TP BENEFIT, BUT MODEL SHOULD BE PENELIZED FOR IT, OR WE SHOULD HAVE TWO MODELS, ONE FOR ACTUAL EXPECTED COSTS AND ONE FOR EXPECTED COSTS INCLUDING OPPORUTNITY COSTS)

	# regular expected value equation
	exp_value = expected_value(n_occur=c(model_true_pos, model_true_neg, model_false_pos, model_false_neg), benefits=c(tp_cost_benefit, tn_cost_benefit, fp_cost_benefit, fn_cost_benefit))
	expect_equal(exp_value, 50.33636, tolerance=0.0001)
	# expected value including conditionals probabilities
	exp_value = expected_value_conditional(o_tp=model_true_pos, o_tn=model_true_neg, o_fp=model_false_pos, o_fn=model_false_neg, b_tp=tp_cost_benefit, b_tn=tn_cost_benefit, b_fp=fp_cost_benefit, b_fn=fn_cost_benefit)
	expect_equal(exp_value, 50.33636, tolerance=0.0001)
	# expected_value & expected_value_conditional give the same value.
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
	p_b = 0.04 # prob of being cheated on (base rate)

	# P(A|B) is the probability that the underwear appeared because you are in fact being cheated on.
	# If he's cheating on you, it's certainly easy enough to imagine how the panties got there. Then again, even (and perhaps especially) if he is cheating on you, might expect him to be more careful.
	p_a_given_b = 0.5 # Let's say the probability of the panties appearing, conditional on him cheating on you, is 50 percent

	# P(A|B`) is the probability that you found the panties and he's not cheating on you.
	# Next, you want to estimate the probability that the underwear appears but he's not cheating on you - maybe they're a gift, or a platonic female friend stayed over and left them, or there was a luggage mix up.
	# Silver says that you could put the probability at 5%
	p_a_given_nb = 0.05 # probability that the underwear appears but he's not cheating on you

	p_b_given_a = bayes(p_b, p_a_given_b, p_a_given_nb)
	expect_equal(p_b_given_a, 0.2941176, tolerance=0.0000001) #29% chance of being cheated on.
})

test_that("probability: bayes:", {

})
