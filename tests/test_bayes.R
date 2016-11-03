library('testthat')
source('../tools.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_bayes.R")

test_that("probability: bayes_explicit", {
	# example from Signal v. Noise (Nate Silver) http://www.businessinsider.com/bayess-theorem-nate-silver-2012-9

	# Suppose you are living with a partner and come home from a business trip to discover a strange pair of underwear in your dresser drawer.
	# You will probably ask yourself: what is the probability that your partner is cheating on you?
	# hypothesis (h) is that you are being cheated on
	# evidence (e) are strange panties
	# probability of being cheated on (p_h a.k.a P(H)) given there exists strange panties (p_a P(E))
	# P(E) (probability of panties) is clearly 1 (i.e. you see them) and irrelevant

	p_h_given_e = NULL # need to figure this out!!

	# P(E) is the probability that A is happening, even if you didn't know about B. (The general probability you're being cheated on)
	# This is like a 'base rate'... Studies have found, for instance, that around 4 percent of married partners cheat on their spouses in any given year, so we'll set that as our prior.
	p_h = 0.04 # prob of being cheated on (base rate)

	# P(E|H) is the probability that the underwear appeared because you are in fact being cheated on.
	# If he's cheating on you, it's certainly easy enough to imagine how the panties got there. Then again, even (and perhaps especially) if he is cheating on you, might expect him to be more careful.
	p_e_given_h = 0.5 # Let's say the probability of the panties appearing, conditional on him cheating on you, is 50 percent

	# P(E|H`) is the probability that you found the panties and he's not cheating on you.
	# Next, you want to estimate the probability that the underwear appears but he's not cheating on you - maybe they're a gift, or a platonic female friend stayed over and left them, or there was a luggage mix up.
	# Silver says that you could put the probability at 5%
	p_e_given_nh = 0.05 # probability that the underwear appears but he's not cheating on you

	p_h_given_e = bayes_explicit(p_h, p_e_given_h, p_e_given_nh)
	expect_equal(p_h_given_e, 0.2941176, tolerance=0.0000001) #29% chance of being cheated on.

	# https://www.math.hmc.edu/funfacts/ffiles/30002.6.shtml

})

test_that("probability: bayes_confusion, bayes_simple", {
	model_true_neg = 1820
	model_false_neg = 10
	model_false_pos = 180
	model_true_pos = 20
	conf_list=confusion_list(true_pos=model_true_pos, true_neg=model_true_neg, false_pos=model_false_pos, false_neg=model_false_neg)

	bayes_c = bayes_confusion(conf_list)
	bayes_s1 = bayes_simple(p_e=(conf_list$false_pos + conf_list$true_pos) / conf_list$total, p_h=prevalence(conf_list), p_e_given_h=sensitivity(conf_list))
	bayes_s2 = bayes_simple(p_e=prob_e(p_h=prevalence(conf_list), p_e_given_h=sensitivity(conf_list), p_e_given_nh=false_positive_rate(conf_list)), p_h=prevalence(conf_list), p_e_given_h=sensitivity(conf_list))

	# these three bayes methods are equal
	expect_equal(bayes_c, bayes_s1, bayes_s2)
	# which means that
	expect_equal(prob_e(p_h=prevalence(conf_list), p_e_given_h=sensitivity(conf_list), p_e_given_nh=false_positive_rate(conf_list)), (conf_list$false_pos + conf_list$true_pos) / conf_list$total)

	#specificity is 1-false_positive_rate
	expect_equal(specificity(conf_list), 1 - false_positive_rate(conf_list))
})

test_that("probability: bayes_prevalence", {
	# You are running a mammography screening program in a van that travels around your health district. A 45 year old woman has a mammogram. The study is interpreted as "suspicious for malignancy" by the radiologist. The patient asks you: "Does this mean I have cancer?", and you (correctly) answer "No, we have to do further testing." She then asks, "OK, I understand that the mammogram isn’t the final answer, but given what we know now, what are the chances that I have breast cancer?".  Assume that the overall risk of breast cancer in any 45 year old woman, regardless of mammogram result, is 0.1% or one in a thousand. Assume also that mammography is 80% sensitive and 95% specific. What is the probability that this woman actually has breast cancer?
	# prevalence or pretest probability = probability of disease in the relevant population (i.e. 5% of patients presenting with cough have pneumonia)
	# sensitivity = among patients with disease, the probability of a positive test
	# specificity = among patients without disease (i.e. healthy patients), the probability of a negative test
	# post-test probability = probability of disease given a positive or negative test. Also called the "conditional probability", as in "Probability of disease conditional on the patient having a positive test."
	prevalence=0.001
	specificity = 0.95
	sensitivity = 0.8
	bayes_p = bayes_prevalence(prevalence=prevalence, sensitivity=sensitivity, specificity=specificity)
	expect_equal(bayes_p, 0.01576355)
})
