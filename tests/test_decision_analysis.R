library('testthat')
source('../probability/decision_analysis.R', chdir=TRUE)

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

	# probs should equal to one
	expect_error(expected_value(probs = c(0.9999), benefits = c(1)))
	#rounding out to 4 decimal points, so this should work
	expect_equal(expected_value(probs = c(0.99999), benefits = c(1)), 0.99999)
})

test_that("probability: expected_value", {
	model_true_neg = 1820
	model_false_neg = 10
	model_false_pos = 180
	model_true_pos = 20
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

test_that("probability: expected_value_paid_advertising:", {
	# http://andrewchen.co/when-does-paid-acquisition-work-for-saas-startups/
	# Paid user acquisition works for you when the following proves true:
		# LTV > CAC

	annual_retention_rate = .75
	average_annual_revenue = 3000
	contribution_margin_ratio = 1/3
	cost_per_event = 2.50 # $2.50 per CLICK
	acq_cr = prod(c(0.05, 0.10)) # 5% conversion rate from CLICK to signup and then 5 percent CR from signup to acquisition

	clv_info = customer_lifetime_value(retention_rate_monthly=annual_retention_rate^(1/12), avg_monthly_customer_revenue=average_annual_revenue/12, contribution_margin_ratio=contribution_margin_ratio, acquisition_conversion_rate=acq_cr, cost_per_event=cost_per_event)
	expect_equal(clv_info$acquisition_conversion_rate, acq_cr)
	expect_equal(clv_info$cost_per_event, cost_per_event)
	expect_equal(clv_info$cost_of_customer_acquisition, cost_per_event/acq_cr)
	expect_equal(clv_info$retention_rate_annual, annual_retention_rate)
	expect_equal(clv_info$avg_customer_life_span, 4)
	expect_equal(clv_info$avg_annual_customer_revenue, average_annual_revenue)
	expect_equal(clv_info$contribution_margin_ratio, contribution_margin_ratio)
	expect_equal(clv_info$contribution_margin_annual, 1000)
	expect_equal(clv_info$customer_lifetime_profit, 4000)
	expect_equal(clv_info$customer_lifetime_value, 3500)
	expect_equal(clv_info$clv_to_cac, 7)
	expect_equal(clv_info$payback_period_years, 0.5)
	expect_equal(clv_info$return_on_investment, 6)
	expect_equal(clv_info$annualized_return, 0.6265766, tolerance=0.0000001)
	expect_equal(clv_info$customer_lifetime_value_npv, 2669.865, tolerance=0.001)
	expect_equal(clv_info$clv_to_cac_pv, 5.339731, tolerance=0.0000001)
	expect_equal(clv_info$return_on_investment_pv, 4.339731, tolerance=0.0000001)
	expect_equal(clv_info$annualized_return_pv, 0.5201269, tolerance=0.0000001)

	clv_info = customer_lifetime_value(retention_rate_annual=annual_retention_rate, avg_annual_customer_revenue=average_annual_revenue, contribution_margin_ratio=contribution_margin_ratio, acquisition_conversion_rate=acq_cr, cost_per_event=cost_per_event)
	expect_equal(clv_info$acquisition_conversion_rate, acq_cr)
	expect_equal(clv_info$cost_per_event, cost_per_event)
	expect_equal(clv_info$cost_of_customer_acquisition, cost_per_event/acq_cr)
	expect_equal(clv_info$retention_rate_annual, annual_retention_rate)
	expect_equal(clv_info$avg_customer_life_span, 4)
	expect_equal(clv_info$avg_annual_customer_revenue, average_annual_revenue)
	expect_equal(clv_info$contribution_margin_ratio, contribution_margin_ratio)
	expect_equal(clv_info$contribution_margin_annual, 1000)
	expect_equal(clv_info$customer_lifetime_profit, 4000)
	expect_equal(clv_info$customer_lifetime_value, 3500)
	expect_equal(clv_info$clv_to_cac, 7)
	expect_equal(clv_info$payback_period_years, 0.5)
	expect_equal(clv_info$return_on_investment, 6)
	expect_equal(clv_info$annualized_return, 0.6265766, tolerance=0.0000001)
	expect_equal(clv_info$customer_lifetime_value_npv, 2669.865, tolerance=0.001)
	expect_equal(clv_info$clv_to_cac_pv, 5.339731, tolerance=0.0000001)
	expect_equal(clv_info$return_on_investment_pv, 4.339731, tolerance=0.0000001)
	expect_equal(clv_info$annualized_return_pv, 0.5201269, tolerance=0.0000001)

	clv_info = customer_lifetime_value(retention_rate_annual=annual_retention_rate, avg_annual_customer_revenue=average_annual_revenue, contribution_margin_ratio=contribution_margin_ratio, acquisition_conversion_rates_per_stage=c(0.10, 0.05), cost_per_event=cost_per_event)
	expect_equal(clv_info$acquisition_conversion_rate, acq_cr)
	expect_equal(clv_info$cost_per_event, cost_per_event)
	expect_equal(clv_info$cost_of_customer_acquisition, cost_per_event/acq_cr)
	expect_equal(clv_info$retention_rate_annual, annual_retention_rate)
	expect_equal(clv_info$avg_customer_life_span, 4)
	expect_equal(clv_info$avg_annual_customer_revenue, average_annual_revenue)
	expect_equal(clv_info$contribution_margin_ratio, contribution_margin_ratio)
	expect_equal(clv_info$contribution_margin_annual, 1000)
	expect_equal(clv_info$customer_lifetime_profit, 4000)
	expect_equal(clv_info$customer_lifetime_value, 3500)
	expect_equal(clv_info$clv_to_cac, 7)
	expect_equal(clv_info$payback_period_years, 0.5)
	expect_equal(clv_info$return_on_investment, 6)
	expect_equal(clv_info$annualized_return, 0.6265766, tolerance=0.0000001)
	expect_equal(clv_info$customer_lifetime_value_npv, 2669.865, tolerance=0.001)
	expect_equal(clv_info$clv_to_cac_pv, 5.339731, tolerance=0.0000001)
	expect_equal(clv_info$return_on_investment_pv, 4.339731, tolerance=0.0000001)
	expect_equal(clv_info$annualized_return_pv, 0.5201269, tolerance=0.0000001)

	annual_retention_rate = .75
	average_annual_revenue = 300
	contribution_margin_ratio = 1/3
	cost_per_event = 4.50
	acquisition_conversion_rates_per_stage = c(0.05, 0.90)
	clv_info = customer_lifetime_value(retention_rate_annual=annual_retention_rate, avg_annual_customer_revenue=average_annual_revenue, contribution_margin_ratio=contribution_margin_ratio, acquisition_conversion_rates_per_stage=acquisition_conversion_rates_per_stage, cost_per_event=cost_per_event)
	expect_equal(clv_info$acquisition_conversion_rate, 0.045)
	expect_equal(clv_info$cost_per_event, 4.5)
	expect_equal(clv_info$cost_of_customer_acquisition, 100)
	expect_equal(clv_info$retention_rate_annual, .75)
	expect_equal(clv_info$avg_customer_life_span, 4)
	expect_equal(clv_info$avg_annual_customer_revenue, 300)
	expect_equal(clv_info$contribution_margin_ratio, 0.3333333, tolerance=0.000001)
	expect_equal(clv_info$contribution_margin_annual, 100)
	expect_equal(clv_info$customer_lifetime_profit, 400)
	expect_equal(clv_info$customer_lifetime_value, 300)
	expect_equal(clv_info$clv_to_cac, 3)
	expect_equal(clv_info$payback_period_years, 1)
	expect_equal(clv_info$return_on_investment, 2)
	expect_equal(clv_info$annualized_return, 0.316074, tolerance=0.0000001)
	expect_equal(clv_info$customer_lifetime_value_npv, 216.9865, tolerance=0.001)
	expect_equal(clv_info$clv_to_cac_pv, 2.169865, tolerance=0.000001)
	expect_equal(clv_info$return_on_investment_pv, 1.169865, tolerance=0.000001)
	expect_equal(clv_info$annualized_return_pv, 0.2136912, tolerance=0.000001)
})

test_that("probability: churn/rention conversion:", {
	yearly_churn = 0.05 # 5%
	yearly_retention = 1 - yearly_churn
	monthly_retention = yearly_retention^(1/12)
	monthly_churn = 1 - monthly_retention
	lifespan_years = 1 / (1 - yearly_retention)
	lifespan_months = 1 / (1 - monthly_retention)
	expect_equal(yearly_churn + yearly_retention, 1)
	expect_equal(monthly_churn + monthly_retention, 1)
	#expect_equal(lifespan_months, lifespan_years*12) WHY?

	expect_equal(monthly_churn_to_annual(monthly_churn), yearly_churn)
	expect_equal(annual_churn_to_monthly(yearly_churn), monthly_churn)
	expect_equal(monthly_retention_to_annual(monthly_retention), yearly_retention)
	expect_equal(annual_retention_to_monthly(yearly_retention), monthly_retention)

	expect_equal(churn_to_lifespan(yearly_churn), lifespan_years)
	expect_equal(churn_to_lifespan(monthly_churn), lifespan_months)

	expect_equal(retention_to_lifespan(yearly_retention), lifespan_years)
	expect_equal(retention_to_lifespan(monthly_retention), lifespan_months)
})
