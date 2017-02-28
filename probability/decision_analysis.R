source('../general/model_measurements.R', chdir=TRUE)

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
	if(1 - round(sum(probs),4) != 0)
	{
		stop("`probs` don't add up to 1 (not within 4 decimal places)")
	}
	return (sum(probs * benefits))
}

# same value as expected-value
expected_value_conditional <- function(o_tp, o_tn, o_fp, o_fn, b_tp, b_tn, b_fp, b_fn)
{
	conf_list = confusion_list(true_pos=o_tp, true_neg=o_tn, false_pos=o_fp, false_neg=o_fn)
	qom = quality_of_model(conf_list)

	# this equation corresponds to equation in Data Science for Business (Provost, Fawcett, kindle loc 4424)
	# sensitivity == TRUE POSITIVE RATE
	# specificity == TRUE NEGATIVE RATE
	return ((qom$actual_pos_prob * (qom$sensitivity * b_tp + qom$false_negative_rate * b_fn)) +
			(qom$actual_neg_prob * (qom$specificity * b_tn + qom$false_positive_rate * b_fp)))

}

monthly_churn_to_annual <- function(monthly_churn_rate)
{
	return (1- (1 - monthly_churn_rate)^12)
}

annual_churn_to_monthly <- function(annual_churn_rate)
{
	return (1- (1 - annual_churn_rate)^(1/12))
}

monthly_retention_to_annual <- function(monthly_retention_rate)
{
	return (monthly_retention_rate^12)
}

annual_retention_to_monthly <- function(annual_retention_rate)
{
	return (annual_retention_rate^(1/12))
}

# time-frame is same as passed in (monthy churn gives lifespan in months, yearly, churn gives lifespan in years)
churn_to_lifespan <- function(churn_rate)
{
	return (1 / churn_rate)
}

retention_to_lifespan <- function(retention_rate)
{
	return (1 / (1 - retention_rate))
}

library('FinCal')
customer_lifetime_value <- function(retention_rate_monthly=NULL, retention_rate_annual=NULL,
	avg_monthly_customer_revenue=NULL, avg_annual_customer_revenue=NULL, contribution_margin_ratio,
	acquisition_conversion_rates_per_stage=NULL, acquisition_conversion_rate=NULL, cost_per_event=NULL, cost_of_customer_acquisition=NULL,
	discount_rate=.10)
{
	clv_info = list()

	if(is.null(cost_of_customer_acquisition)) # if cost_of_customer_acquisition is null, we have to try to calculate it
	{
		if(is.null(acquisition_conversion_rate)) # calculate it from acquisition_conversion_rate
		{
			if(is.null(acquisition_conversion_rates_per_stage))
			{
				clv_info$acquisition_conversion_rate = NULL
			}
			else
			{
				clv_info$acquisition_conversion_rate = prod(acquisition_conversion_rates_per_stage)
			}
		}
		else
		{
			clv_info$acquisition_conversion_rate = acquisition_conversion_rate
		}

		if(!is.null(clv_info$acquisition_conversion_rate))
		{
			if(is.null(cost_per_event))
			{
				stop("cost_per_event is null (and shouldn't be) but cost_of_customer_acquisition is not 0 or not NULL")
			}
			clv_info$cost_per_event = cost_per_event
			clv_info$cost_of_customer_acquisition = clv_info$cost_per_event / clv_info$acquisition_conversion_rate
		}
	}
	else
	{
		clv_info$cost_of_customer_acquisition = cost_of_customer_acquisition
	}

	# need to only provide monthly or annual retention rate
	if(is.null(retention_rate_annual))
	{
		if(is.null(retention_rate_monthly))
		{
			stop("retention_rate_monthly or retention_rate_annual must be given")
		}
		clv_info$retention_rate_annual = monthly_retention_to_annual(retention_rate_monthly)
	}
	else
	{
		clv_info$retention_rate_annual = retention_rate_annual
	}

	clv_info$avg_customer_life_span = retention_to_lifespan(clv_info$retention_rate_annual) # ANNUAL! if not annual then you have to adjust your discount rate to monthly (and you can't just divide by 12, you have to do (e.g. in excel) RATE(12, 0, −1, 1.1) i.e. what is the monthly rate that gets you to 10% annual rate (i.e. 1.1), in this case it is 0.797414042890369% so 0.00797 monthly rate

	if(is.null(avg_annual_customer_revenue))
	{
		if(is.null(avg_monthly_customer_revenue))
		{
			stop("avg_annual_customer_revenue and avg_monthly_customer_revenue are both NULL")
		}
		clv_info$avg_annual_customer_revenue = avg_monthly_customer_revenue * 12
	}
	else
	{
		clv_info$avg_annual_customer_revenue = avg_annual_customer_revenue
	}

	clv_info$contribution_margin_ratio = contribution_margin_ratio
	clv_info$contribution_margin_annual = clv_info$avg_annual_customer_revenue * clv_info$contribution_margin_ratio
	clv_info$customer_lifetime_profit = clv_info$avg_customer_life_span * clv_info$contribution_margin_annual
	if(is.null(clv_info$cost_of_customer_acquisition))
	{
		cac = 0
	}
	else
	{
		cac = clv_info$cost_of_customer_acquisition
	}

	clv_info$customer_lifetime_value = clv_info$customer_lifetime_profit - cac
	clv_info$clv_to_cac = clv_info$customer_lifetime_value / cac
	clv_info$payback_period_years = cac/clv_info$contribution_margin_annual

	clv_info$return_on_investment = (clv_info$customer_lifetime_value - cac) / cac
	clv_info$annualized_return = (clv_info$customer_lifetime_value / cac)^(1 / clv_info$avg_customer_life_span) - 1

	clv_info$customer_lifetime_value_npv = abs(sum(map_dbl(1:clv_info$avg_customer_life_span, ~pv(discount_rate, n=., fv = clv_info$contribution_margin_annual, pmt = 0, type = 0)))) - cac
	clv_info$clv_to_cac_pv = clv_info$customer_lifetime_value_npv / cac
	clv_info$return_on_investment_pv = (clv_info$customer_lifetime_value_npv - cac) / cac
	clv_info$annualized_return_pv = (clv_info$customer_lifetime_value_npv / cac)^(1 / clv_info$avg_customer_life_span) - 1

	return (clv_info)
}
