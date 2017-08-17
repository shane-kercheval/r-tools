library('testthat')
source('../general/cohort_conversion_rates.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_cohort_conversion_rates.R")

test_that("cohort_conversion_rates: main", {
	
	simulated_data <- readRDS(file = './data/simulated_evolution_data.RDS')
	
	# test the data so that cohorts are weeks, evolution is shown over days (units_in_age = 1, age_units = 'days) over 30 days (age_limit = 30)
	cohorted_crs <- get_cohorted_crs(lifecycle_data = simulated_data, cohort_type_func = custom_iso_week, age_units = 'days', units_in_age = 1, age_limit = 30, cutoff_date_time = ymd_hms('2017-08-16 19:03:06'))
	
	#expected_data <- cohorted_crs
	#saveRDS(expected_data, file = './data/expected_data_cohorted_crs_days.RDS')
	expected_data <- readRDS(file = './data/expected_data_cohorted_crs_days.RDS')
	expect_true(all(cohorted_crs == expected_data))

	# Test the first couple evolution days.
	expected_data_first_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'days') <= 1, na.rm = TRUE) / n())
	expect_true(all(cohorted_crs %>% filter(cohort_age == 1) %>% select(-cohort_age) == expected_data_first_day_conversions))

	expected_data_second_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'days') <= 2, na.rm = TRUE) / n())
	expect_true(all(cohorted_crs %>% filter(cohort_age == 2) %>% select(-cohort_age) == expected_data_second_day_conversions))

	# test the data so that cohorts are weeks, evolution is shown over days (units_in_age = 1, age_units = 'days) over 30 days (age_limit = 30)
	cohorted_crs <- get_cohorted_crs(lifecycle_data = simulated_data, cohort_type_func = custom_iso_week, age_units = 'weeks', units_in_age = 1, age_limit = NULL, cutoff_date_time = ymd_hms('2017-08-16 19:03:06'))

	#expected_data <- cohorted_crs
	#saveRDS(expected_data, file = './data/expected_data_cohorted_crs_weeks.RDS')
	expected_data <- readRDS(file = './data/expected_data_cohorted_crs_weeks.RDS')
	expect_true(all(cohorted_crs == expected_data))

	# Test the first couple evolution days.
	expected_data_first_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'weeks') <= 1, na.rm = TRUE) / n())

	expect_true(all(cohorted_crs %>% filter(cohort_age == 1) %>% select(-cohort_age) == expected_data_first_day_conversions %>% filter(!(cohort %in% c('2017-32', '2017-33')))))

	expected_data_second_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'weeks') <= 2, na.rm = TRUE) / n())
	expect_true(all(cohorted_crs %>% filter(cohort_age == 2) %>% select(-cohort_age) == expected_data_second_day_conversions %>% filter(!(cohort %in% c('2017-31', '2017-32', '2017-33')))))

	# should get the same date if we go by days and check evolution after every 7 days than if we do weekly
	cohorted_crs_daily <- get_cohorted_crs(lifecycle_data = simulated_data, cohort_type_func = custom_iso_week, age_units = 'days', units_in_age = 7, age_limit = NULL, cutoff_date_time = ymd_hms('2017-08-16 19:03:06'))
	cohorted_crs_weekly <- get_cohorted_crs(lifecycle_data = simulated_data, cohort_type_func = custom_iso_week, age_units = 'weeks', units_in_age = 1, age_limit = NULL, cutoff_date_time = ymd_hms('2017-08-16 19:03:06'))
	expect_true(all(cohorted_crs_daily == cohorted_crs_weekly %>% mutate(cohort_age = cohort_age * 7)))

})

test_that("cohort_conversion_rates: cohort_cumulative_cr_plot", {
	
	simulated_data <- readRDS(file = './data/simulated_evolution_data.RDS')
	# test the data so that cohorts are weeks, evolution is shown over days (units_in_age = 1, age_units = 'days) over 30 days (age_limit = 30)
	cohorted_crs <- get_cohorted_crs(lifecycle_data = simulated_data, cohort_type_func = custom_iso_week, age_units = 'days', units_in_age = 1, age_limit = 30, cutoff_date_time = ymd_hms('2017-08-16 19:03:06'))
	file.remove('../general/example_cohort_cumulative_cr_plot.png')
	cohort_cumulative_cr_plot(	cohort_df =  cohorted_crs %>%
							   	filter(cohort != paste0(year(Sys.Date()), '-', isoweek(Sys.Date()))) %>%
							   	rename(y_value = cummulative_cr),
							   title = 'Evolution of EventA to EventB Convertsion Rate Over Time (Days)',
							   y_label = 'Cummulative conversion rate for cohort',
							   caption = '\nShows evolution of each cohort over time (x-axis, days).\nOlder cohorts have more transparent lines.')
	ggsave(filename = '../general/example_cohort_cumulative_cr_plot.png')
	expect_true(file.exists('../general/example_cohort_cumulative_cr_plot.png'))
	
	cohort_x_month_snapshot <- cohorted_crs %>% filter(cohort != paste0(year(Sys.Date()), '-', isoweek(Sys.Date())))
	file.remove('../general/example_cumulative_cr_snapshot_plot.png')
	cumulative_cr_snapshot_plot(cohort_df = cohort_x_month_snapshot, event_label = 'EventB CR', initial_event = 'Initial EventA')
	ggsave(filename = '../general/example_cumulative_cr_snapshot_plot.png')
	file.exists('../general/example_cumulative_cr_snapshot_plot.png')


	cohort_rate_of_change <- full_join(data.frame(cohort = sort(unique(cohorted_crs$cohort)),
										cohort_age = 0,
										cummulative_cr = 0),
							cohorted_crs,
							by = c('cohort', 'cohort_age', 'cummulative_cr')) %>%
						arrange(cohort, cohort_age) %>%
						mutate(previous_cr = lag(cummulative_cr), change = cummulative_cr - previous_cr) %>%
						filter(cohort_age != 0)
	file.remove('../general/example_cumulative_cr_snapshot_plot_rate.png')
	cohort_cumulative_cr_plot(	cohort_df = cohort_rate_of_change %>% rename(y_value = change),
								title = 'Rate of Capture - CR (Days)',
								y_label = 'Incremental % captured (Difference in CR between days)')
	ggsave(filename = '../general/example_cumulative_cr_snapshot_plot_rate.png')
	file.exists('../general/example_cumulative_cr_snapshot_plot_rate.png')	
})
