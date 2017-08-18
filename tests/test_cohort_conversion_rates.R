library('testthat')
source('../general/cohort_conversion_rates.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
# test_file("test_cohort_conversion_rates.R")

simulated_current_date_time <- '2017-08-16 19:03:06'

test_that("cohort_conversion_rates: main", {
	################
	# LOAD DATA - COHORT BASED ON WEEK (EVOLUTION IN DAYS)
	################
	simulated_data <- readRDS(file = './data/simulated_evolution_data.RDS')
	# test the data so that cohorts are weeks, evolution is shown over days (units_in_age = 1, age_units = 'days) over 30 days (age_limit = 30)
	cohorted_crs <- get_cohorted_crs(	lifecycle_data = simulated_data,
										cohort_type_func = custom_iso_week,
										age_units = 'days',
										units_in_age = 1,
										age_limit = 30,
										cutoff_date_time = ymd_hms(simulated_current_date_time))
	
	################
	# VALIDATE
	################
	#expected_data <- cohorted_crs
	#saveRDS(expected_data, file = './data/expected_data_cohorted_crs_days.RDS')
	expected_data <- readRDS(file = './data/expected_data_cohorted_crs_days.RDS')
	expect_true(all(cohorted_crs == expected_data))

	# Test the first couple evolution days.
	expected_data_first_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'days') <= 1, na.rm = TRUE) / n()) %>%
		filter(cohort != create_cohort(date_vector = ymd_hms(simulated_current_date_time), custom_iso_week))
	expect_true(all(cohorted_crs %>% filter(cohort_age == 1) %>% select(-cohort_age) == expected_data_first_day_conversions))

	expected_data_second_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'days') <= 2, na.rm = TRUE) / n()) %>%
		filter(cohort != create_cohort(date_vector = ymd_hms(simulated_current_date_time), custom_iso_week))
	expect_true(all(cohorted_crs %>% filter(cohort_age == 2) %>% select(-cohort_age) == expected_data_second_day_conversions))

	################
	# COHORT BASED ON WEEK (EVOLUTION IN WEEKS)
	# test the data so that cohorts are weeks, evolution is shown over weeks
	################
	cohorted_crs <- get_cohorted_crs(	lifecycle_data = simulated_data,
										cohort_type_func = custom_iso_week,
										age_units = 'weeks',
										units_in_age = 1,
										age_limit = NULL,
										cutoff_date_time = ymd_hms(simulated_current_date_time))

	#expected_data <- cohorted_crs
	#saveRDS(expected_data, file = './data/expected_data_cohorted_crs_weeks.RDS')
	expected_data <- readRDS(file = './data/expected_data_cohorted_crs_weeks.RDS')
	expect_true(all(cohorted_crs == expected_data))

	# Test the first couple evolution days.
	expected_data_first_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'weeks') <= 1, na.rm = TRUE) / n()) %>%
		filter(cohort != create_cohort(date_vector = ymd_hms(simulated_current_date_time), custom_iso_week))

	expect_true(all(cohorted_crs %>% filter(cohort_age == 1) %>% select(-cohort_age) ==
					expected_data_first_day_conversions %>% filter(!(cohort %in% c('2017-32', '2017-33')))))

	expected_data_second_day_conversions <- simulated_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = custom_iso_week)) %>%
		group_by(cohort) %>%
		summarise(conversion_rate = sum(difftime(date_converted, date_initial, units = 'weeks') <= 2, na.rm = TRUE) / n()) %>%
		filter(cohort != create_cohort(date_vector = ymd_hms(simulated_current_date_time), custom_iso_week))
	expect_true(all(cohorted_crs %>% filter(cohort_age == 2) %>% select(-cohort_age) == 
					expected_data_second_day_conversions %>% filter(!(cohort %in% c('2017-31',
																					'2017-32',
																					'2017-33')))))

	################
	# should get the same date if we go by days and check evolution after every 7 days than if we do weekly
	################
	cohorted_crs_daily <- get_cohorted_crs(	lifecycle_data = simulated_data,
											cohort_type_func = custom_iso_week,
											age_units = 'days',
											units_in_age = 7,
											age_limit = NULL,
											cutoff_date_time = ymd_hms(simulated_current_date_time))
	cohorted_crs_weekly <- get_cohorted_crs(lifecycle_data = simulated_data,
											cohort_type_func = custom_iso_week,
											age_units = 'weeks',
											units_in_age = 1,
											age_limit = NULL,
											cutoff_date_time = ymd_hms(simulated_current_date_time))
	expect_true(all(cohorted_crs_daily == cohorted_crs_weekly %>% mutate(cohort_age = cohort_age * 7)))

})

test_that("cohort_conversion_rates: plots", {
	# load data and convert it to cohort evolution data
	simulated_data <- readRDS(file = './data/simulated_evolution_data.RDS')
	cohorted_crs <- get_cohorted_crs(	lifecycle_data = simulated_data,
										cohort_type_func = custom_iso_week,
										age_units = 'days',
										units_in_age = 1,
										age_limit = 30,
										cutoff_date_time = ymd_hms(simulated_current_date_time))

	################
	# EVOLUTION
	# test the data so that cohorts are weeks, evolution is shown over days (units_in_age = 1, age_units = 'days) over 30 days (age_limit = 30)
	################	
	test_file <- '../general/example_cohort_cumulative_cr_plot.png'
	file.remove(test_file)
	cohort_cumulative_cr_plot(	cohort_df =  cohorted_crs,
								cohort_indexes_to_label = c('2017-01', '2017-25', '2017-18'), # highlight certain rows
								title = 'Evolution of EventA to EventB Convertsion Rate Over Time (Days)',
								y_label = 'Cummulative conversion rate for cohort',
								caption = '\nShows evolution of each cohort over time (x-axis, days).\nOlder cohorts have more transparent lines.')
	ggsave(filename = test_file, height = 8, width = 14, units = c('in'))
	expect_true(file.exists(test_file))

	################
	# SNAPSHOT
	################
	test_file <- '../general/example_cumulative_cr_snapshot_plot.png'
	file.remove(test_file)
	cumulative_cr_snapshot_plot(cohort_df = cohorted_crs,
								initial_event = 'Initial EventA',
								conversion_event_label = 'EventB CR')
	ggsave(filename = test_file, height = 8, width = 14, units = c('in'))
	file.exists(test_file)

	################
	# RATE OF CHANGE
	################
	test_file <- '../general/example_cumulative_cr_snapshot_plot_rate.png'

	cohort_rate_of_change <- calculate_evolution_rate_of_change(cohort_df = cohorted_crs)
	#expected_data <- cohort_rate_of_change
	#saveRDS(expected_data, file = './data/expected_data_cohort_rate_of_change_days.RDS')
	expected_data <- readRDS(file = './data/expected_data_cohort_rate_of_change_days.RDS')
	expect_true(all(cohort_rate_of_change == expected_data))

	file.remove(test_file)
	cohort_cumulative_cr_plot(	cohort_df = cohort_rate_of_change,
								title = 'Rate of Capture - CR (Days)',
								y_label = 'Incremental % captured (Difference in CR between days)')
	ggsave(filename = test_file, height = 8, width = 14, units = c('in'))
	file.exists(test_file)	
})
