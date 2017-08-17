library(lubridate)
library(tidyverse)
library(ggplot2)

custom_iso_week <- function(date) {
	if(isoweek(date) == 52 & month(date) == 1) { # isoweek seems to classify some initial days of the year as week 52 which is fucked up; if this is the case, we will return 1; this might cause the first week to an oversized sample (it is putting more samples into the next week that it would have classified as week 1)
		return (1)
	} else {
		return (isoweek(date))
	}
}

create_cohort <- function(date_vector, cohort_type_func) {

	return (map_chr(date_vector, ~ paste0(year(.),'-', str_pad(cohort_type_func(.), 2, side = 'left', pad = '0')))) # this will work for weeks/months
}

get_cohorted_crs <- function(lifecycle_data, cohort_type_func = custom_iso_week, age_units = 'days', units_in_age = 7, age_limit = NULL, cutoff_date_time = Sys.time()) {

	lifecycle_cohorts <- lifecycle_data %>%
		mutate(cohort = create_cohort(date_vector = date_initial, cohort_type_func = cohort_type_func))

	unique_cohorts <- sort(unique(lifecycle_cohorts$cohort))

	# empty data frame
	cohort_conversion_rates_df <- data.frame(cohort = character(),
											 cohort_age = numeric(),
											 cummulative_cr = numeric())

	stopifnot(!any(is.na(lifecycle_cohorts$date_initial)))

	if(is.null(age_limit)) {
		age_limit <- length(unique_cohorts)
	}
	# for each cohort, for each cohort_age during / after the cohort, get the cumulative CR to X over time (i.e. more people in the cohort will have converted each additional week, we want to see total CR over time)
	for(index in 1:length(unique_cohorts)) {

		current_cohort <- lifecycle_cohorts %>%
			filter(cohort == unique_cohorts[index]) %>% # get all the observations created in a specific week 
			mutate(time_creation_to_x = difftime(date_converted, date_initial, units = age_units))
		# so we look at all the observations that were created in week X. Regardless if they create the observation on a Monday, or a Friday, see how many of those observations converted within 7 days, 14 days, etc.

		stopifnot(length(unique(current_cohort$cohort)) == 1) # all these values should match
	
		# the max age that should be considered is 'youngest' of the current cohort population..
		# For example, the current cohort is from last week. And today is Tuesday, and there were observations
		# that eventB (for the first time) on Sunday, those observations have only had (e.g.) 2
		# days to do EventB, so that's the maximum age we can go to. Any age beyond that means we haven't
		# given the cohort a chance for everyone to convert within that that timeframe. If the age was 7,
		# for example, the observations that had eventB on Sunday would not have had 7 days to do the conversion. 
		max_age = min(floor(as.numeric(difftime(cutoff_date_time, current_cohort$date_initial, units = age_units))))
		for(cohort_age in 1:age_limit) {
			cohort_age <- cohort_age * units_in_age

			if(cohort_age > max_age) { 
				break
			}
			
			cummulative_conversions <- sum(	!is.na(current_cohort$date_converted) &
											(current_cohort$time_creation_to_x <= cohort_age))
			cummulative_cr <- cummulative_conversions / nrow(current_cohort)
			cohort_conversion_rates_df <- rbind(cohort_conversion_rates_df, 
												data.frame(	cohort = unique_cohorts[index],
															cohort_age = cohort_age,
															cummulative_cr = cummulative_cr))
		}
	}
	return (cohort_conversion_rates_df)
}

cohort_cumulative_cr_plot <- function(cohort_df, title, y_label, x_label = 'Number of days after EventA', caption = '') {
	# the inner join selects all the rows from cohort_df that have the max age i.e. latest cummulative CR
	final_cohorts <- inner_join(cohort_df,
								cohort_df %>%
									group_by(cohort) %>%
									summarise(cohort_age = max(cohort_age)), by = c('cohort', 'cohort_age'))

	unique_cohorts <- sort(unique(cohort_df$cohort))

	graph_attributes <- data.frame(	cohort = unique_cohorts,
									alpha = seq(from = 0.5, to = 1, 
												length.out = length(unique_cohorts)))

	cohort_df <- inner_join(cohort_df, graph_attributes, by = 'cohort')

	y_min_max <- c(	floor(min(cohort_df$y_value) / 0.025) * 0.025,
					ceiling(max(cohort_df$y_value) / 0.025) * 0.025)
	cr_plot <- ggplot(data = cohort_df, aes(x = cohort_age, y = y_value, col = cohort, group = cohort)) +
		geom_line(aes(alpha = alpha), size = 1.0) +
		coord_cartesian(ylim = y_min_max) +
		scale_y_continuous(breaks = seq(from = y_min_max[1], to = y_min_max[2], by = 0.025), labels = scales::percent) +
		geom_label(data =  subset(final_cohorts, cohort_age < 10 | cohort == '2017-01' | cohort == '2017-25' | cohort == '2017-18'), aes(label = cohort),
				   nudge_x = 1.1, alpha = 1, na.rm = TRUE) +
		#geom_text(	data = subset(final_cohorts, cohort_age < 10),
		#			aes(x = cohort_age, y = y_value, label = cohort, vjust = 0.3, hjust = -0.1)) + #, check_overlap = TRUE) + 
		scale_x_continuous(breaks = seq(from = 1, to = max(final_cohorts$cohort_age), by = 1)) +
		labs(	title = title,
				x = x_label,
				y = y_label,
				caption = caption) + guides(alpha = FALSE)

	return (cr_plot)
}

cumulative_cr_snapshot_plot <- function(cohort_df, snapshot_ages = c(1, 7, 30), age_label = 'days', event_label, initial_event, highlight_cohort_labels = NULL) {
	
	age_labels = paste(snapshot_ages, age_label)
	
	snapshot_corhot_data <- cohort_df %>%
		filter(cohort_age %in% snapshot_ages) %>%
		mutate(cohort_age = factor(cohort_age, labels = age_labels))

	y_min_max <- c(	floor(min(snapshot_corhot_data$cummulative_cr) / 0.05) * 0.05,
					ceiling(max(snapshot_corhot_data$cummulative_cr) / 0.05) * 0.05)

	cohort_labels <- NULL
	if(!is.null(highlight_cohort_labels)) {

		cohort_labels <- ifelse(sort(unique(snapshot_corhot_data$cohort)) %in% highlight_cohort_labels, 'red', 'black')
	}
	
	snapshot_plot <- ggplot(data = snapshot_corhot_data, aes(x = cohort, y = cummulative_cr, group = cohort_age, colour = cohort_age)) +
		geom_line() + geom_point() + geom_smooth(method = 'loess', se = FALSE) +
		coord_cartesian(ylim = y_min_max) +
		scale_y_continuous(breaks = seq(from = y_min_max[1], to = y_min_max[2], by = 0.025), labels = scales::percent) +
		theme(axis.text.x = element_text(angle = 60, hjust = 1, colour = cohort_labels)) +
		labs(	title = paste('Snapshot of Evolution of ', event_label, 'After X', age_label,'from', initial_event),
				y = '% conversion rate (for each cohort)',
				x = paste0('Cohort (i.e week of ', initial_event, ')'),
				colour = 'Snapshot',
				caption = paste('\nThis graph shows a snapshot of the conversion rate after the first x', age_label, 'of one cohort\ncompared to the conversion rate after x', age_label, 'of another cohort.\nSo, we can accurately compare an older group of observations to a younger group.'))
	return (snapshot_plot)	
}