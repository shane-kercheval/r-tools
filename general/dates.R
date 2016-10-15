convert_to_date <- function(vector_string_date, date_string_format='%Y-%m-%d', has_time=TRUE)
{
	stop('use lubridate::ymd')
	# cols = t(sapply(strsplit(x=vector_string_date, split=' '),c))
	# if(has_time)
	# {
	# 	return (as.Date(cols[,1], format=date_string_format))
	# }
	# return (as.Date(cols[1, ], format=date_string_format))
}

convert_to_date_time <- function(date_time_string_vector, date_time_string_format="%Y-%m-%d %H:%M:%S", time_zone='GMT')
{
	stop('use lubridate::ymd_hms')
	# return (as.POSIXct(strptime(date_time_string_vector, format=date_time_string_format, tz=time_zone), tz=time_zone))
}

add_date_columns <- function(data_frame, date_column)
{# adds year, month, and week date_columns to data frame for grouping purposes, assumes ymd field.
	data_frame$year = as.numeric(format(data_frame[, date_column], '%Y'))
	data_frame$month = as.numeric(format(data_frame[, date_column], '%m'))
	data_frame$day_of_month = as.numeric(format(data_frame[, date_column], '%d'))
	data_frame$weekday =  factor(weekdays(data_frame[, date_column]),levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),ordered=TRUE)
	weeks = data.frame(Dates=data_frame[, date_column], Week = as.numeric(format(data_frame[, date_column], format = '%W')))
	data_frame$week = weeks$Week
	data_frame$day_of_year =  as.numeric(strftime(data_frame[, date_column], format = '%j'))
	return (data_frame)
}

string_to_date <-function(date_string)
{
	stop('use lubridate::ymd')
	#return (as.Date(date_string, format='%Y-%m-%d'))
}
