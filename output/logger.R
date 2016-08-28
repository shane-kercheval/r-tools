logger.output_file = './output.txt'

logger.DEBUG = 100
logger.INFO = 200
logger.WARNING = 300
logger.ERROR = 400
logger.NOTE = 10000 #always log LOG, no formatting

logger.threshold = logger.WARNING
logger.use_markdown = FALSE

logger.set_output <- function(output_file)
{
	assign('logger.output_file', output_file, envir = .GlobalEnv) 
}

logger.set_threshold <- function(threshold = logger.DEBUG)
{
	assign('logger.threshold', threshold, envir = .GlobalEnv) 
}

logger.reset_log_file <- function()
{
	if(!is.null(logger.output_file))
	{
		file.remove(logger.output_file)
	}
}

log.DEBUG <- function(message, prefix='', postfix='\n', should_log=TRUE)
{
	log_helper(log_level=logger.DEBUG, log_name='DEBUG', message=message, prefix=prefix, postfix=postfix, should_log=should_log)
}

log.INFO <- function(message, prefix='', postfix='\n', should_log=TRUE)
{
	log_helper(log_level=logger.INFO, log_name='INFO', message=message, prefix=prefix, postfix=postfix, should_log=should_log)
}

log.WARNING <- function(message, prefix='', postfix='\n', should_log=TRUE)
{
	log_helper(log_level=logger.WARNING, log_name='WARNING', message=message, prefix=prefix, postfix=postfix, should_log=should_log)
}

log.ERROR <- function(message, prefix='', postfix='\n', should_log=TRUE)
{
	log_helper(log_level=logger.ERROR, log_name='ERROR', message=message, prefix=prefix, postfix=postfix, should_log=should_log)
}

log.NOTE <- function(message, prefix='', postfix='\n', should_log=TRUE)
{
	log_helper(log_level=logger.NOTE, log_name='NOTE', message=message, prefix=prefix, postfix=postfix, should_log=should_log)
}

log_helper <- function(log_level, log_name, message, prefix, postfix, markdown_function=NULL, should_log=TRUE)
{
	if(log_level >= logger.threshold && should_log)
	{
		if(!is.null(logger.output_file))
		{
			sink(file=logger.output_file, append=TRUE)		
		}

		log_message = sprintf('%s%s%s', prefix, message, postfix)
		if(log_level != logger.NOTE) # NOTE is unformatted, everything else is formatted
		{
			log_message = sprintf('%s\t[%s]: %s%s%s', log_name, Sys.time(), prefix, message, postfix)
		}

		if(logger.use_markdown && !is.null(markdown_function))
		{
			cat(markdown_function(log_message))
		}
		else
		{
			cat(log_message)
		}
		
		if(!is.null(logger.output_file))
		{
			sink() # removes the sink		
		}
	}
}
