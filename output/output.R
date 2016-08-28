library(stringr)

note_if <- function(x, description=NULL, should_note=FALSE)
{
	if(should_note)
	{
		if(!is.null(description))
		{
			cat(paste("####### ", description, " ###########"))
		}

		cat(sprintf('%s\n', x))
	}
}

note <- function(text)
{
	cat(sprintf('\n###: %s\n', text))
}

capture <- function(something)
{
	return (capture.output(something)) # http://stackoverflow.com/questions/16358435/in-r-is-it-possible-to-redirect-console-output-to-a-variable
}

print_c <- function(something, collapse=TRUE, trim=TRUE)
{
	output_something = capture.output(print(something))
	if(collapse)
	{
		output_something = paste(output_something, collapse = '\n')
	}

	if(trim)
	{
		output_something = str_trim(output_something)
	}

	return (output_something) # http://stackoverflow.com/questions/16358435/in-r-is-it-possible-to-redirect-console-output-to-a-variable	
}
