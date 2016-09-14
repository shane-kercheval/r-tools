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

print_c <- function(something, collapse='\n', trim=FALSE, print_row_names=FALSE)
{
	output_something = capture.output(print(something, row.names = print_row_names))
	if(!is.null(collapse))
	{
		output_something = paste(output_something, collapse=collapse)
	}

	if(trim)
	{
		output_something = str_trim(output_something)
	}

	return (output_something) # http://stackoverflow.com/questions/16358435/in-r-is-it-possible-to-redirect-console-output-to-a-variable	
}
