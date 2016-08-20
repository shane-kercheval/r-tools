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
