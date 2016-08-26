h1 <- function(text, postfix='\n')
{
	return (sprintf('# %s%s', text, postfix))
}

h2 <- function(text, postfix='\n')
{
	return (sprintf('## %s%s', text, postfix))
}

h3 <- function(text, postfix='\n')
{
	return (sprintf('### %s%s', text, postfix))
}

h4 <- function(text, postfix='\n')
{
	return (sprintf('#### %s%s', text, postfix))
}

bold <- function(text, postfix='')
{
	return (sprintf('**%s**%s', text, postfix))
}

ital <- function(text, postfix='')
{
	return (sprintf('_%s_%s', text, postfix))
}

strike <- function(text, postfix='')
{
	return (sprintf('~~%s~~%s', text, postfix))
}

code <- function(text, postfix='')
{
	return (sprintf('`%s`%s', text, postfix))
}

codeb <- function(text, postfix='\n', syntax='')
{
	return (sprintf('```%s\n%s\n```%s', syntax, text, postfix))
}

bqoute <- function(text, postfix='\n')
{
	return (sprintf('>%s%s', text, postfix))
}
