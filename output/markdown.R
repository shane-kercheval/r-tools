h1 <- function(text, postfix='')
{
	return (sprintf('# %s%s', text, postfix))
}

h2 <- function(text, postfix='')
{
	return (sprintf('## %s%s', text, postfix))
}

h3 <- function(text, postfix='')
{
	return (sprintf('### %s%s', text, postfix))
}

h4 <- function(text, postfix='')
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

codeb <- function(text, postfix='', syntax='')
{
	return (sprintf('```%s\n%s\n```%s', syntax, text, postfix))
}

blockq <- function(text, postfix='')
{
	return (sprintf('>%s%s', text, postfix))
}

table_matrix <- function(a_matrix, title=NULL, row_header='Row', title_format=h1, postfix='\n')
{
	table_markup = ''
	if(!is.null(title))
	{
		table_markup = title_format(title)
	}
	
	col_names = colnames(a_matrix)
	row_names = rownames(a_matrix)
	
	headers = c(row_header, col_names)
	header_markup = paste(headers, collapse = '|')
	header_separater = paste(rep('-',length(col_names)+1), collapse = '|')
	table_markup = sprintf('%s%s\n%s\n', table_markup, header_markup, header_separater)
	
	vals = apply(format(a_matrix, trim=TRUE), 1, paste, collapse="|")
	
	for (i in 1:length(row_names))
	{
		table_markup = sprintf('%s%s%s%s\n', table_markup, row_names[i], '|', vals[i])
	}
	table_markup = sprintf('%s%s', table_markup, postfix)
	return (table_markup)
}
