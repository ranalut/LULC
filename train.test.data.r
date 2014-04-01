
# Identify Test Data Records

create.test.data <- function(row.numbers, proportion, file.name)
{
	n <- ceiling(proportion * length(row.numbers))
	test.rows <- sample(row.numbers,size=n)
	write(test.rows, file=file.name)
	# return(test.rows)
}

# Drop test rows from full row list
drop.test.rows <- function(row.numbers, test.rows)
{
	temp <- row.numbers %in% test.rows
	output <- row.numbers[temp==FALSE]
	return(output)
}

