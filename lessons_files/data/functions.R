# define the function average for two values
arithmetic_mean <- function(a_set_of_values) {
	# sum the two values and divide by the number of values
	the_sum <- sum(a_set_of_values)
	# compute the number of values in the vector
	the_n_of_vals <- length(a_set_of_values)
	# compute the average
	average <- the_sum/the_n_of_vals
	# return the average value we just computed
	return(average)
}
