# define function to compute the log of some values
# and returns a vector
log_with_vector <- function(some_values) {
  
  # define variable that will be returned
  computed_log <- vector()
  
  # check whether the log can be computed
  # i.e. for zero, there is no log
  for (a_value in some_values) {
    # test the value and compute its log
    if(a_value != 0) {
      computed_log <- append(computed_log, log(a_value))
    } else {
      computed_log <- append(computed_log, NA)
    }
  }
  # finally, return the logs
  return(computed_log)
}

# define function to compute the log of some values
# and returns a count-named list
log_with_nums_one <- function(some_values) {
  
  # define variable that will be returned
  computed_log <- list()
  
  # define variable to count, starting at 1
  count <- 1
  
  # check whether the log can be computed
  # i.e. for zero, there is no log
  for (a_value in some_values) {
    # test the value and compute its log
    if(a_value != 0) {
      computed_log[[count]] <- log(a_value)
    } else {
      computed_log[[count]] <- NA
    }
    # update counter AFTER USING IT
    count <- count + 1
  }
  # finally, return the logs
  return(computed_log)
}

# SAME AS ABOVE, WITH COUNTER STARTING AT ZERO
# define function to compute the log of some values
# and returns a count-named list
log_with_nums_zero <- function(some_values) {
  
  # define variable that will be returned
  computed_log <- list()
  
  # define variable to count, starting at zero
  count <- 0
  
  # check whether the log can be computed
  # i.e. for zero, there is no log
  for (a_value in some_values) {
    # update counter BEFORE USING IT
    count <- count + 1
    # test the value and compute its log
    if(a_value != 0) {
      computed_log[[count]] <- log(a_value)
    } else {
      computed_log[[count]] <- NA
    }
  }
  # finally, return the logs
  return(computed_log)
}

# define function to compute the log of some values
# and return a object-named list
# if the list contains the same value multiple times
# the field with that name will be updated
log_with_names <- function(some_values) {
  
  # define variable that will be returned
  computed_log <- list()
  
  # check whether the log can be computed
  # i.e. for zero, there is no log
  for (a_value in some_values) {
  
    # create label for the list
    lbl <- as.character(a_value)
    
    # test the value and compute its log
    if(a_value != 0) {
      computed_log[[lbl]] <- log(a_value)
    } else {
      computed_log[[lbl]] <- NA
    }
  }
  # finally, return the logs
  return(computed_log)
}

# define function to compute the log of some values
# and returns a count-named list
log_with_while <- function(some_values) {
  
  # define variable that will be returned
  computed_log <- list()
  
  # define variable to count, starting at 1
  count <- 1
  
  # check whether the log can be computed
  # i.e. for zero, there is no log
  while (count <= length(some_values)) {
    # get value n. count from some_values
    a_value <- some_values[count]
    
    # test the value and compute its log
    if(a_value != 0) {
      computed_log[[count]] <- log(a_value)
    } else {
      computed_log[[count]] <- NA
    }
    # update counter AFTER USING IT
    count <- count + 1
  }
  # finally, return the logs
  return(computed_log)
}

# run a test
some_values <- c(1, 3, 5, 6, 0, 7)

log_with_while(some_values)
log_with_names(some_values)
log_with_nums_zero(some_values)
log_with_nums_one(some_values)
log_with_vector(some_values)
