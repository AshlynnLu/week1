

#' Calculate the geometric mean of a numeric vector.
#'
#' @param x numeric vector
#' @param remove_NA logical scalar, indicating whether NA values should be 
#'                  stripped before computation proceeds.
#'
#' @return the geometric mean of the values in `x`, a numeric scalar value.
#'
#' @examples 
#' geom_mean(x = 1:10)
#' geom_mean(x = c(1:10, NA), remove_NA = TRUE)
geom_mean <- function(x, remove_NA = FALSE){
  prod <- 1
  
  if(remove_NA == TRUE){
    x <- na.omit(x)
  }
  
  N <- length(x)
  
  for (i in 1:N) {
    prod <- prod * x[i]
  }
  return(prod^(1/N))
}

# Example usage:
geom_mean(x = 1:10)
geom_mean(x = c(1:10, NA), remove_NA = TRUE)


# window position? 
rolling_mean <- function(x, window_size) {
  N <- length(x)
  end <- N - window_size + 1
  res <- numeric(end)
  
  if (window_size > N){
    print("The window size should be shorter than the length of the vector.")
  }
  else{
      for (i in 1:end) {
    res[i] <- mean(x[i:(i+window_size-1)], na.rm = TRUE)
  }
  return(res)
  }
}

# Example usage:
my_vector <- c(3, 5, 2, 8, 4, 7, 1, 9, 6)
window_size <- 3

result <- rolling_mean(my_vector, window_size)
print(result)
