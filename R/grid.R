

make_grid <- function(n, ncol = NULL, nrow = NULL,
                      shape = NULL, fill = "bottom"){

  if(is_square(n)){
    shape <- shape %||% "square"
  } else {
    shape <- shape %||% "rectangle"
  }

  if(shape == "rectangle"){
    #nrow <- floor(sqrt(n))

    if(is.null(ncol) && is.null(nrow)){
      factors <- find_middle_factors(n)
      ncol <- max(factors)
      nrow <- min(factors)
    } else if(is.null(ncol) && !is.null(nrow)) {
      ncol <- n/nrow
    } else if(is.null(nrow) && !is.null(ncol)){
      nrow <- n/ncol
    }
    if(nrow * ncol != n)
      stop("nrow and ncol must multiply to n")
  }
  if(shape == "square"){
    if(is_square(n)){
      ncol <- nrow <- sqrt(n)
    }
  }


  v <- 1:n
  m <- matrix(v, ncol = ncol, byrow = FALSE)
  if(fill == "botton") m <- apply(m, 1, rev)

  # Convert matrix to dataframe
  df <- matrix_to_position_df(m)
  df
}

matrix_to_position_df <- function(m) {
  nrow <- nrow(m)
  ncol <- ncol(m)

  # Create a grid of row and column indices
  indices <- expand.grid(x = 1:ncol, y = 1:nrow)

  # Map indices to values
  indices$idx <- c(m)

  # Order by row first, which is typical for data frames
  indices <- indices[order(indices$y, indices$x), ]
  indices
}




is_square <- function(n){
  as.integer(sqrt(n)) == sqrt(n)
}



find_two_factors <- function(number) {
  if (number <= 1) {
    stop("The number should be greater than 1.")
  }

  # If the number is a prime or 1, it can only be factored into 1 and itself.
  for (i in 2:floor(sqrt(number))) {
    if (number %% i == 0) {
      return(c(i, number / i))
    }
  }

  # If no factors found other than 1 and the number itself, it is a prime number.
  return(c(1, number))
}

find_middle_factors <- function(number) {
  if (number <= 1) {
    stop("The number should be greater than 1.")
  }

  sqrt_num <- sqrt(number)
  lower_factor <- floor(sqrt_num)
  while (lower_factor > 1) {
    if (number %% lower_factor == 0) {
      return(c(lower_factor, number / lower_factor))
    }
    lower_factor <- lower_factor - 1
  }

  return(c(1, number))  # If the number is prime
}



