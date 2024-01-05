

make_grid <- function(n, ncol = NULL, nrow = NULL,
                      fill = "bottom",
                      ar = NULL,
                      exact_n = FALSE){

  # If AR and not grid_shape
  ration <- NULL
  if(!is.null(ar)){
    ar_nums <- as.numeric(strsplit("1:1", split = ":")[[1]])
    ratio <- ar_nums[1]/ar_nums[2]
  } else {
    if(is_square(n)){
      ratio <- 1
    }else{
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
        stop("nrow * ncol must be equal to n")
    }
  }


  if(shape == "rectangle"){
    #nrow <- floor(sqrt(n))


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



best_2_factors <- function(n, ratio
                           #tol = 0.05
                           ){
  #n <- 135
  #ratio <- 1.33
  rat <- ratio
  value <- n
  fctrs <- ggpictorial::factors

  fs <- fctrs |>
    dplyr::filter(n >= value) |>
    dplyr::filter(n <= 2 * value) |>
    dplyr::mutate(ratio1 = abs(ratio - rat),
                  ratio2 = abs(1/ratio - 1/rat))

  fs2 <- fs |>
    dplyr::mutate(rat_dist = pmin(ratio1,ratio2)) |>
    dplyr::mutate(rat_dist2 = rat_dist + (n - value + 1)) |>
    dplyr::arrange(rat_dist)
  fs3 <- fs2 |>
    dplyr::slice(1)

  dim <- c(fs3$n2, fs3$n1)
  if(ratio > 1){
    dim <- rev(dim)
  }
  dim

}




find_2_factors <- function(n) {
  factors <- c()
  for (i in 1:sqrt(n)) {
    if (n %% i == 0) {  # Check if 'i' is a factor
      factors <- rbind(factors, c(i, n / i))
    }
  }
  return(factors)
}


find_first_2_factors <- function(number) {
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




is_square <- function(n){
  as.integer(sqrt(n)) == sqrt(n)
}



