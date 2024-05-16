test_that("Make grid", {




  # With AR


  n <- 12
  ncol <- 3
  df <- make_grid(n, ncol = ncol)
  ggplot(df) + geom_text(aes(y,x, label = idx))

  n <- 100
  df <- make_grid(n)

  ggplot(df) + geom_text(aes(y,x, label = idx))

  n <- 55
  df <- make_grid(n)
  ggplot(df) + geom_text(aes(y,x, label = idx))

  n <- 28
  df <- make_grid(n)
  ggplot(df) + geom_text(aes(y,x, label = idx))






})

test_that("grid factors", {



  #
  best_2_factors(135, ratio = 0.3)
  best_2_factors(135, ratio = 1.333, tol = 0.3)

  # best_2_factors(1000, ratio = 1/10)


  expect_true(is_square(100))
  expect_false(is_square(1000))
  expect_true(is_square(10000))


  # find 2 factors

  find_2_factors(123900)




  # Example usage
  find_two_factors(28)  # Should return 4 and 7 since 28 = 4 * 7
  find_middle_factors(28)

  find_two_factors(366)
  find_middle_factors(366)


  find_two_factors(55)
  find_two_factors(1001)

  n <- 284320
  find_middle_factors(n)





})



