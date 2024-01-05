library(tidyverse)

ns <- 1:1e5
factors <- purrr::map_df(ns, function(n) {
  if(n %% 1000 == 0) message(n)
  f <- find_2_factors(n)
  colnames(f) <- c("n1", "n2")
  f <- as_tibble(f)
  f$n <- n
  f |>
    mutate(ratio = n1/n2)
})
factors <- factors |> select(n, n1, n2, ratio)
usethis::use_data(factors, overwrite = TRUE)

