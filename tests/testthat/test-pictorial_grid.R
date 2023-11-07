test_that("", {

  df <- tibble(
    categories = c("house", "building"),
    values = c(20, 35)
  )
  pictorial_grid(df, color = "green")

  value <- 0.31
  pictorial_unit(value, icon_name = "square")

  value <- 0.998
  pictorial_unit(value, icon_name = "square")

  value <- 0.9
  pictorial_unit(value, icon_name = "circle")


})
