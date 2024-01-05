test_that("", {

  df <- tibble(
    categories = c("house", "building"),
    values = c(20, 35)
  )
  pictorial_grid(df, color = "green")

  df <- tibble::tribble(
    ~categories, ~values, ~icon,
    "Economic", 10, "male",
    "Social", 50,"female",
    "Environmental", 2, "book"
  )
  pictorial_grid(df, title = "Hello")


  value <- 0.31
  pictorial_unit(value, icon_name = "square")
  pictorial_unit(value, icon_name = "square", palette = c("orange", "yellow"))
  pictorial_unit(value, icon_name = "square", palette = rainbow(5))



  value <- 0.998
  pictorial_unit(value, icon_name = "square")

  value <- 1/366
  pictorial_unit(value, icon_name = "square")


  df <- tibble::tribble(
    ~categories, ~values,
    "day", 1,
    "rest", 365
  )
  pictorial_grid(df, palette = c("#cc9a00", "#ffffff"),
                color = "#c5c5c5", ncol = 6)

  value <- 0.9
  pictorial_unit(value, icon_name = "circle")


})
