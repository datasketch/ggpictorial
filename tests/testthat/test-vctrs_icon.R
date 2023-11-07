test_that("multiplication works", {

  iconR6()
  iconR6(NA)
  new_Icn()
  new_Icn(NA)
  Icn(NA)


  # Create from an iconR6
  x <- iconR6("square", phi = pi/2)
  expect_true(is_iconR6(x))

  icn <- new_Icn("circle")
  as.character(icn)
  expect_true(is_Icn(icn))

  x <- Icn("square", center = c(10, 10), phi = pi / 4)
  x
  Icn_object(x)

  x <- Icn("square", center = c(2, 5), radius = 0.7)
  x
  Icn_object(x)


  x <- Icn("square", color = "green")
  Icn_object(x)

  # Create from vectors

  # Vector of names
  x <- c("square", "triangle")
  Icn(x)
  x <- Icn(x)
  Icn(x)

  # Vectors or iconR6
  x <- c(iconR6("square"), iconR6("circle",center = c(5, 5), phi = pi / 6))
  xi <- Icn(x)
  xi
  xi[1]
  xi[2]
  Icn(xi)


  # Coercion

  x <- Icn("square")
  x
  y <- Icn("circle")
  y
  c(x, y)
  c(Icn("square"), Icn("circle"))




  c(Icn("square"), "triangle")

  c("triangle", Icn("square"))





  d <- tibble(icons = icons, a = 1:2)

  x <- d$icons[1]
  x
  str(x)
  class(x)
  y <- d$icons[2]
  y

  c(a1, a2)


  d <- tibble(a = 1:2)
  d$icon <- Icn("square")
  d



})
