test_that("Pictorial", {

  icon <- "tmp/noun/noun-person-1218618.svg"
  value <- 0.1
  pictorial_icon(value, icon)

  icon <- "~/Downloads/petro.jpeg"
  value <- 0.34
  pictorial_icon(value, icon)


  icon <- "tmp/noun/noun-house-6194195.svg"
  value <- 0.8
  pictorial_icon(value, icon, fill = "green")




  icon <- "tmp/moon-daguerre.png"
  value <- 0.5
  pictorial_icon(value, icon, fill = "lightblue",
                 bg_color = "yellow",
                 na_color = "white")

  icon <- "tmp/gato-amalia.jpeg"
  value <- 0.3
  pictorial_icon(value, icon, fill = "yellow",
                 bg_color = "black",
                 na_color = "lightblue")


  icon <- "tmp/perro-amalia.jpeg"
  value <- 0.1
  pictorial_icon(value, icon, fill = "green",
                 bg_color = "gray30",
                 na_color = "orange")




})


test_that("Rotation works", {


  library(sf)


  # Example usage
  coords <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow = TRUE)
  polygon <- st_polygon(list(coords))
  sf_polygon <- st_sf(geometry = st_sfc(polygon))

  # Rotate the polygon by 45 degrees (0.7854 radians)
  rotated_sf_polygon <- rotate_polygon(sf_polygon, 0.7854)

  # Plot original and rotated polygons
  plot(st_geometry(sf_polygon), col='blue', main="Original (Blue) and Rotated (Red) Polygons")
  plot(st_geometry(rotated_sf_polygon), col='red', add=TRUE)





})
