test_that("Pictorial", {

  icon <- "tmp/noun/noun-person-1218618.svg"
  fill <- 0.1
  pictorial(icon, fill)


  icon <- "tmp/noun/noun-house-6194195.svg"
  fill <- 0.8
  pictorial(icon, fill)

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
