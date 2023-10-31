test_that("multiplication works", {


  # Example usage:
  coords <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow = TRUE)
  polygon <- st_polygon(list(coords))
  sf_polygon <- st_sf(geometry = st_sfc(polygon))

  plot(sf_polygon)

  # Scale the polygon by a factor of 2 in x and 3 in y
  scaled_sf_polygon <- scale_polygon(sf_polygon, 2)
  plot(st_geometry(sf_polygon), col=NA, border="blue", xlim = c(-3, 3), ylim = c(-3, 3))
  plot(st_geometry(scaled_sf_polygon), add=TRUE, col=NA, border="red", lwd=2)


})
