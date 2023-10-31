test_that("multiplication works", {


  triangle <- create_polygon(3)
  plot(triangle)

  triangle2 <- translate_polygon(triangle, x_shift = 1, y_shift = 2)
  plot(triangle2)

  combined_sf <- rbind(triangle, triangle2)
  plot(combined_sf)

  square <- create_polygon(4)
  plot(square)

  comb <- combine_polygons(triangle, triangle2, square)
  plot(comb)

  translated_comb <- translate_polygon(comb, x_shift = 0.5, y_shift = 0.5)
  plot(translated_comb, border = "green", add = TRUE)

  recentered_polygon <- recenter_polygon(comb)
  plot(recentered_polygon, border = "red", add = TRUE)

  rotated <- rotate_polygon(comb, pi/4)
  plot(rotated, border = "blue", add = TRUE)

  pentagon <- create_polygon(5)
  plot(pentagon)

  hexagon <- create_polygon(6)
  plot(hexagon)



})

test_that("Rotate works", {


  # Example usage
  coords <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow = TRUE)
  coords2 <- matrix(c(0,0, 2,0, 2,2, 0,2, 0,0), ncol=2, byrow = TRUE)

  polygon1 <- st_polygon(list(coords))
  sf_polygon1 <- st_sf(geometry = st_sfc(polygon1))
  polygon2 <- st_polygon(list(coords2))
  sf_polygon2 <- st_sf(geometry = st_sfc(polygon2))

  combined_sf <- rbind(sf_polygon1, sf_polygon2)
  plot(st_geometry(combined_sf), main="Original and Rotated Polygons")

  # Rotate the polygons by 45 degrees (0.7854 radians)
  rotated_sf_polygons <- rotate_polygons(combined_sf, 0.7854)

  plot(rotated_sf_polygons, border = "red", add = TRUE)


})


