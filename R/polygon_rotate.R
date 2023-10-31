


# Rotate function for a single matrix of coordinates
rotate_coordinates <- function(coords, centroid, angle) {
  rotation_matrix <- matrix(c(cos(angle), -sin(angle),
                              sin(angle),  cos(angle)), ncol=2)
  centered_coords <- coords - centroid # Translate to origin
  rotated_coords <- matrix(c(centered_coords %*% rotation_matrix), ncol=2)

  make_sure_close_coords(rotated_coords + centroid)
}


# Rotate function for sf polygons (can handle multiple polygons)
rotate_polygon <- function(sf_polygons, angle) {
  polygons <- st_geometry(sf_polygons)
  centroid <- st_coordinates(st_centroid(sf_polygons))[1, 1:2]

  rotated_polygons <- lapply(1:length(polygons), function(i) {
    # Get the centroid of the individual polygon
    #centroid <- st_coordinates(st_centroid(polygons[[i]]))[1, 1:2]

    # Rotate the individual polygon
    rotated <- lapply(polygons[[i]], rotate_coordinates, centroid=centroid, angle=angle)
    st_polygon(rotated)
  })

  # Return the rotated polygons as an sf object
  st_sf(geometry = st_sfc(rotated_polygons))
}


