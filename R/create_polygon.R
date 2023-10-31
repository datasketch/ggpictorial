
#' @export
create_polygon <- function(n, center = c(0, 0), phi = 0, radius = 1) {
  # Ensure n is at least 3
  if (n < 3) {
    stop("A polygon must have at least 3 sides.")
  }

  phi0 <- 0
  if(n == 3) phi0 <- -pi/6
  if(n == 4) phi0 <- pi/4
  if(n == 5) phi0 <- pi/5/2
  if(n == 6) phi0 <- 0


  # Calculate the coordinates for each vertex
  angles <- seq(0, 2*pi, length.out = n + 1) # +1 because start and end point is same
  x_coords <- center[1] + radius * cos(angles + phi0 + phi)
  x_coords <- c(x_coords[1:n], x_coords[1])
  y_coords <- center[2] + radius * sin(angles+ phi0 + phi)
  y_coords <- c(y_coords[1:n], y_coords[1])

  # Convert to matrix and then to sf polygon
  coords <- matrix(c(x_coords, y_coords), ncol = 2)
  poly <- st_polygon(list(coords))
  sf_poly <- st_sf(geometry = st_sfc(poly))

  sf_poly
}

#' @export
translate_polygon <- function(sf_poly, x_shift = 0, y_shift = 0) {
  # Check if the input object is of class "sf"
  if (!inherits(sf_poly, "sf")) {
    stop("Input must be of class 'sf'.")
  }

  # Transform the coordinates
  shifted_geometry <- st_geometry(sf_poly) + c(x_shift, y_shift)

  # Update the geometry of the original sf object
  st_geometry(sf_poly) <- shifted_geometry

  return(sf_poly)
}


combine_polygons <- function(...){
  rbind(...)
}



# Rotate function for a single matrix of coordinates
rotate_coordinates <- function(coords, centroid, angle) {
  rotation_matrix <- matrix(c(cos(angle), -sin(angle),
                              sin(angle),  cos(angle)), ncol=2)
  centered_coords <- coords - centroid # Translate to origin
  rotated_coords <- matrix(c(centered_coords %*% rotation_matrix), ncol=2)

  make_sure_close_coords(rotated_coords + centroid)
}

make_sure_close_coords <- function(mat){
  n <- nrow(mat)
  mat[n,] <- mat[1,]
  mat
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









library(sf)

recenter_to_origin <- function(sf_polygon) {
  # Calculate the centroid of the combined polygon
  centroid <- st_coordinates(st_centroid(sf_polygon))[1, 1:2]

  # Translate the coordinates to recenter the polygon
  translated_coords <- lapply(
    sf_polygon$geometry[[1]],
    function(coords) {
      cbind(
        coords[, 1] - centroid[1],
        coords[, 2] - centroid[2]
      )
    }
  )

  # Return the translated polygon as an sf object
  st_sf(geometry = st_sfc(do.call(st_polygon, list(translated_coords))))
}

# Create two example polygons
coords <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow = TRUE)
coords2 <- matrix(c(0,0, 2,0, 2,2, 0,2, 0,0), ncol=2, byrow = TRUE)

polygon1 <- st_polygon(list(coords))
sf_polygon1 <- st_sf(geometry = st_sfc(polygon1))
polygon2 <- st_polygon(list(coords2))
sf_polygon2 <- st_sf(geometry = st_sfc(polygon2))

combined_sf <- rbind(sf_polygon1, sf_polygon2)

# Recenter the combined polygon
recentered_polygon <- recenter_to_origin(combined_sf)

# Plot original (blue) and recentered (red) polygons
plot(st_geometry(combined_sf), col=NA, border="blue", lwd=2, xlim = c(-3, 3), ylim = c(-3, 3))
plot(st_geometry(recentered_polygon), add=TRUE, col=NA, border="red", lwd=2)

