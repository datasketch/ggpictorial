
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
combine_polygons <- function(...){
  rbind(...)
}




