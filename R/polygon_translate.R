


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


#' @export
recenter_polygon_at <- function(sf_polygon, x, y){
  sf_polygon <- recenter_polygon(sf_polygon)
  translate_polygon(sf_polygon, x, y)
}


#' @export
recenter_polygon <- function(sf_polygon){
  centroid <- st_coordinates(st_centroid(sf_polygon))[1, 1:2]

  translate_polygon(sf_polygon, x_shift = -centroid[1],
                    y_shift = -centroid[2])
}


