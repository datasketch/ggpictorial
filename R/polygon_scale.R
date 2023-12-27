

library(sf)

scale_polygon <- function(sf_polygon, scale) {

  # Compute the centroid of the polygon for scaling from the center
  centroid <- st_coordinates(st_centroid(sf_polygon))[1, 1:2]

  # Translate the polygon to the origin
  translated_to_origin <- st_geometry(sf_polygon) - centroid

  # Scale the polygon
  scaled_geometry <- translated_to_origin * scale

  # Translate the polygon back to its original position
  final_geometry <- scaled_geometry + centroid

  # Update the geometry of the sf object

  st_sf(geometry = sf::st_sfc(final_geometry))

  # st_geometry(sf_polygon) <- final_geometry
  #
  # return(sf_polygon)
}

