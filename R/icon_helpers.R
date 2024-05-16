

icon_read_raster <- function(path){

  if(dstools::file_ext(path) == "svg"){
    tmp <- TRUE
    icon_path_tmp <- svg_to_png(path)
  }else if(dstools::file_ext(path) == "jpeg"){
    icon_path_tmp <- jpeg_negate(path)
    tmp <- TRUE
  }
    else{
    icon_path_tmp <- path
    tmp <- FALSE
  }

  r <- terra::rast(icon_path_tmp)
  #plot(r)
  if(dstools::file_ext(path) == "jpeg"){
    r2 <- r
  }else{
    r2 <- r[[2]]
  }

  #plot(r2)
  # Convert to logical raster
  r2 <- terra::ifel(r2 < 255/2, NA, 1)
  if(tmp){unlink(icon_path_tmp)}
  r2
}






get_icon_pct_height <- function(r, target){

  dist <- get_raster_y_distribution(r)
  dist |>
    dplyr::filter(cumdist < (1-target)) |>
    dplyr::slice(1) |>
    dplyr::pull(y)


}


get_raster_y_distribution <- function(r){
  pts <- terra::as.points(r)
  pts <- terra::geom(pts, df=T)
  y_dist <- pts |>
    dplyr::group_by(y) |>
    dplyr::summarize(n = n()) |>
    dplyr::mutate(cumsum = cumsum(n))
  y_dist$cumdist <- 1 - y_dist$cumsum/sum(y_dist$n)
  y_dist
}


icon_sf_polygon <- function(r){


  p <- terra::as.polygons(r, values=TRUE)

  # Convert terra object to sf object
  sf_object <- sf::st_as_sf(p)

  sf_object

}

icon_sf_clip <- function(sf_object, y_value){

  bbox <- st_bbox(sf_object)
  m <- matrix(c(bbox["xmin"], bbox["xmin"], bbox["xmax"], bbox["xmax"], bbox["xmin"],
                y_value,bbox["ymin"],bbox["ymin"],y_value,y_value), ncol = 2)
  clip_polygon <- st_sfc(st_polygon(list(m)),
                         crs = sf::st_crs(sf_object))
  clipped <- sf::st_intersection(sf_object, clip_polygon)
  clipped

}









