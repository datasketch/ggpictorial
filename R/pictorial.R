

pictorial <- function(icon, fill){

  r <- icon_read_raster(icon)
  psf <- icon_sf_polygon(r)

  y_value <- get_icon_pct_height(r, fill)

  psf2 <- icon_sf_clip(psf, y_value = y_value)


  gg <- ggplot() +
    geom_sf(data = psf) +
    theme_void()

  gg + geom_sf(data = psf2, fill = "red")



}
