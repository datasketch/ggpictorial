
#' @export
pictorial_icon <- function(value, icon, fill = "#4c3f4c",
                           bg_color = "#FAFAFA",
                           na_color = "#dadac0"){

  r <- icon_read_raster(icon)
  psf <- icon_sf_polygon(r)

  y_value <- get_icon_pct_height(r, value)

  psf2 <- icon_sf_clip(psf, y_value = y_value)


  gg <- ggplot() +
    geom_sf(data = psf, fill = na_color, color = NA) +
    #scale_fill_manual(na.value = "red") +
    theme_void()

  gg + geom_sf(data = psf2, fill = fill, color = NA) +
    theme_background(bg_color)



}
