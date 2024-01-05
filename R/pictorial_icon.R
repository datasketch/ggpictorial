
#' @export
pictorial_icon <- function(value, icon, fill = "gray40",
                           bg_color = "#eeeeee",
                           na_color = "#d9d9d9"){

  r <- icon_read_raster(icon)
  psf <- icon_sf_polygon(r)

  y_value <- get_icon_pct_height(r, value)

  psf2 <- icon_sf_clip(psf, y_value = y_value)


  gg <- ggplot() +
    geom_sf(data = psf, fill = na_color) +
    #scale_fill_manual(na.value = "red") +
    theme_void()

  gg + geom_sf(data = psf2, fill = fill) +
    theme_background(bg_color)



}
