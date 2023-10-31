



icon_number <- function(icon_path, value,
                             image_size = 0.5,
                             image_color = "gray",
                             text_size = 20,
                             text_x_offset = 0.2,
                             text_y_offset = 0.1,
                             text_color = "black"){

  if(!file.exists(icon_path))
    stop("icon path not found")

  image <- icon_path

  df <- data.frame(x = -0.5, y = 0)
  # Plot
  ggplot(df, aes(x, y)) +
    geom_image(aes(image = image), size = image_size, color = image_color) +
    geom_text(aes(x = 0 + text_x_offset, y = 0 + text_y_offset, label = value),
              nudge_x = 0, size = text_size, color = text_color) +
    #coord_fixed() +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    theme_void()
  # theme_minimal()


}


icon_path <- function(nm){
  icon_path <- available_icons |>
    filter(name == nm) |>
    pull(path)
  sroi_sys_file(icon_path)

}


