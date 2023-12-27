
#' @export
pictorial_grid <- function(df,
                           icon_name = "square",
                           fill = NA,
                           color = NA,
                           palette = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL){

  palette <- palette %||% rainbow(nrow(df))

  if(!is.na(fill)){
    df$fill <- fill
  }
  if(!"fill" %in% names(df)){
    df$fill <- map_categories_to_palette(df$categories, palette = palette)
  }


  if(!is.na(color)){
    df$color <- color
  }
  if(!"color" %in% names(df)){
    df$color <- df$fill
  }


  if(inherits(df, "data.frame")){
    value <- sum(df$values)
  }
  d <- make_grid(n = value)
  # Fill grid with information for each value
  d$categories <- df |>
    tidyr::uncount(values) |>
    dplyr::pull(categories)
  d <- d |> dplyr::left_join(df, by = "categories")

  ld <- purrr::transpose(d)

  icons_sf <- purrr::map(ld, function(p){
    #p <- ld[[10]]
    icon <- Icn(icon_name, center = c(p$x, p$y), radius = 0.5,
                fill = p$fill, color = p$color)
    icon_obj <- Icn_object(icon)
    sf <- icon_obj$sf
    sf$fill <- icon_obj$fill
    sf$color <- icon_obj$color
    sf
  })
  sfs <- dplyr::bind_rows(icons_sf)

  ggplot(sfs) + geom_sf(aes(fill = fill, color = color)) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_void() +
    labs(title = title, subtitle = subtitle, caption = caption)


}

map_categories_to_palette <- function(categories, palette) {
  if (length(unique(categories)) != length(palette)) {
    stop("The number of unique categories must match the length of the palette.")
  }
  factor_categories <- factor(categories, levels = unique(categories))
  palette[factor_categories]
}


#' @export
pictorial_unit <- function(value, max_value = NULL, icon_name = "square",
                           color = "red", na_color = "#dadada",
                           palette = NULL, title = NULL, subtitle = NULL,
                           caption = NULL){

  # make the dataframe

  value1 <- value
  if(!is.null(max_value)){
    value2 <- max_value - value1
  } else{
    value1 <- value * 10^count_decimals(value)
    value2 <- 10^count_decimals(value) - value1
  }

  df <- tibble(
    categories = c("value", "empty"),
    values = c(value1, value2)
  )
  palette <- palette %||% c(color, na_color)
  palette <- palette[1:nrow(df)]

  pictorial_grid(df,
                 icon_name = icon_name,
                 palette = palette,
                 title = title,
                 subtitle = subtitle,
                 caption = caption)

}


count_decimals <- function(n) {
  # Handle the case where the number is an integer
  if (floor(n) == n) {
    return(0)
  }
  # Convert to a character string
  num_str <- as.character(n)
  # Split the string into two parts: before and after the decimal point
  str_parts <- strsplit(num_str, "\\.")[[1]]
  # Count the number of characters after the decimal point
  return(nchar(str_parts[[2]]))
}

