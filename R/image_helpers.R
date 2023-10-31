

svg_to_png <- function(input_svg_path, width = 800) {
  img <- magick::image_read_svg(input_svg_path, width = width)
  f <- tempfile(fileext = ".png")
  magick::image_write(img, path = f, format = "png")
  f
}
