
# Define the Icon R6 class
IconR6 <- R6::R6Class(
  "Icon",
  public = list(
    shape = NULL,
    color = NULL,
    fill = NULL,
    linewidth = NULL,
    center = NULL,
    phi = NULL,
    radius = NULL,
    sf = NULL,

    # Constructor
    initialize = function(shape = NULL, color = NULL, fill = NULL,
                          linewidth = 1, center = c(0, 0),
                          phi = 0, radius = 1,
                          sf = NULL) {
      self$shape <- shape
      self$color <- color
      self$fill <- fill
      self$linewidth <- linewidth
      self$center <- center
      self$shape <- shape
      self$phi <- phi
      self$radius <- radius
      self$sf <- sf
      self$create_shape()
    },

    # Method to create the shape
    create_shape = function() {

      basic_shapes <- c("circle", "triangle", "square", "pentaon",
                        "hexagon", "heptagon", "octagon")

      if(is.null(self$shape)){
        self$sf <- NULL
      } else if(is.na(self$shape)){
        self$sf <- NULL
      } else if(self$shape %in% basic_shapes){
        params <- list(center = self$center, phi = self$phi,
                       radius = self$radius)
        self$sf <- do.call(paste0("icon_", self$shape), params)
      } else if (self$shape == "image") {
        self$sf <- icon_image(self$path, center = self$center)
      }

    },

    # Method to rotate the shape
    rotate = function(phi) {
      self$phi <- self$phi + phi
      self$sf <- rotate_polygon(self$sf, phi)
    },
    # Method to rotate the shape
    move_to = function(x, y) {
      self$sf <- recenter_polygon_at(self$sf, x = x, y = y)
      self$center <- c(x,y)
    },
    # Plot
    plot = function(fill = NULL, color = NULL,
                    linewidth = NULL,
                    grid = F
    ){
      fill <- fill %||% self$fill %||% "#99f0d4"
      color <- color %||% self$color %||% "#486890"
      linewidth <- linewidth %||% self$linewidth
      gg <- ggplot() +
        geom_sf(data = self$sf, fill = fill, color = color,
                size = linewidth)
      if(!grid) gg <- gg + theme_void()
      gg
    },
    # Custom print method
    show = function(){
      paste0(self$shape,"  ·(", toString(self$center),") ", "⊾", self$phi)
    },
    print = function() {
      cat(self$show())
      # Check if there is an sf object to plot
      if (!is.null(self$sf)) {
        plot <- self$plot(grid = TRUE) +
          ggtitle(paste("Center: ", toString(self$center), "Phi: ", self$phi))
        print(plot)
      }
      invisible(self) # Return the object invisibly
    }
  )
)


is_iconR6 <- function(x){
  inherits(x, c("Icon", "R6"))
}


#' @export
iconR6 <- function(x = NULL, color = NULL, fill = NULL,
                   linewidth = 1, center = c(0, 0),
                   radius = 1,
                   phi = 0){

  IconR6$new(x,
             color = color,
             fill = fill,
             linewidth = linewidth,
             center = center,
             radius = radius,
             phi = phi)

}




icon_square <- function(...){
  create_polygon(n = 4, ...)
}

icon_triangle <- function(...){
  create_polygon(n = 3, ...)
}

icon_pentagon <- function(...){
  create_polygon(n = 5, ...)
}

icon_circle <- function(points = 100, ...){
  create_polygon(n = points, ...)
}

icon_image <- function(path, center = c(0, 0)){
  r <- icon_read_raster(path)
  psf <- icon_sf_polygon(r)
  psf
}


