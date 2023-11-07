

new_Icn <- function(x = NULL, object = NULL, ...){

  if(is.null(x)) return()
  if(is_Icn(x)){
    return(x)
  }

  if(length(x) < 2){
    if(!is_iconR6(x)){
      obj <- iconR6(x = x, ...)
      shape <- obj$shape
    } else{
      shape <- x$shape
      obj <- attr(x, "object")
    }
  }else{
    icons <- purrr::map(x, function(xi){
      if(!is_iconR6(xi)){
        xi <- iconR6(xi, ...)
      }
      xi
    })
    shape <- purrr::map_chr(icons, function(obj) obj$shape)
    obj <- icons
  }
  if(!is.null(object)) obj <- object
  vctrs::new_vctr(shape, object = obj, class = "hd_Icn")
}

Icn <- function(x, ...){
  new_Icn(x, ...)
}

is_Icn <- function(x){
  inherits(x, "hd_Icn")
}

Icn_object <- function(x){
  attr(x, "object")
}


# Methods

## Format method

#' @export
format.hd_Icn <- function(x, ...) {
  obj <- attr(x, "object")
  if(is_iconR6(obj)){
    obj$show()
  } else{
    purrr::map_chr(obj, function(x) x$show())
  }
}

#' @export
vec_ptype_abbr.hd_Icn <- function(x, ...) {
  "Icn"
}



#' @method as.character hd_Icn
#' @export
as.character.hd_Icn <- function(x){
  vctrs::vec_data(x)
}


# Define the [ method for subsetting
#' @method `[` hd_Icn
#' @export
`[.hd_Icn` <- function(x, i, ...) {
  xi <- vctrs::vec_data(x)[i]
  obj <- attr(x, "object")[[i]]
  attr(xi, "object") <- obj
  obj
}


#' # Define a method for combining two icon lists
#' #' @method vec_c hd_Icn
#' #' @export
#' vec_c.hd_Icn <- function(x, y, ...) {
#'   message("combining")
#'   Icn(c(vctrs::vec_data(x), vctrs::vec_data(y)))
#'   #z <- c(vctrs::vec_data(x), vctrs::vec_data(y))
#'   #obj_x <- attr(x, "object")
#'   #obj_y <- attr(y, "object")
#'   #new_Icn(z, object = c(obj_x, obj_y))
#' }



# Coercion

#' @rdname vctrs-compat
#' @method vec_ptype2 hd_Icn
#' @export
#' @export vec_ptype2.hd_Icn
vec_ptype2.hd_Icn <- function(x, y, ...) UseMethod("vec_ptype2.hd_Icn", y)

#' @method vec_ptype2.hd_Icn default
#' @export
vec_ptype2.hd_Icn.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


# A Icn combined with a Icn returns a Icn
#' @method vec_ptype2.hd_Icn hd_Icn
#' @export
vec_ptype2.hd_Icn.hd_Icn <- function(x, y, ...) new_Icn()

# Icn and character return double
#' @method vec_ptype2.hd_Icn character
#' @export
vec_ptype2.hd_Icn.character <- function(x, y, ...) character()

#' @method vec_ptype2.character hd_Icn
#' @export
vec_ptype2.character.hd_Icn <- function(x, y, ...) character()




# Casting

#' @rdname vctrs-compat
#' @method vec_cast hd_Icn
#' @export
#' @export vec_cast.hd_Icn
vec_cast.hd_Icn <- function(x, to, ...) UseMethod("vec_cast.hd_Icn")

#' @method vec_cast.hd_Icn default
#' @export
vec_cast.hd_Icn.default <- function(x, to, ...) vec_default_cast(x, to)

# Coerce Icn to Icn

#' @method vec_cast.hd_Icn hd_Icn
#' @export
vec_cast.hd_Icn.hd_Icn <- function(x, to, ...) x

#' @method vec_cast.hd_Icn character
#' @export
vec_cast.hd_Icn.character <- function(x, to, ...) Icn(x)

#' @method vec_cast.character hd_Icn
#' @export
vec_cast.character.hd_Icn <- function(x, to, ...) vctrs::vec_data(x)





