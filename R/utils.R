


make_sure_close_coords <- function(mat){
  n <- nrow(mat)
  mat[n,] <- mat[1,]
  mat
}

