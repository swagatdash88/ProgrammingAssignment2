## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  set_mat <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(x_inv) x_inverse <<- x_inv
  get_inv <- function() x_inverse
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  new_mat <- x$get_mat()
  new_mat_inv <- x$get_inv()
  if(identical(new_mat, x) && !is.null(new_mat_inv)) {
    message("getting cached data")
    return(new_mat_inv)
  }
  data <- x$get_mat()
  new_mat_inv <- solve(new_mat)
  x$set_inv(new_mat_inv)
  new_mat_inv
}
