## Following two functions are used to calulate and store 
## the inverse of a matrix. When there is no change in the 
## input matrix, inverse is fetched from cache which will save
## computation time

## This Function takes a matrix as an input and 
## generates a list of functions to get and set the 
## values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inverse<<-NULL
  set_mat <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(x_inv) x_inverse <<- x_inv
  get_inv <- function() x_inverse
  
  ## List of get-set functions
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)

}


## This Function takes a matrix and function 
## list (from makeCacheMatrix function) as input. 
## If the input matrix and that stored in cache are same 
## and the inverse is available in the cache then inverse
## fetched and returned from cache. Otherwise, inverse is
## calculated and stored in cache

cacheSolve <- function(x, func_list) {
  new_mat <- func_list$get_mat()
  new_mat_inv <- func_list$get_inv()
  
  ## Checks if matrices are identical and inverse 
  ## is available in cache
  if(identical(new_mat, x) && !is.null(new_mat_inv)) {
    message("getting cached data")
    return(new_mat_inv)
  }
  
  data <- func_list$get_mat()
  new_mat_inv <- solve(new_mat)
  func_list$set_inv(new_mat_inv)
  new_mat_inv
}
