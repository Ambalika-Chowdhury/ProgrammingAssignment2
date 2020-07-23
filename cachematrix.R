## the file contains two functions,one to create a special object, that is able
## to cache its inverse, and the second to compute the inverse of the matrix 
## returned by the first function, checking and retrieving the cached inverse
## if the matrix has not been changed.

## this function creates a special "matrix" object - a list, which stores and 
##caches the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  set_inv <- function(inverse) {inv <<- inverse}
  get_inv <- function() {inv}
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## this function computes the inverse of a matrix, taking as argument the list 
## returned by makeCacheMatrix().It first checks if the inverse has already been
## calculated, returning the same, provided the matrix has not been changed

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    print ("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$set_inv(inv)
  inv
}
