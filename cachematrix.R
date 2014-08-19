## These functions create a list that stores a matrix and caches its inverse

## The first function creates the list.  It uses the variable 'inv' to store 
#the inverse of the matrix - it is set to 'NULL' to begin with

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  set_inv <- function(inverse) {inv <<- inverse}
  get_inv <- function() {inv}
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## This function calculates the inverse of the matrix and caches it

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
