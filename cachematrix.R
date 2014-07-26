## Contains two functions: makeCacheMatrix creates a cache matrix with
## getters and setters, cacheSolve inverses a matrix and caches the result
## for possible later use.


## cache matrix front-end interface

makeCacheMatrix <- function(x = matrix()) {
  c_inverse <- NULL
  set <- function(aMatrix) {
    c_matrix <<- aMatrix
    c_inverse <<- NULL
  }
  get <- function() c_matrix
  set_inverse <- function(anInverse) c_inverse <<- anInverse
  get_inverse <- function() c_inverse
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Computes an inverse of a matrix and stores results in internal cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$set_inverse(inv)
  inv
}
