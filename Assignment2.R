# RProgramming_assignment2

## Put comments here that give an overall description of what your
## functions do
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the matrix
##4.get the value of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible. 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(m)
  m
}
