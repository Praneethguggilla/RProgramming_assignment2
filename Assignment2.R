# RProgramming_assignment2

Code of Makecachematrix

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

Code for cachesolve 

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
