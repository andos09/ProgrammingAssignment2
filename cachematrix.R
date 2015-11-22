## This pair of functions are able to cache the inverse of a given matrix

## The first function makeCacheMatrix creates a matrix object that
## is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The second function cacheSolve either calculates the invert of
## the matrix returned by the first function, or (if the invert
## already has been calculated and the matrix did not change)
## retrieves the invert from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached invert")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}
