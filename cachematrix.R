## Put comments here that give an overall description of what your
## functions do


## Function creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function solves the inverse of the matrix created by 
## makeCacheMatrix. If the inverse is not NULL, then it retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("From cache data")
    return(inv)
  }
  tmpMat <- x$get()
  inv <- solve(tmpMat, ...)
  x$setInverse(inv)
  inv
}
