## This file is submitted to implement two functions that are used for:
## 1) creating and managing a matrix whose inverse can be cached
## 2) calculating a cached version of the inverse of the matrix

## Creates a special matrix whose inverse can be cached (x is the matrix)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix that can be cached (x is the matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
