## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("cached!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

## Example
## x <- rbind(c(-1/4, 1, 2), c(2, 1, -1/4), c(1, 2, -1/4))
## m <- makeCacheMatrix(x)
## cacheSolve(m)
##            [,1]       [,2]       [,3]
## [1,] 0.04040404  0.6868687 -0.3636364
## [2,] 0.04040404 -0.3131313  0.6363636
## [3,] 0.48484848  0.2424242 -0.3636364

## cacheSolve(m)
## cached!
## [,1]       [,2]       [,3]
## [1,] 0.04040404  0.6868687 -0.3636364
## [2,] 0.04040404 -0.3131313  0.6363636
## [3,] 0.48484848  0.2424242 -0.3636364