## Author: Tarek Darwish
## Date  : Jun 10, 2016
## Description: This file contains R code to create a specail "matrix" object
##              that can  cache its inverse. If asked to compute the inverse,
##              the code will check to see if the inverse of the matrix has 
##              already been calcualted in the cache, then the code will
##              retrieve the inverse from the cache.




## The following function creates a special matrix variable
## If it is in cache, it will assign it NULL. The cache name is "inv"
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list (se = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function, computes the inverse of a matrix.
## if the inverse of this matrix has been computed before,
## then the function will instead retrieve the inverse from the 
## cache and skip doing the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ... )
  x$setinv(inv)
  inv
}
