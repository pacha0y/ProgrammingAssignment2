## The following two functions are used to cache
## an inverse of a matrix hence preventing R to recompute the solve 
## function every time we call it.

## This function, makeCacheMatrix creates a special "matrix", which is just a
## list containing a function to
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse
## d) get the result of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve)
    inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.
## The function use solve(x) to return an inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("Getting inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv) 
  inv
}
