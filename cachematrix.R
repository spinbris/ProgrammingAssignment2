## John Hopkins University, Coursera, R Programming 
## Week 3, Assignment 2 - Lexical Scoping


# Overview of Functions
# The combination of the following two functions is intended to save computations 
# in calculating the inverse of a matrix by caching the initial result and only recalculating if the 
# matrix is changed or reset.


## makeCacheMatrix Function:
#    - stores the matrix to be inversed as x,
#    - sets m to NULL to indicate that the inverse has yet to be calculated.This will also reset any previously stored 
#           'm' to NULL. (m is updated subsequently by CacheSolve function)
#       (x and m are both stored in parent environment  - ie that of the function)
#    - creates a list of functions ( set(),get(), setsolve() and getsolve()) which can be called using 
#       named ('$' style) notation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## CacheSolve Function:
##     - uses getsolve to retrieve m, the matrix inversion, from makeCacheMatrix object. 
##     - if m is not NULL, the data is returned with a message ""getting cached data"
##     - If m is NULL, then the solve function is used to calculate the matrix inversion.
##     - x$setsolve(m) then updates the value of m in the makeCacheMatrix object for possible future reference, 
##        and m is returned

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
