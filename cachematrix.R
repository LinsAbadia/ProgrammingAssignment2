## Put comments here that give an overall description of what your
## functions do
##   Per the assignment requirements, the cachematrix.R script implement the two (2) functions
##    requested in this branch: makeCacheMarix and ccheSolve.  These essentially, caches
##    the inverse of a supplied matrix.  To test these, user-defined, 'singular' matrices 
##    (and their inverses) and compared against each functions internalvariables and
##    functions (basically, a list composed of four (4) elements).  Descriptions are given 
##    before each function definition.

## Write a short comment describing this function
##  This is adapted from the supplied MakeVector.R and insteads 'caches' the matrix provided.
##  It changes the following:  m (mean) to s (solve),  setmean to setMInv,
##   getmean to getMInv, and "function(mean) m <<- mean" to "function(solve) s <<- solve".

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setMInv <- function(solve) s <<- solve
  getMInv <- function() s
  
  list(set = set, get = get,
       setMInv = setMInv,
       getMInv = getMInv)
}

#  This is adapted from the supplied CacheMean.R and insteads 'caches' the matrix provided.
#  It changes the following:  m (mean) to s (solve),  setmean to setMInv,
#   getmean to getMInv, data to mData, " message("getting cached data")" to 
#   "message("getting cached matrix data")". and "m <- mean(data, ...)" to
#    "s <- solve(mData, ...)".
cacheSolve <- function(x, ...) {
  s <- x$getMInv()
  if(!is.null(s)) {
    message("getting cached matrix data")
    return(s)
  }
  mData <- x$get()
  s <- solve(mData, ...)
  x$setMInv(s)
  s
}
