## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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
       getMInv = getMInv()
}


}



cacheSolve <- function(x, ...) {
  s <- x$getMInv()
  if(!is.null(s)) {
    message("getting cached matrixdata")
    return(s)
  }
  mData <- x$get()
  s <- solve(mData, ...)
  x$setMInv(s)
  s
}
