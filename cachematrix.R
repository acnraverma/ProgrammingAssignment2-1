## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  inverseVal <- NULL
  set <- function(x) {
    mtrx <<- x;
    inverseVal <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) inverseVal <<- inv;
  getinv <- function() return(inverseVal);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}




cacheSolve <- function(mtrx, ...) {
## Return a matrix that is the inverse of 'x'

    inverseVal <- mtrx$getinv()
    if(!is.null(inverseVal)) {
        message("Getting cached data...")
        return(inverseVal)
    }
    data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverseVal)
    return(inverseVal)
}
