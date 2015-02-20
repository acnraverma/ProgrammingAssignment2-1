## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Assignment: Caching the Inverse of a Matrix

##create a function which starts with a null matrix argument
makeCacheMatrix <- function(mtrx = matrix()) {
  inverseVal <- NULL

  ## create another function to set where the value will be cached in Matrix  
  set <- function(x) {
    mtrx <<- x;
    ## change the value of inverse of the matrix in case the matrix was changed
	inverseVal <<- NULL;
  }
  # gets the inverse 
  get <- function() return(mtrx);
  setinv <- function(inv) inverseVal <<- inv;
  getinv <- function() return(inverseVal);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}



# used to get the cache of the matrix
cacheSolve <- function(mtrx, ...) {
## Return a matrix that is the inverse of 'x'
    inverseVal <- mtrx$getinv()
	#if the inverse exists, it gets it.
    if(!is.null(inverseVal)) {
        message("Getting cached data...")
        return(inverseVal)
    }
    #if the inverse if not there, first it is calculated and then retrieved
	data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverseVal)
    return(inverseVal)
}
