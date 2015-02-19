## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
