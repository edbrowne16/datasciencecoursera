## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mSolve <- NULL
  set <- function(y) {
    x <<- y
    mSolve <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) mSolve <<- solve
  getSolve <- function() mSolve
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mSolve <- x$getSolve()
  if (!is.null(mSolve)) {
    message("getting cached data")
    return(mSolve)
  }
  data <- mSolve$get()
  mSolve <- solve(data, ...)
  x$setSolve(mSolve)
  mSolve
}
