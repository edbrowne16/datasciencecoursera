## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will enable us to cache the inverse of a matrix
# which we have calculated, so that if we call it once it will
# calculate the inverse, but for any subsequent call(s), it will
# not have to calculate it, but instead re-use the cached version

makeCacheMatrix <- function(mSolve = matrix()) {
  mSolve <- NULL
  # In object-oriented terms, we need a getter and a setter for
  # this, so create them here.
  # set the value of the matrix
  set <- function(formalArg) {
    mSolve <<- formalArg
    mSolve <<- NULL
  }
  # get the value of the matrix
  get <- function() {
    return(mSolve)
  }
  setSolve <- function(formalArg) {
    mSolve <<- formalArg
  } 
  getSolve <- function() {
    return(mSolve)
  }
  
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (class(x) == "matrix") {
    message("you sent us a matrix")
  } else {
    message("your argument is not a matrix - exiting")
    exit("no")
  }
  mSolve <- x$getSolve()
# If mSolve is not null, then it means we've already calculated
# this answer and cached it, so just return it.
  if (!is.null(mSolve)) {
    message("getting cached data")
    return(mSolve)
  }
  # If there is not already an answer cached, then calculate it.
  data <- mSolve$get()
  mSolve <- solve(data, ...)
  # Now if we've had to calculate it, we cache it for next time.
  x$setSolve(mSolve)
  mSolve
}
