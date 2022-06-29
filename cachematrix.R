## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) m <<- solve
  getmatrixinv <- function() m
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by the makeCacheMatrix
## above. If the inverse has already been calculated, then the cachsolve should retrieve the
## inverse from the cache.

## Sample matrix generation code
## x <- matrix(c(0,11,2,3,4,5,6,7,8), c(3,3))

## Sample solve call code
## xMat <- makeCacheMatrix(x)
## cacheSolve(xMat)
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinv(m)
  m
}
