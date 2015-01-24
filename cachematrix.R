## Put comments here that give an overall description of what your
## functions do:

## This function receives a matrix "x" .  This fuction creates and object that can cache the matrix inverse
## it assumes an invertible matrix
## for matrix "x"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## set the value 
    x <<- y
    inv <<- NULL  
  }
  get <- function() x
  ## get the value
  setinverse <- function(inverse) inv <<- inverse
  ## set the value of inverse 
  getinverse <- function() inv
  ## get the value of inverse 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If it has, it returns the result and skips the
## computation. If it has not, it computes the inverse, sets the value in the cache via
## setinverse function.
## This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}