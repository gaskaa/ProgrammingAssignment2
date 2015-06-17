## June 17, 2015
## The following script uses a pair of functions, 'makeCacheMatrix' and 'cacheSolve',
## to cache the inverse of a matrix (assume the matrix is invertible).
## The <<- operator is used to assign a value to an object in an environment that is 
## different from the current environment.
##-------------------------------------------------------------------------------------------

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #x is a square matrix
  
    inv <- NULL
  
    # Return a list of funcions that:
    #   1. set the matrix
    #   2. get the matrix
    #   3. set the inverse
    #   4. get the inverse
  
    set <- function(y) {
      x <<- y          
      inv <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
## If the inverse has already been calculated, then 'cacheSolve' will retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
    #The output from 'makeCacheMatrix' is used here as an input
  
    # Get the inverse of matrix 'x'
    inv <- x$getinverse()
  
    #If the inverse is already cahed, retrieve it from the cache and return it
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  
    #If the inverse has not been calculated, find the inverse and return it
    data <- x$get()
    inv <- solve(data, ...)  #The solve() function finds the inverse of a square, invertibe matrix
    x$setinverse(inv)
    inv
}
