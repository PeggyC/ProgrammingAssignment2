## Caching the Inverse of a Matrix
## In order to save computing time, these routines 
## either retrieve the inverse from the cache, or 
## calculate the inverse of the matrix and cache it.

## makeCacheMatrix:
## This function create a special "matrix", i.e. a list containing
## a function to set the value of the matrix, get the value of the 
## matrix, set the value of the inverse, and get the value of the 
##inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
 

## cacheSolve:
## This function computes the inverse of the special matrix
## defined by makeCacheMatrix. If the inverse is already calculated
## cachesolve retrieves the inverse form the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
  
  }
