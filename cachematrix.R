# Programming assignment 2, part of Coursera course "R Programming course". 
# Author: TK
# Date: 2015-11-19 
#
# This file contains the following two functions:
# 1.makeCacheMatrix: 
#     Creates a special "matrix" object that can cache its inverse.
# 2.cacheSolve: 
#     Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#     If the inverse has already been calculated (and the matrix has not changed), 
#     cachesolve retrieves the inverse from the cache.


#------------------------------------------------------------------------------
# Create a special "matrix" object that can cache its inverse. 
# The function takes as its argument a matrix and returns a list of functions
# that can be used to set or retrieve the matrix or its inverse.
makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inverse of the matrix with NULL.
  inv <- NULL
  
  # Define function to set the matrix data
  set <- function(y) {
    x <<- y
    inv <<- NULL # Reinitialize inverse because matrix data is being changed
  }
  
  # Define function to retrieve matrix data
  get <- function() x
  
  # Define function to compute the inverse "from scratch"
  setinv <- function(solve) inv <<- solve
  
  # Define function to get inverse of matrix from cache
  getinv <- function() inv
  
  # Return a list of functions to set or retrieve the matrix or its inverse. 
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


#------------------------------------------------------------------------------
# Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been computed and the matrix has not changed, then
# the cached inverse is returned.
cacheSolve <- function(x, ...) {
  
  # Retrieve inverse of special matrix
  inv <- x$getinv()
  
  # Return retrieved inverse provided it is not NULL
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Retrieve matrix data
  data <- x$get()
  
  # Use solve function to compute the inverse "from scratch"
  inv <- solve(data, ...)
  
  # Cache the computed inverse so it can be reused next time
  x$setinv(inv)
  
  # Return the computed inverse
  inv
  
}
