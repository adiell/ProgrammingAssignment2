## Functions for caching inversing of a matrix
## 

# This function creates a "wrapper" around the matrix
# containing it's inverse + utility functions

makeCacheMatrix <- function(x = matrix()) {
   
   # This function creates a "wrapper" around the matrix
   # containing it's inverse
   # initialize to NULL
   inv <- NULL
   
   # In addition the "wrapper" object has several functions
   # Initializing the data
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   # Getting the original matrix
   get <- function() x
   # storing the inverse
   setInv <- function(inverse) inv <<- inverse
   # getting the inverse
   getInv <- function() inv
   list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## This function calcultes the inverse of the matrix of the "wrapper" matrix

cacheSolve <- function(x, ...) {
   # First we check if we already have a cached inverse
   inv <- x$getInv()
   # If we have then we return it
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   # else we compute the inverse and cache it, before return it's value
   data <- x$get()
   inv <- solve(data)
   x$setInv(inv)
   inv
}
