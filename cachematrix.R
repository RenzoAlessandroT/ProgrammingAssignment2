## pair of functions that cache the inverse of a matrix.

## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the cache for the inverse
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cache when the matrix changes
  }
          # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
}
