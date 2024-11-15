## The makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse. The object is essentially a list of functions that
## allow you to set and get the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the cache for the inverse
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y         # Assign the new matrix to the variable x in the parent environment
    inv <<- NULL    # Reset the inverse cache to NULL when the matrix is updated
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" object 
## created by makeCacheMatrix. If the inverse has already been computed and 
## cached, it retrieves the cached inverse. If not, it calculates the inverse
## and stores it in the cache.
cacheSolve <- function(x, ...) {
  
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is cached, return it
  if (!is.null(inv)) {
    message("getting cached data")  # Provide a message indicating the inverse is cached
    return(inv)
  }
  
  # Otherwise, compute the inverse of the matrix
  data <- x$get()            # Get the matrix from the special object
  inv <- solve(data, ...)    # Compute the inverse using solve()
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}

