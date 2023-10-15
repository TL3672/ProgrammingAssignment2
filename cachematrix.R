# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function() {
  # Initialize a variable to store the matrix
  mat <- NULL
  
  # Initialize a variable to store the cached inverse
  cache <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() mat
  
  # Function to compute and cache the inverse
  cacheInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached inverse")
      return(cache)
    }
    else {
      message("Calculating inverse")
      cache <<- solve(mat)
      return(cache)
    }
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}


## Write a short comment describing this function

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(cacheMatrix) {
  # Check if a valid cacheMatrix was provided
  if (!is.list(cacheMatrix) || !all(c("set", "get", "cacheInverse") %in% names(cacheMatrix))) {
    stop("Argument 'cacheMatrix' must be a valid object created by 'makeCacheMatrix'")
  }
  
  # Calculate and return the cached inverse
  cacheMatrix$cacheInverse()
}

## How to use
# Create a cacheable matrix
matrixCache <- makeCacheMatrix()

# Set the matrix
matrixCache$set(matrix)

# Retrieve the matrix
matrixCache$get()

# Compute and cache the inverse
inverse <- cacheSolve(matrixCache)