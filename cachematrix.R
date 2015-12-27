# The function returns the inverse of a matrix, assuming that the matrix is invertible. 
# It first checks if the inverse of the given matrix has already been calculated. 
# If so, it returns the cached result, otherwise it calculates it and stores the result in the cache.

# makeCacheMatrix(matrix) creates a list containing a function to:
#   1. setMatrix the value of the matrix
#   2. getMatrix the value of the matrix
#   3. setInverse the value of inverse of the matrix
#   4. getInverse the value of inverse of the matrix

# Here
# - x is a matrix argument
# - inv is the variable in which the inverse will be saved
# - setMatrix and getMatrix are methods that allow the stored matrix to be saved or retrieved
# - setInverse and getInverse are methods that allow the stored inverse of the matrix to be saved or retrieved

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


# Returns inverse of the matrix immediately if it has already been calculated, otherwise it calls the solve(matrix) function and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # calculate inverse and cache the result
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
