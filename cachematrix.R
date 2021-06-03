# Set of functions that save an inverse of a matrix and later checks
# if the matrix already has been inverted (and hasn't changed)

# This function will set up the cache matrix parts so that the other 
# function (cacheSolve) can check if it doesn't need to invert

makeCacheMatrix <- function(matr = matrix()) {
      matr2 <- NULL
      set <- function(matr3) {
          matr <<- matr3
          matr2 <<- NULL
      }
      
      get <- function() matr
      setinverse <- function(inverse) matr2 <<- inverse
      getinverse <- function() matr2
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
# This function will take the inverse, but only if needed.  It first
# checks to see if the original matrix is the same, and if the inverse
# has already been computed.

cacheSolve <- function(matr, ...) {
      matr2 <- matr$getinverse()
      
      # does inverse already exist (original the same)
      
      if (!is.null(matr2)) {
          message("getting cached data")
          return(matr2) # 'return' ends function, saves compute time
      }
      
      matr4 <- matr$get()
      matr2 <- solve(matr4, ...)
      matr$setinverse(matr2)
      matr2
}
