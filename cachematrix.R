##
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## These are a pair of functions that cache the inverse of a matrix.
##
## Coursera - R Programming 
## Agustín Alvarez, 2018-12-03, agustin.alvarezconesa@gmail.com
##

## This function creates a special "matrix" object that can cache its inverse.
## It also creates a special vector, which is really a list containing 4 functions: setMatrix,getMatrix, setInverse, getInverse
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse
       )
}

## This function computes the inverse of the special "matrix" returned by the previous makeCacheMatrix  
## If the inverse has already been calculated then cacheSolve just retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Inverse Matrix")
    return(invMatrix)
  }
  Matrixdata <- x$getMatrix()
  invMatrix <- solve(Matrixdata, ...)
  x$setInverse(invMatrix)
  invMatrix
}
##    
##    
##    mym <- matrix(1:4,2,2)
##    > mym
##    [,1] [,2]
##    [1,]    1    3
##    [2,]    2    4
##    
##    > matrixcache <- makeCacheMatrix(mym)
##    
##    > matrixcache$getMatrix()
##    [,1] [,2]
##    [1,]    1    3
##    [2,]    2    4
##    > matrixcache$getInverse()
##    NULL
##    
##    > cacheSolve(matrixcache)
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    
##    > cacheSolve(matrixcache)
##    Getting Cached Inverse Matrix
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5


