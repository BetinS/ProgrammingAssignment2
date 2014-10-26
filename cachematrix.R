## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## reset cache value 
  matrix2cache <- x ## remember original matrix
  checkCache <- function(x) {
    if (!is.null(i) & identical(x,matrix2cache)) { 
      message("getting cached data")
      return(i) ##return cached value if it exists for requested matrix
    }
    message("calculating")
    i <<- solve(x) ##calculate inverse matrix for mx, update cache
    matrix2cache <<- x ## remember original matrix (it may be changed)
    i
   }

list(matrix=x, checkCache=checkCache)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x$checkCache(x$matrix)
}
