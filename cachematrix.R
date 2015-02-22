## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## and also the master trigger/flag (CacheMatrix.Master) indicating if it's 
## inverse values have already been calculated and stored in cache.
makeCacheMatrix <- function(x = matrix()) {
     CacheMatrix.Master <- NULL 
     CacheMatrix.New <- function(y) {
          x <<- y
          CacheMatrix.Master <<- NULL
     }
     CacheMatrix.Values <- function() x
     CacheMatrix.InverseProcess <- function(z) CacheMatrix.Master <<- z
     CacheMatrix.InversedValues <- function() CacheMatrix.Master
     list(CacheMatrix.New = CacheMatrix.New, CacheMatrix.Values = CacheMatrix.Values,
          CacheMatrix.InverseProcess = CacheMatrix.InverseProcess,
          CacheMatrix.InversedValues = CacheMatrix.InversedValues)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix and checks if the inverse values have already been
## calculated and if so to retrieve the values directly from cache
cacheSolve <- function(x) {
     CacheMatrix.Master <- x$CacheMatrix.InversedValues()
     if(!is.null(CacheMatrix.Master)) {
          message("getting cached data")
          return(CacheMatrix.Master)
     }
     Matrixdata <- x$CacheMatrix.Values()
     CacheMatrix.Master <- solve(Matrixdata)
     x$CacheMatrix.InverseProcess(CacheMatrix.Master)
     CacheMatrix.Master
}