
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## The following two functions are designed for caching the inverse of a matrix.



## makeCacheMatrix function creates a special "matrix" which is really a list containing 
## a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  
                                
  i<- NULL                                           ## initiatize i(inverse) == NULL
  setmatrix <- function(y) {                         ## set the value of the matrix
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x                             ## set the value of the matrix
  setinverse <- function(inverse) i <<- inverse         ## set the value of the inverse 
  getinverse <- function() i                           ## get the value of the inverse 
  list(setmatrix = setmatrix, getmatrix = getmatrix,   ## return a list 
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##If not, it computes the inverse, then sets the value in the cache by using setinverse function.
##In this project, we assume that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
       
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)     ## solve function is used to Compute the inverse of a square matrix in R
  x$setinverse(i)
  i                         ## Return a matrix that is the inverse of 'x'
}
