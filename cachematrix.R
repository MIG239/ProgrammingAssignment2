##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.
##cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## functions do

## creates special vector containing a matrix, inversematrix and functions
## to operate with these objects

makeCacheMatrix <- function(mat=numeric()){
  invmat <- NULL
  set <- function(y) {
    mat <<- y
    invmat <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}
## The function calculates inverse matrix in case it nis not calculated and returns it.
## If inverse matrix is already calculated it just returns the calculated one
cacheSolve<-function(mat, ...){
  invmat <- mat$getInverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- mat$get()
  invmat <- solve(data, ...)
  mat$setInverse(invmat)
  invmat
}