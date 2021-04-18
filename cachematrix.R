## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function allows to caching the inverse of a matrix without compute it repeatedly
#It creates a matrix object which may cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
          x <<- y
          inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <-  function() {inv}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function

#The function created before can be computed with this second function and the result is its inverse. 

cacheSolve <- function(x, ...) { 
    inv <- x$getInverse()
    if(!is.null(inv)){
           message("getting cached data")
           return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
