## Put comments here that give an overall description of what your
## functions do


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



#The function created before can be computed with this second function and the result is its inverse. 

cacheSolve <- function(x, ...) { 
     ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
           message("getting cached data")
           return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}
