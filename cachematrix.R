##These functions take an invertable matrix, and caches its inverse
##library(MASS) allows matrix of any dimentions to be taken as the input

library(MASS)

##Function makeCacheMatrix creates a matrix so that fuctions set, get, 
##setinverse, and getinverse are available for this matrix.
##It caches the inverse of the function.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(ginv) i <<- ginv
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


##Function cacheSolve takes matrix of any dimentions and returns its inverse.
##If the inverse has been calculated already, it gets the inverse from the cache; 
##otherwise, it calculates the inverse and sets its value in the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     m <- x$get()
     i <- ginv(m, ...)
     x$setinverse(i)
     i
     
}
