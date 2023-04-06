## Put comments here that give an overall description of what your
## functions do:
## These are a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function:
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
## First we initialize the inverse property, then provide a mehtod to set the 
## matrix and then a method to get the matrix. After that we provide a method 
## to set the inverse of the matrix and then another method to get the inverse
## of the matrix. 
        i <- NULL
        set <- function( matrix ) {
        m <<- matrix
       i <<- NULL
  }
  
  get <- function() {m}
  
  setInverse <- function(inverse) { i <<- inverse }
  
  getInverse <- function() {i}
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function:
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

## In this function we first try to return the inverse of the matrix through 
## the getInverse() method. or just return the inverse if its already set. 
## After that we calculate the inverse of the matric using matrix multiplicaiton. 
## Then set the inverse to the object and return the matrix. 
        
        m <- x$getInverse()
        if( !is.null(m) ) {
           message("getting cached data")
           return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
