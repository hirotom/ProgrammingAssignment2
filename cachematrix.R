## R Programming, Programming Assignment 2
## Hiroto Miyake, 2014.08.16

## Assignment: Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
#This function creates a special "matrix" object that can cache its inverse.
    
    solve <- NULL
    
    set <- function(y) {
      x <<- y
      solve <- NULL
    }
    
    get <- function() x
    
    setsolve<- function(solve) solve <<- solve
    
    getsolve <- function() solve
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}

cachesolve <- function(x, ...) {
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

    solve <- x$getsolve()
    
    if(!is.null(solve)) {
        message("getting cached data")
        return(solve)
    }
    
    data <- x$get()
    
    solve <- solve(data, ...)
    
    x$setsolve(solve)
    
    solve
}
