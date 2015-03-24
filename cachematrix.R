## Below are two functions that are used to create a special object that stores a 
## matrix and caches its inverse.

## Below function creates a special "matrix", which is really a list containing a 
## function to
## setmatrix: set the value of the matrix
## getmatrix: get the value of the matrix
## setinverse: cache the value of the inverse
## getinverse: returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special matrix created by the makeCacheMatrix function
## and sets the cache. If the result is already available in cache it returns the 
## result from cache and avoids re-calculate

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$getmatrix()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
