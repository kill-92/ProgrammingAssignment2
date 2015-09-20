## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix", which is really
## a list of getters and setters for matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with
## the makeCacheMatrix function. First it tries to get the inverse from cache.
## If it fails it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
