## Caching the Inverse of a Matrix
##

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## makeVector creates a special "vector", which is really a list containing a function to
## set: set the value of the matrix
## get: get the value of the matrix
## setInverse: set the value of the inverse
## getInverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getInverse())) {
        x$setInverse(solve(x$get()))
    }
    x$getInverse()
}
