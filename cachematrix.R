## This R file provides a mechanism for caching the inverse of a matrix
## to avoid recomputation.
##
## This is accomplished by creating an R object capable to storing its own inverse
##
## To use this, 
## First create the inverse caching R object by calling the makeCacheMatrix function
## Then call the cacheSolve (possibly multiple times)

## Creates an R object which is a wrapper over a matrix and capable of caching its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(inv) {
        inv <<- inv
    }
    getInv <- function() {
        inv 
    }
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Computes the inverse of the matrix given the R object created by the makeCacheMatrix function
## The actual computation is done on the first call(with a particular object), 
## the subsequent calls return the cached value
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ## Return a matrix that is the inverse of 'data'
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
