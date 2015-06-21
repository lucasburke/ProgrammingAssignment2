## Allow a matrix's inverse, after it is calculated the first time, to be 
## cached with the matrix; if calculated again, the cached result will be
## returned.

## makeCacheMatrix:
## Provides a list of functions to: cache a matrix in its environment, return
## the matrix, cache a calculated inverse of the matrix, and return the cached
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinv <- function(inv) v <<- inv
    getinv <- function() v
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve:
## Request and return the inverse of a makeCacheMatrix if it is cached,
## otherwise calculate the inverse and cache the result.

cacheSolve <- function(x, ...) {
cacheSolve <- function(x) {
    v <- x$getinv()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data)
    x$setinv(v)
    v
}