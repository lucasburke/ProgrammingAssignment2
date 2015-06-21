## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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