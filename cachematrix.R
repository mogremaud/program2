## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse a special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}

