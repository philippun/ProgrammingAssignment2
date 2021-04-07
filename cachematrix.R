## Program that calculates the inverse of a matrix and caches it for future
## use. So as long as the initial matrix does not change, every additional 
## calculation of the inverse will just be retrieved from the cache instead
## of calculating it again. 

## Function that creates a special "vector" that stores the matrix and the
## the inverse and provides functions for getting and setting each of them.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function(solve)
        i <<- solve
    getinverse <- function()
        i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Function that tries to retrieve the inverse from cache and checks whether it
## was already calculated. If yes, then the cache inverse is returned. 
## Otherwise it is calculated, stored into cache and returned. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
