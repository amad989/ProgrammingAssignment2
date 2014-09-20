##These two functions can be used to repeatedly find the inverse of a matrix more quickly,
##by storing the value to reuse rather than recalculate it

## Creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(square) m <<- square
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If the inverse of x is cached, pulls from cache and returns inverse
## If the inverse of x is not cached, solves for inverse, stores in cache, and returns the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## Return a matrix that is the inverse of 'x'