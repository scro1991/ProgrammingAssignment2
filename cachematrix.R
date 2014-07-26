## These two functions allow to catching the inverse of a matrix

## makeCacheMatrix creates a matrix object which can catch the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve<- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## CacheSolve computes the inverse of the object returned by makeCatcheMatrix
## If the inverse has been calculated for a given matrix, the function retrieves
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
