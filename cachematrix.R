## This pair of functions calculate the inverse of a matrix.
## The result is cached, so that the inverse doesn't have
## calculated again, as long as the matrix is unchanged.

## makeCacheMatrix is a function that creates a
## special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve is a function that computes the 
## inverse of the special "matrix" returned 
## by makeCacheMatrix.  If the inverse has already 
## been calculated, the cachesolve should retrieve the
## inverse from the cache.

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