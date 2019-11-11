## Complmentary functions that
##  construct a matrix object that can cache its inverse.
#   given such an object return either the cached inverse,
#   or calculate an inverse to return and cache that inverse.

# Given a matrix, construct a matrix object that can
# cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function () m
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Return a matrix that is the inverse of a
# "cachematrix".
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        # matrix was previously inverted and the result cached.
        message("getting cached data")
        return(i)
    }
    # invert the matrix and cache the result.
    data <- x$get()
    i <- solve(data, ...)
    print(i);
    x$setinverse(i)
    i
}
