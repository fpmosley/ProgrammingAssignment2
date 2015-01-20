## These functions create a matrix object, with built-in cache, and computes the
## inverse of an invertible matrix in a matrix object  If the inverse of the
## matrix has been previously calculated, for the matrix object, then the result
## is returned from the cache.

## Function to create a matrix object that contains the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        if (!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) 
              && all(x == y))) {
            message("Matrices not identical. Set new matrix to cache.")
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function to compute the inverse for the given matrix object. If the inverse
## is cached, then the result is retrieved from the cache. If not, then the
## inverse is calculated, and stored in the matrix object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached inverse.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}