
## These functions create a special matrix object
## and cache its inverse to avoid repeated calculations.

## Creates a special matrix object that stores the matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() {
                x
        }

        setinverse <- function(inv) {
                inverse <<- inv
        }

        getinverse <- function() {
                inverse
        }

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special matrix.
## If already calculated, returns the cached inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()

        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- x$get()
        inverse <- solve(data, ...)

        x$setinverse(inverse)

        inverse
}
