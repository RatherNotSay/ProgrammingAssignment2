## These functions implement matrix inverse cache based on the vector mean cache example.

## Returns a list of functions to manipulate the supplied matrix.
## There are getter and setter methods get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a matrix encapsulated with makeCacheMatrix.
## If the inverse was already calcutated, calculation is skipped and the cached inverse
## is returned, else it is computed, stored in the cache and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
}
