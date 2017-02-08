## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##This function will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    myinverse <- NULL
    set <- function(y) {
        x <<- y
        myinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) myinverse <<- inverse
    getinverse <- function() myinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    myinverse <- x$getinverse()
    if(!is.null(myinverse)) {
        message("getting cached data")
        return(myinverse)
    }
    mymatrix <- x$get()
    myinverse <- solve(mymatrix, ...)
    x$setinverse(myinverse)
    myinverse
}
