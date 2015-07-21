## The function makeCacheMatrix takes a matrix and returns a list 
## containing a function for setting/getting the matrix and the
## value of the inverse. The function cacheSolve takes the resulting
## list and computes the inverse of the matrix only if necessary,
## otherwise returning the cached value.

## Take a matrix, return a function for getting/setting the matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Take a list returned by makeCacheMatrix and retrieve the cached inverse
## (or compute the inverse if necessary).

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
