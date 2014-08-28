## The two functions below compute and cache the inverse of a matrix. Once the
## inverse is cached, repeated calls to the cacheSolve function will return the
## cached value to avoid recomputation.

## Creates a special "matrix" able to cache its inverse
## Parameters:
##      'x' is a square matrix to be converted into a special "matrix"
##      with a cache-able inverse
## Returns: the special "matrix" (a list)
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse variable
    inv <- NULL
    
    ## Define set, get, setinverse, and getinverse functions
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## Return functions as part of list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns the inverse of the special "matrix", using the cached value if
## available
## Parameters:
##      'x' is a special "matrix" created by the makeCacheMatrix() function
## Returns: the inverse of the original matrix
cacheSolve <- function(x, ...) {
    ## Check cached value and return if exists
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If cached value is null, get original matrix and compute inverse, then
    ## cache it and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
