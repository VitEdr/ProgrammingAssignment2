## Functions below create square matrix, calculate and cache its inverse.
## Whenever an inverse of matrix is called cache is checked first.

## This function creates matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function checks if the inverse have already been calculated
## and calculates inverse if not.

cacheSolve <- function(x, ...) {
    m <- x$getinvers()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m ## Return a matrix that is the inverse of 'x'
}
